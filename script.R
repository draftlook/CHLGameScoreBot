###Functions

##Function: Get box score urls
get_game_urls <- function(date, league) {
  ##Loading packages
  library(rvest)
  library(xml2)
  library(glue)
  library(jsonlite)
  
  ohl_key <- "2976319eb44abe94"
  whl_key <- "41b145a848f4bd67"
  lhjmq_key <- "f322673b6bcae299"
  
  ##Getting JSON link for the day's link
  if(league == "ohl") {
  json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", ohl_key, "&fmt=json&client_code=", league, "&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  }
  if(league == "whl") {
    json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", whl_key, "&fmt=json&client_code=", league, "&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  }
  if(league == "lhjmq") {
    json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", lhjmq_key, "&fmt=json&client_code=", league, "&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  }

  game_ids <- jsonlite::fromJSON(json_link)[['SiteKit']][['Gamesbydate']][['id']]
  if(length(game_ids) == 0) {
    stop()
  }
  if(league == "ohl") {
  output <- paste(paste("https://ontariohockeyleague.com/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  }
  if(league == "whl") {
    output <- paste(paste("https://whl.ca/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  }
  if(league == "lhjmq") {
    output <- paste(paste("https://theqmjhl.ca/gamecentre/", game_ids, sep=""),"/boxscore", sep="")
  }
  return(output)
}

##Function: Get JSON url
get_json_url <- function(url) {
  ##Loading packages
  library(rvest)
  library(xml2)
  library(glue)
  
  ##Setting url and static codes
  feed <- "gc"
  fmt <- "json"
  tab <- "gamesummary"
  
  ##Extracting key
  key <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-feed_key")
  
  ##Extracting client_code (league)
  client_code <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-league")
  
  ##Extracting game_id
  game_id <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-path")
  
  ##Extracting lang_code
  lang_code <- read_html(url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-lang")
  
  ##Formulating json_url
  json_url <- glue(
    "https://cluster.leaguestat.com/feed/index.php?feed=",
    feed,
    "&key=",
    key,
    "&client_code=",
    client_code,
    "&game_id=",
    game_id,
    "&lang_code=",
    lang_code,
    "&fmt=",
    fmt,
    "&tab=",
    tab
  )
  return(json_url)
}

##Function: Get lineup data
get_lineup_data <- function(json_url) {
  library(tidyr)
  library(dplyr)
  #Getting JSON data
  data <- jsonlite::fromJSON(json_url)
  #Getting lineups
  home_lineup <- data[['GC']][['Gamesummary']][['home_team_lineup']][['players']]
  away_lineup <- data[['GC']][['Gamesummary']][['visitor_team_lineup']][['players']]
  #Uniting full name
  home_lineup <- unite(home_lineup, col="name", c("first_name", "last_name"), sep=" ")
  away_lineup <- unite(away_lineup, col="name", c("first_name", "last_name"), sep=" ")
  
  #Getting goal data
  goals <- data[['GC']][['Gamesummary']][['goals']][['goal_scorer']]$player_id
  #Adding individual goals to lineup data
  home_lineup <- cbind(home_lineup, as.data.frame(table(goals)[home_lineup$player_id])$Freq)
  away_lineup <- cbind(away_lineup, as.data.frame(table(goals)[away_lineup$player_id])$Freq)
  
  #Getting primary assist data
  first_assists <- data[['GC']][['Gamesummary']][['goals']][['assist1_player']]$player_id
  #Adding individual primary assists to lineup data
  home_lineup <- cbind(home_lineup, as.data.frame(table(first_assists)[home_lineup$player_id])$Freq)
  away_lineup <- cbind(away_lineup, as.data.frame(table(first_assists)[away_lineup$player_id])$Freq)
  
  #Getting secondary assist data
  second_assists <- data[['GC']][['Gamesummary']][['goals']][['assist2_player']]$player_id
  #Adding individual secondary assists to lineup data
  home_lineup <- cbind(home_lineup, as.data.frame(table(second_assists)[home_lineup$player_id])$Freq)
  away_lineup <- cbind(away_lineup, as.data.frame(table(second_assists)[away_lineup$player_id])$Freq)
  
  ## Getting goal info
  goal_info <- cbind(as.data.frame(as.numeric(data[['GC']][['Gamesummary']][['goals']][['power_play']])), as.data.frame(as.numeric(data[['GC']][['Gamesummary']][['goals']][['empty_net']])), as.data.frame(as.numeric(data[['GC']][['Gamesummary']][['goals']][['penalty_shot']])), as.data.frame(as.numeric(data[['GC']][['Gamesummary']][['goals']][['short_handed']]))) %>%
    dplyr::rename(
      power_play = 'as.numeric(data[["GC"]][["Gamesummary"]][["goals"]][["power_play"]])',
      empty_net = 'as.numeric(data[["GC"]][["Gamesummary"]][["goals"]][["empty_net"]])',
      penalty_shot = 'as.numeric(data[["GC"]][["Gamesummary"]][["goals"]][["penalty_shot"]])',
      short_handed = 'as.numeric(data[["GC"]][["Gamesummary"]][["goals"]][["short_handed"]])'
    )
  #Getting whether a goal was scored at even strength or not
  even_strength <- unlist(lapply(1:nrow(goal_info), FUN = function(x) if(goal_info$power_play[x] == 0 && goal_info$empty_net[x] == 0 && goal_info$penalty_shot[x] == 0 && goal_info$short_handed[x] == 0) {TRUE} else {FALSE}))
  
  #Getting pluses
  pluses <- as.data.frame(unlist(lapply(which(even_strength == TRUE), FUN = function(x) data[['GC']][['Gamesummary']][['goals']][['plus']][[x]]$player_id)))
  colnames(pluses)[1] <- "player_id"
  pluses <- pluses %>% group_by(player_id) %>% tally()
  colnames(pluses)[2] <- "goals_for"
  #Adding individual pluses to lineup data
  home_lineup <- merge(home_lineup, pluses, by="player_id", all.x=TRUE)
  away_lineup <- merge(away_lineup, pluses, by="player_id", all.x=TRUE)
  
  #Getting minuses
  minuses <- as.data.frame(unlist(lapply(which(even_strength == TRUE), FUN = function(x) data[['GC']][['Gamesummary']][['goals']][['minus']][[x]]$player_id)))
  colnames(minuses)[1] <- "player_id"
  minuses <- minuses %>% group_by(player_id) %>% tally()
  colnames(minuses)[2] <- "goals_against"
  #Adding individual minuses to lineup data
  home_lineup <- merge(home_lineup, minuses, by="player_id", all.x=TRUE)
  away_lineup <- merge(away_lineup, minuses, by="player_id", all.x=TRUE)
  
  ##Getting penalties
  penalties <- data[['GC']][['Gamesummary']][['penalties']]$player_penalized_info$player_id
  home_lineup <- cbind(home_lineup, as.data.frame(table(penalties)[home_lineup$player_id])$Freq)
  away_lineup <- cbind(away_lineup, as.data.frame(table(penalties)[away_lineup$player_id])$Freq)
  
  home_lineup[is.na(home_lineup)] <- 0
  home_lineup <- home_lineup %>%
    dplyr::rename(
      g = "as.data.frame(table(goals)[home_lineup$player_id])$Freq",
      a1 = "as.data.frame(table(first_assists)[home_lineup$player_id])$Freq",
      a2 = "as.data.frame(table(second_assists)[home_lineup$player_id])$Freq",
      penalties_taken = "as.data.frame(table(penalties)[home_lineup$player_id])$Freq"
    )
  
  away_lineup[is.na(away_lineup)] <- 0
  away_lineup <- away_lineup %>%
    dplyr::rename(
      g = "as.data.frame(table(goals)[away_lineup$player_id])$Freq",
      a1 = "as.data.frame(table(first_assists)[away_lineup$player_id])$Freq",
      a2 = "as.data.frame(table(second_assists)[away_lineup$player_id])$Freq",
      penalties_taken = "as.data.frame(table(penalties)[away_lineup$player_id])$Freq"
    )
  
  ##Cleaning home lineup
  home_lineup <- mutate(home_lineup, faceoff_losses = as.numeric(home_lineup$faceoff_attempts) - as.numeric(home_lineup$faceoff_wins))
  hometeam <- data[['GC']][['Gamesummary']][['home']][['name']]
  homecode <- data[['GC']][['Gamesummary']][['home']][['code']]
  home_lineup <- cbind(home_lineup, as.data.frame(hometeam))
  home_lineup <- cbind(home_lineup, as.data.frame(homecode))
  home_lineup <- home_lineup[c("player_id", "name", "hometeam", "homecode", "position_str", "g", "a1", "a2", "shots_on", "penalties_taken", "faceoff_wins", "faceoff_losses", "goals_for", "goals_against")] %>%
    dplyr::rename(
      team = hometeam,
      code = homecode,
      pos = position_str,
      goals = g,
      first_assists = a1,
      second_assists = a2,
      sog = shots_on
    )
  
  ##Cleaning away lineup
  away_lineup <- mutate(away_lineup, faceoff_losses = as.numeric(away_lineup$faceoff_attempts) - as.numeric(away_lineup$faceoff_wins))
  awayteam <- data[['GC']][['Gamesummary']][['visitor']][['name']]
  awaycode <- data[['GC']][['Gamesummary']][['visitor']][['code']]
  away_lineup <- cbind(away_lineup, as.data.frame(awayteam))
  away_lineup <- cbind(away_lineup, as.data.frame(awaycode))
  away_lineup <- away_lineup[c("player_id", "name", "awayteam", "awaycode", "position_str", "g", "a1", "a2", "shots_on", "penalties_taken", "faceoff_wins", "faceoff_losses", "goals_for", "goals_against")] %>%
    dplyr::rename(
      team = awayteam,
      code = awaycode,
      pos = position_str,
      goals = g,
      first_assists = a1,
      second_assists = a2,
      sog = shots_on
    )
  
  #Calculating game scores
  home_lineup <- mutate(home_lineup, game_score = ((0.75 * as.numeric(home_lineup$goals)) + (0.7 * as.numeric(home_lineup$first_assists)) + (0.55 * as.numeric(home_lineup$second_assists)) + (0.075 * as.numeric(home_lineup$sog)) - (0.15 * as.numeric(home_lineup$penalties_taken)) + (0.01 * as.numeric(home_lineup$faceoff_wins)) - (0.01 * as.numeric(home_lineup$faceoff_losses)) + (0.15 * as.numeric(home_lineup$goals_for)) - (0.15 * as.numeric(home_lineup$goals_against))))
  home_lineup <- mutate(home_lineup, home_or_away = "home")
  away_lineup <- mutate(away_lineup, game_score = ((0.75 * as.numeric(away_lineup$goals)) + (0.7 * as.numeric(away_lineup$first_assists)) + (0.55 * as.numeric(away_lineup$second_assists)) + (0.075 * as.numeric(away_lineup$sog)) - (0.15 * as.numeric(away_lineup$penalties_taken)) + (0.01 * as.numeric(away_lineup$faceoff_wins)) - (0.01 * as.numeric(away_lineup$faceoff_losses)) + (0.15 * as.numeric(away_lineup$goals_for)) - (0.15 * as.numeric(away_lineup$goals_against))))
  away_lineup <- mutate(away_lineup, home_or_away = "away")
  lineup_data <- rbind(home_lineup, away_lineup)
  return(lineup_data)
}

##Function: Get match score
get_match_score <- function(json_url) {
  library(jsonlite)
  library(dplyr)
  # Getting JSON file
  data <- jsonlite::fromJSON(json_url)
  data <- as.data.frame(data[['GC']][['Gamesummary']][['goals']][['goal_scorer']][['team_code']])
  colnames(data)[1] <- "team"
  data <- data %>% group_by(team) %>% tally()
  return(data)
}

##Function: Build game score table
get_table <- function(json_urls, x, home_or_visitor, date) {
  library(gt)
  library(glue)
  library(webshot)
  lineup <- lapply(json_urls, get_lineup_data)[[x]]
  score <- lapply(json_urls, get_match_score)[[x]]
  
  home_team <- lineup$team[1]
  home_code <- lineup$code[1]
  away_team <- lineup$team[nrow(lineup)]
  away_code <- lineup$code[nrow(lineup)]
  lineup <- filter(lineup, lineup$home_or_away == home_or_visitor)
  
  #Home team
  table <- lineup[c("name", "code", "pos", "goals", "first_assists", "second_assists", "sog", "penalties_taken", "faceoff_wins", "faceoff_losses", "goals_for", "goals_against", "game_score")][order(-lineup$game_score),] %>%
    gt() %>%
    tab_header(title = glue(home_team, " (", score$n[score$team == home_code], ") ", "vs ", away_team, " (", score$n[score$team == away_code], ")"), subtitle = date) %>%
    cols_label(
      name = "Player",
      code = "Team",
      pos = "Pos",
      goals = "G",
      first_assists = "A1",
      second_assists = "A2",
      sog = "SOG",
      penalties_taken = "Pen.",
      faceoff_wins = "FOW",
      faceoff_losses = "FOL",
      goals_for = "GF",
      goals_against = "GA",
      game_score = "Game Score"
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    cols_align(
      align = "left",
      columns = "name"
    ) %>%
    cols_width(
      "game_score" ~ px(100)
    )
  #Saving table as png
  gtsave(table, "table.png")
  ###twitteR
  library(twitteR)
  #Loading app keys/tokens
  api_key <- Sys.getenv("TWITTER_API_KEY")
  api_secret <- Sys.getenv("TWITTER_API_SECRET")
  access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
  access_secret <- Sys.getenv("TWITTER_ACCESS_SECRET")
  #Authorizing twitteR
  setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
  #Getting "hometeam vs awayteam/awayteam @hometeam" string
  twitter_string <- ifelse(home_or_visitor == "home", glue(home_team, " vs ", away_team), glue(away_team, " @ ", home_team))
  #Tweeting table
  tweet(glue("#CHL Game Score Card: ", twitter_string, " on ", date), mediaPath = "table.png")
}

#Function: Tweet tables
tweet_tables <- function(x, date, json_urls) {
  json_urls %>% get_table(x, "home", date)
  json_urls %>% get_table(x, "away", date)
}

#Function: Tweet all games from specified league
tweet_league <- function(date, league) {
  #Getting game urls
  game_urls <- get_game_urls(date, league)
  #Extracting JSON file urls
  json_urls <- sapply(game_urls, get_json_url)
  #Getting lineup data & tweeting game score cards
  lapply(1:length(json_urls), tweet_tables, date = date, json_urls = json_urls)
}

### SCRIPT
#Tweet all leagues
try(tweet_league(as.character(Sys.Date()-710), "lhjmq"))
try(tweet_league(as.character(Sys.Date()-710), "ohl"))
tweet_league(as.character(Sys.Date()-710), "whl")
