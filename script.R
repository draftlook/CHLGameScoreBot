###Function 1: Get game urls for specified date
get_daily_urls <- function(date) {
  #Loading packages
  library(rvest)
  library(xml2)
  library(glue)
  library(jsonlite)
  #Setting league keys
  ohl_key <- "2976319eb44abe94"
  whl_key <- "41b145a848f4bd67"
  qmjhl_key <- "f322673b6bcae299"
  #Getting OHL games
  ohl_json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", ohl_key, "&fmt=json&client_code=ohl&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  ohl_game_ids <- jsonlite::fromJSON(ohl_json_link)[['SiteKit']][['Gamesbydate']][['id']]
  if(length(ohl_game_ids) == 0) {ohl_game_links <- NULL} else {
    ohl_game_links <- paste(paste("https://ontariohockeyleague.com/gamecentre/", ohl_game_ids, sep=""),"/boxscore", sep="")
  }
  #Getting WHL games
  whl_json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", whl_key, "&fmt=json&client_code=whl&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  whl_game_ids <- jsonlite::fromJSON(whl_json_link)[['SiteKit']][['Gamesbydate']][['id']]
  if(length(whl_game_ids) == 0) {whl_game_links <- NULL} else {
    whl_game_links <- paste(paste("https://whl.ca/gamecentre/", whl_game_ids, sep=""),"/boxscore", sep="")
  }
  #Getting QMJHL games
  qmjhl_json_link <- glue("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=gamesbydate&key=", qmjhl_key, "&fmt=json&client_code=lhjmq&lang=en&league_code=&fetch_date=", date, "&fmt=json")
  qmjhl_game_ids <- jsonlite::fromJSON(qmjhl_json_link)[['SiteKit']][['Gamesbydate']][['id']]
  if(length(qmjhl_game_ids) == 0) {qmjhl_game_links <- NULL} else {
    qmjhl_game_links <- paste(paste("https://theqmjhl.ca/gamecentre/", qmjhl_game_ids, sep=""),"/boxscore", sep="")
  }
  #Returning one vector of all games for all leagues
  return(c(ohl_game_links, whl_game_links, qmjhl_game_links))
}

###Function 2: Get game urls for a range of dates
get_range_urls <- function(start_date, end_date) {
  dates <- sapply(seq(as.Date(start_date), as.Date(end_date), "days"), as.character)
  return(unlist(lapply(dates, get_daily_urls)))
}

###Function 3: Get game data, create table, tweet table
tweet_game_table <- function(game_url) {
  #Loading packages
  library(tidyr)
  library(dplyr)
  library(gt)
  ##Building JSON URL
  #Extracting key
  key <- read_html(game_url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-feed_key")
  key <- read_html(game_url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-feed_key")
  #Extracting client_code (league)
  client_code <- read_html(game_url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-league")
  #Extracting game_id
  game_id <- read_html(game_url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-path")
  #Extracting lang_code
  lang_code <- read_html(game_url) %>%
    html_nodes("#gamecentre") %>%
    html_attr("data-lang")
  ##Formulating json_url
  json_url <- glue(
    "https://cluster.leaguestat.com/feed/index.php?feed=gc&key=",
    key,
    "&client_code=",
    client_code,
    "&game_id=",
    game_id,
    "&lang_code=",
    lang_code,
    "&fmt=json&tab=gamesummary"
  )
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
  if(!(length(pluses) == 0)) {
    colnames(pluses)[1] <- "player_id"
    pluses <- pluses %>% group_by(player_id) %>% tally()
    colnames(pluses)[2] <- "goals_for"
  } else {
    pluses <- data.frame(0,0)
    colnames(pluses)[1] <- "player_id"
    colnames(pluses)[2] <- "goals_for"
  }
  #Adding individual pluses to lineup data
  home_lineup <- merge(home_lineup, pluses, by="player_id", all.x=TRUE)
  away_lineup <- merge(away_lineup, pluses, by="player_id", all.x=TRUE)
  
  #Getting minuses
  minuses <- as.data.frame(unlist(lapply(which(even_strength == TRUE), FUN = function(x) data[['GC']][['Gamesummary']][['goals']][['minus']][[x]]$player_id)))
  if(!(length(minuses) == 0)) {
    colnames(minuses)[1] <- "player_id"
    minuses <- minuses %>% group_by(player_id) %>% tally()
    colnames(minuses)[2] <- "goals_against"
  } else {
    minuses <- data.frame(0,0)
    colnames(minuses)[1] <- "player_id"
    colnames(minuses)[2] <- "goals_against"
  }
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
  homescore <- data[['GC']][['Gamesummary']][['meta']][['home_goal_count']]
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
  awayscore <- data[['GC']][['Gamesummary']][['meta']][['visiting_goal_count']]
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
  lineups <- rbind(home_lineup, away_lineup)
  
  #Building subtitle string
  date <- data[['GC']][['Gamesummary']][['game_date']]
  if(client_code == "ohl") {
    subtitle <- glue("Ontario Hockey League | ", date)
  } else if (client_code == "whl") {
    subtitle <- glue("Western Hockey League | ", date)
  } else if (client_code == "lhjmq") {
    subtitle <- glue("Quebec Major Junior Hockey League | ", date)
  }
  
  table <- lineups[c("name", "pos", "code", "goals", "first_assists", "second_assists", "sog", "penalties_taken", "faceoff_wins", "faceoff_losses", "goals_for", "goals_against", "game_score")][order(-lineups$game_score),] %>%
    gt() %>%
    tab_header(title= glue(awayteam, " (", awayscore, ") @ ", hometeam, " (", homescore, ")"), subtitle= subtitle) %>%
    tab_source_note("Bot by @DraftLook using data from chl.ca") %>%
    tab_source_note("Game score concept from Dom Luszczyszyn") %>%
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
      game_score = "Score"
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    cols_align(
      align = "left",
      columns = "name"
    ) %>%
    cols_align(
      align = "right",
      columns = "game_score"
    ) %>%
    cols_width(
      "game_score" ~ px(30)
    ) %>%
    tab_options(data_row.padding = px(3)) %>%
    tab_style(
      style = list(
        cell_text(
          font = "Bahnschrift",
          align = "center"
        )
      ),
      locations = list(
        cells_title(groups = "title")
      )) %>%
    tab_style(
      style = list(
        cell_text(
          font = "Bahnschrift",
          align = "center"
        )
      ),
      locations = list(
        cells_title(groups = "subtitle")
      )) %>%
    opt_table_font(google_font("Karla"), weight="bolder")
  #Saving table as 'table.png' to project directory
  gtsave(table, "table.png")
  #Setting Twitter auth info
  api_key <- Sys.getenv("TWITTER_API_KEY")
  api_key_secret <- Sys.getenv("TWITTER_API_SECRET")
  access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
  access_token_secret <- Sys.getenv("TWITTER_ACCESS_SECRET")
  #Authorizing twittR
  setup_twitter_oauth(api_key, api_key_secret, access_token, access_token_secret)
  #Tweeting table
  tweet(glue("#CHL Game Score Card: ", hometeam, " vs ", awayteam, " on ", data[['GC']][['Gamesummary']][['meta']][['date_played']]), mediaPath = "table.png")
  return(lineups)
}

tweet_all <- function(date) {
  all_games_list <- get_daily_urls(date) %>% lapply(tweet_game_table)
  all_games_df <- bind_rows(all_games_list)
  all_games_df <- head(all_games_df[order(-all_games_df$game_score),],25)
  table <- all_games_df[c("name", "pos", "code", "goals", "first_assists", "second_assists", "sog", "penalties_taken", "faceoff_wins", "faceoff_losses", "goals_for", "goals_against", "game_score")][order(-all_games_df$game_score),] %>%
    gt() %>%
    tab_header(title= "Today's Top Performers", subtitle= glue("Canadian Hockey League | ", date)) %>%
    tab_source_note("Bot by @DraftLook using data from chl.ca") %>%
    tab_source_note("Game score concept from Dom Luszczyszyn") %>%
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
      game_score = "Score"
    ) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    cols_align(
      align = "left",
      columns = "name"
    ) %>%
    cols_align(
      align = "right",
      columns = "game_score"
    ) %>%
    cols_width(
      "game_score" ~ px(30)
    ) %>%
    tab_options(data_row.padding = px(3)) %>%
    tab_style(
      style = list(
        cell_text(
          font = "Bahnschrift",
          align = "center"
        )
      ),
      locations = list(
        cells_title(groups = "title")
      )) %>%
    tab_style(
      style = list(
        cell_text(
          font = "Bahnschrift",
          align = "center"
        )
      ),
      locations = list(
        cells_title(groups = "subtitle")
      )) %>%
    opt_table_font(google_font("Karla"), weight="bolder")
  #Saving top performers table as 'table.png'
  gtsave(table, "table.png")
  #Setting Twitter auth info
  api_key <- Sys.getenv("TWITTER_API_KEY")
  api_key_secret <- Sys.getenv("TWITTER_API_SECRET")
  access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
  access_token_secret <- Sys.getenv("TWITTER_ACCESS_SECRET")
  #Authorizing twittR
  setup_twitter_oauth(api_key, api_key_secret, access_token, access_token_secret)
  #Tweeting table
  tweet(glue("CHL Top Performers on ", date), mediaPath = "table.png")
}

tweet_all(toString(Sys.Date()))
