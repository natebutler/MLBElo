install.packages(c("httr", "jsonlite", "dplyr", "tibble", "stringr", "tidyr"))
library(httr)
library(jsonlite)
library(tidyverse)

# Yeah I know the name needs some work...
ParserTodayGame <- function() {
  # Initial GET Call to Today's Game API
  res <- GET("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1")
  
  # Create raw data frame
  data <- fromJSON(rawToChar(res$content), flatten = TRUE)
  
  # Convert raw data frame to unlisted tibble
  dataRaw <- enframe(unlist(data))
  
  # Replace '.' with '_'
  # Simply helps later down the road with column manipulation
  rgxDelim <- "\\."
  dataRaw$name <- str_replace_all(dataRaw$name, rgxDelim, "_")
  
  # Remove digits associated to column names
  cleanedNames <- str_replace_all(dataRaw$name, "[:digit:]", "")
  
  # Create list of unique columns
  uniqueColumns <- unique(cleanedNames)
  
  # Columns for specific game information
  # Not required... I just don't care about the other columns
  todaysGamesColumns <- uniqueColumns[11:65]
  
  # Determine number of games happening today
  numGames <- dataRaw[dataRaw$name == "totalItems",]
  
  # Loop through each column name
  # Create a new dataframe with column names
  parsedTodaysGames <- data.frame(matrix(nrow = as.integer(numGames$value), ncol = length(todaysGamesColumns)))
  colnames(parsedTodaysGames) <- todaysGamesColumns
  
  for(i in 1:length(todaysGamesColumns)){
    values <- dataRaw %>%
      filter(grepl(todaysGamesColumns[i], dataRaw$name))
    if (length(values$value) > as.integer(numGames$value)){
      values <- values[1:as.integer(numGames$value),]
    }
    parsedTodaysGames[,i] <- values$value
  }
  return(parsedTodaysGames)
}

letsSeeWhatHappens <- ParserTodayGame()


################################################################################
ParserGameByDate <- function(game_date = Sys.Date()) {
  # Format the input date
  formatted_date <- format(as.Date(game_date), "%Y-%m-%d")
  
  # Construct the URL with date filtering
  url <- paste0("http://statsapi.mlb.com/api/v1/schedule/games?sportId=1&startDate=", 
                formatted_date, "&endDate=", formatted_date)
  
  # API call
  res <- GET(url)
  
  # Parse the JSON content
  data <- fromJSON(rawToChar(res$content), flatten = TRUE)
  
  # Unlist and convert to tibble
  dataRaw <- enframe(unlist(data))
  
  # Clean column names
  dataRaw$name <- str_replace_all(dataRaw$name, "\\.", "_")
  cleanedNames <- str_replace_all(dataRaw$name, "[:digit:]", "")
  uniqueColumns <- unique(cleanedNames)
  
  # You can adjust this slice depending on which columns you want
  todaysGamesColumns <- uniqueColumns[11:65]
  
  # Get number of games
  numGames <- dataRaw[dataRaw$name == "totalItems",]
  
  # Preallocate output dataframe
  parsedGames <- data.frame(matrix(nrow = as.integer(numGames$value), 
                                   ncol = length(todaysGamesColumns)))
  colnames(parsedGames) <- todaysGamesColumns
  
  # Fill in the columns
  for(i in 1:length(todaysGamesColumns)){
    values <- dataRaw %>%
      filter(grepl(todaysGamesColumns[i], dataRaw$name))
    if (length(values$value) > as.integer(numGames$value)){
      values <- values[1:as.integer(numGames$value),]
    }
    parsedGames[,i] <- values$value
  }
  return(parsedGames)
}

# Today
today_games <- ParserGameByDate()

# Yesterday
yesterday_games <- ParserGameByDate(Sys.Date() - 1)

tomorrow_games <- ParserGameByDate(Sys.Date() + 1)

# Specific date
opening_day_games <- ParserGameByDate("2025-03-28")
first_games <- ParserGameByDate("2025-03-18")

################################################################################
library(dplyr)
library(stringr)
library(tibble)
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)

# Your updated game parser
ParserGameByDate <- function(game_date = Sys.Date()) {
  formatted_date <- format(as.Date(game_date), "%Y-%m-%d")
  url <- paste0("http://statsapi.mlb.com/api/v1/schedule/games?sportId=1&startDate=", 
                formatted_date, "&endDate=", formatted_date)
  res <- GET(url)
  data <- fromJSON(rawToChar(res$content), flatten = TRUE)
  dataRaw <- enframe(unlist(data))
  dataRaw$name <- str_replace_all(dataRaw$name, "\\.", "_")
  
  totalItems <- dataRaw %>%
    filter(name == "totalItems") %>%
    pull(value) %>%
    as.integer()
  
  # If no games on this date, skip
  if (is.na(totalItems) || totalItems == 0) {
    #message(paste("No games found on", formatted_date))
    return(NULL)
  }
  
  cleanedNames <- str_replace_all(dataRaw$name, "[:digit:]", "")
  uniqueColumns <- unique(cleanedNames)
  todaysGamesColumns <- uniqueColumns#[11:65]
  numGames <- dataRaw[dataRaw$name == "totalItems",]
  parsedGames <- data.frame(matrix(nrow = as.integer(numGames$value), 
                                   ncol = length(todaysGamesColumns)))
  colnames(parsedGames) <- todaysGamesColumns
  # for(i in 1:length(todaysGamesColumns)){
  #   values <- dataRaw %>%
  #     filter(grepl(todaysGamesColumns[i], dataRaw$name))
  #   if (length(values$value) > as.integer(numGames$value)){
  #     values <- values[1:as.integer(numGames$value),]
  #   }
  #   parsedGames[,i] <- values$value
  # }
  
  for (i in seq_along(todaysGamesColumns)) {
    values <- dataRaw %>%
      filter(str_detect(name, fixed(todaysGamesColumns[i])))
    
    val_vec <- values$value
    n <- as.integer(numGames$value)
    
    # Pad with NA if needed
    if (length(val_vec) < n) {
      val_vec <- c(val_vec, rep(NA, n - length(val_vec)))
    }
    
    if (length(val_vec) > n) {
      val_vec <- val_vec[1:n]
    }
    
    parsedGames[, i] <- val_vec
  }
  
  
  return(parsedGames)
}

# Elo update function
update_elo <- function(team_elo, opponent_elo, outcome, k = 10) {
  expected_score <- 1 / (1 + 10 ^ ((opponent_elo - team_elo) / 400))
  new_elo <- team_elo + k * (outcome - expected_score)
  return(new_elo)
}

initialize_elo <- function(regress_to_mean = 0.75){
  # Old Elo ratings and the corresponding team names (last year's ranking)
  last_year_teams <- c("Los Angeles Dodgers", "San Diego Padres", "New York Yankees", "Milwaukee Brewers", 
                       "Arizona Diamondbacks", "New York Mets", "Atlanta Braves", "Houston Astros", 
                       "Seattle Mariners", "Philadelphia Phillies", "Chicago Cubs", "Detroit Tigers", 
                       "Baltimore Orioles", "Cleveland Guardians", "Kansas City Royals", "Tampa Bay Rays", 
                       "San Francisco Giants", "St. Louis Cardinals", "Boston Red Sox", "Texas Rangers", 
                       "Cincinnati Reds", "Minnesota Twins", "Toronto Blue Jays", "Pittsburgh Pirates", 
                       "Washington Nationals", "Miami Marlins", "Athletics", "Los Angeles Angels", "Colorado Rockies", 
                       "Chicago White Sox")
  
  last_year_elo_ratings <- c(1583, 1555, 1545, 1544, 1543, 1542, 1541, 1541, 1535, 1533, 1529, 
                             1521, 1520, 1517, 1512, 1510, 1507, 1500, 1498, 1495, 1494, 1492, 
                             1489, 1476, 1463, 1455, 1453, 1434, 1431, 1394)
  
  elo_df <- data.frame(team = last_year_teams, elo_old = last_year_elo_ratings)
  
  # Regress Elo ratings toward the mean (default mean = 1500)
  elo_df$elo <- regress_to_mean * elo_df$elo_old + (1 - regress_to_mean) * 1500
  
  # Sort by Elo (descending)
  new_year_elo <- elo_df %>% arrange(desc(elo)) %>% select(team, elo)
  
  return(new_year_elo)
}

# Example use:
init_elo2 <- initialize_elo()

update_elo_ratings <- function(regress = 0.75, start_date = "2025-03-15", end_date = Sys.Date(), k = 10) {
  date_seq <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  
  elo_ratings <- tibble(initialize_elo(regress))
  elo_history <- list()
  previous_elo <- elo_ratings
  
  # Initialize team records to 0-0 for all teams
  team_records <- tibble(team = elo_ratings$team, wins = 0, losses = 0)
  
  for (current_date in date_seq) {
    current_date <- format(as.Date(current_date), "%Y-%m-%d")
    games <- ParserGameByDate(current_date)
    
    if (is.null(games)) {
      #message("No games found on ", current_date, "; skipping.")
      next
    }
    
    valid_names <- names(games)
    valid_names <- valid_names[!is.na(valid_names) & valid_names != ""]
    games <- games %>% select(all_of(valid_names))
    
    games <- games %>%
      filter(dates_games_seriesDescription == "Regular Season") %>%
      filter(dates_games_status_detailedState == "Final")
    
    if (nrow(games) == 0) {
      #message("No *finished* regular season games on ", current_date, "; skipping.")
      next
    }
    
    games <- games %>%
      filter(!is.na(dates_games_teams_home_score), !is.na(dates_games_teams_away_score)) %>%
      mutate(
        home_team = dates_games_teams_home_team_name,
        away_team = dates_games_teams_away_team_name,
        home_score = as.numeric(dates_games_teams_home_score),
        away_score = as.numeric(dates_games_teams_away_score)
      )
    
    if (nrow(games) == 0) {
      #message("No *finished* regular season games on ", current_date, "; skipping.")
      next
    }
    
    for (i in seq_len(nrow(games))) {
      game <- games[i, ]
      
      home <- game$home_team
      away <- game$away_team
      home_win <- as.numeric(game$dates_games_teams_home_isWinner == 'TRUE')
      away_win <- as.numeric(game$dates_games_teams_away_isWinner == 'TRUE')
      
      if ((home %in% elo_ratings$team) & (away %in% elo_ratings$team)) {
        home_elo <- elo_ratings$elo[elo_ratings$team == home]
        away_elo <- elo_ratings$elo[elo_ratings$team == away]
        
        new_home_elo <- update_elo(home_elo, away_elo, home_win, k)
        new_away_elo <- update_elo(away_elo, home_elo, away_win, k)
        
        elo_ratings$elo[elo_ratings$team == home] <- new_home_elo
        elo_ratings$elo[elo_ratings$team == away] <- new_away_elo
      } else {
        print(glue::glue("One or both teams ({home}, {away}) are not found in elo_ratings"))
      }
    }
    
    # Round Elo ratings
    elo_ratings$elo <- round(elo_ratings$elo, 1)
    
    # Create record_df for the teams that played today
    record_df <- tibble(
      team = c(games$home_team, games$away_team),
      wins = c(as.integer(games$dates_games_teams_home_leagueRecord_wins),
               as.integer(games$dates_games_teams_away_leagueRecord_wins)),
      losses = c(as.integer(games$dates_games_teams_home_leagueRecord_losses),
                 as.integer(games$dates_games_teams_away_leagueRecord_losses))
    ) %>%
      distinct(team, .keep_all = TRUE)
    
    # Update the master record
    team_records <- team_records %>%
      rows_update(record_df, by = "team")
    
    # Join today's Elo snapshot with previous day's and updated record
    daily_elo <- elo_ratings %>%
      left_join(previous_elo, by = "team", suffix = c("", "_prev")) %>%
      mutate(
        date = current_date,
        delta_elo = elo - elo_prev
      ) %>%
      left_join(team_records, by = "team") %>%
      arrange(desc(elo)) %>%
      select("Team" = team, "Elo Rating" = elo, "Daily Elo Change" = delta_elo, 
             "Wins" = wins, "Losses" = losses)
    
    elo_history[[as.character(current_date)]] <- daily_elo
    previous_elo <- elo_ratings
  }
  
  latest_day <- names(elo_history)[length(elo_history)]
  return(elo_history[[latest_day]])
}




elo_results <- update_elo_ratings(regress = 0.75, k=5)
elo_results

elo_results2 <- update_elo_ratings(regress = 0.25, end_date = "2025-03-30")

install.packages("mlbplotR")
library(mlbplotR)
team_logos <- load_mlb_teams() %>% select(team_name, team_logo_espn)

elo_today <- elo_results %>%
  mutate(Team = ifelse(Team == "Athletics", "Oakland Athletics", Team))

elo_with_logos <- elo_today %>%
  left_join(team_logos, by = c("Team" = "team_name")) %>%
  mutate(elo_rank = min_rank(desc(`Elo Rating`))) %>%
  select("Rank" = elo_rank, "Logo" = team_logo_espn, Team, `Elo Rating`, 
         `Daily Elo Change`, Wins, Losses) %>%
  mutate(Team = ifelse(Team == "Oakland Athletics","Athletics", Team))

library(gt)

elo_with_logos %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(Logo)),
    fn = function(x) {
      web_image(url = x, height = 40)
    }
  ) %>%
  tab_header(
    title = "MLB Elo Ratings"
  )

