# =============================================================================
# 01_load_clean.R
# Data loading and missing value handling for the Video Game Sales analysis.
# Feature engineering and modeling to be completed in session.
# =============================================================================

library(tidyverse)
library(janitor)

# ── Constants -----------------------------------------------------------------

DATA_PATH <- here::here("data", "vgsales.csv")

KNOWN_GENRES <- c(
  "Action", "Adventure", "Fighting", "Misc", "Platform",
  "Puzzle", "Racing", "Role-Playing", "Shooter",
  "Simulation", "Sports", "Strategy"
)

STANDARD_RATINGS <- c("E", "E10+", "T", "M", "AO", "RP")


# ── Load ----------------------------------------------------------------------

load_raw <- function(path = DATA_PATH) {
  message("[load] Reading: ", path)
  df <- read_csv(
    path,
    col_types = cols(
      Name             = col_character(),
      Platform         = col_character(),
      Year_of_Release  = col_character(),   # read as char; coerce later
      Genre            = col_character(),
      Publisher        = col_character(),
      NA_Sales         = col_double(),
      EU_Sales         = col_double(),
      JP_Sales         = col_double(),
      Other_Sales      = col_double(),
      Global_Sales     = col_double(),
      Critic_Score     = col_double(),
      Critic_Count     = col_double(),
      User_Score       = col_character(),   # contains "tbd"
      User_Count       = col_double(),
      Developer        = col_character(),
      Rating           = col_character()
    ),
    na = c("", "NA", "N/A")
  )
  message("[load] Raw shape: ", nrow(df), " rows x ", ncol(df), " cols")
  df
}


# ── Column summary helper -----------------------------------------------------

col_summary <- function(df) {
  tibble(
    column   = names(df),
    dtype    = sapply(df, class),
    non_null = sapply(df, function(x) sum(!is.na(x))),
    null_n   = sapply(df, function(x) sum(is.na(x))),
    null_pct = round(sapply(df, function(x) mean(is.na(x))) * 100, 2)
  )
}


# ── Cleaning steps ------------------------------------------------------------

# Remove rows where Global_Sales is missing or zero (can't model without a target)
clean_target <- function(df, target = "Global_Sales") {
  before <- nrow(df)
  df <- df %>% filter(!is.na(.data[[target]]), .data[[target]] > 0)
  message("[clean] Target: removed ", before - nrow(df),
          " rows with missing/zero ", target)
  df
}

# Coerce Year_of_Release to integer; drop implausible values
clean_year <- function(df) {
  before <- nrow(df)
  df <- df %>%
    mutate(Year_of_Release = as.integer(Year_of_Release)) %>%
    filter(!is.na(Year_of_Release), between(Year_of_Release, 1980, 2020))
  message("[clean] Year: removed ", before - nrow(df), " rows with bad/missing year")
  df
}

# Drop rows with unrecognised or missing genres
clean_genre <- function(df) {
  before <- nrow(df)
  df <- df %>% filter(Genre %in% KNOWN_GENRES)
  message("[clean] Genre: removed ", before - nrow(df), " rows with unknown genre")
  df
}

# Coerce User_Score: replace "tbd" with NA, convert to numeric
clean_user_score <- function(df) {
  df %>%
    mutate(
      User_Score = na_if(User_Score, "tbd"),
      User_Score = as.numeric(User_Score)
    )
}

# Standardise ESRB Rating: collapse rare values to "Other", fill NA as "Unknown"
clean_rating <- function(df) {
  df %>%
    mutate(
      Rating = if_else(Rating %in% STANDARD_RATINGS, Rating, "Other"),
      Rating = replace_na(Rating, "Unknown"),
      Rating = factor(Rating, levels = c("E", "E10+", "T", "M", "AO", "RP", "Other", "Unknown"))
    )
}


# ── Pipeline (stops after missing value handling) ----------------------------

#' Load and clean the dataset through the missing value stage.
#' Feature engineering to be added in session.
#'
#' @param path Path to raw CSV.
#' @return Cleaned tibble with missing values handled.
run_pipeline <- function(path = DATA_PATH) {
  load_raw(path) %>%
    clean_target() %>%
    clean_year() %>%
    clean_genre() %>%
    clean_user_score() %>%
    clean_rating()
}


# ── Run standalone ------------------------------------------------------------
if (sys.nframe() == 0) {
  df <- run_pipeline()
  print(glimpse(df))
  print(col_summary(df))
}
