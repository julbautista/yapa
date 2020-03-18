library(rvest)
library(tidyverse)

# function to calculate numeric difference in two dates in days.
date_diff <- function(t1, t2) {
  round(
    as.numeric(
      difftime(t1, t2, unit = 'days')
    )
  )
}

# Function to process state-level polls from RCP for the 2020 presidential election.
# Parameters:
## site: character string to web address of rcp site;
## election day: character string of election day, used for weighting polls;
## wt_function: function for weighting polls.  Default is 1/sqrt(days from election).
process_rcp <- function(site, election_day = "2020-11-06",
                        wt_function = function(days_out) 1/sqrt(days_out),
                        n = Inf) {
  
  # If site is NULL, assume there is no data
  if(is.null(site)) {
    polls <- data_frame(
      Sample = 0, `Biden (D)` = 0, `Trump (R)` = 0, Other = 0
    )
  } else {
    
    # Read html from site
    poll_tbl <- site %>%
      read_html() %>%
      html_table() 
    
    # Process data: format sample and date; drop unnecessary rows and columns; weight.
    raw_data <- poll_tbl %>%
      .[[length(poll_tbl)]] %>%
      filter(!Poll %in% c("RCP Average", "Final Results")) %>%
      mutate(Sample = as.numeric(gsub("([0-9]+).*$", "\\1", Sample))) %>%
      mutate(end_date = sapply(strsplit(Date, " - "), tail, 1),
             end_date = as.Date(paste0(end_date, "/2020"), "%m/%d/%Y"),
             end_date = if_else(end_date > Sys.Date(), as.Date(gsub("2020", "2019", end_date)),
                                end_date)) %>%
      mutate_if(is.character, function(x) gsub("--", "0", x)) %>%
      mutate(days_out = date_diff(election_day, end_date)) %>%
      mutate(wt = wt_function(days_out),
             Sample = round(Sample*wt)) %>%
      select(-Poll, -Date, -Spread, -days_out, -wt) %>%
      mutate_if(is.character, as.numeric)  %>%
      na.omit() %>%
      filter(Sample < Inf) %>%
      head(n = n)
    
    # Drop MoE column if exists (not in all for some reason)
    if(any(names(raw_data) == "MoE")) raw_data <- select(raw_data, -MoE)
    
    # Process raw data into weighted counts.  Account for "other"
    polls <- raw_data 
    for(n in names(polls)[!names(polls) %in% c("Sample", "end_date")]) {
      polls[, n] <- round(polls[, 1]*polls[, n]/100)
    }
    polls$Other <- polls$Sample - apply(polls[!names(polls) %in% c("Sample", "end_date")], 1, sum)
    polls$Other <- if_else(polls$Other < 0, 0, polls$Other)
  }
  
  polls
  
}

