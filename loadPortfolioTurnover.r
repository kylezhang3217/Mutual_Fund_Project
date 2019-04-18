#pulls corresponding SEC Mutual Fund Return Data for that Year/Quarter and loads portfolio turnover graph
loadPortfolioTurnover <- function(year, quarter) {
  library(readr)
  library(dplyr)
  library(stringr)
  
  yearPath <- paste(toString(year), toString("q"), toString(quarter), toString("_rr1"), sep="", collapse=NULL)
  subPath <- paste(yearPath, toString("/sub.tsv"), sep="", collapse=NULL)
  numPath <- paste(yearPath, toString("/num.tsv"), sep="", collapse=NULL)
  
  sub_long <- read_delim(subPath, "\t", escape_double = FALSE, trim_ws = TRUE)
  num_long <- read_delim(numPath, "\t", escape_double = FALSE, trim_ws = TRUE)
  
  #select only relevant columns in our dataframe
  sub <- dplyr::select(sub_long, adsh, name)
  num <- dplyr::select(num_long, adsh, tag, measure, value, series, class)
  
  #computes dataframe of mutual fund companies and their corresponding portfolio turnover rate
  portfolio_turnover <- dplyr::filter(num, str_detect(tag, "Portfolio"))
  trial <- left_join(portfolio_turnover, sub, by = "adsh")
  portfolio_turnover_table <- select(trial, name, tag, value, series, class)
}