#pulls corresponding SEC Mutual Fund Return Data for that Year/Quarter and loads graphs
loadMFGraph <- function(year, quarter) {
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
  
  #computes index comparison
  index_comparison <- dplyr::filter(num, str_detect(measure, "Index") | str_detect(measure, "index"))
  index_long_term <- dplyr::filter(index_comparison, str_detect(tag, "Inception") 
                                   | str_detect(tag, "05") 
                                   | str_detect(tag, "10"))
  
  after_comparison <- dplyr::filter(num, (str_detect(measure, "After") | str_detect(measure, "after"))
                                      &  (str_detect(measure, "Sale") | str_detect(measure, "sale")))
  after_long_term <- dplyr::filter(after_comparison, str_detect(tag, "Inception") 
                                    | str_detect(tag, "05") 
                                    | str_detect(tag, "10"))
  
  final_graph <- left_join(after_long_term, index_long_term, by = c("adsh", "series", "tag"))
  final_graph1 <- select(final_graph, adsh, tag, value.x, value.y, measure.y, series, class.x)
  final_graph1 <- rename(final_graph1, "Comparison_Term" = tag, "Portfolio_Return" = value.x, 
                                      "Benchmark_Return" = value.y, "Benchmark" = measure.y,
                                      "Class" = class.x)
  ultimate_graph1 <- left_join(final_graph1, sub, by="adsh")
  ultimate_graph <- select(ultimate_graph1, name, Comparison_Term, Portfolio_Return, Benchmark_Return,
                           Benchmark, series, Class) %>% rename("Name" = name, "Series" = series)
  return(ultimate_graph)
 }