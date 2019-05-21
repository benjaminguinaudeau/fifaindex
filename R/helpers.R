#' @export
parse_date <- function(x){
  mth_a <- stringr::str_extract(x, "^\\w+") 
  month <- dplyr::case_when(
    mth_a == "Jan" ~ "January", 
    mth_a == "Feb" ~ "February",
    mth_a == "Aug" ~ "August",
    mth_a == "Sept"~ "September",
    mth_a == "Oct" ~ "October",
    mth_a == "Nov" ~ "November",
    mth_a == "Dec" ~ "December", 
    T ~ mth_a
  )
  day <- stringr::str_extract(x, "(?<=.\\s{1,3})\\d+")
  year <- stringr::str_extract(x, "\\d+$")
  
  date <- lubridate::as_date(paste(year, month, day, sep = "-"))
  
  return(date)
}

#' @export
get_header <- function(card){
  card %>%
    rvest::html_node(".card-header") %>%
    rvest::html_text(.)
}

#' @export
is_stat_card <- function(x){
  start <- x %>%
    tolower %>%
    stringr::str_detect("ball\\sskills") %>%
    which %>%
    .[1]
  
  return(start:length(x))
}

#' @export
get_period <- function(){
  tibble::tribble(
  ~year, ~period,
  2019, 336,
  2018, 278,
  2017, 173,
  2016, 73,
  2015, 14,
  2014, 13,
  2013, 10,
  2012, 11,
  2011, 8,
  2010, 7,
  2009, 6,
  2008, 5,
  2007, 4,
  2006, 3,
  2005, 2, 
  2004, 1
) 
}
