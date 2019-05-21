#' @export
get_stats <- function(cards){
  headers <- cards %>%
    purrr::map_chr(get_header) 
  
  stat_cards <- cards %>%
    .[is_stat_card(headers)]
  
  stat_headers<- headers[is_stat_card(headers)]
  
  out <- purrr::map2_dfc(stat_cards, stat_headers, ~get_stats_card(.x, .y))
  
  return(out)
  
}

#' @export
get_stats_card <- function(card, header){
  stat <- card %>%
    rvest::html_node(".card-body") %>%
    rvest::html_nodes("p") %>%
    purrr::map_chr(rvest::html_text)
  
  if(stringr::str_detect(stat, "\\s+\\d+$")[1]){
    name <- stat %>%
      stringr::str_remove("\\s+\\d+$") %>%
      paste(header, ., sep = "_")
    
    value <- stat %>%
      stringr::str_extract("\\d+$")
  } else {
    name <- header
    value <- stat %>%
      paste(collapse = "|")
  }
  
  out <- tibble::tibble(name, value) %>%
    #rowid_to_column() %>%
    tidyr::spread(name, value) %>%
    janitor::clean_names(.)
  
  return(out)
}

#' @export
parse_meta <- function(p){
  if(length(rvest::html_node(p,".star")) != 0) return(NULL)
  
  value <- p %>%
    rvest::html_node("span") %>%
    rvest::html_text(.)
  
  
  name <- p %>%
    rvest::html_text(.) %>%
    stringr::str_remove(stringr::fixed(value)) %>%
    stringr::str_trim(.)
  
  out <- tibble::tibble(name, value) %>%
    tidyr::spread(name, value) %>%
    janitor::clean_names(.)
  
  return(out)
}

#' @export
get_data <- function(url){
  
  page <- xml2::read_html(url)
  
  period_date <- page %>%
    rvest::html_nodes(".dropdown-toggle") %>%
    rvest::html_text(.) %>%
    purrr::keep(stringr::str_detect,"20\\d{2}") %>%
    stringr::str_trim(.) %>%
    parse_date %>%
    tibble::tibble(date = .)
  
  cards <- page %>%
    rvest::html_nodes(".card.mb-5")
  
  meta <- cards[[2]] %>%
    rvest::html_node(".card-body") %>%
    rvest::html_nodes("p") %>%
    purrr::map_dfc(parse_meta)
  
  team <- cards[[3]] %>%
    rvest::html_node(".card-body") %>%
    rvest::html_nodes("p") %>%
    purrr::map_dfc(parse_meta)
  
  team_name <- get_header(cards[[3]]) %>%
    tibble::tibble(team = .)
  
  stats <- get_stats(cards)
  
  out <- dplyr::bind_cols(meta, period_date, team_name, team, stats)
  
  return(out)
}

#' @export
get_player_link <- function(player_id){
  url <- glue::glue("https://www.fifaindex.com/player/{ player_id }/")
  
  out <- xml2::read_html(url) %>%
    rvest::html_nodes(".dropdown-item") %>%
    rvest::html_attr("href") %>%
    purrr::keep(stringr::str_detect, "^/player/") %>%
    paste0("https://www.fifaindex.com", .)
  
  return(out)
}

#' @export
get_player_stats <- function(player_id, year = NULL){
  urls <- get_player_link(player_id)
  
  if(!is.null(year)){
    periods <- get_period() %>%
      dplyr::filter(year %in% as.numeric(!!year)) %>%
      dplyr::pull(period)
  } else {
    periods <- get_period() %>%
      dplyr::pull(period)
  }
  
  
  period <- urls %>% 
    stringr::str_extract("\\d+(?=/$)") 
  
  message(glue::glue("Player: {player_id} \t|  {sum(period %in% periods)} years\n"))
  
  pb <- dplyr::progress_estimated(length(period[!is.na(period)]))
  
  out <- tibble::tibble(urls, period, player_id) %>%
    dplyr::filter(period %in% periods) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate(data = urls %>% purrr::map(get_data)) %>%
    tidyr::unnest(data)
  
  return(out)
}