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
parse_rating <- function(p){
  if(length(rvest::html_node(p,".star")) != 0) return(NULL)
  
  value  <- p %>%
    rvest::html_node(".rating") %>%
    rvest::html_text(.)
  
  junk <- p %>%
    xml2::xml_children() %>%
    #rvest::html_nodes("span") %>%
    rvest::html_text(.) %>%
    paste(collapse = " ")
  
  
  name <- p %>%
    rvest::html_text(.) %>%
    paste(collapse = " ") %>%
    stringr::str_remove(stringr::fixed(junk)) %>%
    stringr::str_trim(.)
  
  out <- tibble::tibble(name, value) %>%
    tidyr::spread(name, value) %>%
    janitor::clean_names(.)
  
  return(out)
}

#' @export
get_stats_card <- function(card, header){
  stat <- card %>%
    rvest::html_node(".card-body") %>%
    rvest::html_nodes("p") %>%
    purrr::map_chr(rvest::html_text)
  
  if(stringr::str_detect(stat, "\\s+\\d+$")[1]){
    p <- card %>%
      rvest::html_node(".card-body") %>%
      rvest::html_nodes("p")
    
    out <- p %>% 
      purrr::map_dfc(parse_rating) %>%
      dplyr::rename_all(~{paste(header, .x, sep = "_")}) %>%
      janitor::clean_names(.)
    
  } else {
    name <- header
    value <- stat %>%
      paste(collapse = "|")
    
    out <- tibble::tibble(name, value) %>%
      #rowid_to_column() %>%
      tidyr::spread(name, value) %>%
      janitor::clean_names(.)
    
  }
  
  return(out)
}

#' @export
parse_meta <- function(p){
  if(length(rvest::html_node(p,".star")) != 0) return(NULL)
  
  value <- p %>%
    xml2::xml_children() %>%
    #rvest::html_nodes("span") %>%
    rvest::html_text(.) %>%
    paste(collapse = " ") %>%
    stringr::str_trim(.)
  
  
  name <- p %>%
    rvest::html_text(.) %>%
    paste(collapse = " ") %>%
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
  
  player_name <- page %>%
    rvest::html_nodes("h1") %>%
    rvest::html_text(.) %>%
    tibble::tibble(player_name = .)
  

  country_div <- page %>%
    rvest::html_nodes("h2") %>%
    map(rvest::html_nodes, "a") %>%
    purrr::compact(.) %>%
    .[[1]]
    
  country <- country_div %>%
    rvest::html_text(.) %>%
    purrr::discard(~.x == "") %>%
    tibble::tibble(country = .)
  
  country_id <- country_div %>%
    rvest::html_attr("href") %>%
    unique %>%
    stringr::str_extract("\\d+$") %>%
    tibble::tibble(country_id = .)
  
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
  
  ratings <- cards[[2]] %>%
    rvest::html_nodes(".rating") %>%
    rvest::html_text(.) %>%
    map2_dfc(c("overall_rating", "potential_rating"),~{
      #print(.x)
      tibble::tibble(.x) %>% purrr::set_names(.y)
      }) 

  team <- cards[[3]] %>%
    rvest::html_node(".card-body") %>%
    rvest::html_nodes("p") %>%
    purrr::map_dfc(parse_meta)
  
  team_link <- cards[[3]] %>%
    rvest::html_node(".card-header") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    unique %>%
    tibble::tibble(team_link = .)
  if(nrow(team_link) == 0) team_link <- tibble::tibble(team_link = NA)
  
  team_name <- get_header(cards[[3]]) %>%
    tibble::tibble(team = .)
  
  stats <- get_stats(cards)
  
  out <- dplyr::bind_cols(player_name, ratings, country, country_id, meta, period_date, team_name, team_link, team, stats)
  
  return(out)
}

#' @export
get_player_link <- function(player_id){
  url <- glue::glue("https://www.fifaindex.com/player/{ player_id }/")
  
  if(rvest::html_session(url)$response$status_code == 404) return(NULL)
    
  out <- xml2::read_html(url) %>%
    rvest::html_nodes(".dropdown-item") %>%
    rvest::html_attr("href") %>%
    purrr::keep(stringr::str_detect, "^/player/") %>%
    paste0("https://www.fifaindex.com", .)
  
  return(out)
}

#' @export
get_player_stats <- function(player_id, year = NULL){
  urls <- suppressWarnings(get_player_link(player_id))
  
  if(is.null(urls)){
    message(glue::glue("Player: {player_id} \t|  0 years\n"))
    return(tibble::tibble(player_id, period = NA))
  }
  
  
  if(!is.null(year)){
    year_to_include <- as.numeric(year)
    periods <- get_period() %>%
      dplyr::filter(year %in% year_to_include) %>%
      dplyr::pull(period)
  } else {
    periods <- get_period() %>%
      dplyr::pull(period)
  }
  
  period <- urls %>% 
    stringr::str_extract("\\d+(?=/$)") 
  
  message(glue::glue("Player: {player_id} \t|  {sum(period %in% periods)} years\n"))
  
  get_data_pos <- purrr::possibly(get_data, otherwise = tibble::tibble(team = NA))
  
  out <- tibble::tibble(urls, period, player_id) %>%
    dplyr::filter(period %in% periods) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate(data = urls %>% purrr::map(get_data))
  
  return(out)
}
 