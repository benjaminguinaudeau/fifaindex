
#' @export
get_all_league_ids <- function(potential_ids = 1:5000){
  
  all_ids <- tibble::tibble(potential_ids) %>%
    #slice(1:20) %>%
    dplyr::mutate(url = glue::glue("https://www.fifaindex.com/teams/fifa19_279/?league={ potential_ids }")) %>%
    dplyr::mutate(exists = url %>% furrr::future_map_lgl(~{
      page <- xml2::read_html(.x)
      
      exists <- page %>%
        rvest::html_node(".invalid-feedback") %>%
        length %>%
        magrittr::equals(0)
      
      return(exists)
    }, .progress = T)) %>%
    dplyr::select(-url)
  
  return(all_ids)
}

#' @export
get_league_data <- function(url){
  page <- xml2::read_html(url)
  
  period_date <- page %>%
    rvest::html_nodes(".dropdown-toggle") %>%
    rvest::html_text(.) %>%
    purrr::keep(stringr::str_detect,"20\\d{2}") %>%
    stringr::str_trim(.) %>%
    parse_date
  
  league_data <- page %>%
    rvest::html_table() %>%
    .[[1]] %>%
    janitor::clean_names(.)
  
  team_link <- page %>%
    rvest::html_nodes("tr") %>%
    tail(-1) %>%
    purrr::map_chr(~{
      .x %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") %>%
        purrr::keep(~stringr::str_detect(.x, "/team/\\d+")) %>%
        unique
    })
  
  team_id <- team_link %>%
    stringr::str_extract("(?<=/team/)\\d+") %>%
    as.numeric()
  
  out <- league_data %>%
    dplyr::mutate(period_date = period_date, 
           team_link = team_link, 
           team_id = team_id, 
           url = url)
}

get_league <- function(league_id, year = NULL){
  periods <- get_period() %>%
    dplyr::filter(year %in% year) %>%
    dplyr::pull(period)
  
  urls <- glue::glue("https://www.fifaindex.com/teams/fifa16_{ periods }/?league={ league_id }") %>%
    as.character()
  
  out <- urls %>%
    purrr::map_dfr(~{
      if(rvest::html_session(.x)$response$status_code == 404) return(NULL)
      return(get_league_data(.x))
    })
  
  return(out)
}
