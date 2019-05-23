longest <- function(char){
  char %>%
    purrr::map_chr(~{
      .x %>%
        stringr::str_split(" ") %>%
        .[[1]] %>%
        tibble::tibble(x = .) %>%
        dplyr::mutate(len = stringr::str_length(x)) %>%
        dplyr::arrange(-len) %>%
        dplyr::slice(1) %>%
        dplyr::pull(x)
    })
}

fifa_monks_dict <- tibble::tribble(
  ~fifa_id, ~monks_id,
  1, 271,
  4, 208,
  7, NA,
  10, 72,
  13, 8,
  14, 9, 
  16, 301,
  17, NA,
  19, 82,
  20,NA,
  31, 384,
  32, NA,
  39, NA,
  41, 444,
  50, 501,
  53, 564,
  54, NA,
  56, 573,
  60, 12,
  61, NA,
  65, NA,
  66, NA,
  68, 600,
  76, NA,
  80, 181,
  83, NA,
  189, NA,
  308, 462,
  335, NA,
  336,NA,
  341, NA,
  349, NA,
  350, NA,
  351, NA,
  353, NA,
  382,  NA,
  2012, NA,
  2076, NA
)

fix_team_names <- function(name, league_id){
  dplyr::case_when(
    name == "Manchester Utd" ~ "Manchester United",
    name == "Sheffield Utd" ~ "Sheffield United",
    name == "Sheffield Wed" ~ "Sheffield Wednesday",
    name == "West Ham United" ~ "West Ham United", #14
    name == "West Ham United" ~ "West Ham United", #14
    name == "Nîmes Olympique" ~ "Nîmes", #16
    name == "Olympique Lyon" ~ "Olympique Lyonnais", #16
    name == "Olympique de Marseille" ~ "Olympique Marseille", # 16
    name == "RC Deportivo" ~ "Deportivo La Coruña", #53
    name == "R.C. Deportivo La Coruna" ~ "Deportivo La Coruña", #53
    name == "Vitória SC" ~ "Vitória Guimarães", #308
    name == "Sporting Lisbon" ~ "Sporting CP", #308
    name == "Clube Desportivo Nacional" ~ "Nacional", #308
    
    
    name == 'Odense BK' & league_id == 1 ~ 'OB',
    name == 'Aalborg BK' & league_id == 1 ~ 'AaB',
    name == 'Aarhus GF' & league_id == 1 ~ 'AGF',
    name == 'Vejle Boldklub' & league_id == 1 ~ 'Vejle',
    name == 'FC Vestsjælland' & league_id == 1 ~ 'Vestsjaelland',
    name == 'Odense Boldklub' & league_id == 1 ~ 'OB',
    
    name == 'Waasl. Beveren' & league_id == 4 ~ 'Waasland-Beveren',
    name == 'Waasl.-Beveren' & league_id == 4 ~ 'Waasland-Beveren',
    
    name == 'Sparta R\'dam' & league_id == 10 ~ 'Sparta Rotterdam',
    name == 'Roda JC' & league_id == 10 ~ 'Roda JC Kerkrade',
    name == 'N.E.C.' & league_id == 10 ~ 'NEC',
    
    name == 'Spurs' & league_id == 13 ~ 'Tottenham Hotspur',
    name == 'West Ham' & league_id == 13 ~ 'West Ham United',
    name == 'Wolves' & league_id == 13 ~ 'Wolverhampton Wanderers',
    name == 'West Brom' & league_id == 13 ~ 'West Bromwich Albion',
    name == 'QPR' & league_id == 13 ~ 'Queens Park Rangers',
    
    name == 'West Brom' & league_id == 14 ~ 'West Bromwich Albion',
    name == 'Nott\'m Forest' & league_id == 14 ~ 'Nottingham Forest',
    name == 'QPR' & league_id == 14 ~ 'Queens Park Rangers',
    name == 'Bolton' & league_id == 14 ~ 'Bolton Wanderers',
    name == 'Wolves' & league_id == 14 ~ 'Wolverhampton Wanderers',
    name == 'MK Dons' & league_id == 14 ~ '',
    
    name == 'Paris' & league_id == 16 ~ 'PSG',
    name == 'OL' & league_id == 16 ~ 'Olympique Lyonnais',
    name == 'OM' & league_id == 16 ~ 'Olympique Marseille',
    name == 'ASSE' & league_id == 16 ~ 'Saint-Étienne',
    name == 'Stade Rennais' & league_id == 16 ~ 'Rennes',
    name == 'Girondins de Bx' & league_id == 16 ~ 'Bordeaux',
    name == 'Stade de Reims' & league_id == 16 ~ 'Reims',
    name == 'Olym. Lyonnais' & league_id == 16 ~ 'Olympique Lyonnais',
    name == 'Olym. Marseille' & league_id == 16 ~ 'Olympique Marseille',
    name == 'GFC Ajaccio' & league_id == 16 ~ 'Gazélec Ajaccio',
    name == 'Marseille' & league_id == 16 ~ 'Olympique Marseille',
    name == 'Saint-Etienne' & league_id == 16 ~ 'Saint-Étienne',
    name == 'Stade Reims' & league_id == 16 ~ 'Reims',
    name == 'Evian Thonon FC' & league_id == 16 ~ 'Evian TG',
    
    name == 'FC Bayern' & league_id == 19 ~ 'Bayern München',
    name == 'Dortmund' & league_id == 19 ~ 'Borussia Dortmund',
    name == 'Frankfurt' & league_id == 19 ~ 'Eintracht Frankfurt',
    name == 'Bor. Dortmund' & league_id == 19 ~ 'Borussia Dortmund',
    name == 'Bayer 04' & league_id == 19 ~ 'Bayer Leverkusen',
    name == 'Eint. Frankfurt' & league_id == 19 ~ 'Eintracht Frankfurt',
    
    name == 'Spal' & league_id == 31 ~ 'SPAL', 
    
    name == 'Odds BK' & league_id == 41 ~ 'Odd',
    name == 'Stabæk Fotball' & league_id == 41 ~ 'Stabæk',
    name == 'FK Bodø/Glimt' & league_id == 41 ~ 'Bodø / Glimt',
    name == 'Aalesunds FK' & league_id == 41 ~ 'Aalesund',
    name == 'ODD' & league_id == 41 ~ 'Odd',
    name == 'Sandnes Ulf' & league_id == 41 ~ '',
    
    name == 'Hamilton' & league_id == 50 ~ 'Hamilton Academical',
    
    name == 'Djurgårdens IF' & league_id == 56 ~ 'Djurgården',
    name == 'Trelleborgs FF' & league_id == 56 ~ 'Trelleborg',
    name == 'Halmstads BK' & league_id == 56 ~ 'Halmstad',
    name == 'Helsingborgs IF' & league_id == 56 ~ 'Helsingborg',
    name == 'Falkenbergs FF' & league_id == 56 ~ 'Falkenberg',
    name == 'Åtvidabergs FF' & league_id == 56 ~ 'Åtvidaberg',
    name == 'AIK Fotboll' & league_id == 56 ~ 'AIK',
    name == 'Mjällby AIF' & league_id == 56 ~ '',
    
    name == 'D. Alavés' & league_id == 53 ~ 'Deportivo Alavés',
    name == 'Deport. Alavés' & league_id == 53 ~ 'Deportivo Alavés',
    
    name == 'SCR Altach' & league_id == 80 ~ 'Rheindorf Altach',
    name == "SK Austria Kärnten" & league_id == 80 ~ "", #80
    
    name == "Sheffield Wednesday" & league_id == 60 ~ "", #60
    name == 'Wycombe' & league_id == 60 ~ 'Wycombe Wanderers',
    name == 'MK Dons' & league_id == 60 ~ 'Milton Keynes Dons',
    name == 'Bolton' & league_id == 60 ~ 'Bolton Wanderers',
    
    name == 'Kasimpaşa' & league_id == 68 ~ 'Kasımpaşa',
    name == 'Kasimpaşa SK' & league_id == 68 ~ 'Kasımpaşa',
    name == 'Akhisarspor' & league_id == 68 ~ 'Akhisar Belediyespor',
    name == 'Mersin' & league_id == 68 ~ 'Mersin İdmanyurdu',
    
    name == "F. Santa Maria" & league_id == 308 ~ "",	#308
    name == "União Desportivo de Leiria" & league_id == 308 ~ "",
    name == 'SC Braga' & league_id == 308 ~ 'Sporting Braga',
    name == 'V. Setúbal' & league_id == 308 ~ 'Vitória Setúbal',
    name == 'Funchal' & league_id == 308 ~ 'Nacional',
    name == 'CD Aves' & league_id == 308 ~ 'Desportivo Aves',
    name == 'Portimão' & league_id == 308 ~ 'Portimonense',
    name == 'Estoril-Praia' & league_id == 308 ~ 'Estoril',
    T ~ name
  )
}
