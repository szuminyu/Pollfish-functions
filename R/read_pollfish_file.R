#' Reading a pollfish file
#'
#' Function that reads a Pollfish file and automatically creates the region and income variables
#' @param enter_file: .xlsx file with the data
#' @keywords Pollfish
#' @export
#' @examples
#' read_pollfish_file("Pollfish_Survey.xls")



read_pollfish_file <- function(enter_file){
  
  ##This sheet contains income
  x <- readxl::read_excel(enter_file, sheet = "Individuals") %>%
    dplyr::select(ID,`Income`) %>%
    magrittr::set_colnames(c("ID","income"))
  
  ##Fix income variable
  x <- x %>% 
    dplyr::mutate(income = case_when(
                  income %in% c("prefer_not_to_say") ~ NA_character_,
                  income %in% c("lower_i", "lower_ii") ~ "Under 50K",
                  TRUE ~ "Over 50K"))
  
  ##This sheet contains regions
  library(noncensus)
  data('states')
  regions <- states[, c('name','region')]
  
  ## Add Southwest category
  regions$region = as.character(regions$region)
  regions$region[regions$name == 'Oklahoma'] = 'Southwest'
  regions$region[regions$name == 'Texas'] = 'Southwest'
  regions$region[regions$name == 'New Mexico'] = 'Southwest'
  regions$region[regions$name == 'Arizona'] = 'Southwest'
  
  
  #Now we add merge the sheet with income and then with region
  y <- readxl::read_excel(enter_file, sheet = "Individuals Coded") %>%
    dplyr::inner_join(x, by = c("ID" = "ID")) %>%
    dplyr::left_join(regions, by = c("Area" = "name"))
  
  ##Order region, income
  y <- y %>% 
    dplyr::mutate(income = factor(income, levels = c("Under 50K", "Over 50K")),
           region = factor(region, levels = c("South", "Northeast", "Midwest", "West", "Southwest")))
  
  ##Fix age and gender
  y <- y %>% 
    dplyr::rename(age = Age,
                  gender = Gender) %>%
    dplyr::mutate(age = case_when(
      age == "> 54" ~ "Over 54",
      TRUE ~ age
    )) %>%
    dplyr::mutate(age = factor(age, levels = c("18 - 24", "25 - 34","35 - 44","45 - 54", "Over 54"))) %>%
    dplyr::mutate(gender = str_to_title(gender))
    
  y <- y %>%
    dplyr::mutate_at(vars(matches("Q[1-9]{1,2}\\.*")),parse_number)

  y <- y %>% 
    dplyr::mutate(sample = "Sample")
    
  return(y)

}
