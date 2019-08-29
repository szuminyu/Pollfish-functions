#' Reading a pollfish file
#'
#' Function that reads a Pollfish file and automatically creates the region and income variables
#' @param enter_file: .xlsx file with the data
#' @keywords Pollfish
#' @export
#' @examples
#' read_pollfish_file("Pollfish_Survey.xls")



read_pollfish_file = function(enter_file){
  
  ##This sheet contains income
  individuals = readxl::read_excel(enter_file, sheet = "Individuals") %>%
    dplyr::select(ID,`Income`) %>%
    magrittr::set_colnames(c("ID","income"))
  
  ##Fix income variable
  individuals = individuals %>% 
    dplyr::mutate(income = case_when(
      income %in% c("prefer_not_to_say") ~ NA_character_,
      income %in% c("lower_i", "lower_ii") ~ "Under 50K",
      TRUE ~ "Over 50K"))
  
  ##This sheet contains regions
  data(states, package = 'noncensus')
  regions = states[, c('name','region')]
  
  ## Add Southwest category
  regions$region = as.character(regions$region)
  regions$region[regions$name == 'Delaware'] = 'Northeast'
  regions$region[regions$name == 'District of Columbia'] = 'Northeast'
  regions$region[regions$name == 'Maryland'] = 'Northeast'
  regions$region[regions$name == 'Oklahoma'] = 'Southwest'
  regions$region[regions$name == 'Texas'] = 'Southwest'
  regions$region[regions$name == 'New Mexico'] = 'Southwest'
  regions$region[regions$name == 'Arizona'] = 'Southwest'
  
  
  #Now we add merge the sheet with income and then with region
  individuals_coded = readxl::read_excel(enter_file, sheet = "Individuals Coded") %>%
    dplyr::inner_join(individuals, by = c("ID" = "ID")) %>%
    dplyr::left_join(regions, by = c("Area" = "name"))
  
  ##Order region, income
  individuals_coded = individuals_coded %>% 
    dplyr::mutate(income = factor(income, levels = c("Under 50K", "Over 50K")),
                  region = factor(region, levels = c("South", "Northeast", "Midwest", "West", "Southwest"))) %>%
    dplyr::rename(Income = income,
                  Region = region)
  
  ##Fix age and gender
  individuals_coded = individuals_coded %>% 
    dplyr::mutate(Age = case_when(
      Age == "> 54" ~ "Over 54",
      TRUE ~ Age
    )) %>%
    dplyr::mutate(Age = factor(Age, levels = c("18 - 24", "25 - 34","35 - 44","45 - 54", "Over 54"))) %>%
    dplyr::mutate(Gender = str_to_title(Gender))
  
  individuals_coded = individuals_coded %>% 
    dplyr::mutate(Sample = "Sample")
  
  return(individuals_coded)
  
}
