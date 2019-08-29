#' Creating an appendix and a map from a pollfish file
#'
#' Function that reads a Pollfish file and automatically creates frequency tables for the appendix and a map
#' @param pollfish_file: .xlsx file with the data
#' @keywords Pollfish
#' @export
#' @examples
#' create_appendix_and_map("Pollfish_Survey.xls")



create_appendix_and_map <- function(pollfish_file){
  x <- read_pollfish_file(pollfish_file)
  
  
  ##Worksheets for appendix file
  states <- dplyr::count(x, Area, Region) %>%
    dplyr::mutate(n = scales::comma(n)) %>%
    magrittr::set_colnames(c("state","region", "n"))
  
  
  region <- x %>%
    dplyr::count(Region) %>%
    dplyr::mutate(Region = dplyr::case_when(is.na(as.character(Region)) ~'Missing', TRUE ~as.character(Region))) %>%
    dplyr::mutate(percent = scales::percent(n/sum(n)),
                  n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Region","Total", "Percent"))
  
  age <- dplyr::count(x, Age) %>% 
    dplyr::mutate(percent = scales::percent(n/sum(n)), n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Age","Total", "Percent"))
  
  gender <- dplyr::count(x, Gender) %>% 
    dplyr::mutate(percent = scales::percent(n/sum(n)),n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Gender","Total", "Percent"))
  
  income <- x %>%
    dplyr::count(Income) %>%
    dplyr::mutate(Income = dplyr::case_when(is.na(as.character(Income)) ~'Missing', TRUE ~as.character(Income))) %>%
    dplyr::mutate(percent = scales::percent(n/sum(n)),
                  n = scales::comma(n))%>%
    magrittr::set_colnames(c("Income","Total", "Percent"))
  
  list_to_return <- list(states,region, age, gender, income)
  names(list_to_return) <- c("states_for_map","Region", "Age", "Gender", "Income")
  writexl::write_xlsx(list_to_return, "appendix_file.xlsx")
  
  ##Create map
  
  my_map <-create_map("appendix_file.xlsx")
  my_map
  ggplot2::ggsave("plot.png", width=12, height=8, dpi=300)
  
  #Return list
  #return(list(states,region, age, gender, income))
}
