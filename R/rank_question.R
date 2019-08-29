#' Creating a basic crosstab for a single rank question from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var for a single rank choice question. Output is a list with rank choices for all factors of x_var.
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that represent the categories
#' @param y_var: Variable that represent the ranking
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' rank_question(x, "age", "Q8")

rank_question = function(d_frame, x_var, y_var){
  x_var_sym = rlang::sym(x_var)
  
  #get percentage crosstab
  df = d_frame %>% 
    dplyr::select(!!x_var_sym, dplyr::matches(paste0(y_var,'.')))%>%
    tidyr::drop_na() %>%
    tidyr::gather(key = choice, value = answer, -!!x_var_sym) %>%
    dplyr::filter(answer != 0) %>%
    dplyr::group_by(!!x_var_sym, choice, answer) %>%
    dplyr::tally()%>%
    dplyr::ungroup() %>%
    dplyr::mutate(choice = stringr::str_extract(choice, pattern = '(?<=\\.)\\d*')) %>%
    dplyr::group_by(!!x_var_sym, choice) %>%
    dplyr::mutate(percent = n/sum(n)) %>%
    dplyr::select(!!x_var_sym, choice, answer, percent)
  
  #creat list
  level = df %>%
    dplyr::pull(!!x_var_sym) %>%
    unique()
  #get crosstabs from list
  list = purrr::map(level, function(i) df %>% dplyr::filter(!!x_var_sym == i) %>% dplyr::ungroup() %>% dplyr::select(-!!x_var_sym))
  list = purrr::map(list, function(i) i %>% tidyr::spread(key = answer, value = percent) %>% dplyr::mutate_at(dplyr::vars(dplyr::matches("[0-9]{1,3}")), ~scales::percent(.,accuracy = 0.1)))
  names(list) = level
  return(list)
}
