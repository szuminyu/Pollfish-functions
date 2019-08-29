#' Creating a single crosstab from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var. Proportions displayed are row proportions
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param y_var: Variable that will represent the columns
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' single_crosstab(x, "Age", "Q4")


single_crosstab <- function(d_frame, x_var, y_var){
  x_var_sym = rlang::sym(x_var)
  y_var_sym = rlang::sym(y_var)
  
  df = d_frame %>%
    #first select columns
    dplyr::select(!! x_var_sym, !! y_var_sym)  %>%
    #drop nas
    tidyr::drop_na() %>%
    #filter for answers not equal to 0
    dplyr::filter(!!y_var_sym != 0) %>%
    #group by columns
    dplyr::group_by(!! x_var_sym, !! y_var_sym) %>%
    #get calculations
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    #group by only main columns
    dplyr::group_by(!! x_var_sym) %>%
    #and calcuate the percentage
    dplyr::mutate(percent = n/sum(n)) %>%
    dplyr::select(-n) %>%
    #spread it into crosstab
    tidyr::spread(key = !!y_var_sym, value = percent) %>%
    #add percentages
    dplyr::mutate_if(is_double, ~scales::percent(., accuracy = .1))
  return(df)
  
}
