#' Creating multiple crosstabs from a pollfish file
#'
#' Function that will run all multiple choice crosstabs for a single variable, save them in a list, and write them to file
#' @param d_frame1: Data frame created from a pollfish file
#' @param x_var1: Variable that will represent the rows
#' @param multiple_choice_columns: Vector with the code for all multiple choice variables
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' multiple_choice <- c("Q2", "Q9")
#' multiple_crosstabs_by_variable(x,multiple_choice)

multiple_crosstabs_by_variable <- function(d_frame1, multiple_choice_columns){
  
  a = map(multiple_choice_columns, function(i)age_multiple_question(d_frame1, i))
  b = map(multiple_choice_columns, function(i)income_multiple_question(d_frame1, i))
  c = map(multiple_choice_columns, function(i)gender_multiple_question(d_frame1, i))
  d = map(multiple_choice_columns, function(i)region_multiple_question(d_frame1, i))
  
  names(a) <- multiple_choice_columns
  names(b) <- multiple_choice_columns
  names(c) <- multiple_choice_columns
  names(d) <- multiple_choice_columns
  
  writexl::write_xlsx(a, "age_multiple_choice", ".xlsx")
  writexl::write_xlsx(b, "income_multiple_choice", ".xlsx")
  writexl::write_xlsx(c, "gender_multiple_choice", ".xlsx")
  writexl::write_xlsx(d, "region_multiple_choice", ".xlsx")
}
