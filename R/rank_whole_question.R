#' Creating rank crosstabs from a pollfish file
#'
#' Function that will run all rank questions crosstabs for a single variable, save them in a list, and write them to different files. There is a different file for each level of x_var
#' @param d_frame1: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param rank_variables: Vector with the code for all rank questions
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' rank_choice <- c("Q7","Q8")
#' rank_whole_question(x,"Region", rank_choice)


rank_whole_question = function(d_frame, x_var,rank_variables){
  
  x_var_sym = rlang::sym(x_var)
  
  #big toptics like age, gender, income, etc
  topic = d_frame %>%
    dplyr::select(!!x_var_sym) %>%
    tidyr::drop_na() %>%
    dplyr::pull(!!x_var_sym) %>%
    unique()
  
  #categories under topics
  category = purrr::map(rank_variables, function(i) rank_question(d_frame, x_var, i))
  
  #output excel
  output = map(1:length(rank_variables), function(i){
    map(1:length(topic), function(j){
      result = purrr::pluck(category, i, topic[j])
      xlsx::write.xlsx(as.data.frame(result), file = paste0(tolower(topic[j]),'_rank_questions.xlsx'), sheetName = rank_variables[i], row.names = FALSE, append = TRUE)
    })
  })
}
