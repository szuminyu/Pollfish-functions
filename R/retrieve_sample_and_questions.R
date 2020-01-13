#' Creating sample frequencies and question names from a pollfish file
#'
#' Function that will run all rank questions crosstabs for a single variable, save them in a list, and write them to different files. There is a different file for each level of x_var
#' @param pollfish_file: Pollfish file to process
#' @param column_names: Columns to pull information from. Do not mix question type
#' @param rank: Do you want to retrieve information for rank questions. Default is FALSE
#' @param prefix_for_file: Prefix for the .xlsx file created
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' #Retrieve sample frequencies for rank choices
#' rank_choice <- c("Q7","Q8")
#' retrieve_sample_and_questions("Pollfish_Survey.xls",rank_choice,rank = TRUE,"rank")
#' 
#' #Retrieve sample frequencies for single choices
#' single_choice <-c("Q1","Q3","Q4","Q5","Q6", "Q11")
#' retrieve_sample_and_questions("Pollfish_Survey.xls",single_choice,rank = FALSE, "single")


retrieve_sample_and_questions <- function(pollfish_file, column_names,rank=FALSE,prefix_for_file){
  
  ##Function to get rid of parentheses
  remove_parentheses <- function(x){stringr::str_trim(stringr::str_remove_all(x,"\\(.*\\)"))}
  
  
  ##Sample totals
  work_sheets <- column_names
  work_sheets <- unlist(stringr::str_extract_all(work_sheets, "Q[0-9]{1,2}.*"))
  
  #update 12192019
  
  if(rank == FALSE){
    
    sample_answers = purrr::map(work_sheets, function(i){
      df = readxl::read_excel(pollfish_file, sheet = i, skip = 1)
      ## single choice questioins will have 5 columns; multiple choice questions will have more than 5
      if (ncol(df) == 5) {
        df = df %>% rename('Percent' = 'Answers(%)')
      }
      else {df = df %>% select(Answers,`Respondents(%)`, Count) %>% rename('Percent' = 'Respondents(%)')}
    })
    
    sample_answers <- lapply(sample_answers, function(i) i %>% dplyr::mutate(row = dplyr::row_number(),
                                                                      Answers = paste0(row,". ", Answers),
                                                                      Percent = scales::percent(Percent,accuracy = .1)) %>%
                                                                dplyr::select(-row))
    
  }else{
    sample_answers = purrr::map(work_sheets, function(i) readxl::read_excel(pollfish_file, sheet = i, skip = 1))
    sample_answers <- lapply(sample_answers, function(i) i %>% dplyr::mutate(row = dplyr::row_number(),
                                                                      Answers = paste0(row,". ", Answers)) %>%
                                                                dplyr::select(-row))
  }
  names(sample_answers) <- column_names
  writexl::write_xlsx(sample_answers, paste0(prefix_for_file, "_sample_answers.xlsx"))
  
  ###
  questions <- sapply(work_sheets, function(i)readxl::read_excel(pollfish_file, sheet = i) %>% 
                        dplyr::select(matches('[a-zA-Z]')) %>%
                        colnames())
  
  questions = dplyr::tibble(code = column_names, text = rtweet::plain_tweets(questions)) %>%
    dplyr::mutate(text = paste0(code,". ",text))
  
  readr::write_csv(questions, paste0(prefix_for_file, "_text.csv"))
  
  return(sample_answers)
}
