#' Creating a basic crosstab for a multiple choice question from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var for a single multiple choice question. Proportions displayed are row proportions and refer to % of total answers.
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param y_var: Variable that will represent the columns
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' multiple_question(x, "Age", "Q2")



age_multiple_question <- function(dataframe, y_var){
  y_var_sym = rlang::sym(y_var)
  ## Get age category numbers
  num = dataframe %>% group_by(Age) %>% tally()
  
  
  ##Get columns
  df = dataframe %>% 
    dplyr::select(Age, dplyr::matches(!!paste0(y_var,"\\."))) %>%
    tidyr::gather(answer, value, -Age) %>%
    dplyr::group_by(Age, answer, value) %>% 
    dplyr::tally() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value)
  ##Get Persentages
  df = df %>%
    dplyr::mutate(total_each = dplyr::case_when(
      Age == "18 - 24" ~num[[1,2]],
      Age == "25 - 34" ~num[[2,2]],
      Age == "35 - 44" ~num[[3,2]],
      Age == "45 - 54" ~num[[4,2]],
      Age == "Over 54" ~num[[5,2]]
    ))  %>%
    dplyr::mutate(percent = n/total_each) %>%
    dplyr::select(Age, answer, percent) %>%
    tidyr::spread(key = answer, percent) %>%
    dplyr::mutate_if(is_double, ~scales::percent(.,accuracy = .1)) %>%
    dplyr::select_at(dplyr::vars(dplyr::matches('Q[1-9]\\.*')), function(i)stringr::str_extract(i, pattern = '(?<=\\.)\\d*'))  %>%
    tidyr::drop_na()
  
  return(df)
}
income_multiple_question <- function(dataframe, y_var){
  y_var_sym = rlang::sym(y_var)
  ## Get age category numbers
  num = dataframe %>% group_by(Income) %>% tally()
  
  
  ##Get columns
  df = dataframe %>% 
    dplyr::select(Income, dplyr::matches(!!paste0(y_var,"\\."))) %>%
    tidyr::gather(answer, value, -Income) %>%
    dplyr::group_by(Income, answer, value) %>% 
    dplyr::tally() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value)
  ##Get Persentages
  df = df %>%
    dplyr::mutate(total_each = dplyr::case_when(
      Income == "Under 50K" ~num[[1,2]],
      Income == "Over 50K" ~num[[2,2]],
    ))  %>%
    dplyr::mutate(percent = n/total_each) %>%
    dplyr::select(Income, answer, percent) %>%
    tidyr::spread(key = answer, percent) %>%
    dplyr::mutate_if(is_double, ~scales::percent(.,accuracy = .1)) %>%
    dplyr::select_at(dplyr::vars(dplyr::matches('Q[1-9]\\.*')), function(i)stringr::str_extract(i, pattern = '(?<=\\.)\\d*')) %>%
    tidyr::drop_na()
  
  return(df)
}
gender_multiple_question <- function(dataframe, y_var){
  y_var_sym = rlang::sym(y_var)
  ## Get age category numbers
  num = dataframe %>% group_by(Gender) %>% tally()
  
  
  ##Get columns
  df = dataframe %>% 
    dplyr::select(Gender, dplyr::matches(!!paste0(y_var,"\\."))) %>%
    tidyr::gather(answer, value, -Gender) %>%
    dplyr::group_by(Gender, answer, value) %>% 
    dplyr::tally() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value)
  ##Get Persentages
  df = df %>%
    dplyr::mutate(total_each = dplyr::case_when(
      Gender == "Female" ~num[[1,2]],
      Gender == "Male" ~num[[2,2]],
    ))  %>%
    dplyr::mutate(percent = n/total_each) %>%
    dplyr::select(Gender, answer, percent) %>%
    tidyr::spread(key = answer, percent) %>%
    dplyr::mutate_if(is_double, ~scales::percent(.,accuracy = .1)) %>%
    dplyr::select_at(dplyr::vars(dplyr::matches('Q[1-9]\\.*')), function(i)stringr::str_extract(i, pattern = '(?<=\\.)\\d*')) %>%
    tidyr::drop_na()
  
  return(df)
}
region_multiple_question <- function(dataframe, y_var){
  y_var_sym = rlang::sym(y_var)
  ## Get age category numbers
  num = dataframe %>% group_by(Region) %>% tally()
  
  
  ##Get columns
  df = dataframe %>% 
    dplyr::select(Region, dplyr::matches(!!paste0(y_var,"\\."))) %>%
    tidyr::gather(answer, value, -Region) %>%
    dplyr::group_by(Region, answer, value) %>% 
    dplyr::tally() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value)
  ##Get Persentages
  df = df %>%
    dplyr::mutate(total_each = dplyr::case_when(
      Region == "South" ~num[[1,2]],
      Region == "Northeast" ~num[[2,2]],
      Region == "Midwest" ~num[[3,2]],
      Region == 'West' ~num[[4,2]],
      Region == 'Southwest'~num[[5,2]]
    ))  %>%
    dplyr::mutate(percent = n/total_each) %>%
    dplyr::select(Region, answer, percent) %>%
    tidyr::spread(key = answer, percent) %>%
    dplyr::mutate_if(is_double, ~scales::percent(.,accuracy = .1)) %>%
    dplyr::select_at(dplyr::vars(dplyr::matches('Q[1-9]\\.*')), function(i)stringr::str_extract(i, pattern = '(?<=\\.)\\d*')) %>%
    tidyr::drop_na()
  
  return(df)
}
