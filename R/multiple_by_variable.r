## multiple questions crossed by variable

multiple_by_variable = function(dataframe, x_var, y_var){
  x_var_sym = rlang::sym(x_var)
  y_var_sym = rlang::sym(y_var)
  ## Get age category numbers
  num = dataframe %>% group_by(!!x_var_sym) %>% tally()
  
  ##Get columns
  df = dataframe %>% 
    dplyr::select(!!x_var_sym, dplyr::matches(!!paste0(y_var,"\\."))) %>%
    tidyr::gather(answer, value, -!!x_var_sym) %>%
    dplyr::group_by(!!x_var_sym, answer, value) %>% 
    dplyr::tally() %>%
    dplyr::filter(value == 1) %>%
    dplyr::select(-value) 
  ## calculate the total each; suppose choices range from 2 to 5
  if (nrow(num) == 2){
    df = df %>%
      dplyr::mutate(total_each = dplyr::case_when(
        !!x_var_sym == 1 ~num[[1,2]],
        !!x_var_sym == 2 ~num[[2,2]]
      ))   
  }
  else if  (nrow(num) == 3){
    df = df %>%
      dplyr::mutate(total_each = dplyr::case_when(
        !!x_var_sym == 1 ~num[[1,2]],
        !!x_var_sym == 2 ~num[[2,2]],
        !!x_var_sym == 3 ~num[[3,2]]
      ))   
  }
  else if (nrow(num) == 4){
    df = df %>%
      dplyr::mutate(total_each = dplyr::case_when(
        !!x_var_sym == 1 ~num[[1,2]],
        !!x_var_sym == 2 ~num[[2,2]],
        !!x_var_sym == 3 ~num[[3,2]],
        !!x_var_sym == 4 ~num[[4,2]]
      )) 
  }
  else {
      df = df %>%
        dplyr::mutate(total_each = dplyr::case_when(
          !!x_var_sym == 1 ~num[[1,2]],
          !!x_var_sym == 2 ~num[[2,2]],
          !!x_var_sym == 3 ~num[[3,2]],
          !!x_var_sym == 4 ~num[[4,2]],
          !!x_var_sym == 5 ~num[[5,2]]
        ))   
    }
  
  df = df %>%
    dplyr::mutate(percent = n/total_each) %>%
    dplyr::select(!!x_var_sym, answer, percent) %>%
    tidyr::spread(key = answer, percent) %>%
    dplyr::mutate_if(is_double, ~scales::percent(.,accuracy = .1)) %>%
    dplyr::select_at(dplyr::vars(dplyr::matches('Q[1-9]\\.*')), function(i)stringr::str_extract(i, pattern = '(?<=\\.)\\d*')) %>%
    tidyr::drop_na()
  
  return(df)
}

