## Pollfish Functions by SM YU ##

#Read file - basic variables are upper case for the first letter
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
  library(noncensus)
  data('states')
  regions = states[, c('name','region')]
  
  ## Add Southwest category
  regions$region = as.character(regions$region)
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

#Singles
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
single_crosstabs_by_variable <- function(d_frame1, x_var1,single_choice_columns){
  z <- lapply(single_choice_columns, function(i)single_crosstab(d_frame1, x_var = x_var1, y_var = i))
  names(z) <- single_choice_columns
  writexl::write_xlsx(z, paste0(tolower(x_var1), "_single_choice", ".xlsx"))
  return(z)
}

#Multiples
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

#Ranks
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


#Get samples (from Harro)
retrieve_sample_and_questions <- function(pollfish_file, column_names,rank=FALSE,prefix_for_file){
  
  ##Function to get rid of parentheses
  remove_parentheses <- function(x){str_trim(str_remove_all(x,"\\(.*\\)"))}
  
  
  ##Sample totals
  work_sheets <- column_names
  work_sheets <- unlist(str_extract_all(work_sheets, "Q[0-9]{1,2}.*"))
  sample_answers <- lapply(work_sheets, function(i)read_excel(pollfish_file, sheet = i, skip = 1) %>%
                             select(-matches("Respondents")) %>%
                             rename_all(remove_parentheses))
  
  if(rank == FALSE){
    sample_answers <- lapply(sample_answers, function(i) i %>% mutate(row = row_number(),
                                                                      Answers = paste0(row,". ", Answers),
                                                                      Percent = scales::percent(Percent,accuracy = .1)) %>%
                               select(-row))
    
  }else{
    sample_answers <- lapply(sample_answers, function(i) i %>% mutate(row = row_number(),
                                                                      Answers = paste0(row,". ", Answers)) %>%
                               select(-row))
  }
  names(sample_answers) <- column_names
  writexl::write_xlsx(sample_answers, paste0(prefix_for_file, "_sample_answers.xlsx"))
  
  ###
  questions <- sapply(work_sheets, function(i)read_excel(pollfish_file, sheet = i) %>% 
                        select(-matches("X__")) %>%
                        colnames())
  questions <- tibble(code = names(questions),
                      text = rtweet::plain_tweets(questions)) %>%
    mutate(text = paste0(code,". ",text))
  
  readr::write_csv(questions, paste0(prefix_for_file, "_text.csv"))
  
  return(sample_answers)
}

#Generate basic crosstabs (mostly from Harro)
basic_survey_automation = function(survey_file, single_questions = NULL,multiple_questions = NULL, rank_questions = NULL){
  
  ##Create folders and copy file
  lapply(c("single_questions", "multiple_questions", "rank_questions"), function(i){
    dir.create(i, showWarnings = FALSE)
    file.copy(from = file.path(survey_file), to = i)
  })
  
  
  ##List of basic variables
  basic_variables <- c("Region", "Gender", "Age", "Income")
  
  ##Write single questions
  if(is.null(single_questions)){
    print("No single choice questions. Is this correct?")
  }else{
    setwd("single_questions/")
    
    retrieve_sample_and_questions(survey_file,single_questions,rank = FALSE, "single")
    x <- read_pollfish_file(survey_file)
    lapply(basic_variables, function(i)single_crosstabs_by_variable(x,i,single_questions))
    
    setwd("..//")
  }
  
  ##Write multiple questions
  if(is.null(multiple_questions)){
    print("No multiple choice questions. Is this correct")
  }else{
    setwd("multiple_questions/")
    
    retrieve_sample_and_questions(survey_file,multiple_questions,rank = FALSE, "multiple")
    x <- read_pollfish_file(survey_file)
    multiple_crosstabs_by_variable(x,multiple_questions)
    
    setwd("..//")
  }  
  
  ##Write rank questions
  if(is.null(rank_questions)){
    print("No rank questions. Is this correct?")
  }else{
    setwd("rank_questions/")
    
    retrieve_sample_and_questions(survey_file,rank_questions,rank = TRUE, "rank")
    
    x <- read_pollfish_file(survey_file)
    print("File read")
    lapply(c(basic_variables,"sample"), function(i)rank_whole_question(x,i,rank_questions))
    
    setwd("..//")
  }  
  
  ##
  print("Basic crosstabs created")
}
