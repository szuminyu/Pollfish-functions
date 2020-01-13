## Pollfish Functions by SM YU ##
library(tidyverse)
options(stringsAsFactors = FALSE)
options(scipen = 999)

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
#from Harro
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
  
  writexl::write_xlsx(a, "age_multiple_choice.xlsx")
  writexl::write_xlsx(b, "income_multiple_choice.xlsx")
  writexl::write_xlsx(c, "gender_multiple_choice.xlsx")
  writexl::write_xlsx(d, "region_multiple_choice.xlsx")
}

#Ranks
rank_question = function(d_frame, x_var, y_var){
  x_var_sym = rlang::sym(x_var)
  
  #get percentage crosstab
  df = d_frame %>% 
    dplyr::select(!!x_var_sym, dplyr::matches(paste0(y_var,'\\.')))%>%
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
    unique() %>%
    as.character()
  
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


#Get samples (mostly from Harro)
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
      if (ncol(df) == 3) {
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
    lapply(c(basic_variables,"Sample"), function(i)rank_whole_question(x,i,rank_questions))
    
    setwd("..//")
  }  
  
  ##
  print("Basic crosstabs created")
}

#Create appendix crosstab and map
create_map <- function(appendix_file){
  library(tidyverse);library(readxl);library(urbnmapr)
  x <- read_excel(appendix_file, sheet = "states_for_map")
  
  
  k <- urbnmapr::states %>%
    inner_join(x, by = c("state_name" = "state")) %>%
    ggplot(aes(long, lat, group = group, fill = region)) + 
    geom_polygon(alpha =0.75, show.legend = FALSE, color = "gray30") + 
    theme_minimal() + 
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 8),
          text = element_text(face = "bold"),
          legend.title = element_text(size =10)) + 
    scale_fill_manual(values = c("royalblue3", "darkorange2", "firebrick2", "darkslategray3","forestgreen", "darkorchid3")) + 
    guides(fill = guide_legend(title = "Region\n", title.position = "top", 
                               title.hjust = 0.5)) + 
    geom_text(data = urbnmapr::get_urbn_labels(map = "states") %>% inner_join(x, by = c("state_name" = "state")) %>%
                mutate(for_label = ifelse(state_abbv %in% c("MD", "DE","NJ","CT","RI","MA","ME","NH","VT", "DC","HI","AK"),state_abbv, "")) %>%
                mutate(label = ifelse(for_label == "",n, paste0(for_label,"-",n))), aes(x = long, lat, label = label), 
              size = 3.9, inherit.aes = FALSE, fontface = "bold", color = "black")
  
  return(k)
}
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
