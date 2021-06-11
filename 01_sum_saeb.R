library(tidyverse)

saeb_5ef <- read_csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_5EF.csv", na = c("*","."))
saeb_9ef <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_9EF.csv", na = c("*","."))
saeb_3em <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_34EM.csv", na = c("*","."))

mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

mode_data <- function(data) {
  data <- data %>% 
    filter(ID_UF == 35,
           ID_DEPENDENCIA_ADM %in% c(2,3),
           IN_PREENCHIMENTO_LP == 1|| IN_PREENCHIMENTO_MT == 1,
           IN_PREENCHIMENTO_QUESTIONARIO == 1) %>% 
    select(ID_ESCOLA,
           ID_ALUNO,
           TX_RESP_Q001:TX_RESP_Q006D,
           TX_RESP_Q007:TX_RESP_Q012,
           TX_RESP_Q014,
           TX_RESP_Q016:TX_RESP_Q018C)

  
  data_mode <- data %>% 
    group_by(ID_ESCOLA) %>% 
    summarise_if(is.character, mode) %>% 
    arrange(ID_ESCOLA) %>% 
    rename_with(~ tolower(gsub("TX_RESP", "MODE", .x, fixed = TRUE)))
  
  # moda da graduacao da mae desconsiderando a alternativa F (Não sei)
  data_Q04 <- data %>% 
    filter(TX_RESP_Q004 != 'F') %>% 
    transmute(ID_ESCOLA, ID_ALUNO, TX_RESP_Q004_F = TX_RESP_Q004) %>% 
    group_by(ID_ESCOLA) %>% 
    summarise_if(is.character, mode) %>% 
    arrange(ID_ESCOLA) %>% 
    rename_with(~ tolower(gsub("TX_RESP", "MODE", .x, fixed = TRUE)))
  
  # moda da graduacao do pai desconsiderando a alternativa F (Não sei)
  data_Q05 <- data %>% 
    filter(TX_RESP_Q005 != 'F') %>% 
    transmute(ID_ESCOLA, ID_ALUNO, TX_RESP_Q005_F = TX_RESP_Q005) %>% 
    group_by(ID_ESCOLA) %>% 
    summarise_if(is.character, mode) %>% 
    arrange(ID_ESCOLA) %>% 
    rename_with(~ tolower(gsub("TX_RESP", "MODE", .x, fixed = TRUE)))
  
  data_mode <- data_mode %>% 
    left_join(data_Q04, by = 'id_escola') %>% 
    left_join(data_Q05, by = 'id_escola') %>% 
    rename_at(vars(-id_escola), ~paste0(., "_sb"))
}



saeb5ef_mode <- mode_data(saeb_5ef)
saeb9ef_mode <- mode_data(saeb_9ef)
saeb3em_mode <- mode_data(saeb_3em)


