library(tidyverse)
library(readxl)
setwd("C:\\Users\\livia\\OneDrive - TRIBUNAL DE CONTAS DO ESTADO DE SAO PAULO\\efeito_escola")
data <- read_csv2("data/saresp/MICRODADOS_SARESP_2019.csv")

data %>% filter(CD_ALUNO == 19726902)
#CD_UE (base quesitonario) = CODESC (base resultados saresp)
#ESCOLA 16 DE JULHO: 923370

# Procurando nome da escola
## não tem no saeb

censo <- read_delim("data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/ESCOLAS.CSV", delim = "|")
censo <- censo %>% filter(CO_UF == 35)
censo %>% filter(str_detect(NO_ENTIDADE,pattern = "16"))
# CO_ENTIDADE = 35923370


saresp <- data %>% transmute(CODESC,
                             ID_ESCOLA = as.numeric(paste0("35",CODESC)),
                             CODMUN,
                             MUN
                          ) %>% distinct()
head(saresp)
censo <- censo %>% select(CO_ENTIDADE:CO_MUNICIPIO)
head(censo)

cruza_saresp_censo <- saresp %>% 
  left_join(censo, by = c("ID_ESCOLA" = "CO_ENTIDADE"))
  
  
  
  