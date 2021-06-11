library(tidyverse)
library(readxl)
library(caret)
library(ranger)
# =============================== RESULTADOS =============================

#TS_RESULTADO_ESCOLA ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_RESULTADO_ESCOLA.csv"
ts_resultado_escola_2011 <- read.csv2(url)

#TS_RESULTADO_ALUNO ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_RESULTADO_ALUNO.csv"
ts_resultado_aluno2011 <- read.csv2(url)
ts_resultado_aluno2011 <-  ts_resultado_aluno2011 %>%filter(ID_UF == 35)

#TS_RESULTADO_MUNICIPIO ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_RESULTADO_MUNICIPIO.csv"
ts_resultado_municipio_2011 <- read.csv2(url)


# =============================== QUESTIONARIOS =============================

#TS_QUEST_ESCOLA ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_QUEST_ESCOLA.csv"
ts_quest_escola_2011 <- read.csv2(url)

#TS_QUEST_DIRETOR ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_QUEST_DIRETOR.csv"
ts_quest_diretor_2011 <- read.csv2(url)

#TS_QUEST_PROFESSOR ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_QUEST_PROFESSOR.csv"
ts_quest_professor_2011 <- read.csv2(url)

#TS_QUEST_ALUNO ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_QUEST_ALUNO.csv"
ts_quest_aluno_2011 <- read.csv2(url)
ts_quest_aluno_2011 <-  ts_quest_aluno_2011 %>%filter(ID_UF == 35)

url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dicionário/Dicionario_Prova_Brasil_2011.xlsx"
label_quest_aluno <- read_xlsx(url, sheet = "TS_QUEST_ALUNO", skip = 27)
label_quest_aluno_5serie <- data.frame(questao = label_quest_aluno$...1[1:54], 
                                       enunciado = label_quest_aluno$Enunciado[1:54])
label_quest_aluno_9serie <- data.frame(questao = label_quest_aluno$...1[59:116],
                                         enunciado = label_quest_aluno$Enunciado[59:116])
# =============================== OUTROS =============================

# TS_ITEM ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_ITEM.csv"
ts_item_2011 <- read.csv2(url)

# TS_PESOS ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_PESOS.csv"
ts_pesos_2011 <- read.csv2(url)

# TS_RESPOSTA_ALUNO ----
url <- "data/microdados_prova_brasil_2011/Microdados Prova Brasil 2011/Dados/TS_RESPOSTA_ALUNO.csv"
ts_resposta_aluno_2011 <- read.csv2(url)


# CENSO ESCOLA ----
url <- "data/micro_censo_escolar_2011/2011/DADOS/ESCOLAS.CSV"
censo_escolar <- read.delim(url, sep = "|")

# IBGE
ibge <- read_xls("data/estimativa_dou_2020.xls", sheet = 2, skip = 1)[1:5570,]
colnames(ibge) <- c("uf", "cod_uf", "cod_municipio", "nome_municipio", "pop_estimada")
ibge <- ibge %>% mutate(pk = as.integer(str_c(cod_uf, cod_municipio)))


