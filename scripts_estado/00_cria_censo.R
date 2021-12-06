library(tidyverse)

url <- "data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/"
# Escolas
censo_escolas <- read_delim(paste0(url, "DADOS/ESCOLAS.CSV"), delim = "|")
censo_escolas <- censo_escolas %>% filter(CO_UF == 35)
write.csv2(censo_escolas, paste0(url, "DADOS_SP/ESCOLAS.CSV"), na = "")

# Turmas
censo_turmas <- read_delim(paste0(url, "DADOS/TURMAS.CSV"), delim = "|")
censo_turmas <- censo_turmas %>% filter(CO_UF == 35)
write.csv2(censo_turmas, paste0(url, "DADOS_SP/TURMAS.CSV"), na = "")

# Gestor
censo_gestor <- read_delim(paste0(url, "DADOS/GESTOR.CSV"), delim = "|")
censo_gestor <- censo_gestor %>% filter(CO_UF == 35)
write.csv2(censo_gestor, paste0(url, "DADOS_SP/GESTOR.CSV"), na = "")

# Matriculas
censo_matriculas <- read_delim(paste0(url, "DADOS/MATRICULA_SUDESTE.CSV"), delim = "|")
censo_matriculas <- censo_matriculas %>% filter(CO_UF == 35)

write.csv2(censo_matriculas, paste0(url, "DADOS_SP/MATRICULA_SUDESTE.CSV"))

#censo_matriculas <- read_delim("data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/MATRICULA_SUDESTE.CSV", delim = "|")

censo_escolas_sel <- censo_escolas %>% 
  select(
    NU_ANO_CENSO,
    CO_ENTIDADE,
    NO_ENTIDADE,
    CO_ORGAO_REGIONAL,
    TP_SITUACAO_FUNCIONAMENTO,
    CO_REGIAO,
    CO_MESORREGIAO,
    CO_MICRORREGIAO,
    CO_UF,
    CO_MUNICIPIO,
    CO_DISTRITO,
    TP_DEPENDENCIA,
    TP_LOCALIZACAO,
    IN_GRUPOS_NAO_SERIADOS,
    IN_EDUCACAO_INDIGENA,
    IN_REGULAR,
    IN_EJA,
    IN_PROFISSIONALIZANTE,
    IN_COMUM_FUND_AI,
    IN_COMUM_FUND_AF,
    IN_COMUM_MEDIO_MEDIO,
    IN_COMUM_MEDIO_INTEGRADO,
    IN_COMUM_MEDIO_NORMAL,
    IN_ESP_EXCLUSIVA_FUND_AI,
    IN_ESP_EXCLUSIVA_FUND_AF,
    IN_ESP_EXCLUSIVA_MEDIO_MEDIO,
    IN_ESP_EXCLUSIVA_MEDIO_INTEGR,
    IN_ESP_EXCLUSIVA_MEDIO_NORMAL
  )
rm(censo_escolas)
colnames(censo_escolas_sel) <- tolower(colnames(censo_escolas_sel))
#Saresp
#18  4620    18 - Ensino Fundamental de 9 anos - 5º Ano
#41 10904    41 - Ensino Fundamental de 9 anos - 9º Ano
#27 12504    27 - Ensino Médio - 3ºano/3ª Série

#22    53    22 - Ensino Fundamental de 9 anos - Multi
#23     1    23 - Ensino Fundamental de 9 anos - Correção de Fluxo
#29     5    29 - Ensino Médio - Não Seriada
#NA    13


censo_turmas_sel <- censo_turmas %>% 
  filter(TP_ETAPA_ENSINO %in% c(18,41,22,23,27:29,32:34,37:74)) %>% 
  select(
    CO_ENTIDADE,
    ID_TURMA,
    TP_ETAPA_ENSINO,
    QT_MATRICULAS) %>% 
  mutate(tipo_turma = case_when(
    TP_ETAPA_ENSINO == 18 ~ 'EF5',
    TP_ETAPA_ENSINO == 41 ~ 'EF9',
    TP_ETAPA_ENSINO == 27 ~ 'EM3',
    TP_ETAPA_ENSINO %in% c(22,23,29) ~ 'Multi_fluxo',
    TP_ETAPA_ENSINO %in% c(65:74) ~'EJA',
    TP_ETAPA_ENSINO %in% c(30:40,64) ~'Tecnico',
    TRUE ~'Outros'
  ))
  
rm(censo_turmas)

censo_turmas_spread <- censo_turmas_sel %>% spread(tipo_turma, QT_MATRICULAS)
head(censo_turmas_spread)

censo_turmas_sum <- censo_turmas_spread %>% 
  group_by(CO_ENTIDADE) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  select(-ID_TURMA, -TP_ETAPA_ENSINO)
head(censo_turmas_sum)

colnames(censo_turmas_sum) <- tolower(colnames(censo_turmas_sum))

output <- censo_escolas_sel %>% 
  left_join(censo_turmas_sum, by = "co_entidade")
  
getwd()
write.csv2(output, "output/books/censo_tratado.csv", row.names = FALSE, na = "")
# teste <- saresp_alunos %>% distinct(CODESC) %>% 
#   transmute(CO_ENTIDADE = as.integer(paste0("35",CODESC))) %>% 
#   left_join(censo_turmas_sel, by = "CO_ENTIDADE")
#     
#   teste %>% group_by(TP_ETAPA_ENSINO) %>% 
#     count()


    
    
    
    
    
    
    
    
    
    
    
    
    