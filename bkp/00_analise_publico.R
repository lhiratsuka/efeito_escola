library(tidyverse)

df_publico <- read.csv2("./output/books/df_publico.csv")
saeb_5ef <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_5EF.csv")
saeb_9ef <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_9EF.csv")
saeb_3em <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ALUNO_34EM.csv")
##resultados saeb por escola
escolas <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ESCOLA.csv")

## censo escolar 2019
censo <- read_delim("data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/ESCOLAS.CSV", delim = "|")


#Waterfall -----
count_censo <- censo %>%                                   
  mutate(UF = if_else(CO_UF == 35, 'SP', 'Outros')) %>% 
  group_by(
    UF,
    TP_SITUACAO_FUNCIONAMENTO,
    TP_DEPENDENCIA,
    IN_COMUM_FUND_AI,
    IN_COMUM_FUND_AF,
    IN_COMUM_MEDIO_MEDIO,
    IN_COMUM_MEDIO_INTEGRADO) %>% 
  summarise(qtd = n())

write.csv2(count_censo, "output/count_censo.csv", row.names = FALSE, na = "")
                                                                                                                                                                             
## Censo SP Ativas ----

##5EF
censo %>% 
  filter(CO_UF == 35, 
                 TP_SITUACAO_FUNCIONAMENTO ==1,
                 IN_COMUM_FUND_AI == 1) %>% 
  count()

##9EF
censo %>% 
  filter(CO_UF == 35, 
         TP_SITUACAO_FUNCIONAMENTO ==1,
         IN_COMUM_FUND_AF == 1) %>% 
  count()
  
##3EM 
censo %>% 
  filter(CO_UF == 35, 
         TP_SITUACAO_FUNCIONAMENTO ==1,
         IN_COMUM_MEDIO_MEDIO == 1) %>% 
  count()

# Qtd alunos: 464420
saeb_5ef %>% filter(CO_UF == 352) %>% 
  count()

# Qtd escolas (base alunos): 7945
saeb_5ef %>% filter(ID_DEPENDENCIA_ADM == 2) %>% 
  distinct(ID_ESCOLA) %>% 
  count()

# Qtd escolas (base escolas): 7937
escolas %>% 
  filter(!is.na(NU_MATRICULADOS_CENSO_5EF)) %>% 
  group_by(ID_DEPENDENCIA_ADM) %>% count() 



## Qtd escolas Saeb ----
count_escolas <- escolas %>% 
  filter(
    ID_UF == 35,
  ) %>% 
  transmute(
    ID_DEPENDENCIA_ADM,
    ID_ESCOLA,
    FL_MASCARA = if_else(ID_ESCOLA >= 60000000, 1, 0),
    NU_MATRICULADOS_CENSO_5EF,
    NU_MATRICULADOS_CENSO_9EF,
    NU_MATRICULADOS_CENSO_EM,
    TAXA_PARTICIPACAO_5EF,
    TAXA_PARTICIPACAO_9EF,
    TAXA_PARTICIPACAO_EM,
    MEDIA_5EF_LP,
    MEDIA_9EF_LP,
    MEDIA_EM_LP
  )


#5 EF
count_escolas %>% 
  filter(!is.na(NU_MATRICULADOS_CENSO_5EF)) %>% 
  count()

#9 EF
count_escolas %>% 
  filter(!is.na(NU_MATRICULADOS_CENSO_9EF)) %>% 
  count()

#3EM
count_escolas %>% 
  filter(!is.na(NU_MATRICULADOS_CENSO_EM)) %>% 
  count()

# Qtd escolas com resultado Saeb e sem mascara -----
#5 EF
count_escolas %>% 
  filter(!is.na(MEDIA_5EF_LP), FL_MASCARA == 0) %>% 
  count()

#9 EF
count_escolas %>% 
  filter(!is.na(MEDIA_9EF_LP), FL_MASCARA == 0) %>% 
  count()

#3EM
count_escolas %>% 
  filter(!is.na(MEDIA_EM_LP), FL_MASCARA == 0) %>% 
  count()

# Qtd escolas estaduais/municipais -----
#5 EF
count_escolas %>% 
  filter(!is.na(MEDIA_5EF_LP), FL_MASCARA == 0, ID_DEPENDENCIA_ADM %in% c(2,3)) %>% 
  group_by(ID_DEPENDENCIA_ADM) %>% 
  count()

#9 EF
count_escolas %>% 
  filter(!is.na(MEDIA_9EF_LP), FL_MASCARA == 0, ID_DEPENDENCIA_ADM %in% c(2,3)) %>% 
  group_by(ID_DEPENDENCIA_ADM) %>% 
  count()

#3EM
count_escolas %>% 
  filter(!is.na(MEDIA_EM_LP), FL_MASCARA == 0, ID_DEPENDENCIA_ADM %in% c(2,3)) %>% 
  group_by(ID_DEPENDENCIA_ADM) %>% 
  count()


# Analise notas por tipo de escolas ----

# Resultado por id_dependencia_adm
df_publico %>% 
  group_by(id_dependencia_adm) %>% 
  summarise(qtd = n(),
            mean_lp = mean(media_lp),
            mean_mt = mean(media_mt),
            mean = mean(media))

# Resultado por id_localizacao
df_publico %>% 
  group_by(id_localizacao) %>% 
  summarise(qtd = n(),
            mean_lp = mean(media_lp),
            mean_mt = mean(media_mt),
            mean = mean(media))

# Resultado por id_localizacao
df_publico %>% 
  group_by(id_localizacao) %>% 
  summarise(qtd = n(),
            mean_lp = mean(media_lp),
            mean_mt = mean(media_mt),
            mean = mean(media))

# Distribuicao por serie ----
# Histograma lp
ggplot(df_publico, aes(media_lp,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_serie) +
  theme_minimal()

# Histograma mt
ggplot(df_publico, aes(media_mt,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_serie) +
  theme_minimal()

# Histograma mediA
ggplot(df_publico, aes(media,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_serie) +
  theme_minimal()


# Distribuicao por esfera ----
# Histograma lp
ggplot(df_publico, aes(media_lp,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_dependencia_adm) +
  theme_minimal()

# Histograma mt
ggplot(df_publico, aes(media_mt,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_serie) +
  theme_minimal()

# Histograma mediA
ggplot(df_publico, aes(media,y = ..density..)) +
  geom_histogram(bins = 15, fill = "gray") +
  facet_wrap(~id_serie) +
  theme_minimal()



