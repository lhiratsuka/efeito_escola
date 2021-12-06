library(tidyverse)
library(readxl)
library(rio)

# Import ----

##resultados saeb por escola
escolas <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ESCOLA.csv")

## censo escolar 2019
censo <- read_delim("data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS/ESCOLAS.CSV", delim = "|")

## depara codigo dos municipios
url <- "data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/ANEXOS/ANEXO I - Dicionario de Dados e Tabelas Auxiliares/Tabelas Auxiliares.xlsx"
municipios <- read_xlsx(url, sheet = "Anexo10 - UFS e Municípios", skip = 4)
  
## PEI e ETI
pei_eti <- read_csv2("data/pei/ESCOLAS PEI E ETI 2021 (003).csv")
  
# Trata escolas 5EF -----
colnames(escolas)

publico_5EF <- escolas %>% 
  filter(ID_UF == 35,
         #ID_DEPENDENCIA_ADM == 2,
         !is.na(MEDIA_5EF_LP) | !is.na(MEDIA_5EF_MT)) %>% 
  transmute(
    ID_SERIE = '5EF',
    PK = as.integer(str_c(5,ID_ESCOLA)),
    ID_MUNICIPIO,
    ID_DEPENDENCIA_ADM,
    ID_ESCOLA,
    FL_MASCARA = if_else(ID_ESCOLA >= 60000000, 1, 0),
    ID_LOCALIZACAO,
    NIVEL_SOCIO_ECONOMICO,
    NU_MATRICULADOS_CENSO = NU_MATRICULADOS_CENSO_5EF,
    NU_PRESENTES = NU_PRESENTES_5EF,
    MEDIA_LP = MEDIA_5EF_LP,
    MEDIA_MT = MEDIA_5EF_MT
  )



publico_5EF %>% count(FL_MASCARA)
publico_5EF %>% count(ID_DEPENDENCIA_ADM)


# Trata escolas 9EF ----- 
publico_9EF <- escolas %>% 
  filter(ID_UF == 35,
         #ID_DEPENDENCIA_ADM == 2,
         !is.na(MEDIA_9EF_LP) | !is.na(MEDIA_9EF_MT)) %>% 
  transmute(
    ID_SERIE = '9EF',
    PK = as.integer(str_c(9,ID_ESCOLA)),
    ID_MUNICIPIO,
    ID_DEPENDENCIA_ADM,
    ID_ESCOLA,
    FL_MASCARA = if_else(ID_ESCOLA >= 60000000, 1, 0),
    ID_LOCALIZACAO,
    NIVEL_SOCIO_ECONOMICO,
    NU_MATRICULADOS_CENSO = NU_MATRICULADOS_CENSO_9EF,
    NU_PRESENTES = NU_PRESENTES_9EF,
    MEDIA_LP = MEDIA_9EF_LP,
    MEDIA_MT = MEDIA_9EF_MT
  )
# Trata escolas 3EM ----- 
publico_3EM <- escolas %>% 
  filter(ID_UF == 35,
         #ID_DEPENDENCIA_ADM == 2,
         !is.na(MEDIA_EM_LP) | !is.na(MEDIA_EM_MT)) %>% 
  transmute(
    ID_SERIE = '3EM',
    PK = as.integer(str_c(3,ID_ESCOLA)),
    ID_MUNICIPIO,
    ID_DEPENDENCIA_ADM,
    ID_ESCOLA,
    FL_MASCARA = if_else(ID_ESCOLA >= 60000000, 1, 0),
    ID_LOCALIZACAO,
    NIVEL_SOCIO_ECONOMICO,
    NU_MATRICULADOS_CENSO = NU_MATRICULADOS_CENSO_EM,
    NU_PRESENTES = NU_PRESENTES_EM,
    MEDIA_LP = MEDIA_EM_LP,
    MEDIA_MT = MEDIA_EM_MT
  )


# Trata censo -----
censo <- censo %>% filter(CO_UF == 35) %>% 
  select(
    CO_ENTIDADE,
    NO_ENTIDADE,
    TP_SITUACAO_FUNCIONAMENTO,
    IN_COMUM_MEDIO_INTEGRADO,
    CO_DISTRITO
  )

municipios <- municipios %>% 
  filter(CO_UF == 35) %>% 
  select(CO_MUNICIPIO,
         NO_MUNICIPIO)


# Trata pei
pei_eti <- pei_eti %>% 
  transmute(ano_implantacao = `ANO IMPLANTACAO`,
         cod_escola = CODESCMEC,
         pei = PEI,
         eti = ETI)

  
# Cria base publico ------
publico <- rbind(publico_5EF,
                 publico_9EF,
                 publico_3EM)

df_publico <- publico %>% 
  filter(ID_DEPENDENCIA_ADM %in% c(2,3)) %>% 
  mutate(MEDIA = (MEDIA_LP + MEDIA_MT)/2,
         ID_DEPENDENCIA_ADM = factor(ID_DEPENDENCIA_ADM, labels = c("Estadual", "Municipal")),
         ID_LOCALIZACAO = factor(ID_LOCALIZACAO, labels = c("Urbana", "Rural")),
         ) %>% 
  left_join(censo, by = c("ID_ESCOLA" = "CO_ENTIDADE")) %>% 
  left_join(municipios, by = c("ID_MUNICIPIO" = "CO_MUNICIPIO")) %>% 
  left_join(pei_eti, by = c("ID_ESCOLA" = "cod_escola")) 

df_publico <- df_publico %>% 
  mutate(tipo_ensino = case_when(
    IN_COMUM_MEDIO_INTEGRADO == 1 ~ 'ETEC',
    pei == "SIM" ~ "PEI",
    eti == "SIM" ~ "ETI",
    TRUE ~ "Tradicional"
  ),
  tipo_publico = if_else(ano_implantacao > 2019 & !is.na(ano_implantacao), "Tradicional",tipo_ensino)) %>% 
  select(- pei, 
         - eti,
         -TP_SITUACAO_FUNCIONAMENTO,
         -IN_COMUM_MEDIO_INTEGRADO,
         - ano_implantacao,
         -tipo_ensino)
       
colnames(df_publico) <- tolower(colnames(df_publico))

df_publico %>% 
  count(tipo_publico)

write.csv2(df_publico, "output/books/df_publico.csv", row.names = FALSE, na = "")




