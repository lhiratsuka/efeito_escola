library(tidyverse)

# Import

## Publico
df_publico <- read.csv2("output/books/df_publico.csv")
head(df_publico)
colnames(df_publico)

## Saeb
saeb5ef <-  read.csv2("output/books/saeb5ef_prop.csv") 
saeb9ef <-  read.csv2("output/books/saeb9ef_prop.csv") 
saeb3em <-  read.csv2("output/books/saeb3em_prop.csv") 

# IPVS
ipvs <- read.csv2("output/books/book_ipvs.csv")

# Seade
seade2010 <- read_csv2("data/regionais/seade_2010.csv", locale = locale(encoding = "ISO-8859-1"), col_names = TRUE, col_types = NULL)
colnames(seade2010) <- c("no_municipio","ano", "idh_longevidade_2010_imp", "coleta_lixo_2010_imp", "esgoto_2010_imp", "id_municipio")
head(seade2010)
seade2010 <- seade2010 %>% select(-no_municipio,-ano)

seade2019 <- read_csv2("data/regionais/seade_2019.csv", locale = locale(encoding = "ISO-8859-1"), col_names = TRUE, col_types = "cddddd", na = c("-"))
colnames(seade2019) <- c("no_municipio","ano", "nascidos_vivos_mae18_2019_imp", "rendimento_medio_2019_imp", "ind_envelhecimento_2019_imp", "id_municipio")
head(seade2019)
seade2019 <- seade2019 %>% select(-no_municipio,-ano)

# SSP
ssp <- read.csv2("data/regionais/ssp_2019.csv")
ssp <- ssp %>% transmute(cod_ibge, qtd_crime = total)
# Cruza tudo
cruza <- function(serie, saeb) {
  df_publico %>% 
    filter(id_serie == serie,
           id_dependencia_adm == 'Municipal') %>% 
    left_join(saeb, by = "id_escola") %>% 
    left_join(ipvs, by = "co_distrito") %>% 
    left_join(seade2010, by = "id_municipio") %>% 
    left_join(seade2019, by = "id_municipio") %>% 
    left_join(ssp, by = c("id_municipio" = "cod_ibge"))
    
}

df5ef <- cruza('5EF',saeb5ef)
df9ef <- cruza('9EF',saeb9ef)
df3em <- cruza('3EM',saeb3em)

write.csv2(df5ef, "output/books/base5ef.csv", row.names = FALSE, na = "")
write.csv2(df9ef, "output/books/base9ef.csv", row.names = FALSE, na = "")

# baixo volume
write.csv2(df3em, "output/books/base3em.csv", row.names = FALSE, na = "")