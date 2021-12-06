library(tidyverse)
library(caret)
library(readxl)
options(scipen = 999)
# Import ----

saeb5ef <-  read.csv2("output/books/base5ef.csv")
saeb9ef <-  read.csv2("output/books/base9ef.csv") 


colSums(is.na(saeb9ef))


metadados <- read_xlsx("selecao_variaveis.xlsx", sheet = "labels")
vars_base <- colnames(saeb5ef)[1:17]

# NZV ---------------------------------------------------------------------

selecao <- function(data) {
  # Filtra escolas municipais ----
  saeb_filter <- data %>% 
    filter(id_dependencia_adm == 'Municipal') %>% 
    select(id_escola,
           media,
           nivel_socio_economico,
           starts_with('tx'),
           ends_with('ipvs'),
           ends_with('imp'),
           -c(v44_ipvs,v45_ipvs,v47_ipvs))
  dim(saeb_filter)
  
  
  # Remove NZV -----
  remove_cols <- nearZeroVar(saeb_filter, names = TRUE)
  remove_cols
  
  total_cols <- colnames(saeb_filter)
  final_cols <- setdiff(total_cols, remove_cols)
  
  saeb_filter[,final_cols]

  
}

saeb5ef_nzv <- selecao(saeb5ef)
saeb9ef_nzv <- selecao(saeb9ef)



# Impute missing ----------------------------------------------------------
colSums(is.na(saeb5ef_nzv))

impute_missing <- function(data) {
  saeb5ef_nzv %>% 
    mutate(
      nascidos_vivos_mae18_2019_imp = replace_na(nascidos_vivos_mae18_2019_imp, mean(nascidos_vivos_mae18_2019_imp, na.rm = T))
    )
}
saeb5ef_nzv <- impute_missing(saeb5ef_nzv)
saeb9ef_nzv <- impute_missing(saeb9ef_nzv)

# Boruta ------------------------------------------------------------------
set.seed(111)
train_boruta <- Boruta::Boruta(media~., data = saeb5ef_nzv, doTrace = 0)

iv5ef <- data.frame(Boruta::attStats(train_boruta))
iv5ef <- tibble::rownames_to_column(iv5ef, "var") %>% view()
vars_5ef <- (iv5ef %>%  
    filter(decision == 'Confirmed') %>% 
    arrange(desc(medianImp)) %>% 
    head(40)
  )$var

saeb5ef_boruta <- saeb5ef %>% select(all_of(c(vars_base,vars_5ef)))

write.csv2(iv5ef, "output/analises/boruta_escolas_municipais_5ef.csv", row.names = FALSE, na = "")
write.csv2(saeb5ef_boruta, "output/selecao/escolas_municipais/selecao_saeb5ef.csv", row.names = FALSE, na = "")


set.seed(111)
train_boruta <- Boruta::Boruta(media~., data = saeb9ef_nzv, doTrace = 0)

iv9ef <- data.frame(Boruta::attStats(train_boruta))
iv9ef <- tibble::rownames_to_column(iv9ef, "var") %>% view()
vars_9ef <- (iv9ef %>%  
               filter(decision == 'Confirmed') %>% 
               arrange(desc(medianImp)) %>% 
               head(40)
)$var

saeb9ef_boruta <- saeb9ef %>% select(all_of(c(vars_base,vars_9ef)))

write.csv2(iv9ef, "output/analises/boruta_escolas_municipais_9ef.csv", row.names = FALSE, na = "")
write.csv2(saeb9ef_boruta, "output/selecao/escolas_municipais/selecao_saeb9ef.csv", row.names = FALSE, na = "")



