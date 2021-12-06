library(tidyverse)
library(caret)
options(scipen = 999)

url <-  getwd()
url

# Importa ----
# IEE
iee <- read.csv2(paste0(url,"/output/v2/iee_5ef_v2.csv"))

# Questionario Escola
saeb_escola <- read.csv(paste0(url,"/data/microdados_saeb_2019_v2/DADOS/TS_ESCOLA.csv"))
colnames(saeb_escola) <- tolower(colnames(saeb_escola))

saeb_escola <- 
  saeb_escola %>% 
  filter(id_uf == 35,
         id_dependencia_adm == 2) %>% 
  transmute(id_escola,
            id_area = factor(id_area, labels = c("Capital", "Interior")),
             pc_formacao_docente_inicial,
             pc_formacao_docente_final,
             pc_formacao_docente_medio)

# Questionario diretor
saeb_diretor <- read_csv(paste0(url,"/data/microdados_saeb_2019_v2/DADOS/TS_DIRETOR.csv"), na = c(""))
colnames(saeb_diretor) <- tolower(colnames(saeb_diretor))
saeb_diretor <- 
  saeb_diretor %>% 
  filter(id_uf == 35,
         id_dependencia_adm == 2,
         in_preenchimento_questionario == 1) %>% 
  select(id_escola,
            starts_with('tx'))

saeb_diretor %>% 
  group_by(id_escola) %>% 
  count() %>% 
  arrange(desc(n))

# Cruza 
data <- iee %>% 
  select(
    id_escola,
    media, 
    diff) %>% 
  left_join(
    saeb_diretor, by = "id_escola"
  )

data_t <- data %>% 
  mutate_if(is.numeric,~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% 
  mutate_if(is.character, ~replace(., is.na(.), "NA")) %>% 
  mutate_if(is.character, as.factor)

## Remove variáveis com zero ou baixa variância

remove_cols <- nearZeroVar(data_t, names = TRUE)
remove_cols 


total_vars <- colnames(data_t)
final_cols <- setdiff(total_vars, remove_cols)

data_final <- data_t[, final_cols]
paste("Nº colunas iniciais:", ncol(data_t))
paste("Nº colunas finais:", ncol(data_final))

vars_modelo <- colnames(data_final)[-c(1:3)]

# COm o efeito escola não tem relação
y_resp <- "media"

tb_r2 <- data.frame(coluna = vars_modelo)


rsquared <- c()

for (variable in vars_modelo) {
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, data_final)
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}
tb_r2$rsquared <- rsquared
