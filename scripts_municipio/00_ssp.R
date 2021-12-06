
#remotes::install_github("abjur/brcrimR")
library(corrplot)
library(brcrim)
library(tidyverse)
library(readxl)

df_2019 <-  purrr::map_dfr(1:645, ~brcrimR::get_summarized_table_sp(year = '2019', city = .x))
df_2019 <- janitor::clean_names(df_2019)    
municipios <- read_excel("data/regionais/depara_municipio_ssp.xlsx")                   

unique(df_2019$natureza)
df_2019 %>% group_by(natureza) %>% summarise(n = sum(total)) %>%  arrange(desc(n)) %>% view()

crimes_spp <- df_2019 %>% 
  select(municipio,
         natureza,
         total) %>%
  mutate(natureza_g = fct_collapse(df_2019$natureza,
               furto = c("FURTO - OUTROS", "FURTO DE VEÍCULO"),
               roubo = c("ROUBO - OUTROS","TOTAL DE ROUBO - OUTROS (1)", "ROUBO DE VEÍCULO", "ROUBO DE CARGA","ROUBO A BANCO"),
               homicidio = c("HOMICÍDIO DOLOSO (2)","TENTATIVA DE HOMICÍDIO","Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO (3)","HOMICÍDIO DOLOSO (2)","Nº DE VÍTIMAS EM LATROCÍNIO","Nº DE VÍTIMAS EM LATROCÍNIO","HOMICÍDIO CULPOSO OUTROS","LESÃO CORPORAL SEGUIDA DE MORTE","LATROCÍNIO"),
               acidente_transito = c("HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO","HOMICÍDIO CULPOSO POR ACIDENTE DE TRÂNSITO", "LESÃO CORPORAL CULPOSA POR ACIDENTE DE TRÂNSITO","HOMICÍDIO CULPOSO POR ACIDENTE DE TRÂNSITO","Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO","HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO"),
               lesao_corporal = c("LESÃO CORPORAL DOLOSA","LESÃO CORPORAL CULPOSA - OUTRAS"),
               estupro = c("TOTAL DE ESTUPRO (4)","ESTUPRO DE VULNERÁVEL","ESTUPRO")
               ))
  
crimes_spp_group <- crimes_spp %>% 
  group_by(municipio,
           natureza_g) %>% 
  summarise(total = sum(total)) %>% 
  ungroup() %>% 
  left_join(municipios, by = c("municipio" = "id")) %>% 
  select(cod_ibge,
         municipio_ibge,
         natureza_g,
         total)


crimes_ssp_spread <- crimes_spp_group %>% 
  pivot_wider(names_from = natureza_g, values_from = total) %>% 
  mutate(total = rowSums(crimes_ssp_spread[,3:8]))
  
write.csv2(crimes_ssp_spread,"data/regionais/ssp_2019.csv", row.names = FALSE, na = "")


# Analise univariada 5EF ------------------------------------------------------
saeb5ef <-  read.csv2("output/books/base5ef.csv")

df5ef <- saeb5ef %>% 
  filter(id_dependencia_adm == 'Municipal') %>% 
  select(media,
         id_municipio) %>% 
  left_join(crimes_ssp_spread, by = c("id_municipio" = "cod_ibge")) %>% 
  select(-id_municipio,-municipio_ibge)

target <- "media"
vars <- colnames(df5ef)[-1]
tb_r2 <- data.frame(vars = vars)
r2 <- c()

for (var in vars) {
  lm_formula <- as.formula(str_glue("{target} ~ {var}"))
  model_lm <- lm(lm_formula, df5ef)
  r2 <- append(r2, summary(model_lm)$r.squared)
}
tb_r2$r2 <- r2
tb_r2

corrplot(cor(df5ef[,3:8]), method = "number")
# Analise univariada 9EF ------------------------------------------------------
saeb9ef <-  read.csv2("output/books/base9ef.csv") 

df9ef <- saeb9ef %>% 
  filter(id_dependencia_adm == 'Municipal') %>% 
  select(media,
         id_municipio) %>% 
  left_join(crimes_ssp_spread, by = c("id_municipio" = "cod_ibge")) %>% 
  select(-id_municipio,-municipio_ibge)

target <- "media"
vars <- colnames(df9ef)[-1]
tb_r2 <- data.frame(vars = vars)
r2 <- c()

for (var in vars) {
  lm_formula <- as.formula(str_glue("{target} ~ {var}"))
  model_lm <- lm(lm_formula, df9ef)
  r2 <- append(r2, summary(model_lm)$r.squared)
}
tb_r2$r2 <- r2
tb_r2


corrplot(cor(df9ef[,3:8]), method = "number")

