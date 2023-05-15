library(tidyverse)
# Import df_publico ------

df_publico <- read.csv2("output/books/df_publico.csv")
df_publico <- df_publico %>% 
  filter(id_dependencia_adm == 'Estadual') %>% 
  select(id_serie,
         id_escola,
         id_municipio,
         no_municipio,
         co_distrito,
         media)


ipvs <- read.csv2("data/regionais/BaseIPVS2010.csv")
options(scipen = 999)
# Qtd de distritos por municipio ------------
qtd_distritos <- ipvs %>% group_by(v2) %>% 
  summarise(qtd_distritos = n_distinct(v61),
            qtd_setor = n_distinct(v7)) %>% 
  mutate(fl_tem_distrito = if_else(qtd_distritos > 1, 1, 0),
         fl_tem_setor = if_else(qtd_setor > 1, 1, 0)
         ) %>% 
  arrange(desc(qtd_distritos))
head(qtd_distritos)

# % de municipios com distrito e setor
mean(qtd_distritos$fl_tem_distrito)
mean(qtd_distritos$fl_tem_setor)

# media de distritos e setores
mean(qtd_distritos$qtd_distritos)
mean(qtd_distritos$qtd_setor)

write.csv2(qtd_distritos, "output/analises/qtd_distritos.csv", row.names = FALSE, na = "")

# Trata ipvs -----
ipvs <- ipvs %>%
  select(
    v1, 
    v2,
    v61,
    v62,
    v10:v35,
    v40:v49
  ) %>% 
  mutate(v10 = if_else(v10 == 0, NA_integer_, v10))


# Novas variaveis
ipvs_new <- ipvs %>% 
   mutate(pc_domicilios_ate2sm = v20 + v21 + v22 + v23,
          fl_domicilio_ruim = if_else(v40 < 80 | v41 < 80 | v42 < 80 | v43 < 80, 0, 1)
            )

# Transformacoes do IPVS

ipvs_grou <- 
  ipvs %>% 
  group_by(v61, v10) %>% 
  summarise(qtd_grupo = sum(v14, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(v61) %>% 
  mutate(total = sum(qtd_grupo, na.rm = TRUE),
         pc_grupo = round(qtd_grupo/total,2)) 

head(ipvs_grou)

ipvs_spread <- ipvs_grou %>% 
  select(v61, v10, pc_grupo) %>% 
  spread(v10, pc_grupo) %>% 
  mutate_all(~replace(., is.na(.),0))
colnames(ipvs_spread) <- c("co_distrito", "ipvs1","ipvs2","ipvs3","ipvs4","ipvs5","ipvs6","ipvs7","ipvsna")

ipvs_new2 <- ipvs_new %>% 
  left_join(ipvs_spread, by = c("v61" = "co_distrito")) %>% 
  mutate(ipvs567 = ipvs5 + ipvs6 + ipvs7)

## Por municipio ----
ipvs_mun <- ipvs %>% 
  select(-c(v2,v61,v62)) %>% 
  group_by(v1) %>% 
  summarise_all(., ~mean(.x, na.rm = TRUE))

df_ipvs_mun <- df_publico %>% 
  left_join(ipvs_mun, by = c("id_municipio" = "v1"))

y_resp <- "media"
vars_modelo <- colnames(df_ipvs_mun)[-c(1:6)]


tb_r2 <- data.frame(var = vars_modelo)
rsquared <- c()

for (variable in vars_modelo) {
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, df_ipvs_mun)
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}
tb_r2$rsquared <- rsquared

tb_r2 %>% arrange(desc(rsquared)) %>% head(10)

## Por distrito ----

ipvs_dist <- ipvs_new2 %>% 
  select(-c(v1,v2,v62)) %>% 
  group_by(v61) %>% 
  summarise_all(., ~mean(.x, na.rm = TRUE)) 


df_ipvs_dist <- df_publico %>% 
  left_join(ipvs_dist, by = c("co_distrito" = "v61"))
head(df_ipvs_dist)

vars_modelo <- colnames(df_ipvs_dist)[-c(1:6)]
tb_r2 <- data.frame(var = vars_modelo)
tb_r2
rsquared <- c()
for (variable in vars_modelo) {
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, df_ipvs_dist %>% filter(id_serie == '5EF'))
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}
tb_r2$r2_dist_5EF <- rsquared

rsquared <- c()
for (variable in vars_modelo) {
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, df_ipvs_dist %>% filter(id_serie == '9EF'))
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}
tb_r2$r2_dist_9EF <- rsquared

rsquared <- c()
for (variable in vars_modelo) {
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, df_ipvs_dist %>% filter(id_serie == '3EM'))
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}
tb_r2$r2_dist_3EM <- rsquared

vars <- c("co_distrito","id_serie")
vars5ef <- append(vars,(tb_r2 %>% filter(r2_dist_5EF >= 0.05) %>% arrange(desc(r2_dist_5EF)) %>% head(10))$var)
vars9ef <- append(vars,(tb_r2 %>% filter(r2_dist_9EF >= 0.05) %>% arrange(desc(r2_dist_9EF)) %>% head(10))$var)
vars3em <- append(vars,(tb_r2 %>% filter(r2_dist_3EM >= 0.05) %>% arrange(desc(r2_dist_3EM)) %>% head(10))$var)


write.csv2(tb_r2, "output/analises/selecao_ipvs.csv", row.names = FALSE, na = "")

# Usar apenas IPVS e v20:Proporção de Domicílios sem Renda per Capita

ipvs5ef <- df_ipvs_dist[,vars5ef] %>% filter(id_serie == '5EF') 
ipvs9ef <- df_ipvs_dist[,vars9ef] %>% filter(id_serie == '9EF')
ipvs3em <- df_ipvs_dist[,vars3em] %>% filter(id_serie == '3EM')

ipvs5ef <- ipvs5ef %>% rename_at(vars(-c(co_distrito,id_serie)), ~paste0(., "_ipvs"))
ipvs9ef <- ipvs9ef %>% rename_at(vars(-c(co_distrito,id_serie)), ~paste0(., "_ipvs"))
ipvs3em <- ipvs3em %>% rename_at(vars(-c(co_distrito,id_serie)), ~paste0(., "_ipvs"))

write.csv2(ipvs5ef, "output/books/ipvs5ef.csv", row.names = FALSE, na = "")
write.csv2(ipvs9ef, "output/books/ipvs9ef.csv", row.names = FALSE, na = "")
write.csv2(ipvs3em, "output/books/ipvs3em.csv", row.names = FALSE, na = "")

