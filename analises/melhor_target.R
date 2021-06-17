library(tidyverse)
library(caret)

#Publico
df_publico <- read.csv2("output/books/df_publico.csv")
#Saeb
saeb <- read.csv2("output/books/saeb3em_mode.csv")


# Base de público

df_publico <- df_publico %>% 
  filter(id_serie == '3EM') %>% 
  select(-c(tp_situacao_funcionamento,in_comum_medio_integrado,ano_implantacao,tipo_ensino))

df_publico %>% select(media_lp, media_mt, media) %>% summary()
# Filtra escolas estaduais da série
saeb_filter <- saeb %>% 
  filter(id_escola %in% df_publico$id_escola)

# Remove variáveis com zero ou baixa variância
remove_cols <- nearZeroVar(saeb_filter, names = TRUE)
remove_cols 

total_vars <- colnames(saeb)
final_cols <- setdiff(total_vars, remove_cols)

saeb_final <- saeb_filter[, final_cols]


# Cruza publico com Saeb
data <- df_publico %>% 
  select(id_escola,
         starts_with("media"),
         nivel_socio_economico) %>% 
  mutate(media_lp_std = scale(media_lp),
         media_mt_std = scale(media_mt),
         media_std = (media_lp_std+media_mt_std)/2,
         ) %>% 
  left_join(saeb_final, by = "id_escola")

head(data) 

# Modelos Lp x Mt x Media -----
#LP
model_lp <- lm(media_lp ~ .,
               data = data %>% select(media_lp, nivel_socio_economico, starts_with("mode")))
r2_lp <- summary(model_lp)$r.squared
r2_lp

#MT
model_mt <- lm(media_mt ~ .,
               data = data %>% select(media_mt, nivel_socio_economico, starts_with("mode")))
r2_mt <- summary(model_mt)$r.squared
r2_mt

#Media
model_media <- lm(media ~ .,
               data = data %>% select(media, nivel_socio_economico, starts_with("mode")))
r2_media <- summary(model_media)$r.squared
r2_media

# Modelos Lp x Mt x Media Standard -----
#LP
model_lp <- lm(media_lp_std ~ .,
               data = data %>% select(media_lp_std, nivel_socio_economico, starts_with("mode")))
r2_lp <- summary(model_lp)$r.squared
r2_lp

#MT
model_mt <- lm(media_mt_std ~ .,
               data = data %>% select(media_mt_std, nivel_socio_economico, starts_with("mode")))
r2_mt <- summary(model_mt)$r.squared
r2_mt

#Media
model_media <- lm(media_std ~ .,
                  data = data %>% select(media_std, nivel_socio_economico, starts_with("mode")))
r2_media <- summary(model_media)$r.squared
r2_media