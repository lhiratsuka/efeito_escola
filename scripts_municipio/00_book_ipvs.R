library(tidyverse)

#Import ----
## Publico
df_publico <- read.csv2("output/books/df_publico.csv")
## IPVS
ipvs <- read.csv2("data/regionais/BaseIPVS2010.csv")

# Trata ipvs -----
ipvs <- ipvs %>%
  select(
    v61,
    v10:v35,
    v40:v49
  )


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
colnames(ipvs_spread) <- c("co_distrito","ipvs0", "ipvs1","ipvs2","ipvs3","ipvs4","ipvs5","ipvs6","ipvs7")

ipvs_new2 <- ipvs_new %>% 
  left_join(ipvs_spread, by = c("v61" = "co_distrito")) %>% 
  mutate(ipvs567 = ipvs5 + ipvs6 + ipvs7)

# Agrupa Por distrito ----

ipvs_dist <- ipvs_new2 %>% 
  group_by(v61) %>% 
  summarise_all(., ~mean(.x, na.rm = TRUE)) %>% 
  select(-v10) %>% 
  rename(co_distrito = v61) %>% 
  rename_at(vars(-c(co_distrito)), ~paste0(., "_ipvs")) 

anyDuplicated(ipvs_dist$co_distrito)
write.csv2(ipvs_dist, "output/books/book_ipvs.csv", row.names = FALSE, na = "")
