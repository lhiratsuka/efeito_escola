df_publico <- read.csv2("output/books/df_publico.csv")
ranking5ef <- read.csv2("output/v1/ranking5ef.csv")
ranking9ef <- read.csv2("output/v1/ranking9ef.csv")
ranking3em <- read.csv2("output/v1/ranking3em_lm.csv")

trata_ranking <- function(data) {
  data %>% 
    select(
      id_escola,
      media,
      nota_estimada,
      diff,
      ranking_nota,
      ranking_iee,
      diff_ranking,
      grupo_ranking_nota,
      grupo_ranking_iee
    )
}

iee5ef <- trata_ranking(ranking5ef)
iee9ef <- trata_ranking(ranking9ef)
iee3em <- trata_ranking(ranking3em)

base_final <- function(data, serie) {
  df_publico %>% 
    select(-starts_with('media')) %>% 
    mutate(tx_participacao = round(nu_presentes/nu_matriculados_censo,2)) %>% 
    filter(id_dependencia_adm == 'Estadual',
           id_serie == serie) %>% 
    left_join(data, by = "id_escola")
}

df5ef <- base_final(iee5ef, '5EF') 
df9ef <- base_final(iee9ef, '9EF')
df3em <- base_final(iee3em, '3EM')

write.csv2(df5ef, "output/v1/iee_5ef_v1.csv", row.names = FALSE, na = "")
write.csv2(df9ef, "output/v1/iee_9ef_v1.csv", row.names = FALSE, na = "")
write.csv2(df3em, "output/v1/iee_3em_v1.csv", row.names = FALSE, na = "")

