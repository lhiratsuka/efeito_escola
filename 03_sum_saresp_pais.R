library(tidyverse)
saresp_pais <- read_csv2("data/saresp/Questionario_pais_2019.csv", na = c(NA,"NULL", "", "*"))

colnames(saresp_pais) <- tolower(colnames(saresp_pais))

colSums(is.na(saresp_pais))/nrow(saresp_pais)

mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


saresp_pais_trata <- saresp_pais %>% 
  filter(!is.na(q01),
         !is.na(q50)) %>% 
  mutate(id_escola = as.integer(paste0("35",cd_ue))
  ) %>% 
  select(cd_aluno,
         id_escola,
         cd_ue,
         serie,
         #nomesc,
         #cd_mun,
         #nm_mun,
         #serie,
         q12,
         q18:q50) %>% 
  mutate(q19e20 = paste0(q19,q20)) %>% 
  arrange(id_escola)

# saresp_pais_trata %>% 
#   group_by(q19_q20) %>% 
#   summarise(n = n()/nrow(saresp_pais)*100) %>% view()

#saresp_5EF <- saresp_5EF %>% mutate_at(vars(q01:q49), ~replace(., is.na(.), ".")

saresp_mode <- saresp_pais_trata %>% 
  group_by(id_escola,serie) %>% 
  summarise_if(is.character, mode) %>% 
  arrange(cd_ue) %>% 
  rename_with(~gsub("q", "mode_pais_q", .x, fixed = TRUE))

saresp_mode %>% group_by(mode_pais_q19e20) %>% summarise(n = n()/nrow(saresp_mode)) %>% arrange(desc(n))

saresp_mode_collapse <- saresp_mode %>% 
  mutate(mode_pais_q19e20 = case_when(
                                      mode_pais_q19e20 == "DD" ~ 'DD',
                                      mode_pais_q19e20 == "BB" ~ 'BB',
                                      mode_pais_q19e20 == "CC" ~'CC',
                                      mode_pais_q19e20 == "AA" ~'AA',
                                      str_detect(mode_pais_q19e20,"E|F") ~'E+',
                                      TRUE ~'Outros'
                                         ))

saresp_mode_collapse %>% group_by(mode_pais_q19e20) %>% summarise(n = n()/nrow(saresp_mode)) %>% arrange(desc(n))
saresp5ef_mode <- saresp_mode_collapse %>% filter(serie == '5º Ano EF')
saresp9ef_mode <- saresp_mode_collapse %>% filter(serie == '9º Ano EF')
saresp3em_mode <- saresp_mode_collapse %>% filter(serie == 'EM-3ª série')



write.csv2(saresp5ef_mode,"output/books/saresp5ef_pais_mode.csv", row.names = FALSE, na = "")
write.csv2(saresp9ef_mode, "output/books/saresp9ef_pais_mode.csv", row.names = FALSE, na = "")
write.csv2(saresp3em_mode, "output/books/saresp3em_pais_mode.csv", row.names = FALSE, na = "")