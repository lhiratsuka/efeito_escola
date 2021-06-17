library(tidyverse)

saresp_alunos <- read_csv2("data/saresp/Questionario_alunos_2019.csv", na = c(NA,"NULL", "", "*"))

head(saresp_alunos)
colnames(saresp_alunos) <- tolower(colnames(saresp_alunos))




mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


saresp_alunos_trata <- saresp_alunos %>% 
  filter(!is.na(q01),
         !is.na(q49)) %>% 
  mutate(id_escola = as.numeric(paste0("35",cd_ue))
         ) %>% 
  select(cd_aluno,
         id_escola,
         cd_ue,
         serie,
         #nomesc,
         #cd_mun,
         #nm_mun,
         #serie,
         q16,
         q19:q21)
  
  
#saresp_5EF <- saresp_5EF %>% mutate_at(vars(q01:q49), ~replace(., is.na(.), ".")

saresp_mode <- saresp_alunos_trata %>% 
  group_by(id_escola,serie) %>% 
  summarise_if(is.character, mode) %>% 
  arrange(cd_ue) %>% 
  rename_with(~gsub("q", "mode_q", .x, fixed = TRUE)) %>% 
  rename_at(vars(-id_escola,-serie), ~paste0(., "_spa"))


saresp5ef_mode <- saresp_mode %>% filter(serie == '5º Ano EF')
saresp9ef_mode <- saresp_mode %>% filter(serie == '9º Ano EF')
saresp3em_mode <- saresp_mode %>% filter(serie == 'EM-3ª série')

write.csv2(saresp5ef_mode,"output/books/saresp5ef_mode.csv", row.names = FALSE, na = "")
write.csv2(saresp9ef_mode, "output/books/saresp9ef_mode.csv", row.names = FALSE, na = "")
write.csv2(saresp3em_mode, "output/books/saresp3em_mode.csv", row.names = FALSE, na = "")



