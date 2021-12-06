library(tidyverse)
library(caret)
library(readxl)
options(scipen = 999)

# Import ----

## ranking ----
iee5ef <- read.csv2("output/v2/iee_5ef_v2.csv")
iee9ef <- read.csv2("output/v2/iee_9ef_v2.csv")
iee3em <- read.csv2("output/v2/iee_3em_v2.csv")

## quest.escola ----
saeb_escola <- read.csv("data/microdados_saeb_2019_v2/DADOS/TS_ESCOLA.csv")
colnames(saeb_escola) <- tolower(colnames(saeb_escola))

## quest.diretor ----
saeb_diretor <- read_csv("data/microdados_saeb_2019_v2/DADOS/TS_DIRETOR.csv", na = c(""))
label_diretor <- read_xlsx("data/label_questoes_saeb_2019.xlsx", sheet = "diretor")
colnames(saeb_diretor) <- tolower(colnames(saeb_diretor))

## quest.professor ----
saeb_professor <- read_csv("data/microdados_saeb_2019_v2/DADOS/TS_PROFESSOR.csv", na = c(""))
label_professor <- read_xlsx("data/label_questoes_saeb_2019.xlsx", sheet = "professor")
colnames(saeb_professor) <- tolower(colnames(saeb_professor))

## Censo ----
censo <- read_csv2("data/microdados_educacao_basica_2019/microdados_educacao_basica_2019/DADOS_SP/ESCOLAS2.CSV", na = c(""))
colnames(censo) <- tolower(colnames(censo))

## idesp ----
idesp <- read_xlsx("data/ESCOLA_DE_IDES_INSE.xlsx", na = c("-"), skip = 1)
nomes <- c("municipio", "id_escola", "cod_diretoria", "nome_diretoria", "nome_escola", "meta2019_ai", "idesp2019_ai", "atingiu_ai","meta2019_af", "idesp2019_af", "atingiu_af", "meta2019_em", "idesp2019_em", "atingiu_em","inse")
colnames(idesp) <- nomes

## Indicadores escola
indicadores <- read_xlsx("data/Indicadores_painel.xlsx",na = c("--"), skip = 1)


# Empilha iee ----
iee <- rbind(iee5ef,iee9ef,iee3em)

# Painel principal ----
# Trata base de escolas
saeb_escola <- 
  saeb_escola %>% 
  filter(id_uf == 35,
         id_dependencia_adm == 2) %>% 
  transmute(id_escola,
            id_area = factor(id_area, labels = c("Capital", "Interior")),
            pc_formacao_docente_inicial,
            pc_formacao_docente_final,
            pc_formacao_docente_medio)


idesp <- idesp %>% 
  select(id_escola,
         nome_diretoria,
         meta2019_ai:inse) 

indicadores <- indicadores %>% 
  select(id_escola,
         COMP_GEST,	
         ALUNOS_TU_EF,	
         ALUNOS_TU_EM,	
         HORAS_AULA_EF,	
         HORAS_AULA_EM,	
         ED_12_AI,
         ED_56_AI,	
         ED_12_AF, 
         ED_56_AF,	
         ED_12_EM, 
         ED_456_EM) %>% 
  mutate(across(ALUNOS_TU_EF:ED_456_EM, as.numeric))

colnames(indicadores) <- tolower(colnames(indicadores))

output_escola <- iee %>% 
  left_join(saeb_escola, by = "id_escola") %>% 
  left_join(idesp, by = "id_escola") %>% 
  left_join(indicadores, by = "id_escola") %>% 
  mutate(
    comp_gest = as.numeric(gsub(".*?([0-9]+).*", "\\1", comp_gest)),
    pc_formacao_docente = case_when(
      id_serie == '5EF' ~ pc_formacao_docente_inicial,
      id_serie == '9EF' ~ pc_formacao_docente_final,
      id_serie == '3EM' ~ pc_formacao_docente_medio),
    meta_idesp = case_when(
      id_serie == '5EF' ~ meta2019_ai,
      id_serie == '9EF' ~ meta2019_af,
      id_serie == '3EM' ~ meta2019_em),
    idesp = case_when(
      id_serie == '5EF' ~ idesp2019_ai,
      id_serie == '9EF' ~ idesp2019_af,
      id_serie == '3EM' ~ idesp2019_em),
    atingiu_meta = case_when(
      id_serie == '5EF' ~ atingiu_ai,
      id_serie == '9EF' ~ atingiu_af,
      id_serie == '3EM' ~ atingiu_em),
    alunos_tu = case_when(
      id_serie == '5EF' ~ alunos_tu_ef,
      id_serie == '9EF' ~ alunos_tu_ef,
      id_serie == '3EM' ~ alunos_tu_em),
    horas_aula = case_when(
      id_serie == '5EF' ~ horas_aula_ef,
      id_serie == '9EF' ~ horas_aula_ef,
      id_serie == '3EM' ~ horas_aula_em),
    ed_12 = case_when(
      id_serie == '5EF' ~ ed_12_ai,
      id_serie == '9EF' ~ ed_12_af,
      id_serie == '3EM' ~ ed_12_em),
    ed_56 = case_when(
      id_serie == '5EF' ~ ed_56_ai,
      id_serie == '9EF' ~ ed_56_af,
      id_serie == '3EM' ~ ed_456_em)
    ) %>% 
  select(-c(pc_formacao_docente_inicial,
            pc_formacao_docente_final,
            pc_formacao_docente_medio
            ),
         -ends_with(c('ai','ef','em','af'))
         )


diretoria <- output_escola %>% 
  group_by(id_serie,nome_diretoria) %>% 
  summarise(alunos_diretoria = sum(nu_matriculados_censo),
            inse_diretoria = weighted.mean(inse,nu_matriculados_censo, na.rm = TRUE),
            media_diretoria = weighted.mean(media,nu_matriculados_censo, na.rm = TRUE),
            iee_diretoria = weighted.mean(diff,nu_matriculados_censo, na.rm = TRUE)
            ) %>% 
  filter(!is.na(nome_diretoria))


painel_escola <- output_escola %>% 
  left_join(diretoria, by = c("nome_diretoria", "id_serie"))

write.csv2(painel_escola, "output/painel/painel_escolas.csv", row.names = FALSE, na = "")

# Painel diretor ----
# Trata base de diretor
saeb_diretor <- 
  saeb_diretor %>% 
  filter(id_uf == 35,
         id_dependencia_adm == 2,
         in_preenchimento_questionario == 1) %>% 
  select(id_escola,
         starts_with('tx')) %>% 
  group_by(id_escola) %>% 
  mutate(id_diretor = row_number()) %>% 
  ungroup()

saeb_diretor %>% filter(id_diretor >1) %>% nrow()
## Cruza
data <- output_escola %>% 
  left_join(
    saeb_diretor, by = "id_escola"
  ) 

data %>% 
  group_by(id_escola) %>% 
  count() %>% 
  group_by(n) %>% count()

## Gather
data_gather <- data %>% 
  select(id_escola,
         no_entidade,
         no_municipio,
         id_localizacao,
         id_area,
         nivel_socio_economico,
         tipo_publico,
         nome_diretoria,
         id_diretor,
         starts_with('tx_resp')) %>% 
  distinct() %>% 
  gather("id_questao", "resposta", -c(id_escola, no_entidade, no_municipio,id_localizacao,id_area,nivel_socio_economico,tipo_publico,
                                      nome_diretoria,id_diretor))

label_questao <- label_diretor %>% 
  distinct(id_questao, tipo_diretor, questao_diretor)

label_resposta <- label_diretor %>% 
  distinct(id_questao, alt_diretor, resposta_diretor)

painel_diretor <-  data_gather %>% 
  left_join(label_questao, by = "id_questao") %>% 
  left_join(label_resposta, by = c("id_questao", "resposta" = "alt_diretor")) %>% 
  mutate(resposta_f = coalesce(resposta_diretor,resposta),
         resposta_f = if_else(is.na(resposta_f), "Sem resposta", resposta_f)
         ) %>% 
  filter(!id_questao %in% c('tx_resp_q252','tx_resp_q253'))

    

write.csv2(painel_diretor, "output/painel/painel_diretor.csv", row.names = FALSE, na = "")


# Painel censo ----
data <- output_escola %>% 
  left_join(
    saeb_diretor, by = "id_escola"
  ) 

data %>% 
  group_by(id_escola) %>% 
  count() %>% 
  group_by(n) %>% count()

## Gather
output_censo <- output_escola %>% 
  select(id_escola,
         id_serie,
         no_municipio,
         id_localizacao,
         id_area,
         nivel_socio_economico,
         tipo_publico,
         nome_diretoria) %>% 
left_join(censo, by = c("id_escola" = "co_entidade"))

write.csv2(output_censo, "output/painel/painel_censo.csv", row.names = FALSE, na ="")
