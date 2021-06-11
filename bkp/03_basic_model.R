
df <- ts_resultado_aluno2011 %>% 
  filter(ID_UF == 35) %>% 
  left_join(
    censo_escolar[,c("PK_COD_ENTIDADE", "NO_ENTIDADE")], 
                     by = c("ID_ESCOLA" = "PK_COD_ENTIDADE")) %>% 
  left_join(ibge, by = c("ID_MUNICIPIO" = "pk"))


ts_quest_aluno_2011 <- ts_quest_aluno_2011 %>% 
  select(ID_ALUNO, TX_RESP_Q001:TX_RESP_Q058)

df <- df %>% 
  left_join(ts_quest_aluno_2011, by = "ID_ALUNO") %>% 
  mutate(ID_LOCALIZACAO = as.factor(ID_LOCALIZACAO),
         ID_DEPENDENCIA_ADM = as.factor(ID_DEPENDENCIA_ADM))

df_5serie <-  df %>% 
  filter(IN_PREENCHIMENTO == 1, IN_PROFICIENCIA == 1,
         ID_SERIE == 5, ID_DEPENDENCIA_ADM ==2) %>% 
  select(PROFICIENCIA_LP, 
         PROFICIENCIA_LP_SAEB,
         PROFICIENCIA_MT,
         PROFICIENCIA_MT_SAEB,
         ID_LOCALIZACAO,
         ID_DEPENDENCIA_ADM,
         pop_estimada,
         TX_RESP_Q001:TX_RESP_Q054) %>% 
  mutate_if(is_character, as.factor) 




vars <- colnames(df_5serie)
vars <- vars[8:61]

y_resp <- "PROFICIENCIA_LP"
y_resp <- "PROFICIENCIA_MT"



tb_r2_5serie <- data.frame(var = vars, label = label_quest_aluno[1:54, c("Enunciado")])

rsquared <- c()
for (variable in vars) {
  
  lm_formula <- as.formula(str_glue("{y_resp} ~ {variable}"))
  model_lm <- lm(lm_formula, df_5serie)
  rsquared <- append(rsquared, summary(model_lm)$r.squared)
}


tb_r2_5serie$LP <- rsquared
tb_r2_5serie$MT <- rsquared

index <- createDataPartition(df_5serie$PROFICIENCIA_LP, p = 0.7, list = F)
train <- df_5serie[index,]
test <- df_5serie[-index,]

vars_modelo <- tb_r2_5serie %>% arrange(desc(LP)) %>% select(var) %>% head(15)
colnames(vars_modelo)
model <- ranger(data = df_5serie,
                log(PROFICIENCIA_LP_SAEB) ~ TX_RESP_Q048 + TX_RESP_Q050 + TX_RESP_Q045 + TX_RESP_Q027 + TX_RESP_Q040 + TX_RESP_Q019 + TX_RESP_Q046 + TX_RESP_Q052 + TX_RESP_Q017 + TX_RESP_Q041 + TX_RESP_Q001 + TX_RESP_Q039 + TX_RESP_Q004 + TX_RESP_Q029 + TX_RESP_Q002)

model_all <- ranger(data = df_5serie %>% select(-PROFICIENCIA_LP, - PROFICIENCIA_MT, - PROFICIENCIA_MT_SAEB, - ID_LOCALIZACAO, -ID_DEPENDENCIA_ADM, - pop_estimada),
                    PROFICIENCIA_LP_SAEB ~ .,
                    importance = 'permutation')


model$r.squared
model_all$r.squared
iv_ranger <- data.frame(model_all$variable.importance)
