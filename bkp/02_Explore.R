# TS_RESPOSTA_ALUNO ----
head(ts_resposta_aluno_2011)

summary(ts_resposta_aluno_2011)


ts_resposta_aluno_2011 %>% 
  summarise_all(n_distinct)
 
ts_resposta_aluno_2011 %>% 
  group_by(ID_UF) %>% 
  count() %>% 
  arrange(desc(n))


# TS_QUEST_ESCOLA ----
head(ts_quest_escola_2011)

summary(ts_quest_escola_2011)

ts_quest_escola_2011 %>% 
  summarise_all()

ts_quest_escola_2011 <- ts_quest_escola_2011 %>% 
  mutate()



ggplot(df_5serie, aes(PROFICIENCIA_LP_SAEB)) + geom_histogram()
ggplot(df_5serie, aes(PROFICIENCIA_MT_SAEB)) + geom_histogram()