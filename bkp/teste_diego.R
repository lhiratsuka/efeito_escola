vars <- colnames(df_5serie)
vars <- vars[8:61]

config_nona_serie <- list(
  vars=as.character(vars),
  var_resp=c('PROFICIENCIA_LP_SAEB', 'PROFICIENCIA_MT_SAEB'),
  tb_r2_5serie <- label_quest_aluno_5serie
)

a <- expand.grid(vars=as.character(vars),
                 var_resp=c('PROFICIENCIA_LP_SAEB', 'PROFICIENCIA_MT_SAEB'))
a$tb_r2_5serie <- label_quest_aluno_5serie

for (idx in 1:nrow(a)) {
  # reg linear
  nome_var <- a[idx, "vars"]
  tipo_prova <- a[idx, "var_resp"]
  # fazer filtro na tabela
  fit <- lm(as.formula(str_glue("{tipo_prova} ~ {nome_var}")), df_5serie)
  r2 <- summary(fit)$r.squared
  print(r2)
  a[idx, "r2"] <- r2 
}

resultados_5serie <- spread(a, key = var_resp, value = r2)

