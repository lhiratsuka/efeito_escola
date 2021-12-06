library(tidyverse)
library(caret)


# Import ------------------------------------------------------------------
saeb5ef <- read.csv2("output/selecao/escolas_municipais/selecao_saeb5ef.csv")

anyNA(saeb5ef)



# Base Modelo -------------------------------------------------------------
saeb5ef %>% 
  group_by(nivel_socio_economico) %>% 
  summarise(n = n(), media = mean(media))

train5ef <-
  saeb5ef %>% 
    select(
      id_escola,
      media,
      nivel_socio_economico,
      starts_with('tx'),
      ends_with('ipvs'),
      ends_with('imp')
    ) %>% 
    mutate(
      nivel_socio_economico = fct_collapse(nivel_socio_economico,
                                                IV = c("Nível II","Nível III","Nível IV"),
                                                V = c("Nível V",""),
                                                VI = c("Nível VI","Nível VII")
                                                ))



# CV ------------------------------------------------------------------

set.seed(231192)
myFolds <- createFolds(train5ef$media, k = 5)
trControl <- trainControl(
  method = "cv",
  number = 5,
  index = myFolds
)
# Ranger  (53,9) ------------------------------------------------------------------
set.seed(231192)
ranger <- train(media ~.,
                      data = train5ef %>% select(-id_escola),
                      method = "ranger",
                      metric = "Rsquared",
                      importance = 'permutation',
                      trControl = trControl)


mean(ranger$resample$Rsquared)

imp_ranger <- data.frame(imp = varImp(ranger)$importance) %>% rownames_to_column()
imp_ranger <- imp_ranger %>% 
  rename(imp = Overall) %>% 
  filter(!str_detect(rowname, "nivel")) %>%  
  arrange(desc(imp))

# Ranger Top 22 (54,3) ----

for (i in 1:30) {
  vars <- imp_ranger[1:i,1]
  vars <- c(vars,'media', 'nivel_socio_economico')
  data <- train5ef %>% 
    select_at(vars)
  
  myFolds <- createFolds(data$media, k = 5)
  trControl <- trainControl(
    method = "cv",
    number = 5,
    index = myFolds
  )
  
  ranger_top <- train(media ~ .,
                  data = data,
                  method = "ranger",
                  metric = "Rsquared",
                  trControl = trControl)
  r2 <- mean(ranger_top$resample$Rsquared)
  print(paste0(i, ": ",round(r2,3)))
}
# LM  (54,3) ----
lm <- train(media ~.,
                  data = train5ef %>% select(-id_escola),
                  method = "lm",
                  metric = "Rsquared",
                  importance = 'permutation',
                  trControl = trControl)


mean(lm$resample$Rsquared)
df <- data.frame(imp =lm$finalModel$coefficients[-1]) %>% rownames_to_column()
df <- df %>% 
  mutate(imp = abs(imp)) %>% 
  filter(!str_detect(rowname, "nivel")) %>%  
  arrange(desc(imp))

# LM Top 18 (56,2) ----

for (i in 1:30) {
  vars <- df[1:i,1]
  vars <- c(vars,'media', 'nivel_socio_economico')
  data <- train5ef %>% 
    select_at(vars)
  
  myFolds <- createFolds(data$media, k = 5)
  trControl <- trainControl(
    method = "cv",
    number = 5,
    index = myFolds
  )
  
  lm_top <- train(media ~ .,
                  data = data,
                  method = "lm",
                  metric = "Rsquared",
                  trControl = trControl)
  r2 <- mean(lm_top$resample$Rsquared)
  print(paste0(i, ": ",round(r2,3)))
}

# 18 var
