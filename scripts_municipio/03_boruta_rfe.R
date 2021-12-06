library(tidyverse)
library(caret)
library(Boruta)
saeb5ef <- read.csv2("output/selecao/escolas_municipais/selecao_saeb5ef.csv")
saeb9ef <- read.csv2("output/selecao/escolas_municipais/selecao_saeb9ef.csv")

seleciona <- function(data) {
  data %>% 
    select(media,
           nivel_socio_economico,
           starts_with('tx'),
           ends_with('ipvs'), 
           ends_with('imp')
    )
}
saeb5ef <- seleciona(saeb5ef) 
saeb9ef <- seleciona(saeb9ef) 

# Boruta ------------------------------------------------------------------
## todas variaveis confirmadas
set.seed(111)

train_boruta <- Boruta(media~., data = saeb, doTrace = 2)

iv <- data.frame(attStats(train_boruta))
iv <- tibble::rownames_to_column(iv, "var") %>% view()



# RFE ---------------------------------------------------------------------

## não elimina variáveis
set.seed(111)
options(warn=-1)

subsets <- c(10,15,20,25,30)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "cv",
                   repeats = 5,
                   verbose = FALSE)


rfe_5ef <- rfe(x=saeb5ef[, 2:32], 
                 y=saeb5ef$media,
                 sizes = subsets,
                 rfeControl = ctrl)

rfe_9ef <- rfe(x=saeb9ef[, 2:32], 
               y=saeb9ef$media,
               sizes = subsets,
               rfeControl = ctrl)

imp_rfe <- train_rfe$optVariables


cat('5ef: ', rfe_5ef$bestSubset) 
cat('9ef: ', rfe_5ef$bestSubset) 


rfeImp <- data.frame(rfe_imp = varImp(train_rfe))

rfeImp <- tibble::rownames_to_column(rfeImp, "var") %>% view()

