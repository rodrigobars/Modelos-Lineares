library(tidyverse)
library(readxl)
library(corrplot)

# Y=consumo per capita de frango por ano (Kg por pessoa)
# X2=renda per capita (R$)
# X3=preço do frango (R$/kg)
# X4=preço da carne de porco (R$/Kg), X5=preço da carne de vaca (R$/kg)

dados <- read_excel("C:/Users/rodri/OneDrive/Área de Trabalho/Modelos Lineares I/prática/2/DADOS demanda por frango.xlsx")
dados <- dados |>
  select(-Ano) |> 
  mutate(X6 = (X4+X5)/2)

# Calcule a matriz de correlação
matriz_correlacao <- cor(dados)

# Imprima a matriz de correlação
print(matriz_correlacao)

# Crie um gráfico de matriz de correlação usando a biblioteca corrplot
corrplot(
  matriz_correlacao, 
  method = "number",   # Define o método como "triangle"
  type = "upper",        # Exibe apenas a parte superior da matriz
  tl.col = "black",      # Cor do texto
  tl.srt = 45             # Rotação do texto
)

############ [1]
# (1)	Y= f(X2,X3)
model1 <- lm(Y~X2+X3, dados)
summary(model1) # Adjusted R-squared:  0.9019 

# (2)	Y= f(X2,X3,X4)
model2 <- lm(Y~X2+X3+X4, dados)
summary(model2) # Adjusted R-squared:  0.9267

# (3)	Y= f(X2,X3,X5)
model3 <- lm(Y~X2+X3+X5, dados)
summary(model3) # Adjusted R-squared:  0.8977

# (4)	Y=f(X2,X3,X6)
model4 <- lm(Y~X2+X3+X6, dados)
summary(model4) # Adjusted R-squared:  0.9208

AIC_model1 <- AIC(model1)
AIC_model2 <- AIC(model2)
AIC_model3 <- AIC(model3)
AIC_model4 <- AIC(model4)

summary(model1)$adj.r.squared

calc_models = function(...){
  model_list <- list(...)
  models <- tibble(
    call = character(),
    r2_ajustado = numeric(),
    aic = numeric()
  )
  for (model in model_list) {
    models <- add_row(models, 
                      call = deparse(model$call), 
                      r2_ajustado = summary(model)$adj.r.squared,
                      aic = AIC(model))
  }
  return(models)
}

models <- calc_models(model1,model2,model3,model4)
models

max(models$r2_ajustado)
min(models$aic)
