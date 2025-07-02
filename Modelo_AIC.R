############# ANÁLISE DESCRITIVA ############# 

# Carregando bibliotecas necessárias
library(corrplot)
library(olsrr)
library(car)
library(ggplot2)
library(lmtest)

# Preparação dos dados
dados <- read.csv("brazil.csv", header = TRUE)
dados_2023 <- read.csv("brazil2023.csv", header = TRUE)

# Extrair dados reais de 2023
dados_reais_2023 <- data.frame(
  Ano_2023 = 2023,
  GDP_2023 = as.numeric(dados_2023[2, 5]),
  FDI_2023 = as.numeric(dados_2023[1, 5]),
  Gross_Savings_2023 = as.numeric(dados_2023[3, 5])
)

# Extrair séries temporais
anos <- 1975:2022
GDP <- as.numeric(dados[1, 5:ncol(dados)])
FDI <- as.numeric(dados[2, 5:ncol(dados)])
Pop_Growth <- as.numeric(dados[3, 5:ncol(dados)])
Gross_Savings <- as.numeric(dados[4, 5:ncol(dados)])

# Criar dataframe
df <- data.frame(
  Ano = anos,
  GDP = GDP,
  FDI = FDI,
  Pop_Growth = Pop_Growth,
  Gross_Savings = Gross_Savings
)

# Análise exploratória
summary(df)

# Visualização das variáveis
pairs(df[, c("GDP", "FDI", "Pop_Growth", "Gross_Savings")], 
      upper.panel = NULL,
      main = "Relações entre Variáveis")

# Matriz de correlação
cor_matrix <- cor(df[, c("GDP", "FDI", "Pop_Growth", "Gross_Savings")])
corrplot(cor_matrix, method = "number", 
         title = "Matriz de Correlação",
         mar = c(0, 0, 1, 0))

corrplot(cor_matrix, 
         method = "color",        # Usar cores gradientes
         type = "upper",          # Mostrar apenas metade superior
         order = "hclust",        # Agrupar variáveis correlacionadas
         addCoef.col = "black",   # Cor dos coeficientes
         number.cex = 0.8,        # Tamanho dos números
         tl.col = "black",        # Cor dos rótulos
         tl.srt = 0,             # Rotação dos rótulos
         diag = FALSE,            # Ocultar diagonal
         title = "Matriz de Correlação",
         mar = c(0, 0, 3, 0),
         col = colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                                  "#F4A582", "#FDDBC7", "#FFFFFF",
                                  "#D1E5F0", "#92C5DE", "#4393C3", 
                                  "#2166AC", "#053061"))(100))

############# MODELAGEM BASEADA NO AIC ############# 

# Modelo completo inicial
modelo_completo <- lm(GDP ~ FDI + Pop_Growth + Gross_Savings, data = df)
summary(modelo_completo)

# Seleção de variáveis passo a passo
modelo_step <- ols_step_both_aic(modelo_completo, details = TRUE)

# Modelo final selecionado
modelo_final <- lm(modelo_step$model, data = df)
summary(modelo_final)

modelo_final <- lm(GDP ~ FDI + Gross_Savings, data = df)

############# TESTES #############

# Teste de normalidade dos resíduos
ols_test_normality(modelo_final)

# Teste de heterocedasticidade
ols_test_breusch_pagan(modelo_final)

# Testes de linearidade
cat("\nTESTES DE LINEARIDADE\n")

# 1. Teste RESET de Ramsey
cat("\nTeste RESET de Ramsey:\n")
print(resettest(modelo_final))

# 2. Teste de Harvey-Collier
cat("\nTeste de Harvey-Collier:\n")
print(harvtest(modelo_final))

# 3. Gráfico de resíduos
cat("\nPlotando gráfico de resíduos vs ajustados...\n")
plot(modelo_final, which = 1)

############# Previsão para 2023 

# Preparar os dados de 2023 no formato correto para previsão
dados_2023 <- data.frame(
  FDI = dados_reais_2023$FDI_2023,
  Gross_Savings = dados_reais_2023$Gross_Savings_2023
)

# Fazer a previsão para 2023
previsao_2023 <- predict(modelo_final, newdata = dados_2023)

# Calcular o MAPE
valor_real_2023 <- dados_reais_2023$GDP_2023
erro_percentual <- abs((valor_real_2023 - previsao_2023) / valor_real_2023) * 100
mape <- mean(erro_percentual)

# Exibir resultados
cat("Previsão para 2023:", previsao_2023, "\n")
cat("Valor real em 2023:", valor_real_2023, "\n")
cat("Erro percentual absoluto:", erro_percentual, "%\n")
cat("MAPE:", mape, "%\n")

# Visualização comparativa
comparativo <- data.frame(
  Ano = c("2023 (Real)", "2023 (Previsto)"),
  GDP = c(valor_real_2023, previsao_2023)
)

