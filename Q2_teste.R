#----------------- BIBLIOTECAS ------------------#

library(car)
library(tidyverse)
library(corrplot)
library(olsrr)
library(e1071)
library(readr)
library(caret)
library(forecast)
library(lmtest)
library(strucchange)
library(psych)
library(moments) 
library(nortest)
library(tseries)

#----------------- CARREGANDO DADOS ------------------#

brazil <- read_csv("brazil.csv")
brazil <- read_csv("brazil2023")

ANO <- 1975:2022
GDP <- as.numeric(brazil[1, -1:-4])
FDI <- as.numeric(brazil[2, -1:-4])
POP_GROWTH <- as.numeric(brazil[3, -1:-4])
SAVINGS <- as.numeric(brazil[4, -1:-4])

# Montar o data.frame
dados <- data.frame(
  ANO = ANO,
  GDP = GDP,
  FDI = FDI,
  POP_GROWTH = POP_GROWTH,
  SAVINGS = SAVINGS)

#----------------- DESCRITIVA ------------------#

summary(dados)

# GDP e FDI apresentam média e mediana distantes

psych::describe(dados)

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  #lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

pairs(dados,upper.panel = NULL,diag.panel = panel.hist)

boxplot(dados)

# Autocorrelação 
par(mfrow = c(2, 2)); acf(dados$GDP); acf(dados$FDI); acf(dados$POP_GROWTH); acf(dados$SAVINGS)

## Todos apresentam autocorrelação; SAVINGS apresenta sazonalidade 

# Matriz de correlação das variáveis
cor_matrix <- cor(dados)
corrplot(cor_matrix, 
         method = "color",        # Usar cores gradientes
         type = "upper",          # Mostrar apenas metade superior
         order = "hclust",        # Agrupar variáveis correlacionadas
         addCoef.col = "black",   # Cor dos coeficientes
         number.cex = 0.8,        # Tamanho dos números
         tl.col = "black",        # Cor dos rótulos
         tl.srt = 0,             # Rotação dos rótulos
         diag = TRUE,            # Ocultar diagonal
         title = "Matriz de Correlação",
         mar = c(0, 0, 3, 0),
         col = colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                                  "#F4A582", "#FDDBC7", "#FFFFFF",
                                  "#D1E5F0", "#92C5DE", "#4393C3", 
                                  "#2166AC", "#053061"))(200))

po.test(as.matrix(dados))

#----------------- Testes para as variáveis ------------------#
#Normalidade
shapiro.test(GDP) # útil para amostras pequenas (< 50)
shapiro.test(FDI)
shapiro.test(POP_GROWTH)
shapiro.test(SAVINGS)

#Normalidade sensível à outliers
ad.test(GDP) 

#Assimetria
agostino.test(GDP)
anscombe.test(GDP) # útil para amostras pequenas

#Curtose
anscombe.test(GDP, type = "kurtosis")
bonett.test(GDP) # útil para amostras pequenas

#----------------- MODELAGEM ------------------#

fit <- lm(GDP~FDI+POP_GROWTH+SAVINGS)

summary(fit)

anova(fit)

# Teste de linearidade 
harvtest(fit, data = dados)

# Teste de normalidade
shapiro.test(residuals(fit))

qqPlot(fit, id = FALSE)

# Teste de aleatoriedade
durbinWatsonTest(fit)

# Autocorrelação dos resíduos
acf(residuals(fit), main = "ACF dos Resíduos")

# Teste de homocedasticidade
lmtest::bptest(fit)

# resíduos x ajustados
plot(fit, which = 1)

# Modelo apresentou problemas de aleatoriedade, autocorrelação dos resíduos e homocedasticidade
# Visualmente, é perceptível problema na linearidade

ols_step_forward_aic(fit,details = TRUE)

ols_step_forward_sbic(fit,details = TRUE)
