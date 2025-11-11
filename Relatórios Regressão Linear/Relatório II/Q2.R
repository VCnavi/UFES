#----------------- BIBLIOTECAS ------------------#
{
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
library(itsmr)
library(EnvStats)
library(TTR)
library(boot)
library(stats)
library(Metrics)
library(ggplot2)
library(knitr)
}
#----------------- CARREGANDO df ------------------#
{
brazil <- read_csv("brazilate2023.csv")

ANO <- (1975):(2023)
PIB <- as.numeric(brazil[1, -1:-4])
FDI <- as.numeric(brazil[2, -1:-4])
PG <- as.numeric(brazil[3, -1:-4])
GS <- as.numeric(brazil[4, -1:-4])

# Montar o data.frame
df <- data.frame(
  PIB = PIB,
  FDI = FDI,
  PG = PG,
  GS = GS)
df <- apply(df, 2, as.numeric)
df <- as.data.frame(df)
colnames(df) <- c("PIB", "FDI", "PG", "GS")
rownames(df) <- 1975:2023
}


#----------------- DESCRITIVA ------------------#
{panel.hist <- function(x, ...) {
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

  {
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    
    # Gráfico 1: PIB
    plot(df$PIB, type = "l", col = "blue", lwd = 2,
         xlab = "", ylab = "PIB", main = "PIB")
    abline(lm(df$PIB ~ seq_along(df$PIB)), col = "black", lty = 2, lwd = 1.5)
    
    # Gráfico 2: FDI
    plot(df$FDI, type = "l", col = "red", lwd = 2,
         xlab = "", ylab = "FDI", main = "FDI")
    abline(lm(df$FDI ~ seq_along(df$FDI)), col = "black", lty = 2, lwd = 1.5)
    
    # Gráfico 3: Crescimento Populacional
    plot(df$PG, type = "l", col = "darkgreen", lwd = 2,
         xlab = "", ylab = "Crescimento Populacional", 
         main = "Crescimento Populacional")
    abline(lm(df$PG ~ seq_along(df$PG)), col = "black", lty = 2, lwd = 1.5)
    
    # Gráfico 4: Poupança
    plot(df$GS, type = "l", col = "purple", lwd = 2,
         xlab = "", ylab = "Poupança", main = "Poupança")
    abline(lm(df$GS ~ seq_along(df$GS)), col = "black", lty = 2, lwd = 1.5)
    
    par(mfrow = c(1, 1))
  }

pairs(df,upper.panel = NULL,diag.panel = panel.hist)

# Autocorrelação e autocorrelação parcial
{
  # Definindo o número máximo de defasagens (lags)
  lag_max <- nrow(df)  # Você pode ajustar este valor conforme necessário
  
  # Configuração do layout para 4 gráficos (2x2)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # Plotando ACFs com lag.max
  acf(df$PIB, lag.max = lag_max, main = "ACF - PIB", 
      ylab = "Autocorrelação", xlab = "Defasagem")
  acf(df$FDI, lag.max = lag_max, main = "ACF - FDI", 
      ylab = "Autocorrelação", xlab = "Defasagem")
  acf(df$PG, lag.max = lag_max, main = "ACF - Crescimento Populacional", 
      ylab = "Autocorrelação", xlab = "Defasagem")
  acf(df$GS, lag.max = lag_max, main = "ACF - Poupança", 
      ylab = "Autocorrelação", xlab = "Defasagem")
  
  # Plotando PACFs com lag.max
  pacf(df$PIB, lag.max = lag_max, main = "PACF - PIB", 
       ylab = "Autocorrelação Parcial", xlab = "Defasagem")
  pacf(df$FDI, lag.max = lag_max, main = "PACF - FDI", 
       ylab = "Autocorrelação Parcial", xlab = "Defasagem")
  pacf(df$PG, lag.max = lag_max, main = "PACF - Crescimento Populacional", 
       ylab = "Autocorrelação Parcial", xlab = "Defasagem")
  pacf(df$GS, lag.max = lag_max, main = "PACF - Poupança", 
       ylab = "Autocorrelação Parcial", xlab = "Defasagem")
  
  # Resetando o layout gráfico
  par(mfrow = c(1, 1))
}
## Todos apresentam autocorrelação; Todas apresentam PACF com valores significativos em lag=2;
## GS apresenta em lag = 2 e 3

# Matriz de correlação das variáveis
cor_matrix <- cor(df)
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

fit <- lm(PIB~FDI+PG+GS)
par(mfrow = c(2, 2))
plot(fit) # não sugere resíduos aleatórios, poucos df não colaboram
par(mfrow = c(1, 1))
}
#----------------- Transformações e adições para a modelagem ------------------#
{
#linearizando as variáveis
df$log_PIB <- log(df$PIB)
df$log_GS  <- log(df$GS)
df$log_FDI <- log(df$FDI)
df$log_PG <- log(df$PG)

{
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  # Gráfico 1: log PIB
  plot(df$log_PIB, type = "l", col = "blue", lwd = 2,
       xlab = "", ylab = "PIB", main = "log PIB")
  abline(lm(df$log_PIB ~ seq_along(df$log_PIB)), col = "black", lty = 2, lwd = 1.5)
  
  # Gráfico 2: log FDI
  plot(df$log_FDI, type = "l", col = "red", lwd = 2,
       xlab = "", ylab = "FDI", main = "log FDI")
  abline(lm(df$log_FDI ~ seq_along(df$log_FDI)), col = "black", lty = 2, lwd = 1.5)
  
  # Gráfico 3: log Crescimento Populacional
  plot(df$log_PG, type = "l", col = "darkgreen", lwd = 2,
       xlab = "", ylab = "log Crescimento Populacional", 
       main = "log Crescimento Populacional")
  abline(lm(df$log_PG ~ seq_along(df$log_PG)), col = "black", lty = 2, lwd = 1.5)
  
  # Gráfico 4: log Poupança
  plot(df$log_GS, type = "l", col = "purple", lwd = 2,
       xlab = "", ylab = "log Poupança", main = "log Poupança")
  abline(lm(df$log_GS ~ seq_along(df$log_GS)), col = "black", lty = 2, lwd = 1.5)
  
  par(mfrow = c(1, 1))
}

pairs(df[,5:8],upper.panel = NULL,diag.panel = panel.hist)

df$log_PIB_lag1 <- lag(df$log_PIB, 1)
df$log_GS_lag1 <- lag(df$log_GS, 1)
df$log_FDI_lag1 <- lag(df$log_FDI, 1)
df$log_PG_lag1 <- lag(df$log_PG, 1)

df$PIB_lag1 <- lag(df$PIB, 1)
df$GS_lag1 <- lag(df$GS, 1)
df$FDI_lag1 <- lag(df$FDI, 1)
df$PG_lag1 <- lag(df$PG, 1)

df <- na.omit(df)

df_reg <- df

n <- nrow(df_reg)
df_train <- df_reg[1:(n-2), , drop = FALSE]
df_test  <- df_reg[(n - 1), , drop = FALSE]
df_2023  <- df_reg[n, , drop = FALSE]

# Modelo com variáveis ajustadas
modelo_rl <- lm(log_PIB ~ PG +              # PG sem log e sem lag
                  log_GS + log_GS_lag1 +        # log de GS com lag
                  log_FDI + log_FDI_lag1 +
                  log_PIB_lag1,                 # lag de log PIB
                data = df_train)

par(mfrow = c(2, 2))
plot(modelo_rl) # não sugere resíduos aleatórios, poucos df não colaboram
par(mfrow = c(1, 1))
}
# --- Previsão para 2021 e 2022 (teste) ---
{
  pred_test <- predict(modelo_rl, newdata = df_test, interval = "confidence")
  prev_test <- exp(pred_test[, "fit"])  # Usar apenas a coluna "fit" para o MAPE
  df_test$prev <- exp(pred_test[, "fit"])
  df_test$lower <- exp(pred_test[, "lwr"])
  df_test$upper <- exp(pred_test[, "upr"])
  real_PIB_test <- df_test$PIB
  
  mape_teste <- mape(real_PIB_test, prev_test)
  cat("MAPE Teste (2021-2022):", round(mape_teste * 100, 2), "%\n")
  
  # Previsão para 2023
  pred_2023 <- predict(modelo_rl, newdata = df_2023, interval = "confidence")
  prev_2023 <- exp(pred_2023[, "fit"])  # Usar apenas a coluna "fit" para o MAPE
  df_2023$prev <- exp(pred_2023[, "fit"])
  df_2023$lower <- exp(pred_2023[, "lwr"])
  df_2023$upper <- exp(pred_2023[, "upr"])
  real_PIB_2023 <- df_2023$PIB
  
  mape_2023 <- mape(real_PIB_2023, prev_2023)
  cat("MAPE Previsão 2023:", round(mape_2023 * 100, 2), "%\n")
}
# --- gráficos ---
{
  # Ajustes do modelo no conjunto de treino
  df_train$prev <- exp(fitted(modelo_rl))
  df_train$lower <- NA
  df_train$upper <- NA
  
  # Previsões no conjunto de teste
  pred_test <- predict(modelo_rl, newdata = df_test, interval = "confidence")
  df_test$prev <- exp(pred_test[, "fit"])
  df_test$lower <- exp(pred_test[, "lwr"])
  df_test$upper <- exp(pred_test[, "upr"])
  
  # Previsão para 2023
  pred_2023 <- predict(modelo_rl, newdata = df_2023, interval = "confidence")
  df_2023$prev <- exp(pred_2023[, "fit"])
  df_2023$lower <- exp(pred_2023[, "lwr"])
  df_2023$upper <- exp(pred_2023[, "upr"])
  
  # Adicionar coluna de Ano
  df_train$Ano <- 1975:(1975 + nrow(df_train) - 1)
  df_test$Ano <- 2022
  df_2023$Ano <- 2023
  
  # Unir todos os conjuntos com mesmas colunas
  cols_final <- c("Ano", "PIB", "prev", "lower", "upper")
  df_plot <- rbind(df_train[, cols_final], df_test[, cols_final], df_2023[, cols_final])
  
  ggplot(df_plot, aes(x = Ano)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.4) +
    geom_line(aes(y = PIB, color = "PIB Observado"), size = 1.2) +
    geom_line(aes(y = prev, color = "PIB Ajustado/Previsto"), size = 1.2, linetype = "dashed") +
    
    labs(
      title = "Previsões do PIB (PIB)",
      x = "Ano",
      y = "PIB",
      color = "Legenda:",
      fill = ""
    ) +
    
    scale_color_manual(values = c(
      "PIB Observado" = "black",
      "PIB Ajustado/Previsto" = "blue"
    )) +
    scale_fill_manual(values = c("IC 95%" = "darkgray")) +
    
    scale_x_continuous(breaks = seq(1975, 2023, 4)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# --- Gráficos recentes ---
{
df_recent <- df_plot %>% filter(Ano >= 2015)

  ggplot(df_recent, aes(x = Ano)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.4) +
    geom_line(aes(y = PIB, color = "PIB Observado"), size = 1.2) +
    geom_line(aes(y = prev, color = "PIB Ajustado/Previsto"), size = 1.2, linetype = "dashed") +
    
    labs(
      title = "Previsões Recentes do PIB (2015–2023)",
      x = "Ano",
      y = "PIB ",
      color = "Legenda:",
      fill = ""
    ) +
    
    scale_color_manual(values = c(
      "PIB Observado" = "black",
      "PIB Ajustado/Previsto" = "blue"
    )) +
    scale_fill_manual(values = c("IC 95%" = "darkgray")) +
    
    scale_x_continuous(breaks = 2015:2023) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

# --- bootstrap ---
# Função estatística: estatística t (coeficientes)
{
boot_fn <- function(data, indices) {
  modelo <- lm(log_PIB ~ PG + log_GS + log_GS_lag1 + log_FDI + log_FDI_lag1 + log_PIB_lag1, data = data[indices, ])
  return(coef(modelo))
}

# Aplicar bootstrap com 500 repetições
boot_results <- boot(data = df_train, statistic = boot_fn, R = 500)

# Intervalos de confiança via bootstrap
boot.ci(boot_results, type = "perc", index = 2) #PG #ok
boot.ci(boot_results, type = "perc", index = 3) #log_GS
boot.ci(boot_results, type = "perc", index = 4) #log_GS_lag1
boot.ci(boot_results, type = "perc", index = 5) #log_FDI
boot.ci(boot_results, type = "perc", index = 6) #log_FDI_lag1
boot.ci(boot_results, type = "perc", index = 7) #log_PIB_lag1 #ok
}

# ---- Análise após verificação dos coeficientes via bootstrap ----
# Remoção de FDI (menor correlação com GPD e bootstrap indicou baixa significância)
{
modelo_rl <- lm(log_PIB ~ PG +              # PG sem log e sem lag
                  log_GS + log_GS_lag1 +        # log de GS com lag
                  log_PIB_lag1,                 # lag de log PIB
                data = df_train)

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(modelo_rl)
par(mfrow = c(1, 1))
}
  # ---- testes resíduos ----
  {
    # Função para executar todos os testes em um tipo de resíduo
    testar_residuos <- function(residuos, modelo, nome) {
      # Testes de normalidade
      shapiro <- shapiro.test(residuos)
      anderson <- ad.test(residuos)
      lillie <- lillie.test(residuos)
      jarque <- jarque.bera.test(residuos)
      
      # Testes de aleatoriedade
      dw <- dwtest(modelo)
      box <- Box.test(residuos, lag = 10, type = "Ljung-Box")
      
      # Testes de linearidade
      harvey <- harvtest(modelo)
      reset <- resettest(modelo)
      
      # Testes de homocedasticidade
      bp <- bptest(modelo)
      gq <- gqtest(modelo)
      
      # Organizar resultados
      resultados <- data.frame(
        Teste = c("Shapiro-Wilk", "Anderson-Darling", "Lilliefors", "Jarque-Bera",
                  "Durbin-Watson", "Ljung-Box", "Harvey", "RESET",
                  "Breusch-Pagan", "Goldfeld-Quandt"),
        Estatística = c(shapiro$statistic, anderson$statistic, lillie$statistic, jarque$statistic,
                        dw$statistic, box$statistic, harvey$statistic, reset$statistic,
                        bp$statistic, gq$statistic),
        p_valor = c(shapiro$p.value, anderson$p.value, lillie$p.value, jarque$p.value,
                    dw$p.value, box$p.value, harvey$p.value, reset$p.value,
                    bp$p.value, gq$p.value),
        Resíduo = nome
      )
      
      return(resultados)
    }
    
    # 1. Calcular todos os tipos de resíduos
    res_classico <- residuals(modelo_rl)
    res_stud_int <- rstandard(modelo_rl)
    res_stud_ext <- rstudent(modelo_rl)
    res_recursivo <- recresid(modelo_rl)
    
    # 2. Executar testes para cada tipo
    resultados <- c(
      testar_residuos(res_classico, modelo_rl, "Clássicos"),
      testar_residuos(res_stud_int, modelo_rl, "Studentizados Internos"),
      testar_residuos(res_stud_ext, modelo_rl, "Studentizados Externos"),
      testar_residuos(res_recursivo, modelo_rl, "Recursivos")
    )
    
    # 3. Exibir tabela de resultados formatada
    tabela <- kable(resultados, digits = 4, caption = "Resultados dos Testes para Diferentes Tipos de Resíduos")
    
    # 4. Plotar QQ-plots comparativos
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    
    qqPlot(res_classico, main = "QQ-plot: Resíduos Clássicos",
           xlab = "Quantis teóricos", ylab = "Quantis amostrais")
    
    qqPlot(res_stud_int, main = "QQ-plot: Resíduos Studentizados Internos",
           xlab = "Quantis teóricos", ylab = "Quantis amostrais")
    
    qqPlot(res_stud_ext, main = "QQ-plot: Resíduos Studentizados Externos",
           xlab = "Quantis teóricos", ylab = "Quantis amostrais")
    
    qqPlot(res_recursivo, main = "QQ-plot: Resíduos Recursivos",
           xlab = "Quantis teóricos", ylab = "Quantis amostrais")
    
    par(mfrow = c(1, 1))
  }
  # --- Previsão para 2021 e 2022 (teste) ---
  {
    pred_test <- predict(modelo_rl, newdata = df_test, interval = "confidence")
    prev_test <- exp(pred_test[, "fit"])  # Usar apenas a coluna "fit" para o MAPE
    df_test$prev <- exp(pred_test[, "fit"])
    df_test$lower <- exp(pred_test[, "lwr"])
    df_test$upper <- exp(pred_test[, "upr"])
    real_PIB_test <- df_test$PIB
    
    mape_teste <- mape(real_PIB_test, prev_test)
    cat("MAPE Teste (2021-2022):", round(mape_teste * 100, 2), "%\n")
    
    # Previsão para 2023
    pred_2023 <- predict(modelo_rl, newdata = df_2023, interval = "confidence")
    prev_2023 <- exp(pred_2023[, "fit"])  # Usar apenas a coluna "fit" para o MAPE
    df_2023$prev <- exp(pred_2023[, "fit"])
    df_2023$lower <- exp(pred_2023[, "lwr"])
    df_2023$upper <- exp(pred_2023[, "upr"])
    real_PIB_2023 <- df_2023$PIB
    
    mape_2023 <- mape(real_PIB_2023, prev_2023)
    cat("MAPE Previsão 2023:", round(mape_2023 * 100, 2), "%\n")
  }
  # --- gráficos ---
  {
    # Ajustes do modelo no conjunto de treino
    df_train$prev <- exp(fitted(modelo_rl))
    df_train$lower <- NA
    df_train$upper <- NA
    
    # Previsões no conjunto de teste
    pred_test <- predict(modelo_rl, newdata = df_test, interval = "confidence")
    df_test$prev <- exp(pred_test[, "fit"])
    df_test$lower <- exp(pred_test[, "lwr"])
    df_test$upper <- exp(pred_test[, "upr"])
    
    # Previsão para 2023
    pred_2023 <- predict(modelo_rl, newdata = df_2023, interval = "confidence")
    df_2023$prev <- exp(pred_2023[, "fit"])
    df_2023$lower <- exp(pred_2023[, "lwr"])
    df_2023$upper <- exp(pred_2023[, "upr"])
    
    # Adicionar coluna de Ano
    df_train$Ano <- 1975:(1975 + nrow(df_train) - 1)
    df_test$Ano <- 2022
    df_2023$Ano <- 2023
    
    # Unir todos os conjuntos com mesmas colunas
    cols_final <- c("Ano", "PIB", "prev", "lower", "upper")
    df_plot <- rbind(df_train[, cols_final], df_test[, cols_final], df_2023[, cols_final])
    
    ggplot(df_plot, aes(x = Ano)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.4) +
      geom_line(aes(y = PIB, color = "PIB Observado"), size = 1.2) +
      geom_line(aes(y = prev, color = "PIB Ajustado/Previsto"), size = 1.2, linetype = "dashed") +
      
      labs(
        title = "Previsões do PIB",
        x = "Ano",
        y = "PIB",
        color = "Legenda:",
        fill = ""
      ) +
      
      scale_color_manual(values = c(
        "PIB Observado" = "black",
        "PIB Ajustado/Previsto" = "blue"
      )) +
      scale_fill_manual(values = c("IC 95%" = "darkgray")) +
      
      scale_x_continuous(breaks = seq(1975, 2023, 4)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
  
  # --- Gráficos recentes ---
  {
    df_recent <- df_plot %>% filter(Ano >= 2015)
    
    ggplot(df_recent, aes(x = Ano)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.4) +
      geom_line(aes(y = PIB, color = "PIB Observado"), size = 1.2) +
      geom_line(aes(y = prev, color = "PIB Ajustado/Previsto"), size = 1.2, linetype = "dashed") +
      
      labs(
        title = "Previsões Recentes do PIB (2015–2023)",
        x = "Ano",
        y = "PIB ",
        color = "Legenda:",
        fill = ""
      ) +
      
      scale_color_manual(values = c(
        "PIB Observado" = "black",
        "PIB Ajustado/Previsto" = "blue"
      )) +
      scale_fill_manual(values = c("IC 95%" = "darkgray")) +
      
      scale_x_continuous(breaks = 2015:2023) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }