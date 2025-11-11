# librarys
{
  library(tidyverse)
  library(dplyr)
  library(MASS)
  library(corrplot)
  library(caret)
  library(lawstat)
  library(pROC)
  library(ROSE)
  library(glmnet)
  library(gamlss)
}

# banco de dados
{
  df <- read.csv("despesas.csv", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(
      # Garantindo idade em anos
      AGE = floor(AGE)
    )
  card <- df %>%
  filter(CARDHLDR == 1) # cartão de crédito aceito
  
  # Remoção de dados inconsistentes (idades <= 3)
  # Número alto de dados, remoção não afeta de maneira grave
  card <- card %>%
    filter(
      AGE >= 18
    )
  
  # GASTOS X RELATÓRIOS NEGATIVOS  
  card$TOTAL_DRG <- card$MAJORDRG + card$MINORDRG
  
  # dados para modelo (sem variáveis tipo string)
  card_model <- card
  card_model$DEFAULT <- as.factor(card_model$DEFAULT)
  card_model$CARDHLDR <- as.factor(card_model$CARDHLDR)
  card_model$OWNRENT <- as.factor(card_model$OWNRENT)
  card_model$SELFEMPL <- as.factor(card_model$SELFEMPL) 
  
  # FAIXA_ETARIA
  card <- card %>%
    mutate(FAIXA_ETARIA = cut(AGE,
                                breaks = c(0, 25, 35, 45, 55, 65, 100),
                                labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"))) %>%
    ungroup()

  # FAIXA_INCOME E FAIXA_INCPER
  card <- card %>%
    mutate(FAIXA_INCOME = cut(INCOME,
                              breaks = c(0, 2000, 5000, 10000, 20000, Inf),
                              labels = c("Até 2k", "2k-5k", "5k-10k", "10k-20k", "20k+"),
                              include.lowest = TRUE),
           
           FAIXA_INCPER = cut(INCPER,
                              breaks = c(0, 10000, 20000, 50000, 100000, 200000, Inf),
                              labels = c("Até 10k","10k-20k", "20k-50k", "50k-100k", "100k-200k", "200k+"),
                              include.lowest = TRUE))
  
  # ESTABILIDADE HABITACIONAL
  card$ESTABILIDADE <- cut(card$ACADMOS,
                           breaks = c(0, 12, 36, 120, max(card$ACADMOS, na.rm = TRUE)),
                           labels = c("Baixa (<1 ano)", "Média (1-3 anos)", 
                                      "Alta (3-10 anos)", "Muito Alta (>10 anos)"),
                           include.lowest = TRUE,
                           right = FALSE)
  }

# descritiva
{
  # Estatísticas das variáveis
  # SPENDING
  summary_select <- card %>% 
    summarise(
      n = n(),
      miss = sum(is.na(SPENDING)),
      media = mean(SPENDING, na.rm = TRUE),
      mediana = median(SPENDING, na.rm = TRUE),
      dp = sd(SPENDING, na.rm = TRUE),
      p10 = quantile(SPENDING, 0.10, na.rm = TRUE),
      p25 = quantile(SPENDING, 0.25, na.rm = TRUE),
      p75 = quantile(SPENDING, 0.75, na.rm = TRUE),
      p90 = quantile(SPENDING, 0.90, na.rm = TRUE),
      min = min(SPENDING, na.rm = TRUE),
      max = max(SPENDING, na.rm = TRUE)
    )
  cat("\n===== Estatísticas de 'SPENDING' (titulares) =====\n")
  print(summary_select)
  
  # GASTO X FAIXA ETÁRIA
  gasto_idade <- card %>%
    group_by(FAIXA_ETARIA) %>%
    summarise(
      Media_Gasto    = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto  = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup()
  
  # GASTOS X INADIMPLENTES
  gasto_inadimplencia <- card %>%
    group_by(DEFAULT) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(DEFAULT_Label = ifelse(DEFAULT == 1, "Inadimplente", "Não Inadimplente"))
  
  # GASTOS X NÚMERO DE DEPENDENTES
  gasto_dependentes <- card %>%
    group_by(ADEPCNT) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    )
  
  gasto_relatorios <- card %>%
    group_by(TOTAL_DRG) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    )
  
  # GASTOS X RENDAS
  gasto_income <- card %>%
    group_by(FAIXA_INCOME) %>%
    summarise(Media_Gasto = mean(SPENDING, na.rm = TRUE),
              Mediana_Gasto = median(SPENDING, na.rm = TRUE),
              Variancia_Gasto = var(SPENDING, na.rm = TRUE),
              n = n())
  
  gasto_incper <- card %>%
    group_by(FAIXA_INCPER) %>%
    summarise(Media_Gasto = mean(SPENDING, na.rm = TRUE),
              Mediana_Gasto = median(SPENDING, na.rm = TRUE),
              Variancia_Gasto = var(SPENDING, na.rm = TRUE),
              n = n())
  
  # AUTÔNOMOS VS GASTOS
  gasto_selfemp <- card %>%
    group_by(SELFEMPL) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    ) %>%
    mutate(SELFEMPL_Label = ifelse(SELFEMPL == 1, "Autônomo", "Não Autônomo"))
  
  # GASTOS X ESTABILIDADE HABITACIONAL
  gasto_estabilidade <- card %>%
    group_by(ESTABILIDADE) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    )
  
  # GASTOS X OWNRENT
  gasto_moradia <- card %>%
    group_by(OWNRENT) %>%
    summarise(
      Media_Gasto = mean(SPENDING, na.rm = TRUE),
      Mediana_Gasto = median(SPENDING, na.rm = TRUE),
      Variancia_Gasto = var(SPENDING, na.rm = TRUE),
      n = n()
    )
  
  # Histograma dos card
  ggplot(card, aes(x = SPENDING)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = "Distribuição dos card dos Titulares de Cartão",
         x = "card", y = "Frequência") +
    theme_minimal()
  
  # Histograma das idades
  ggplot(card, aes(x = AGE)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = "Distribuição das Idades dos Titulares de Cartão",
         x = "card", y = "Frequência") +
    theme_minimal()
  
  # Barplots GASTO X FAIXA ETARIA
  ggplot(gasto_idade, aes(x = FAIXA_ETARIA, y = Mediana_Gasto, fill = FAIXA_ETARIA)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Faixa Etária",
         x = "Faixa Etária",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_idade$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # INADIMPLÊNCIA VS GASTOS
  # Visualização
  ggplot(gasto_inadimplencia, aes(x = DEFAULT_Label, y = Mediana_Gasto, fill = DEFAULT_Label)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Caso de Inadimplência",
         x = "Caso de Inadimplência",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_idade$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # GASTOS X NÚMERO DE DEPENDENTES
  # Visualização
  ggplot(card, aes(x = factor(ADEPCNT), y = SPENDING, fill = factor(ADEPCNT))) +
    geom_boxplot() +
    labs(title = "Gastos por Número de Dependentes",
         x = "Número de Dependentes", y = "Gastos") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # GASTOS X RELATÓRIOS NEGATIVOS
  # Visualização
  ggplot(card, aes(x = factor(TOTAL_DRG), y = SPENDING, fill = factor(TOTAL_DRG))) +
    geom_boxplot() +
    labs(title = "Gastos por Relatórios Negativos",
         x = "Número de Relatórios", y = "Gastos") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # GASTOS X RENDA   
  # Visualizações
  # ----------- GRÁFICO PARA INCOME -----------
  ggplot(gasto_income, aes(x = FAIXA_INCOME, y = Mediana_Gasto, fill = FAIXA_INCOME)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Faixa de Renda",
         x = "Faixa de Renda",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_income$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # ----------- GRÁFICO PARA INCPER -----------
  ggplot(gasto_incper, aes(x = FAIXA_INCPER, y = Mediana_Gasto, fill = FAIXA_INCPER)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Faixa de Renda per Capita",
         x = "Faixa de Renda per Capita",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_incper$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # AUTÔNOMOS VS GASTOS
  ggplot(gasto_selfemp, aes(x = SELFEMLP_Label, y = Mediana_Gasto, fill = SELFEMLP_Label)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Faixa de Renda per Capita",
         x = "Faixa de Renda per Capita",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_incper$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # GASTOS X ESTABILIDADE HABITACIONAL
  # Visualização
  ggplot(card, aes(x = ESTABILIDADE, y = SPENDING, fill = ESTABILIDADE)) +
    geom_boxplot() +
    labs(title = "Gastos por Estabilidade Habitacional",
         x = "Meses na mesma moradia", y = "Gastos") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # GASTOS X MORADIA
  # Visualização
  ggplot(card, aes(x = factor(OWNRENT), y = SPENDING, fill = factor(OWNRENT))) +
    geom_boxplot() +
    labs(title = "Gastos por Tipo de Moradia",
         x = "Tipo de Moradia", y = "Gastos") +
    theme_minimal() +
    theme(legend.position = "none")

  # Correlações
  var_numericas <- c("DEFAULT","AGE", "ADEPCNT", "ACADMOS", "MAJORDRG", "MINORDRG", 
                     "INCOME", "INCPER", "EXP_INC", "SPENDING", "LOGSPEND")
  
  cor_matrix <- cor(card[, var_numericas], use = "pairwise.complete.obs")
  corrplot(cor_matrix, 
           method = "color", 
           type = "upper",
           addCoef.col = "black",
           number.cex = 0.7,
           tl.cex = 0.7,
           diag = FALSE)
}

# -------------- Variáveis mais relevantes na pontuação de crédito -------------- #
# Verificar natureza dos dados (se os dados seguem alguma distribuição específica);
# Aplicar testes de significância dentro das bases "gasto_x", onde 
# x = c("dependentes","estabilidade","idade","income","incper","relatorios","selfemp","inadimplencia"),
# para verificar se os diferentes fatores internos levam à uma diferença no comportamento de gastos.
# Destacar as variáveis mais relevantes na pontuação do crédito via modelo robusto ou logístico.

{
    dados_modelo <- card_model[names(card_model) %in% c("DEFAULT", "AGE", "ADEPCNT", "ACADMOS","MAJORDRG","MINORDRG", "SPENDING",
                                                      "OWNRENT", "INCOME","INCPER", "SELFEMPL", "EXP_INC", "LOGSPEND")]
  
  # Modelo de regressão logística para identificar variáveis importantes
  modelo <- glm(DEFAULT ~ ., data = dados_modelo, family = "binomial")
  importancia <- varImp(modelo)
  importancia$VARIAVEL <- rownames(importancia)
  importancia <- importancia %>% arrange(desc(Overall))
  
  print("Variáveis mais relevantes para inadimplência:")
  print(head(importancia, 10))
}

# -------------- Inadimplência e gastos associados? ---------------- #
# Responder pergunta: Com as informações dos itens anteriores, 
# você poderia afirmar que a inadimplência de um usuário
# está associada com seus níveis de gastos?
# Aplicar testes caso necessário
{
  # -------------------------
  # Inadimplência x Gastos
  # -------------------------
  
  # Testes não paramétricos
  wilcox <- wilcox.test(SPENDING ~ DEFAULT, data = card)
  brunner <- brunner.munzel.test(card$SPENDING[card$DEFAULT==0],
                                 card$SPENDING[card$DEFAULT==1])
  ks <- ks.test(card$SPENDING[card$DEFAULT==0],
                card$SPENDING[card$DEFAULT==1])
  
  # Resumir em tabela
  nao_parametricos <- data.frame(
    Teste = c("Mann-Whitney (Wilcoxon)", "Brunner-Munzel", "Kolmogorov-Smirnov"),
    Estatística = c(wilcox$statistic, brunner$statistic, ks$statistic),
    p_valor = c(wilcox$p.value, brunner$p.value, ks$p.value)
  )
  
  print(nao_parametricos)
  
  # -------------------------
  # Interpretação dos resultados
  # -------------------------
  alpha <- 0.05
  significativos <- nao_parametricos$p_valor < alpha
  
  if(any(significativos)){
    cat("\n>>> Conclusão: Existe evidência estatística de que os níveis de gastos diferem entre inadimplentes e não inadimplentes.\n")
  } else {
    cat("\n>>> Conclusão: Não há evidência estatística suficiente para afirmar que a inadimplência está associada aos níveis de gastos.\n")
  }
  
  # Comparação visual
  ggplot(card, aes(x = factor(DEFAULT), y = SPENDING, fill = factor(DEFAULT))) +
    geom_boxplot() +
    scale_x_discrete(labels = c("Não Inadimplente", "Inadimplente")) +
    labs(title = "Comparação de Gastos por Status de Inadimplência",
         x = "Status de Inadimplência", y = "Gastos") +
    theme_minimal()
  
  ggplot(gasto_inadimplencia, aes(x = DEFAULT_Label, y = Mediana_Gasto, fill = DEFAULT_Label)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = paste0("n=", n)), 
              vjust = -0.5, size = 3, color = "darkblue") +
    geom_text(aes(label = paste0(round(Mediana_Gasto, 2))), 
              vjust = 1.5, size = 3, color = "white", fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Mediana de Gastos por Casos de Inadimplência",
         x = "Casos de Inadimplência",
         y = "Mediana de Gastos ($)",
         caption = paste("Total de observações:", sum(gasto_inadimplencia$n))) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
}

# -------------- Modelo adequado para aprovação de crédito? -------------- #
# Verificar proporção de inadimplentes dentro dos aprovados;
# Pesquisar proporção desejável em referências;
# Apresentar modelo que melhore essa proporção.

# Modelo final = Regressão logística penalizada
{
  # Separar variáveis preditoras e resposta
  x <- as.matrix(card_model[,!names(card_model) %in% c("DEFAULT", "CARDHLDR")])
  y <- card_model$DEFAULT
  
  # ------------------------------
  # 3b. Class Weights
  # ------------------------------
  # Definir pesos inversamente proporcionais às classes
  wts <- ifelse(y == "1",
                sum(y == "0") / length(y),  # peso para inadimplente
                sum(y == "1") / length(y))  # peso para adimplente
  
  # ------------------------------
  # 4b. Regressão logística penalizada com Class Weights
  # ------------------------------
  cv.elastic_wt <- cv.glmnet(
    x, y,
    family = "binomial",
    alpha = 0.5,
    type.measure = "auc",
    weights = wts
  )
  
  best_lambda_wt <- cv.elastic_wt$lambda.min
  
  modelo_wt <- glmnet(
    x, y,
    family = "binomial",
    alpha = 0.5,
    lambda = best_lambda_wt,
    weights = wts
  )
  
  # ------------------------------
  # 5. Avaliação
  # ------------------------------

  # Previsões com class weights
  prob_wt <- predict(modelo_wt, newx = x, type = "response")
  roc_wt <- roc(as.numeric(as.character(y)), as.numeric(prob_wt))
  auc_wt <- auc(roc_wt)
  
  pred_wt <- ifelse(prob_wt > 0.5, "1", "0") %>% as.factor()
  cm_wt <- confusionMatrix(pred_wt, y, positive = "1")
  
  cat("AUC com Class Weights:", auc_wt, "\n")
  print(cm_wt)
}
