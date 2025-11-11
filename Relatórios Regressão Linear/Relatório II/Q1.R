system.time({# Configuração inicial
library(lmtest)
library(nortest)
library(tseries)
library(randtests)
library(moments)
library(car)
library(knitr)

set.seed(505)

# Função para análise das propriedades de AIC e BIC
analisar_propriedades <- function(n_obs, beta, n_simulacoes) {
  
  # 2. Função para realizar testes em um modelo
  realizar_testes <- function(modelo) {
    res <- rstandard(modelo)
    
    # Testes de normalidade
    adsd <- ad.test(res)
    llt <- lillie.test(res)
    jqbr <- jarque.bera.test(res)
    
    # Testes de aleatoriedade
    runs <- runs.test(res)
    ljung <- Box.test(res, lag = 10, type = "Ljung-Box")
    
    # Testes de linearidade
    harvey <- harvtest(modelo)
    reset <- resettest(modelo)
    
    # Testes de homocedasticidade
    bp <- bptest(modelo)
    gq <- gqtest(modelo)
    
    return(c(
      adsd = adsd$p.value,
      llt = llt$p.value,
      jqbr = jqbr$p.value,
      runs = runs$p.value,
      ljung = ljung$p.value,
      harvey = harvey$p.value,
      reset = reset$p.value,
      bp = bp$p.value,
      gq = gq$p.value
    ))
  }
  
  # 3. Função para simular dados
  simular_dados <- function() {
    # Gerar matriz X com posto completo
    X <- cbind(1, matrix(rnorm(n_obs * (length(beta) - 1)), nrow = n_obs, ncol = (length(beta) - 1)))
    
    # Simular erros e resposta Y
    epsilon <- rnorm(n_obs, mean = 0, sd = 1)
    Y <- X %*% beta + epsilon
    
    # Ajustar modelo
    modelo <- lm(Y ~ X - 1)
    
    # Calcular critérios
    aic <- AIC(modelo)
    bic <- BIC(modelo)
    
    # Realizar testes
    testes <- realizar_testes(modelo)
    
    calcular_esperanca_aic <- function(modelo, n_obs) {
      sigma_hat <- summary(modelo)$sigma^2
      k <- length(coef(modelo))
      
      # Fórmula simplificada assumindo que o modelo está corretamente especificado
      esperanca <- n_obs * (log(2*pi*sigma_hat) + 1) + 2*(k + 1)
      
      return(esperanca)
    }
    
    calcular_esperanca_bic <- function(modelo, n_obs) {
      sigma_hat <- summary(modelo)$sigma^2
      k <- length(coef(modelo))
      
      # Fórmula simplificada assumindo que o modelo está corretamente especificado
      esperanca <- n_obs * (log(2*pi*sigma_hat) + 1) + (k + 1)*log(n_obs)
      
      return(esperanca)
    }
    
    esperanca_aic <- calcular_esperanca_aic(modelo, n_obs)
    esperanca_bic <- calcular_esperanca_bic(modelo, n_obs)
    
    return(c(aic = aic, 
                bic = bic, 
                esperanca_aic = esperanca_aic, 
                esperanca_bic = esperanca_bic,
                testes = testes))
  }
  
  # 4. Realizar simulações
  resultados <- replicate(n_simulacoes, simular_dados())
  
  # 5. Extrair resultados
  aics <- resultados["aic", ]
  bics <- resultados["bic", ]
  
  # Matrizes de p-valores
  p_valores <- resultados[-(1:4), ]
  nomes_testes <- rownames(p_valores)
  
  esperanca_aics <- mean(resultados["esperanca_aic", ])
  esperanca_bics <- mean(resultados["esperanca_bic", ])
  
  # 6. Calcular proporções de rejeição
  proporcoes <- apply(p_valores, 1, function(x) mean(x < 0.05))
  
  # 7. Estatísticas descritivas
  stats_aic <- c(mean = mean(aics), sd = sd(aics), 
                 skewness = skewness(aics), kurtosis = kurtosis(aics))
  stats_bic <- c(mean = mean(bics), sd = sd(bics), 
                 skewness = skewness(bics), kurtosis = kurtosis(bics))
  
  # 8. Retornar resultados
  list(
    estatisticas = list(AIC = stats_aic, BIC = stats_bic),
    proporcoes_rejeicao = proporcoes,
    p_valores = p_valores,
    valores = list(AIC = aics, BIC = bics),
    esperanca_aics = esperanca_aics,
    esperanca_bics = esperanca_bics
  )
}

verificar_teoremas <- function(dados, nome, esperanca) {
  n <- length(dados)
  medias_acumuladas <- cumsum(dados)/seq_along(dados)
  
  # Cálculo do IC para a esperança teórica
  desvio_padrao <- sd(dados)
  erro_padrao <- desvio_padrao/sqrt(n)
  margem_erro <- qt(0.975, df=n-1) * erro_padrao
  ic_inf_teorico <- esperanca - margem_erro
  ic_sup_teorico <- esperanca + margem_erro
  
  # Plotagem
  plot(medias_acumuladas, type="l", 
       main=paste("Convergência com IC 95% (", nome, ")"),
       xlab="Número de simulações", ylab="Média acumulada",
       ylim=range(c(medias_acumuladas, esperanca, ic_inf_teorico, ic_sup_teorico)))
  
  # Linhas de referência
  abline(h=mean(dados), col="red", lty=2)
  abline(h=esperanca, col="blue", lty=3)
  
  # Intervalo de confiança da esperança teórica
  abline(h=ic_inf_teorico, col="blue", lty=4)
  abline(h=ic_sup_teorico, col="blue", lty=4)
  
  # Área do IC
  polygon(c(1, n, n, 1), 
          c(ic_inf_teorico, ic_inf_teorico, ic_sup_teorico, ic_sup_teorico),
          col=rgb(0,0,1,0.1), border=NA)
}

# Executar análise
# Alterar n_obs para o tamanho amostral desejado
resultados <- analisar_propriedades(n_obs = 50, beta = c(0.2, 0.4, -0.8), n_simulacoes = 10000)

# Visualizar resultados
print(resultados$estatisticas)
print(resultados$proporcoes_rejeicao)

# Gráficos
qqPlot(resultados$valores$AIC, main = "QQ-plot AIC",
       xlab="Quantis teóricos", ylab="Quantis amostrais")
qqPlot(resultados$valores$BIC, main = "QQ-plot BIC",
       xlab="Quantis teóricos", ylab="Quantis amostrais")
verificar_teoremas(resultados$valores$AIC, "AIC", resultados$esperanca_aics)
verificar_teoremas(resultados$valores$BIC, "BIC", resultados$esperanca_bics)

# Função para executar todos os testes e retornar uma tabela
gerar_tabela_pvalores <- function(dados) {
  # Executar todos os testes
  ad_aic <- ad.test(dados$AIC)
  ad_bic <- ad.test(dados$BIC)
  
  lillie_aic <- lillie.test(dados$AIC)
  lillie_bic <- lillie.test(dados$BIC)
  
  jb_aic <- jarque.bera.test(dados$AIC)
  jb_bic <- jarque.bera.test(dados$BIC)
  
  # Criar data frame com os resultados
  resultados <- data.frame(
    Teste = c("Anderson-Darling", "Lilliefors", "Jarque-Bera"),
    AIC = c(ad_aic$p.value, lillie_aic$p.value, jb_aic$p.value),
    BIC = c(ad_bic$p.value, lillie_bic$p.value, jb_bic$p.value)
  )
  
  # Arredondar p-valores para 4 casas decimais
  resultados$AIC <- round(resultados$AIC, 4)
  resultados$BIC <- round(resultados$BIC, 4)
  
  return(resultados)
}

# Gerar a tabela
tabela_pvalores <- gerar_tabela_pvalores(resultados$valores)

# Imprimir a tabela formatada
kable(tabela_pvalores, 
      caption = "P-valores dos testes de normalidade para AIC e BIC",
      col.names = c("Teste", "p-valor (AIC)", "p-valor (BIC)"),
      align = c('l', 'c', 'c'))

# Esperanças teóricas
print(resultados$esperanca_aics)
print(resultados$esperanca_bics)

# Função para calcular IC para a esperança teórica
calcular_IC_esperanca <- function(dados, esperanca_teorica, nome, confiança = 0.95) {
  n <- length(dados)
  media_amostral <- mean(dados)
  desvio_padrao <- sd(dados)
  erro_padrao <- desvio_padrao / sqrt(n)
  graus_liberdade <- n - 1
  
  # Valor crítico da t-Student
  t_critico <- qt((1 + confiança)/2, df = graus_liberdade)
  
  # Limites do IC
  margem_erro <- t_critico * erro_padrao
  ic_inferior <- media_amostral - margem_erro
  ic_superior <- media_amostral + margem_erro
  
  # Verificar se a esperança teórica está dentro do IC
  contem_esperanca <- (esperanca_teorica >= ic_inferior) && (esperanca_teorica <= ic_superior)
  
  # Retornar resultados
  list(
    media_amostral = media_amostral,
    esperanca_teorica = esperanca_teorica,
    ic_inferior = ic_inferior,
    ic_superior = ic_superior,
    contem_esperanca = contem_esperanca,
    margem_erro = margem_erro,
    resultados = data.frame(
      Métrica = nome,
      Média_Amostral = media_amostral,
      Esperança_Teórica = esperanca_teorica,
      IC_Inferior = ic_inferior,
      IC_Superior = ic_superior,
      Contém_Esperança = ifelse(contem_esperanca, "Sim", "Não"),
      Margem_Erro = margem_erro,
      N = n
    )
  )
}

# Aplicar a função aos resultados
ic_aic <- calcular_IC_esperanca(
  dados = resultados$valores$AIC,
  esperanca_teorica = mean(resultados$esperanca_aics),
  nome = "AIC"
)

ic_bic <- calcular_IC_esperanca(
  dados = resultados$valores$BIC,
  esperanca_teorica = mean(resultados$esperanca_bics),
  nome = "BIC"
)

# Exibir resultados
print(ic_aic$resultados)
print(ic_bic$resultados)
})