# Carregar bibliotecas necessárias
library(GA)
library(ggplot2)

set.seed(123)
# Definir as distâncias entre as cidades (matriz de distâncias)
distancias <- matrix(c(
  0, 42, 61, 30, 17, 82, 31, 11, 27,
  42, 0, 14, 87, 28, 70, 19, 33, 13,
  61, 14, 0, 20, 81, 21, 8, 29, 15,
  30, 87, 20, 0, 34, 33, 91, 10, 20,
  17, 28, 81, 34, 0, 41, 34, 82, 22,
  82, 70, 21, 33, 41, 0, 19, 32, 27,
  31, 19, 8, 91, 34, 19, 0, 59, 30,
  11, 33, 29, 10, 82, 32, 59, 0, 32,
  27, 13, 15, 20, 22, 27, 30, 32, 0
), nrow = 9, byrow = TRUE)

# Nomes das cidades (opcional)
nomes_cidades <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

# Função para calcular o custo total de uma rota
calcular_custo <- function(rota) {
  custo_total <- 0
  for (i in 1:(length(rota) - 1)) {
    cidade_atual <- rota[i]
    proxima_cidade <- rota[i + 1]
    custo_total <- custo_total + distancias[cidade_atual, proxima_cidade]
  }
  # Retornar à cidade inicial
  custo_total <- custo_total + distancias[rota[length(rota)], rota[1]]
  return(custo_total)
}

# Função de aptidão (quanto menor o custo, melhor)
funcao_aptidao <- function(rota) {
  return(-calcular_custo(rota))  # Negativo porque queremos minimizar o custo
}

# Configurar e executar o Algoritmo Genético
resultado <- ga(
  type = "permutation",               # Tipo de problema (permutação)
  fitness = funcao_aptidao,           # Função de aptidão
  lower = 1,                          # Valor mínimo para cada gene (cidade)
  upper = 9,                          # Valor máximo para cada gene (cidade)
  popSize = 15,                       # Tamanho da população
  maxiter = 3500,                     # Número máximo de gerações
  pmutation = 0.5,                    # Taxa de mutação (taxa de mortalidade)
  elitism = 1,                        # Número de indivíduos que sobrevivem sem alteração
  run = 100,           # Critério de parada personalizado
  monitor = TRUE                      # Monitorar o progresso
)

# Melhor solução encontrada
melhor_rota <- resultado@solution[1, ]
melhor_custo <- -resultado@fitnessValue
primeira_ocorrencia <- which(-evolucao$max == melhor_custo)[1]

# Exibir resultados
cat("Melhor rota encontrada:", nomes_cidades[melhor_rota], "\n")
cat("Custo total:", melhor_custo, "km\n")
cat("Iteração onde o valor ótimo foi encontrado pela primeira vez:", primeira_ocorrencia, "\n")

# Plotar a evolução das populações
evolucao <- resultado@summary
evolucao <- as.data.frame(evolucao)
evolucao$iteracao <- 1:nrow(evolucao)

# Plotar a evolução da melhor aptidão ao longo das gerações (valores positivos)
ggplot(evolucao, aes(x = iteracao)) +
  geom_line(aes(y = -max, color = "Melhor Custo"), linewidth = 1) +  # Multiplicar por -1 para valores positivos
  labs(title = "Evolução do Melhor Custo ao Longo das Gerações",
       x = "Iteração",
       y = "Melhor Custo (km)",
       color = "Legenda") +
  theme_minimal() +
  scale_color_manual(values = c("Melhor Custo" = "red"))  # Cor da linha
