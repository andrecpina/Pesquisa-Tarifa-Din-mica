#Instalar pacotes
# Pacote para ajustar o modelo de regressão multinomial e prever probabilidade
install.packages("nnet")
# Pacote de visualização grafica
install.packages("ggplot2")
# Pacote de funções basicas para manipulação de dados
install.packages("dplyr")
# Pacote para diminuir a quantidade de colunas e subir a quantidade de linhas
install.packages("tidyr")
# Adiciona funcionalidades de classificação e regressão
install.packages("caret")

# Carregar os pacotes intalados
library(nnet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(caret)

# Carrega o banco de dados em excel para o R
file_path <- "~/Documents/USP/TCC/03 - TCC Final/base_original.xlsx"
data <- read_excel(file_path)

# Teste para verificar se o banco foi carregado corretamente
head(data)

# Comando para converter a variável Genero em Fator pois ela é uma variável categorica discreta e não numérica
data$Genero <- as.factor(data$Genero)

# ************************ Gênero x Distância *********************************

# Converter a variável Distancia para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Distancia, levels = c("1 - Muito Pouco Importante", "2 - Pouco Importante", 
                                                    "3 - Indiferente", "4 - Importante", "5 - Muito Importante"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Distancia, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Distancia, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Importância da Distância",
       x = "Importância da Distância", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Potência *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Potencia para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Potencia, levels = c("1 - Muito Pouco Importante", "2 - Pouco Importante", 
                                                    "3 - Indiferente", "4 - Importante", "5 - Muito Importante"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Potencia, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Potencia, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Importância da Potência",
       x = "Importância da Potência", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Preço *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Preço para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Preco, levels = c("1 - Muito Pouco Importante", "2 - Pouco Importante", 
                                                   "3 - Indiferente", "4 - Importante", "5 - Muito Importante"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Preco, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Preco, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Importância do Preco",
       x = "Importância do Preco", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Serviço *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Serviço para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Servico, levels = c("1 - Muito Pouco Importante", "2 - Pouco Importante", 
                                                "3 - Indiferente", "4 - Importante", "5 - Muito Importante"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Servico, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Servico, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Importância do Serviço",
       x = "Importância do Serviço", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Segurança *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Segurança para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Seguranca, levels = c("1 - Muito Pouco Importante", "2 - Pouco Importante", 
                                                  "3 - Indiferente", "4 - Importante", "5 - Muito Importante"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Seguranca, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Seguranca, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Importância da Segurança",
       x = "Importância da Segurança", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Distância x Barato *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Barato para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Barato, levels = c("1 - Menos de 2km", "2 - de 2km - 5km", 
                                                    "3 - de 5km - 7km", "4 - de 7km - 10km", "5 - Mais de 10km"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Barato, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Barato, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Distância pelo preço mais baixo",
       x = "Distância à percorrer", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Distância x Media *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Media para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Media, levels = c("1 - Menos de 2km", "2 - de 2km - 5km", 
                                                 "3 - de 5km - 7km", "4 - de 7km - 10km", "5 - Mais de 10km"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Media, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Media, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Distância pelo preço médio",
       x = "Distância à percorrer", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Distância x Cara *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Cara para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Cara, levels = c("1 - Menos de 2km", "2 - de 2km - 5km", 
                                                "3 - de 5km - 7km", "4 - de 7km - 10km", "5 - Mais de 10km"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Cara, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Cara, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Distância pelo preço mais caro",
       x = "Distância à percorrer", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Distância x Cara Serviço *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Caraservico para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Caraservico, levels = c("1 - Menos de 2km", "2 - de 2km - 5km", 
                                               "3 - de 5km - 7km", "4 - de 7km - 10km", "5 - Mais de 10km"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Caraservico, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Caraservico, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por Distância pelo preço mais caro com disponibilidade de serviços",
       x = "Distância à percorrer", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))

# ************************ Gênero x Justiça *********************************

# Excluir as quatro últimas colunas para melhor impressão do proximo grafico
data <- data %>% select(-ncol(data):- (ncol(data)-3))

# Converter a variável Justica para fator ordenado (apesar de já estar em uma escala Likert, ela é uma variavel categorica não numerica) caso isso não for realizado, o R pode entender os numeros da escala Likert como numeros continuos.
data$Distancia <- factor(data$Justica, levels = c("1 - Discordo Totalmente", "2 - Discordo", 
                                                      "3 - Indeciso", "4 - Concordo", "5 - Concordo Totalmente"),
                         ordered = TRUE)

# Ajusta um modelo de regressão logística multinomial onde o objetivo é prever a variável Genero com base na variável explicativa Distancia com ajuste do log-likelihood atraves da quantidade de interações
modelo <- multinom(Genero ~ Justica, data = data)

# Coeficiente de Regressão, Erro Padrão em relação a cada coeficiente, Residual Deviance e AIC
summary(modelo)

# colocar em uma variavel para usar nas funções a seguir
summary_modelo <- summary(modelo)

# Calcular o Odds Ratio (OR)
cat("\nOdds Ratio (OR):\n")
odds_ratios <- exp(summary_modelo$coefficients)
print(odds_ratios)

# Valor p (p-value)
cat("\nValores p (P-value):\n")
z_values <- summary_modelo$coefficients / summary_modelo$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print(p_values)

# Pseudo R²
cat("\nPseudo R² (McFadden):\n")
ll_null <- logLik(multinom(Genero ~ 1, data = data))  # Modelo nulo
ll_full <- logLik(modelo)  # Modelo ajustado
pseudo_r2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(pseudo_r2)

# Imprimir a matriz de confusão
#cat("\nMatriz de Confusão:\n")
#matriz_confusao <- confusionMatrix(as.factor(previsoes_classe), as.factor(data$Genero))
#print(matriz_confusao)

# Prever as probabilidades preditivas
cat("\nProbabilidades Preditivas:\n")
probabilidades <- predict(modelo, newdata = data, type = "probs")
head(probabilidades)

# Nomear as colunas de acordo com os variaveis de "Genero"
# colnames(probabilidades) <- levels(data$Genero)
# Adicionar as colunas de probabilidades ao conjunto de dados original
data <- cbind(data, probabilidades)

# Reformular os dados para formato longo, para facilitar o gráfico multiplicando os mesmos ID porém com uma varialve apenas por linha
data_long <- data %>%
  pivot_longer(cols = levels(data$Genero),
               names_to = "Genero_predito", 
               values_to = "probabilidade")

ggplot(data_long, aes(x = Justica, y = probabilidade * 100, color = Genero_predito, group = Genero_predito)) +  # Multiplicar por 100 para converter em percentuais
  geom_smooth(se = FALSE, method = "loess") +  # Linhas suavizadas
  geom_point(size = 3) +  # Pontos "grandes" nos dados preditivos
  scale_color_manual(values = c("Masculino" = "blue", 
                                "Feminino" = "orange", 
                                "Outros" = "gray", 
                                "Prefiro não informar" = "black")) +  # Definição de cores
  labs(title = "Probabilidades de Gênero por senso de justiça no preço dinâmico",
       x = "Distância à percorrer", y = "Probabilidade (%)") + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, 100, by = 5))
