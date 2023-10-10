
######################### Regressao Linear Simples #########################


# Passo 1: Carregar os pacotes que serao usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpmisc)


# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.table('dados_exercicio_regressao.txt') # Carregamento do arquivo txt
View(dados) # Visualizacao dos dados em janela separada

# Limpeza dos dados
colnames(dados) <- c("x", "y")
dados <- dados[-1, ]
unique(dados)
dados$x <- as.numeric(gsub(",", ".", dados$x))
dados$y <- as.numeric(gsub(",", ".", dados$y))

glimpse(dados)  # Visualizacao de um resumo dos dados



# Passo 3: Verificacao dos pressupostos para a regressao linear


## Relacao linear entre a x e y:


plot(dados$x, dados$y)


## Construcao do modelo:
mod <- lm(y ~ x, dados)


## Analise grafica:

par(mfrow=c(2,2))

plot(mod)

### Interpretacao: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))


## Normalidade dos residuos:
shapiro.test(mod$residuals)
# H0: a distribuição se aproxima da normal 
# Se p > 0,05, aceita h0.

## Outliers nos residuos:
summary(rstandard(mod))
# Os valores de min e max não devem passar de 3 e -3
# A mediana proximo de 0 é um bom indicativo


## Independencia dos residuos (Durbin-Watson):
durbinWatsonTest(mod)
# A recomendação é que a D-W Ststistic esteja próximo de 2
# Para que haja independência dos resíduos
# H0: autocorrelação dos resíduos é = 0 (NÃO EXISTE AUTOCORRELAÇÃO)
# Se p > 0,05, aceita H0

## Homocedasticidade (Breusch-Pagan):
bptest(mod)
# H0: existe homocedasticidade
# Se p > 0,05, aceita H0.

# Passo 4: Analise do modelo
summary(mod)


ggplot(data = dados, mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label),
                                 sep = "*plain(\",\")~~")),
               label.x = 0.05, label.y = 400,
               parse = TRUE, coef.digits = 5) +
  theme_classic()


# https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph

