# -------------------- PREAMBULO
library(readr) 
dados <- read_csv("dados.csv")
View(dados)

# -------------------- ANÁLISE ESTATÍSTICA
# REGRESSÃO LOGÍSTICA BINÁRIA 
# Veja https://online.stat.psu.edu/stat504/lesson/6 
modelo = glm(y ~ ., data = dados, family = binomial("logit"))
# em que: 
# y = variável resposta (questão 1, sem relação com rede social)
# x = variáveis explicativas ou preditoras (questões de 2 a 5, que relaciona o uso de rede social)
summary(modelo) 

# ANÁLISE DE DEVIÂNCIA E SELEÇÃO DE MODELO 
# Teste de efeito de alguma variável latente (explicativa ou preditora)
modelo_0 = glm(y ~ 1, data = dados, family = binomial("logit"))
anova(modelo_0, modelo, test = "LRT") # teste da razão da verossimilhança (Likelyhood Ratio Test)
exp(coef(modelo)) # cálculo dos coeficientes do modelo


# -------------------- DEFINIÇÕES E CONSIDERAÇÕES
# Por definição, o desvio é uma estatística de qualidade de ajuste para um modelo 
# e é frequentemente usado para testes estatísticos de hipóteses. 
# É uma generalização -- do uso da soma dos quadrados dos resíduos em ajustes pormínimos quadrados ordinários --
# para casos em que o ajuste do modelo é obtido por máxima verossimilhança (likelyhood). 

# O desvio mede a discrepância entre o modelo completo e o modelo com apenas o intercepto.
# O modelo completo é o modelo que possui n parâmetros. Em particular, n=10.
# O modelo completo maximiza a função de probabilidade de log.
# Veja https://online.stat.psu.edu/stat504/lesson/6/6.3/6.3.4

# Nesta análise, utilizamos o conceito de variáveis latentes. 
# Variáveis latentes não são diretamente observadas, mas são inferidas (através de um 
# modelo matemático-estatístico) através da mensuração de variáveis observáveis.
# Em geral, são variáveis que não podem ser acessadas diretamente, mas que possuem manifestações 
# no mundo real (e.g., personalidade como extroversão e ou introversão).
# Veja https://pt.wikipedia.org/wiki/Vari%C3%A1vel_latente

# Probabilidades para "chances" (odds)
# Sucesso é um conceito estatístico que representa uma quantidade a qual estamos interessados em modelar,   
# a depender do nosso objetivo. 
# Veja https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/

# EXEMPLO: Digamos que a probabilidade de sucesso de determinado evento seja de 0,6 (60\%). 
# Se a probabilidade de sucesso é 0,6 então a probabilidade de fracasso é 1-0,6=0,4. 
# Já a chance de sucesso é uma razão da probabilidade de sucesso sobre a probabilidade de fracasso. 
# Logo, a chance de sucesso é 0,6/0,4=1,5. Isto significa que a chance de sucesso é de 1,5 para 1. 
# Consequentemente, se a probabilidade de sucesso é 50% então a chance sucesso é 1 para 1.
# Quanto maior a probabilidade de sucesso maior a chance de sucesso, 
# e esta probabilidade assume valores no intervalo entre $0$ e $1$. 
# Já a chance de sucesso assume valores entre 0 e +\infty.

# -------------------- INTERPRETAÇÃO 
# Em particular, podemos inferir que: 
# (1) o gênero influência, sendo que homens tem 0,506 vezes mais chances que mulheres de responder sim na questão 1 (y)
# (2) seguir conteúdos na rede social influência, sendo que quem respondeu sim tem 3,029 vezes mais chances de 
# responder sim na questão 1 (y) do que quem respondeu não (em seguir conteúdos em rede social).

# Reveja os parâmetros que permitem esta conclusão.
exp(coef(modelo))  

# Tabela de frequência das variáveis preditoras significativas na análise
with(dados, table(dados$genero,dados$segue))

