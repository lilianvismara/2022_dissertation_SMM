library(readr)
dados <- read_csv("dados.csv")
#View(dados)

modelo = glm(y ~ ., data = dados, family = binomial("logit"))
summary(modelo)

#teste de efeito de alguma variável explicativa (análise de deviância)
modelo_0 = glm(y ~ 1, data = dados, family = binomial("logit"))
anova(modelo_0, modelo)

exp(coef(modelo))

#variáveis latentes para melhorar a interpretação
#genero influencia, sendo que homens tem 0,56 vezes mais chances que mulheres de responder sim na questão 1 (y)
#seguir conteudos na rede social influencia, sendo que quem respondeu sim tem 3,03 vezes mais chances de 
#responder sim na questão 1 (y) do que quem respondeu não (em seguir conteúdos)


#
## De probabilidades para "chances" (odds)
#Digamos que a probabilidade de sucesso de determinado evento seja de $0,7 (70\%)$. 
#Sucesso é um conceito estatístico, ele representa uma quantidade a qual estamos interessados em modelar. 
#Esta quantidade pode ser a morte ou sobrevivência de indívíduos, depende do nosso objetivo. 
#
#Se a probabilidade de sucesso é $0,7$, então a probabilidade de fracasso é $1-0,7=0,3$. 
#A chance de sucesso é uma razão da probabilidade de sucesso sobre a probabilidade de fracasso. 
#No nosso exemplo a chance de sucesso é $0,7/0,3=$ `r 0.7/0.3`. 
#Isto significa que a chance de sucesso é de $2,33$ para $1$. 
#Se a probabilidade de sucesso é $50\%$ então a chance sucesso é $1$ para $1$.
#
#Quanto maior a probabilidade de sucesso maior a chance de sucesso, 
#mas enquanto a probabilidade assume valores no intervalo entre $0$ e $1$, 
#chance de sucesso assume valores entre $0$ e $+\infty$.

with(dados)
? with
with(dados, list(modelo_0,modelo))

