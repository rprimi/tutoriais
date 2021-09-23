### Aplicações da Psicometria Bayesiana: Do Básico ao Avançado

### Limpar a área de trabalho, os gráficos e o console
rm(list=ls())
dev.off()
cat("\014")

######### Escore de soma====
### Gerar dados aleatórios de padrão de resposta de um estudante em 100 testes,
### com 4 questões cada. O viés verdadeiro do estudante é igual a 0,50.
set.seed(123)
n     <- 100
probs <- .5
x     <- rbinom(n, 4, probs)
  
### Ajustar os dados a um modelo binomial
fitdistrplus::fitdist(x, "binom", fix.arg=list('size'=4), start=list("prob"=.7))

### Transformar os dados para um vetor binário
bin <- as.vector(sapply(seq_along(x), function(g) rep(c(0,1), c(4-x[g], x[g]))))
## Ajustar os dados a um modelo de Bernoulli
fitdistrplus::fitdist(bin, "binom", fix.arg=list('size'=1), start=list("prob"=.7))

### Usar o modelo Bayesiano
install.packages("remotes")
remotes::install_github("vthorrf/bsem")
require(bsem)
modeloBayes <- bern.score(bin)
## Recuperar a estimativa pontual
modeloBayes$abil
## Recuperar HDI da estimativa
modeloBayes$abilHDI[,1]
## Distribuição posteriori da estimativa
plot(density(modeloBayes$abilFull[,1]), main="Distribuição posteriori da aptidão",
     xlab=expression(theta), ylab="Densidade")

######### Modelagem por Equações Estruturais Bayesiana====
require(psych)
data(bfi)
ocean <- bfi[complete.cases(bfi),1:25] # Selecionar apenas os itens do questionário

set.seed(123)
amostra <- sample(1:nrow(ocean), 300, replace=F)
myData <- ocean[amostra,]

factors <- rep(1:5, each=5) # Definir quais itens fazem parte de qual fator

MEEB <- BSEM(myData, factors)

MEEB$output

MEEB$corr

plot(MEEB$abil)

######### MEEB - Dois parâmetros logísticos====
myData <- myData - 1
min(myData) == 0
RevData <- reverse.code(c(-1,1,1,1,1, 
                          1,1,1,-1,-1, 
                          -1,-1,1,1,1, 
                          1,1,1,1,1, 
                          1,-1,1,1,-1), myData)

k <- max(myData)
MEEBIRT <- BSEMIRT(RevData, factors, k=k)

MEEBIRT$output

MEEBIRT$corr

plot(MEEBIRT$abil)

######### Comparação====
MEEB$dic; MEEBIRT$dic

####====---- FIM ----====####