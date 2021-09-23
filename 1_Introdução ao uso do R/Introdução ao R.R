###===--- Introducao ao R ---===###

### Limpa tudo da secao. USE COM CUIDADO!====
rm(list=ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

### Usando o R como uma calculadora====
## Adicao
2 + 6
## Multiplicacao
2 * 6
## Subtracao
2 - 6
## Divisao
2 / 6
## Exponencial
2 ^ 6
# ou voce pode usar tambem
2 ** 6
## Operador logico IGUAL A
3 == 4
## Operador logico DIFERENTE DE
3 != 4
## Operador logico VERDADEIRO
isTRUE(3 == 4)
## Operador logico FALSO
isFALSE(3 == 4)

### Tipos de objetos====
## Numérico
x = 3
## Caracter
x = "pipoca"

## Vetor numérico
x = c(1, 2, 3)

## Vetor de caracteres
x = c("amarelo", "verde", "3")

## Lista
x = list(1, 2, 3)

## Lista de listas
x = list(numero=list(1, 2, 3), caractere=list("amarelo", "verde", "3"))

## Matriz
x = matrix(c(1, 2, 3,
             3, 1, 2,
             2, 3, 1),
           nrow = 3,
           ncol = 3,
           byrow = TRUE)
## ou voce pode usar tambem
a = c(1, 3, 2)
b = c(2, 1, 3)
c = c(3, 2, 1)
x = cbind(a, b, c)

## Data frame
a = c(1, 3, 2)
b = c(2, 1, 3)
c = c(3, 2, 1)
x = data.frame(a, b, c)

### Carregar um banco de dados====
data <- read.csv("Exemplo.csv")

### Como instalar pacotes====
install.packages("psych")

### Como carregar os pacotes instalados====
require(psych)
## ou voce pode usar tambem
library(psych)

### Analise exploratoria de dados====
describe(data)
mean(data$qualidade)
mean(data[,4])
mean(data[,c("qualidade","preco")])
table(data$grupo)

### Estatisticas inferenciais====
## Correlacao
bem = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
satcoj = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
cor.test(bem, satcoj, method = " pearson")

## Regressao
amfa = c(0,10,20,30,40,50)
desmp = c(4,22,44,60,82,100)
lm.r = lm(desmp ~ amfa)
summary(lm.r)
plot(lm.r)

## Teste t
lider = c(0,10,20,30,40,50)
mandao = c(4,22,44,60,82,100)
t.test(lider, mandao, alternative = c("greater"), mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

## ANOVA
memoria = c(2,13,17,13,8,5,11,2,10,14,9,12,11,18,14)
grupo = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
fit <- aov(memoria ~ grupo)
anova(fit)

## Qui-quadrado
produto = c(1,0,0,1,1,0,0,1,1,1,1,1)
propaganda = c(2,1,2,1,2,3,1,3,1,3,2,2)
chisq.test(produto, propaganda)

####====---- THE END ----====####