# Pacotes:

# Visualizacao de dados psicometricos
#install.packages("qgraph")
# Bootstraping Techniques for networks
#install.packages("bootnet")
# Analises psicometricas EFA/IRT/Reliability e outras
#install.packages("psych")
# Estimação de redes mistas 
# install.packages("mgm")
library(mgm)
library(qgraph)
library(bootnet)
library(psych)

#exemplo
x<-matrix(c(1,.3,.5,.3,1,.7,.5,.7,1),3,3,byrow = F)
x
par(mfrow=c(1,1))
svg(filename="figura1.svg", 
    width=5, 
    height=5, 
    pointsize=12)
cor.plot(x,numbers = T,upper =F,cex=1.5)
dev.off()

svg(filename="figura2.svg", 
    width=5, 
    height=5, 
    pointsize=12)
qgraph(x,layout="spring",labels=c("V1","V2","V3"),posCol="blue",edge.labels=T,edge.label.cex=1.5,edge.label.color="black")
dev.off()
### Exemplo parametrico (Gaussian network), no caso presumidamente parametricos (correlacoes policoricas)
dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames
# Grupos de itens por dimensao (estresse, ansiedade, depressao)
# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)
# Exemplo inicial com a subescala de depressao 
dasspoly_sub<-dasspoly[,c(3,5,10,13,16,17,21)]
# Visualizacoes de correlacoes, informacao utilizada nos modelos fatoriais (var/covar)
svg(filename="figura3.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
cor.plot(cor_auto(dasspoly_sub),numbers = T)
dev.off()
# Correlacoes em redes (http://psychosystems.org/files/Literature/EpskampEtAl2012.pdf)
# Rede correlacoes bivariadas
svg(filename="figura4.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)],posCol="blue",labels=colnames(dasspoly_sub))
dev.off()
# Algoritmo de posicionamento (https://onlinelibrary.wiley.com/doi/abs/10.1002/spe.4380211102)
svg(filename="figura5.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),nodeNames=dasslabels[c(3,5,10,13,16,17,21)],posCol="blue",labels=colnames(dasspoly_sub),layout="spring")
dev.off()
# paradoxo da informacao: muita informacao é pouca informacao, poucas distincoes
# correlacao carrega relacoes espurias, vieses
# uma solucao possivel e a correlacao parcial
# correlacoes pariciais sao utilizadas em outros modelos, como por exemplo, nas regressoes multiplas

# Rede de correlacoes parciais (http://psycnet.apa.org/record/2018-13501-001?doi=1)
# Utiliza os argumentos graph, sampleSize e threshold para estimar as correlacoes parciais regularizadas
svg(filename="figura6.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
dasspoly_sub_g<-qgraph(cor_auto(dasspoly_sub),layout="spring",nodeNames=dasslabels[c(3,5,10,13,16,17,21)],posCol="blue",labels=colnames(dasspoly_sub),graph="glasso",sampleSize=nrow(dasspoly_sub),threshold=T,minimum=0.1,lambda.min.ratio=.002)
dev.off()
# usa um metodo regularizado, fixando em zero valores pequenos e resolvendo assim o problema de esparcialidade

# Como identificar nodos mais relevantes: centralidade
# Medidas de centralidade (http://psychosystems.org/files/Literature/Bootnet.pdf)
#(https://www.sciencedirect.com/science/article/abs/pii/S0092656614000701)
centralityPlot(dasspoly_sub_g,include = "all",labels = dasslabels[c(3,5,10,13,16,17,21)])
svg(filename="figura7.svg", 
    width=3, 
    height=5, 
    pointsize=12)
centralityPlot(dasspoly_sub_g,include = "ExpectedInfluence",orderBy = "ExpectedInfluence",labels=colnames(dasspoly_sub))
dev.off()
# Matriz esparsa de correlacao parcial regularizada ou matriz de predicao: os numeros!!!
# Sao interpretados como betas de regressao: 0.1 = pequeno, 0.3 = medio e 0.5 = grande
# Matriz adjacente, função getWmat
View(getWmat(dasspoly_sub_g))
svg(filename="figura8.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
cor.plot(getWmat(dasspoly_sub_g),numbers = T)
dev.off()

###########
library(bootnet)
Network <- estimateNetwork(dasspoly_sub,default = "EBICglasso",corMethod ="cor_auto",threshold=T)
boot1 <- bootnet(Network, nBoots = 2500, nCores = 8,statistics = c("edge","ExpectedInfluence"),type = "nonparametric") 
svg(filename="figura9.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
plot(boot1,plot="interval",order = "sample")
dev.off()
svg(filename="figura10.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
plot(boot1, "ExpectedInfluence", order = "sample")
dev.off()

boot2 <- bootnet(Network, nBoots = 2500,
                  type = "case", nCores = 8,statistics = c("edge","ExpectedInfluence"))
svg(filename="figura11.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
plot(boot2,"ExpectedInfluence")
dev.off()
corStability(boot2)

#####
describe(dasspoly_sub)
library(mgm)
dasspoly_sub
net_fit<-mgm(dasspoly_sub,
             type = c(rep("g",7)),
             level = c(rep(1,7)),
             ruleReg = "AND",
             k = 2,
             threshold = "HW",
             lambdaGam = .25)
pred_fit<-predict(net_fit,dasspoly_sub,
                  errorCon = "R2")
svg(filename="figura12.svg", 
    width=7.5, 
    height=5, 
    pointsize=12)
qgraph(net_fit$pairwise$wadj,
       pie=pred_fit$errors[,2],
       nodeNames=dasslabels[c(3,5,10,13,16,17,21)],
       labels=colnames(dasspoly_sub),
       layout="spring",
       posCol="blue")
dev.off()
