#instalar e habilitar o pacote lavaan
#install.packages("lavaan")
library(lavaan)
#carregar o banco de dados
banco<-readRDS (url("https://github.com/GustavHM/Capitulo-18_ESEM/raw/main/banco_CAASB_30.rds"))

################################## AFE ##################################
#criar o modelo da AFE
modelo_AFE <- '
efa("efa")*f1 + 
efa("efa")*f2 + 
efa("efa")*f3 +
efa("efa")*f4 +
efa("efa")*f5 =~ 
CAASB1  + CAASB2  + CAASB3  + CAASB4  + CAASB5  + CAASB6  +
CAASB7  + CAASB8  + CAASB9  + CAASB10 + CAASB11 + CAASB12 +
CAASB13 + CAASB14 + CAASB15 + CAASB16 + CAASB17 + CAASB18 +
CAASB19 + CAASB20 + CAASB21 + CAASB22 + CAASB23 + CAASB24 +
CAASB25 + CAASB26 + CAASB27 + CAASB28 + CAASB29 + CAASB30'

#rodar a AFE
AFE<-sem(modelo_AFE, data=banco, ordered=names(banco))

#checar os índices de ajuste e os resultados padronizados da AFE
options(max.print=1000000)
summary(AFE, fit.measures=T)

################### ESEM com controle de aquiescência ###################
#criar o modelo ESEM com controle de aquiescência por Intercepto Randômico
modelo_ESEM_IR <- '
efa("efa")*f1 + 
efa("efa")*f2 + 
efa("efa")*f3 +
efa("efa")*f4 +
efa("efa")*f5 =~
CAASB1  + CAASB2  + CAASB3  + CAASB4  + CAASB5  + CAASB6  +
CAASB7  + CAASB8  + CAASB9  + CAASB10 + CAASB11 + CAASB12 +
CAASB13 + CAASB14 + CAASB15 + CAASB16 + CAASB17 + CAASB18 +
CAASB19 + CAASB20 + CAASB21 + CAASB22 + CAASB23 + CAASB24 +
CAASB25 + CAASB26 + CAASB27 + CAASB28 + CAASB29 + CAASB30

aq =~ 
1*CAASB1  + 1*CAASB2  + 1*CAASB3  + 1*CAASB4  + 1*CAASB5  + 1*CAASB6  +
1*CAASB7  + 1*CAASB8  + 1*CAASB9  + 1*CAASB10 + 1*CAASB11 + 1*CAASB12 +
1*CAASB13 + 1*CAASB14 + 1*CAASB15 + 1*CAASB16 + 1*CAASB17 + 1*CAASB18 +
1*CAASB19 + 1*CAASB20 + 1*CAASB21 + 1*CAASB22 + 1*CAASB23 + 1*CAASB24 +
1*CAASB25 + 1*CAASB26 + 1*CAASB27 + 1*CAASB28 + 1*CAASB29 + 1*CAASB30
aq ~~ 0*f1 + 0*f2 + 0*f3 + 0*f4 + 0*f5'

#rodar a ESEM com controle de aquiescência
ESEM_IR<-sem(modelo_ESEM_IR, data=banco, ordered=names(banco))

#checar os índices de ajuste e os resultados padronizados da ESEM
summary(ESEM_IR, fit.measures=T)

#comprar modelos
anova(AFE, ESEM_IR)