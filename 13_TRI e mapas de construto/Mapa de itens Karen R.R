##### Caminho da pasta de trabalho

# ajustar no seu computador o caminho da pasta de trabalho
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documentos icloud/Artigos e capitulos em construcao/Capítulo Karen 2020/Base de dados")

##### Bibliotecas

install.packages("TAM")
install.packages("devtools")
install.packages("GPArotation")
install.packages('WrightMap')
install.packages('eRm')
install.packages("psych")
install.packages("tidyverse")
install.packages("sjmisc")

devtools::install_github('datarootsio/artyfarty') ## instala pacote artyfarty diretamente do github

library(tidyverse)
library(sjmisc)
library(TAM)
library(psych)
library(readxl)
library(knitr)
library(RColorBrewer)
library(GPArotation)
library(WrightMap)
library(eRm)


source("http://www.labape.com.br/rprimi/R/utils_construct_maps.R")

# library(xlsx)

##### 1. Lê/importa banco

base <- read_excel("base e dicionário.xlsx", sheet = "base")

# carrega a descrição das variáveis da planilha "dic"
dic <- read_excel("base e dicionário.xlsx", sheet = "dic")

##### 2. Examina base e seleciona os itens

dic_etape <- dic %>% filter(test == "etape") # se quisermos analisar apenas um fator, mudar aqui (domain == "C", por exemplo)

# observação: o comando "dic %>%" indica que a operacão que segue será feita com a base "dic"
# detalhes em https://uc-r.github.io/pipe
# é um recurso chamado "pipe"

vars <- dic_etape$coditem
labels <- dic_etape$text

dt <- base[ , vars]

##### 3. Análise psicométrica classica

alpha(dt, check.keys = TRUE)
omega(dt,nfactors = 2, plot = T)
glb(dt)

##### 4. Inverte itens negativos e transforma de 1-5 para 0-4, inverte itens reversos

i_reversos<-dic_etape$pole==0 # seleciona apenas os itens reversos (pole=0)
ni_reversos=sum(i_reversos)   # conta quantos itens são reversos

if (ni_reversos>0) {
  dt[,i_reversos]<-6-dt[,i_reversos]
  }

# muda a escala de 1-5 para 0-4

dt<-dt-1


## observação: o comando abaxo (Ricardo) dá uma mensagem de erro indicando que "funs" foi deprecado. Ainda assim roda

dt  <- dt %>%
  mutate_if(dic_etape$pole == 0, funs(6 -.)) %>% # inverte negativos
  na.omit %>%                                # elimina missings
  mutate_all(funs(. -1)) # transforma a escala para 0 a 4   


##### 5. Calibra modelo de créditos parciais

pcm  <- tam.mml(resp = dt, irtmodel = "PCM")

##### 6. Examina ajuste dos itens 

fit_itens<- msq.itemfit(pcm) # calcula fit por itens sem simulações
summary(fit_itens)

fit <- tam.fit(pcm) # calcula fit por categorias
summary(fit)

##### Calcula separação e estratos

PSI<-pcm$EAP.rel # IRT person separation reliability
PSI

G<-(PSI/(1-PSI))^.5 # índice de separação
H<-(4*G+1)/3 # nº de estratos


# plot(tri, ngroups = 24)

##### 7. Examina curva dos itens

summary(pcm)

# plot(pcm, type = "items") # comando original do Ricardo

plot(pcm, type="items",items = c(1,2,3)) # itens selecionados

plot(pcm, type="items") # todos os itens

##### 7. Descritiva e Mapa de construto

describe_likert5_items(
  data = dt,
  item_tresh =  tam.threshold(pcm),
  coditem = vars,
  item_text = vars, # era =labels
  pole = dic_etape$text
)
  
##### 8. Mapa de construto: 

person_item_map_v3( 
  item_tresh =  tam.threshold(pcm),
  coditem = dic_etape$coditem,
  item_text = dic_etape$text2,
  pole = dic_etape$pole,
  theta = pcm$person$EAP,
  min = -3, 
  max = 4,
  item_text_max = 80, # estava 28
  binwidth = .4,
  size_categ_label = 3,
  size_bar = 4,
  categ_label = c("1", "2", "3", "4", "5"),
  categ_color = c("#DF4949", "#FBB941","#EEE657", "#2B82C9",  "#2CCA90"),
  intercept=0,
  color_hist = "#DF4949"
)

##### Curva de informação do teste

info_curves<-IRT.informationCurves(pcm,theta=seq(-3,3,len=40))
plot(y=info_curves$test_info_curve,x = info_curves$theta,type="l")

##### Produz mapa de construto dicotômico 

# apresenta em ordem de dificuldade

ordem<-order(pcm$item_irt$beta)

itens<-substr(vars,1,3)

wrightMap( thetas = pcm$person$EAP, thresholds = pcm$item_irt$beta[ordem],label.items.rows = 2,label.items.cex = .8, vertLines="T",thr.lab.text=itens[ordem], label.items=itens[ordem],item.prop = .7)

# problema: o mapa a seguir apresenta a posiçào os itens e não seu rótulo

wrightMap( thetas = pcm$person$EAP, thresholds = pcm$item_irt$beta,label.items.rows = 3, item.side=itemClassic, item.prop = .4)


# a opção label.items=vars não funciona no formato clássico

##

# Compute Separation Reliability for a Rasch Model using eRm: 

objeto<-PCM(dt) ## roda PCM pelo pacote eRm

pers <- person.parameter(objeto)
res <- SepRel(pers)
res
summary(res)

library(RColorBrewer)



describe_likert5_items(
  data = dt,
  item_tresh =  tam.threshold(pcm),
  coditem = vars,
  item_text = vars, # era =labels
  pole = dic_etape$text
)

# define função
Apresenta<-function(s1,s2,s3,s4,s5)
 
{
  print(s1)
  print(s2)
  print(s3)
  print(s4)
  print(s5)
  }


# chama função
a<-"tempo"
b<-"12h"
c<-5

grava_orig(
  variavel="tempo",
  resposta=b,
  id_respondente = c)
)
