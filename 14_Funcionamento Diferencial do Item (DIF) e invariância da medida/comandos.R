library (mirt)
library(lordif)


# itens politômicos -------------------------------------------------------

# gerar banco de dados para simulação

# indicar a semente
set.seed (1234)

# gerar dois objetos com os valores do parâmetro a (discriminação) aleatoriamente
# com probabilidade que segue distribuição lognormal, com média 1 e desvio 
# padrão 0,3
a1 = a2 = rlnorm (45, 1, .3)

# o segundo e o terceiro elementos do objeto a2 serão divididos por 8;
# isso faz com que esses itens fiquem menos discriminativos para o grupo 2
a2 [2:3] = a2 [2:3] / 8

# gerar dois objetos com os valores do parâmetro b (dificuldade) da categoria mais
# fácil aleatoriamente com probabilidade que segue distribuição normal, com 
# média -1 e desvio padrão 1
b = rnorm (45, -1, 1)

# determinar os valores de dificuldade das demais categorias dos itens
b1 = b2 = data.frame (b1 = b, b2 = b+.8, b3 = b+1.6, b4 = b+2.4)

# as dificuldades das categorias do primeiro e do terceiro itens do objeto b2
# serão acrescidas de 1,5;
# isso faz com que eles fiquem mais difíceis para o grupo 2
b2 [c(1,3),] = b2 [c(1,3),] + 1.5

# o pacote mirt trabalha com valores do intercepto em vez de valores de
# dificuldade do item; por isso, precisamos transformá-los
d1 = as.matrix (-a1*b1)
d2 = as.matrix (-a2*b2)

# gerar o banco com as respostas de cada grupo
sim1 = simdata (a1, d1, 1000, 'graded')
sim2 = simdata (a2, d2, 1000, 'graded')

# criar um banco único com todas as respostas
dados = rbind (sim1, sim2)

# objeto que contém a informação do grupo de cada sujeito
grupo = rep (c('G1', 'G2'), c(1000, 1000))



# análise de dif ----------------------------------------------------------

# calibração dos itens
calib1 = multipleGroup(dados, 1, group = grupo, 
                       itemtype = 'graded', TOL = .01, 
                       invariance = c('free_means', 'free_var', 
                                      colnames(dados)))

# estimação do escore
theta1 = fscores(calib1)

# identificação dos itens com DIF
dif = rundif(item = colnames(dados), resp = dados, 
             theta = as.numeric(theta1), gr = grupo,
             criterion = 'CHISQR', alpha = .05, wt = NULL)

which (dif$stats$chi12 <= .05)
which (dif$stats$pseudo12.Nagelkerke >= .035)

which (dif$stats$chi13 <= .05)
which (dif$stats$pseudo13.Nagelkerke >= .035)

which (dif$stats$chi23 <= .05)
which (dif$stats$pseudo23.Nagelkerke >= .035)

# purificação do escore
calib2 = multipleGroup(dados, 1, group = grupo, 
                       itemtype = 'graded', TOL = .01, 
                       invariance = c('free_means', 'free_var', 
                                      colnames(dados)[-c(1,2,3)]))

theta2 = fscores(calib2)

# Confirmação dos itens com DIF
dif = rundif(item = colnames(dados), resp = dados, 
             theta = as.numeric(theta2), gr = grupo,
             criterion = 'CHISQR', alpha = .05, wt = NULL)

which (dif$stats$chi12 <= .05)
which (dif$stats$pseudo12.Nagelkerke >= .035)

which (dif$stats$chi13 <= .05)
which (dif$stats$pseudo13.Nagelkerke >= .035)

which (dif$stats$chi23 <= .05)
which (dif$stats$pseudo23.Nagelkerke >= .035)

# verificação do ajuste do modelo
anova(calib1, calib2)


# gráficos ----------------------------------------------------------------


# Figura 4
# ATENÇÃO: este gráfico é referente à simulação com itens politômicos
plot (calib2, type="trace", which.items = 1, par.strip.text = list(cex = 0))


# Figura 5
# ATENÇÃO: este gráfico é referente à simulação com itens politômicos
itemplot (calib2, type="trace", item = 1, main = "Item 1",
          par.settings = list(strip.background = NULL),
          par.strip.text = list(cex = .8))



# Figura 6
# ATENÇÃO: este gráfico é referente à simulação com itens politômicos

# obter o modelo para cada grupo
g1 = extract.group(calib2, 1)
g2 = extract.group(calib2, 2)

# calcular a probabilidade de escolha de cada categoria de cada item
p1 = probtrace(g1, seq(-4, 4, 0.01))
p2 = probtrace(g2, seq(-4, 4, 0.01))

# comando para exportar o gráfico
# caso não queira salvar o gráfico, ignore o próximo comando e o comando dev.off()
jpeg (filename = "poli_meugrafico.jpg", width = 1500, 
      height = 1500, quality = 100, res = 300)

# plotar a base do gráfico
plot (seq(-4, 4, 0.01), p1[,1], ylim = c(0, 1), 
      type = "n", main = 'Item 1', ylab = 'Probabilidade', xlab = expression(theta))

# inserir as legendas
legend(x = 2.5, y = 1, legend = c('G1', 'G2', 'G1', 'G2'), lty = c(1,2, NA, NA), 
       pch = c(NA,NA,21,22), cex = .8, bty = 'n', title = 'Grupo', merge = TRUE,)
legend(x = 2.5, y = .6, legend = paste0('Cat_', 1:5), lty = 1, 
       cex = .8, col = 1:5, bty = 'n', title = 'Categoria',
       merge = TRUE,)

# inserir as curvas de cada grupo
# o loop vai de 1 a 5 porque são as colunas que contêm as probabilidades das
# categorias do item 1; se fosse para o item 2, o loop deveria ir de 6 a 10
for (i in 1:5)
{lines (seq(-4, 4, 0.01), p1[,i], lty = 1, col = i)
  lines (seq(-4, 4, 0.01), p2[,i], lty = 2, col = i)}

# inserir as frequências de respostas
# primeiro, determinar os níveis usando a função cut; serão 32 níveis, de -3 a +3, com intervalo de 0,2
niveis = cut(theta2, c (-Inf, seq(-3,3,.2), Inf), labels = 1:32)
# agora, criar uma tabela de frequência para o item 1 cruzando o acerto, o grupo e o nível; na verdade será uma array com três dimensões
tab.cat = table (dados[,1], grupo, niveis)

# o loop abaixo insere o ponto que representa a frequência de cada resposta para cada grupo e nível; cada grupo terá um símbolo associado
# o objeto i é o nível
for (i in 1:32)
{
  # o objeto k é a categoria
  for (k in 1:5)
  {
    # o objeto g é o grupo
    for (g in 1:2)
    {
      points (
        x = -3+(i-1)*.2,
        # a posição no eixo y é a frequência relativa
        y = tab.cat[k,g,i]/sum(tab.cat[,g,i]),
        # o argumento cex indica o tamanho do símbolo; ele será tão maior quanto for a frequência de resposta neste ponto
        cex = ifelse (tab.cat[k,g,i]<=10, .4,
          ifelse (tab.cat[k,g,i]<=100, .8,
            ifelse (tab.cat[k,g,i]<=1000, 1.2, 1.6))),
        # a cor do símbolo e o tipo
        col = k, pch = g+20
      )
    }
  }
}
dev.off()


# itens dicotômicos -------------------------------------------------------

# gerar banco de dados para simulação

set.seed (1234)
a1 = a2 = rlnorm (45, 1, .3)

# gerar dois objetos com os valores do parâmetro b (dificuldade)
b1 = b2 = rnorm (45, 0, 1)

# alteração nos primeiro, segundo e terceiro itens
a2 [2:3] = a2 [2:3] / 8
b2 [c(1,3)] = b2 [c(1,3)] + 1.5

d1 = -a1*b1
d2 = -a2*b2
modelo = rep ('2PL', 45)
sim1 = simdata (a1, d1, 1000, modelo)
sim2 = simdata (a2, d2, 1000, modelo)
dados = rbind (sim1, sim2)
grupo = rep (c('G1', 'G2'), c(1000, 1000))


# análise de DIF ----------------------------------------------------------

# calibração dos itens
calib1 = multipleGroup(dados, 1, group = grupo, 
                       itemtype = '2PL', TOL = .01, 
                       invariance = c('free_means', 'free_var', 
                                      colnames(dados)))

# estimação do escore
theta1 = fscores(calib1)

# identificação dos itens com DIF
dif = rundif(item = colnames(dados), resp = dados, 
             theta = as.numeric(theta1), gr = grupo,
             criterion = 'CHISQR', alpha = .05, wt = NULL)

which (dif$stats$chi12 <= .05)
which (dif$stats$pseudo12.Nagelkerke >= .035)

which (dif$stats$chi13 <= .05)
which (dif$stats$pseudo13.Nagelkerke >= .035)

which (dif$stats$chi23 <= .05)
which (dif$stats$pseudo23.Nagelkerke >= .035)

# purificação do escore
calib2 = multipleGroup(dados, 1, group = grupo, 
                       itemtype = '2PL', TOL = .01, 
                       invariance = c('free_means', 'free_var', 
                                      colnames(dados)[-c(1,2,3)]))

theta2 = fscores(calib2)

# Confirmação dos itens com DIF
dif = rundif(item = colnames(dados), resp = dados, 
             theta = as.numeric(theta2), gr = grupo,
             criterion = 'CHISQR', alpha = .05, wt = NULL)

which (dif$stats$chi12 <= .05)
which (dif$stats$pseudo12.Nagelkerke >= .035)

which (dif$stats$chi13 <= .05)
which (dif$stats$pseudo13.Nagelkerke >= .035)

which (dif$stats$chi23 <= .05)
which (dif$stats$pseudo23.Nagelkerke >= .035)

# verificação do ajuste do modelo
anova(calib1, calib2)

# gráficos ----------------------------------------------------------------


# Figura 3
# ATENÇÃO: este gráfico é referente à simulação com itens dicotômicos
library (difNLR)
dif.nlr = difORD(dados, grupo, focal.name = 'G2', model = 'cumulative')
plot (dif.nlr, item = 1)


# Figura 7
# ATENÇÃO: este gráfico é referente à simulação com itens dicotômicos

# obter o modelo para cada grupo
g1 = extract.group(calib2, 1)
g2 = extract.group(calib2, 2)

# calcular a probabilidade de escolha de cada categoria de cada item
p1 = probtrace(g1, seq(-4, 4, 0.01))
p2 = probtrace(g2, seq(-4, 4, 0.01))

# comando para exportar o gráfico
# caso não queira salvar o gráfico, ignore o próximo comando e o comando dev.off()
jpeg (filename = "dico_meugrafico.jpg", width = 1500, 
      height = 1500, quality = 100, res = 300)

# plotar a base do gráfico
plot (seq(-4, 4, 0.01), p1[,1], ylim = c(0, 1), 
      type = "n", main = 'Item 1', ylab = 'Probabilidade', xlab = expression(theta))

# inserir as legendas
legend(x = 2.5, y = .9, legend = c('G1', 'G2', 'G1', 'G2'), lty = c(1,2, NA, NA), pch = c(NA,NA,0,1), cex = .8, bty = 'n', xjust = 0, title = 'Grupo')

# inserir as linhas dos grupos
lines (seq(-4, 4, 0.01), p1[,2], lty = 1)
lines (seq(-4, 4, 0.01), p2[,2], lty = 2)

# inserir as frequências de respostas
niveis = cut(theta2, c (-Inf, seq(-3,3,.2), Inf), labels = 1:32)
tab.cat = table (dados[,1], grupo, niveis)

# inserir o ponto que representa a frequência de cada resposta para cada grupo e nível
for (i in 1:32)
{
  for (g in 1:2)
  {
    points (x = -3+(i-1)*.2, 
            y = tab.cat[2,g,i]/sum(tab.cat[,g,i]), 
            # tamanho do símbolo 
            cex = if (tab.cat[2,g,i]<=10) {.4} else
              if (tab.cat[2,g,i]<=100) {.8} else
                if (tab.cat[2,g,i]<=1000) {1.2} else {1.6},
            # o tipo do símbolo
            pch = g-1)
  }
}
dev.off()


