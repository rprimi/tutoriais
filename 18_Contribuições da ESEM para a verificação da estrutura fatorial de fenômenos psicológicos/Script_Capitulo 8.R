#instalar e habilitar os pacotes lavaan e semTools
library(lavaan)
library(semTools)

#carregar o banco de dados
#no banco homem = 1 e mulher = 2
banco <- readRDS(
  url(
    "https://github.com/GustavHM/Capitulo-8_AFC/raw/main/banco_DASS21.rds"
  )
)

############################## AFC ###################################
#modelo 1 fator
M_1F <- '
Distress =~ DASS1  + DASS2  + DASS3  + DASS4  + DASS5  + DASS6  + DASS7  + 
            DASS8  + DASS9  + DASS10 + DASS11 + DASS12 + DASS13 + DASS14 +
            DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21'

#modelo 3 fatores
M_3F <- '
Estresse =~  DASS1  + DASS6  + DASS8  + DASS11 + DASS12 + DASS14 + DASS18 
Ansi     =~  DASS2  + DASS4  + DASS7  + DASS9  + DASS15 + DASS19 + DASS20
Depre    =~  DASS3  + DASS5  + DASS10 + DASS13 + DASS16 + DASS17 + DASS21'

#rodar os resultados da AFC
resultados_1F <- sem(
  model=M_1F, 
  data=banco, 
  ordered=names(banco), 
  estimator="WLSMV"
)

resultados_3F <- sem(
  model=M_3F, 
  data=banco, 
  ordered=names(banco), 
  estimator="WLSMV"
)

#comparar estruturas fatoriais 
anova(
  resultados_1F, 
  resultados_3F
)

#indices de ajuste das estruturas fatoriais
fitMeasures(
  resultados_1F,
  fit.measures=c(
    "chisq.scaled",
    "df","pvalue.scaled",
    "cfi.scaled",
    "tli.scaled",
    "rmsea.scaled",
    "srmr"
               )
)

fitMeasures(
  resultados_3F,
  fit.measures=c(
    "chisq.scaled",
    "df",
    "pvalue.scaled",
    "cfi.scaled",
    "tli.scaled",
    "rmsea.scaled",
    "srmr"
               )
)

#checar os resultados da AFC com 1 fator
summary(
  resultados_1F, 
  standardized=TRUE
)

#checar os resultados da AFC com 3 fatores
summary(
  resultados_3F, 
  standardized=TRUE
)

####################### AFCMG ################################
#CONFIGURAL
configural <- measEq.syntax(
  configural.model=M_3F, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",
  group="Sexo", 
  return.fit=TRUE
)

#THRESHOLDS
thresholds <- measEq.syntax(
  configural.model=M_3F, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",
  group="Sexo", 
  return.fit=TRUE,
  group.equal="thresholds"
)

#MÉTRICA
metrica <- measEq.syntax(
  configural.model=M_3F, 
  data=banco,
  ordered=names(banco),
  estimator="WLSMV",
  group="Sexo", 
  return.fit=TRUE,
  group.equal=c(
    "thresholds",
    "loadings"
              )
)

#ESCALAR
escalar <- measEq.syntax(
  configural.model=M_3F, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV", 
  group="Sexo", 
  return.fit=TRUE,
  group.equal=c(
    "thresholds",
    "loadings", 
    "intercepts"
              )
) 

#RESIDUAL
residual <- measEq.syntax(
  configural.model=M_3F, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV", 
  group="Sexo", 
  return.fit=TRUE,
  parameterization="theta",
  group.equal=c(
    "thresholds",
    "loadings", 
    "intercepts",
    "residuals"
              )
)

#comparar os modelos de invariância
invariancia <- compareFit(
  configural,
  thresholds,
  metrica, 
  escalar, 
  residual
)

summary(
  invariancia,
  fit.measures=c(
    "cfi.scaled",
    "rmsea.scaled",
    "srmr",
    "mfi"
               )
)

#Gamma hat
moreFitIndices(
  configural, 
  fit.measures="gammaHat.scaled"
)

moreFitIndices(
  thresholds, 
  fit.measures="gammaHat.scaled"
)

moreFitIndices(
  metrica, 
  fit.measures="gammaHat.scaled"
)

moreFitIndices(
  escalar, 
  fit.measures="gammaHat.scaled"
)

moreFitIndices(
  residual, 
  fit.measures="gammaHat.scaled"
)