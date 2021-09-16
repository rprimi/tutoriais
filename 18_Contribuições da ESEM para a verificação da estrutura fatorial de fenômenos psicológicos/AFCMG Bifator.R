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
#Modelo bifactor
modelo_BF <- '
Estresse =~ DASS1  + DASS6  + DASS8  + DASS11 + DASS12 + DASS14 + DASS18 
Ansi     =~ DASS2  + DASS4  + DASS7  + DASS9  + DASS15 + DASS19 + DASS20
Depre    =~ DASS3  + DASS5  + DASS10 + DASS13 + DASS16 + DASS17 + DASS21

Distress =~ DASS1  + DASS2  + DASS3  + DASS4  + DASS5  + DASS6  + DASS7  + 
            DASS8  + DASS9  + DASS10 + DASS11 + DASS12 + DASS13 + DASS14 +
            DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21

Distress  ~~ 0*Estresse + 0*Ansi + 0*Depre
Estresse  ~~ 0*Ansi + 0*Depre
Ansi      ~~ 0*Depre
'

#rodar os resultados da AFC
resultados_BF <- sem(
  modelo_BF, 
  data = banco,                  
  ordered = names(banco), 
  estimator = "WLSMV"
)

#checar os resultados da AFC
summary(
  resultados_BF, 
  standardized=TRUE, 
  fit.measures=TRUE
)

############################# AFCMG #############################
#CONFIGURAL
configural <- measEq.syntax(
  configural.model=modelo_BF, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",
  group="Sexo", 
  return.fit=TRUE
)

#THRESHOLDS
thresholds <- measEq.syntax(
  configural.model=modelo_BF, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",
  group="Sexo", 
  return.fit=TRUE,
  group.equal="thresholds"
)

#MÉTRICA
metrica <- measEq.syntax(
  configural.model=modelo_BF, 
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

#ESCALAR (em modelos bifactor é necessário liberar manualmente as 
#médias/interceptos dos fatores)
#para saber mais consultar: https://github.com/simsem/semTools/issues/60

#gerar a sintaxe
sintaxe_escalar <- measEq.syntax(
  configural.model=modelo_BF, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",
  group="Sexo",  
  group.equal=c(
    "thresholds",
    "loadings",
    "intercepts"
              ),
  ID.fac = "Uv"
)

#abra a sintaxe e copie o resultado
cat(
  as.character(sintaxe_escalar)
)

#cole a sintaxe e altere o segundo "0" de LATENT MEANS/INTERCEPTS para "NA"
modelo_manual_escalar <- '
## LOADINGS:

Estresse =~ c(NA, NA)*DASS1 + c(lambda.1_1, lambda.1_1)*DASS1
Estresse =~ c(NA, NA)*DASS6 + c(lambda.2_1, lambda.2_1)*DASS6
Estresse =~ c(NA, NA)*DASS8 + c(lambda.3_1, lambda.3_1)*DASS8
Estresse =~ c(NA, NA)*DASS11 + c(lambda.4_1, lambda.4_1)*DASS11
Estresse =~ c(NA, NA)*DASS12 + c(lambda.5_1, lambda.5_1)*DASS12
Estresse =~ c(NA, NA)*DASS14 + c(lambda.6_1, lambda.6_1)*DASS14
Estresse =~ c(NA, NA)*DASS18 + c(lambda.7_1, lambda.7_1)*DASS18
Ansi =~ c(NA, NA)*DASS2 + c(lambda.8_2, lambda.8_2)*DASS2
Ansi =~ c(NA, NA)*DASS4 + c(lambda.9_2, lambda.9_2)*DASS4
Ansi =~ c(NA, NA)*DASS7 + c(lambda.10_2, lambda.10_2)*DASS7
Ansi =~ c(NA, NA)*DASS9 + c(lambda.11_2, lambda.11_2)*DASS9
Ansi =~ c(NA, NA)*DASS15 + c(lambda.12_2, lambda.12_2)*DASS15
Ansi =~ c(NA, NA)*DASS19 + c(lambda.13_2, lambda.13_2)*DASS19
Ansi =~ c(NA, NA)*DASS20 + c(lambda.14_2, lambda.14_2)*DASS20
Depre =~ c(NA, NA)*DASS3 + c(lambda.15_3, lambda.15_3)*DASS3
Depre =~ c(NA, NA)*DASS5 + c(lambda.16_3, lambda.16_3)*DASS5
Depre =~ c(NA, NA)*DASS10 + c(lambda.17_3, lambda.17_3)*DASS10
Depre =~ c(NA, NA)*DASS13 + c(lambda.18_3, lambda.18_3)*DASS13
Depre =~ c(NA, NA)*DASS16 + c(lambda.19_3, lambda.19_3)*DASS16
Depre =~ c(NA, NA)*DASS17 + c(lambda.20_3, lambda.20_3)*DASS17
Depre =~ c(NA, NA)*DASS21 + c(lambda.21_3, lambda.21_3)*DASS21
Distress =~ c(NA, NA)*DASS1 + c(lambda.1_4, lambda.1_4)*DASS1
Distress =~ c(NA, NA)*DASS6 + c(lambda.2_4, lambda.2_4)*DASS6
Distress =~ c(NA, NA)*DASS8 + c(lambda.3_4, lambda.3_4)*DASS8
Distress =~ c(NA, NA)*DASS11 + c(lambda.4_4, lambda.4_4)*DASS11
Distress =~ c(NA, NA)*DASS12 + c(lambda.5_4, lambda.5_4)*DASS12
Distress =~ c(NA, NA)*DASS14 + c(lambda.6_4, lambda.6_4)*DASS14
Distress =~ c(NA, NA)*DASS18 + c(lambda.7_4, lambda.7_4)*DASS18
Distress =~ c(NA, NA)*DASS2 + c(lambda.8_4, lambda.8_4)*DASS2
Distress =~ c(NA, NA)*DASS4 + c(lambda.9_4, lambda.9_4)*DASS4
Distress =~ c(NA, NA)*DASS7 + c(lambda.10_4, lambda.10_4)*DASS7
Distress =~ c(NA, NA)*DASS9 + c(lambda.11_4, lambda.11_4)*DASS9
Distress =~ c(NA, NA)*DASS15 + c(lambda.12_4, lambda.12_4)*DASS15
Distress =~ c(NA, NA)*DASS19 + c(lambda.13_4, lambda.13_4)*DASS19
Distress =~ c(NA, NA)*DASS20 + c(lambda.14_4, lambda.14_4)*DASS20
Distress =~ c(NA, NA)*DASS3 + c(lambda.15_4, lambda.15_4)*DASS3
Distress =~ c(NA, NA)*DASS5 + c(lambda.16_4, lambda.16_4)*DASS5
Distress =~ c(NA, NA)*DASS10 + c(lambda.17_4, lambda.17_4)*DASS10
Distress =~ c(NA, NA)*DASS13 + c(lambda.18_4, lambda.18_4)*DASS13
Distress =~ c(NA, NA)*DASS16 + c(lambda.19_4, lambda.19_4)*DASS16
Distress =~ c(NA, NA)*DASS17 + c(lambda.20_4, lambda.20_4)*DASS17
Distress =~ c(NA, NA)*DASS21 + c(lambda.21_4, lambda.21_4)*DASS21

## THRESHOLDS:

DASS1 | c(NA, NA)*t1 + c(DASS1.thr1, DASS1.thr1)*t1
DASS1 | c(NA, NA)*t2 + c(DASS1.thr2, DASS1.thr2)*t2
DASS1 | c(NA, NA)*t3 + c(DASS1.thr3, DASS1.thr3)*t3
DASS6 | c(NA, NA)*t1 + c(DASS6.thr1, DASS6.thr1)*t1
DASS6 | c(NA, NA)*t2 + c(DASS6.thr2, DASS6.thr2)*t2
DASS6 | c(NA, NA)*t3 + c(DASS6.thr3, DASS6.thr3)*t3
DASS8 | c(NA, NA)*t1 + c(DASS8.thr1, DASS8.thr1)*t1
DASS8 | c(NA, NA)*t2 + c(DASS8.thr2, DASS8.thr2)*t2
DASS8 | c(NA, NA)*t3 + c(DASS8.thr3, DASS8.thr3)*t3
DASS11 | c(NA, NA)*t1 + c(DASS11.thr1, DASS11.thr1)*t1
DASS11 | c(NA, NA)*t2 + c(DASS11.thr2, DASS11.thr2)*t2
DASS11 | c(NA, NA)*t3 + c(DASS11.thr3, DASS11.thr3)*t3
DASS12 | c(NA, NA)*t1 + c(DASS12.thr1, DASS12.thr1)*t1
DASS12 | c(NA, NA)*t2 + c(DASS12.thr2, DASS12.thr2)*t2
DASS12 | c(NA, NA)*t3 + c(DASS12.thr3, DASS12.thr3)*t3
DASS14 | c(NA, NA)*t1 + c(DASS14.thr1, DASS14.thr1)*t1
DASS14 | c(NA, NA)*t2 + c(DASS14.thr2, DASS14.thr2)*t2
DASS14 | c(NA, NA)*t3 + c(DASS14.thr3, DASS14.thr3)*t3
DASS18 | c(NA, NA)*t1 + c(DASS18.thr1, DASS18.thr1)*t1
DASS18 | c(NA, NA)*t2 + c(DASS18.thr2, DASS18.thr2)*t2
DASS18 | c(NA, NA)*t3 + c(DASS18.thr3, DASS18.thr3)*t3
DASS2 | c(NA, NA)*t1 + c(DASS2.thr1, DASS2.thr1)*t1
DASS2 | c(NA, NA)*t2 + c(DASS2.thr2, DASS2.thr2)*t2
DASS2 | c(NA, NA)*t3 + c(DASS2.thr3, DASS2.thr3)*t3
DASS4 | c(NA, NA)*t1 + c(DASS4.thr1, DASS4.thr1)*t1
DASS4 | c(NA, NA)*t2 + c(DASS4.thr2, DASS4.thr2)*t2
DASS4 | c(NA, NA)*t3 + c(DASS4.thr3, DASS4.thr3)*t3
DASS7 | c(NA, NA)*t1 + c(DASS7.thr1, DASS7.thr1)*t1
DASS7 | c(NA, NA)*t2 + c(DASS7.thr2, DASS7.thr2)*t2
DASS7 | c(NA, NA)*t3 + c(DASS7.thr3, DASS7.thr3)*t3
DASS9 | c(NA, NA)*t1 + c(DASS9.thr1, DASS9.thr1)*t1
DASS9 | c(NA, NA)*t2 + c(DASS9.thr2, DASS9.thr2)*t2
DASS9 | c(NA, NA)*t3 + c(DASS9.thr3, DASS9.thr3)*t3
DASS15 | c(NA, NA)*t1 + c(DASS15.thr1, DASS15.thr1)*t1
DASS15 | c(NA, NA)*t2 + c(DASS15.thr2, DASS15.thr2)*t2
DASS15 | c(NA, NA)*t3 + c(DASS15.thr3, DASS15.thr3)*t3
DASS19 | c(NA, NA)*t1 + c(DASS19.thr1, DASS19.thr1)*t1
DASS19 | c(NA, NA)*t2 + c(DASS19.thr2, DASS19.thr2)*t2
DASS19 | c(NA, NA)*t3 + c(DASS19.thr3, DASS19.thr3)*t3
DASS20 | c(NA, NA)*t1 + c(DASS20.thr1, DASS20.thr1)*t1
DASS20 | c(NA, NA)*t2 + c(DASS20.thr2, DASS20.thr2)*t2
DASS20 | c(NA, NA)*t3 + c(DASS20.thr3, DASS20.thr3)*t3
DASS3 | c(NA, NA)*t1 + c(DASS3.thr1, DASS3.thr1)*t1
DASS3 | c(NA, NA)*t2 + c(DASS3.thr2, DASS3.thr2)*t2
DASS3 | c(NA, NA)*t3 + c(DASS3.thr3, DASS3.thr3)*t3
DASS5 | c(NA, NA)*t1 + c(DASS5.thr1, DASS5.thr1)*t1
DASS5 | c(NA, NA)*t2 + c(DASS5.thr2, DASS5.thr2)*t2
DASS5 | c(NA, NA)*t3 + c(DASS5.thr3, DASS5.thr3)*t3
DASS10 | c(NA, NA)*t1 + c(DASS10.thr1, DASS10.thr1)*t1
DASS10 | c(NA, NA)*t2 + c(DASS10.thr2, DASS10.thr2)*t2
DASS10 | c(NA, NA)*t3 + c(DASS10.thr3, DASS10.thr3)*t3
DASS13 | c(NA, NA)*t1 + c(DASS13.thr1, DASS13.thr1)*t1
DASS13 | c(NA, NA)*t2 + c(DASS13.thr2, DASS13.thr2)*t2
DASS13 | c(NA, NA)*t3 + c(DASS13.thr3, DASS13.thr3)*t3
DASS16 | c(NA, NA)*t1 + c(DASS16.thr1, DASS16.thr1)*t1
DASS16 | c(NA, NA)*t2 + c(DASS16.thr2, DASS16.thr2)*t2
DASS16 | c(NA, NA)*t3 + c(DASS16.thr3, DASS16.thr3)*t3
DASS17 | c(NA, NA)*t1 + c(DASS17.thr1, DASS17.thr1)*t1
DASS17 | c(NA, NA)*t2 + c(DASS17.thr2, DASS17.thr2)*t2
DASS17 | c(NA, NA)*t3 + c(DASS17.thr3, DASS17.thr3)*t3
DASS21 | c(NA, NA)*t1 + c(DASS21.thr1, DASS21.thr1)*t1
DASS21 | c(NA, NA)*t2 + c(DASS21.thr2, DASS21.thr2)*t2
DASS21 | c(NA, NA)*t3 + c(DASS21.thr3, DASS21.thr3)*t3

## INTERCEPTS:

DASS1 ~ c(0, 0)*1 + c(nu.1, nu.1)*1
DASS6 ~ c(0, 0)*1 + c(nu.2, nu.2)*1
DASS8 ~ c(0, 0)*1 + c(nu.3, nu.3)*1
DASS11 ~ c(0, 0)*1 + c(nu.4, nu.4)*1
DASS12 ~ c(0, 0)*1 + c(nu.5, nu.5)*1
DASS14 ~ c(0, 0)*1 + c(nu.6, nu.6)*1
DASS18 ~ c(0, 0)*1 + c(nu.7, nu.7)*1
DASS2 ~ c(0, 0)*1 + c(nu.8, nu.8)*1
DASS4 ~ c(0, 0)*1 + c(nu.9, nu.9)*1
DASS7 ~ c(0, 0)*1 + c(nu.10, nu.10)*1
DASS9 ~ c(0, 0)*1 + c(nu.11, nu.11)*1
DASS15 ~ c(0, 0)*1 + c(nu.12, nu.12)*1
DASS19 ~ c(0, 0)*1 + c(nu.13, nu.13)*1
DASS20 ~ c(0, 0)*1 + c(nu.14, nu.14)*1
DASS3 ~ c(0, 0)*1 + c(nu.15, nu.15)*1
DASS5 ~ c(0, 0)*1 + c(nu.16, nu.16)*1
DASS10 ~ c(0, 0)*1 + c(nu.17, nu.17)*1
DASS13 ~ c(0, 0)*1 + c(nu.18, nu.18)*1
DASS16 ~ c(0, 0)*1 + c(nu.19, nu.19)*1
DASS17 ~ c(0, 0)*1 + c(nu.20, nu.20)*1
DASS21 ~ c(0, 0)*1 + c(nu.21, nu.21)*1

## SCALING FACTORS:

DASS1 ~*~ c(1, NA)*DASS1
DASS6 ~*~ c(1, NA)*DASS6
DASS8 ~*~ c(1, NA)*DASS8
DASS11 ~*~ c(1, NA)*DASS11
DASS12 ~*~ c(1, NA)*DASS12
DASS14 ~*~ c(1, NA)*DASS14
DASS18 ~*~ c(1, NA)*DASS18
DASS2 ~*~ c(1, NA)*DASS2
DASS4 ~*~ c(1, NA)*DASS4
DASS7 ~*~ c(1, NA)*DASS7
DASS9 ~*~ c(1, NA)*DASS9
DASS15 ~*~ c(1, NA)*DASS15
DASS19 ~*~ c(1, NA)*DASS19
DASS20 ~*~ c(1, NA)*DASS20
DASS3 ~*~ c(1, NA)*DASS3
DASS5 ~*~ c(1, NA)*DASS5
DASS10 ~*~ c(1, NA)*DASS10
DASS13 ~*~ c(1, NA)*DASS13
DASS16 ~*~ c(1, NA)*DASS16
DASS17 ~*~ c(1, NA)*DASS17
DASS21 ~*~ c(1, NA)*DASS21


## LATENT MEANS/INTERCEPTS:

Estresse ~ c(0, NA)*1 + c(alpha.1.g1, alpha.1.g2)*1
Ansi ~ c(0, NA)*1 + c(alpha.2.g1, alpha.2.g2)*1
Depre ~ c(0, NA)*1 + c(alpha.3.g1, alpha.3.g2)*1
Distress ~ c(0, NA)*1 + c(alpha.4.g1, alpha.4.g2)*1

## COMMON-FACTOR VARIANCES:

Estresse ~~ c(1, NA)*Estresse + c(psi.1_1.g1, psi.1_1.g2)*Estresse
Ansi ~~ c(1, NA)*Ansi + c(psi.2_2.g1, psi.2_2.g2)*Ansi
Depre ~~ c(1, NA)*Depre + c(psi.3_3.g1, psi.3_3.g2)*Depre
Distress ~~ c(1, NA)*Distress + c(psi.4_4.g1, psi.4_4.g2)*Distress

## COMMON-FACTOR COVARIANCES:

Estresse ~~ c(0, 0)*Ansi + c(psi.2_1.g1, psi.2_1.g2)*Ansi
Estresse ~~ c(0, 0)*Depre + c(psi.3_1.g1, psi.3_1.g2)*Depre
Estresse ~~ c(0, 0)*Distress + c(psi.4_1.g1, psi.4_1.g2)*Distress
Ansi ~~ c(0, 0)*Depre + c(psi.3_2.g1, psi.3_2.g2)*Depre
Ansi ~~ c(0, 0)*Distress + c(psi.4_2.g1, psi.4_2.g2)*Distress
Depre ~~ c(0, 0)*Distress + c(psi.4_3.g1, psi.4_3.g2)*Distress
'

#invariância escalar manual
escalar <- cfa(
  modelo_manual_escalar, 
  data = banco, 
  group = "Sexo", 
  ordered=names(banco), 
  estimator="WLSMV",
  std.lv = TRUE
)

#RESIDUAL (tem que fazer a mesma coisa para o modelo residual)

#gerar a sintaxe
sintaxe_residual <- measEq.syntax(
  configural.model=modelo_BF, 
  data=banco,
  ordered=names(banco), 
  estimator="WLSMV",     
  group="Sexo",
  group.equal=c(
    "thresholds",
    "loadings", 
    "intercepts",
    "residuals"),
  parameterization="theta"
)

#abra a sintaxe e copie o resultado
cat(
  as.character(sintaxe_residual)
)

#cole a sintaxe e altere o segundo "0" de LATENT MEANS/INTERCEPTS para "NA"
modelo_manual_residual <- '
## LOADINGS:

Estresse =~ c(NA, NA)*DASS1 + c(lambda.1_1, lambda.1_1)*DASS1
Estresse =~ c(NA, NA)*DASS6 + c(lambda.2_1, lambda.2_1)*DASS6
Estresse =~ c(NA, NA)*DASS8 + c(lambda.3_1, lambda.3_1)*DASS8
Estresse =~ c(NA, NA)*DASS11 + c(lambda.4_1, lambda.4_1)*DASS11
Estresse =~ c(NA, NA)*DASS12 + c(lambda.5_1, lambda.5_1)*DASS12
Estresse =~ c(NA, NA)*DASS14 + c(lambda.6_1, lambda.6_1)*DASS14
Estresse =~ c(NA, NA)*DASS18 + c(lambda.7_1, lambda.7_1)*DASS18
Ansi =~ c(NA, NA)*DASS2 + c(lambda.8_2, lambda.8_2)*DASS2
Ansi =~ c(NA, NA)*DASS4 + c(lambda.9_2, lambda.9_2)*DASS4
Ansi =~ c(NA, NA)*DASS7 + c(lambda.10_2, lambda.10_2)*DASS7
Ansi =~ c(NA, NA)*DASS9 + c(lambda.11_2, lambda.11_2)*DASS9
Ansi =~ c(NA, NA)*DASS15 + c(lambda.12_2, lambda.12_2)*DASS15
Ansi =~ c(NA, NA)*DASS19 + c(lambda.13_2, lambda.13_2)*DASS19
Ansi =~ c(NA, NA)*DASS20 + c(lambda.14_2, lambda.14_2)*DASS20
Depre =~ c(NA, NA)*DASS3 + c(lambda.15_3, lambda.15_3)*DASS3
Depre =~ c(NA, NA)*DASS5 + c(lambda.16_3, lambda.16_3)*DASS5
Depre =~ c(NA, NA)*DASS10 + c(lambda.17_3, lambda.17_3)*DASS10
Depre =~ c(NA, NA)*DASS13 + c(lambda.18_3, lambda.18_3)*DASS13
Depre =~ c(NA, NA)*DASS16 + c(lambda.19_3, lambda.19_3)*DASS16
Depre =~ c(NA, NA)*DASS17 + c(lambda.20_3, lambda.20_3)*DASS17
Depre =~ c(NA, NA)*DASS21 + c(lambda.21_3, lambda.21_3)*DASS21
Distress =~ c(NA, NA)*DASS1 + c(lambda.1_4, lambda.1_4)*DASS1
Distress =~ c(NA, NA)*DASS6 + c(lambda.2_4, lambda.2_4)*DASS6
Distress =~ c(NA, NA)*DASS8 + c(lambda.3_4, lambda.3_4)*DASS8
Distress =~ c(NA, NA)*DASS11 + c(lambda.4_4, lambda.4_4)*DASS11
Distress =~ c(NA, NA)*DASS12 + c(lambda.5_4, lambda.5_4)*DASS12
Distress =~ c(NA, NA)*DASS14 + c(lambda.6_4, lambda.6_4)*DASS14
Distress =~ c(NA, NA)*DASS18 + c(lambda.7_4, lambda.7_4)*DASS18
Distress =~ c(NA, NA)*DASS2 + c(lambda.8_4, lambda.8_4)*DASS2
Distress =~ c(NA, NA)*DASS4 + c(lambda.9_4, lambda.9_4)*DASS4
Distress =~ c(NA, NA)*DASS7 + c(lambda.10_4, lambda.10_4)*DASS7
Distress =~ c(NA, NA)*DASS9 + c(lambda.11_4, lambda.11_4)*DASS9
Distress =~ c(NA, NA)*DASS15 + c(lambda.12_4, lambda.12_4)*DASS15
Distress =~ c(NA, NA)*DASS19 + c(lambda.13_4, lambda.13_4)*DASS19
Distress =~ c(NA, NA)*DASS20 + c(lambda.14_4, lambda.14_4)*DASS20
Distress =~ c(NA, NA)*DASS3 + c(lambda.15_4, lambda.15_4)*DASS3
Distress =~ c(NA, NA)*DASS5 + c(lambda.16_4, lambda.16_4)*DASS5
Distress =~ c(NA, NA)*DASS10 + c(lambda.17_4, lambda.17_4)*DASS10
Distress =~ c(NA, NA)*DASS13 + c(lambda.18_4, lambda.18_4)*DASS13
Distress =~ c(NA, NA)*DASS16 + c(lambda.19_4, lambda.19_4)*DASS16
Distress =~ c(NA, NA)*DASS17 + c(lambda.20_4, lambda.20_4)*DASS17
Distress =~ c(NA, NA)*DASS21 + c(lambda.21_4, lambda.21_4)*DASS21

## THRESHOLDS:

DASS1 | c(NA, NA)*t1 + c(DASS1.thr1, DASS1.thr1)*t1
DASS1 | c(NA, NA)*t2 + c(DASS1.thr2, DASS1.thr2)*t2
DASS1 | c(NA, NA)*t3 + c(DASS1.thr3, DASS1.thr3)*t3
DASS6 | c(NA, NA)*t1 + c(DASS6.thr1, DASS6.thr1)*t1
DASS6 | c(NA, NA)*t2 + c(DASS6.thr2, DASS6.thr2)*t2
DASS6 | c(NA, NA)*t3 + c(DASS6.thr3, DASS6.thr3)*t3
DASS8 | c(NA, NA)*t1 + c(DASS8.thr1, DASS8.thr1)*t1
DASS8 | c(NA, NA)*t2 + c(DASS8.thr2, DASS8.thr2)*t2
DASS8 | c(NA, NA)*t3 + c(DASS8.thr3, DASS8.thr3)*t3
DASS11 | c(NA, NA)*t1 + c(DASS11.thr1, DASS11.thr1)*t1
DASS11 | c(NA, NA)*t2 + c(DASS11.thr2, DASS11.thr2)*t2
DASS11 | c(NA, NA)*t3 + c(DASS11.thr3, DASS11.thr3)*t3
DASS12 | c(NA, NA)*t1 + c(DASS12.thr1, DASS12.thr1)*t1
DASS12 | c(NA, NA)*t2 + c(DASS12.thr2, DASS12.thr2)*t2
DASS12 | c(NA, NA)*t3 + c(DASS12.thr3, DASS12.thr3)*t3
DASS14 | c(NA, NA)*t1 + c(DASS14.thr1, DASS14.thr1)*t1
DASS14 | c(NA, NA)*t2 + c(DASS14.thr2, DASS14.thr2)*t2
DASS14 | c(NA, NA)*t3 + c(DASS14.thr3, DASS14.thr3)*t3
DASS18 | c(NA, NA)*t1 + c(DASS18.thr1, DASS18.thr1)*t1
DASS18 | c(NA, NA)*t2 + c(DASS18.thr2, DASS18.thr2)*t2
DASS18 | c(NA, NA)*t3 + c(DASS18.thr3, DASS18.thr3)*t3
DASS2 | c(NA, NA)*t1 + c(DASS2.thr1, DASS2.thr1)*t1
DASS2 | c(NA, NA)*t2 + c(DASS2.thr2, DASS2.thr2)*t2
DASS2 | c(NA, NA)*t3 + c(DASS2.thr3, DASS2.thr3)*t3
DASS4 | c(NA, NA)*t1 + c(DASS4.thr1, DASS4.thr1)*t1
DASS4 | c(NA, NA)*t2 + c(DASS4.thr2, DASS4.thr2)*t2
DASS4 | c(NA, NA)*t3 + c(DASS4.thr3, DASS4.thr3)*t3
DASS7 | c(NA, NA)*t1 + c(DASS7.thr1, DASS7.thr1)*t1
DASS7 | c(NA, NA)*t2 + c(DASS7.thr2, DASS7.thr2)*t2
DASS7 | c(NA, NA)*t3 + c(DASS7.thr3, DASS7.thr3)*t3
DASS9 | c(NA, NA)*t1 + c(DASS9.thr1, DASS9.thr1)*t1
DASS9 | c(NA, NA)*t2 + c(DASS9.thr2, DASS9.thr2)*t2
DASS9 | c(NA, NA)*t3 + c(DASS9.thr3, DASS9.thr3)*t3
DASS15 | c(NA, NA)*t1 + c(DASS15.thr1, DASS15.thr1)*t1
DASS15 | c(NA, NA)*t2 + c(DASS15.thr2, DASS15.thr2)*t2
DASS15 | c(NA, NA)*t3 + c(DASS15.thr3, DASS15.thr3)*t3
DASS19 | c(NA, NA)*t1 + c(DASS19.thr1, DASS19.thr1)*t1
DASS19 | c(NA, NA)*t2 + c(DASS19.thr2, DASS19.thr2)*t2
DASS19 | c(NA, NA)*t3 + c(DASS19.thr3, DASS19.thr3)*t3
DASS20 | c(NA, NA)*t1 + c(DASS20.thr1, DASS20.thr1)*t1
DASS20 | c(NA, NA)*t2 + c(DASS20.thr2, DASS20.thr2)*t2
DASS20 | c(NA, NA)*t3 + c(DASS20.thr3, DASS20.thr3)*t3
DASS3 | c(NA, NA)*t1 + c(DASS3.thr1, DASS3.thr1)*t1
DASS3 | c(NA, NA)*t2 + c(DASS3.thr2, DASS3.thr2)*t2
DASS3 | c(NA, NA)*t3 + c(DASS3.thr3, DASS3.thr3)*t3
DASS5 | c(NA, NA)*t1 + c(DASS5.thr1, DASS5.thr1)*t1
DASS5 | c(NA, NA)*t2 + c(DASS5.thr2, DASS5.thr2)*t2
DASS5 | c(NA, NA)*t3 + c(DASS5.thr3, DASS5.thr3)*t3
DASS10 | c(NA, NA)*t1 + c(DASS10.thr1, DASS10.thr1)*t1
DASS10 | c(NA, NA)*t2 + c(DASS10.thr2, DASS10.thr2)*t2
DASS10 | c(NA, NA)*t3 + c(DASS10.thr3, DASS10.thr3)*t3
DASS13 | c(NA, NA)*t1 + c(DASS13.thr1, DASS13.thr1)*t1
DASS13 | c(NA, NA)*t2 + c(DASS13.thr2, DASS13.thr2)*t2
DASS13 | c(NA, NA)*t3 + c(DASS13.thr3, DASS13.thr3)*t3
DASS16 | c(NA, NA)*t1 + c(DASS16.thr1, DASS16.thr1)*t1
DASS16 | c(NA, NA)*t2 + c(DASS16.thr2, DASS16.thr2)*t2
DASS16 | c(NA, NA)*t3 + c(DASS16.thr3, DASS16.thr3)*t3
DASS17 | c(NA, NA)*t1 + c(DASS17.thr1, DASS17.thr1)*t1
DASS17 | c(NA, NA)*t2 + c(DASS17.thr2, DASS17.thr2)*t2
DASS17 | c(NA, NA)*t3 + c(DASS17.thr3, DASS17.thr3)*t3
DASS21 | c(NA, NA)*t1 + c(DASS21.thr1, DASS21.thr1)*t1
DASS21 | c(NA, NA)*t2 + c(DASS21.thr2, DASS21.thr2)*t2
DASS21 | c(NA, NA)*t3 + c(DASS21.thr3, DASS21.thr3)*t3

## INTERCEPTS:

DASS1 ~ c(0, 0)*1 + c(nu.1, nu.1)*1
DASS6 ~ c(0, 0)*1 + c(nu.2, nu.2)*1
DASS8 ~ c(0, 0)*1 + c(nu.3, nu.3)*1
DASS11 ~ c(0, 0)*1 + c(nu.4, nu.4)*1
DASS12 ~ c(0, 0)*1 + c(nu.5, nu.5)*1
DASS14 ~ c(0, 0)*1 + c(nu.6, nu.6)*1
DASS18 ~ c(0, 0)*1 + c(nu.7, nu.7)*1
DASS2 ~ c(0, 0)*1 + c(nu.8, nu.8)*1
DASS4 ~ c(0, 0)*1 + c(nu.9, nu.9)*1
DASS7 ~ c(0, 0)*1 + c(nu.10, nu.10)*1
DASS9 ~ c(0, 0)*1 + c(nu.11, nu.11)*1
DASS15 ~ c(0, 0)*1 + c(nu.12, nu.12)*1
DASS19 ~ c(0, 0)*1 + c(nu.13, nu.13)*1
DASS20 ~ c(0, 0)*1 + c(nu.14, nu.14)*1
DASS3 ~ c(0, 0)*1 + c(nu.15, nu.15)*1
DASS5 ~ c(0, 0)*1 + c(nu.16, nu.16)*1
DASS10 ~ c(0, 0)*1 + c(nu.17, nu.17)*1
DASS13 ~ c(0, 0)*1 + c(nu.18, nu.18)*1
DASS16 ~ c(0, 0)*1 + c(nu.19, nu.19)*1
DASS17 ~ c(0, 0)*1 + c(nu.20, nu.20)*1
DASS21 ~ c(0, 0)*1 + c(nu.21, nu.21)*1

## UNIQUE-FACTOR VARIANCES:

DASS1 ~~ c(1, 1)*DASS1 + c(theta.1_1, theta.1_1)*DASS1
DASS6 ~~ c(1, 1)*DASS6 + c(theta.2_2, theta.2_2)*DASS6
DASS8 ~~ c(1, 1)*DASS8 + c(theta.3_3, theta.3_3)*DASS8
DASS11 ~~ c(1, 1)*DASS11 + c(theta.4_4, theta.4_4)*DASS11
DASS12 ~~ c(1, 1)*DASS12 + c(theta.5_5, theta.5_5)*DASS12
DASS14 ~~ c(1, 1)*DASS14 + c(theta.6_6, theta.6_6)*DASS14
DASS18 ~~ c(1, 1)*DASS18 + c(theta.7_7, theta.7_7)*DASS18
DASS2 ~~ c(1, 1)*DASS2 + c(theta.8_8, theta.8_8)*DASS2
DASS4 ~~ c(1, 1)*DASS4 + c(theta.9_9, theta.9_9)*DASS4
DASS7 ~~ c(1, 1)*DASS7 + c(theta.10_10, theta.10_10)*DASS7
DASS9 ~~ c(1, 1)*DASS9 + c(theta.11_11, theta.11_11)*DASS9
DASS15 ~~ c(1, 1)*DASS15 + c(theta.12_12, theta.12_12)*DASS15
DASS19 ~~ c(1, 1)*DASS19 + c(theta.13_13, theta.13_13)*DASS19
DASS20 ~~ c(1, 1)*DASS20 + c(theta.14_14, theta.14_14)*DASS20
DASS3 ~~ c(1, 1)*DASS3 + c(theta.15_15, theta.15_15)*DASS3
DASS5 ~~ c(1, 1)*DASS5 + c(theta.16_16, theta.16_16)*DASS5
DASS10 ~~ c(1, 1)*DASS10 + c(theta.17_17, theta.17_17)*DASS10
DASS13 ~~ c(1, 1)*DASS13 + c(theta.18_18, theta.18_18)*DASS13
DASS16 ~~ c(1, 1)*DASS16 + c(theta.19_19, theta.19_19)*DASS16
DASS17 ~~ c(1, 1)*DASS17 + c(theta.20_20, theta.20_20)*DASS17
DASS21 ~~ c(1, 1)*DASS21 + c(theta.21_21, theta.21_21)*DASS21

## LATENT MEANS/INTERCEPTS:

Estresse ~ c(0, NA)*1 + c(alpha.1.g1, alpha.1.g2)*1
Ansi ~ c(0, NA)*1 + c(alpha.2.g1, alpha.2.g2)*1
Depre ~ c(0, NA)*1 + c(alpha.3.g1, alpha.3.g2)*1
Distress ~ c(0, NA)*1 + c(alpha.4.g1, alpha.4.g2)*1

## COMMON-FACTOR VARIANCES:

Estresse ~~ c(1, NA)*Estresse + c(psi.1_1.g1, psi.1_1.g2)*Estresse
Ansi ~~ c(1, NA)*Ansi + c(psi.2_2.g1, psi.2_2.g2)*Ansi
Depre ~~ c(1, NA)*Depre + c(psi.3_3.g1, psi.3_3.g2)*Depre
Distress ~~ c(1, NA)*Distress + c(psi.4_4.g1, psi.4_4.g2)*Distress

## COMMON-FACTOR COVARIANCES:

Estresse ~~ c(0, 0)*Ansi + c(psi.2_1.g1, psi.2_1.g2)*Ansi
Estresse ~~ c(0, 0)*Depre + c(psi.3_1.g1, psi.3_1.g2)*Depre
Estresse ~~ c(0, 0)*Distress + c(psi.4_1.g1, psi.4_1.g2)*Distress
Ansi ~~ c(0, 0)*Depre + c(psi.3_2.g1, psi.3_2.g2)*Depre
Ansi ~~ c(0, 0)*Distress + c(psi.4_2.g1, psi.4_2.g2)*Distress
Depre ~~ c(0, 0)*Distress + c(psi.4_3.g1, psi.4_3.g2)*Distress
'

#invariância escalar manual
residual <- cfa(
  modelo_manual_residual, 
  data = banco, 
  group = "Sexo", 
  ordered=names(banco), 
  estimator="WLSMV",
  std.lv = TRUE
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
