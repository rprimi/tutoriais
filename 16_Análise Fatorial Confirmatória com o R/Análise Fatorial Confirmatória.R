###===--- Structural Equation Modeling ---===###

### Clean everything
rm(list=ls())
dev.off()
cat("\014")

### Install and load package====
require(lavaan)
require(semPlot)

dt <- read.csv("bfi_simulated.csv")

### CFA - 5 factors - MLM ====
bfi5.model <- '# modelo de medida
                AM =~ Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ AE1 + AE2 + AE3 + AE4 + AE5'

## Ajustando o modelo com estimador de Maxima Verossimilhanca (Maximum Likelihood)
fit <- cfa(bfi5.model, data = dt, estimator = "MLM") # ajusta o modelo usando MLM
summary(fit, fit.measures=TRUE, standardized = TRUE) # imprime o resumo dos resultados
mi <- modindices(fit) # imprime os indices de modificacao
parameterestimates(fit, standardize=T,  boot.ci.type = "perc", rsquare = TRUE) # imprime todos os par?metros estimados com IC
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC
factorScores <- data.frame(lavPredict(fit, type = "lv", method = "regression"))

### CFA - 5 factors - WLSMV ====
bfi5.model <- '# modelo de medida
                AM =~ Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ AE1 + AE2 + AE3 + AE4 + AE5'

fit <- cfa(model = bfi5.model, data = dt, estimator = "WLSMV")
summary(fit, standardized=T, fit.measures=T, rsquare=T)
mi <- modindices(fit)
parameterestimates(fit, standardize=T,  boot.ci.type = "perc") ### boot
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC
factorScores <- data.frame(lavPredict(fit, type = "lv", method = "regression"))

### CFA - Unifatorial - MLM ====
unifatorial.model <- '# modelo de medida
                F1 =~ Am1 + Am2 + Am3 + Am4 + Am5
                    + Co1 + Co2 + Co3 + Co4 + Co5
                    + Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                    + Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                    + AE1 + AE2 + AE3 + AE4 + AE5 '

fit <- cfa(unifatorial.model, data = dt, estimator = "MLM")
summary(fit, standardized=T, fit.measures=T, rsquare=T)
mi <- modindices(fit)
parameterestimates(fit, standardize=T,  boot.ci.type = "perc") ### boot
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC

factorScores <- data.frame(lavPredict(fit, type = "lv", method = "regression"))

### CFA - Unifatorial - WLSMV ====
unifatorial.model <- '# modelo de medida
                F1 =~ Am1 + Am2 + Am3 + Am4 + Am5
                    + Co1 + Co2 + Co3 + Co4 + Co5
                    + Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                    + Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                    + AE1 + AE2 + AE3 + AE4 + AE5'

fit <- cfa(unifatorial.model, data = dt, estimator = "WLSMV")
summary(fit, standardized=T, fit.measures=T, rsquare=T)
mi <- modindices(fit)
parameterestimates(fit, standardize=T,  boot.ci.type = "perc") ### boot
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC

factorScores <- data.frame(lavPredict(fit, type = "lv", method = "regression"))

### CFA - Null Model - MLM =====
fit <- cfa(lav_partable_independence(fit), data = dt, estimator = "MLM", orthogonal = FALSE)
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC

### CFA - Null Model - WLSMV        =====
fit <- cfa(lav_partable_independence(fit), data = dt, estimator = "WLSMV", orthogonal = FALSE)
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi")) #bic2 = SABIC


### CFA - Free all loadings - WLSMV====
cargaL.va1 <- ' AM =~ NA*Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ NA*Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ NA*Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ NA*Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ NA*AE1 + AE2 + AE3 + AE4 + AE5

                AM ~~ 1*AM
                CO ~~ 1*CO 
                EX ~~ 1*EX 
                NE ~~ 1*NE 
                AE ~~ 1*AE '

fit <- cfa(model = cargaL.va1, data = dt, estimator = "WLSMV") 
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi"))

### Modelo AFCMG====
bfi5.model <- ' AM =~ Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ AE1 + AE2 + AE3 + AE4 + AE5 '

# Invariância Configural
fit1 <- cfa(model = bfi5.model, data=dt,
            estimator="MLM", group="sex")
fitMeasures(fit1, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
# Invariância Métrica
fit2 <- cfa(model = bfi5.model, data=dt, 
            estimator="MLM", group="sex",
            group.equal=c("loadings"))
fitMeasures(fit2, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
# Invariância Escalar
fit3 <- cfa(model = bfi5.model, data=dt, 
            estimator="MLM", group="sex",
            group.equal=c("loadings","intercepts"))
fitMeasures(fit3, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
# Invariância Residual
fit4 <- cfa(model = bfi5.model, data=dt, 
            estimator="MLM", group="sex",
            group.equal=c("loadings","intercepts","residuals"))
fitMeasures(fit4, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
# Invariância Estrutural
fit5 <- cfa(model = bfi5.model, data=dt, 
            estimator="MLM", group="sex",
            group.equal=c("loadings","intercepts","residuals",
                          "lv.variances","lv.covariances"))
fitMeasures(fit5, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
# Invariância da Média
fit6 <- cfa(model = bfi5.model, data=dt, 
            estimator="MLM", group="sex",
            group.equal=c("loadings","intercepts","residuals",
                          "lv.variances","lv.covariances", "means"))
fitMeasures(fit6, fit.measures = c("chisq.scaled", "df", "gfi", "ecvi"))
anova(fit1,fit2,fit3,fit4,fit5,fit6)

### Modelo MIMIC====
mimicModel <- ' AM =~ Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ AE1 + AE2 + AE3 + AE4 + AE5 

                AM + CO + EX + NE + AE ~ age '

# Invariância Configural
fit <- lavaan::cfa(model = mimicModel, data=dt, estimator="MLM")
mi   <- modindices(fit)
what <- which(mi[ order(mi$epc, decreasing = T), 3 ] == "age")
mi[ order(mi$epc, decreasing = T)[what], ]
fitMeasures(fit, fit.measures = c("chisq.scaled", "df", "gfi", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", 
                                  "rmsea.ci.upper.scaled", "srmr", "aic", "bic", "ecvi"))

### Gráficos do modelo de 5 fatores com MLM====
bfi5.model <- '# modelo de medida
                AM =~ Am1 + Am2 + Am3 + Am4 + Am5
                CO =~ Co1 + Co2 + Co3 + Co4 + Co5
                EX =~ Ex1 + Ex2 + Ex3 + Ex4 + Ex5
                NE =~ Ne1 + Ne2 + Ne3 + Ne4 + Ne5
                AE =~ AE1 + AE2 + AE3 + AE4 + AE5'
fit <- cfa(bfi5.model, data = dt, estimator = "MLM")

semPaths(fit, title = F, residuals = F, layout = "circle")
semPaths(fit, title = F, label.cex = 1, residuals = F, nCharNodes = 4,
         sizeLat = 6, sizeMan = 5, edge.label.cex = 1, minimum = .3, 
         sizeInt = 0.8, mar = c(1, 1, 1, 1), edge.color = 1,
         intercepts = F, thresholds = F, layout = "circle", 
         "std", cut = .3, levels = c(1.5,2))

####====---- FIN(?) ----====####