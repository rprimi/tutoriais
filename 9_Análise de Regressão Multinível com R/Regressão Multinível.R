###===--- Multilevel regression ---===###

### Start
rm(list=ls())
dev.off()
cat("\014")

### Packages
require(lme4)
require(lmerTest)
require(mlmRev)
require(lattice)

### Personal functions====
BCompare <- function(bics, rounding=4) {
  dbics <- bics - min(bics)  # Difference of the BICs with the best fitting
                             # -min(bics)-model.
  
  BFs <- exp( (-.5) * dbics) # Return the Bayes factors. Values below 1 favor
                             # the model in min(bics).
  BFs <- round(BFs,rounding) # Rounding for aesthetics
  
  wM <- BFs/sum(BFs)         # Return the Bayesian model weights. This is the
                             # posterior probability of the models given the
                             # data, assuming exaustiveness of the models.
  wM <- round(wM, rounding)  # Rounding for aesthetics
  
  Result <- matrix(cbind(bics,dbics,BFs,wM),ncol=4)
  colnames(Result) <- c("BIC","dBIC","BF","w")
  row.names(Result) <- sapply(1:length(bics), function(g) 
    if(g < 10) { paste("Model_",0,g,sep="") } 
    else { paste("Model_",g,sep="") })
  return(Result)
}
ACompare <- function(aics, rounding=4) {
  daics <- aics - min(aics)  # Difference of the BICs with the best fitting
                             # -min(bics)-model.
  
  LRs <- exp( (-.5) * daics) # Return the Bayes factors. Values below 1 favor
                             # the model in min(bics).
  LRs <- round(LRs,rounding) # Rounding for aesthetics
  
  wM <- LRs/sum(LRs)         # Return the Bayesian model weights. This is the
                             # posterior probability of the models given the
                             # data, assuming exaustiveness of the models.
  wM <- round(wM, rounding)  # Rounding for aesthetics
  
  Result <- matrix(cbind(aics,daics,LRs,wM),ncol=4)
  colnames(Result) <- c("AIC","dAIC","LR","w")
  row.names(Result) <- sapply(1:length(aics), function(g) 
    if(g < 10) { paste("Model_",0,g,sep="") } 
    else { paste("Model_",g,sep="") })
  return(Result)
}

### Data====
data(Exam)

### Step 0: Simple linear model====
model0 <- lm(normexam ~ standLRT, data=Exam)
summary(model0)

### Step 1: Null-model====
model1 <- lmerTest::lmer(normexam ~ 1 + (1 | school), data=Exam)
summary(model1)

### Step 2: Random intercept and fixed predictor in individual level====
model2 <- lmerTest::lmer(normexam ~ standLRT + (1 | school), data=Exam)
summary(model2)

### Step 3: Random intercept and random slope====
model3 <- lmerTest::lmer(normexam ~ standLRT + (standLRT | school), data=Exam)
summary(model3)

### Step 4: Random intercept with individual and group level predictors====
model4 <- lmerTest::lmer(normexam ~ standLRT + schavg + (1 + standLRT | school), data=Exam)
summary(model4)

### Step 5: Random intercept with cross-level interaction====
model5 <- lmerTest::lmer(normexam ~ standLRT * schavg + (1 + standLRT | school), data=Exam)
summary(model5)

### Step 6: Comparing models====
Models <- list(model0, model1, model2, model3, model4, model5)
BICs   <- sapply(seq_along(Models), function(g) BIC(Models[[g]]))
AICs   <- sapply(seq_along(Models), function(g) AIC(Models[[g]]))
ResultadoB <- BCompare(BICs); ResultadoA <- ACompare(AICs)
rownames(ResultadoB) <- rownames(ResultadoA) <- c("Modelo Linear", "Modelo Nulo", "Modelo misto", 
                                                  "Modelo aleatório", "Dois níveis", "Interação")
ResultadoA
ResultadoB

### Find specific intercepts and slopes for best BIC model====
coef(Models[[which.min(BICs)]])

### Diagnostic plots====
# Residual plots
plot(Models[[which.min(BICs)]], type = c("p", "smooth"),
     ylab="Resíduos", xlab="Estimativas")

# Scale location plot
plot(Models[[which.min(BICs)]], sqrt(abs(resid(.))) ~ fitted(.),type = c("p", "smooth"),
     ylab=expression(sqrt("|Resíduos|")), xlab="Estimativas")

## QQ-Plot
lattice::qqmath(Models[[which.min(BICs)]], id = 0.05)

### Visualizing Random and Fixed Effects for best BIC model====
fix <- fixef(Models[[which.min(BICs)]])
rand <- ranef(Models[[which.min(BICs)]])
paramsSchool <- cbind((rand$school[1]+fix[1]),(rand$school[2]+fix[2]))

plot(data = Exam, normexam ~ standLRT,type = 'n', 
     ylim = c(min(Exam$normexam),max(Exam$normexam)),
     xlim = c(min(Exam$standLRT),max(Exam$standLRT)),
     cex.main = .75,
     xlab = "Escores std - Capacidade de Leitura", ylab = "Escores std - Conhecimentos gerais")

for(i in 1:length(unique(Exam$school))){
  abline(a = paramsSchool[i,1], b = paramsSchool[i,2],col = 'lightgray')
  par<- par(new=F)
}

abline(a = fix[1], b = fix[2], lwd= 2,col = 'black')


####====---- THE END(?) ----====####