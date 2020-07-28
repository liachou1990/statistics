### Inputs:
## 1) dataDir:  The relative path to the directory containing your data

dataDir  <- "../data/"
fileName <- "wvs_data.rds"
wvs_data <- readRDS(paste0(dataDir, fileName))

### Answers to the questions:

### Question 2.1 Multiple linear regression
## 2.1.1 Which countries are represented in these data?
## Find the unique country values:
unique(wvs_data$V2)

## 2.1.2 What are the sample sizes for each country represented in these data?
## Make a table:
table(wvs_data$V2)

## 2.1.3 Overall, is there a significant effect of country on feelings of happiness?
HappinessModel<-lm(V10~V2, data=wvs_data)
summary(HappinessModel)

## 2.1.4 Which country has the highest level of feelings of happiness?
Happinesstable<-aggregate(wvs_data$V10~wvs_data$V2, FUN=mean)
Happinesstable$`wvs_data$V2`[Happinesstable$`wvs_data$V10` == min(Happinesstable$`wvs_data$V10`)]
min(Happinesstable$`wvs_data$V10`)
Happinesstable

## 2.1.5 Which country has the lowest level of feelings of happiness?
Happinesstable<-aggregate(wvs_data$V10~wvs_data$V2, FUN=mean)
Happinesstable$`wvs_data$V2`[Happinesstable$`wvs_data$V10` == max(Happinesstable$`wvs_data$V10`)]

## 2.1.6 How do the country-specific levels of feelings of happiness change after controlling for subjective state
## of health?
HappinessModel2<-lm(V10~V2+V11, data=wvs_data)
summary(HappinessModel2)$coefficient


### Question 2 Continuous variable moderation
## 2.2.1 After controlling for country, does the importance people afforded to democracy (DemImp)
## significantly predict the extent to which they think their country is being run
## democratically (DemRun)?
DemRunModel<-lm(V141 ~ V140 + V2 ,data=wvs_data)
summary(DemRunModel)

## 2.2.2 After controlling for country, does the DemImp -> DemRun effect vary as a function of
## peoples' satisfaction with their lives (SWL)?
## DemImp = V140, DemRun = V141
DemRunmodel2 <- lm(V141 ~ V140*V23 + V2, data = wvs_data)
summary(DemRunmodel2)

## 2.2.3 Within what range of SWL is the DemImp -> DemRun simple slope from Question 2 statistically significant?
library(rockchalk)
plotOut <- plotSlopes(model = DemRunmodel2, plotx = 'V140', modx = 'V23', plotPoints = FALSE)
plotOut
testOut <- testSlopes(plotOut)
testOut$jn$roots

### Question 2.3 Categorical variable moderation
## 2.3.1 After controlling for SWL, does the DemImp -> DemRun effect vary significantly by country?
## DemImp = V140, DemRun = V141
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '156')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
summary(DemRunCountryModel)

## 2.3.2 Visualize the results from Question 1 in a suitable way.
library(rockchalk)
DemRunCountryModel2 <- lm(V141 ~ V140*V2 + V23, data=wvs_data)
plotDemRunCountryModel <- plotSlopes(model=DemRunCountryModel2, plotx='V140', modx = 'V2', plotPoints=FALSE, 
                                     xlab = 'satisfaction with your life', ylab = 'How democratically is the country being governed')

## 2.3.3 For which country is the effect of DemImp on DemRun strongest, after controlling for SWL?
##Check PDf File
## 2.3.4 For which country is the effect of DemImp on DemRun weakest, after controlling for SWL?
## Repeat for each country and see which one has lowest/highest coefficient?
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '156')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
coef(DemRunCountryModel)["V140"]
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '276')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
coef(DemRunCountryModel)["V140"]
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '356')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
coef(DemRunCountryModel)["V140"]
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '643')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
coef(DemRunCountryModel)["V140"]
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '840')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
coef(DemRunCountryModel)["V140"]

## 2.3.5 Are the simple slopes referenced in Questions 3 and 4 statistically significant?
## Test whether slopes are significantly non-zero 
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '356')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
summary(DemRunCountryModel)
wvs_data$cat<-factor(wvs_data$V2, ordered=FALSE)
wvs_data$cat <- relevel(wvs_data$cat, ref = '643')
DemRunCountryModel <- lm(V141 ~ V140*cat + V23, data = wvs_data)
summary(DemRunCountryModel)


### Question 2.4 Predictive modeling
## 2.4.1 Select and list three (theoretically justified) sets of predictors (or functions thereof, e.g., 
## interactions or polynomials) to use in predicting FinSat.
## Financial satisfaction = V59
## Important to be rich = V71
## Scale of income = V239
## Marital status = V57
ImpRichModel <- lm(V59 ~ V71, data = wvs_data)
ScaleIncModel <- lm(V59 ~ V239, data = wvs_data)
MarStatModel <- lm(V59 ~ V57, data = wvs_data)
coef(ImpRichModel)["V71"]
coef(ScaleIncModel)["V239"]
coef(MarStatModel)["V57"]
Combinedmodel <- lm(V59 ~ V71 + V239 + V57, data = wvs_data)
summary(Combinedmodel)

## 2.4.2 Briefly explain why you expect the three sets of predictors you chose in Question 1 to perform well. 
## That is, explain your rationale for defining these three sets.
## V71 (important to be rich): 
## V239 (scale of income):
## V57 (marital status):

#they are all three important predictors of the satisfaction with the financial situation of
#households, because all three sets have both seperately as combined strong effects

## 2.4.3 Use 10-fold cross-validation to compare the predictive performance of the three models defined in 
## question 1.
library(MLmetrics)
set.seed(1)
cv.lm <- function(data, models, names = NULL, K = 10) {
  ## Create a partition vector:
  part <- sample(rep(1 : K, ceiling(nrow(data) / K)))[1 : nrow(data)]
  
  ## Find the DV:
  dv <- trimws(strsplit(models[1], "~")[[1]][1])
  
  ## Apply over candidate models:
  cve <- sapply(X = models, FUN = function(model, data, dv, K, part) {
    ## Loop over K repititions:
    mse <- c()
    for(k in 1 : K) {
      ## Partition data:
      train <- data[part != k, ]
      valid <- data[part == k, ]
      
      ## Fit model, generate predictions, and save the MSE:
      fit    <- lm(model, data = train)
      pred   <- predict(fit, newdata = valid)
      mse[k] <- MSE(y_pred = pred, y_true = valid[ , dv])
    }
    ## Return the CVE:
    sum((table(part) / length(part)) * mse)
  },
  data = data,
  K    = K,
  dv   = dv,
  part = part)
  
  ## Name output, if applicable:
  if(!is.null(names)) names(cve) <- names
  cve
}

cve <- cv.lm(data   = wvs_data,
             models = c("V59~V71", "V59~V239", "V59~V57"),
             names  = c("ImpRich", "ScaleInc", "MarStat"),
             K      = 10)

cve

## 2.4.4 Which of the three models compared in Question 3 performed best?
which.min(cve)

## 2.4.5 What is the estimated prediction error of the best model?
MSE(y_pred = predict(ScaleIncModel, data = wvs_data), y_true = wvs_data$V59) 

## 2.4.6 Based on the selection you made in Question 4, what can you say about the attributes that are important
## for predicting financial satisfaction?

#The scale of incomes is the best predictor for estimating the satisfaction with the financial situation of households