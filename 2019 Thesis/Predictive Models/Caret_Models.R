###Caret Models 
###Predictions on Cluster outcomes
library(pacman)
p_load(tidyverse, caret)


Raw_Clustered <- read_rds("ClusterResults.rds") %>% dplyr::select(CensusTractID = ID, ClusterCode = km_out1.cluster)

rawdf00 <- readRDS("Geolytics00.rds") %>% mutate(CensusTractID_ = as.character(CensusTractID)) %>% dplyr::select(-HomeVal00, -RentVal00, -HH_income00, -PctCollege00, -PctMulti00, -nonWhite00, -CensusTractID)
rawdf90 <- readRDS("Geolytics90.rds") %>% mutate(CensusTractID_ = as.character(CensusTractID)) %>% dplyr::select(-CensusTractID)
Spatial <- read_rds("SpatialVars00.rds") %>% dplyr::select(-Park_PctCoverage16)
Evictions <- readRDS("Evictions01.rds") 


##Final table w 1990 and 2000 data
joined00 <- inner_join(Raw_Clustered, rawdf00, by= c("CensusTractID" = "CensusTractID_")) %>%
  inner_join(., rawdf90, by= c("CensusTractID" = "CensusTractID_")) %>%
  inner_join(., Spatial, by = "CensusTractID") %>%
  inner_join(., Evictions, by = "CensusTractID") %>% mutate(BooleanGent = ifelse(ClusterCode == 4, 1, 0)) %>% dplyr::select(-ClusterCode, -year) 

Dep00 <- joined00 %>% mutate(BooleanGent1 = as.factor(BooleanGent)) %>% dplyr::select(CensusTractID, BooleanGent= BooleanGent1)

PCA2000 <- joined00 %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
PCA00named <- joined00 %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

prin_comp00 <- prcomp(PCA2000, scale. = T)
PCAval00 <- prin_comp00$x %>% cbind(PCA00named)


PCA00_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")





indexes <- createDataPartition(PCA00_results$BooleanGent, p = .5, list = FALSE) #maintains proportions of gent/nongent
train <- PCA00_results[indexes,]
test <- PCA00_results[-indexes,]



train.control <- trainControl(method = "LOOCV")
caret.cv <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 , data = train, method = "rf", trControl = train.control)


rawprobs <- predict(caret.cv, test, type = "prob") %>% cbind(test$BooleanGent)

thresholds <- SDMTools::optim.thresh(test$BooleanGent, rawprobs$`0`)

preds <- rawprobs %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(preds$FinalPred, test$BooleanGent, positive = "1")





###SVM
svm.control <- trainControl(method = "LOOCV", classProbs = T)
grid <- expand.grid(C = c(5))
train$BooleanGent1 <- as.factor(make.names(train$BooleanGent))
test$BooleanGent1 <- as.factor(make.names(test$BooleanGent))

svm_Linear <- train(BooleanGent1~ PC1 + PC2 + PC3 + PC4 + PC5, data = train, method = "svmRadial",
                    trControl=svm.control,  metric = "ROC")

newsvm <- svm(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5, data = train, type = "C-classification", kernel = "linear", scale = FALSE, cost = 150)

svm_pred <- predict(newsvm, newdata = test)
confusionMatrix(svm_pred, test$BooleanGent, mode = "sens_spec", positive = "1")


blacc = nrow(PCA00_results[PCA00_results$BooleanGent == 0, ])/nrow(PCA00_results)*100
cat('Baseline accuracy:', blacc)

n = ncol(PCA00_results)

m1 = multinom(data = train, BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5)
p1 = predict(m1, test, type = 'class')
confusionMatrix(p1, test$BooleanGent, positive = "1")


gr = expand.grid(C = c(1,2,5, 10, 50, 150), sigma = c(0.01, 0.05,.25, .5,.75, 1))
tr = trainControl(method = 'cv', number = 10, savePredictions = T, summaryFunction = twoClassSummary, classProbs = T)

m2 = train(data = train, BooleanGent1~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, method = 'svmRadial', trControl = tr, tuneGrid = gr, metric = "Spec")
m2$results


p2 = predict(m2, test)
confusionMatrix(p2, test$BooleanGent1, positive = "X1")



##SVM again
tune.out <- tune(svm, BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5, data = train, kernel="radial", 
                 ranges = list(gamma=c(0.1,0.5,1,2,4), 
                               cost = c(0.1,1,10,100,1000)
                 ), 
                 class.weights= c("0" = 1, "1" = 10))

obj <- tune(svm, Species~., data = iris, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "fix")
)

###########More Predictive variable
Pred_vars <- readRDS("UpdatedPredVars.rds")
Pred_vars$ID <- as.character(Pred_vars$CensusTractID)

##Final table w 1990 and 2000 data
Full <- inner_join(Raw_Clustered, Pred_vars, by= c("CensusTractID" = "ID")) %>%
  inner_join(., Spatial, by = "CensusTractID") %>%
  inner_join(., Evictions, by = "CensusTractID") %>% mutate(BooleanGent = ifelse(ClusterCode == 4, 1, 0)) %>% dplyr::select(-ClusterCode, -year, - CensusTractID.y) 

Dep00 <- Full %>% mutate(BooleanGent1 = as.factor(BooleanGent)) %>% dplyr::select(CensusTractID, BooleanGent= BooleanGent1)

Full00x <- Full %>%dplyr::select(CensusTractID, contains("00"), Dist2Downtown, Transit_PctCoverage, EvictionRate, Park_PctCoverage10, BooleanGent, -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00)

PCA2000 <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
PCA00named <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

prin_comp00 <- prcomp(PCA2000, scale. = T)
PCAval00 <- prin_comp00$x %>% cbind(PCA00named)

biplot <-fviz_pca_biplot(prin_comp00, label = "var")
get_eig(prin_comp00)
fviz_eig(prin_comp00)

PCA00_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")


indexes <- createDataPartition(PCA00_results$BooleanGent, p = .5, list = FALSE) #maintains proportions of gent/nongent
train <- PCA00_results[indexes,]
test <- PCA00_results[-indexes,]

train.control <- trainControl(method = "LOOCV")
caret.cv3 <- train(BooleanGent~ PC1 + PC2 + PC3, data = train, method = "rf", trControl = train.control)
caret.cv4 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 , data = train, method = "rf", trControl = train.control)
caret.cv5 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 , data = train, method = "rf", trControl = train.control)
caret.cv6 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 , data = train, method = "rf", trControl = train.control)

rawprobs3 <- predict(caret.cv3, test, type = "prob") %>% cbind(test$BooleanGent)
rawprobs4 <- predict(caret.cv4, test, type = "prob") %>% cbind(test$BooleanGent)
rawprobs5 <- predict(caret.cv5, test, type = "prob") %>% cbind(test$BooleanGent)
rawprobs6 <- predict(caret.cv6, test, type = "prob") %>% cbind(test$BooleanGent)

thresholds3 <- SDMTools::optim.thresh(test$BooleanGent, rawprobs3$`0`)
thresholds4 <- SDMTools::optim.thresh(test$BooleanGent, rawprobs4$`0`)
thresholds5 <- SDMTools::optim.thresh(test$BooleanGent, rawprobs5$`0`)
thresholds6 <- SDMTools::optim.thresh(test$BooleanGent, rawprobs6$`0`)

preds3 <- rawprobs3 %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds3$`sensitivity=specificity`[1], 1, 0)))
preds4 <- rawprobs4 %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds4$`sensitivity=specificity`[1], 1, 0)))
preds5 <- rawprobs5 %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds5$`sensitivity=specificity`[1], 1, 0)))
preds6 <- rawprobs6 %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds6$`sensitivity=specificity`[1], 1, 0)))

cm3 <- confusionMatrix(preds3$FinalPred, test$BooleanGent, positive = "1")
cm4 <- confusionMatrix(preds4$FinalPred, test$BooleanGent, positive = "1")
cm5 <- confusionMatrix(preds5$FinalPred, test$BooleanGent, positive = "1")
cm6 <- confusionMatrix(preds6$FinalPred, test$BooleanGent, positive = "1")

NoLag_PC_stats <- data.frame(cm3$overall) %>% cbind(cm4$overall, cm5$overall, cm6$overall)
NoLag_PC_sens <- data.frame(cm3$byClass) %>% cbind(cm4$byClass, cm5$byClass, cm6$byClass)

