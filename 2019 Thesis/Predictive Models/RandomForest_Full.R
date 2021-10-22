###Final RF Models

library(pacman)
p_load(tidyverse, caret, spdep, rgdal, rgeos, factoextra, tigris, randomForest)

#####Read in Data
Raw_Clustered <- read_rds("ClusterResults.rds") %>% dplyr::select(CensusTractID = ID, ClusterCode = km_out1.cluster)

Spatial <- read_rds("SpatialVars00.rds") %>% dplyr::select(-Park_PctCoverage16)
Evictions <- readRDS("Evictions01.rds") 

Pred_vars <- readRDS("UpdatedPredVars.rds")
Pred_vars$ID <- as.character(Pred_vars$CensusTractID)

Full <- inner_join(Raw_Clustered, Pred_vars, by= c("CensusTractID" = "ID")) %>%
  inner_join(., Spatial, by = "CensusTractID") %>%
  inner_join(., Evictions, by = "CensusTractID") %>% mutate(BooleanGent = ifelse(ClusterCode == 4, 1, 0)) %>% dplyr::select(-ClusterCode, -year, - CensusTractID.y) 

Dep00 <- Full %>% mutate(BooleanGent1 = as.factor(BooleanGent)) %>% dplyr::select(CensusTractID, BooleanGent= BooleanGent1) %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1")))


Full00x <- Full %>%dplyr::select(CensusTractID, contains("00"), Dist2Downtown, Transit_PctCoverage, EvictionRate, Park_PctCoverage10, BooleanGent, -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00)

#Create Spatial Lags
myshp <- readOGR("C:\\School\\Research\\census\\Davidson_Tracts_utm.shp")
myshp@data$JOINID = as.character(myshp@data$GEOID)
sp_jn1 <- geo_join(spatial_data = myshp, data_frame = Full00x, by_sp = "JOINID", by_df = "CensusTractID", how = "inner")

sp_jnx <- geo_join(spatial_data = myshp, data_frame = Full00x, by_sp = "JOINID", by_df = "CensusTractID", how = "inner")
tolag <- sp_jnx@data[,16:35]
queen.nb = poly2nb(sp_jnx, queen = FALSE)
queen.listw = nb2listw(queen.nb)
listw1 <- queen.listw

lag1 <- create_WX(tolag, listw1, prefix= "lag")
lagged_only <- Dep00 %>% cbind(lag1)


FullPCAraw <- Full00x  %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-BooleanGent)

prePCA_data <- FullPCAraw %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BoolGentF) 

prePCA_names <- FullPCAraw %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

PCA <- prcomp(prePCA_data, scale. = T)
postPCA <- PCA$x %>% cbind(prePCA_names)

fviz_pca_biplot(PCA)
PCA_results <- postPCA %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")

trainingindexes <- createDataPartition(PCA_results$BooleanGent, p = .6, list = FALSE) #maintains proportions of gent/nongent
train_pca <- PCA_results[trainingindexes,]
test_pca <- PCA_results[-trainingindexes,]

train.control <- trainControl(method = "LOOCV", classProbs = TRUE, search = "random", summaryFunction = twoClassSummary)
#tunegrid = expand.grid()
model_rf_pca <- train(BoolGentF~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = train_pca, method = "rf", trControl = train.control, tuneLength=10, metric = "ROC")

print(model_rf_pca)
plot(model_rf_pca)

rawprobs <- predict(model_rf_pca, test_pca, type = "prob") %>% cbind(test_pca$BooleanGent)

rawprobs$BOOLGENT01 <- as.factor(ifelse(test_pca$BoolGentF == "X0", 0, 1))
test_pca$BOOLGENT01 <- as.factor(ifelse(test_pca$BoolGentF == "X0", 0, 1))

thresholds <- SDMTools::optim.thresh(test_pca$BOOLGENT01, rawprobs$X0)

preds <- rawprobs %>% mutate(FinalPred = as.factor(ifelse(X0 <= sp_thresholds$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(preds$FinalPred, test_pca$BOOLGENT01, positive = "1")

#noPCA

NoPCAraw <- Full00x  %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-BooleanGent, -CensusTractID)

noPCAtrainingindexes <- createDataPartition(NoPCAraw$BoolGentF, p = .6, list = FALSE) #maintains proportions of gent/nongent
train_noPCA <- NoPCAraw[noPCAtrainingindexes,]
test_noPCA <- NoPCAraw[-noPCAtrainingindexes,]

train.control <- trainControl(method = "LOOCV", classProbs = TRUE, summaryFunction = twoClassSummary)
tunegrid = expand.grid(.mtry = (5:15))

model_rf_Nopca <- train(BoolGentF~ ., data = train_noPCA, method = "rf", trControl = train.control, tuneGrid=tunegrid, metric = "ROC")

print(model_rf_Nopca)
plot(model_rf_Nopca)

rawprobs_noPCA <- predict(model_rf_Nopca, test_noPCA, type = "prob") %>% cbind(test_noPCA$BoolGentF)

rawprobs_noPCA$BOOLGENT01 <- as.factor(ifelse(test_noPCA$BoolGentF == "X0", 0, 1))
test_noPCA$BOOLGENT01 <- as.factor(ifelse(test_noPCA$BoolGentF == "X0", 0, 1))

thresholds_noPCA <- SDMTools::optim.thresh(test_noPCA$BOOLGENT01, rawprobs_noPCA$X0)

preds_noPCA <- rawprobs_noPCA %>% mutate(FinalPred = as.factor(ifelse(X0 <= thresholds_noPCA$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(preds_noPCA$FinalPred, test_noPCA$BOOLGENT01, positive = "1")




##########FULL
Full_rf_data <- Full %>% cbind(lag1) %>% mutate(gentf = as.factor(as.character(BooleanGent))) %>% dplyr::select(-CensusTractID, -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00, -lag.Dist2Downtown) %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-gentf, -BooleanGent)

trainingindexes <- createDataPartition(Full_rf_data$BoolGentF, p = .6, list = FALSE) #maintains proportions of gent/nongent
train <- Full_rf_data[trainingindexes,]
test <- Full_rf_data[-trainingindexes,]


train.control <- trainControl(method = "LOOCV", classProbs = TRUE)
tunegrid <- expand.grid(.mtry = (20:40))


fullmodel<- train(BoolGentF~., data = train, method = "rf", trControl = train.control, tuneGrid = tunegrid, metric = "Accuracy")

print(fullmodel)
plot(fullmodel)

full_rawprobs <- predict(fullmodel, test, type = "prob") %>% cbind(test$BoolGentF)

full_rawprobs$BOOLGENT01 <- as.factor(ifelse(test$BoolGentF == "X0", 0, 1))
test$BOOLGENT01 <- as.factor(ifelse(test$BoolGentF == "X0", 0, 1))


full_thresholds <- SDMTools::optim.thresh(test$BOOLGENT01, full_rawprobs$X0)

full_preds <- full_rawprobs %>% mutate(FinalPred = as.factor(ifelse(`X0` <= full_thresholds$`sensitivity=specificity`[1], 1, 0)))

cmfull<- confusionMatrix(full_preds$FinalPred, test$BOOLGENT01, positive = "1")

varImp(fullmodel)


###No templagged Data
No90Data <- Full %>% cbind(lag1) %>% mutate(gentf = as.factor(as.character(BooleanGent))) %>% dplyr::select(-CensusTractID, -BooleanGent, -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00, -contains("90"), -lag.Dist2Downtown) 

trainingindexes <- createDataPartition(No90Data$gentf, p = .6, list = FALSE) #maintains proportions of gent/nongent
train_no90 <- No90Data[trainingindexes,]
test_no90 <- No90Data[-trainingindexes,]


train.control <- trainControl(method = "LOOCV")
fullmodel_no90<- train(gentf~., data = train_no90, method = "rf", ntree= 1000, trControl = train.control)

full_rawprobs_no90 <- predict(fullmodel_no90, test_no90, type = "prob") %>% cbind(test_no90$gentf)

full_thresholds_no90 <- SDMTools::optim.thresh(test_no90$gentf, full_rawprobs_no90$`0`)

full_preds_no90 <- full_rawprobs_no90 %>% mutate(FinalPred = as.factor(ifelse(`0` <= full_thresholds_no90$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(full_preds_no90$FinalPred, test_no90$gentf, positive = "0")

varImp(fullmodel_no90)

###No splag or 1990 data
No90lagData <- Full %>% cbind(lag1) %>% mutate(gentf = as.factor(as.character(BooleanGent))) %>% dplyr::select(-CensusTractID, -BooleanGent, -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00, -contains("90"), -contains("lag.")) %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-BooleanGent, -gentf)

trainingindexes <- createDataPartition(No90lagData$gentf, p = .6, list = FALSE) #maintains proportions of gent/nongent
train_no90lag <- No90lagData[trainingindexes,]
test_no90lag <- No90lagData[-trainingindexes,]


train.control <- trainControl(method = "LOOCV", classProbs = TRUE)
tunegrid <- expand.grid(.mtry = (1:15))
fullmodel_no90lag<- train(gentf~., data = train_no90lag, method = "rf", tuneGrid = tunegrid, trControl = train.control)

full_rawprobs_no90lag <- predict(fullmodel_no90lag, test_no90lag, type = "prob") %>% cbind(test_no90lag$gentf)

full_thresholds_no90lag <- SDMTools::optim.thresh(test_no90lag$gentf, full_rawprobs_no90lag$`0`)

full_preds_no90lag <- full_rawprobs_no90lag %>% mutate(FinalPred = as.factor(ifelse(`0` <= full_thresholds_no90lag$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(full_preds_no90lag$FinalPred, test_no90lag$gentf, positive = "0")

varImp(fullmodel_no90lag)


###SP Lag00 only
LagOnlyData <- Full %>% cbind(lag1) %>% mutate(gentf = as.factor(as.character(BooleanGent))) %>% dplyr::select(contains("lag."), BooleanGent, -lag.Dist2Downtown, Dist2Downtown) %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-BooleanGent)

trainingindexes <- createDataPartition(LagOnlyData$BoolGentF, p = .6, list = FALSE) #maintains proportions of gent/nongent
train_lag <- LagOnlyData[trainingindexes,]
test_lag <- LagOnlyData[-trainingindexes,]


train.control <- trainControl(method = "LOOCV", classProbs = TRUE)
tunegrid <- expand.grid(.mtry = (10))


fullmodel_lag<- train(BoolGentF~., data = train_lag, method = "rf", trControl = train.control, tuneGrid = tunegrid, metric = "Accuracy")

fullmodel_lagfinal<- train(BoolGentF~., data = train_lag, method = "rf",tuneLength=10, trControl = train.control, metric = "ROC")

print(fullmodel_lag)
plot(fullmodel_lag)

full_rawprobs_lag <- predict(fullmodel_lag, test_lag, type = "prob") %>% cbind(test_lag$BoolGentF)

full_rawprobs_lag$BOOLGENT01 <- as.factor(ifelse(test_lag$BoolGentF == "X0", 0, 1))
test_lag$BOOLGENT01 <- as.factor(ifelse(test_lag$BoolGentF == "X0", 0, 1))

full_thresholds_lag <- SDMTools::optim.thresh(test_lag$BOOLGENT01, full_rawprobs_lag$X0)

full_preds_lag <- full_rawprobs_lag %>% mutate(FinalPred = as.factor(ifelse(`X0` <= full_thresholds_lag$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(full_preds_lag$FinalPred, test_lag$BOOLGENT01, positive = "1")

varImp(fullmodel_lag)



###2000 only
No90lagData <- Full %>% cbind(lag1) %>% dplyr::select(-CensusTractID,  -HomeVal00, -RentVal00, -HH_income00, -PctMulti00, -nonWhite00, -PctCollege00, -contains("90"), -contains("lag.")) %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1"))) %>% dplyr::select(-BooleanGent)


trainingindexes <- createDataPartition(No90lagData$BoolGentF, p = .6, list = FALSE) #maintains proportions of gent/nongent
train <- No90lagData[trainingindexes,]
test <- No90lagData[-trainingindexes,]


train.control <- trainControl(method = "LOOCV", classProbs = TRUE)
tunegrid <- expand.grid(.mtry = (1:15))


model00<- train(BoolGentF~., data = train, method = "rf", trControl = train.control, tuneGrid = tunegrid, metric = "Accuracy")

print(model00)
plot(model00)

model00_rawprobs <- predict(model00, test, type = "prob") %>% cbind(test$BoolGentF)

model00_rawprobs$BOOLGENT01 <- as.factor(ifelse(test$BoolGentF == "X0", 0, 1))
test$BOOLGENT01 <- as.factor(ifelse(test$BoolGentF == "X0", 0, 1))

model00_thresholds <- SDMTools::optim.thresh(test$BOOLGENT01, model00_rawprobs$X0)

model00_preds <- model00_rawprobs %>% mutate(FinalPred = as.factor(ifelse(`X0` <= model00_thresholds$`sensitivity=specificity`[1], 1, 0)))

cm2000 <- confusionMatrix(model00_preds$FinalPred, test$BOOLGENT01, positive = "1")

varImp(model00)



###Correlations
testcors <- cor(Full_rf_data %>% dplyr::select(contains("Family"), contains("Car")))
corrplot::corrplot(testcors)