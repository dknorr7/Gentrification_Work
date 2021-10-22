####Models using Spatial Lags

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

Dep00 <- Full %>% mutate(BooleanGent1 = as.factor(BooleanGent)) %>% dplyr::select(CensusTractID, BooleanGent= BooleanGent1)


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



#Seperate into named and unnamed for PCA
PCA2000 <- lagged_only %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
PCA00named <- lagged_only %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

prin_comp00 <- prcomp(PCA2000, scale. = T)
PCAval00 <- prin_comp00$x %>% cbind(PCA00named)

biplot <-fviz_pca_biplot(prin_comp00, label = "var")
get_eig(prin_comp00)
fviz_eig(prin_comp00)

PCA00_lg_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")

indexes <- createDataPartition(PCA00_lg_results$BooleanGent, p = .5, list = FALSE) #maintains proportions of gent/nongent
train <- PCA00_lg_results[indexes,]
test <- PCA00_lg_results[-indexes,]

train.control <- trainControl(method = "LOOCV")
caret.cv <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 , data = train, method = "rf", trControl = train.control)


rawprobs <- predict(caret.cv, test, type = "prob") %>% cbind(test$BooleanGent)

thresholds <- SDMTools::optim.thresh(test$BooleanGent, rawprobs$`0`)

preds <- rawprobs %>% mutate(FinalPred = as.factor(ifelse(`0` <= thresholds$`sensitivity=specificity`[1], 1, 0)))

confusionMatrix(preds$FinalPred, test$BooleanGent, positive = "1")




###Non sp-lagged and lagged Data
#Seperate into named and unnamed for PCA
PCA2000_tlag <- Full %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
PCA00named_tlag <- Full %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

prin_comp00 <- prcomp(PCA2000_tlag, scale. = T)
PCAval00 <- prin_comp00$x %>% cbind(PCA00named_tlag)

biplot <-fviz_pca_biplot(prin_comp00, label = "var")
get_eig(prin_comp00)
fviz_eig(prin_comp00)

PCA00_lg_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")

indexes <- createDataPartition(PCA00_lg_results$BooleanGent, p = .5, list = FALSE) 
#maintains proportions of gent/nongent

train <- PCA00_lg_results[indexes,]
test <- PCA00_lg_results[-indexes,]

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

LaggedOnly_PC_stats <- data.frame(cm3$overall) %>% cbind(cm4$overall, cm5$overall, cm6$overall)
LaggedOnly_PC_sens <- data.frame(cm3$byClass) %>% cbind(cm4$byClass, cm5$byClass, cm6$byClass)

#RF using temporal lag and no spatial lag

tlag_PCA2000 <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
tlag_PCA00named <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

tlag_prin_comp00 <- prcomp(tlag_PCA2000, scale. = T)
tlag_PCAval00 <- tlag_prin_comp00$x %>% cbind(tlag_PCA00named)

biplot <-fviz_pca_biplot(prin_comp00, label = "var")
get_eig(prin_comp00)
fviz_eig(prin_comp00)

PCA00_tlg_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID")

indexes <- createDataPartition(PCA00_tlg_results$BooleanGent, p = .5, list = FALSE) #maintains proportions of gent/nongent
tlag_train <- PCA00_tlg_results[indexes,]
tlag_test <- PCA00_tlg_results[-indexes,]

train.control <- trainControl(method = "LOOCV")
tlag_caret.cv3 <- train(BooleanGent~ PC1 + PC2 + PC3, data = tlag_train, method = "rf", trControl = train.control)
tlag_caret.cv4 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 , data = tlag_train, method = "rf", trControl = train.control)
tlag_caret.cv5 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 , data = tlag_train, method = "rf", trControl = train.control)
tlag_caret.cv6 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 , data = tlag_train, method = "rf", trControl = train.control)

tlag_rawprobs3 <- predict(tlag_caret.cv3, tlag_test, type = "prob") %>% cbind(tlag_test$BooleanGent)
tlag_rawprobs4 <- predict(tlag_caret.cv4, tlag_test, type = "prob") %>% cbind(tlag_test$BooleanGent)
tlag_rawprobs5 <- predict(tlag_caret.cv5, tlag_test, type = "prob") %>% cbind(tlag_test$BooleanGent)
tlag_rawprobs6 <- predict(tlag_caret.cv6, tlag_test, type = "prob") %>% cbind(tlag_test$BooleanGent)

tlag_thresholds3 <- SDMTools::optim.thresh(tlag_test$BooleanGent, tlag_rawprobs3$`0`)
tlag_thresholds4 <- SDMTools::optim.thresh(tlag_test$BooleanGent, tlag_rawprobs4$`0`)
tlag_thresholds5 <- SDMTools::optim.thresh(tlag_test$BooleanGent, tlag_rawprobs5$`0`)
tlag_thresholds6 <- SDMTools::optim.thresh(tlag_test$BooleanGent, tlag_rawprobs6$`0`)

tlag_preds3 <- rawprobs3 %>% mutate(FinalPred = as.factor(ifelse(`0` <= tlag_thresholds3$`sensitivity=specificity`[1], 1, 0)))
tlag_preds4 <- rawprobs4 %>% mutate(FinalPred = as.factor(ifelse(`0` <= tlag_thresholds4$`sensitivity=specificity`[1], 1, 0)))
tlag_preds5 <- rawprobs5 %>% mutate(FinalPred = as.factor(ifelse(`0` <= tlag_thresholds5$`sensitivity=specificity`[1], 1, 0)))
tlag_preds6 <- rawprobs6 %>% mutate(FinalPred = as.factor(ifelse(`0` <= tlag_thresholds6$`sensitivity=specificity`[1], 1, 0)))

cm3_tlag <- confusionMatrix(tlag_preds3$FinalPred, tlag_test$BooleanGent, positive = "1")
cm4_tlag <- confusionMatrix(tlag_preds4$FinalPred, tlag_test$BooleanGent, positive = "1")
cm5_tlag <- confusionMatrix(tlag_preds5$FinalPred, tlag_test$BooleanGent, positive = "1")
cm6_tlag <- confusionMatrix(tlag_preds6$FinalPred, tlag_test$BooleanGent, positive = "1")

Temp_Lagged_PC_stats <- data.frame(cm3_tlag$overall) %>% cbind(cm4_tlag$overall, cm5_tlag$overall, cm6_tlag$overall)

Temp_Lagged_PC_sens <- data.frame(cm3_tlag$byClass) %>% cbind(cm4_tlag$byClass, cm5_tlag$byClass, cm6_tlag$byClass)

#####
#Spatial and 2000 models

Sp2000 <- cbind(Full00x, lag1)

spx_PCA2000 <- Sp2000 %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
spx_PCA00named <- Sp2000 %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

spx_prin_comp00 <- prcomp(spx_PCA2000, scale. = T)
spx_PCAval00 <- spx_prin_comp00$x %>% cbind(spx_PCA00named)

biplot <-fviz_pca_biplot(spx_prin_comp00, label = "var")
get_eig(spx_prin_comp00)
fviz_eig(spx_prin_comp00)

PCA00_spx_results <- spx_PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10 , PC11, PC12) %>% inner_join(., Dep00, by = "CensusTractID")

spx_indexes <- createDataPartition(PCA00_spx_results$BooleanGent, p = .5, list = FALSE) #maintains proportions of gent/nongent
spx_train <- PCA00_spx_results[spx_indexes,]
spx_test <- PCA00_spx_results[-spx_indexes,]

train.control <- trainControl(method = "LOOCV")
spx_caret.cv3 <- train(BooleanGent~ PC1 + PC2 + PC3, data = spx_train, method = "rf", trControl = train.control)
spx_caret.cv4 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 , data = spx_train, method = "rf", trControl = train.control)
spx_caret.cv5 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 , data = spx_train, method = "rf", trControl = train.control)
spx_caret.cv6 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 , data = spx_train, method = "rf", trControl = train.control)
spx_caret.cv9 <- train(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 +PC10 , data = spx_train, method = "rf", trControl = train.control)

rf <- randomForest(BooleanGent~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 +PC10 + PC11 + PC12 , data = spx_train)

spx_rawprobs3 <- predict(spx_caret.cv3, spx_test, type = "prob") %>% cbind(spx_test$BooleanGent)
spx_rawprobs4 <- predict(spx_caret.cv4, spx_test, type = "prob") %>% cbind(spx_test$BooleanGent)
spx_rawprobs5 <- predict(spx_caret.cv5, spx_test, type = "prob") %>% cbind(spx_test$BooleanGent)
spx_rawprobs6 <- predict(spx_caret.cv6, spx_test, type = "prob") %>% cbind(spx_test$BooleanGent)
spx_rawprobs9 <- predict(spx_caret.cv9, spx_test, type = "prob") %>% cbind(spx_test$BooleanGent)

spx_thresholds3 <- SDMTools::optim.thresh(spx_test$BooleanGent, spx_rawprobs3$`0`)
spx_thresholds4 <- SDMTools::optim.thresh(spx_test$BooleanGent, spx_rawprobs4$`0`)
spx_thresholds5 <- SDMTools::optim.thresh(spx_test$BooleanGent, spx_rawprobs5$`0`)
spx_thresholds6 <- SDMTools::optim.thresh(spx_test$BooleanGent, spx_rawprobs6$`0`)
spx_thresholds9 <- SDMTools::optim.thresh(spx_test$BooleanGent, spx_rawprobs9$`0`)

spx_preds3 <- spx_rawprobs3 %>% mutate(FinalPred = as.factor(ifelse(`0` <= spx_thresholds3$`sensitivity=specificity`[1], 1, 0)))
spx_preds4 <- spx_rawprobs4 %>% mutate(FinalPred = as.factor(ifelse(`0` <= spx_thresholds4$`sensitivity=specificity`[1], 1, 0)))
spx_preds5 <- spx_rawprobs5 %>% mutate(FinalPred = as.factor(ifelse(`0` <= spx_thresholds5$`sensitivity=specificity`[1], 1, 0)))
spx_preds6 <- spx_rawprobs6 %>% mutate(FinalPred = as.factor(ifelse(`0` <= spx_thresholds6$`sensitivity=specificity`[1], 1, 0)))
spx_preds9 <- spx_rawprobs9 %>% mutate(FinalPred = as.factor(ifelse(`0` <= spx_thresholds9$`sensitivity=specificity`[1], 1, 0)))

cm3_spx <- confusionMatrix(spx_preds3$FinalPred, spx_test$BooleanGent, positive = "1")
cm4_spx <- confusionMatrix(spx_preds4$FinalPred, spx_test$BooleanGent, positive = "1")
cm5_spx <- confusionMatrix(spx_preds5$FinalPred, spx_test$BooleanGent, positive = "1")
cm6_spx <- confusionMatrix(spx_preds6$FinalPred, spx_test$BooleanGent, positive = "1")
cm9_spx <- confusionMatrix(spx_preds9$FinalPred, spx_test$BooleanGent, positive = "1")


spx_PC_stats <- data.frame(cm3_spx$overall) %>% cbind(cm4_spx$overall, cm5_spx$overall, cm6_spx$overall, cm9_spx$overall)

spx_PC_sens <- data.frame(cm3_spx$byClass) %>% cbind(cm4_spx$byClass, cm5_spx$byClass, cm6_spx$byClass, cm9_spx$byClass)


###xgboost on 2000 data

p_load(xgboost)

PCA2000 <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID, -BooleanGent) 
PCA00named <- Full00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID)

prin_comp00 <- prcomp(PCA2000, scale. = T)
PCAval00 <- prin_comp00$x %>% cbind(PCA00named)

PCA00_results <- PCAval00 %>% dplyr::select(CensusTractID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10) %>% inner_join(., Dep00, by = "CensusTractID") %>% mutate(BoolGentF = as.factor(ifelse(BooleanGent==0, "X0","X1")))


x00indexes <- createDataPartition(PCA00_results$BoolGentF, p = .6, list = FALSE) #maintains proportions of gent/nongent
train <- PCA00_results[x00indexes,]
test <- PCA00_results[-x00indexes,]



train.control_xgb <- trainControl(method = "CV",number=5, classProbs = TRUE, savePredictions = TRUE)

xg_params <- expand.grid(eta = .1, colsample_bytree = c(.3,.4,.5), max_depth = c(3,6), nrounds = 100, min_child_weight = .3, subsample = 1, gamma = 0)

modelxgboost <- train(BoolGentF~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9, data = train,method = "xgbTree", trControl = train.control_xgb, tuneGrid = xg_params, scale_pos_weight = .1 )

xgpreds <- predict(modelxgboost, test)
confusionMatrix(xgpreds, test$BoolGentF)