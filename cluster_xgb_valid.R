setwd("D:/work/IVUS/scipart2")
library(openxlsx)
library(caret)
library(survival)
all<-read.xlsx("part2_0811.xlsx",sheet=1)

internal<-subset(all,Hospital=="2")


library(openxlsx)
library(glmnet)
library(pROC)
library(xgboost)
library(Matrix)
library(dplyr)

jlist             = c(135)
max_depth_        = c(3)
eta_              = c(0.05)
nround_           = c(4)
gamma_            = c(0)
min_child_weight_ = c(3)
subsample_        = c(0.7)
colsample_bytree_ = c(0.1)

dfTune = data.frame(i                = numeric(),
                    currsplit        = numeric(),
                    j                = numeric(),
                    max_depth        = numeric(),
                    eta              = numeric(),
                    nround           = numeric(),
                    gamma            = numeric(),
                    min_child_weight = numeric(),
                    subsample        = numeric(),
                    colsample        = numeric(),
                    train_auc              = numeric(),
                    test_auc              = numeric(),
                    ex_auc = numeric())


i = 1


  set.seed(779)
  trainIndex <- createDataPartition(internal$CustomLabel, p = .7,
                                    list = FALSE,
                                    times = 1)
  SELECTDATA_train_ori <- internal[ trainIndex,]
  Train_ID <- internal$PatientID[trainIndex]
  SELECTDATA_test_ori  <- internal[-trainIndex,]
  Test_ID <- internal$PatientID[-trainIndex]
  ex<-subset(all,Hospital=="1")
  radiomics_features<-SELECTDATA_train_ori[,134:1359]
  
  
  p<-NULL
  for (ii in 1:length(radiomics_features)){
    feature_model <- glm(CustomLabel~., family = "binomial",data = all %>% select(CustomLabel, colnames(radiomics_features)[ii]))
    if (is.na(feature_model$coefficients[2])){
      p[ii] <- 1
    } else{
      p[ii]<-summary(feature_model)$coefficients[2,4]
    }
  }
  
  pplot <- data.frame(Index = 1:length(p),p)
  pplot[,1]=colnames(radiomics_features)
  pplot <- pplot %>% mutate(Significance = p<0.05)
  library(stringr)
  library(ggplot2)
  pplot %>% mutate(Type = str_match(Index, "_\\s*(.*?)\\s*_")[,2]) -> pplot
  pplot %>% arrange(Type) -> pplot
  pplot %>% mutate(number = 1:dim(pplot)[1]) -> pplot
  # pplot<-pplot[order(pplot$Type,decreasing=T),]
  # ggplot(pplot, aes(number, -log10(p), color =Type)) + geom_point() + geom_hline(yintercept=-log10(0.05))+theme_classic()
  
  #with dendrogram
  library(readr)
  library("reshape2")
  library("pheatmap")
  Features2Exclude <- pplot %>% filter(p == 1) %>% select(Index)
  map1 <- radiomics_features
  map1 %>% select(-Features2Exclude$Index) -> map2
  mcor <- cor(map2)
  aa<-pheatmap(mcor,clustering_method = "ward.D2",show_colnames = F,show_rownames = F, cutree_cols = 8,cutree_rows = 8, silent = T)
  order_row = aa$tree_row$order 
  order_col = aa$tree_col$order    
  datat = data.frame(mcor[order_row,order_col])   
  datat = data.frame(rownames(datat),datat,check.names =F)  
  colnames(datat)[1] = "featurename" 
  x<-dist(mcor,method="euclidean")
  bb<-hclust(x,method = "ward.D2")
  cc<-as.data.frame(cutree(bb,k=8,h=8))
  colnames(cc) <- "Class"
  cc %>% mutate(Index = rownames(cc)) -> cc
  inner_join(pplot,cc) -> pplot1
  # pplot %>% mutate(Class = cc) -> pplot1
  pplot1 %>% group_by(Class) %>% summarise(minp = min(p)) -> xxx
  pplot1[which(pplot1$p %in% xxx$minp),] -> corrselect
  #corrselect <- corrselect[-which(duplicated(corrselect$Class)),]
  # write.csv(corrselect,"corrselect.csv")
  
  SELECTDATA_train <- SELECTDATA_train_ori %>% mutate(Labels = as.factor(CustomLabel)) %>% 
    select(Labels, corrselect$Index)
  SELECTDATA_train <- as.data.frame(SELECTDATA_train)
  SELECTDATA_test <- SELECTDATA_test_ori %>% mutate(Labels = as.factor(CustomLabel)) %>% 
    select(Labels, corrselect$Index)
  SELECTDATA_test <- as.data.frame(SELECTDATA_test)
  SELECTDATA_ex <- ex %>% mutate(Labels = as.factor(CustomLabel)) %>% 
    select(Labels, corrselect$Index)
  SELECTDATA_ex <- as.data.frame(SELECTDATA_ex)
  
  
  train <- SELECTDATA_train
  test <- SELECTDATA_test
  ex <- SELECTDATA_ex
  
  traindata1<-data.matrix(train[2:9])
  traindata2<-Matrix(traindata1,sparse=T)
  traindata3<-as.numeric(train[,1])-1
  traindata4<-list(data=traindata2,label=traindata3)
  dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 
  feature.names <- names(train)[2:9]
  cat("Feature Names\n")
  #feature.names
  
  testdata1<-data.matrix(test[2:9])
  testdata2<-Matrix(testdata1,sparse=T)
  testdata3<-as.numeric(test[,1])-1
  testdata4<-list(data=testdata2,label=testdata3)
  dtest <- xgb.DMatrix(data = testdata4$data, label = testdata4$label) 
  
  exdata1<-data.matrix(ex[2:9])
  exdata2<-Matrix(exdata1,sparse=T)
  exdata3<-as.numeric(ex[,1])-1
  exdata4<-list(data=exdata2,label=exdata3)
  dex <- xgb.DMatrix(data = exdata4$data, label = exdata4$label) 
  
  gc()
  watchlist<-list(val=dtest,train=dtrain,ex=dex)
  
  for (j in jlist){
    for (m in max_depth_){
      for (e in eta_){
        for (n in nround_){
          for (g in gamma_){
            for (mi in min_child_weight_){
              for (s in subsample_){
                for (c in colsample_bytree_){
                  set.seed(j)
                  param <- list(max.depth        = m,
                                eta              = e,
                                gamma            = g,
                                min_child_weight = mi,
                                subsample        = s,
                                colsample_bytree = c,                            
                                silent           = 1, 
                                objective        = "binary:logistic",
                                booster          = "gbtree",
                                eval_metric      = "auc",
                                print.every.n    = 1000)
                  
                  clf <- xgb.train(params            = param,  
                                   data              = dtrain, 
                                   nrounds           = n,      
                                   verbose           = 1,   
                                   # early.stop.round  = 10,
                                   watchlist         = watchlist, 
                                   maximize          = FALSE)
                  
                  
                  ## Generate predictions (probs) on holdout
                  predictions_train <- predict(clf, data.matrix(train[,feature.names]))
                  predictions_test <- predict(clf, data.matrix(test[,feature.names]))
                  predictions_ex <- predict(clf, data.matrix(ex[,feature.names]))
                  #predictions_mace <- predict(clf, data.matrix(mace[,feature.names]))
                  ## AUC
                  train_auc<-roc(train$Labels, as.numeric(predictions_train))$auc
                  test_auc<-roc(test$Labels, as.numeric(predictions_test))$auc
                  ex_auc<-roc(ex$Labels, as.numeric(predictions_ex))$auc
                  
                  
                  cat("iteration = ",i,": Max_Depth, Eta, NRound,Subsample, ColSample = ",m,e,n,s,c,
                      "train = ",train_auc,"test=",test_auc,"ex=",ex_auc,"\n")
                  
                  dfTune[i,] <- c(i,csplit,j,m,e,n, g,mi,s,c,train_auc,test_auc,ex_auc)
                  
                  i = i + 1              
                  
                  
                }
              }
            }
          }
        }
      }
    }
  }


train_result<-cbind(SELECTDATA_train_ori,predictions_train)
colnames(train_result)[length(train_result)] <- c("score")
train_result$group <- "train"
test_result<-cbind(cbind(SELECTDATA_test_ori,predictions_test))
colnames(test_result)[length(test_result)] <- c("score")
test_result$group <- "test"
ex<-subset(all,Hospital=="1")
ex_result<-cbind(ex,predictions_ex)
colnames(ex_result)[length(ex_result)] <- c("score")
ex_result$group <- "ex"

all_result<-rbind(train_result,test_result,ex_result)


mace<-read.xlsx("part2_0811.xlsx",sheet = 5)

predictions_mace<-predict(clf,data.matrix(mace[,feature.names]))

mace$score<-predictions_mace
mace_patient<-mace %>% group_by(PatientName,PatientID,CustomLabel,Interval_three) %>% summarise(ds_prediction = max(ds),hrp_prediction = max(as.numeric(HRP)),
                                                                                                pav=mean(pav), number=sum(as.numeric(score)))

model_mace<-coxph(Surv(Interval_three,CustomLabel)~scale(number),data = mace_patient, x=TRUE,y=TRUE)
summary(model_mace)

model_mace_multi_non_ds<-coxph(Surv(Interval_three,CustomLabel)~scale(number)+hrp_prediction+scale(pav),data = mace_patient%>%filter(ds_prediction ==0), x=TRUE,y=TRUE)
summary(model_mace_multi_non_ds)

importanceRaw <- xgb.importance(feature_names=colnames(dtrain), model = clf)

sheets = list("xgb_withscore" = all_result,"xgb_importance" = importanceRaw,"xgb_opt_param"=dfTune)
write.xlsx(sheets,"xgb_0820.xlsx")

library(RColorBrewer)
library(ggplot2)
library(momr)
importanceRaw[["Feature"]]=factor(importanceRaw[["Feature"]],levels=as.character(importanceRaw[["Feature"]]))
importanceRaw$Feature<- factor(importanceRaw$Feature,levels = c("wavelet.HHL_glszm_SizeZoneNonUniformity","exponential_gldm_SmallDependenceLowGrayLevelEmphasis",	
                                                                "wavelet.HLL_glcm_DifferenceAverage"),ordered = TRUE)

cols<-brewer.pal(4,"PuBuGn")
pal<-colorRampPalette(cols)
mycolors<-pal(nrow(importanceRaw))

p<-ggplot(importanceRaw,aes(x=Feature,y= Gain,fill =Feature))+
  geom_bar(stat="identity",width = 0.5)+
  coord_flip()+
  scale_fill_manual(values=mycolors)+
  xlab("")
p+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = "none")


#score for the second time
second<-read.xlsx("part2_0811.xlsx",sheet = 12)
second_prediction <- predict(clf, data.matrix(second[,feature.names]))
second$score<-second_prediction
write.csv(second,"second.csv")
