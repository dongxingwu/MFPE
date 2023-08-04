#这个包是xgboost
library(xgboost)
library(Matrix)
library(caret)
library(SHAPforxgboost)
##构造方程
xgboosti <- function(
data_train,#这个是训练数据集,
data_predict #这个是预测数据集
##这里可以训练和预测是一个数据集，例如RF(a,a)，a是读取的数据
#训练和预测的数据格式是：自变量name编写为x1,x2,..，因变量name编写为Y
){
data.all <- data_train
set.seed(9)
#根据因变量设置10折交叉
folds <- createFolds(y=data.all$Y,k=10)
fold_accuracy <- array()
##循环计算每折的准确率
for(i in 1:10){    
fold_test <- data.all[folds[[i]],]   #取folds[[i]]作为测试集,十分之一  
fold_train <- data.all[-folds[[i]],]   # 剩下的数据作为训练集，十分之九    
print("***组号***")
##开始构造xgboost需要的数据格式
##将自变量转换为矩阵
traindata1 <- data.matrix(fold_train[,c(1:(length(fold_train[1,])-1))]) 
#转换为稀疏矩阵
#traindata2 <- Matrix(traindata1,sparse=T)
##因变量
traindata3 <- fold_train[,length(fold_train[1,])]
##自变量和因变量拼接为list
traindata4 <- list(data=traindata1,label=traindata3)
##构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵
dtrain <- xgb.DMatrix(data=traindata4$data,label=traindata4$label)
##按照上面的步骤对测试数据集处理
testset1 <- data.matrix(fold_test[,c(1:(length(fold_test[1,])-1))]) 
#testset2 <- Matrix(testset1,sparse=T) 
testset3 <- fold_test[,length(fold_test[1,])]
testset4 <- list(data=testset1,label=testset3) 
dtest <- xgb.DMatrix(data=testset4$data,label=testset4$label)
##开始xgboost
##参数含义silent：默认值是0，需要指定0连续打印消息,静默模式1。
##booster：默认值是gbtree，需要指定要使用的上升模型:gbtree(树)或gblinear(线性函数)
##eta：默认值设置为0.3，需要指定用于更新步长收缩来防止过度拟合。每个提升步骤后,我们可以直接获得新特性的权重。
##实际上 eta 收缩特征权重的提高过程更为保守。范围是0到1。低η值意味着模型过度拟合更强。
##max_depth：默认值设置为6，需要指定一个树的最大深度。参数范围是1到无穷大
##objective：如果是回归就不用，如果是分类选择’binary:logistic’
##nround：迭代次数
xgb <- xgboost(data=dtrain,max.depth=6,eta=0.5,nround=25)
#xgb_train <- xgb.train(data=dtrain,max.depth=6,eta=0.5,nround=25)
pred_y <- predict(xgb,testset4$data)
##检测准确性
test_y <- testset3
# mse = mean((test_y - pred_y)^2)
# mae = caret::MAE(test_y, pred_y)
#rmse = caret::RMSE(test_y, pred_y)
#R2 =  caret::R2(test_y, pred_y)
#roc = roc(test_y, pred_y)
fold_error = test_y-pred_y  
#精确度--判断正确的数量占总数的比例
fold_accuracy[i] = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test)   
#fold_accuracy[i] <- caret::RMSE(test_y, pred_y)
#fold_accuracy[i] = median(abs(fold_error))/sd(fold_test$Y)
print(i)  
print("***测试集精确度***") 
print(fold_accuracy)  
}
num <- which.max(fold_accuracy)
#fold_accuracy_s <- max(fold_accuracy)
#精度最高样本
#测试
testi <- data.all[folds[[num]],]
#训练
traini <- data.all[-folds[[num]],]
#利用精度最高样本重新构建模型
trainset_new1 <- data.matrix(traini[,c(1:(length(traini[1,])-1))]) 
#testset2 <- Matrix(testset1,sparse=T) 
trainset_new3 <- traini[,length(traini[1,])]
trainset_new4 <- list(data=trainset_new1,label=trainset_new3) 
train_new <- xgb.DMatrix(data=trainset_new4$data,label=trainset_new4$label)
#全部训练数据
data_train_new1 <- data.matrix(data_train[,c(1:(length(data_train[1,])-1))]) 
#testset2 <- Matrix(testset1,sparse=T) 
data_train_new3 <- data_train[,length(data_train[1,])]
data_train_new4 <- list(data=data_train_new1,label=data_train_new3) 
data_train_new <- xgb.DMatrix(data=data_train_new4$data,label=data_train_new4$label)
#预测数据
data_predict_new1 <- data.matrix(data_predict[,c(1:(length(data_predict[1,])-1))]) 
#testset2 <- Matrix(testset1,sparse=T) 
data_predict_new3 <- data_predict[,length(data_predict[1,])]
data_predict_new4 <- list(data=data_predict_new1,label=data_predict_new3) 
#data_predict_new <- xgb.DMatrix(data=data_predict_new4$data,label=data_predict_new4$label)
data_predict_new <- xgb.DMatrix(data=data_predict_new4$data,label=rep(1,length(data_predict_new3)))
##计算最佳模型
xgb_new <- xgboost(data=train_new,max.depth=6,eta=0.5,nround=25)
#xgb_train <- xgb.train(data=train_new,max.depth=6,eta=0.5,nround=25)
#importance_matrix <- xgb.importance(model=xgb_new)
####计算shap值
##全局特征重要性
##柱状图a，表示每个变量的重要性
shap_values <- shap.values(xgb_model = xgb_new, X_train = trainset_new4$data)
mean_shap_score <- shap_values$mean_shap_score
##散点图b，表示每个变量的重要性
shap_long <- shap.prep(xgb_model = xgb_new, X_train = trainset_new4$data)
#shap.plot.summary(shap_long)
##单一变量的SHAP主效应
#shap.plot.dependence(data_long = shap_long, x="GPP",add_hist = TRUE, add_stat_cor = TRUE)
##交互作用
data_int <- shap.prep.interaction(xgb_mod = xgb_new,X_train = trainset_new4$data)
shap_int <- predict(xgb_new, trainset_new4$data,predinteraction = TRUE)
#shap.plot.dependence(data_long = shap_long,
						#data_int = shap_int,
							#x="GPP",
								#y = "TA",
									#color_feature = "TA")
##预测全部训练集样本
train_new_y <- predict(xgb_new,data_train_new)
data_train$predict <- train_new_y
data_train_score_R2 <- R2(data_train_new3, train_new_y)
data_train_score_RMSE <- RMSE(data_train_new3, train_new_y)
data_train_score_MAE <- MAE(data_train_new3, train_new_y)
#data_error <- data_train$Y-train_new_y
#data_accuracy = (nrow(data_train)-sum(abs(data_error)))/nrow(data_train)
##预测全部预测集样本
pred_new_y <- predict(xgb_new,data_predict_new)
data_predict$predict <- pred_new_y
data_predict_score_R2 <- R2(data_predict_new3, pred_new_y)
data_predict_score_RMSE <- RMSE(data_predict_new3, pred_new_y)
data_predict_score_MAE <- MAE(data_predict_new3, pred_new_y)
##收集
output<-list()
output$data_train <- data_train
output$data_predict <- data_predict
output$train_new_y <- train_new_y
output$pred_new_y <- pred_new_y
output$train_new <- trainset_new4
output$train_score$R2 <- R2(data_train_new3, train_new_y)
output$train_score$RMSE <- RMSE(data_train_new3, train_new_y)
output$train_score$MAE <- MAE(data_train_new3, train_new_y)
output$predict_score$R2 <- R2(data_predict_new3, pred_new_y)
output$predict_score$RMSE <- RMSE(data_predict_new3, pred_new_y)
output$predict_score$MAE <- MAE(data_predict_new3, pred_new_y)
output$xgb_new <- xgb_new
output$shap_values <- shap_values
output$mean_shap_score <- mean_shap_score
output$shap_long <- shap_long
output$data_int <- data_int
output$shap_int <- shap_int
#输出训练、预测值
#write.csv(data_train,"data_train.csv")
#write.csv(data_predict,"data_predict.csv")
return(output)
}
