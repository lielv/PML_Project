require(caret)
trainingData<-read.csv("pml-training.csv")
testData<-read.csv("pml-testing.csv")

##find which columns of the training set have all NA
isna<-sapply(trainingData, function(x)sum(is.na(x))>0.3)
sum(isna)

col_class<-sapply(trainingData,function(x)(class(x[1])))
is_factor<-col_class=="factor"
is_factor['user_name']<-F
is_factor['classe']<-F


cols<-isna
cols[c(1,3:7)]<-TRUE #not relevant

inds<-cols==T|is_factor==T
NewData<-trainingData[trainingData$new_window=="no",!inds]

set.seed(1)
inTrain = createDataPartition(NewData$classe, p = 3/4)[[1]]
training<-NewData[ inTrain,]
cv<-NewData[ -inTrain,]


#yaw_pitch_roll
ypr<-grep("yaw|pitch|roll",names(training))

tra_ypr<-training[,ypr]
tra_ypr$classe<-training$classe

cv_ypr<-cv[,ypr]
cv_ypr$classe<-cv$classe

set.seed(1)
fit_rf<-train(classe~.,method="rf",data=tra_ypr)

##rf prediction
pred_train_rf<-predict(fit_rf,tra_ypr)
sum(pred_train_rf==tra_ypr$classe)/dim(tra_ypr)[1]
#[1] 1 - 100% prediction of the training

pred_cv_rf<-predict(fit_rf,cv_ypr)
sum(pred_cv_rf==cv_ypr$classe)/dim(cv_ypr)[1]

# [1] 0.9868805 - 98.7% prediction of the cv

##NOW fit over all training data to get better predication
all_tra<-NewData[,ypr]
all_tra$classe<-NewData$classe

fit_rf_for_test<-train(classe~.,method="rf",data=all_tra)
pred_test<-predict(fit_rf_for_test,testData)

pred_test
# [1] B A B A A E D B A A B C B A E E A B B B #The results

#creating the files
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0(".\\project\\problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(pred_test)
