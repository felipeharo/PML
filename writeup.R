download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile = "pml-training.csv")

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",destfile = "pml-testing.csv")

training<-read.csv("pml-training.csv",na.strings=c("","NA"))
testing<-read.csv("pml-testing.csv")

# If the variable has 1 NA value it will likely have more than 90% NA's, so for simplicity
# we leave only the variables that are complete
names<-as.character()
for(i in 1:dim(training)[2]){
  if(sum(is.na(training[names(training)[i]]))==0){
    names<-c(names,names(training)[i])
  }
}
subtraining<-training[names]

# cvtd_timestamp depends on raw_timestamp_part 1 and since the name of the user doesn't say anything and we will assume that all the users have the same characteristics, we remove those 2 variables
# we also remove all timestamps since it is a unique value that will never repeat itself in the future. We remove X because it,s just an ID of the measurement.
# Finally, new_window and num_window are beeing removed aswell.
subtraining<-subtraining[,-c(1,2,3,4,5,6,7)]


trans <- preProcess(subtraining[-53], method  = "pca",thresh=0.9)
trainPC<-predict(trans,subtraining[-53])
modfit<-randomForest(trainPC,training$classe)


names2<-as.character()
for(i in 1:dim(testing)[2]){
  if(sum(is.na(testing[names(testing)[i]]))==0){
    names2<-c(names2,names(testing)[i])
  }
}


subtest<-testing[names2]
subtest<-subtest[,-c(1,2,3,4,5,6,7,60)]
testPC<-predict(trans,subtest)
a<-predict(modfit,testPC)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# names(testing)[160]<-"classe"
# subtest<-testing[names2]
# subtest<-subtest[,-c(1,2,3,4,5,6,7)]




#modelfit<-train(subtraining$classe~.,method="rf",data=trainPC)












#The goal of your project is to predict the manner in which they did the exercise
#outcome: class
# You may use any of the other variables to predict with
# . You should create a report describing how you built your model,
# how you used cross validation
# what you think the expected out of sample error is,
# WHy you made the choices you did

#You will also use your prediction model to predict 20 different test cases. 

# Does the submission build a machine learning algorithm to predict activity quality from activity monitors?
# Do the authors describe what they expect the out of sample error to be and estimate the error appropriately with cross-validation?

