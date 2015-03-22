dataTraining = read.csv('pml-training.csv') 
str(dataTraining)
names(dataTraining)
dataTraining = data.frame(
dataTraining$roll_belt, 
dataTraining$pitch_belt, 
dataTraining$yaw_belt, 
dataTraining$gyros_belt_x,
dataTraining$gyros_belt_y, 
dataTraining$gyros_belt_z, 
dataTraining$accel_belt_x,
dataTraining$accel_belt_y, 
dataTraining$accel_belt_z,
dataTraining$magnet_belt_x,
dataTraining$magnet_belt_y, 
dataTraining$magnet_belt_z,
dataTraining$roll_dumbbell, 
dataTraining$pitch_dumbbell, 
dataTraining$yaw_dumbbell, 
dataTraining$gyros_dumbbell_x, 
dataTraining$gyros_dumbbell_y, 
dataTraining$gyros_dumbbell_z, 
dataTraining$accel_dumbbell_x, 
dataTraining$accel_dumbbell_y, 
dataTraining$accel_dumbbell_z, 
dataTraining$magnet_dumbbell_x, 
dataTraining$magnet_dumbbell_y, 
dataTraining$magnet_dumbbell_z, 
dataTraining$roll_arm, 
dataTraining$pitch_arm, 
dataTraining$yaw_arm, 
dataTraining$gyros_arm_x , 
dataTraining$gyros_arm_y, 
dataTraining$accel_arm_x, 
dataTraining$accel_arm_y, 
dataTraining$accel_arm_z, 
dataTraining$magnet_arm_x, 
dataTraining$magnet_arm_y, 
dataTraining$magnet_arm_z, 
dataTraining$roll_forearm, 
dataTraining$pitch_forearm, 
dataTraining$yaw_forearm,
dataTraining$accel_forearm_x,
dataTraining$accel_forearm_y,
dataTraining$accel_forearm_z,
dataTraining$magnet_forearm_x,
dataTraining$magnet_forearm_y,
dataTraining$magnet_forearm_z,
dataTraining$classe)

dataTraining$dataTraining.yaw_belt <- NULL 
dataTraining$dataTraining.accel_belt_z <- NULL 
dataTraining$dataTraining.accel_arm_y <- NULL 
dataTraining$dataTraining.accel_belt_z <- NULL 
dataTraining$dataTraining.accel_arm_y <- NULL 
dataTraining$dataTraining.pitch_belt <- NULL 
dataTraining$dataTraining.magnet_belt_x <- NULL 
dataTraining$dataTraining.accel_dumbbell_x <- NULL 
dataTraining$dataTraining.gyros_dumbbell_z <- NULL 

library(caret)
inBuild<- createDataPartition(y = dataTraining$dataTraining.classe, p=0.7, list = FALSE)
validation <- dataTraining[-inBuild,]
buildData<-dataTraining[inBuild,]
inTrain<-createDataPartition(y = buildData$dataTraining.classe, p=0.7, list = FALSE)
training<-buildData[inTrain,]
test<-buildData[-inTrain,]
mod1<-train(dataTraining.classe~.,method="rf",data=training)

predTrain<-predict(mod1, training)
predTest<-predict(mod1, test)
confusionMatrix(predTest, test$dataTraining.classe)

validationTest<-predict(mod1, validation)
confusionMatrix(validationTest, validation$dataTraining.classe)

gbmFit1 <- train(dataTraining.classe~., data = training, method = "gbm", verbose = FALSE)
predTrain<-predict(gbmFit1, training)
predTest<-predict(gbmFit1, test)
confusionMatrix(predTest, test$dataTraining.classe)

validationTest<-predict(gbmFit1, validation)
confusionMatrix(validationTest, validation$dataTraining.classe)

