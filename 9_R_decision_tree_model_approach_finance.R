## Model Building- Model 2: Decision Tree

# Packages Required

library(rpart)
library(rattle)

#---------------------------------------------------------    

# Load the dataset

bank <- bank_data_DT[,-21]


# Let's split the data in training and test datasets. 

set.seed(100)
split_indices <- sample.split(bank$response, SplitRatio = 0.70)

train_dt <- bank[split_indices, ]

test_dt <- bank[!split_indices, ]

nrow(train_dt)/nrow(bank)

nrow(test_dt)/nrow(bank)

#---------------------------------------------------------    

# building a tree with arbitrary minsplit and cp
banktree_1 <-  rpart(response ~ ., data=train_dt, method= "class", 
                     control=rpart.control(minsplit=65, cp=0.001))

plot(banktree_1)

# This is clearly an overfitted tree
# Classic decision tree problem


# Increasing the minsplit two fold to 130 
banktree_2 <-  rpart(response ~ ., data=train_dt, method= "class",
                     control=rpart.control(minsplit=130, cp=0.001))

plot(banktree_2)

# This one is better, but still looks a little too complex
# install rpart.plot and load the library
library(rpart.plot)

fancyRpartPlot(banktree_2)

# Listing the variables by importance: Duration, poutcome, month are the top 3
banktree_2$variable.importance

# We can further simplify the tree by increasing minsplit
banktree_3 <-  rpart(response ~ ., data=train_dt, method= "class",
                     control=rpart.control(minsplit=400, cp=0.001))

banktree_3$variable.importance
fancyRpartPlot(banktree_3)

# banktree_3 looks like an acceptable model; lets increase minsplit a litte more
banktree_4 <- rpart(response ~ ., data=train_dt, method= "class",
                    control=rpart.control(minsplit=800, cp=0.001))

fancyRpartPlot(banktree_4)
banktree_4$variable.importance

#---------------------------------------------------------    
## Model Evaluation for banktree_3 and banktree_4
# using test data from now on
# banktree_3
banktree_3_pred <- predict(banktree_3, test_dt[, -20], type = "class")

banktree_3_pred <- ifelse(banktree_3_pred==1,"yes","no")
test_dt$response <-ifelse(test_dt$response==1,"yes","no")

confusionMatrix(banktree_3_pred, test_dt[, 20], positive = "yes")

# Accuracy is 91.07%, sensitivity is only 50.04%

# banktree_4
banktree_4_pred <- predict(banktree_4, test_dt[, -20], type = "class")
banktree_4_pred <- ifelse(banktree_4_pred==1,"yes","no")
confusionMatrix(banktree_4_pred, test_dt[, 20], positive = "yes")

# Sensitivity is again low here; we can improve the model quite a bit since logistic model has sensitivtiy around 75%
# Though we can choose banktree_4, we should rather build a random forest instead
# It will avoid ovetfitting 

#---------------------------------------------------------    


#---------------------------------------------------------  