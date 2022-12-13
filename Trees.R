library(tidymodels)
library(skimr)
library(stringr)
library(vip) # for variable importance
library(probably) # for balancing performance
library(smotefamily)

hcv_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv"
df <- read.csv(url(hcv_url))

skim(df)

#drop column X since its just a record index
df <- subset(df, select = -c(X))

#convert character to numeric 
df[] <- lapply(df, function(Category) as.numeric(as.factor(Category)))
df
hist(df$Category, col="maroon")

#check na values
sum(is.na(df))

#calculation median for all columns
all_column_median <- apply(df, 2, median, na.rm=TRUE)

# imputing median value with NA 
for(i in colnames(df))
  df[,i][is.na(df[,i])] <- all_column_median[i]

df
#check na values
sum(is.na(df))

#summary of df
skim(df)
summary(df$Category)

df$Category = as.factor(df$Category)

#train-test split
set.seed(1)
data_split <- initial_split(df, prop = 3/4, strata = Category)

df_train <- training(data_split)
df_test <- testing(data_split) 


nrow(df_train)
nrow(df_test)

library(caret)
#Decision Tree
train_control = trainControl(method = "cv", number = 5, search = "grid")

## Customsing the tuning grid (ridge regression has alpha = 0)
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))

# training a Regression model while tuning parameters (Method = "rpart")
model = train(Category~., data = df_train, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)

# summarising the results
print(model)

#use model to make predictions on test data
pred_y = predict(model, df_test)

# confusion Matrix
confusionMatrix(data = pred_y, df_test$Category)


#######################  Trees and pruning 
#simple tree model with its summary
model.tree <- tree(Category~., data=df_train)
summary(model.tree)

model.tree

#plot the tree
plot(model.tree)
text(model.tree)

model.pred = predict(model.tree, df_test, type = "class")
table(df_test$Category, model.pred)

#prune the tree to reduce overfitting
cv.model <- cv.tree(model.tree, FUN = prune.tree)
cv.model

#plot pruned tree and its summary
plot(cv.model$size, cv.model$dev, type = 'b', xlab = "Tree size", ylab = "Cross Validated Classification Error")

tree.pruned = prune.tree(model.tree, best = 5)
tree.pruned

summary(tree.pruned)

#comparison of pruned and unpruned tree
pred.unpruned <- predict(model.tree, df_test, type = "class")
misclass.unpruned <- sum(df_test$Category != pred.unpruned)/length(pred.unpruned)
misclass.unpruned

pred.pruned <- predict(tree.pruned, df_test, type = "class")
misclass.pruned <- sum(df_test$Category != pred.pruned)/length(pred.pruned)
misclass.pruned
























########Extra 
skim(df)

unique(df$Sex)
gender <- c(unique(df$Sex))
gender
temp.map <- c("m"=1, "f"=2)
sex <- temp.map[as.character(gender)]
sex

df_sex <- c(df$Sex)
df_sex
df$Sex= ifelse(df_sex=='m', 1,2)
df

unique(df$Category)

df_Category <- c(df$Category)
df_Category
df$Category=ifelse('0=Blood Donor', 1,ifelse('0s=suspect Blood Donor',2, ifelse('1=Hepatitis',3,ifelse('2=Fibrosis',4,5))))
unique(df$Category)

#tree model using rpart
library(party)
library(rpart)
library(rpart.plot)
model<- ctree(Category ~ ., df_train)
plot(model)

rtree <- rpart(Category ~ ., df_train)
rpart.plot(rtree)

#tree model using tree library
library(ISLR)
library(tree)

dim(df_train)
dim(df_test)

model.tree <- tree(Category~., data=df_train)
summary(model.tree)

plot(model.tree)
text(model.tree)

model.pred = predict(model.tree, df_test, type = "class")
table(df_test$Category, model.pred)
