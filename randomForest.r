library(tidymodels)
library(skimr)
library(stringr)
library(vip) # for variable importance
library(probably) # for balancing performance
library(smotefamily)
library(UBL)
library(ggplot2)
library(caret)
library(scutr)
library(randomForest)

hcv_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv"
df <- read.csv(url(hcv_url))

skim(df)

#drop column X since its just a record index
df <- subset(df, select = -c(X))

#convert character to numeric 
response_variable = df$Category
levels(response_variable) <- c('0', '1', '2', '3', '4')
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

df[["Category"]] <- encode_ordinal(df[["Category"]])
df[["Sex"]] <- encode_ordinal(df[["Sex"]])
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

df$Category<-as.character(df$Category)
#summary of df
skim(df)
summary(df$Category)

#Plot clsses
ggplot(data = df, aes(fill = Category)) +
  geom_bar(aes(x = Category))+
  ggtitle("Number of samples in each class", subtitle = "Original dataset")+
  xlab("")+
  ylab("Samples")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#SMOTE 
oversampled_data <- oversample_smote(
  df,  
  c(0, 1, 2, 3, 4), 
  'Category',         # Option for oversampling
  m <- 615              # Value of 0 creates 50:50 split
)

df = oversampled_data

ggplot(data = df, aes(fill = Category)) +
  geom_bar(aes(x = Category))+
  ggtitle("Number of samples in each class", subtitle = "Balanced dataset")+
  xlab("")+
  ylab("Samples")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(expand = c(0,0))+
  theme(legend.position = "none", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

#train-test split
set.seed(1)
data_split <- initial_split(df, prop = 3/4, strata = Category)

df_train <- training(data_split)
df_test <- testing(data_split) 


nrow(df_train)
nrow(df_test)

train_ctrl <- trainControl(method="cv", # type of resampling in this case Cross-Validated
                           number=3, # number of folds
                           search = "random", # we are performing a "random
)

model_rf <- train(Category ~ .,
                  data = df_train,
                  method = "rf", # this will use the randomForest::randomForest function
                  metric = "Accuracy", # which metric should be optimized for 
                  trControl = train_ctrl,
                  # options to be passed to randomForest
                  ntree = 50,
                  keep.forest=TRUE,
                  importance=TRUE) 

model_rf

randomForest::varImpPlot(model_rf$finalModel)

probs <- predict(model_rf, df_test, 'prob')
probs
class <- predict(model_rf, df_test, 'raw')
class

truth_num_fac <- factor(df_test$Category)
levels(truth_num_fac)
levels(truth_num_fac) <- levels(predictions2)
head(truth_num_fac)
confusionMatrix(class, truth_num_fac)

#############################################
#Alternate implementation
modfit.rf <- randomForest(as.factor(Category
                                    )~. , data=df_train)

# Predict the testing set with the trained model
predictions2 <- predict(modfit.rf, df_test, type = "class")

#if u get Error: `data` and `reference` should be factors with the same levels. in confusion matrux
#Its due to mismatch of levels of factor
truth_num_fac <- factor(df_test$Category)
levels(truth_num_fac)
levels(truth_num_fac) <- levels(predictions2)
head(truth_num_fac)
confusionMatrix(predictions2, truth_num_fac)

# Accuracy and other metrics
confusionMatrix(predictions2, df_test$Category)
table(predictions2)
table(df_test$Category)

