library(ggplot2)
library(scutr)
library(smotefamily)
## Load Data into R 

hcv_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/00571/hcvdat0.csv"

hcv_data <- read.csv(url(hcv_url))


## EDA 
head(hcv_data)

## Target Distribution

barplot(table(hcv_data$Category), main="HCV Prediction", xlab="Distribution of Categories")


# Missing Values
rowSums(is.na(hcv_data))

colSums(is.na(hcv_data)) 

# SMOTE Example 
hcv_data$Category <- as.factor(hcv_data$Category)
levels(hcv_data$Category) <- c(unique(hcv_data$Category))
summary(hcv_data$Category)

options(scipen=10000) # remove scientific notation when viewing plot

ggplot(data = hcv_data, aes(fill = Category)) +
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

predictor_variables <- hcv_data[,-which(names(hcv_data) == "Category")] # Select everything except response
response_variable <- hcv_data$Category   # Only select response variable

levels(response_variable) <- c('0', '1', '2', '3', '4')
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

table(hcv_data[["Category"]], encode_ordinal(hcv_data[["Category"]]), useNA = "ifany")

hcv_data[["Category"]] <- encode_ordinal(hcv_data[["Category"]])
hcv_data[["Sex"]] <- encode_ordinal(hcv_data[["Sex"]])
hcv_data
hcv_data$Category_encoded
#Count Missing Values
sapply(hcv_data, function(x) sum(is.na(x)))
#Remove Missing Values 
hcv_data$ALB[is.na(hcv_data$ALB)]<- median(hcv_data$ALB,na.rm = TRUE)
hcv_data$ALP[is.na(hcv_data$ALP)]<- median(hcv_data$ALP,na.rm = TRUE)
hcv_data$ALT[is.na(hcv_data$ALT)]<- median(hcv_data$ALT,na.rm = TRUE)
hcv_data$CHOL[is.na(hcv_data$CHOL)]<- median(hcv_data$CHOL,na.rm = TRUE)
hcv_data$PROT[is.na(hcv_data$PROT)]<- median(hcv_data$PROT,na.rm = TRUE)
sapply(hcv_data, function(x) sum(is.na(x)))
# run oversampled function on data
oversampled_data <- oversample_smote(
  hcv_data,  
  c(1, 2, 3, 4, 5), 
  'Category',         # Option for oversampling
  m <- 2500             # Value of 0 creates 50:50 split
)

ggplot(data = oversampled_data, aes(fill = Category)) +
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

table(oversampled_data$Category)

