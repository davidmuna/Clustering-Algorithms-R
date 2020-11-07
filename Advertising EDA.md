
# Core Week 12 IP : Site Ad-clicks Prediction Model


###################################### 1. Defining the Question

```## A Kenyan entrepreneur has created an online cryptography course and would want to advertise it on her blog. She currently targets audiences originating from various countries. In the past, she ran ads to advertise a related course on the same blog and collected data in the process. My task as a Data Science Consultant is to to help her by creating a model that predicts which individuals are most likely to click on her ads ```

###################################### 2. Metrics for Success

                              ``` 1. An Accuracy Score of atleast 85%
                                    2. A low RMSE Score
                                    3. Confusion Matrix with more correct predictions than incorrect ones
```

###################################### 3. Understanding the Context

``` It is imperative that all entrepreneurs identify all opportunities to advertise and boost their sales in whatever business they are engaged in. More over, online advertisements are a key driver of sales of products and services and its important for entrepreneurs to capitalise on this by identifying peak hours for ads to be clicked, the type of ads that people like, personality traits of site visitors and other demographics of potential clients such as age, employment status, interests, hobbies, career etc. Knowing such information creates an avenue for channeling the right adverts to potential clients and classifying or predicting whether or not they'll click on the ad.
```

###################################### 4. Recording the Experimental Design

                                ``` 1. Data Preparation and Cleaning
                                      2. Feature Engineering
                                      3. Exploratory Data Analysis
                                      4. Modelling
                                      5. Cross- Validation and Hyperparameter Tuning
                                      6. Evaluation
```

###################################### 5. Loading our dataset.

# Loading libraries for cleaning ,plotting and manipulating data using pacman. It installs and loads packages if a package is not installed.```


pacman :: p_load(pacman, dplyr, tidyverse, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
  plotly, rio, rmarkdown, shiny, stringr, tidyr, psych, corrplot, caret)


# previewing first 6 records

advertising <- import("~/R/Advertising Project (Cryptography)/Data/advertising.csv")
head(advertising)

# checking the size and shape of data
dim(advertising)


# viewing data types using str().

str(advertising)





###################################### 6. Data Preparation.

# checking for missing data in our columns

colSums(is.na(advertising)) 
# there are no missing records in the data

# checking and dealing with duplicates
# there are no duplicates in the data

duplicated_rows <- advertising[duplicated(advertising),]  
duplicated_rows  


# column names

colnames(advertising)  

# renaming and standardising columns

names(advertising)[names(advertising) == "Daily Time Spent on Site"] <- "dto_site"
names(advertising)[names(advertising) == "Age"] <- "age"
names(advertising)[names(advertising) == "Area Income"] <- "income"
names(advertising)[names(advertising) == "Daily Internet Usage"] <- "di_usage"
names(advertising)[names(advertising) == "Ad Topic Line"] <- "topic_line"
names(advertising)[names(advertising) == "City"] <- "city"
names(advertising)[names(advertising) == "Male"] <- "gender"
names(advertising)[names(advertising) == "Country"] <- "country"
names(advertising)[names(advertising) == "Timestamp"] <- "timestamp"
names(advertising)[names(advertising) == "Clicked on Ad"] <- "clicked"


# new column names

colnames(advertising) 

# converting time variable from character to date and time (POSIXct) format

advertising$timestamp <- as.POSIXct(advertising$timestamp, "%Y-%m-%d %H:%M:%S",tz = "GMT")


# checking final data types

columns = colnames(advertising)
for (column in seq(length(colnames(advertising)))){
    print(columns[column])
    print(class(advertising[, column]))
    cat('\n')
}




###################################### 7. Feature Engineering

# stripping date and time and creating new variables date and hour

advertising$date <- format(advertising$timestamp, "%Y-%m-%d")
advertising$hour <- format(advertising$timestamp, "%H")

# selecting gender, hour and clicked variables
# first line prevents ommission of row records 

options(max.print = 1000000)
hour_clicked <- advertising %>% select(10,12)
hour_clicked

# selecting clicked ads only
hour_clicked2 <- hour_clicked %>% filter(clicked == 0)

# checking if clicked is a factor and previewing
is.factor(hour_clicked2$clicked)
head(hour_clicked2)

# number of clicks grouped by hour
# we drop "clicked" feature since its a factor and create a new feature "clicked_new" then populate with 1's  

hour_clicked3 <- hour_clicked2 %>% select(2)
head(hour_clicked3)

# add new feature after hour feature
new <- hour_clicked3 %>% add_column(clicked_new = 1, .after = 1)
head(new)

# grouping
new2 <- aggregate(new$clicked_new, by=list(Category=new$hour), FUN=sum)
new2  

# new2 produces columns names as Category and x for hour and clicked_new respectively
# new2 will be used in time series plot under BIVARIATE




###################################### 8. Exploratory Data Anaylsis

#### UNIVARIATE 


# Categorical Variables using barplots

# Giving color to barplot

library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 

# creating gender table
gender <- table(advertising$gender)

# gender indicates slightly more males(0) than females(1)
barplot(gender,
        col=coul,
        main = "Count of Gender",
        xlab = "Males (0), Females(1)",
        ylab = "Count")  

# clicked below indicates that people who clicked and Ad or didnt are the same.
# this means the dataset is balanced

clicked <- table(advertising$clicked)
barplot(clicked,
        col=coul,
        main = "Clicked on Ad",
        xlab = "Clicked (0), No Click(1)",
        ylab = "Count")

# plotting numerical variables using histograms

# age seems to be slightly skewed to the right indicating younger people visit the site 
hist(advertising$age,
        main = "Distribution of Age",
        xlab = "Age",
        ylab = "Frequency")  


# First line puts graphs into 4 columns with 2 rows
par(mfrow=c(4,2))
par(mar=c(2,2,2,2))

# Histograms of distribution of time spent on site for men and women and whether they clicked on Ads

hist(advertising$dto_site [advertising$clicked == 0 & advertising$gender == 0],
    main = "Time on Site and Ad Clicks by Men ",
    xlab = "Time on Site",
    col = "blue")
    
hist(advertising$dto_site [advertising$clicked == 0 & advertising$gender == 1],
    main = "Time on Site and Ad Clicks by Women",
    xlab = "Time on Site",
    col = "red")
    
hist(advertising$dto_site [advertising$clicked == 1 & advertising$gender == 0],
    main = "Time on Site and No Clicks by Men ",
    xlab = "Time on Site",
    col = "lightblue")
    
hist(advertising$dto_site [advertising$clicked == 1 & advertising$gender == 1],
    main = "Time on Site and No Clicks by Women ",
    xlab = "Time on Site",
    col = "pink")

# We can see with Ad Clicks, women spent more time on the site than men and time spent on site is not normally distributed

# reset graph to 1 column 1 row
par(mfrow=c(1,1)) 


# Area income distribution is skewed to the left indicating a lot of people have high income
 
hist(advertising$income,
        col=coul,
        main = "Distribution of Area Income",
        xlab = "Income",
        ylab = "Frequency")
        
# Plotting Boxplots of numerical variables

num_vars <- advertising %>% select(dto_site,age,income, di_usage)

# income variable has a few outliers (ie income less than 20,000), other variables are okay
boxplot(num_vars)


# calculating measures of central tendency and dispersion using summary() to get mean, median, max/min, 1st & 3rd quartiles
summary(num_vars)

# using describe() to get range, skewness, kurtosis and standard deviation among others
describe(num_vars)

# calculating variance for the numerical variables 
var(num_vars$dto_site)
var(num_vars$age)
var(num_vars$income)
var(num_vars$di_usage)



# daily time on site and area income have very high variance. This indicates the two are highly skewed and are not normally distributed.

# defining a function called mode since mode has no in-built function in R

mode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

# calculating the mode for the numerical variables, this indicates the value that is most common
# or appears the most in each of the variables. For instance, the most common area income is 61833.9 as per the output below
mode(num_vars$dto_site)
mode(num_vars$age)
mode(num_vars$income)
mode(num_vars$di_usage)

#### BIVARIATE


# time series line plot

time_series = ggplot(data = new2, aes(x = Category, y = x, group = 3))+
  geom_line(color = "#00AFBB", size = 2) + labs(title = 'Peak Hours For Ad Clicks', x = 'Hour of Day', y = 'Number of Ad Clicks') +
    theme(plot.title=element_text(size=18, face="bold", color="navyblue",hjust=0.5, lineheight=1.2), 
          plot.subtitle=element_text(size=15, face="bold", hjust=0.5),
         axis.title.x = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = -0.5),
         axis.title.y = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = 0.5),
         axis.text.y = element_text(size = 13),
         axis.text.x = element_text(size = 13),
         legend.title = element_text(size = 13, color = 'navyblue'),
        legend.text = element_text(size = 11))
plot(time_series)
        
# colclusion from the plot above is that our entrepreneur should target advertisements during the peak hours of 7am and 9pm for more clicks and views

# computing a correlation matrix between all numerical variables using pearson method

all_num_vars <- advertising %>% select(dto_site,age,income, di_usage,gender,clicked)
correlations <- cor(all_num_vars, method = "pearson")
round(correlations, 2)

# daily time on site and daily internet usage are strongly correlated to each other with a coefficient of # 0.52 indicating a positive relationship.
# the two variables also have a strong negative correlation of -0.79 and -0.75 with the target 
# variable'clicked'

# viewing the correlations better

corrplot(correlations, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# plotting a scatter plot of daily time spent on site versus daily internet usage

# converting all categorical variables to factors first```

advertising$topic_line <- factor(advertising$topic_line) 
advertising$city <- factor(advertising$city) 
advertising$gender <- factor(advertising$gender) 
advertising$country <- factor(advertising$country) 
advertising$clicked <- factor(advertising$clicked) 


# a scatter plot showing daily time on site vs daily internet usage by gender

options(repr.plot.width = 13, repr.plot.height = 7)

dto_site_di_usage = ggplot(data = advertising, aes(x = di_usage, y = dto_site , col = gender)) + 
    geom_point() + 
    labs(title = 'Daily Time On Site Vs Daily Internet Usage', x = 'Time Spent On Site', y = 'Internet Usage') + 
    scale_color_brewer(palette = 'Set2') +
    theme(plot.title=element_text(size=18, face="bold", color="navyblue",hjust=0.5, lineheight=1.2), 
          plot.subtitle=element_text(size=15, face="bold", hjust=0.5),
         axis.title.x = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = -0.5),
         axis.title.y = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = 0.5),
         axis.text.y = element_text(size = 13),
         axis.text.x = element_text(size = 13),
         legend.title = element_text(size = 13, color = 'navyblue'),
        legend.text = element_text(size = 11))
        
plot(dto_site_di_usage)

# from the plot above, there seems to be some not so perfect relationship between internet usage and time on the site. Time on site is lower when internet usage is lower and vice versa as seen by the dense dots at the bottom left and top right sections of the plot. This means internet usage and time on site goes on both extremes, either low or high.

    
# visualising covariance between daily internet usage and other variables

income_cov <- advertising$income
age_cov <- advertising$age
internetusage <- advertising$di_usage
timeonsite <- advertising$dto_site

cov(internetusage, timeonsite)
cov(internetusage, income_cov)
cov(internetusage, age_cov)

# we can see from the results that internet usage and income have a very high covariance indicating a      strong relationship.

# internet usage and age negative covariance suggests that as younger people have higher internet usage    but the relationship is not perfect or linear

# scatterplot of income and age by gender

options(repr.plot.width = 13, repr.plot.height = 7)

income_age = ggplot(data = advertising, aes(x = income, y = age , col = gender)) + 
    geom_point() + 
    labs(title = 'Distribution of Area Income and Age by gender', x = 'Area Income', y = 'Age') + 
    scale_color_brewer(palette = 'Set2') +
    theme(plot.title=element_text(size=18, face="bold", color="navyblue",hjust=0.5, lineheight=1.2), 
          plot.subtitle=element_text(size=15, face="bold", hjust=0.5),
         axis.title.x = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = -0.5),
         axis.title.y = element_text(color = 'navyblue', size = 13, face = 'bold', vjust = 0.5),
         axis.text.y = element_text(size = 13),
         axis.text.x = element_text(size = 13),
         legend.title = element_text(size = 13, color = 'navyblue'),
        legend.text = element_text(size = 11))
        
plot(income_age)

# from the plot we can say that majority of people are within the ages of 25-45 years and earn incomes between the range of 40,000-80,000.


#### dealing with outliers

```## from the boxplots in univariate analysis, there were outliers in the area income that caused its distribution to be skewed.We retain them since income can be skewed in real life scenario.```

#### dealing with missing data

```## theres no missing data in our dataset```

#### BIVARIATE RECOMMENDATIONS

```## to conduct a better bivariate analysis, more variables such as personality traits, interests of site visitors and site visitors' profession(student, salesman, broker etc) need to be captured.```

```## additionally, my recommendation is to implement the solution by creating a model that predicts whether a person will click an Ad or not.```


###################################### 9. Implementing the Solution.

#### K Nearest Neighbours

```R
# Lets now create test and train data sets

train <- advertising[1:900,1:4]
test <- advertising[901:1000,1:4]
train_sp <- advertising[1:900,10]
test_sp <- advertising[901:1000,10]
```

```R
# Now we can use the K-NN algorithm by calling the "class" package which contains the K-NN algorithm.
# We then have to provide 'k' value which is no of nearest neighbours(NN) to look for in order to classify the test data
# cl is the class of the training data set and k is the no of neighbours 

library(class)    
require(class)

model <- knn(train= train,test=test, ,cl= train_sp,k=13)
model


# Converting model to a factor and creating a table to be used in confusion matrix

table(factor(model))

# Comparing the model and the test data to see classifications 

table(test_sp,model)


# Obtaining accuracy of correct classifications

accuracy <- mean(test_sp==model)
accuracy
```
# KNN Model Produces an Accuracy of 0.7
# Adjusting the k value to try and get better accuracy

model2 <- knn(train= train,test=test, ,cl= train_sp,k=17)
model2


# Converting model to a factor and creating a table to be used in confusion matrix

table(factor(model2))

# Comparing the model and the test data to see classifications 

table(test_sp,model2)


# Obtaining accuracy of correct classifications

accuracy2 <- mean(test_sp==model2)
accuracy2

# Increasing the k from 13 to best of value of 17 gives an accuracy of 0.75 with reduced incorrect classifications 
# based on the new confusion matrix


#### Decision Trees

# loadind necessary packages
library("caret")
library("rpart") ## for recursive partitioning
library("rpart.plot")

# Modelling 
model_dt <- rpart(clicked ~ ., data = advertising,
           method = "class")
rpart.plot(model_dt)

# Prediction
```R
pred <- predict(model_dt, advertising, type = "class")
table(pred, advertising$clicked)

mean(pred == advertising$clicked)  # Calculating the Accuracy
```
# The model has an accuracy of 100%, this is probably a case of overfitting.
# COnfusion matrix reveals no incorrect classifications
```R

# Tuning the model by using ranger method, 5-fold cross validation, and setting a tuneLength of 5 to see if we can reduce overfitting

set.seed(42)
model_dt_tuned <- train(clicked ~ .,
               data = advertising,
               method = "ranger",
               tuneLength = 5,
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        verboseIter = FALSE))
                                        
# Plotting tuned rf model and previewing model's metrics
plot(model_dt_tuned)
model_dt_tuned
```{R}
# Tuning parameter 'min.node.size' was held constant at a value of 1
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were mtry = 412, splitrule = extratrees and min.node.size = 1.
# This returned an accuracy of 0.966
# and Kappa of 0.932
```

#### Naive Bayes Algorithm

```
```R
# Splitting data into training and testing data sets

# First converting hour column to numeric and date to date
advertising$hour <- as.numeric(advertising$hour)
advertising$date <- as.POSIXct(advertising$date)
str(advertising)

# Partitionind data into 75% training
indxTrain <- createDataPartition(y = advertising$clicked,p = 0.75,list = FALSE)

# Splitting
training <- advertising[indxTrain,]
testing <- advertising[-indxTrain,]
 
```

```R
# Checking dimensions of the split
prop.table(table(advertising$clicked)) * 100
prop.table(table(advertising$clicked)) * 100
prop.table(table(advertising$clicked)) * 100

```
```R
# Comparing the outcome of the training and testing phase by creating objects x which holds the predictor variables and y which holds the response variables
x = training[,-(9:11)] # exluding timestamp, date and clicked
y = training$clicked
```
```R
# Loading our inbuilt e1071 package that holds the Naive Bayes algorithm

library(e1071)
```

```R
# Now building Naive Bayes Model with 10 fold cross validation 
model_nb = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

# Previwing model metrics
model_nb
```

```R
# Naive Bayes Model Evalution

# Predicting our testing set

predictions <- predict(model_nb,newdata = testing )

# Getting the confusion matrix and calculating accuracy value 

confusionmatrix <- table(predictions, testing$clicked )
confusionmatrix

accuracy <- mean(predictions == testing$clicked)
accuracy
```
```{R}
# The Naive Bayes Model also performs well with an accuracy of 0.952 and matrix has very few incorrect classifications

```
```
#### SVM Algorithm

# Viewing plot of some numerical variables by clicked to identify possibility of seperation/classification

qplot(advertising$income, advertising$dto_site, color=advertising$clicked)

# The scatter plot shows possibility os seperation hence SVM is suitable

```
# Library for SVM (e1071) was already loaded previously

model_svm <- svm(clicked ~.,
                  data = advertising)
                  
# Viewing svm model summary
summary(model_svm)

# Plotting svm model results
plot(model_svm, data=advertising,
              di_usage~dto_site)
              
# Predictions and accuracy and confusion matrix to see classifications

svm_pred <- predict(model_svm, data=advertising)

accuracy <- mean(svm_pred==advertising$clicked)
accuracy

confusionmatrix_svm <- table(svm_pred, advertising$clicked)
confusionmatrix_svm
```

# From the confusion matrix, the number of misclassification were very little with an accuracy of 0.945. However, we can still improve our model by tuning parameters a little.

# Tuning SVM model
set.seed(134)

# In ranges parameter, seq() creates a sequence of 0 to 0.5 for epsilon with increments of 0.1
# epsilon is the margin of tolerance where no penalty is given to errors.
# cost captures cost of contraint violation with default of 1. If too high, it means high penalty for non seperable points leading to the model storing too many support vectors leading to overfitting and vice versa. 
# We use a small range of between 2^(2:3) to capture optimal cost value since our dataset is large. ie 2 to the powers of 2-4
# The set epsilon(6) and cost(3) above gives us 18 different combinations

model_svm_tuned <- tune(svm, clicked~.,data =advertising,
                        ranges =list(epsilon = seq(0,0.5,0.1), cost=2^(2:4)))
                        
plot(model_svm_tuned,xlab = "Epsilon", ylab = "Cost")

# This svm performance plot means that darker regions have better results. ie regions below cost of 8 and various values for epsilon

# Getting summary of best svm model from tuned model
# Code below retuns SVM-Kernel as raddial since this is best for classification purposes with a cost of 4

final_svm_model <- model_svm_tuned$best.model
summary(final_svm_model)
```
# Predictions and accuracy and confusion matrix to see classifications of tuned svm model

tuned_svm_pred <- predict(final_svm_model, data=advertising)

accuracy_tuned <- mean(tuned_svm_pred==advertising$clicked)
accuracy_tuned

tuned_svm_cm <- table(tuned_svm_pred, advertising$clicked)
tuned_svm_cm
confusionmatrix_svm

```{R}
# Tuned accuracy of SVM model has improved from 0.945 to 0.967 with reduced misclassifications
# Therefore, models performance after tuning were KNN (0.75), Decision trees(0.966), Naive Bayes(0.952) and SVM (0.967)

# The best performing model was SVM with an accuracy of 0.967. This is our final choice.
```


