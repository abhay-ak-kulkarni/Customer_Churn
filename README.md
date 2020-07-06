# Customer_Churn
---
title: "Customer Churn"
author: "Abhay Kulkarni"
date: "9/21/2019"
output:
  pdf_document: 
    fig_caption: yes
    number_sections: yes
    toc: yes
    latex_engine: lualatex
  github_document: default
  html_document:
    toc: yes
    toc_depth: '3'
    df_print: paged
header-includes:
- \usepackage{titling}
- \pretitle{\begin{center}\LARGE\includegraphics[height=20cm]{customer-churn-edit.jpeg}\\[\bigskipamount]}
- \posttitle{\end{center}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
\newpage
**Customer Churn in Telecom Industry**

# Introduction
![Customer Churn ](customer-churn-edit.jpeg)

## What is Customer Churn   
   
   
Customer attrition, also known as customer churn, customer turnover, or customer defection, is the loss of clients or customers.    
   
      
         
## Why Predict Customer Churn   
   
      
         
Current organizations face a huge challenge: to be able to anticipate to customers’ abandon in order to retain them on time, reducing this way costs and risks and gaining efficiency and competitivity. 

\newpage

# Libraries/ Packages

```{r}
library(DataExplorer)
library(ggplot2)
library(caTools)
library(xlsx)
library(skimr)
library(caret)
library(cowplot)
library(caTools)
library(ROSE)
library(ROCR)
library(MLmetrics)
library(MASS)
library(class)
library(e1071)
library(car)

```

\newpage



# Speeding Processor Cores

```{r}
library(parallel)
library(doParallel)
clusterforspeed <- makeCluster(detectCores() - 1) ## convention to leave 1 core for OS
registerDoParallel(clusterforspeed)
```

 
\newpage

# Project Objective   
     
        
Customer Churn is a burning problem for Telecom companies. In this project, we simulate one such case of customer churn where we work on a data of postpaid customers with a contract. The data has information about the customer usage behaviour, contract details and the payment details. The data also indicates which were the customers who cancelled their service. Based on this past data, we need to build a model which can predict whether a customer will cancel their service in the future or not.

You are expected to do the following : 

## EDA   
  
  * How does the data look like, Univariate and bivariate analysis. Plots and charts which illustrate the relationships between variables
  * Look out for outliers and missing value
  * Check and treat for multicollinearity
  * Summarize the insights you get from EDA   
     
         
             
## Build Models and compare them to get to the best one   
   
   * Logistic Regression
   * KNN 
   * Naive Bayes
   * Model Comparison using Model Performance metrics & Interpretation   
      
          
              
## Actionable Insights   
   
   * Interpretation & Recommendations from the best model

   
      
         
         
               
            





\newpage
# Step by Step Approach   
   
      
## EDA   
   
       
           
### Set Working Directory   
   
       
          
          

```{r}
setwd("H:\\Github PROJECTS\\Customer Churn\\Customer_Churn")
getwd()
```


   
      
         
### Import Data   
```{r}
mydata<-read.xlsx2("Cellphone.xlsx",  sheetIndex = 2, header = TRUE)
```
Imported the xlsx datasheet with index 2 as the data is in the 2nd sheet.
   
       
          
             
                
### Check Dataset
```{r}
skim(mydata)
```
         
              
                   
                        
                             
                             
 **Findings **      
   
*There are 3333 observations and 11 variables.   
   
      
      
```{r}
str(mydata)
```
   
**Findings**   
* AccountWeeks, DataUsage, DataUsage, CustServCalls, DayMins, DayCalls, MonthlyCharge, OverageFee, RoamMins are incorrectly imported as factors. Convert factors to numeric data types.                    
* Churn , ContractRenewal and DataPlan are factor variables.    



\newpage
******   

### Creating Backup of the original dataset

```{r}
cleandata <- data.frame(mydata)
```  
         
###  AccountWeeks, DataUsage, DataUsage, CustServCalls, DayMins, DayCalls, MonthlyCharge, OverageFee, RoamMins are incorrectly imported as factors. Convert factors to numeric data types.

```{r}
cleandata$AccountWeeks<-as.numeric(as.character(cleandata$AccountWeeks))

```



```{r}
cleandata$DataUsage<-as.numeric(as.character(cleandata$DataUsage))

```


```{r}
cleandata$CustServCalls<-as.numeric(as.character(cleandata$CustServCalls))

```


```{r}
cleandata$DayMins<-as.numeric(as.character(cleandata$DayMins))

```


```{r}
cleandata$DayCalls<-as.numeric(as.character(cleandata$DayCalls))

```



```{r}
cleandata$DayCalls<-as.numeric(as.character(cleandata$DayCalls))

```


```{r}
cleandata$MonthlyCharge<-as.numeric(as.character(cleandata$MonthlyCharge))

```



```{r}
cleandata$OverageFee<-as.numeric(as.character(cleandata$OverageFee))

```



```{r}
cleandata$RoamMins<-as.numeric(as.character(cleandata$RoamMins))

```



******
\newpage

### Checking sturcture of data 

```{r}
str(cleandata)
```



   
      
          
### Dataset details

```{r}
introduce(cleandata)
```
   
**Findings** 
  
* There are **3333 Rows** and **11 Columns**
* 3 discrete colums and 8 continuous columns

### Visualize dataset   
   
      
```{r}
plot_intro(cleandata)
```






### Checking for **Missing Values**   
   
     
```{r}
plot_missing(cleandata)
```

**Findings** 
  
* There are no missing values in the dataset.   


******
\newpage
        

```{r}
scaledata <- data.frame(cleandata)
```


### Check if there are Outliers



```{r}
boxplot(scaledata[,c(2,5,6,7,8,9,10,11)],las=2,cex.axis=0.7)
```
   
      
**Findings** 
  
* There are outliers in the dataset.    
   
      
*****************
\newpage
           
### Outlier Treatment. Winsorizing.   
     
```{r}
summary(scaledata$AccountWeeks)
benchAccountWeeks <- 127.0 + 1.5* IQR(scaledata$AccountWeeks)
scaledata$AccountWeeks[scaledata$AccountWeeks>benchAccountWeeks] <- benchAccountWeeks

```

```{r}
summary(scaledata$DataUsage)
```


```{r}
benchDataUsage <- 1.78 + 1.5* IQR(scaledata$DataUsage)
scaledata$DataUsage[scaledata$DataUsage>benchDataUsage] <- benchDataUsage
```


```{r}
summary(scaledata$DayMins)
```


```{r}
benchDayMins <- 216.4 + 1.5* IQR(scaledata$DayMins)
scaledata$DayMins[scaledata$DayMins>benchDayMins] <- benchDayMins
```


```{r}
summary(scaledata$DayCalls)
```


```{r}
benchDayCalls <- 114.0 + 1.5* IQR(scaledata$DayCalls)
scaledata$DayCalls[scaledata$DayCalls>benchDayCalls] <- benchDayCalls
```

```{r}
summary(scaledata$MonthlyCharge)
```

```{r}
benchMonthlyCharge <- 66.20 + 1.5* IQR(scaledata$MonthlyCharge)
scaledata$MonthlyCharge[scaledata$MonthlyCharge>benchMonthlyCharge] <- benchMonthlyCharge
```


```{r}
summary(scaledata$OverageFee)
```

```{r}
benchOverageFee <- 11.77 + 1.5* IQR(scaledata$OverageFee)
scaledata$OverageFee[scaledata$OverageFee>benchOverageFee] <- benchOverageFee
```


```{r}
summary(scaledata$RoamMins)
```

```{r}
benchRoamMins <- 12.10 + 1.5* IQR(scaledata$RoamMins)
scaledata$RoamMins[scaledata$RoamMins>benchRoamMins] <- benchRoamMins
```


### Outliers Treated. 

```{r}
boxplot(scaledata[,c(2,5,6,7,8,9,10,11)],las=2,cex.axis=0.7)
```

      
### Checking Churn data split         
```{r}
table(scaledata$Churn)
```
  
```{r}
483/(2850+483)*100
```
      
         

**Findings** 
  
* Minority Class (1) is only 14.49 percent of the total Churn class. It is less than 15%.   
* Data will be balanced.     
    
         






         
### Visualize Dependent Variable(Churn) proportion split   
   
```{r}
ggplot(scaledata) +
 aes(x = Churn, fill = Churn) +
 geom_bar() +
 scale_fill_hue() +
 labs(title = "Target Variable(Churn) Split") +theme(plot.title = element_text(hjust = 0.5))
 
```

   
### Dependent Variables VS Independent Variables   
       
```{r}
d1 <- ggplot(scaledata) +
 aes(x = AccountWeeks, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "AccountWeeks Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h1 <- ggplot(scaledata) +
 aes(x = AccountWeeks) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "AccountWeeks Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d1,h1)

```



**Findings** 
  

**Density plot:** AccountWeeks around 100 has more Churn Count.

**Histogram:** We can say that the frequency distribution for AccountWeeks are normally distributed.    
   
       
```{r}
d2 <- ggplot(scaledata) +
 aes(x = DataUsage, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "DataUsage Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h2 <- ggplot(scaledata) +
 aes(x = DataUsage) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "DataUsage Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d2,h2)

```
   
       
**Findings** 
  

**Density plot:** There are more **0** DataUsage in the dataset. 

**Histogram:** There are more **0** DataUsage in the dataset.


       
```{r}
d3 <- ggplot(scaledata) +
 aes(x = DayMins, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "DayMins Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h3 <- ggplot(scaledata) +
 aes(x = DayMins) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "DayMins Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d3,h3)

```


**Findings** 
  

**Density plot:** Customers have Churned more for DayMins between **150 and 280** . 

**Histogram:** DaysMins is normally Distributed




```{r}
d4 <- ggplot(scaledata) +
 aes(x = DayCalls, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "DayCalls Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h4 <- ggplot(scaledata) +
 aes(x = DayCalls) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "DayCalls Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d4,h4)

```

**Findings** 
  

**Density plot:** DayCalls are almost evenly distributed for Churn and Not Churn . 

**Histogram:** DayCalls is normally Distributed




```{r}
d5 <- ggplot(scaledata) +
 aes(x = MonthlyCharge, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "MonthlyCharge Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h5 <- ggplot(scaledata) +
 aes(x = MonthlyCharge) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "MonthlyCharge Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d5,h5)

```


**Findings** 
  

**Density plot:** Customer with average monthly bill between **50 and 75** is more likely to Churn. 

**Histogram:** MonthlyCharge is normally Distributed




```{r}
d6 <- ggplot(scaledata) +
 aes(x = OverageFee, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "OverageFee Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h6 <- ggplot(scaledata) +
 aes(x = OverageFee) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "OverageFee Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d6,h6)

```




**Findings** 
  

**Density plot:** Customer with Overcharge fee between **5 and 15** are more likely to cancel the service. 

**Histogram:** Overcharge is normally Distributed




```{r}
d7 <- ggplot(scaledata) +
 aes(x = RoamMins, fill = Churn) +
 geom_density(adjust = 1L) +
 scale_fill_hue() +
 labs(title = "RoamMins Vs Churn") +
 theme(plot.title = element_text(hjust = 0.5))   
   

h7 <- ggplot(scaledata) +
 aes(x = RoamMins) +
 geom_histogram(bins = 30L, fill = "#0c4c8a",colour="black") +
 labs(title = "RoamMins Histogram") +
 theme(plot.title = element_text(hjust = 0.5))

plot_grid(d7,h7)

```


**Findings** 
  

**Density plot:** Out of Customers who have cancelled the service for RoamMins are most arund 10 minutes 

**Histogram:** RoamMins is normally Distributed






```{r}
h8<- ggplot(scaledata) +
 aes(x = ContractRenewal, fill = Churn) +
 geom_bar() +
 scale_fill_hue() +
 labs(title = "ContracRenewal VS Churn") +
 theme(plot.title = element_text(hjust = 0.5))

h8
```



**Findings** 
  

**Bar Chart:** Customers who have renewed the plan are more likely to stay with the company



```{r}
plot_correlation(scaledata, type = c("continuous"))
```



**Findings**   

* DataUsage and MonthlyCharge are Highly Correlated.   

* MonthlyCharge and DayMins are Highly Correlated.


*****
\newpage

# **Build Models**   
  
  
## Pre Process( Train and Test Split)

```{r}
seed <- 101
set.seed(seed)
sample <- sample.split(scaledata,SplitRatio = 0.7)
churn.train <- subset(scaledata,sample == TRUE)
churn.test <- subset(scaledata,sample == FALSE)
nrow(churn.train)
nrow(churn.test)
```
   
      
 ### Balance Dataset with ROSE package          
```{r}
BalancedData=ROSE(Churn~.,data=churn.train,seed=seed)$data
 
```


### Before Balance VS After Balance

```{r}
table(churn.train$Churn) 
table(BalancedData$Churn)
```

 **Findings**   

* Dataset is now BALANCED  
  
  
  
## Logistic Regression   
   
Logistic regression is a statistical model that in its basic form uses a logistic function to model a binary dependent variable, although many more complex extensions exist. In regression analysis, logistic regression (or logit regression) is estimating the parameters of a logistic model (a form of binary regression).   
    
    

 

![Logistic VS Linear ](linear_vs_logistic_regression.jpg)
  



\newpage


### Full Model for stepwise variable selection and elimination
```{r}
fullmod <- glm(Churn ~.,family=binomial,data = BalancedData)

summary(fullmod)
```


### Empty Model for stepwise variable selection and elimination

```{r}
emptyModel<- glm(Churn ~ 1,family=binomial,data = BalancedData)

summary(emptyModel)
```

### Baclward Selection of significant variables
```{r}
backwards = step(fullmod) 
```



### Forward Slection

```{r}
forwards = step(emptyModel,scope=list(lower=formula(emptyModel),upper=formula(fullmod)), direction="forward")
```

 **Findings**   

* From both Forward Selection and Backward Elimination, we see that DataUsage is insignificant feature. 

* Build model without DataUsage


### Modelling Logistic Regression with Cross Validation   
   
```{r}
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
 
  savePredictions = TRUE
  
)
```


### Logistic Regression Model   


```{r}

lregmodel<- glm(Churn ~  ContractRenewal + CustServCalls + DayMins + DataPlan + 
                  OverageFee + MonthlyCharge + RoamMins + DayCalls + AccountWeeks,data=BalancedData,family="binomial")
summary(lregmodel)

```



### Checking for Multicollinearity    

```{r}
vif(lregmodel)
```

 **Findings**   

* Values below 4 indicates that there is no Multicollinearity




### Predicting for Train Data




```{r}
logpred<- predict(lregmodel,BalancedData[-1],type = "response")
```


### Probabilities

```{r}
logpred
```

### Coverting Probabilites to 0 and 1

```{r}
ypredlog <- ifelse(logpred > 0.5,1,0)
ypredlog
```






### ConfusionMatrix and Training Model Evaluation


```{r}
ConfusionMatrix(ypredlog, BalancedData$Churn)

```

### Accuracy of Train, Precision , Recall, Sensitivity and F score

```{r}
accuracy.meas(ypredlog,BalancedData$Churn)
```

```{r}
Accuracy(ypredlog,BalancedData$Churn)
```


```{r}
Sensitivity(BalancedData$Churn, ypredlog)
```


```{r}
Specificity(BalancedData$Churn,ypredlog)
```

```{r}
AUC(y_pred = ypredlog,y_true = BalancedData$Churn)
```



### **Model Evaluation Table**

|   Accuracy  | 0.7322018 |
|:-----------:|:---------:|
| Sensitivity | 0.7551789 |
| Specificity | 0.7091596 |
|   F Score   |   0.323   |
|   AUC   |    0.7321692  |



### ROC Curve Plot

```{r}
rocrpred2=prediction(ypredlog,BalancedData$Churn)
as.numeric(performance(rocrpred2,"auc")@y.values)
pref2=performance(rocrpred2,"tpr","fpr")
plot(pref2)
```





\newpage




### Predicting Test Data


```{r}
logpredtest <- predict(lregmodel,churn.test[-1],type = "response")
```


### Coverting Probabilites to 0 and 1

```{r}
logpredtestprob <- ifelse(logpredtest > 0.5,1,0)
logpredtestprob
```

### ConfusionMatrix and Test Data Model Evaluation


```{r}
ConfusionMatrix(logpredtestprob, churn.test$Churn)

```

### Accuracy of Train, Precision , Recall, Sensitivity and F score

```{r}
accuracy.meas(logpredtestprob,churn.test$Churn)
```

```{r}
Accuracy(logpredtestprob,churn.test$Churn)
```


```{r}
Sensitivity(churn.test$Churn, logpredtestprob)
```


```{r}
Specificity(churn.test$Churn,logpredtestprob)
```


```{r}
AUC(y_pred = logpredtestprob,y_true = churn.test$Churn)
```


### **Model Evaluation Table**

|   Accuracy  | 0.7772277 |
|:-----------:|:---------:|
| Sensitivity | 0.7852941 |
| Specificity | 0.734375 |
|   F Score   |   0.229   |
|   AUC   |   0.7598346   |

   
### ROC Curve Plot

```{r}
rocrpred=prediction(logpredtestprob,churn.test$Churn)
as.numeric(performance(rocrpred,"auc")@y.values)
pref=performance(rocrpred,"tpr","fpr")
plot(pref)
```







******
\newpage

## KNN

### **What is KNN**   


KNN (K — Nearest Neighbors) is one of many (supervised learning) algorithms used in data mining and machine learning, it’s a classifier algorithm where the learning is based “how similar” is a data (a vector) from other .   
  
     
![KNN ](KNN.png)


### Fitting KNN to the Training set and Predicting the Test set results

```{r}
ypredKNN <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=5
                )

```



```{r}
ypredKNN
```

### Confusion Matrix

```{r}
ConfusionMatrix(ypredKNN, churn.test$Churn)

```



```{r}

Accuracy(ypredKNN,churn.test$Churn)
```


### Trying Different K value

```{r}
ypredKNN2 <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=3
                )
```


```{r}
ConfusionMatrix(ypredKNN2, churn.test$Churn)

```   

```{r}

Accuracy(ypredKNN2,churn.test$Churn)
```

### Try k 9

```{r}
ypredKNN3 <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=9
                )
```


```{r}
ConfusionMatrix(ypredKNN3, churn.test$Churn)

``` 



```{r}

Accuracy(ypredKNN3,churn.test$Churn)
```


### Try K 15 

```{r}
ypredKNN4 <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=15
                )
```




```{r}
ConfusionMatrix(ypredKNN4, churn.test$Churn)

``` 


```{r}

Accuracy(ypredKNN4,churn.test$Churn)
```



### Try K 25

```{r}
ypredKNN5 <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=25
                )
```



```{r}
ConfusionMatrix(ypredKNN5, churn.test$Churn)

``` 

```{r}

Accuracy(ypredKNN5,churn.test$Churn)
```


### Try K 35

```{r}
ypredKNN6 <- knn(train = BalancedData[,-1],
                test = churn.test[,-1],
                cl= BalancedData[,1],
                k=35
                )
```


```{r}
ConfusionMatrix(ypredKNN6, churn.test$Churn)

``` 


```{r}

Accuracy(ypredKNN6,churn.test$Churn)
```


```{r}
Sensitivity(churn.test$Churn, ypredKNN6)
```


```{r}
Specificity(churn.test$Churn,ypredKNN6)
```


```{r}
accuracy.meas(ypredKNN6,churn.test$Churn)
```


```{r}
KNNAUC<- AUC(ypredKNN6,churn.test$Churn)

KNNAUC
```

\newpage

### **Model Evaluation Table**

|   Accuracy  | 0.7508251 |
|:-----------:|:---------:|
| Sensitivity | 0.8107843 |
| Specificity | 0.4322917 |
|   F Score   |   0.185   |
|   AUC   |   0.621538   |


******

\newpage

## Naive Bayes

### What is Naive Bayes

In machine learning, naive Bayes classifiers are a family of simple "probabilistic classifiers" based on applying Bayes' theorem with strong (naive) independence assumptions between the features. 

![Naive Bayes ](naive-bayes.png)


### Naive Bayes. Is it applicable here? 

**YES** it is applicable. Naibe Bayes is a **probabilistic classifiers**. Here we'll use it to predict probability if Customer Cancelled(Churn) or Not Cancelled the service.

### Fitting Naive Bayes to training set
```{r}
classnaive <- naiveBayes(x=BalancedData[-1],
                         y=BalancedData$Churn)
```

### Prediciting Naive Bayes Test Dataset

```{r}
ypredNB<- predict(classnaive,newdata = churn.test[-1])
ypredNB
```

### ConfusionMatrix

```{r}
ConfusionMatrix(ypredNB,churn.test$Churn)
```



```{r}

Accuracy(ypredNB,churn.test$Churn)
```


```{r}
Sensitivity(churn.test$Churn, ypredNB)
```


```{r}
Specificity(churn.test$Churn,ypredNB)
```


```{r}
accuracy.meas(ypredNB,churn.test$Churn)
```


```{r}
NBAUC<- AUC(ypredNB,churn.test$Churn)

NBAUC
```

\newpage

### **Model Evaluation Table**

|   Accuracy  | 0.8531353 |
|:-----------:|:---------:|
| Sensitivity | 0.8686275 |
| Specificity | 0.7708333 |
|   F Score   |   0.189   |
|   AUC   |   0.8197304   |



******

\newpage


## Model Comparison using Model Performance metrics 


|             | Logistic Regression |    KNN    | Naive Bayes |
|:-----------:|:-------------------:|:---------:|:-----------:|
|   Accuracy  |      0.7772277      | 0.7508251 |  0.8531353  |
| Sensitivity |      0.7852941      | 0.8107843 |  0.8686275  |
| Specificity |       0.734375      | 0.4322917 |  0.7708333  |
|   F Score   |        0.229        |   0.185   |    0.189    |
|     AUC     |      0.7598346      |  0.621538 |  0.8197304  |



### Interpretation 

* From the above model Comparison we can see that **Naive Bayes** is performing the best.



### **Conclusion:**   

* We can see that all the models are performing similarly. However, Naive Bayes is performing the best. Naive Bayes is able to predict Customers who are going to cancel the service.


* ContractRenewal + CustServCalls + DayMins are the main factors which makes customer continue or cancel the service.


### **Recommendation:**

* CustServCalls greatly contribute influence customer's decision to continue or cancel the service. It look like Customer Service has to be improved. There are many customers calling customer service 10 times. Company must invest in training resources and improve their customer service quality and reduce the number of customer service calls.














