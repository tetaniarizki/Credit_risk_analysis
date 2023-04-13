library(openxlsx)
library(ggplot2)
library(ggcorrplot)
library(ggfortify)


#import data
cslarge_raw <- read.xlsx('https://storage.googleapis.com/dqlab-dataset/dqlab_pcadata.xlsx', sheet= 'cslarge')

str(cslarge_raw)

head(cslarge_raw,10)

summary(cslarge_raw)

#Income distribution based on dependents
ggplot(cslarge_raw, aes(as.factor(dependents), income)) +
  geom_boxplot()+xlab('Dependents')+ggtitle('Income Boxplot Based on Dependents')
#""For the income boxplot based on dependents, it can be inferred that there is no big difference in 
#the median and the distribution of individual income for each number of dependents. Where the median
# and income distribution of groups with 5 and 6 dependents have a greater amount of income compared
#to the previous 55 groups""

#Dept distribution based on dependents
ggplot(cslarge_raw, aes(as.factor(dependents), debt))+
  geom_boxplot()+xlab('Dependents')+ggtitle('Debt Boxplot Based on Dependents')
#""From the debt boxplot based on dependents, it is clear that the more dependents, 
#the greater the amount of debt held"" 


#Record the index or line number on each risk rating
index1 <- which(cslarge_raw$riskrating == 1)
index2 <- which(cslarge_raw$riskrating == 2)
index3 <- which(cslarge_raw$riskrating == 3)
index4 <- which(cslarge_raw$riskrating == 4)
index5 <- which(cslarge_raw$riskrating == 5)

#Prepare a number of training data sets and testing sets for each risk rating (use proportion 80:20)
set.seed(100)
ntrain1 <- round(0.8*length(index1))
ntrain2 <- round(0.8*length(index2))
ntrain3 <- round(0.8*length(index3))
ntrain4 <- round(0.8*length(index4))
ntrain5 <- round(0.8*length(index5))

#sampling data for each risk rating for the training and testing sets
train1_index <- sample(index1, ntrain1)
train2_index <- sample(index2, ntrain2)
train3_index <- sample(index3, ntrain3)
train4_index <- sample(index4, ntrain4)
train5_index <- sample(index5, ntrain5)

test1_index <- setdiff(index1, train1_index)
test2_index <- setdiff(index2, train2_index)
test3_index <- setdiff(index3, train3_index)
test4_index <- setdiff(index4, train4_index)
test5_index <- setdiff(index5, train5_index)

#Combine the sampling results of each risk rating into the training and testing set
cslarge_train <- do.call('rbind', list(cslarge_raw[train1_index,],
                                       cslarge_raw[train2_index,], cslarge_raw[train3_index,],
                                       cslarge_raw[train4_index,], cslarge_raw[train5_index,]))
cstrain <- subset(cslarge_train, select= -c(contractcode,riskrating))
nrow(cstrain)

cslarge_test <-do.call('rbind', list(cslarge_raw[test1_index,],
                                     cslarge_raw[test2_index,], cslarge_raw[test3_index,],
                                     cslarge_raw[test4_index,], cslarge_raw[test5_index,]))
cstest <- subset(cslarge_test, select = -c(contractcode,riskrating))
nrow(cstest)

csclass_train <- subset(cslarge_train, select=c(riskrating))

#Calculate the correlation between variables in the training sets
cor(cstrain)
#correlation matrix chart
ggcorrplot(cor(cstrain))
#""The correlation matrix graph shows that there are two groups of variables that have a fairly high correlation value. 
#The first group is between the variables age, work experience (empyear), assests, and income.
#Then the second group, namely past due (midoverdue) and dependents, Thus, the two groups indicate that 
#there are two  principal components that will be formed.""


#Perform PCA analysis with prcomp()
pca <- prcomp(cstrain, scale=TRUE, center=TRUE)

pca

summary(pca)

#visualization output of PCA
screeplot(pca, type='line', ylim=c(0,2))
abline(h=1, lty=3, col='red')

#plot PCA with risk rating
autoplot(pca, data=cslarge_train, colour='riskrating',
         loadings=TRUE, loadings.label=TRUE, loading.label.size=3, scale=0)

#""The application of Kaiser's criteria to the screeplot resulted in 2 PCs, namely PC1 and PC2, 
#which explained as much as 76% of the variability in the data.
#This is also consistent with the rotational inspection in the previous section,
#where PC3 and PC4 are repeats of the pattern on PC1 and PC2..""