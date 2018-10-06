#importing the dataset
train<-read.csv('train.csv',header=TRUE)

test<-read.csv('test.csv',header=TRUE)

#add a veriable "super" to the test set to allow for combining data sets
test.Survived<-data.frame(Survived=rep("None",nrow(test)),test[,])

#combining the data set
data.combined <-rbind(train, test.Survived)

#Preprocess the passenger names to come up with a list of titles that 
#represent families and represent using appropriate visualization graph.

titles <- apply(data.combined,1,function(row){
  strsplit(strsplit(as.character(row['Name']),', ')[[1]][2],'\\.')[[1]][1]
})
summary(as.factor(titles))

library(ggplot2)
ggplot(data.combined, aes(titles, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('tittle Impact on Survival') 

#Represent the proportion of people survived from the family size using a graph.
library(ggplot2)

familySize <- data.combined$SibSp + data.combined$Parch + 1
ggplot(data.combined, aes(familySize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('Family Size Impact on Survival') + 
  labs(y = '%')

#Impute the missing values in Age variable using Mice Library, 
#create two different graphs showing Age distribution before and after 
#imputation.

install.packages("mice")
library(mice)
sum(is.na(data.combined$Age))
imputes = mice(data.combined, m=5, maxit = 40)
Imputed=complete(imputes,5)
hist(data.combined$Age,  main='Histogram Before impute',col="green")
hist(Imputed$Age, main='Histogram after impute',col="black")
