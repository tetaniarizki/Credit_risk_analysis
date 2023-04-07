library('openxlsx')
library('C50')
library('reshape2')

#import data set
dataCreditRating <- read.xlsx(xlsxFile = 'https://storage.googleapis.com/dqlab-dataset/credit_scoring_dqlab.xlsx')
str(dataCreditRating)


#dataCreditRating$risk_rating[dataCreditRating$risk_rating == '1'] <- 'satu'
#dataCreditRating$risk_rating[dataCreditRating$risk_rating == '2'] <- 'dua'
#dataCreditRating$risk_rating[dataCreditRating$risk_rating == '3'] <- 'tiga'
#dataCreditRating$risk_rating[dataCreditRating$risk_rating == '4'] <- 'empat'
#dataCreditRating$risk_rating[dataCreditRating$risk_rating == '5'] <- 'lima'



#Convert the risk_rating column in a factor
dataCreditRating$risk_rating <- as.factor(dataCreditRating$risk_rating)
str(dataCreditRating)

#Remove some input variables from the dataset
#drop_column <- c('pendapatan_setahun_juta','kpr_aktif','rata_rata_overdue','risk_rating')
#datafeed <- dataCreditRating[,!names(dataCreditRating) %in% drop_column]
#str(datafeed)
input_columns <- c('jumlah_tanggungan','durasi_pinjaman_bulan')
datafeed <- dataCreditRating[, input_columns]
str(datafeed)

#Prepare random index portions for training and testing set
set.seed(100)
indeks_training_set <- sample(900,800) #90:10

input_training_set <- datafeed[indeks_training_set,]
input_testing_set <- datafeed[-indeks_training_set,]
class_training_set <- dataCreditRating[indeks_training_set,]$risk_rating


#Build a model decision tree for classification risk rating
risk_rating_model <- C5.0(input_training_set, class_training_set, control=C5.0Control(label='Risk Rating'))
summary(risk_rating_model)
plot(risk_rating_model)

#using the model for prediction testing
predict(risk_rating_model,input_testing_set)

#comparison of the actual and prediction data for risk rating
comparison_data <- input_testing_set
comparison_data$actual_risk_rating <- dataCreditRating[-indeks_training_set,]$risk_rating
comparison_data$prediction_results <- predict(risk_rating_model,input_testing_set)
comparison_data

#make confusion matrix for the testing data set
dcast(prediction_results~actual_risk_rating, data=comparison_data)

#calculating the number of correct prediction
nrow(comparison_data[comparison_data$actual_risk_rating==comparison_data$prediction_results,])
#calculating the number of wrong prediction
nrow(comparison_data[comparison_data$actual_risk_rating!=comparison_data$prediction_results,])

#if we have the new data and want to predict the new risk rating for the data
new_data <- data.frame(jumlah_tanggungan=c(3,2,1,4,5), durasi_pinjaman_bulan=c(24,12,28,24,36))
new_data$prediction_risk_rating <- predict(risk_rating_model,new_data)
new_data