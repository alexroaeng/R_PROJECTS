library(missMDA)
library(rio)
library(mice)
library(skimr)
library(summarytools)
library(tidyverse)
library(skimr)
library(lubridate)
library(GGally)
library(stringr)
library(corrgram)
library(ggplot2)
setwd("C:/Users/ALEX/Desktop/FOR UPLOAD/to be zip")
link = 'listings .csv'
df<-import(link) 
dim(df)
#obs      variables
# 20021    74
#reducing no usable colums

# skip = 3)
head(df,5)
df1 = df[,-c(1:3)]
df1 = df1[,-c(4:6)]
head(df1,5)

data.frame(colnames(df1)) ## getting the number of columns
df1 = df1[,-c(4:4)]
df1 = df1[,-c(12:13)]
unique(df1$calendar_updated)
df1 = df1[,-c(40)]
unique(df1$bathrooms)
df1 = df1[,-c(26)]
unique(df1$neighbourhood_cleansed)# eliminate just 16 variables 
df1 = df1[,-c(19)]

unique(df1$neighbourhood_group_cleansed)
df1 = df1[,-c(19)]
###290721
data.frame(colnames(df1))
unique(df1$license)
df1 = df1[,-c(55)]
df1$license
#here i finish the the cleaning of null N/A on mi dataset
df1$host_total_listings_count == df1$host_listings_count
df1 = df1[,-c(14)]
#change character for logical vectors
#chosse the variable who are logic on the data firts 

df1$host_is_superhost <- recode(df1$host_is_superhost, 'f' = '0')
df1$host_is_superhost <- recode(df1$host_is_superhost, 't' = '1')
df1$host_has_profile_pic <- recode(df1$host_has_profile_pic, 'f' = '0')
df1$host_has_profile_pic <- recode(df1$host_has_profile_pic, 't' = '1')
df1$has_availability <- recode(df1$has_availability, 'f' = '0')
df1$has_availability <- recode(df1$has_availability, 't' = '1')
df1$host_identity_verified <- recode(df1$host_identity_verified, 'f' = '0')
df1$host_identity_verified <- recode(df1$host_identity_verified, 't' = '1')
#  i finish the convertion on the last step 
dft <- df1[,c("host_is_superhost","host_has_profile_pic","host_identity_verified","has_availability")]
skim(dft)
cond =dft$host_is_superhost == ''
dft[cond,]$host_is_superhost <- NA
unique(dft$host_is_superhost)
as.numeric(dft$host_is_superhost)->dft$host_is_superhost

cond =dft$host_has_profile_pic == ''
dft[cond,]$host_has_profile_pic <- NA
unique(dft$host_has_profile_pic)
as.numeric(dft$host_has_profile_pic)->dft$host_has_profile_pic

cond =dft$host_identity_verified == ''
dft[cond,]$host_identity_verified <- NA
unique(dft$host_identity_verified)
as.numeric(dft$host_identity_verified)->dft$host_identity_verified

#filling emtpy space and convert to numeric 
cond =dft$has_availability == ''
dft[cond,]$has_availability <- NA
unique(dft$has_availability)
as.numeric(dft$has_availability)->dft$has_availability
# replace na with impute PCA
filledcolumns <- imputePCA(dft)
filledcolumns <- filledcolumns$completeObs
dft <- filledcolumns
as.data.frame(dft) -> dft
md.pattern(dft,
           rotate.names = F) ### for analize the empty values 
as.integer(dft$host_is_superhost)->df1$host_is_superhost
as.integer(dft$host_has_profile_pic)->df1$host_has_profile_pic
as.integer(dft$host_identity_verified)->df1$host_identity_verified
as.integer(dft$has_availability)->df1$has_availability

#finish the cleaning of logic look to maximun and realize minimun maximun 
#there are column who dont make sense 300721
data.frame(colnames(df1))
df1 = df1[,-c(30:35)]

#host_verifications
#hots neighbourhood
#neighbourhood
data.frame(colnames(df1))

df1 = df1[,-c(14)]
data.frame(colnames(df1))
df1 = df1[,-c(16)]
data.frame(colnames(df1))
df1 = df1[,-c(12)]

skim(df1$host_neighbourhood)

#divide by sections and look for enmpty values 
##48:50 cleanning
dft <- df1[,c(48:50)]
cond =dft$reviews_per_month  <= 0
dft[cond,]$reviews_per_month <- 0
cond =dft$reviews_per_month ==''
dft[cond,]$reviews_per_month <- NA
dft$reviews_per_month<-as.integer(dft$reviews_per_month)
###
cond =dft$calculated_host_listings_count_private_rooms  <= 0
dft[cond,]$calculated_host_listings_count_private_rooms <- 0
cond =dft$calculated_host_listings_count_private_rooms ==''
dft[cond,]$calculated_host_listings_count_private_rooms <- NA
dft$calculated_host_listings_count_private_rooms<-as.integer(dft$calculated_host_listings_count_private_rooms)

cond =dft$calculated_host_listings_count_shared_rooms  <= 0
dft[cond,]$calculated_host_listings_count_shared_rooms <- 0
cond =dft$calculated_host_listings_count_shared_rooms ==''
dft[cond,]$calculated_host_listings_count_shared_rooms <- NA
dft$calculated_host_listings_count_shared_rooms<-as.integer(dft$calculated_host_listings_count_shared_rooms)

filledcolumns <- imputePCA(dft)
filledcolumns <- filledcolumns$completeObs
dft <- filledcolumns
as.data.frame(dft) -> dft
md.pattern(dft,
           rotate.names = F)
df1$reviews_per_month<-as.integer(dft$reviews_per_month)
df1$calculated_host_listings_count_shared_rooms<-as.integer(dft$calculated_host_listings_count_shared_rooms)
df1$calculated_host_listings_count_private_rooms<-as.integer(dft$calculated_host_listings_count_private_rooms)
table(dft$calculated_host_listings_count_shared_rooms)
#COMPLETE VARIABLES 48-50 310821
df1$instant_bookable <- recode(df1$instant_bookable, 'f' = '0')
df1$instant_bookable <- recode(df1$instant_bookable, 't' = '1')
cond =df1$instant_bookable ==''
df1[cond,]$instant_bookable <- NA

as.integer(dft$instant_bookable)->df1$instant_bookable
##done with instant bookable 

dft <- df1[,c("review_scores_value","review_scores_location","review_scores_communication","review_scores_cleanliness","review_scores_checkin","review_scores_rating","review_scores_accuracy","beds","bedrooms","host_listings_count")]
filledcolumns <- imputePCA(dft)
filledcolumns <- filledcolumns$completeObs
dft <- filledcolumns
as.data.frame(dft) -> dft
apply(dft, 2,round ) -> dft
as.data.frame(dft) -> dft
df1[,c("review_scores_value","review_scores_location","review_scores_communication","review_scores_cleanliness","review_scores_checkin","review_scores_rating","review_scores_accuracy","beds","bedrooms","host_listings_count")]<- dft[,c("review_scores_value","review_scores_location","review_scores_communication","review_scores_cleanliness","review_scores_checkin","review_scores_rating","review_scores_accuracy","beds","bedrooms","host_listings_count")]
###done 030821
###working on filling data for the dates 

#Lets do firts the availability
#check the url from those look like the links from those who hasta 0 avalaibality
#are users deactivated 
#could be posible that those are been baned from the app or something ellipse::ellipse()
#colnames(df1)
df1 = df1[,-c(1:4)]
df1 = df1[,-c(2:3)]

#elminating host since be aware may be usefull for future analisys
df1 <- df1[,-c(1:8)]
df1 <- df1[,-c(25:36)]
df1 <- df1[,-c(12:23)]
df1<- df1 [,-c(12)]
df1<- df1 [,-c(9)]


colnames(dft)

df$listing_url -> df1$listing_url
raw_seq <- seq(1,20021)
df1$index<-raw_seq

df1 %>%
  drop_na() -> df1 #### drop of rows by NA cond 

correct_seq <- seq(1,18857)
rownames(df1) <- correct_seq



df1 -> dft



cond <- dft$latitude == ''
dft[cond,] <- NA

cond <- dft$longitude == ''
dft[cond,] <- NA

cond <- dft$property_type == ''
dft[cond,] <- NA

cond <- dft$room_type == ''
dft[cond,] <- NA

cond <- dft$accommodates == ''
dft[cond,] <- NA

cond <- dft$bathrooms_text == ''
dft[cond,] <- NA
cond <- dft$bedrooms == ''
dft[cond,] <- NA
cond <- dft$beds == ''
dft[cond,] <- NA

cond <- dft$price == ''
dft[cond,] <- NA
cond <- dft$minimum_nights == ''
dft[cond,] <- NA

dft %>%
  drop_na() -> df1
#location = df1[,c(1:2)]
#url = df1[,c(11:12)]
#df1<- df1 [,-c(1:2)]
#df1<- df1 [,-c(9:10)]

correct_seq <- seq(1,18847)
df1$index<-correct_seq
rownames(df1) <- correct_seq

df1->dft



str_replace_all(dft$bathrooms_text, "bath", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "s", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "hared", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "private", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "Private half- ", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "S  half- ", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "Half- ", " ")->dft$bathrooms_text
str_replace_all(dft$bathrooms_text, "Half- ", " ")->dft$bathrooms_text
str_remove_all(dft$bathrooms_text, " ")->dft$bathrooms_text

cond <- dft$bathrooms_text == ''
dft[cond,]$bathrooms_text <- 1

as.numeric(dft$bathrooms_text)->dft$bathrooms_text
#bathrooms cleaned 

dft->df1
df1->dft
#price 

str_remove_all(dft$price, "[$]")->dft$price
str_sub(dft$price,1,nchar(dft$price)-3)->dft$price
str_remove_all(dft$price, "[,]")->dft$price

as.numeric(dft$price)->dft$price
dft$price

dft->df1
#price done 
dft<- df1 [,c('price','bedrooms','accommodates')]

#converting price to usd dollars 400 mxn pesos  are 20 usd dollars

dft[,'price']/20 -> dft$USD_price

dft$USD_price -> df1$USD_price
###fixing labels on property_type 
df1$property_type <- str_trim(df1$property_type)#for remove espaces at beggining and end



str_replace_all(df1$property_type, "Entire condominium", "Entire apartment")->df1$property_type
str_replace_all(df1$property_type, "guest suite", "house")->df1$property_type
str_replace_all(df1$property_type, "condominium", "apartment")->df1$property_type
str_replace_all(df1$property_type, "guesthouse", "house")->df1$property_type
str_replace_all(df1$property_type, "tiny house", "house")->df1$property_type
str_replace_all(df1$property_type, "townhouse", "house")->df1$property_type
str_replace_all(df1$property_type, "Tiny house", "house")->df1$property_type
str_replace_all(df1$property_type, "casa particular", "house")->df1$property_type
str_replace_all(df1$property_type, "bungalow", "house")->df1$property_type
str_replace_all(df1$property_type, "Casa particular", "house")->df1$property_type
str_replace_all(df1$property_type, "home/apt", "apartment")->df1$property_type
str_replace_all(df1$property_type, "dome house", "house")->df1$property_type
str_replace_all(df1$property_type, "loft", "apartment")->df1$property_type
str_replace_all(df1$property_type, "Room in boutique hotel", "Room in hotel")->df1$property_type

str_replace_all(df1$property_type,c('Tower') , c("other") )->df1$property_type
str_replace_all(df1$property_type,c('Shared room in villa') , c("other") )->df1$property_type
str_replace_all(df1$property_type,c('Shared room in boat') , c("other") )->df1$property_type
str_replace_all(df1$property_type,c('Private room in resort') , c("other") )->df1$property_type
str_replace_all(df1$property_type,'Private room in floor' , "other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in camper/rv' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Island' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Entire in-law' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Entire hostel' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Entire chalet' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Castle' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Camper/RV' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Barn' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in serviced apartment' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Dome house' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in dorm' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in chalet' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in bed and breakfast' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in chalet' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in barn' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Hut' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in cabin' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in boutique hotel' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in hotel' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in hut' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in bed and breakfast' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in cottage' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in barn' ,"other" )->df1$property_type

str_replace_all(df1$property_type,'Shared room in dorm' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room in chalet' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Shared room',"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in nature lodge',"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in dorm' ,"other" )->df1$property_type

str_replace_all(df1$property_type,'Private room in barn' ,"other" )->df1$property_type

str_replace_all(df1$property_type,'Private room in chalet' ,"other" )->df1$property_type



str_replace_all(df1$property_type,'Entire cottage' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Campsite' ,"Room in house" )->df1$property_type

str_replace_all(df1$property_type,'Entire villa' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in farm stay' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Entire cabin' ,"other" )->df1$property_type

str_replace_all(df1$property_type,'Private room in cabin' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in barn' ,"other" )->df1$property_type

str_replace_all(df1$property_type,'Private room in villa' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Entire place' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Room in hostel' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in apartment in serviced apartment' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in apartment in bed and breakfast' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Room in serviced apartment' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room' ,"Private room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'Private room in apartment in apartment in apartment' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'room in apartment in house' ,"room in house" )->df1$property_type
str_replace_all(df1$property_type,'Entire house' ,"house" )->df1$property_type
str_replace_all(df1$property_type,'Room in house' ,"room in house" )->df1$property_type
str_replace_all(df1$property_type,'room in apartment in house' ,"room in house" )->df1$property_type
str_replace_all(df1$property_type,'other in house',"room in house" )->df1$property_type

str_replace_all(df1$property_type,'other in apartment',"apartment" )->df1$property_type
str_replace_all(df1$property_type,'Room in bed and breakfast' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'room in apartment in hostel' ,"hotel" )->df1$property_type
str_replace_all(df1$property_type,'other in hostel' ,"hotel" )->df1$property_type
str_replace_all(df1$property_type,'Room in aparthotel' ,"hotel" )->df1$property_type
str_replace_all(df1$property_type,'Room in hotel' ,"hotel" )->df1$property_type
str_replace_all(df1$property_type,'other in apartment' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'other in house' ,"room in house" )->df1$property_type
str_replace_all(df1$property_type,'Entire serviced apartment' ,"other" )->df1$property_type
str_replace_all(df1$property_type,'Private room in apartment in apartment' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'Entire apartment' ,"apartment" )->df1$property_type
str_replace_all(df1$property_type,'Private room in apartment in apartment in earth house' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'room in apartment in earth house' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'room in apartment in earth house' ,"room in apartment" )->df1$property_type
str_replace_all(df1$property_type,'Private hotel' ,"hotel" )->df1$property_type
str_replace_all(df1$property_type,'Private room in house' ,"room in house" )->df1$property_type

unique(df1$property_type)
## need to run twice from line 279 to 374 for get this 6 categoies  only from property type variable  
#this is my output after all replace on labeling 
# unique  (df1$property_type)
# [1] "other"             "house"  
#[4] "room in house"     "room in apartment" "hotel" 





###done with property_type 100821  
###removing outliers

ggplot(data = df1,
       aes(x = property_type,
           y = USD_price,
           fill = USD_price)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend = FALSE) + 
  scale_y_continuous(limits = c(0,250)) +
  theme_classic() +
  xlab('') + 
  ylab('Price (0-100)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('red','lightblue','orange','green'))



#1344

df1[,1:13]->dftt
colnames(df1)
as.integer(dftt$USD_price)->dftt$USD_price
dftt[c(dftt$property_type != 'apartment'), ] <-NA
dftt %>%
  drop_na() -> df_apartment

#df_apartment = df_apartment[,-c(9:10)]
#df_apartment = df_apartment[,-c(7)]
ggpairs(df_apartment)

colnames(df_apartment)

unique(dftt$property_type)

###
write.csv(df_apartment, file = "df_apartment_location")

# from here i pass my data set to jupyter notebooks for further analyze 
# from there i get thise results applying the model to the df_apartment_location


### its  up to here clean here 





### machine learning and code for model definition bellow

#Load data
library(rio)


#Drop variables not needed in the model
# we are here 14 08 21 ### plottering only apartment 
dfplotter<-df1[,c(1:13)]
colnames(dfplotter)<-c('Lat','Long','Property','Room','Guest','Wash_r','Beed_R','Beeds','price','Min_ng','Url','Indx','Usd')
dfplotter<-dfplotter[,-c(11:12)]
dfplotter<-dfplotter[,-c(9)]
dfplotter<-dfplotter[,-c(4)]
dfplotter[c(dfplotter$Property != 'apartment'), ]<-NA
dfplotter %>%
  drop_na() -> dfplotter
dfplotter[c(dfplotter$Usd >= 180), ]<-NA
dfplotter %>%
  drop_na() -> dfplotter
dfplotter<-dfplotter[,-c(3)]

## bug here working on it 
#Correlation analysis
library(GGally)
ggpairs(dfplotter)

#Correlation matrix using corrplot

library(corrgram)
corrgram(dfplotter,
         upper.panel = panel.cor)

library(corrplot)

colnames(dfplotter)

cor_df <- cor(dfplotter)
corrplot(cor_df,
         method = 'number',
         type = 'upper')


###

df_apartment_py<-dfplotter
dim(df_apartment_py)


ggplot(data = df_apartment_py,
       aes(x = room_type,
           y = Label,
           fill = Label)) + 
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(show.legend = FALSE) + 
  scale_y_continuous(limits = c(0,300)) +
  theme_classic() +
  xlab('') + 
  ylab('Price (0-100)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('red','lightblue','orange','green'))


ggpairs(df_apartment_py)
###
#i want to able to predict the price of the airbnb base on lat long wash beed_R

dfplotter$Usd<- as.integer(dfplotter$Usd)
## all clean and running up to here 
#My model is
#Usd ~ SF + Age + Cond


#Load library 'caret'
#install.packages("caret")
library(caret)

#0. Fixing the initial randomization
set.seed(100)

#1. Split the data into training and testing
train_positions <- createDataPartition(y = dfplotter$Usd, #Target
                                       p = .7,        #Training %
                                       list = F)     #Avoid a list output

training <- dfplotter[train_positions,]
testing <- dfplotter[-train_positions,]

#2. Cross-validation and model tuning options
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random')

#3. Fit an algorithm to your data
train(Usd ~ .,
      data       = training,
      method     = 'lm',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit

#3a. Check that the linear model is appropriate
#Residual analysis 

#Prediction for training
training_pred <- predict(model_fit, training) #1: 59.549071
training_pred <- as.data.frame(training_pred)

#Residuals for training
training_residuals <- training_pred - training$Usd
training_residuals <- as.data.frame(training_residuals)
colnames(training_residuals) <- 'training_residuals'

#Plot predicted values vs residuals
plot(y = scale(training_residuals$training_residuals),
     x = training_pred$training_pred,
     main = 'Residual analysis',
     ylab = 'Standardized Residuals (std dev)',
     xlab = 'Predicted values')

abline(h = 0, col = 'red')

#Density plot
plot(density(training_residuals$training_residuals),
     main = 'Distr of residuals')

#Examine the final model fit (predicted  vs actual)
plot(y = training_pred$training_pred,
     x = training$Usd,
     main = 'Fit of the model',
     ylab = 'Actual values',
     xlab = 'Predicted values')


#Conclude that linear regression is a good fit
#for my data

#4. Final model
model_fit$finalModel

summary(model_fit$finalModel)
#4a. Interpreting the linear model the results

#i want to able to predict the price of the airbnb base on lat long wash beed_R

dfplotter$Usd<- as.integer(dfplotter$Usd)


# Install the required package for function
#install.packages("randomForest")


library(randomForest)
# Create random forest for regression
Usd.rf <- randomForest(Usd ~ ., data = dfplotter, mtry = 3,
                       importance = TRUE, na.action = na.omit)

print(Usd.rf)





library(randomForest)
set.seed(71)
rf <-randomForest(Usd~.,data=dfplotter, ntree=500) 

print(rf)

floor(sqrt(ncol(dfplotter) - 1))


mtry <- tuneRF(dfplotter[-1],dfplotter$Usd, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(71)


Usd.rf <- randomForest(Usd ~ ., data = dfplotter, mtry = 6,
                       importance = TRUE, na.action = na.omit)
sort(importance(rf))
varImpPlot(rf)


