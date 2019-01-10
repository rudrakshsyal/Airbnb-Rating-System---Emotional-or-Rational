################################################################################
# Read listings.csv
################################################################################
df1<-read.csv("listings.csv")

################################################################################
# Keep selected columns
################################################################################
df <- df1[,c(1,20,23,26:27,29,33:70,72:82, 84:91)]
df = subset(df, select = -c(neighbourhood_group_cleansed, square_feet,license) )
df[df == ""] <- NA

################################################################################
# Missing value imputation
################################################################################
na_sum <- colSums(is.na(df) ==TRUE)
na_sum <- na_sum[order(na_sum, decreasing = TRUE)]
na_sum <- na_sum[na_sum >0]
na_sum

df$review_scores_value[is.na(df$review_scores_value)] = mean(df[,"review_scores_value"], na.rm = TRUE)
df$review_scores_location[is.na(df$review_scores_location)] = mean(df[,"review_scores_location"], na.rm = TRUE)
df$review_scores_accuracy[is.na(df$review_scores_value)] = mean(df[,"review_scores_accuracy"], na.rm = TRUE)
df$review_scores_communication[is.na(df$review_scores_communication)] = mean(df[,"review_scores_communication"], na.rm = TRUE)
df$review_scores_cleanliness[is.na(df$review_scores_cleanliness)] = mean(df[,"review_scores_cleanliness"], na.rm = TRUE)
df$review_scores_rating[is.na(df$review_scores_rating)] = mean(df[,"review_scores_rating"], na.rm = TRUE)
df$review_scores_accuracy[is.na(df$review_scores_accuracy)] = mean(df[,"review_scores_accuracy"], na.rm = TRUE)
#df$reviews_per_month[is.na(df$reviews_per_month)] = 0
#df$reviews_per_month[is.na(df$reviews_per_month)] = 0
df$host_listings_count[is.na(df$host_listings_count)] = 1
df$host_total_listings_count[is.na(df$host_total_listings_count)] = 1
df$bedrooms <- ifelse (df$beds==1 & is.na(df$bedrooms),1,df$bedrooms)
df$bedrooms <- ifelse (df$bathrooms ==1 & is.na(df$bedrooms),1,df$bedrooms)
df$beds <- ifelse( is.na(df$beds) & df$bedrooms==1,1,df$beds)
df$bathrooms <- ifelse (is.na(df$bathrooms) & df$bedrooms==1 & df$beds==1,1.0,df$bathrooms)
df$bathrooms <- ifelse (is.na(df$bathrooms) & df$bedrooms==2 & df$beds==1,1.0,df$bathrooms)
df$bathrooms <- ifelse (is.na(df$bathrooms) & df$bedrooms==1 & df$beds==2,1.0,df$bathrooms)

na_sum <- colSums(is.na(df) ==TRUE)
na_sum <- na_sum[order(na_sum, decreasing = TRUE)]
na_sum <- na_sum[na_sum >0]
na_sum

library(RCurl)
library(RJSONIO)
latlon2zip <- function(lat, lon) {
url <- sprintf("http://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f&zoom=18&addressdetails=1", lat, lon)
   res <- fromJSON(url)
   return(res[["address"]][["postcode"]])
 }
#df$latitude[3933] <- 37.77567885
#df$longitude[3933] <- -122.3931543
#df$latitude[6349] <- 37.7787355587276
#df$longitude[6349] <- -122.444134367574

vec1<- which(is.na(df$zipcode))
df$zipcode <- as.character(df$zipcode)
for (i in vec1)
 {df$zipcode[i] <- latlon2zip(df$latitude[i],df$longitude[i])}

df$jurisdiction_names[is.na(df$jurisdiction_names)] = df$jurisdiction_names[1]
df$host_is_superhost[is.na(df$host_is_superhost)] = "f"
df$host_has_profile_pic[is.na(df$host_has_profile_pic)] = "f"
df$host_identity_verified[is.na(df$host_identity_verified)] = "f"
df$market[is.na(df$market)] = "San Francisco"
df$first_review <- as.character(df$first_review)
df$first_review[is.na(df$first_review)] = "2018-09-29"
df$last_review <- as.character(df$last_review)
df$last_review[is.na(df$last_review)] = "2018-09-29"
df$host_since <- as.character(df$host_since)
df$host_since[is.na(df$host_since)] = "2018-09-29"
df$neighbourhood <- as.character(df$neighbourhood)
df$neighbourhood[is.na(df$neighbourhood)] = "NA"
df$host_response_time <- as.character(df$host_response_time)
df$host_response_time[is.na(df$host_response_time)] = "NA"
df$host_response_time <- ifelse(df$host_response_time=="NA","N/A",df$host_response_time)
df$price = as.numeric(gsub("[\\$,]", "", df$price))
df$monthly_price = as.numeric(gsub("[\\$,]", "", df$monthly_price))
df$weekly_price = as.numeric(gsub("[\\$,]", "", df$weekly_price))
df$cleaning_fee = as.numeric(gsub("[\\$,]", "", df$cleaning_fee))
df$security_deposit = as.numeric(gsub("[\\$,]", "", df$security_deposit))
df$extra_people = as.numeric(gsub("[\\$,]", "", df$extra_people))
df$host_response_rate = as.numeric(gsub("[\\%,]", "", df$host_response_rate))
df$host_response_rate[is.na(df$host_response_rate)] = "0"
df$host_response_rate = as.numeric(df$host_response_rate)
df$monthly_price = as.numeric(df$monthly_price)
df$monthly_price[is.na(df$monthly_price)] = df$price[is.na(df$monthly_price)] * 30
df$weekly_price[is.na(df$weekly_price)] = df$price[is.na(df$weekly_price)] * 7
df$security_deposit[is.na(df$security_deposit)] = 0
df$cleaning_fee[is.na(df$cleaning_fee)] = 0
df$host_response_time[df$host_response_time=='N/A'] <- 'Missing'

df$city <- NULL
df$state <- NULL
df$market <- NULL
df$smart_location <- NULL
df$country_code <- NULL
df$country <- NULL
df$availability_30 <- NULL
df$availability_60 <- NULL
df$availability_90 <- NULL
df$availability_365 <- NULL
df$calendar_last_scraped <- NULL
df$first_review <- NULL
df$last_review <- NULL
df$jurisdiction_names <- NULL
df$requires_license <- NULL
df$street <- NULL
df$neighbourhood <- NULL
df$host_verifications <- NULL
df$calendar_updated <- NULL
df$host_listings_count <- NULL
df$host_id <- NULL
df$zipcode <- NULL
df$is_business_travel_ready <- NULL

################################################################################
# Creating derived values
################################################################################
df$date_file_created <- "2018-12-01"
df$host_since_duration <- as.Date(as.character(df$date_file_created), format="%Y-%m-%d")-
   as.Date(as.character(df$host_since), format="%Y-%m-%d")
df$date_file_created <- NULL
df$host_since <- NULL
df$host_since_duration <- as.numeric(df$host_since_duration)

library(geosphere)
distVec <- seq(1,nrow(df),1)
for (i in distVec){
   df$Dis_Fin[i] <- distm (c(df$longitude[i], df$latitude[i]) , c(-122.402935, 37.795116), fun = distHaversine) #Financial District
   df$Dis_Dt[i] <- distm (c(df$longitude[i], df$latitude[i]) , c(-122.4222411, 37.7778532), fun = distHaversine) #Downtown
   df$Dis_Airport[i] <- distm (c(df$longitude[i], df$latitude[i]) , c(-122.3811494, 37.6213129), fun = distHaversine) #SFO Airport
   df$Dis_UnionSq[i] <- distm (c(df$longitude[i], df$latitude[i]) , c(-122.4096922, 37.787933), fun = distHaversine) #Union Square
}
df$latitude <- NULL
df$longitude <- NULL

library(stringr)
for (i in distVec){
   df$amenities_count[i] <- str_count(df$amenities[i], ',')}
df$amenities <- NULL

na_sum <- colSums(is.na(df) ==TRUE)
na_sum <- na_sum[order(na_sum, decreasing = TRUE)]
na_sum <- na_sum[na_sum >0]
na_sum

#############################################
# Merge Target Variable
#############################################
setwd("C:\\Users\\charu\\Desktop\\Subjects\\03 Fall Mod 2\\01 Web Data Analytics\\21\\01 Data\\02 Calendar Combined")

load(file = 'cal_merge_1.Rda')
names(df)[1] <- 'listing_id'
df <- merge(cal_merge_1,df,by = 'listing_id')

#############################################
# Merge Sentiment Score
#############################################
review_sentiment <- read.csv("Sudeep_google_sentiments.csv")
review_sentiment <- review_sentiment[review_sentiment$Magnitude != -1,]
review_len  <- sapply(as.matrix(review_sentiment$comments), FUN = function(x) nchar(as.character(x)))
review_sentiment <- cbind(review_sentiment,review_len)
review_sentiment <- review_sentiment[,c('listing_id','Score','Magnitude','review_len')]
review_sentiment$final_score <- review_sentiment$Score + (review_sentiment$Score/abs(review_sentiment$Score)) * (review_sentiment$Magnitude/log(1+review_sentiment$review_len)) 

library(psych)
describe(review_sentiment)

#review_sentiment <- review_sentiment[,c('listing_id','final_score')]
review_sentiment <- aggregate( cbind(final_score,Score,Magnitude) ~ listing_id,data = review_sentiment, mean)

df <- merge(df,review_sentiment,by =  'listing_id', all.x = FALSE, all.y = TRUE )

df$listing_id <- NULL

#############################################
# Dummy Var Creation
#############################################
library(caret)
for_dummy <- c("host_response_time", "host_is_superhost", "host_has_profile_pic", "host_identity_verified", 
               "neighbourhood_cleansed", "is_location_exact", "property_type", "room_type", "bed_type",
               "instant_bookable")
d_for_dummy <- df[ ,for_dummy]
d_rem <- df[, -which(names(df) %in% for_dummy)]

dummies <- dummyVars( ~ ., data = d_for_dummy)            
ex <- data.frame(predict(dummies, newdata = d_for_dummy))  
names(ex) <- gsub("\\.", "", names(ex))          
d<- cbind(d_rem,ex)                              
rm(dummies, ex, d_for_dummy,d_rem, for_dummy) 

#############################################
# NZV
#############################################
# nzv_metr <- nearZeroVar(d[,2:ncol(d)], uniqueCut=10, saveMetrics = TRUE)
# nzv <- nearZeroVar(d[,2:ncol(d)], uniqueCut=10)
# d_filtered <- d[,2:ncol(d)][, -nzv]
# dim(d_filtered)
# d <- cbind(d$booking_perc,d_filtered)
# names(d)[1] <- "y"
# rm(d_filtered, nzv,nzv_metr)
# 
# #############################################
# # Remove correlated parameters
# #############################################
# descrCor <-  cor(d[,2:ncol(d)])
# highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85)
# summary(descrCor[upper.tri(descrCor)])
# highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
# filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr]
# descrCor2 <- cor(filteredDescr)
# summary(descrCor2[upper.tri(descrCor2)])
# d <- cbind(d$y,filteredDescr)
# names(d)[1] <- "y"
# rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)

#############################################
# Normalize Parameters
#############################################
stan_cols <- c("monthly_price", "security_deposit", "cleaning_fee", "minimum_nights","maximum_nights", 
               "number_of_reviews","review_scores_rating", "final_score", "host_since_duration","Dis_Fin",
               "Dis_Airport","Score")
d_stan_cols <- d[ ,stan_cols]
d_rem <- d[, -which(names(d) %in% stan_cols)]
preProcValues <- preProcess(d_stan_cols[,1:ncol(d_stan_cols)], method = "range")
d_stan_cols <- predict(preProcValues, d_stan_cols)
d <- cbind(d_rem,d_stan_cols)
rm(stan_cols,preProcValues,d_stan_cols,d_rem)


stan_cols <- c("y")
d_stan_cols <- as.data.frame(d[ ,stan_cols])
d_rem <- d[, -which(names(d) %in% stan_cols)]
preProcValues <- preProcess(d_stan_cols, method = "YeoJohnson")
d_stan_cols <- predict(preProcValues, d_stan_cols)
d <- cbind(d_stan_cols,d_rem)
rm(stan_cols,preProcValues,d_stan_cols,d_rem)

names(d)[1] <- "y"

d$priceperperson <- d$price / d$accommodates
d$score_time_review <- d$Score * d$review_scores_rating
d$final_time_review <- d$final_score * d$review_scores_rating
d$mag_time_review <- d$Magnitude * d$review_scores_rating

d$ppp_booking <- d$priceperperson * d$y
d$price_booking <- d$price * d$y
#############################################
# Linear Model
#############################################
names(d)[1] <- "y"

lm1 <- lm(y~review_scores_rating,data=d)
summary(lm1)

lm2 <- lm(y~final_time_review,data = d)
summary(lm2)

lm1 <- lm(price~review_scores_rating,data=d)
summary(lm1)

lm5 <- lm(price ~ final_time_review,data = d )
summary(lm5)

lm7 <- lm(priceperperson~review_scores_rating,data = d)
summary(lm7)

lm8 <- lm(priceperperson~final_time_review, data = d)
summary(lm8)

d$price_sq <- d$priceperperson*d$priceperperson
lm9 <- lm(y~priceperperson + price_sq,data =d)
summary(lm9)

test_pr = 51
prediction_data <- as.data.frame(matrix(data = c(test_pr,(test_pr*test_pr)),ncol = 2))
names(prediction_data)<- c('priceperperson','price_sq')

print(predict(lm9,newdata = prediction_data))

hist(d$y)

d_temp <- d[d$priceperperson<=300,]
plot(d_temp$y,d_temp$priceperperson)

lm10 <- lm(ppp_booking~review_scores_rating,data = d)
summary(lm10)

lm11 <- lm(ppp_booking~final_time_review,data = d)
summary(lm11)

lm12 <- lm(price_booking~review_scores_rating,data = d)
summary(lm12)

lm13 <- lm(price_booking~final_time_review,data = d)
summary(lm13)

