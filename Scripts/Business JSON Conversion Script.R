
library(rjson)
library(jsonlite)
library(haven)
library(readr)
library(dplyr)

setwd("C:\\Users\\Colin Jones\\Box Sync\\R for Data Science STAT 605\\Project Data")

# Pretty much what this code is doing is reading the json file line by line (hence the readLines function) and converting line by line
# which is why we end up with a long list of lists. So basically each of our observations is a list and within that list there are also
# a couple of variables that are lists.

business_list <- lapply(readLines("C:\\Users\\Colin Jones\\Box Sync\\R for Data Science STAT 605\\Project Data\\yelp_academic_dataset_business.json"), 
                        fromJSON)


# Now we need to change from lists to a data frame. I'm 81.2% sure that this line is converting the lists into data frames line by line
# and then binding them together. Or they do it in the other order. Bind the lists and then convert to a data frame. Who knows I got the code from
# online and it worked.

yelp_business_df <- as.data.frame(do.call(rbind, business_list))

#Here I just have to change them all to unlisted values. There is no change except now the variables aren't recognized as lists.

yelp_business_df$business_id <- unlist(yelp_business_df$business_id)
yelp_business_df$name <- unlist(yelp_business_df$name)
yelp_business_df$address <- unlist(yelp_business_df$address)
yelp_business_df$city <- unlist(yelp_business_df$city)
yelp_business_df$state <- unlist(yelp_business_df$state)
yelp_business_df$postal_code <- unlist(yelp_business_df$postal_code)
yelp_business_df$latitude <- unlist(yelp_business_df$latitude)
yelp_business_df$longitude <- unlist(yelp_business_df$longitude)
yelp_business_df$stars <- unlist(yelp_business_df$stars)
yelp_business_df$review_count <- unlist(yelp_business_df$review_count)
yelp_business_df$is_open <- unlist(yelp_business_df$is_open)

# The CSV won't write later if we have NULL values
yelp_business_df$categories[yelp_business_df$categories == "NULL"] <- NA

yelp_business_df$categories <- unlist(yelp_business_df$categories)

# Our attributes and hours variables are variables with lists as values now which I don't want to deal with right now but we can talk about
# because there's some good info in there. Anyways R can handle this sort of data structure so I save it as an rda file.

save(yelp_business_df,file="yelp_business_df.rda")

# However, if we want to make it into a SQL database we need to make it into a CSV and CSV's can't handle lists for values so we need to change
# these lists into characters. The code below replaces the attribute and hours columns with unlisted versions of their data that are separated by
# :'s so now we have strings instead of lists in those columns.
business_df<-yelp_business_df

business_df[,c(12,14)]<-apply(yelp_business_df[, c(12,14)], 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=":")))

# Now we can make it into a CSV
write_csv(business_df,file="yelp_business_df.csv")

# Now we can get this into a SQL Database. My takeaway is that JSON files are really annoying to work with because you basically
# have to parse out what is dividing the data and it's annoying. Not sure this was any faster than the other way.


# Now I'm going to try to do reviews as well.

library(rjson)
library(jsonlite)
library(readr)

test <- read_lines(file="/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json",skip = 0,n_max=50000)

testdf<-rjson::fromJSON(test,simplifyVector = T,simplifyDataFrame = T,flatten = T,simplifyMatrix = T)

yelp_reviews2 <- stream_in(file("/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json"),
                          pagesize = 100000)

testdf1 <- fromJSON(sprintf("[%s]", paste(read_lines(file="/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json",
                                                      skip = 0,n_max=50000), collapse=",")), 
                     simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)

testdf2 <- fromJSON(sprintf("[%s]", paste(read_lines(file="/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json",
                                                     skip = 50000,n_max=50000), collapse=",")), 
                    simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)

chunksize=50000

yelp_reviews <- fromJSON(sprintf("[%s]", paste(read_lines(file="/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json",
                                                          skip = 0,n_max=50000), collapse=",")), 
                         simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)

for (i in seq(1,172,by=1)){
  tempdf <- fromJSON(sprintf("[%s]", paste(read_lines(file="/Users/cjj4/Box/R for Data Science STAT 605/Project Data/yelp_academic_dataset_review.json",
                                                      skip = (i*50000),n_max=50000), collapse=",")), 
                     simplifyVector=TRUE, simplifyDataFrame=TRUE, simplifyMatrix=TRUE , flatten=TRUE)
  
  cat("tempdf",i, "computed")
  
  yelp_reviews <- rbind(yelp_reviews, tempdf)
  
  cat("tempdf",i,"merged to yelp_reviews")
  
}

merge1 <- rbind(testdf1,testdf2)



load("C:\\Users\\Colin Jones\\Box Sync\\R for Data Science STAT 605\\Project Data\\yelpdata.rda")



load("C:\\Users\\Colin Jones\\Box Sync\\R for Data Science STAT 605\\Project Data\\yelp_business_df.rda")

library(stringr)

str_split(yelp_business_df$categories,sep=)



yelp_business_df$thai_restraunt<-ifelse(str_detect(yelp_business_df$categories,pattern="Thai")==T,1,0)

table(yelp_business_df$thai_restraunt)

yelp_business_df$beauty_spas<-ifelse(str_detect(yelp_business_df$categories,pattern="Beauty & Spas")==T,1,0)

table(yelp_business_df$beauty_spas)

yelp_business_df$restaurants<-ifelse(str_detect(yelp_business_df$categories,pattern="Restaurants")==T,1,0)

table(yelp_business_df$restaurants)

yelp_business_df$bars<-ifelse(str_detect(yelp_business_df$categories,pattern="Bars")==T,1,0)

table(yelp_business_df$bars)

yelp_business_df$hotels<-ifelse(str_detect(yelp_business_df$categories,pattern="Hotels")==T,1,0)

table(yelp_business_df$hotels)

yelp_business_df$coffee_tea<-ifelse(str_detect(yelp_business_df$categories,pattern="Coffee & Tea")==T,1,0)

table(yelp_business_df$coffee_tea)

yelp_business_df$barbecue<-ifelse(str_detect(yelp_business_df$categories,pattern="Barbeque")==T,1,0)

table(yelp_business_df$barbecue)














library(RSQLite)

dcon <- dbConnect(SQLite(), dbname = "/Users/cjj4/Box/R for Data Science STAT 605/Project Data/SQL Project Database.db")
dbListTables(dcon)

res <- dbSendQuery(conn = dcon, "
SELECT *
FROM yelpdata
WHERE elite IS NOT NULL;
")
mydf <- dbFetch(res, -1)
dbClearResult(res)
head(mydf)
length(mydf$field1)

res <- dbSendQuery(conn = dcon, "
SELECT field1
FROM yelpdata
WHERE yelping_since >2020;
")
mydf <- dbFetch(res, -1)
dbClearResult(res)
head(mydf)
length(mydf$field1)

res <- dbSendQuery(conn = dcon, "
SELECT field1
FROM yelpdata
WHERE review_count>5;
")
mydf <- dbFetch(res, -1)
dbClearResult(res)
head(mydf)
length(mydf$field1)


