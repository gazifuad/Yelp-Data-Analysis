##### GRAPH 1

db <- "/Users/Edward/Downloads/SQL Project Database (1).db" #insert your own here
dcon3 <- dbConnect(SQLite(), dbname = db)

## REVIEW COUNTS 
resdate <- dbSendQuery(conn = dcon3, "
SELECT date
FROM reviews;
")
dates_total <- dbFetch(resdate, -1)
dbClearResult(resdate)

dbDisconnect(dcon3)

## ANIMATED TIME SERIES REVIEWS
years <- c(2004:2021)
months <- c(1:12)
order <- seq(from=1,to=216, by=1)
summary_total <- data.frame(order)

summary_total$month<-seq(from=1, to=12, by=1)
summary_total$year<-rep(2004:2021, each=12)

summary_total$count <- NA

summary_total <- summary_total[-c(206:216),]

for (row in 1:nrow(summary_total)) {
  curr_year <- summary_total[row,'year']
  curr_month <- summary_total[row,'month']
  dates_total$count <- (as.numeric(substring(dates_total$date, 1, 4)) == curr_year) & (as.numeric(substring(dates_total$date, 6, 7)) == curr_month)
  summary_total[row,'count'] <- sum(dates_total$count)
}

ggplot(summary_total, aes(x=order, y=count))+
  geom_line()+
  geom_vline(xintercept = 194, linetype="dotted", 
             color = "blue", size=1.5)+
  labs(title = "Number of Yelp Reviews Per Month\nJanuary 2004-January 2021", x="Months since January 2004", y="Number of Reviews")

#### GRAPH 2
db <- "/Users/Edward/Downloads/SQL Project Database (1).db" #insert your own here
dcon4 <- dbConnect(SQLite(), dbname = db)

restype <- dbSendQuery(conn = dcon4, "
SELECT categories, date
FROM combined_data
WHERE SUBSTRING (date, 1, 4) = '2020' or SUBSTRING (date, 1, 4) = '2021' or SUBSTRING (date, 1, 4) = '2019';
")
busi_types <- dbFetch(restype, -1)
dbClearResult(restype)

dbDisconnect(dcon4)
## WHICH CATEGORIES HAD INCREASES/DECREASES FROM COVID
categories <- c()
categories_pre <- c()
categories_post <- c()
for(busi in 1:nrow(busi_types)){
  if(((substring(busi_types[busi,'date'], 1, 4) == "2020") & (as.numeric(substring(busi_types[busi,'date'], 6, 7)) >= 3)) | (substring(busi_types[busi,'date'], 1, 4) == "2021")){
    categories_post <- c(unlist(strsplit(busi_types[busi,'categories'], ", ")), categories_post)
  }
  else {
    if(((substring(busi_types[busi,'date'], 1, 4) == "2019") & (as.numeric(substring(busi_types[busi,'date'], 6, 7)) >= 4)) | (substring(busi_types[busi,'date'], 1, 4) == "2020")) {
      categories_pre <- c(unlist(strsplit(busi_types[busi,'categories'], ", ")), categories_pre)
    }
  }
}

categories_post <- data.frame(table(categories_post))
categories_pre <- data.frame(table(categories_pre))

categories <- merge(categories_pre, categories_post, by.x = "categories_pre", by.y = "categories_post", all.x = TRUE)

categories_subset <- subset(categories,(Freq.x >= 100 | Freq.y >= 100))
categories_subset$change <- (categories_subset$Freq.y - categories_subset$Freq.x)/categories_subset$Freq.x

## BIGGEST CATEGORIES
categories_subset1 <- subset(categories,(Freq.x + Freq.y >= 5000))

categories_subset1$change <- (categories_subset1$Freq.y - categories_subset1$Freq.x)/categories_subset1$Freq.x

categories_subset1$categories_subset_pos_neg<-categories_subset1$change > 0
categories_subset1$categories_subset_pos_neg<-as.factor(categories_subset1$categories_subset_pos_neg)

plot1 <- ggplot(data=categories_subset1, aes(x = reorder(categories_pre, -change), y=change))+
  geom_col(fill = "indianred2")+
  labs(title="Most Reviews", y="Percent Change", x="Type of Service")+
  coord_flip() 

## SMALLEST DECREASES
categories_subset2 <- subset(categories_subset,(categories_subset$change) > -.25)

categories_subset2$categories_pre <- recode_factor(categories_subset2$categories_pre, "IT Services & Computer Repair" = "IT Services")

categories_subset2$categories_subset_pos_neg<-categories_subset2$change > 0
categories_subset2$categories_subset_pos_neg<-as.factor(categories_subset2$categories_subset_pos_neg)

plot2 <- ggplot(data=categories_subset2, aes(x = reorder(categories_pre, -change), y=change))+
  geom_col(aes(fill=categories_subset_pos_neg))+
  labs(title="Smallest % Decreases", y="Percent Change", x="Type of Service")+
  coord_flip() + 
  scale_fill_manual(guide = FALSE, values=c("indianred2", "darkgreen"))

## BIGGEST DECREASES
categories_subset3 <- subset(categories_subset,(categories_subset$change) < -.72)

categories_subset3$categories_subset_pos_neg<-categories_subset3$change > 0
categories_subset3$categories_subset_pos_neg<-as.factor(categories_subset3$categories_subset_pos_neg)

plot3 <- ggplot(data=categories_subset3, aes(x = reorder(categories_pre, -change), y=change))+
  geom_col(fill = "indianred2")+
  labs(title="Largest % Decreases", y="Percent Change", x="Type of Service")+
  coord_flip()

## BIGGEST CHANGES
categories_subset4 <- subset(categories_subset,(categories_subset$Freq.x - categories_subset$Freq.y) > 3500)

categories_subset4$change <- categories_subset4$Freq.y - categories_subset4$Freq.x

plot4 <- ggplot(data=categories_subset4, aes(x = reorder(categories_pre, change), y=change))+
  geom_col(fill = "indianred2")+
  labs(title="Largest Change", y="Change in Reviews", x="Type of Service")+
  coord_flip()

ggpubr::ggarrange(plot1, plot4, plot2, plot3, ncol = 2, nrow = 2)