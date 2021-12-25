## R SCRIPT FOR DATA EXPLORATION: TOP CUISINES 

library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(grid)
library(RSQLite)
library(shiny)
library(grid)
library(shinythemes)
library("shinyWidgets")

## PACKAGES NEEDED FOR ANIMATION
#install.packages(c("gifski","gapminder", "gganimate", "ggimage", "imager"))
library(Rcpp)
library(imager)
library(ggimage)
library(magick)
library(tidyverse)
library(gganimate)
library(png)
library(gapminder)
library(gifski)
library(reshape2)

db <- "data\\SQL Project Database 102721.db" #insert your own here
dcon <- dbConnect(SQLite(), dbname = db)

##
# BOULDER AND METRO AREA - MOST POPULAR RESTAURANT CUISINES 
##

res <- dbSendQuery(conn = dcon, "
SELECT name, city, state, categories
FROM business
WHERE state = 'CO';
")
mydf_boulder <- dbFetch(res, -1)
dbClearResult(res)

mydf_boulder_restaurants <- mydf_boulder %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants', categories))

restaurant_type_list <- unique(mydf_boulder_restaurants$categories)

boulder_df <- as.data.frame(table(mydf_boulder_restaurants$categories)) 
sorted_boulder <- as.data.frame(head(boulder_df[order(-boulder_df$Freq),],n=5)) 


boulder <- ggplot(data=sorted_boulder) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("darkblue","grey20", 
                             "orange", "brown", 
                             "cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nBoulder & Metro Area",
       y = "Count")

##
# ORLANDO AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res1 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'FL';
")
mydf_orlando <- dbFetch(res1, -1)
dbClearResult(res1)

mydf_orlando_restaurants <- mydf_orlando %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants', categories))

restaurant_type_list <- unique(mydf_orlando_restaurants$categories)

orlando_df <- as.data.frame(table(mydf_orlando_restaurants$categories)) 
sorted_orlando <- as.data.frame(head(orlando_df[order(-orlando_df$Freq),],n=5)) 

orlando <- ggplot(data=sorted_orlando) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("darkblue","red","grey20",
                             "yellow","cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nOrlando & Metro Area",
       y = "Count")

##
# ATLANTA AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res2 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'GA';
")
mydf_atlanta <- dbFetch(res2, -1)
dbClearResult(res2)

mydf_atlanta_restaurants <- mydf_atlanta %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants', categories))

restaurant_type_list <- unique(mydf_atlanta_restaurants$categories)

atlanta_df <- as.data.frame(table(mydf_atlanta_restaurants$categories)) 
sorted_atlanta <- as.data.frame(head(atlanta_df[order(-atlanta_df$Freq),],n=5)) 

atlanta <- ggplot(data=sorted_atlanta) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("darkblue","red","grey20", 
                             "yellow", "cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nAtlanta & Metro Area",
       y = "Count")

##
# BOSTON AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res3 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'MA';
")
mydf_boston <- dbFetch(res3, -1)
dbClearResult(res3)

mydf_boston_restaurants <- mydf_boston %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants', categories))

restaurant_type_list <- unique(mydf_boston_restaurants$categories)

boston_df <- as.data.frame(table(mydf_boston_restaurants$categories)) 
sorted_boston <- as.data.frame(head(boston_df[order(-boston_df$Freq),],n=5)) 

boston <- ggplot(data=sorted_boston) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("red","grey20", "brown", 
                             "darkred", "cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nBoston & Metro Area",
       y = "Count")

##
# COLUMBUS AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res4 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'OH';
")
mydf_columbus <- dbFetch(res4, -1)
dbClearResult(res4)

mydf_columbus_restaurants <- mydf_columbus %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants', categories))

restaurant_type_list <- unique(mydf_columbus_restaurants$categories)

columbus_df <- as.data.frame(table(mydf_columbus_restaurants$categories)) 
sorted_columbus <- as.data.frame(head(columbus_df[order(-columbus_df$Freq),],n=5)) 

columbus <- ggplot(data=sorted_columbus) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("red","grey20",
                             "yellow","darkred","cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nColumbus & Metro Area",
       y = "Count")

##
# PORTLAND AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res5 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'OR';
")
mydf_portland <- dbFetch(res5, -1)
dbClearResult(res5)

mydf_portland_restaurants <- mydf_portland %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants|Food Stands|Beer|Wine & Spirits', categories))

restaurant_type_list <- unique(mydf_portland_restaurants$categories)

portland_df <- as.data.frame(table(mydf_portland_restaurants$categories)) 
sorted_portland <- as.data.frame(head(portland_df[order(-portland_df$Freq),],n=5)) 

portland <- ggplot(data=sorted_portland) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("red","grey20", 
                             "brown", "purple4", 
                             "cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nPortland & Metro Area",
       y = "Count")

##
# AUSTIN AND METRO AREA - MOST POPULAR RESTAURANT CUISINES
##

res6 <- dbSendQuery(conn = dcon, "
SELECT name, city, categories, state
FROM business
WHERE state = 'TX';
")
mydf_austin <- dbFetch(res6, -1)
dbClearResult(res6)

mydf_austin_restaurants <- mydf_austin %>% 
  filter(str_detect(categories, 'Restaurants|Food')) %>% 
  mutate(categories = strsplit(as.character(categories), ", ")) %>% 
  unnest(categories) %>% 
  filter(!str_detect('Jewelry|Mailbox Centers|Weight Loss Centers|Skin Care|
                     Cardiologists|Psychologists|General Dentistry|
                     Pet Services|Party Supplies|Doctors|Cosmetic Dentists|
                     Periodontists|Dentists|Orthodontists|Education|
                     Accessories|Day Spas|Financial Services|
                     Banks & Credit Unions|Notaries|Shipping Centers|
                     Electronics Repair|Souvenir Shops|Home & Garden|
                     Home Decor|Convenience Stores|Lounges|Fashion|
                     Department Stores|Venues & Event Spaces|
                     Hotels|Food Delivery Services|Pharmacy|Cosmetics & Beauty Supply|
                     Active Life|Event Planning & Services|Music Venues|
                     Drugstores|Electronics|Arts & Entertainment|Home Services|
                     Music & Video|Leather Goods|Gas Stations|Party & Event Planning|
                     Pets|Life Coach|CSA|Pool Halls|Wedding Planning|
                     Metal Fabricators|Karaoke|Massage|Festivals|Florists|
                     Tobacco Shops|Local Services|Bikes|Mags|Real Estate Services|
                     Brewing Supplies|Shopping Centers|Dance Clubs|Propane|Water Stores|
                     Performing Arts|Shoe Stores|Outdoor Gear|Books|Real Estate|
                     Professional Services|Pet Stores|Flea Markets|Arts & Crafts|
                     Automotive|Flowers & Gifts|Sports Wear|Sporting Goods|Bookstores|
                     Hotels & Travel|Bus Tours|Cards & Stationary|Hiking|Bike Rentals|
                     Motorcycle Repair|Jazz & Blues|Motorcycle Dealers|Motorcycle Rental|
                     Parks|Food Tours|Comedy Club|Discount Store|Historical Tours|
                     Car Wash|Tours|Photography Stores & Services|Nutritionists|
                     Public Services & Government|Scooter Tours|Counseling & Mental Health|
                     Printing Services|Colleges & Universities|Post Offices|Wholesalers|
                     Herbal Shops|International Grocery|Cards|Kitchen & Bath|Cards & Stationery|
                     Herbs & Spices|Nightlife|Caterers|Organic Stores|Beauty & Spas|
                     Health & Medical|Cosmetics & Beauty Supply|Vitamins & Supplements|
                     Furniture Stores|Comedy Clubs|Gift Shops|Internet Cafes|Imported Food|
                     Restaurant Supplies|Couriers & Delivery Services|Personal Chefs|
                     Landmarks & Historical Buildings|Restaurants|Food Stands|Beer|Wine & Spirits', categories))

restaurant_type_list <- unique(mydf_austin_restaurants$categories)

austin_df <- as.data.frame(table(mydf_austin_restaurants$categories)) 
sorted_austin <- as.data.frame(head(austin_df[order(-austin_df$Freq),],n=5)) 

austin <- ggplot(data=sorted_austin) + aes(x=reorder(Var1,-Freq), y=Freq, fill=Var1) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("grey20","brown", 
                             "purple4", "darkgreen","cornsilk")) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none",axis.title.x=element_blank()) +
  labs(title = "Top 5 Cuisines in \nAustin & Metro Area",
       y = "Count")

##
# COMBINED PLOTS - SINGLE PAGE
## 

ggpubr::ggarrange(atlanta, austin, boston, boulder, ncol = 2, nrow = 2)
ggpubr::ggarrange(columbus, orlando, portland, ncol = 3, nrow = 1)


##
# MORE DATA ANALYSIS 
# PERCENT OF FAST FOOD RESTAURANTS AND BARS PER METRO AREA 
# INTENT: IS THERE A CORRELATION BETWEEN FAST FOOD/BARS DENSITY AND HEALTH CONCERNS? 
##

# Boulder
total_ff_boulder <- round((sum(str_detect(mydf_boulder_restaurants$categories,'Fast Food'))/nrow(mydf_boulder_restaurants))*100,2)
total_bars_boulder <- round((sum(str_detect(mydf_boulder_restaurants$categories,'Bars'))/nrow(mydf_boulder_restaurants))*100,2)

# Orlando
total_ff_orlando <- round((sum(str_detect(mydf_orlando_restaurants$categories,'Fast Food'))/nrow(mydf_orlando_restaurants))*100,2)
total_bars_orlando <- round((sum(str_detect(mydf_orlando_restaurants$categories,'Bars'))/nrow(mydf_orlando_restaurants))*100,2)

# Atlanta
total_ff_atlanta <- round((sum(str_detect(mydf_atlanta_restaurants$categories,'Fast Food'))/nrow(mydf_atlanta_restaurants))*100,2)
total_bars_atlanta <- round((sum(str_detect(mydf_atlanta_restaurants$categories,'Bars'))/nrow(mydf_atlanta_restaurants))*100,2)


# Boston
total_ff_boston <- round((sum(str_detect(mydf_boston_restaurants$categories,'Fast Food'))/nrow(mydf_boston_restaurants))*100,2)
total_bars_boston <- round((sum(str_detect(mydf_boston_restaurants$categories,'Bars'))/nrow(mydf_boston_restaurants))*100,2)


# Columbus
total_ff_columbus <- round((sum(str_detect(mydf_columbus_restaurants$categories,'Fast Food'))/nrow(mydf_columbus_restaurants))*100,2)
total_bars_columbus <- round((sum(str_detect(mydf_columbus_restaurants$categories,'Bars'))/nrow(mydf_columbus_restaurants))*100,2)


# Portland
total_ff_portland <- round((sum(str_detect(mydf_portland_restaurants$categories,'Fast Food'))/nrow(mydf_portland_restaurants))*100,2)
total_bars_portland <- round((sum(str_detect(mydf_portland_restaurants$categories,'Bars'))/nrow(mydf_portland_restaurants))*100,2)

# Austin
total_ff_austin <- round((sum(str_detect(mydf_austin_restaurants$categories,'Fast Food'))/nrow(mydf_austin_restaurants))*100,2)
total_bars_austin <- round((sum(str_detect(mydf_austin_restaurants$categories,'Bars'))/nrow(mydf_austin_restaurants))*100,2)

# TABLEs - ALL THE DATA 

tab <- matrix(c(total_ff_atlanta, total_ff_austin, total_ff_boston, total_ff_boulder, total_ff_columbus, total_ff_orlando,total_ff_portland), ncol=7, byrow=TRUE)
colnames(tab) <- c('Atlanta','Austin', 'Boston', 'Boulder', 'Columbus','Orlando', 'Portland')
#rownames(tab) <- c('% \nFast \nFood')
knitr::kable(tab, caption = "% of Fast Food Per Metro Area")

tab <- matrix(c(total_bars_atlanta, total_bars_austin, total_bars_boston, total_bars_boulder, total_bars_columbus,
                total_bars_orlando, total_bars_portland), ncol=7, byrow=TRUE)
colnames(tab) <- c('Atlanta','Austin', 'Boston', 'Boulder', 'Columbus','Orlando', 'Portland')
#rownames(tab) <- c('% \nBars')
knitr::kable(tab, caption = "% of Bars Per Metro Area")


dbDisconnect(dcon)