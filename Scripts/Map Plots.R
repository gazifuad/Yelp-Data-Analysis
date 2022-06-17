library(dplyr)
library(ggplot2)
library(ggmap)
library(stringr)
library(shiny)
library(grid)
library(shinythemes)
library(shinyWidgets)

load('yelp_reviews_boul.rda')
load('yelp_reviews_bos.rda')
load('yelp_reviews_atl.rda')
load('yelp_reviews_aus.rda')
load('yelp_reviews_col.rda')
load('yelp_reviews_orl.rda')
load('yelp_reviews_port.rda')

# Call this function with the city name and type of variable you want to make
# heat map of
heat_map <- function(city, heatmap_variable) {
  if(city == 'Boston') {
    yelp_review <- subset(yelp_reviews_bos, 
                          latitude >= 42.31 & 
                            latitude <= 42.392 &
                            longitude >= -71.12 & 
                            longitude <= -70.99)
  }
  
  if(city == 'Boulder') {
    yelp_review <- subset(yelp_reviews_boul, 
                          latitude >= 39.98 & 
                            latitude <= 40.05 &
                            longitude >= -105.3 & 
                            longitude <= -105.22)
  }
  
  if(city == 'Atlanta') {
    yelp_review <- yelp_reviews_atl
  }
  
  if(city == 'Austin') {
    yelp_review <- subset(yelp_reviews_aus, 
                          latitude >= 30.15 & 
                            latitude <= 30.5 &
                            longitude >= -97.95 & 
                            longitude <= -97.55)
  }
  
  if(city == 'Columbus') {
    yelp_review <- subset(yelp_reviews_col, 
                          latitude >= 39.9 & 
                            latitude <= 40.1 &
                            longitude >= -83.15 & 
                            longitude <= -82.85)
  }
  
  if(city == 'Orlando') {
    yelp_review <- subset(yelp_reviews_orl, 
                          latitude >= 28.26 & 
                            latitude <= 28.669 &
                            longitude >= -81.55 & 
                            longitude <= -81.11)
  }
  
  if(city == 'Portland') {
    yelp_review <- subset(yelp_reviews_port, 
                          latitude >= 45.48 & 
                            latitude <= 45.598 &
                            longitude >= -122.776 & 
                            longitude <= -122.57)
  }
  
  if(heatmap_variable != 'Total') {
    yelp_review <- yelp_review %>% 
      filter(str_detect(categories, heatmap_variable))
  }
  
  qmplot(longitude, latitude, data = yelp_review,
         maptype = 'terrain', geom='density2d', color = I('red'))
}

heat_map('Boston', 'Chinese')

cities = c('Atlanta', 'Austin', 'Boston', 'Boulder', 
           'Columbus', 'Orlando', 'Portland')
variable_types = c('Total', 'American (New)', 'American (Traditional)',
                   'Mexican', 'Fast Food', 'Pizza', 'Bars', 'Sandwiches', 
                   'Japanese', 'Thai', 'Chinese', 'Greek', 'Middle Eastern',
                   'Indian', 'Italian', 'Halal', 'Nightlife')

