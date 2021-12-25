library(tidyverse)
library(ggplot2)
library(ggmap)
library(stringr)
library(dplyr)
library(tidyr)
library(grid)
library(RSQLite)
library(shiny)
library(grid)
library(png)


load("~/Desktop/STAT405 Home/allbusi_states.rda")
load("All Bars.rda")
all_rest_zip_zc <<- all_bars


create_rest_df <- function(cuisine_type) {
  if (cuisine_type == "Bar") {
    all_rest_zip_zc <<- all_bars
  }
  
  if (cuisine_type == "Fast Food") {
    all_rest_zip_zc <<- all_ff
  }
  
  if (cuisine_type == "Vegan") {
    all_rest_zip_zc <<- all_vegan
  }
  
  if (cuisine_type == "Pizza") {
    all_rest_zip_zc <<- all_pizza
  }
  
  if (cuisine_type == "America") {
    all_rest_zip_zc <<- all_american
  }
}





#Step 4: Heart Sizes
#This function creates the relative size of hearts, matching an overall percentage to the heart size 
#Also Note: this is just for
heart_ranks <- function() {
  hearts <- allbusi_state[,"heart"] #grab health statistic
  states <- allbusi_state[,"state"] #grab states
  heart_data <- cbind.data.frame(hearts, states)
  heart_data <- heart_data %>%
    arrange(desc(hearts)) #arrange health statistic from most to least
  ranks <- c(7:1)
  heart_rankings <- cbind.data.frame(heart_data, ranks)
  heart_rankings
  heart_sum <- sum(heart_rankings[1])
  heart_pct <- heart_rankings[1] / heart_sum
  heart_rankings <- cbind.data.frame(heart_rankings, heart_pct)
  heart_pct_rel <- heart_rankings[4] / max(heart_rankings[4])
  final_ranks <<- cbind.data.frame(heart_rankings, heart_pct_rel)
}
heart_ranks()

#Step 5: Load USA Data Points
load("US Data Points.Rda") #add ur own wd(), but this creates US data points


#Step 6: Intializer Function creates initial set of data, and should remain unchanged throughout
initializer <- function() {
  grid.newpage() #Creates new page
  vp0 <<- dataViewport(long_lat$longitude, long_lat$latitude) #US Map viewport #Note: << means store in global envior.
  pushViewport(vp0) #pushes US map as viewport
  city_x <<- c(-122.6784, -105.2705, -97.7431, -84.3880, -82.9988, -81.3789, -71.0589) #city latitudes
  city_y <<- c(45.5152, 40.0150, 30.2672,33.7490, 39.9612, 28.5384, 42.3601) #city longitudes
  vp_x <<- (city_x - min(vp0$xscale)) / (max(vp0$xscale) - min(vp0$xscale)) #starting points for latitude
  vp_y <<- (city_y - min(vp0$yscale)) / (max(vp0$yscale) - min(vp0$yscale)) #starting points for longitude
  x0 <<- c(.5, 1.67, .5, .5) #heart x's
  y0 <<- c(.0, 1.1, 1.1, .9) #heart y's
} 


#Step 7: zip_code_data, grabbing the health statistics you want
#The function assumes you want review counts for x-axis, then a secondary health statistic as y-axis
zip_code_data <- function(health_stat, restaurant_type) {
  restaurant_type %>% 
    filter(state == "OR") %>% 
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_port
  x_port <<- all_bar_port$`Review Count`
  y_port <<- all_bar_port[,health_stat]
  
  restaurant_type %>% 
    filter(state == "CO") %>% 
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_boul
  x_boul <<- all_bar_boul$`Review Count`
  y_boul <<- all_bar_boul[,health_stat]
  
  restaurant_type %>%
    filter(state == "TX") %>%
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_aust
  x_aust <<- all_bar_aust$`Review Count`
  y_aust <<- all_bar_aust[,health_stat]
  
  restaurant_type %>%
    filter(state == "GA") %>%
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_ATL
  x_ATL <<- all_bar_ATL$`Review Count`
  y_ATL <<- all_bar_ATL[,health_stat]
  
  restaurant_type %>%
    filter(state == "OH") %>%
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_colu
  x_colu <<- all_bar_colu$`Review Count`
  y_colu <<- all_bar_colu[,health_stat]
  
  restaurant_type %>%
    filter(state == "FL") %>%
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_FL
  x_orl <<- all_bar_FL$`Review Count`
  y_orl <<- all_bar_FL[,health_stat]
  
  restaurant_type %>%
    filter(state == "MA") %>%
    dplyr::select(`Review Count`, health_stat, `Average Stars`) ->> all_bar_BOS
  x_BOS <<- all_bar_BOS$`Review Count`
  y_BOS <<- all_bar_BOS[,health_stat]
}

#Step 8: Create Viewports
#Would only need to change xscale and yscale for viewports 8-14, which gets complicated
#For viewports 1-7: hearts
#vp_x: where to center heart on x-axis, no change
#vp_y: where to center heart on y-axis, no change
#xscale: the min. and max. values of x based on longtitude values, do not change
#yscale: the min. and max. values of y based on latitude values, do not change
#height: value for heart height that varies based on values from earlier, no change
#width: constant value for heart width, no change

#for viewports 8-14: rectangles, axes, points, labels, everything else
#vp_x: where to center chart on x-axis, no change
#vp_y: where to center chart on y-axis, no change
#xscale: the min. and max. values of x based on review counts, do not change since keeping xaxis same
#yscale: the min. and max. values of y based on health stats, no change
#height: value for heart height that varies based on values from earlier, no change
#width: constant value for heart width, no change
create_viewports <- function() {
  
  #Portland
  vp1 <<- viewport(x = vp_x[1]+.02, y = vp_y[1], xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "OR",5]), width = .15) 
  vp8 <<- viewport(x = vp_x[1]+.02, y = vp_y[1], xscale = c((min(x_port) - .1 * min(x_port)), (max(x_port) + .1 * max(x_port))), yscale = c((min(y_port[,1]) - .1 * min(y_port[,1])), (max(y_port[,1]) + .1 * max(y_port[,1]))),
                   height = .3*(final_ranks[final_ranks$state == "OR",5]), width = .15) 
  
  #Boulder
  vp2 <<- viewport(x = vp_x[2]+.025, y = vp_y[2], xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "CO",5]), width = .15)
  vp9 <<- viewport(x = vp_x[2]+.025, y = vp_y[2], xscale = c((min(x_boul) - .1 * min(x_boul)), (max(x_boul) + .1 * max(x_boul))),
                   yscale = c((min(y_boul[,1]) - .1 * min(y_boul[,1])), (max(y_boul[,1]) + .1 * max(y_boul[,1]))), height = .3*(final_ranks[final_ranks$state == "CO",5]), width = .15) 
  
  #Austin
  vp3 <<- viewport(x = vp_x[3]-.05, y = vp_y[3], xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "TX",5]), width = .15)
  vp10 <<- viewport(x = vp_x[3]-.05, y = vp_y[3], xscale = c((min(x_aust) - .1 * min(x_aust)), (max(x_aust) + .1 * max(x_aust))),
                    yscale = c((min(y_aust[,1]) - .1 * min(y_aust[,1])), (max(y_aust[,1]) + .1 * max(y_aust[,1]))), height = .3*(final_ranks[final_ranks$state == "TX",5]), width = .15)
  
  #Atlanta
  vp4 <<- viewport(x = vp_x[4]-.05, y = vp_y[4], xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "GA",5]), width = .15)
  vp11 <<- viewport(x = vp_x[4]-.05, y = vp_y[4], xscale = c((min(x_ATL) - .1 * min(x_ATL)), (max(x_ATL) + .1 * max(x_ATL))),
                    yscale = c((min(y_ATL[,1]) - .1 * min(y_ATL[,1])), (max(y_ATL[,1]) + .1 * max(y_ATL[,1]))), height = .3*(final_ranks[final_ranks$state == "GA",5]), width = .15)
  
  #Columbus
  vp5 <<- viewport(x = vp_x[5]-.02, y = vp_y[5]+.02, xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "OH",5]), width = .15)
  vp12 <<- viewport(x = vp_x[5]-.02, y = vp_y[5]+.02, xscale = c((min(x_colu) - .1 * min(x_colu)), (max(x_colu) + .1 * max(x_colu))),
                    yscale = c((min(y_colu[,1]) - .1 * min(y_colu[,1])), (max(y_colu[,1]) + .1 * max(y_colu[,1]))), height = .3*(final_ranks[final_ranks$state == "OH",5]), width = .15)
  
  #Orlando
  vp6 <<- viewport(x = vp_x[6]+.07, y = vp_y[6]+.07, xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "FL",5]), width = .15)
  vp13 <<- viewport(x = vp_x[6]+.07, y = vp_y[6]+.07, xscale = c((min(x_orl) - .1 * min(x_orl)), (max(x_orl) + .1 * max(x_orl))),
                    yscale = c((min(y_orl[,1]) - .1 * min(y_orl[,1])), (max(y_orl[,1]) + .1 * max(y_orl[,1]))), height = .3*(final_ranks[final_ranks$state == "FL",5]), width = .15)
  
  #Boston
  vp7 <<- viewport(x = vp_x[7]+.01, y = vp_y[7], xscale = vp0$xscale, yscale = vp0$yscale, height = .3*(final_ranks[final_ranks$state == "MA",5]), width = .15)
  vp14 <<- viewport(x = vp_x[7]+.01, y = vp_y[7], xscale = c((min(x_BOS) - .1 * min(x_BOS)), (max(x_BOS) + .1 * max(x_BOS))),
                    yscale = c((min(y_BOS[,1]) - .1 * min(y_BOS[,1])), (max(y_BOS[,1]) + .1 * max(y_BOS[,1]))), height = .3*(final_ranks[final_ranks$state == "MA",5]), width = .15)
}   #Here you can change heart size(heights) and scales to match data, right now only manually

#Step 9: Create Rectangles
#Creates boxes around viewport as well as the US map
#Nothing in the function needs to be changed
create_rectangles <- function() {
  #creates box around each relevant viewport
  grid.rect(vp = vp8)
  grid.rect(vp = vp9)
  grid.rect(vp = vp10)
  grid.rect(vp = vp11)
  grid.rect(vp = vp12)
  grid.rect(vp = vp13)
  grid.rect(vp = vp14)
  grid.lines(long_lat$longitude, long_lat$latitude, gp = gpar(col="grey"), default.units = "native") #creates US map
} 

#Step 10: Generate Hearts
heart <- function(x, y, viewporth, color) {
  #Note: color is where you can change color of heart, likely must be done manually
  x1 <- x
  y1 <- y
  x2 <- c(x[1], x[1] - (x[2] - x[1]), x[3], x[4])
  y2 <- y
  grid.bezier(x1, y1, vp = viewporth,
              gp = gpar(col = as.character(color),
                        lwd = 3))
  grid.bezier(x2, y2, vp = viewporth,
              gp = gpar(col = as.character(color),
                        lwd = 3))
} #Options: Heart Color

generate_hearts <- function(logical_h) {
  if (logical_h == TRUE) {
    terrain_colors <- terrain.colors(7)
    terrain_colors[7] <- "red"
    terrain_colors <- rev(terrain_colors)
    final_ranks <- cbind(final_ranks, terrain_colors)
    heart(x0, y0, vp1, final_ranks[final_ranks$state == "OR",6]) #Portland
    heart(x0, y0, vp2, final_ranks[final_ranks$state == "CO",6]) #Boulder
    heart(x0, y0, vp3, final_ranks[final_ranks$state == "TX",6]) #Austin
    heart(x0, y0, vp4, final_ranks[final_ranks$state == "GA",6]) #Atlanta 
    heart(x0, y0, vp5, final_ranks[final_ranks$state == "OH",6]) #Columbus
    heart(x0, y0, vp6, final_ranks[final_ranks$state == "FL",6]) #Orlando
    heart(x0, y0, vp7, final_ranks[final_ranks$state == "MA",6]) #Boston
  }
  else {
    NULL
  }
} 

#Step 11: Generate Axes
#Based off viewports in generate viewports, so nothing to change
generate_axes <- function() {
  grid.xaxis(vp = vp8)
  grid.yaxis(vp = vp8, main = FALSE) #Portland
  
  grid.xaxis(vp = vp9)
  grid.yaxis(vp = vp9) #Boulder
  
  grid.xaxis(vp = vp10)
  grid.yaxis(vp = vp10) #Austin
  
  grid.xaxis(vp = vp11)
  grid.yaxis(vp = vp11) #Atlanta
  
  grid.xaxis(vp = vp12, main = FALSE)
  grid.yaxis(vp = vp12) #Columbus
  
  grid.xaxis(vp = vp13)
  grid.yaxis(vp = vp13, main = F) #Orlando
  
  grid.xaxis(vp = vp14)
  grid.yaxis(vp = vp14) #Boston
  
} #Same for all scatterplots, diff. for other types, would make sense to have indicator variable for type

#Step 12: Generate Points
#Nothing to change, have already accumulated everything from this
generate_points <- function() {
  
  grid.points(x_port, as.list(y_port)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_port$`Average Stars`), vp = vp8)
  
  grid.points(x_boul, as.list(y_boul)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_boul$`Average Stars`), vp = vp9)
  
  grid.points(x_aust, as.list(y_aust)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_aust$`Average Stars`), vp = vp10)
  
  grid.points(x_ATL, as.list(y_ATL)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_ATL$`Average Stars`), vp = vp11)
  
  grid.points(x_colu, as.list(y_colu)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_colu$`Average Stars`), vp = vp12)
  
  grid.points(x_orl, as.list(y_orl)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_FL$`Average Stars`), vp = vp13)
  
  grid.points(x_BOS, as.list(y_BOS)[[1]], pch = 1, size = unit(0.04, "npc"),
              gp = gpar(col = all_bar_BOS$`Average Stars`), vp = vp14)
}

#Step 13: Generate Labels
#I can clean this up later as needed, but this generates labels
generate_prez_labels_y <- function(health_stat) {
  
  if(health_stat=="Binge Drinking"){
    grid.text("     Y Axes: Crude Prevalance of Binge Drinking", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="High Blood Pressure"){
    grid.text("     Y Axes: Crude Prevalance of High Blood Pressure", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Stroke"){
    grid.text("     Y Axes: Crude Prevalance of Stroke", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Obesity"){
    grid.text("     Y Axes: Crude Prevalance of Obesity", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Any Cancer"){
    grid.text("     Y Axes: Crude Prevalance of Cancer", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  if(health_stat=="Mental Health Problems"){
    grid.text("     Y Axes: Crude Prevalance of Length of Mental Health Issues", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Arthritis"){
    grid.text("     Y Axes: Crude Prevalance of Arthritis", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Asthma"){
    grid.text("     Y Axes: Crude Prevalance of Asthma", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Coronary Heart Disease"){
    grid.text("     Y Axes: Crude Prevalance of Coronary Heart Disease", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(health_stat=="Chronic Kidney Disease"){
    grid.text("     Y Axes: Crude Prevalance of Chronic Kidney Disease", x = unit(0.5, "npc"),
              y = unit(0.89, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
}

generate_prez_labels_x <- function(cuisine) {
  
  if(cuisine=="Bar"){
    grid.text("X Axes: Number of Bar Reviews", x = unit(0.5, "npc"),
              y = unit(.96, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(cuisine=="Fast Food"){
    grid.text("X Axes: Number of Fast Food Reviews", x = unit(0.5, "npc"),
              y = unit(0.96, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(cuisine=="Pizza"){
    grid.text("X Axes: Number of Pizza Reviews", x = unit(0.5, "npc"),
              y = unit(0.96, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(cuisine=="American"){
    grid.text("X Axes: Number of American Reviews", x = unit(0.5, "npc"),
              y = unit(0.96, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
  
  if(cuisine=="Vegan"){
    grid.text("X Axes: Number of Vegan Reviews", x = unit(0.5, "npc"),
              y = unit(0.96, "npc"), gp = gpar(fontsize = 15), vp = vp0)
  }
}


generate_city_names <- function(logical) {
  if (logical == TRUE) {
    grid.text("Portland", gp = gpar(fontsize = 15,
                                    col = "darkblue",
                                    fontface = "bold"), vp = vp8)
    
    grid.text("Boulder", gp = gpar(fontsize = 15,
                                   col = "darkblue",
                                   fontface = "bold"), vp = vp9)
    
    grid.text("Austin", gp = gpar(fontsize = 15,
                                  col = "darkblue",
                                  fontface = "bold"), vp = vp10)
    
    grid.text("Atlanta", gp = gpar(fontsize = 15,
                                   col = "darkblue",
                                   fontface = "bold"), vp = vp11)
    
    grid.text("Columbus", gp = gpar(fontsize = 15,
                                    col = "darkblue",
                                    fontface = "bold"), vp = vp12)
    
    grid.text("Orlando", gp = gpar(fontsize = 15,
                                   col = "darkblue",
                                   fontface = "bold"), vp = vp13)
    
    grid.text("Boston", gp = gpar(fontsize = 15,
                                  col = "darkblue",
                                  fontface = "bold"), vp = vp14)
  }
  else {
    NULL
  }
}
#No Longer Using Labels, will still use in our final report, no shiny required
# generate_labels <-function(x_label, y_label) { 
#   grid.text(as.character(y_label), x = unit(12, "lines"), rot = 270, vp = vp8) #y
#   grid.text(as.character(x_label), y = unit(-2.5, "lines"), vp = vp8) #x
#   
#   
#   grid.text(as.character(y_label), x = unit(-3, "lines"), rot = 90, vp = vp9) #y
#   grid.text(as.character(x_label), y = unit(-2.5, "lines"), vp = vp9) #x
#   
#   grid.text(as.character(y_label), x = unit(-3, "lines"), rot = 90, vp = vp10) #y
#   grid.text(as.character(x_label), y = unit(-2.5, "lines"), vp = vp10) #x
#   
#   
#   grid.text(as.character(y_label), x = unit(-2.5, "lines"), rot = 90, vp = vp11) #y
#   grid.text(as.character(x_label), y = unit(-2.5, "lines"), vp = vp11) #x
#   
#   
#   grid.text(as.character(y_label), x = unit(-3, "lines"), rot = 90, vp = vp12) #y
#   grid.text(as.character(x_label), y = unit(16, "lines"), vp = vp12) #x
#   
#   grid.text(as.character(y_label), x = unit(11, "lines"), rot = 270, vp = vp13) #y
#   grid.text(as.character(x_label), y = unit(-2.1, "lines"), vp = vp13) #x
#   
#   
#   grid.text(as.character(y_label), x = unit(-2.5, "lines"), rot = 90, vp = vp14) #y
#   grid.text(as.character(x_label), y = unit(-2.5, "lines"), vp = vp14) #x
# } #Same for all scatterplots, diff. for other types, would make sense to have indicator variable for type
# generate_labels("Number of Bar Tips", "Binge Prevalence")



server <- function(input, output, session) {
  
  
  
  output$viewPlot <- renderPlot({
    create_rest_df(input$rest_typing)
    initializer()
    zip_code_data(input$variable, all_rest_zip_zc)
    create_viewports()
    create_rectangles()
    generate_hearts(input$heartsfull)
    generate_axes()
    generate_points()
    generate_prez_labels_x(input$rest_typing)
    generate_prez_labels_y(input$variable)
    generate_city_names(input$city)
    
    
    
  })
  
}



ui <- fluidPage(
  theme = shinytheme("united"),
  plotOutput("viewPlot",width="100%"),
  fluidRow(
    column(3,
           checkboxInput("city", "Show City Names", FALSE),
           checkboxInput("heartsfull", "Show Hearts", FALSE)),
    column(4,
           selectInput("variable", "Select Y-Axis Variable",
                       choices = c("Binge Drinking","High Blood Pressure", "Stroke","Obesity","Any Cancer",
                                   "Mental Health Problems","Arthritis", "Asthma", "Coronary Heart Disease", "Chronic Kidney Disease"))),
    column(5,
           selectInput("rest_typing", "Select Cuisine Type",
                       choices = c("Bar","Fast Food", "Pizza", "American", "Vegan")))),
  setBackgroundColor(
    color = "aliceblue",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  )
)


# server <- function(input, output) {
#   output$value <- renderText({ input$somevalue })
# }


shinyApp(ui,server)

#Colors
#Red = 4
#Black = 3

#View(all_bar_port)

#you yourself can talk about it
```