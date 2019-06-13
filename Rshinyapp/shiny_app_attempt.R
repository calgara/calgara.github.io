# Load Data

setwd("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp")
load("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp/data/clean_shiny_data.Rdata")

# Map US Congressional Districts
#install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")
# 
 library(USAboundaries)
# 
cont_48 <- state.name[state.name != c('Hawaii','Alaska')]
# 
plot_states_data <- us_congressional(resolution = 'low',states=cont_48)
# 
# 
plot_states_data$CD <- paste(plot_states_data$state_abbr,plot_states_data$cd115fp,sep="")
plot_states_data$CD <- gsub("00","01",plot_states_data$CD, fixed=TRUE)
# 
cd_data <- subset(house,select=c(year,mc_name,district,median,nominate.dim1,joint_scaling_ideal_pt_district,aldmck_ideal_pt_district,placement.mc_libcon_placement_re,joint_scaling_ideal_pt,pres_approval,cong_approval,member_unity,party))

cd_data$member_unity <- ifelse(cd_data$party == "D",1-cd_data$member_unity,cd_data$member_unity)
cd_data$party <- paste("(",cd_data$party,")",sep="")
cd_data <- subset(cd_data,cd_data$year %in% c(2013:2018))
cd_data$district_mc_name <- paste(cd_data$district,cd_data$mc_name,cd_data$party)
cd_data$party <- NULL
colnames(cd_data)[4] <- "member_approval_rating"

cd_data$mc_name <- NULL
cd_data <- reshape(cd_data, idvar = "district", timevar = "year", direction = "wide")

plot_states_data$district <- plot_states_data$CD
plot_states_data$district <- gsub("01","1",plot_states_data$district)
plot_states_data$district <- gsub("02","2",plot_states_data$district)
plot_states_data$district <- gsub("03","3",plot_states_data$district)
plot_states_data$district <- gsub("04","4",plot_states_data$district)
plot_states_data$district <- gsub("05","5",plot_states_data$district)
plot_states_data$district <- gsub("06","6",plot_states_data$district)
plot_states_data$district <- gsub("07","7",plot_states_data$district)
plot_states_data$district <- gsub("08","8",plot_states_data$district)
plot_states_data$district <- gsub("09","9",plot_states_data$district)

plot_states_data <- merge(plot_states_data,cd_data,by=c("district"),all=T) %>% st_transform("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") %>% st_simplify(TRUE, dTolerance = 10000)  #   st_simplify(TRUE, dTolerance = 10000)

plot(plot_states_data)

ggplot(data=plot_states_data) + geom_sf(aes(fill=plot_states_data$pres_approval.2018)) + scale_fill_continuous(low="black", high="white") + theme_void() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),legend.position = "top", legend.direction = "horizontal")

# Other way to do it

# Congressional District

require(rgdal)
require(maptools)
require(rgeos)
require(ggplot2)
require(tidyverse)

districts <- readOGR(dsn = "/Users/carlosalgara/Downloads/cb_2013_us_cd113_20m/",
                     layer = "cb_2013_us_cd113_20m",
                     stringsAsFactors = FALSE)
## OGR data source with driver: ESRI Shapefile  # TUTORIAL: https://mikegruz.github.io/articles/2017-03/mapping-in-R
## Source: "./data/cb_2013_us_cd113_20m/", layer: "cb_2013_us_cd113_20m"
## with 437 features
## It has 8 fields
## Integer64 fields read as strings:  ALAND AWATER
# pull in row names for merging
districts@data$id <- row.names(districts@data)

# transform shapes to the projection we desire
districts.tran <- spTransform(districts, 
                              CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))

# buffer the shapes
districts.points <- gBuffer(districts.tran, byid=TRUE, width=0)

# fortify the data into a dataframe
districts.df <- fortify(districts.points, region="id")

# merge in metadata from shapefiles
districts.df <- full_join(districts.df, districts@data, by="id")

# create dataframe of FIPS-ICPSR codes
fips.icpsr <- data_frame(
  fips = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56),
  icpsr = c(41,81,61,42,71,62,1,11,55,43,44,82,63,21,22,31,32,51,45,2,52,3,23,33,46,34,64,35,65,4,12,66,13,47,36,24,53,72,14,5,48,37,54,49,67,6,40,73,56,25,68)
)

districts.df <- districts.df %>%
  # get state icpsr code
  mutate(cd = as.numeric(CD113FP),
         fips = as.numeric(STATEFP),
         # recode "0" districts as "1" (these are at-large districts)
         cd = ifelse(cd==0, 1, cd))

# merge in icpsr state codes, joining on fips codes
districts.df <- full_join(districts.df, fips.icpsr, by="fips") %>%
  # rename icpsr to state for merge with DW data
  rename(state = icpsr)

library(maps)
y <- state.fips

y <- subset(y,select=c("fips","abb"))
colnames(y) <- c("fips","state_abb")

districts.df <- full_join(districts.df, y, by="fips") 
districts.df$cd[districts.df$cd == 98] <- 1
districts.df$cd[districts.df$cd == 0] <- 1
districts.df$district <- paste(districts.df$state_abb,districts.df$cd,sep="")

# merge dw and district map
districts.dw <- full_join(districts.df, cd_data, by=c("district")) %>%
  # drop alaska (2), hawaii (15), DC (11) and PR (72) 
  filter(!(fips %in% c(2,15,11,72))) 
# plot party extremes
ggplot(districts.dw) + geom_polygon(aes(x=long, y=lat, group=group, fill=pres_approval.2018), colour="black", size=.1) + scale_fill_gradient2(low="whitesmoke", high="darkred", name="Presidential Approval") + theme_void() + coord_fixed(1) + ggtitle("Presidential Approval in the 115th U.S. Congress")  + scale_fill_gradient("Level of Presidential Approval",low = 'cyan', high = 'red', breaks=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)),labels=c("Minimum","Maximum"),limits=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)))

ggplot(data = plot_states_data) + geom_sf(aes(fill=plot_states_data$pres_approval.2018)) + scale_fill_gradient("Level of Presidential Approval",low = 'cyan', high = 'red', breaks=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)),limits=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)))
#   scale_fill_manual(values=c("blue2", "#E69F00",'purple4','red2')) +
#   ggtitle("Breakdown Map of Congressional versus Presidential Winner in 2016 election") +
#   theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
#                      legend.position = "top", legend.direction = "horizontal") + labs(fill = "Presidential/Congressional Winner Breakdown")

rm(cd_data,districts,districts.df,district.points,districts.tran,house,senate,ui,y,cont_48,server,fips.icpsr,districts.points)

save.image("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp/data/clean_shiny_data.Rdata")
load("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp/data/clean_shiny_data.Rdata")

library(maps)
library(mapproj)
library(ggplot2)
library(fiftystater)
library(USAboundaries)
library(sf)
library(shiny)

# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("P"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Presidential Approval"),
                  selected = "Presidential Approval"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 1, value = c(0, 1))
      ),
    
    mainPanel(plotOutput("map"))
  )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = plot_states_data$pres_approval.2018)
    legend <- switch(input$var, 
                     "Presidential Approval" = "% Approving")
    
    ggplot(data = plot_states_data) + geom_sf(aes(fill=data))
}

# Run app ----
shinyApp(ui, server)

ggplot(data = plot_states_data) + geom_sf(aes(fill=plot_states_data$pres_approval.2018)) + scale_fill_gradient("Level of Presidential Approval",low = 'cyan', high = 'red', breaks=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)),limits=c(min(districts.dw$pres_approval.2018),max(districts.dw$pres_approval.2018)))
