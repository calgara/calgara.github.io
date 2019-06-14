library(tigris)
library(leaflet)
library(plyr)
library(dplyr)

cd114 <- congressional_districts(cb = T, resolution = '20m')
x <- read.csv("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp/shiny_data/state_fips_master.csv")
x <- subset(x,select=c(fips,state_abbr))
x$fips <- as.character(x$fips)
x$fips[x$fips == "1"] <- "01"
x$fips[x$fips == "2"] <- "02"
x$fips[x$fips == "3"] <- "03"
x$fips[x$fips == "4"] <- "04"
x$fips[x$fips == "5"] <- "05"
x$fips[x$fips == "6"] <- "06"
x$fips[x$fips == "7"] <- "07"
x$fips[x$fips == "8"] <- "08"
x$fips[x$fips == "9"] <- "09"
colnames(x) <- c("STATEFP","state")
x$state <- as.character(x$state)
#x <- subset(x,x$state != "AK")
#x <- subset(x,x$state != "HI")

cd114 <- subset(cd114,cd114$STATEFP %in% unique(x$STATEFP))
plot(cd114)

popup <- paste0("State: ", cd114$STATEFP)

leaflet(cd114) %>%
  addTiles() %>%
  addPolygons(popup=popup)

setwd("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp")
load("/Users/carlosalgara/Dropbox/Github_Website/calgara.github.io/Rshinyapp/shiny_data/clean_shiny_data.Rdata")

cd114 <- geo_join(cd114, x, "STATEFP","STATEFP")
cd114$district <- paste(cd114$state,cd114$CD115FP,sep="")

cd114$district  <- gsub("00","1",cd114$district )
cd114$district  <- gsub("01","1",cd114$district )
cd114$district  <- gsub("02","2",cd114$district )
cd114$district  <- gsub("03","3",cd114$district )
cd114$district  <- gsub("04","4",cd114$district )
cd114$district  <- gsub("05","5",cd114$district )
cd114$district  <- gsub("06","6",cd114$district )
cd114$district  <- gsub("07","7",cd114$district )
cd114$district  <- gsub("08","8",cd114$district )
cd114$district  <- gsub("09","9",cd114$district )

popup <- paste0("District: ", cd114$district)

leaflet(cd114) %>%
  addTiles() %>%
  addPolygons(popup=popup)

x <- plot_states_data[,c(1,14:73)]
x$geometry <- NULL
x <- subset(x,x$district %in% cd114$district)
y <- data.frame(district=cd114$district,GEOID=cd114$GEOID)
x <- merge(x,y,by=c("district"))
x$district <- NULL
x$GEOID <- as.character(x$GEOID)
cd114$GEOID <- as.character(cd114$GEOID)

cd114 <- cd114[order(cd114$GEOID),]
x <- x[order(x$GEOID),]
x$placement.mc_libcon_placement_re.2018[x$GEOID == "4001"] <- NA

cd114 <- geo_join(cd114, x, "GEOID","GEOID")

cd114$cong_approval.2018 <- cd114$cong_approval.2018 * 100

cd114 <- cd114[order(cd114$GEOID),]

cd114$pres_approval.2018 <- cd114$pres_approval.2018 * 100
cd114$member_approval_rating.2018 <- cd114$member_approval_rating.2018 * 100

popup <- paste0("District: ", cd114$district_mc_name.2018 , "<br>", "Congressional District Approval: ", paste(round(cd114$cong_approval.2018,2),"%",sep=""))
pal <- colorNumeric(
  palette = "OrRd", #YlGnBu
  domain = cd114$cong_approval.2018
)

map3 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cd114, 
              fillColor = ~pal(cd114$cong_approval.2018), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = cd114$cong_approval.2018, 
            position = "bottomright", 
            title = "Estimated District Congressional Approval (2018)",
            labFormat = labelFormat(suffix = "%")) 
map3

library(RColorBrewer)
display.brewer.all()

library(htmlwidgets)
library(htmltools)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 14px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Aldrich-McKelvey Liberal-Conservative Legislator Ideological Ideal Point")
)  

popup <- paste0("District: ", cd114$district_mc_name.2018 , "<br>", "Legislator Ideological Preferences: ", paste(round(cd114$placement.mc_libcon_placement_re.2018,2),sep=""))
pal <- colorNumeric(
  palette = rev(brewer.pal(11,"RdYlBu")), #YlGnBu
  domain = cd114$placement.mc_libcon_placement_re.2018
)

map3 <- leaflet(width = "100%") %>% #width = "100%"
  addProviderTiles("CartoDB.Positron") %>%
  #addControl(title, position = "topleft", className="map-title") %>%
  setView(lng = -93.5, lat = 39.50, zoom = 04.49) %>% #lng = -98.5, lat = 39.50
  addPolygons(data = cd114, 
              fillColor = ~pal(cd114$placement.mc_libcon_placement_re.2018), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = cd114$placement.mc_libcon_placement_re.2018, 
            position = "bottomright", 
            na.label = "Vacant",
            title = "Aldrich-McKelvey Estimated <br> Liberal-Conservative Ideology <br> (2018)")#,
#labFormat = labelFormat(suffix = "%")) 
map3

css_fix <- "div.info.legend.leaflet-control br {clear: both; clear: both;}"
map3 <- map3 %>% prependContent(tags$style(type = "text/css", css_fix))

saveWidget(map3, file="legislator_ideology_2018.html")

popup <- paste0("District: ", cd114$district_mc_name.2018 , "<br>", "District Ideological Preferences: ", paste(round(cd114$aldmck_ideal_pt_district.2018,2),sep=""))
pal <- colorNumeric(
  palette = rev(brewer.pal(11,"RdYlBu")), #YlGnBu
  domain = cd114$aldmck_ideal_pt_district.2018
)

map3 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cd114, 
              fillColor = ~pal(cd114$aldmck_ideal_pt_district.2018), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = cd114$aldmck_ideal_pt_district.2018, 
            position = "bottomright", 
            title = "Estimated District Ideology (2018)")#,
#labFormat = labelFormat(suffix = "%")) 
map3