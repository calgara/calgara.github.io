library(ggplot2) # Load ggplot2
library(plyr) # Loaad plyr, which provides tools for summarizing data
library(readstata13) # Load readstata13 to read Stata Files
library(dplyr) # Loaad dplyr, which provides tools that I always use, like as_tibble

load("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Dissertation Project/Ch2_Electoral_Implications_Approval/Aldrich_McKelvey_Scaling/Aldrich_McKelvey_Scaling_2008_2016_Positions_Results.Rdata") # Load R environment containing various estimations of candidate & citizen ideological ideal points from 2008-2016.

# Let's explore the nature of ideological preferences in the American electorate over time!

print(as_tibble(rescaled_overtime_idealpts_2008_2016))

# Huge data frame of scaled Aldrich-McKelvey ideal points for each CCES respondent from 2008-2016. For more information on the method deriving these scaled ideal points, see Ramey (2016).

# Of course, countless studies suggest that the distribution of ideological preferences of the mass public is unimodal. Let's make a density plot to see if this is the case.

ggplot(data=rescaled_overtime_idealpts_2008_2016,aes(x=idealpt))

# Not so fast my dude. We have to learn to crawl before we run, what happened?

# Now that we have some knowledge and have to specify our density plot aestic, let's get to work & make our plot!

ggplot(data=rescaled_overtime_idealpts_2008_2016,aes(x=idealpt)) + geom_density()

# Success! But there might be evidence that the distribution of ideological preferences might be trimodal. That is, clear partisan cleavages with respect to ideological preferences. Let's explore this possibility by first coding partisan preferences

rescaled_overtime_idealpts_2008_2016$pid3 <- ifelse(rescaled_overtime_idealpts_2008_2016$pid7 %in% c("Lean Republican","Not very strong Republican","Strong Republican"),"Republican",ifelse(rescaled_overtime_idealpts_2008_2016$pid7 %in% c("Lean Democrat","Not very strong Democrat","Strong Democrat"),"Democrat",ifelse(rescaled_overtime_idealpts_2008_2016$pid7 %in% c("Independent"),"Independent",NA)))

print(as_tibble(rescaled_overtime_idealpts_2008_2016)) # Let's check it out

ggplot(data=rescaled_overtime_idealpts_2008_2016,aes(x=idealpt,color=pid3)) + geom_density()

# Urgh, good start but pretty ugly and nowhere close to publication quality. Let's make this prettier by factoring out our pid3 and getting rid of "NA".

rescaled_overtime_idealpts_2008_2016$pid3 <- factor(rescaled_overtime_idealpts_2008_2016$pid3,levels=c("Democrat","Independent","Republican"))

str(rescaled_overtime_idealpts_2008_2016$pid3)

# Let's make a really pretty plot showing partisan differences.

ggplot(subset(rescaled_overtime_idealpts_2008_2016,rescaled_overtime_idealpts_2008_2016$pid3 != "Independent"),aes(x=idealpt,fill = pid3))+ geom_density(alpha=.2) + theme_bw() + scale_fill_manual("",values =c("blue","red")) + scale_x_continuous("Aldrich-McKelvey Ideological Placement (Liberal-Conservative)",breaks=c(-4,0,4),limits=c(-4,4),labels=c("-4","0","4")) + scale_y_continuous("Density",expand=c(0,0)) + theme(legend.position=c(0.075, 0.90), legend.box.just = "left", legend.key.size = unit(1,"line"), legend.key = element_rect(size = 0, color = 'white'), legend.text.align = 0, legend.box = "horizontal") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# This is cool, let's do House candidates! First, let's check the data frame.

print(as_tibble(aldmck_congress))

# Oh no, we have to manipulate the dataframe. Urgh, bummer, but easy!

house_candidates <- subset(aldmck_congress,select=c("year","district","placement.dem_hse_libcon_placement","placement.rep_hse_libcon_placement")) # Extract the variables you want

house_candidates <- melt(house_candidates,id=c("year","district")) # Melt the dataframe from wide to long!

print(table(house_candidates$variable)) # We have one variable with a string variable indicating candidate type.

house_candidates$variable <- ifelse(house_candidates$variable == "placement.dem_hse_libcon_placement","Democrat",ifelse(house_candidates$variable == "placement.rep_hse_libcon_placement","Republican",NA))

ggplot(house_candidates, aes(x = value, fill = variable,linetype=variable)) + geom_density(alpha=.2) + theme_minimal() + scale_fill_manual("",values =c("blue","red")) + scale_x_continuous("Candidate Aldrich-McKelvey Ideological Placement (Liberal-Conservative)",limits=c(-1,1),breaks=c(-1,0,1),labels=c("-1","0","1")) + scale_y_continuous("Density",expand=c(0,0)) + theme(legend.position="bottom", legend.box.just = "left", legend.key.size = unit(1,"line"), legend.key = element_rect(size = 0, color = 'white'), legend.text.align = 0, legend.box = "horizontal") + scale_linetype_manual("",values=c("solid","dashed")) + facet_wrap(~year)

# What did we just do?

# We can also assess the validity of our ideal points by looking at the box plot distributions

ggplot(rescaled_overtime_idealpts_2008_2016, aes(x=selfplace, y=idealpts_linear_map_overtime, group=selfplace)) + geom_boxplot(colour = "black",outlier.shape = NA) + scale_y_continuous("Liberal-Conservative Aldrich-McKelvey Ideal Point Estimates",limits=c(-2.5,2.5),breaks=seq(-2,2,1)) + scale_x_continuous("Liberal-Conservative Raw Ideological Self-Placement",breaks=seq(1,7,1)) + scale_fill_discrete(guide=F) + stat_summary(fun.y = mean, geom="point",colour="black", size=2.00, shape= 17) + scale_shape_discrete("") + theme_minimal()

# Let's deconstruct our code

# Let's play around with mapping

state_pres_approval <- read.dta13("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Dissertation Project/Ch2_Electoral_Implications_Approval/MRP_Estimation/state_presidential_approval_dynamic_mrp_estimates.dta") # Load the data

as_tibble(state_pres_approval) # Get a glimpse of the data structure. These are Dynamic MRP state-level estimates (see: Caughey & Warshaw (2015) for technical details on the Dynamic MRP model) of presidential approval from 2008-2017. 

# Explore map making descriptives. Let's make a plot of presidential approval for December 2017.

# Make a percentage

state_pres_approval$state_pres_approval_percent <- state_pres_approval$median * 100

as_tibble(state_pres_approval) # Check to see if it worked. Clearly it did!

approval_2016 <- subset(state_pres_approval,state_pres_approval$year == 2016) # Subet our data

# Load the shapefile that has the data to plot maps in ggplot!

library(rgdal)

state_shp <- readOGR("/Users/carlosalgara/Desktop/carlos_school/PhD_UC Davis/Research/Data/American Politics/US House_District_Shape_Files/cb_2016_us_state_20m.shp")

# Let's explore the data embedded in the shape file

as_tibble(state_shp@data) # Dataframe embedded in the Spatial Polygons Data Frame

table(state_shp@data$NAME) # What type of states are in the file? We don't want territories!

state_shp <- subset(state_shp,state_shp@data$NAME != "Puerto Rico")
state_shp <- subset(state_shp,state_shp@data$NAME != "Alaska")
state_shp <- subset(state_shp,state_shp@data$NAME != "Hawaii")
state_shp <- subset(state_shp,state_shp@data$NAME != "District of Columbia")

# Preliminary look at our maps

ggplot(state_shp, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "gray", color = "black") + theme_void() + coord_equal() + coord_fixed(1.3)

# Let's merge our data onto the map!

approval_2016 <- subset(approval_2016,select=c("state","state_pres_approval_percent"))

state_shp@data$id <- rownames(state_shp@data)
state_shp.points <- fortify(state_shp, region="id")

state_shp.df <- join(state_shp.points, state_shp@data, by="id")
state_shp.df$state <- as.character(state_shp.df$STUSPS)

state_shp.df <- merge(state_shp.df,approval_2016,by=c("state"))

ggplot(state_shp.df, aes(x = long, y = lat, group = group,fill=state_pres_approval_percent),color="white") + geom_polygon(color = "black") + theme_void() + coord_equal() + scale_fill_gradient("Presidential Approval",low = 'black', high = 'white') + ggtitle("Presidential Approval in the U.S. States, 2016")