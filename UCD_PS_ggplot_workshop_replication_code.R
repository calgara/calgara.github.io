library(ggplot2) # Load ggplot2
library(plyr) # Loaad plyr, which provides tools for summarizing data
library(readstata13) # Load readstata13 to read Stata Files
library(dplyr) # Loaad dplyr, which provides tools that I always use, like as_tibble
library(reshape) # Load reshape, which provides for the melt function needed for data manipulation.

##### CCES Ideal Points Section of ggplot Tutorial #####################################################

load("/Users/carlosalgara/Desktop/ggplot_tutorial.Rdata") # Load the R environment containing the necessary data to replicate the analysis in this tutorial.

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

print(str(rescaled_overtime_idealpts_2008_2016$pid3))

# Let's make a really pretty plot showing partisan differences.

ggplot(subset(rescaled_overtime_idealpts_2008_2016,rescaled_overtime_idealpts_2008_2016$pid3 != "Independent"),aes(x=idealpt,fill = pid3))+ geom_density(alpha=.2) + theme_bw() + scale_fill_manual("",values =c("blue","red")) + scale_x_continuous("Aldrich-McKelvey Ideological Placement (Liberal-Conservative)",breaks=c(-4,0,4),limits=c(-4,4),labels=c("-4","0","4")) + scale_y_continuous("Density",expand=c(0,0)) + theme(legend.position=c(0.10, 0.90), legend.box.just = "left", legend.key.size = unit(1,"line"), legend.key = element_rect(size = 0, color = 'white'), legend.text.align = 0, legend.box = "horizontal") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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

##### Presidential Approval Section of Tutorial ########################################################

as_tibble(approval)

ggplot(approval, aes(x = approval_measure)) + geom_histogram(alpha=.2,fill="black",color="black") + theme_minimal() + scale_x_continuous("Presidential Approval Polling") + ggtitle("Distribution of Presidential Approval Polling Data, 1969-2017")

approval$Polling.End <- as.Date(approval$Polling.End)
ggplot(approval,aes(x=Polling.End,y=approval_measure)) + geom_line()

approval$year_factor <- factor(approval$year,levels=seq(1969,2017,1))
ggplot(subset(approval,approval$president == "Bush"),aes(x=year_factor,y=approval_measure)) + geom_boxplot() + ggtitle("Presidential Approval during the George W. Bush Administration, 2001-2009")

mean_approval <- ddply(approval, .(year), summarize,  approval_mean =mean(approval_measure, na.rm = T))

mean_approval$president <- ifelse(mean_approval$year %in% 1969, "Nixon",ifelse(mean_approval$year %in% 1974, "Ford",ifelse(mean_approval$year %in% 1977, "Carter",ifelse(mean_approval$year %in% 1981, "Reagan",ifelse(mean_approval$year %in% 1989, "Bush 43",ifelse(mean_approval$year %in% 1993, "Clinton",ifelse(mean_approval$year %in% 2001, "W. Bush",ifelse(mean_approval$year %in% 2009, "Obama",ifelse(mean_approval$year %in% 2017, "Trump",NA)))))))))

library(ggrepel) # Package the allows us to use label repels

ggplot(mean_approval,aes(x=year,y=approval_mean,label=president)) + geom_line() + theme_bw() + geom_label_repel(arrow = arrow(length = unit(0.02, 'npc')),segment.size = 0.5, box.padding = 0.5,point.padding = 1, size = 3.5) + ggtitle("Presidential Approval from Nixon to Trump, 1969-2017") + scale_x_continuous("",breaks=seq(1968,2016,4)) + scale_y_continuous("Yearly Presidential Approval Rating")

##### U.S. House Elections Section of Tutorial #########################################################

as_tibble(house)
ggplot(subset(house,house$year %in% 2016),aes(x=dpres,y=dv)) + geom_point(shape=1) + geom_smooth(method='lm',SE=T,color="black") + theme_bw() + scale_x_continuous("District Two-Party Vote Share Won by the Democratic Presidential Candidate") + scale_y_continuous("District Two-Party Vote Share Won \nby the Democratic House Candidate") + ggtitle("Scatterplot of Democratic House Vote-Share by \nDemocratic Presidential Vote-Share in 2016")

ggplot(subset(house,house$year %in% 2016),aes(x=dpres,y=dv,color=dem_seat,label=district)) + geom_text(size=2) + geom_smooth(method='lm',SE=T,color="black") + theme_bw() + scale_color_manual("Seat Partisanship",values=c("red","blue")) + scale_x_continuous("District Two-Party Vote Share Won by the Democratic Presidential Candidate",breaks=seq(0,100,10)) + scale_y_continuous("District Two-Party Vote Share Won \nby the Democratic House Candidate",breaks=seq(0,100,10)) + ggtitle("Scatterplot of Democratic House Vote-Share by \nDemocratic Presidential Vote-Share")  + geom_hline(yintercept = 50, colour = gray(1/2), lty = 2) + theme(legend.position="bottom")

ggplot(subset(house,house$year %in% c(1974, 1994,2006,2010)),aes(x=dpres,y=dv,color=dem_seat,label=district)) + geom_point(shape=1) + geom_smooth(method='lm',SE=T,color="black") + theme_minimal() + scale_color_manual("Seat Partisanship",values=c("red","blue")) + scale_x_continuous("District Two-Party Vote Share Won by the Democratic Presidential Candidate",breaks=seq(0,100,10)) + scale_y_continuous("District Two-Party Vote Share Won \nby the Democratic House Candidate",breaks=seq(0,100,25)) + ggtitle("Scatterplot of Democratic House Vote-Share by \nDemocratic Presidential Vote-Share")  + geom_hline(yintercept = 50, colour = gray(1/2), lty = 2) + theme(legend.position="bottom") + facet_wrap(~year,ncol=2) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) # The last part of the code takes out all gridlines.

##### CCES DIF Difference of Means Section of Tutorial #################################################

as_tibble(cces)

sample_means <- ddply(cces, .(self_placement), summarize, hrc_sd = sd(hrc_placement,na.rm=T), dt_sd = sd(dt_placement,na.rm=T),hrc_placement = mean(hrc_placement, na.rm = T), dt_placement = mean(dt_placement, na.rm = T))

z_critical_value <- qnorm(0.975)

x <- data.frame(table(cces$self_placement)) # Retrieve Ns

sample_means$self_placement_n <- x[,2]

sample_means$margin_of_error_hrc <- z_critical_value * (sample_means$hrc_sd / sqrt(sample_means$self_placement_n)) # Get margin of error for HRC

sample_means$margin_of_error_dt <- z_critical_value * (sample_means$dt_sd / sqrt(sample_means$self_placement_n)) # Get margin of error for DT

sample_means$hrc_lower_ci <- sample_means$hrc_placement - sample_means$margin_of_error_hrc
sample_means$hrc_upper_ci <- sample_means$hrc_placement + sample_means$margin_of_error_hrc

sample_means$dt_lower_ci <- sample_means$dt_placement - sample_means$margin_of_error_dt
sample_means$dt_upper_ci <- sample_means$dt_placement + sample_means$margin_of_error_dt

# Let's rework our dataframe to get Trump and Clinton on the same plot

colnames(sample_means)

hrc <- sample_means[,c(1,4,9,10)]
dt <- sample_means[,c(1,5,11,12)]

colnames(hrc) <- c("self_placement","mean","lower","upper")
colnames(dt) <- c("self_placement","mean","lower","upper")

hrc$candidate <- "Hillary Clinton"
dt$candidate <- "Donald Trump"

sample_means <- rbind(hrc,dt)

ggplot(sample_means,aes(x=self_placement,y=mean,ymin=lower,ymax=upper,color=candidate)) + geom_pointrange() + scale_color_manual("2016 Presidential Candidate",values=c("red","blue")) +  theme_minimal() + scale_x_continuous("Respondent Liberal-Conservative Self-Placement",breaks=seq(1,7,1)) + scale_y_continuous("Mean Ideological Placement of Hillary Clinton") + ggtitle("Evaluating Differential Item Function in Citizen Ideological Placement \nof the 2016 Presidential Candidates") + theme(legend.position="bottom")

ggplot(subset(sample_means,sample_means$candidate == "Hillary Clinton"),aes(x=self_placement,y=mean,ymin=lower,ymax=upper)) + geom_pointrange() +  theme_minimal() + scale_x_continuous("Respondent Liberal-Conservative Self-Placement",breaks=seq(1,7,1)) + scale_y_continuous("Mean Ideological Placement of Hillary Clinton") + ggtitle("Evaluating Differential Item Function in Citizen Ideological Placement \nof Hillary Clinton, 2016 CCES")

ggplot(subset(sample_means,sample_means$candidate == "Donald Trump"),aes(x=self_placement,y=mean,ymin=lower,ymax=upper)) + geom_pointrange() +  theme_minimal() + scale_x_continuous("Respondent Liberal-Conservative Self-Placement",breaks=seq(1,7,1)) + scale_y_continuous("Mean Ideological Placement of Donald Trump") + ggtitle("Evaluating Differential Item Function in Citizen Ideological Placement \nof Donald Trump, 2016 CCES")

##### Model of U.S. House Elections Section of Tutorial ################################################

as_tibble(house_elexs)

library(margins) # Load Thomas Leeper's Margins Package

summary(model <- glm(dv ~ dem_incumbency + dpres + dem_qual_advantage_tri + dem_seat + dvp, data=house_elexs)) # Estimate our pooled model

mes <- data.frame(summary(margins(model, change="dydx", vcov.=vcovHC(model, "HC3"))))

head(mes)

mes$factor <- ifelse(mes$factor == "dem_incumbency","Incumbency",ifelse(mes$factor == "dem_qual_advantage_tri","Candidate Quality",ifelse(mes$factor == "dem_seatDemocratic Seat","Seat Partisanship",ifelse(mes$factor == "dpres","District Partisanship",ifelse(mes$factor == "dvp","Previous District Dem Vote-Share",NA)))))

mes$factor <- factor(mes$factor)

ggplot(mes,aes(x=factor,y=AME,ymin=lower,ymax=upper,group=factor,shape=factor)) + theme_minimal() + geom_errorbar(width=0.2,size=1) + geom_point(size=3, fill="white",position= position_dodge(width=1.0)) + scale_x_discrete("") + scale_shape_manual("",values=c(21,21,21,21,21)) + theme(legend.position="none") + coord_flip() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_y_continuous("Average Marginal Effect") + ggtitle("Average Marginal Effect of Model Covarates on House Election Vote Shares")

mes_yearly <- list()
for(i in seq(1900,2016,2)){
  x <- subset(house_elexs,house_elexs$year == i)
  model <- glm(dv ~ dem_incumbency + dpres + dem_qual_advantage_tri + dem_seat + dvp, data=x) # Estimate our yearly model
  mes <- data.frame(summary(margins(model, change="dydx", vcov.=vcovHC(x, "HC3"))))
  mes$election_year <- i
  mes_yearly[[i]] <- mes
}

mes_yearly <- ldply(mes_yearly,data.frame) # Unpack the list

head(mes_yearly)

mes_yearly$factor <- ifelse(mes_yearly$factor == "dem_incumbency", "Incumbency Advantage",ifelse(mes_yearly$factor == "dpres", "District Partisanship",mes_yearly$factor))

ggplot(subset(mes_yearly,mes_yearly$factor %in% c("Incumbency Advantage")),aes(x=election_year,y=AME,ymin=lower,ymax=upper)) + geom_pointrange() + theme_minimal() + ggtitle("Effect of Incumbency on House Election Outcomes from 1900-2016") + scale_x_continuous("",breaks=seq(1900,2016,10)) + geom_smooth(method="loess") + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)

ggplot(subset(mes_yearly,mes_yearly$factor %in% c("District Partisanship")),aes(x=election_year,y=AME,ymin=lower,ymax=upper)) + geom_pointrange() + theme_minimal() + ggtitle("Effect of District Partisanship on House Election Outcomes from 1900-2016") + scale_x_continuous("",breaks=seq(1900,2016,10)) + geom_smooth(method="loess") + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)

##### Logit Model Estimation Section of Tutorial #######################################################

library(effects) # Package to extract predicted probabilities
library(multiwayvcov) # Package to extract clustered standard errors

as_tibble(resources)

# Turnout ~ Education

model <- glm(turnout ~ education, data=resources, weights=weight, family = binomial(link = "logit"))
predict <- data.frame(effect("education", se=TRUE, mod = model, confidence.level = 0.95, xlevels=list(vcov. = cluster.vcov(model, cluster=resources$cdid))))

predict$education <- factor(predict$education,levels=c("No HS","High school graduate","Some college","2-year","4-year","Post-grad"))

ggplot(data= predict, mapping=aes(x=education, y=fit, ymin=lower, ymax=upper, fill=education)) + geom_bar(stat="identity") + geom_errorbar(width=.25) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_discrete("Voter Education Level") + scale_y_continuous(limits=c(0,1.0), breaks=seq(0,1.0,0.05), "Probability of Turning Out to Vote") + ggtitle("Probability of Voter Turnout by Education Level in the 2014 Midterm Elections") + coord_cartesian(ylim=c(0.70,1.0)) + scale_fill_discrete(guide=FALSE)

# Turnout ~ Income

model <- glm(turnout ~ income, data=resources, weights=weight, family = binomial(link = "logit"))
predict <- data.frame(effect("income", se=TRUE, mod = model, confidence.level = 0.95, xlevels=list(income = seq(0,15,1), (vcov. = cluster.vcov(model, cluster=resources$cdid)))))

ggplot(data= predict, mapping=aes(x=income, y=fit)) + geom_line(aes(x = income, y = fit), size = 0.50) + geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .2) + scale_colour_manual("",values="black") + scale_fill_manual("",values="grey12") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(limits=c(0,15), breaks=c(0,15), labels=c("Less than \n$10,000", "Greater than \n$500,000"), "Voter Income") + scale_y_continuous(limits=c(0.7,1.0), breaks=seq(0.7,1.0,0.05), "Probability of Turning Out to Vote") + ggtitle("Probability of Voter Turnout by Income Level in the 2014 Midterm Elections")

resources$education <- factor(resources$education,levels=c("No HS","High school graduate","Some college","2-year","4-year","Post-grad"))

model <- glm(turnout ~ education*income, data=resources, weights=weight, family = binomial(link = "logit"))
predict <- data.frame(effect("education*income", se=TRUE, mod = model, confidence.level = 0.95, xlevels=list(income=unique(resources$income),vcov. = cluster.vcov(model, cluster=resources$cdid))))
head(predict)
predict$education <- factor(predict$education,levels=c("No HS","High school graduate","Some college","2-year","4-year","Post-grad"))

ggplot(predict, aes(x=income, y=fit, group=education,fill=education,linetype=education)) + geom_line(aes(x = income, y = fit), size = 0.50) + geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .2) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(limits=c(0,15), breaks=c(0,15), labels=c("Less than \n$10,000", "Greater than \n$500,000"), "Voter Income") + scale_y_continuous(limits=c(0.5,1.0), breaks=seq(0.5,1.0,0.05), "Probability of Turning Out to Vote") + ggtitle("Probability of Voter Turnout by Income & Education Level in the 2014 \nMidterm Elections") + theme(legend.position="bottom", legend.box.just = "left", legend.key.size = unit(1,"line"), legend.key = element_rect(size = 0, color = 'white'), legend.text.align = 0, legend.box = "horizontal") + scale_fill_discrete("") + scale_linetype_discrete("")

library(nnet) # Package to estimate multinomial logistic regression

summary(model <- multinom(pres_approve_clean_factor ~ dem_pid3 + hetero, data=cces_network_module, weights=weight, hess=T))

predict <- effect("hetero", se=TRUE, mod = model, confidence.level = 0.95, xlevels=list(hetero=c(-1,-0.6666667,-0.5000000,-0.3333333,0.0000000,0.3333333,0.5000000,0.6666667,1.0000000)))
predict <- data.frame(predict)

# Reshape Probabilities

fit <- predict[,1:5]
lower <- predict[,c(1,18:21)]
upper <- predict[,c(1,22:25)]

colnames(fit) <- c("hetero","fit.Strongly Disapprove","fit.Somewhat Disapprove","fit.Somewhat Approve","fit.Strongly Approve")
fit <- reshape(fit,idvar="hetero",varying=c("fit.Strongly Disapprove","fit.Somewhat Disapprove","fit.Somewhat Approve","fit.Strongly Approve"),se=".",timevar="approval",times=c("Strongly Disapprove","Somewhat Disapprove","Somewhat Approve","Strongly Approve"),direction="long")

colnames(lower) <- c("hetero","lower.Strongly Disapprove","lower.Somewhat Disapprove","lower.Somewhat Approve","lower.Strongly Approve")
lower <- reshape(lower,idvar="hetero",varying=c("lower.Strongly Disapprove","lower.Somewhat Disapprove","lower.Somewhat Approve","lower.Strongly Approve"),se=".",timevar="approval",times=c("Strongly Disapprove","Somewhat Disapprove","Somewhat Approve","Strongly Approve"),direction="long")

colnames(upper) <- c("hetero","upper.Strongly Disapprove","upper.Somewhat Disapprove","upper.Somewhat Approve","upper.Strongly Approve")
upper <- reshape(upper,idvar="hetero",varying=c("upper.Strongly Disapprove","upper.Somewhat Disapprove","upper.Somewhat Approve","upper.Strongly Approve"),se=".",timevar="approval",times=c("Strongly Disapprove","Somewhat Disapprove","Somewhat Approve","Strongly Approve"),direction="long")

predict <- merge(fit,upper,by=c("hetero","approval"))
predict <- merge(predict,lower,by=c("hetero","approval"))

predict$approval <- factor(predict$approval,levels=c("Strongly Disapprove","Somewhat Disapprove","Somewhat Approve","Strongly Approve"))

ggplot(data= predict, mapping=aes(x=hetero, y=fit)) + geom_line(aes(x = hetero, y = fit), size = 0.50) + geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .2) + scale_colour_manual("",values="black") + scale_fill_manual("",values="grey12") + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + scale_x_continuous(limits=c(-1,1), breaks=c(-1,0,1), labels=c("GOP", "Neutral","Dem"), "Partisan Network Homogenity") + scale_y_continuous(limits=c(0,0.70), breaks=seq(0,0.70,0.10), "Predicted Probability of Presidential Approval") + geom_rug(data=cces_network_module, aes(x=hetero, y=1),color = "gray30", alpha=.5, size = 0.25, position='jitter') + ggtitle("Probability of Approving of President Obama by Partisan Network Homogenity") + facet_wrap(~approval, ncol = 2, scales = "free_y") 

##### Making Geographic Maps in R Section of Tutorial ##################################################

library(rgdal) # Package to read mapping shp files in R

# Let's explore the data embedded in the shape file

as_tibble(state_shp@data) # Dataframe embedded in the Spatial Polygons Data Frame

as_tibble(state_shp@data$NAME) # What type of states are in the file? We don't want territories!

state_shp <- subset(state_shp,state_shp@data$NAME != "Puerto Rico")
state_shp <- subset(state_shp,state_shp@data$NAME != "Alaska")
state_shp <- subset(state_shp,state_shp@data$NAME != "Hawaii")
state_shp <- subset(state_shp,state_shp@data$NAME != "District of Columbia")

# Preliminary look at our map

ggplot(state_shp, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "gray", color = "black") + theme_void() + coord_equal() + coord_fixed(1.3)

as_tibble(state_pres_approval) # Get a glimpse of the data structure of the presidential approval data. These are Dynamic MRP state-level estimates (see: Caughey & Warshaw (2015) for technical details on the Dynamic MRP model) of presidential approval from 2008-2017. 

# Make a percentage

state_pres_approval$state_pres_approval_percent <- state_pres_approval$median * 100

as_tibble(state_pres_approval) # Check to see if it worked. Clearly it did!

approval_2016 <- subset(state_pres_approval,state_pres_approval$year == 2016) # Subet our data

# Let's merge our data onto the map!

approval_2016 <- subset(approval_2016,select=c("state","state_pres_approval_percent"))

state_shp@data$id <- rownames(state_shp@data)
state_shp.points <- fortify(state_shp, region="id")

state_shp.df <- join(state_shp.points, state_shp@data, by="id")
state_shp.df$state <- as.character(state_shp.df$STUSPS)

state_shp.df <- merge(state_shp.df,approval_2016,by=c("state"))

ggplot(state_shp.df, aes(x = long, y = lat, group = group,fill=state_pres_approval_percent),color="white") + geom_polygon(color = "black") + theme_void() + scale_fill_gradient("Presidential Approval",low = 'red', high = 'blue') + ggtitle("Presidential Approval in the U.S. States, 2016")  + coord_equal() + coord_fixed(1.3)

##### Making Network Graphis in R Section of Tutorial ##################################################

library(network) # Package to create network objects
library(igraph) # Package to extract adjacency matrix
library(ggnet) # ggplot add-on package to create network graphs in R
library(data.table) # Package to isolate duplicates to extract Senator-specific attributes

as_tibble(senate_network) # This network data is in dyadic form, where each row indicates a pair of Senators.

adj <- graph.data.frame(senate_network[,c(8,21,43)],directed=T) # Let's create a network object! What we want to extract is a dataframe where the first column is Senator i, the second column is Senator J, and the last column is a binary variable indicating
adj <- get.adjacency(adj,attr='edge',sparse=FALSE) # Convert the edge list to an adjacency matrix
x_ergm <- network(adj, matrix.type = "adjacency", ignore.eval = FALSE,directed = F,weighted = F,names.eval="edge") # Convert the adjacency matrix to a network object

# Now let's create some attributes and load them on the network object!

y <- senate_network[,c(8,10,2)]
y1 <- senate_network[,c(21,23,1)]
colnames(y) <- c("NameFull_senator","Party_senator","State_senator")
colnames(y1) <- c("NameFull_senator","Party_senator","State_senator")
party <- rbind(y,y1)
rm(y,y1)
party$Party_senator <- ifelse(party$Party_senator == 200, "R",ifelse(party$Party_senator == 100, "D",NA))
party <- data.table(party)
party <-  party[, duplicates := 1:.N , by = c("NameFull_senator")]
party <- subset(party,party$duplicates == 1)
party <- data.frame(party)
party$duplicates <- NULL
network.vertex.names(x_ergm)
x_ergm%v%'party'<- party$Party_senator # Merge the party attributes for each Senator!
x_ergm%v%'state'<- party$State_senator # Merge the state attributes for each Senator!
x_ergm%v%"color" = ifelse(x_ergm%v%"party" == "R", "red", "blue") # Create color indicators!

ggnet2(x_ergm, label = "state", color = "grey15", label.color = "color",alpha = 0, size = 0, edge.alpha = 0.5,mode = "kamadakawai",label.size = 3) + ggtitle("113th U.S. Senate Particularistic Cosponsorship Network")

ggnet2(x_ergm, label = "vertex.names", color = "grey15", label.color = "color",alpha = 0, size = 0, edge.alpha = 0.5,mode = "kamadakawai",label.size = 3) + ggtitle("113th U.S. Senate Particularistic Cosponsorship Network")

ggnet2(x_ergm, color = "color", alpha = 0.75, size = 4, edge.alpha = 0.5,vjust = -0.6,mode = "kamadakawai") + ggtitle("113th U.S. Senate Particularistic Cosponsorship Network")