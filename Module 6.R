country<-read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)
#~/Documents/Grad School /courses/R class 2017/Module 6 data/Country_data
primates<-read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)
#~/Documents/Grad School /courses/R class 2017/Module 6 data/KamiliarAndCooperData

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#LOOKING AT DATA
head(country)
tail(country)
str(country)
dim(primates)
names(primates)
colnames(primates)
rownames(primates)
median(country$area, na.rm = TRUE) #69700
median(country$population, na.rm= TRUE) #4911766
country$pop_density<-country$population/country$area

order(country$pop_density, na.last= NA)
# most dense: 206 South Georgia and South Sandwich Islands, 86 Greenland, 70 Falkland Islands, 175 Pitcairn Islands, 146 Mongolia
# least dense: 130 Macau, 145 Monaco, 97 Holy See, 199 Singapore, 99 Hong Kong

country_order<- country[order(-country$pop_density), ]# want to order data by pop density
country_order[1:10, ]
# Most dense: Macau, Monaco, Vatican City, Singapore, Hong Kong, Gibraltar, Bahrain, Maldives, Malta, Bermuda
country_order[232:248, ]
# South Georgia and South Sandwich Islands, Greenland, Falkland Islands, Pitcairn Islands, Mongolia, Western Sahara, French Guiana, Namibia, Australia
order_area<- country[order(-country$area), ]
big<-order_area[1:20, ] # want to get largest 20 countries 
median(big$area, na.rm=TRUE) #2553320
median(big$population, na.rm = T) #43431886
new <- country[grep("^[A-F]", country$country), ] #this variable only contains country starting with A-F
mean(new$area, na.rm = TRUE)#918247.7
mean(new$population, na.rm = T) #35065172

#GRAPHS
par(mfrow = c(2, 3)) #this command makes multiple graphs appear in a grid that is two row and 3 columns
boxplot(country$population)
boxplot(log(country$population))
boxplot(country$area)
boxplot(log(country$area))
pop2<-filter(country, population !="NA")
boxplot(pop2$population)
boxplot(log(country$population))

par(mfrow = c(1, 2))  # gives us two panels
attach(country)
hist(log(population), freq = FALSE, col = "red", main = "Plot 1", xlab = "log(population size)", 
     ylab = "density", ylim = c(0, 0.2))
hist(log(population), freq = TRUE, col = "red", main = "Plot 1", xlab = "log(population size)", 
     ylab = "density")
hist(log(area), freq = FALSE, col = "red", main = "Plot 2", xlab = "log(area)", 
     ylab = "density", ylim = c(0, 0.2))

par(mfrow = c(1, 1))  # set up one panel and redraw the log(population) histogram
hist(log(population), freq = FALSE, col = "white", main = "My Plot with Mean and Density", 
     xlab = "log(population size)", ylab = "density", ylim = c(0, 0.2))
abline(v = mean(log(population), na.rm = TRUE), col = "blue")
lines(density(log(population), na.rm = TRUE), col = "green")
detach(country)

#TABLES 
sort(table(country$govt_form), decreasing = TRUE) # to find out what the most common form of govt is 

#GRAPHS again: compare native graphing and ggplot
boxplot(log(Body_mass_female_mean) ~ Family, primates) # native graphing
library(ggplot2)
p <- ggplot(data = primates, aes(x = Family, y = log(Body_mass_female_mean)))
p <- p + geom_boxplot()
p <- p + theme(axis.text.x = element_text(angle = 90))
p <- p + ylab("log(Female Body Mass)")
p

attach(primates)
par(mfrow = c(1, 2))
plot(x = Body_mass_female_mean, y = Brain_Size_Female_Mean)
plot(x = log(Body_mass_female_mean), y = log(Brain_Size_Female_Mean))
detach(primates)

p <- ggplot(data = primates, aes(x = log(Body_mass_female_mean), y = log(Brain_Size_Female_Mean), 
                          color = factor(Family)))  # first, we build a plot object and color points by Family
p <- p + xlab("log(Female Body Mass)") + ylab("log(Female Brain Size)")  # then we modify the axis labels
p <- p + geom_point()  # then we make a scatterplot
p <- p + theme(legend.position = "bottom", legend.title = element_blank())  # then we modify the legend
p  # and, finally, we plot the object
p <- p + facet_wrap(~Family, ncol = 4) # this will graph each family in a separate graph
p <- p + theme(legend.position = "none")
p

p <- p + facet_wrap(~Family, ncol = 4) # add
p <- p + theme(legend.position = "none")
p

p <- p + geom_smooth(method = "lm", fullrange = TRUE) #add trendlines
p

# AGGREGATE STATS
aggregate(primates$Body_mass_female_mean ~ primates$Family, FUN = "mean", na.rm = TRUE)
#aggregate(data~factor, FUN= function: mean, median, etc, na.rm=T)
# output is mean of female body mass for each family
#can also put:
aggregate(x = d["Body_mass_female_mean"], by = d["Family"], FUN = "mean", na.rm = TRUE)

#or using DPLYR:
highsd <- filter(primates, Family == "Hominidae" & Mass_Dimorphism > 2) #filter to see only certain rows
# filter(dataframe, what you want)
s <- arrange(primates, Family, Genus, Body_mass_male_mean)  # rearranging a data frame...
# arrange(dataframe, new order)
m <- select(primates, Family, Genus, Body_mass_male_mean)  # selecting specific columns...
# select(dataframe, columns you want)
s <- rename(primates, Female_Mass = Body_mass_female_mean)  # renaming columns...
# rename(dataframe, New name= old name)
binomial_nom <- mutate(primates, Binomial = paste(Genus, Species, sep = " ")) # and adding new columns...
# mutate(dataframe, new column name= )
a <- summarise(primates, avgF = mean(Body_mass_female_mean, na.rm = TRUE), avgM = mean(Body_mass_male_mean, na.rm = TRUE))
#to aggregate data:group then summarize
byFamily <- group_by(primates, Family) #create dataframe where data is grouped
# group_by(dataframe, column to group by)
mass <- summarise(byFamily, avgF = mean(Body_mass_female_mean, na.rm = TRUE), avgM = mean(Body_mass_male_mean, na.rm = TRUE))

#CHAINING:aka putting many steps of mutating data together
mass_detail <- mutate(primates, Binomial = paste(Genus, Species, sep = " ")) %>% 
  select(Binomial, Family, Body_mass_female_mean, Body_mass_male_mean, Mass_Dimorphism) %>% 
  group_by(Family) %>% 
  summarise(avgF = mean(Body_mass_female_mean, na.rm = TRUE), avgM = mean(Body_mass_male_mean, na.rm = TRUE), avgBMD = mean(Mass_Dimorphism,na.rm = TRUE))





