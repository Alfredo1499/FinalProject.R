#We will begin by subestting our data
keeps <- c("State", "Bedroom", "Bathroom","Area","PPSq","MarketEstimate", "RentEstimate","ListedPrice")
RealEstate1 <- RealEstate[keeps]

#We still need to subest the data into only TX and CO 
RealEstate1 %>% group_by(State) %>% summarize(count=n())


RealEstate3<-subset(RealEstate1, State %in% c('CO','TX'))
#Data is now subset by CO and TX


#we now need to make bedroom and bathroom one column 
RealEstate4 <- unite(RealEstate3, BedBath, Bedroom, Bathroom, sep = "/")

#now we can drop any missing values or "NA"
RealEstate5<-na.omit(RealEstate4)
#will need seprate data set for second question w
RealEstate6<-na.omit(RealEstate3)

#All NA Values have been removed. we can now begin to test for assumptions. 

#We will begin Our explorotory analysis 

#we will be testing for correlation between 
#Number of bedroms and bathrooms 

d <- ggplot(RealEstate6, aes(x = Bedroom, y = Bathroom))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
#by the look on the graph it seems to be postivilly 
#correlated 

cor.test(RealEstate6$Bedroom, RealEstate6$Bathroom, method="pearson", use = "complete.obs")
#P-value is less than .5 so they are significant correlated 

library("PerformanceAnalytics")

RealEstate_matr <- RealEstate6[, c(2,3,4,8)]
#we can now make a correlation matrix 

chart.Correlation(RealEstate_matr, histogram=FALSE, method="pearson")

library("corrplot")
corr_matrix <- cor(RealEstate_matr)
corr_matrix

corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.05, insig="blank")

#The correlation matrix shows NO correlation between any 
#of the variables.

# we can now begin the analysis 

#we will first answer "How does the average sales price -
# of a three bedroom house in Texas compare to that of one in 
# Colorado?" 
#we will be using an independent T-test 

#first we need to test for normality 

# we only need three bedroom houses so we will filter 
#into a new data frame
threeBedrooms <- na.omit(RealEstate6 %>% filter(Bedroom %in% c("3")))

#now we will make it numeric 
threeBedrooms$ListedPrice <- as.numeric(threeBedrooms$ListedPrice)
threeBedrooms$Bedroom <- as.numeric(threeBedrooms$Bedroom)

#now histograms to test normality 
plotNormalHistogram(threeBedrooms$ListedPrice)
#data has a positive skew to it 

#we need to transform data into normality using square root
threeBedrooms$ListedPriceSQRT <- sqrt(threeBedrooms$ListedPrice)

#graphing to check for normality again 
plotNormalHistogram(threeBedrooms$ListedPriceSQRT)

#we will try Log to be sure it is normally distributed 
threeBedrooms$ListedPriceLOG <- log(threeBedrooms$ListedPrice)

plotNormalHistogram(threeBedrooms$ListedPriceLOG)

#LOG seems to be the most normally distributed data 
#so we will use LOG 

#Now we can run analysis all assumptions have been met
# we need to first recode states 

threeBedrooms$StateR <- NA
threeBedrooms$StateR[threeBedrooms$State=='CO'] <- 0
threeBedrooms$StateR[threeBedrooms$State=='TX'] <- 1

#Now we can run analysis all assumptions have been met

t_ind <- t.test(threeBedrooms$StateR, threeBedrooms$ListedPriceLOG, alternative="two.sided", var.equal=FALSE)
print(t_ind) 

#There is a significant difference in price between 
#three bedroom homes in Texas compared to those in Colorado. 

mean_co_listed_price <- mean(threeBedrooms$ListedPrice[threeBedrooms$State == "CO"])
print(mean_co_listed_price)
mean_co_listed_price2 <- mean(threeBedrooms$ListedPrice[threeBedrooms$State == "TX"])

print(mean_co_listed_price2)

#On avaerage a three bedroom home in Colorado 
#will cost you $628,888. In Texas it will only 
# cost you $269,924 on average to buy a home. 


#Now we will answer the next question, 
#"In texas what is a stronger indicator of higher 
# rental price ,number of bedrooms and bathrooms or square footage. 

# we will use an Ancova and Simple linear regression 
#answer this question. 
library("rcompanion")
library("car")
library("effects")
library("multcomp")

#we first need to make Area categorical 
min(RealEstate6$Area)
max(RealEstate6$Area)


RealEstate6$AreaR <- NA
RealEstate6$AreaR[RealEstate6$Area>= 644] <- 0
RealEstate6$AreaR[RealEstate6$Area>= 1000] <- 1
RealEstate6$AreaR[RealEstate6$Area>= 2000] <- 2
RealEstate6$AreaR[RealEstate6$Area>= 3000] <- 3
RealEstate6$AreaR[RealEstate6$Area>= 4000] <- 4
RealEstate6$AreaR[RealEstate6$Area>= 5000] <- 5
RealEstate6$AreaR[RealEstate6$Area>= 6000] <- 6
RealEstate6$AreaR[RealEstate6$Area>= 7000] <- 7
RealEstate6$AreaR[RealEstate6$Area>= 8000] <- 8
RealEstate6$AreaR[RealEstate6$Area>= 9000] <- 9
RealEstate6$AreaR[RealEstate6$Area>= 10000] <- 10
RealEstate6$AreaR[RealEstate6$Area>= 11000] <- 11
RealEstate6$AreaR[RealEstate6$Area>= 12000] <- 12
RealEstate6$AreaR[RealEstate6$Area>= 13000] <- 13

# Area is now categorical. 

# we can begin to test for assumptions 

str(RealEstate6$AreaR)
str(RealEstate6$Bedroom)
str(RealEstate6$Bathroom)
str(RealEstate6$RentEstimate)
#we will convert all variables to factors 
RealEstate6$AreaR <- as.factor(RealEstate6$AreaR)
RealEstate6$Bedroom <- as.factor(RealEstate6$Bedroom)
RealEstate6$Bathroom <- as.factor(RealEstate6$Bathroom)
RealEstate6$RentEstimate <- as.numeric(RealEstate6$RentEstimate)

#we will test normality for rent estimate, CV
plotNormalHistogram(RealEstate6$RentEstimate)

#it looks slightly postive we will square it 
RealEstate6$RentEstimateSQRT <- sqrt(RealEstate6$RentEstimate)

plotNormalHistogram(RealEstate6$RentEstimateSQRT)
# we will stick with the original 

# we will test for homogeneity of variance 
leveneTest(RentEstimate~AreaR, data=RealEstate6)
#looks like there is significance so we will have to 
# correct for it later 

Homogeneity_RegrSlp = lm(RentEstimate~AreaR, data=RealEstate6)
anova(Homogeneity_RegrSlp)

#P-value is significant, which means the data does not 
# meet the assumption of homogeneity of regression slopes
# meaning Area will affect rent estimate no matter what 


# we can now run the analysis 


ANCOVA = lm(RentEstimate~AreaR + Bedroom*Bathroom, data=RealEstate6)
anova(ANCOVA)

#everything is significant when predicting rent estimate 
#however bathroom and bedrooms together are not strong predictors to rent estimate 



#Visulazation purposes only 

RealEstate7<-na.omit(RealEstate3)


write.csv(RealEstate7, file = "RealEstate7.csv", row.names = FALSE)
write.csv(RealEstate8, file = "RealEstate8.csv", row.names = FALSE)
write.csv(threeBedrooms, file = "threeBedrooms.csv", row.names = FALSE)

RealEstate8<-na.omit(RealEstate1)

























