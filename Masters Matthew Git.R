#Matthew Carr
#Masters data
#Fynbos, forest, fire and invasive alien trees – opportunities for improved management

#############packages###############
library(rlang)
library(tidyverse)
library(ggplot2)
library(ggfortify)
library(vegan)
library(devtools)
library(viridis)
library(viridisLite)
library(readxl)
library(MASS)
library(ggpubr)
library(dplyr)
library(ISLR) #contains Hitters dataset
library(rpart) #for fitting decision trees
library(rpart.plot) #for plotting decision trees
library(party)
library(Fselector)
library(caret)
library(xlsx)
library(data.tree)
library(caTools)
library(ggstatsplot)
library(installr)
library(e1071)
library(readr)
library(PMCMRplus)
library(rstantools)
library(randomForest)


###load data####  
v_gph <- read_xlsx("data/veg_graph.xlsx")

sep_data <- read_xlsx("data/masters_area_sep.xlsx")

veg_data <- read_xlsx("data/masters area data.xlsx")

veg_ext <- read_xlsx("data/masters_area_data_ext.xlsx")

######## graphs###############

#sum pine count compared
ggplot(sep_data, aes(x = cell_number)) + 
  geom_point(shape = 15, size = 2, aes(y = pin_count_co, colour = "control", group = 1)) +
  geom_line(size = 1, aes(y = pin_count_co, colour = "control", group = 1)) + 
  geom_point(shape = 16, size = 2, aes(y = pin_count_bf, colour = "before fire", group = 1)) +
  geom_line(size = 1, aes(y = pin_count_bf, colour = "before fire", group = 1))+ 
  geom_point(shape = 17, size = 2, aes(y = pin_count_af, colour = "after fire", group = 1)) +
  geom_line(size = 1, aes(y = pin_count_af, colour = "after fire", group = 1))+
  geom_point(shape = 18, size = 2, aes(y = pin_count_yr, colour = "year after fire", group = 1)) +
  geom_line(size = 1, aes(y = pin_count_yr, colour = "year after fire", group = 1))+
  labs(colour="vegetation")+
  xlab("Date") +
  ylab("pine count")+
  scale_y_continuous(expand = c(0,0), limits = c(0,3000))

#area of wetland compared over time
ggplot(sep_data, aes(x = wet_part)) + 
  geom_point(shape = 15, size = 2, aes(y = wet_Area_co, colour = "control", group = 1)) +
  geom_line(size = 1, aes(y = wet_Area_co, colour = "control", group = 1))+ 
  geom_point(shape = 16, size = 2, aes(y = wet_Area_bf, colour = "before fire", group = 1)) +
  geom_line(size = 1, aes(y = wet_Area_bf, colour = "before fire", group = 1))+ 
  geom_point(shape = 17, size = 2, aes(y = wet_Area_af, colour = "after fire", group = 1)) +
  geom_line(size = 1, aes(y = wet_Area_af, colour = "after fire", group = 1))+
  geom_point(shape = 18, size = 2, aes(y = wet_Area_yr, colour = "year after fire", group = 1)) +
  geom_line(size = 1, aes(y = wet_Area_yr, colour = "year after fire", group = 1))+
  labs(colour="Date")+
  xlab("Location") +
  ylab("Area sq meter")+
  scale_y_continuous(expand = c(0,0), limits = c(0,3000))


#area of patch compared over time
ggplot(sep_data, aes(x = grn_part)) + 
  geom_point(shape = 15, size = 2, aes(y = grn_Area_co, colour = "control", group = 1)) +
  geom_line(size = 1, aes(y = grn_Area_co, colour = "control", group = 1))+ 
  geom_point(shape = 16, size = 2, aes(y = grn_Area_bf, colour = "before fire", group = 1)) +
  geom_line(size = 1, aes(y = grn_Area_bf, colour = "before fire", group = 1))+ 
  geom_point(shape = 17, size = 2, aes(y = grn_Area_af, colour = "after fire", group = 1)) +
  geom_line(size = 1, aes(y = grn_Area_af, colour = "after fire", group = 1))+
  geom_point(shape = 18, size = 2, aes(y = grn_Area_yr, colour = "year after fire", group = 1)) +
  geom_line(size = 1, aes(y = grn_Area_yr, colour = "year after fire", group = 1))+
  labs(colour="Date")+
  xlab("Location") +
  ylab("Area sq meter")+
  scale_y_continuous(expand = c(0,0), limits = c(0,7000))


#area of forest compared over time
ggplot(sep_data, aes(x = frst_part)) + 
  geom_point(shape = 15, size = 2, aes(y = frst_Area_co, colour = "control", group = 1)) +
  geom_line(size = 1, aes(y = frst_Area_co, colour = "control", group = 1))+ 
  geom_point(shape = 16, size = 2, aes(y = frst_Area_bf, colour = "before fire", group = 1)) +
  geom_line(size = 1, aes(y = frst_Area_bf, colour = "before fire", group = 1))+ 
  geom_point(shape = 17, size = 2, aes(y = frst_Area_af, colour = "after fire", group = 1)) +
  geom_line(size = 1, aes(y = frst_Area_af, colour = "after fire", group = 1))+
  geom_point(shape = 18, size = 2, aes(y = frst_Area_yr, colour = "year after fire", group = 1)) +
  geom_line(size = 1, aes(y = frst_Area_yr, colour = "year after fire", group = 1))+
  labs(colour="Date")+
  xlab("Location") +
  ylab("Area sq meter")+
  scale_y_continuous(expand = c(0,0), limits = c(0,7000))


#sum veg area
ggplot(sep_data, aes(x = grph_date)) + 
  geom_point(shape = 15, size = 2, aes(y = wet_sum, colour = "wetland", group = 1)) +
  geom_line(size = 1, aes(y = wet_sum, colour = "wetland", group = 1)) + 
  geom_point(shape = 16, size = 2, aes(y = grn_sum, colour = "veg patch", group = 1)) +
  geom_line(size = 1, aes(y = grn_sum, colour = "veg patch", group = 1))+ 
  geom_point(shape = 17, size = 2, aes(y = forest_sum, colour = "Natural forest", group = 1)) +
  geom_line(size = 1, aes(y = forest_sum, colour = "Natural forest", group = 1))+
  labs(colour="vegetation")+
  xlab("Date") +
  ylab("sum sq meter")+
  scale_y_continuous(expand = c(0,0), limits = c(0,70000))

#sum of invasive counts
ggplot(sep_data, aes(x = grph_date)) + 
  geom_point(shape = 15, size = 2, aes(y = pin_sum, colour = "invasive", group = 1)) +
  geom_line(size = 1, aes(y = pin_sum, colour = "invasive", group = 1)) + 
  labs(colour="invasive species")+
  xlab("Date") +
  ylab("sum of invasive tree countr")+
  scale_y_continuous(expand = c(0,0), limits = c(0,8000))


###boxplot

#invasive pine counts
ggplot(data = v_gph, aes(x = pine_dates, y = pin_count)) +
  geom_boxplot(show.legend = FALSE, notch = FALSE) 

#All area veg data
ggplot(data = v_gph, aes(x = vegetation, y = area, fill = veg_date)) +
  geom_boxplot(show.legend = FALSE, notch = FALSE) 


####tests parameter#### 

#Seasonal data

#Looking at the species data

glimpse(veg_data )
head(veg_data )
tail(veg_data )
summary(veg_data )

###### remove missing values

veg <- veg_data(na.rm = TRUE)

#Test normality

shapiro.test(veg_data$area_co)# 2.997e-12
shapiro.test(veg_data$area_bf)# 4.485e-12
shapiro.test(veg_data$area_af)# 1.332e-13
shapiro.test(veg_data$area_yr)# 2.282e-13
shapiro.test(veg_data$elev_avg)# 0.003278

shapiro.test(veg_data$perimeter_co)# 0.003467
shapiro.test(veg_data$perimeter_bf)# 3.865e-11
shapiro.test(veg_data$perimeter_af)#  1.403e-11
shapiro.test(veg_data$perimeter_yr)# 4.953e-11
shapiro.test(veg_data$elev_low)#0.002484
shapiro.test(veg_data$elev_high)# 0.003467

##All the data not normal (p value below 0.05?) 
#need to bestandardize with log


#test homoscidisty

var(veg_data$area_co)# 7208768
var(veg_data$area_bf)# 7542741
var(veg_data$area_af)# 6585417
var(veg_data$area_yr)# 7377302
#they are homoscidistic

var(veg_data$perimeter_co)# 89062.98
var(veg_data$perimeter_bf)# 89062.98
var(veg_data$perimeter_af)# 99005.74
var(veg_data$perimeter_yr)# 101072.6
#they are homoscidistic

var(veg_data$elev_low) # 1007041
var(veg_data$elev_high)# 899306.9
var(veg_data$elev_avg)# 946212.1
#they are homoscidistic

##seperated data

#real wetlands cover in sq meters
shapiro.test(sep_data$wet_Area_co)#  0.0005484
shapiro.test(sep_data$wet_Area_bf)#  0.0007686
shapiro.test(sep_data$wet_Area_af)# 0.0001027
shapiro.test(sep_data$wet_Area_yr)# 9.326e-05
#All the data not normal (p value below 0.05?) 

#green patches
shapiro.test(sep_data$grn_Area_co)# 3.055e-07
shapiro.test(sep_data$grn_Area_bf)#5.849e-07
shapiro.test(sep_data$grn_Area_af)# 3.081e-08
shapiro.test(sep_data$grn_Area_yr)# 8.678e-08
#All the data not normal (p value below 0.05?) 

#nat forest cover
shapiro.test(sep_data$frst_Area_co)# 3.36e-06
shapiro.test(sep_data$frst_Area_bf)# 3.477e-06
shapiro.test(sep_data$frst_Area_af)#1.908e-06
shapiro.test(sep_data$frst_Area_yr)# 2.73e-06
#all cover is not normal (p smaller than 0.05)

#pine count
shapiro.test(sep_data$pin_count_co)#  0.8888
shapiro.test(sep_data$pin_count_bf)# 0.9204
shapiro.test(sep_data$pin_count_af)# 0.1702
shapiro.test(sep_data$pin_count_yr)# 0.876
shapiro.test(sep_data$drone_count)#0.9187
#all counts p is larger than 0.05, is normally dist

#########var for homosci

#real wetalnds cover in sq metres
var(sep_data$wet_Area_co,na.rm = TRUE)#  372541.6
var(sep_data$wet_Area_bf,na.rm = TRUE)#  417396.3
var(sep_data$wet_Area_af, na.rm = TRUE)# 99762.24
var(sep_data$wet_Area_yr, na.rm = TRUE)# 197164.9
#not compli homosc, nr 2 and nr is 4x bigger

#green patches
var(sep_data$grn_Area_co,na.rm = TRUE)#   11986912
var(sep_data$grn_Area_bf,na.rm = TRUE)#   12803911
var(sep_data$grn_Area_af, na.rm = TRUE)# 11495393
var(sep_data$grn_Area_yr, na.rm = TRUE)# 14512255
#is homsc

#nat forest cover
var(sep_data$frst_Area_co,na.rm = TRUE)#  6755425
var(sep_data$frst_Area_bf,na.rm = TRUE)#  6662427
var(sep_data$frst_Area_af, na.rm = TRUE)# 5846977
var(sep_data$frst_Area_yr, na.rm = TRUE)# 4612653
#is homosc

#pine count
var(sep_data$pin_count_co, na.rm = TRUE)# 559622
var(sep_data$pin_count_bf, na.rm = TRUE)# 488201.2
var(sep_data$pin_count_af, na.rm = TRUE)# 18559.3
var(sep_data$pin_count_yr, na.rm = TRUE)# 5631.7
var(sep_data$drone_count, na.rm = TRUE)# 599542.7

#not homsc, nr 1 an 2 too large

####t tests####

#WETLAND t test,
#BF and CO
t.test(sep_data$wet_Area_bf, sep_data$wet_Area_co, paired = TRUE, var.equal = FALSE, na.rm = TRUE)#  t test 0.01189
#BF and AF
t.test(sep_data$wet_Area_bf, sep_data$wet_Area_af, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 0.000494
#BF and YR
t.test(sep_data$wet_Area_bf, sep_data$wet_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 3.052e-05 or 0.0002541
#AF and YR
t.test(sep_data$wet_Area_af, sep_data$wet_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 3.052e-05 or 0.00403
#all null hypothesis rejected, p values smaller than 0.05

#GREEN PATCH t test,
#BF and co
t.test(sep_data$grn_Area_bf, sep_data$grn_Area_co, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# p-value = 0.08424
#BF and AF
t.test(sep_data$grn_Area_bf, sep_data$grn_Area_af, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 5.876e-05-09 
#BF and YR
t.test(sep_data$grn_Area_bf, sep_data$grn_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0.008284 or 0.04501
#AF and YR
t.test(sep_data$grn_Area_af, sep_data$grn_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0.0002297
#all null hypothesis rejected, p values smaller than 0.05, except constant

#NATURAL FOREST t test,
#BF and co
t.test(sep_data$frst_Area_bf, sep_data$frst_Area_co, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 0.477
#BF and AF
t.test(sep_data$frst_Area_bf, sep_data$frst_Area_af, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 2.384e-07 or t test 0.001168
#BF and YR
t.test(sep_data$frst_Area_bf, sep_data$frst_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 5.96e-06 or 0.01166
#AF and YR
t.test(sep_data$frst_Area_af, sep_data$frst_Area_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0.1789 or 0.1224
#nr2 & 3 has p value smaller than 0.05, rejecting null hypothesis 
#nr4, 1 (AF and YR, co) is larger than 0.05, thus no difference and null hypothesis is accepted

#iNVASIVE PINES COUNT t test,
#BF and co
t.test(sep_data$pin_count_bf, sep_data$pin_count_co, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 0.3354 or wilcox  0.0625 for all 
#BF and AF
t.test(sep_data$pin_count_bf, sep_data$pin_count_af, paired = TRUE, var.equal = FALSE, na.rm = TRUE)# 0.01101 or wilcox  0.0625 for all 
#BF and YR
t.test(sep_data$pin_count_bf, sep_data$pin_count_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0.01347
#AF and YR
t.test(sep_data$pin_count_af, sep_data$pin_count_yr, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0. 0.1012
#Drone vs bf ssat
t.test(sep_data$pin_count_bf, sep_data$drone_count, paired = TRUE,var.equal = FALSE, na.rm = TRUE)
#nr2 & 3 has p value smaller than 0.05, rejecting null hypothesis 
#nr4 and 1 (AF and YR, co) is larger than 0.05, thus no difference and null hypothesis is accepted

#t test between control and before all area
#wilcox.test(veg_data$area_bf, veg_data$area_co, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0. 0.1012


####ANOVA and post hoc tests####

#standardise
hist(v_gph$area)
####standerdize
vglog<- log10(v_gph$area +1)

hist(vglog)

#area 1 column anova
vgaov <-aov(vglog ~ vegetation + veg_date, data = v_gph)
summary(vgaov)
#sig dif between area of the vegetation, and area at diff time

TukeyHSD(vgaov)
vghoc <-TukeyHSD(vgaov)

plot(vghoc) 

##ELEVATION
shapiro.test(v_gph$elev_avg)# 6.133e-09

#standardise
hist(v_gph$elev_avg)

vgelev<- sqrt(max(v_gph$elev_avg + 1) - v_gph$elev_avg)
hist(vgelev)

#ANOVA showing elevation role
elev_aov <- aov(vgelev ~ vegetation + area, data = v_gph)
summary(elev_aov)
# no sig dif  between area of veg at different height but the veg type has sig difference

elevhoc<- TukeyHSD(elev_aov, which = "vegetation")

plot(elevhoc)

###vegetation area at different time periods

hist(veg_data$area_co) 
hist(veg_data$area_bf) 
hist(veg_data$area_af) 
hist(veg_data$area_yr) 
#skewed to left so log10
#summary(log10(veg_data$area_bf +1)) # or filter c()

#standerdize
logco <- log10(veg_data$area_co +1)
logbf <- log10(veg_data$area_bf +1)
logaf <- log10(veg_data$area_af +1)
logyr <- log10(veg_data$area_yr +1)

hist(logbf)

#pre fire ancova
aovall <-aov(logbf ~ vegetation + logco + logaf + logyr, data = veg_data)
summary(aovall)
#sig dif between area of the vegetation, area yr and co(?)

TukeyHSD(aovall, which = "vegetation")


###sum for area aov, standardize
aovsum <- aov(sum ~ sum_dates + sum_name, data = sep_data)
summary(aovsum)

TukeyHSD(aovsum, conf.level=.95)
forplot <- TukeyHSD(aovsum, conf.level=.95)
#From the output, we can see that the only p-value ("p adj") less than 0.05 those pairs are significantly different from each other.

plot(forplot) 

#### ggstastsplot######

#area of veg
ggbetweenstats(
  data  = v_gph,
  x     = vegetation,
  y     = area,
  title = "area of different veg")

#area over time

ggbetweenstats(
  data  = v_gph,
  x     = veg_date,
  y     = area,
  title = "area of different veg")

#############count data tests###########################

x <- data.frame(sep_data$pin_count_bf, sep_data$drone_count) 
x1 <- na.omit(x)

#t test (0.1748)
t.test(x1$sep_data.pin_count_bf, x1$sep_data.drone_count, paired = TRUE,var.equal = FALSE, na.rm = TRUE)# 0.1748
#p value larger than 0.05, null hypothesis is accepted, no sign difference between counts

#chisquare test 
chisq.test(x1)# p-value < 2.2e-16
#null: variables are independent, alternative: variable are not independent 
#P value below 0.05, so suggests that the association between the 2 variables is significant
#meaning they are not independent and are associated with each other in some way

#Glm 
#poisson regression
p_count <- glm(sep_data.pin_count_bf ~ sep_data.drone_count, family = "poisson", data = x1 )
summary(p_count) #sign difference, Intercept <2e-16 *** and sep_data.drone_count <2e-16 ***

#check if var is lager than mean
mean_sat <- mean(x1$sep_data.pin_count_bf)
vat_sat <- var(x1$sep_data.pin_count_bf)
ratio_sat <- vat_sat/mean_sat
print(ratio_sat)# lager than 1 is  var larger than mean (=356.9244), so it is var larger 

#negative binomial regression (var is greater than mean)
p2_count <- glm.nb(sep_data.pin_count_bf ~ sep_data.drone_count, data = x1 )
summary(p2_count)# (Intercept)    <2e-16 ***,sep_data.drone_count    0.0077 ** 
#has sign difference

#################EXTRA cor test######################################

cor.test(sep_data$pin_count_bf, sep_data$pin_count_af)



########EXTRA Regression tree#####################
set.seed(123)
category1 <- v_gph$vegetation
category2 <- v_gph$veg_date
dependent_var <- v_gph$area
#yr <- veg_data$area_yr

rfdata <- data.frame(category1, category2, dependent_var)
head(rfdata)

data <- rfdata[sample(nrow(rfdata)), ]
head(data)

data$category1 <- factor(data$category1)
data$category2 <- factor(data$category2)

# Separate data into training and testing sets
train_index <- sample(nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


#In this example, we have split the data into a training set (70% of the data) and a testing set (30% of the data).
#We have then used the randomForest() function to build a random forest model with "dependent_var" as the dependent 
#variable, "category1" and "category2" as the independent variables.
#Finally, we can use the model to make predictions on the testing set and evaluate its performance:


# Build random forest model
rf_model <- randomForest(dependent_var ~ category1 + category2, data = train_data)

# Make predictions on testing set
rf_pred <- predict(rf_model, newdata = test_data)

# Calculate mean squared error
mse <- mean((test_data$dependent_var - rf_pred)^2)
mse


#In this example, we have used the predict() function to generate predictions 
#on the testing set, and we have calculated the mean squared error between the predicted 
#values and the actual values. The lower the mean squared error, the better the performance of the model.
#That's it! You now have a random forest model that can be used to predict values of the dependent 
#variable based on the two categorical variables.




#We then use the predict() function to generate a prediction based on the 
#random forest model, and store the result in the prediction variable. In this
#case, the predicted value of the dependent variable is returned as the result.



par(mar = c(5, 5, 2, 2))

# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance Plot")

#The resulting plot will show a bar chart of the relative importance of 
#each independent variable in predicting the dependent variable, ranked in 
#descending order. The variable importance is measured by the decrease in accuracy 
#of the model when a variable is randomly permuted. The more important the variable,
#the greater the decrease in accuracy when it is randomly permuted.
