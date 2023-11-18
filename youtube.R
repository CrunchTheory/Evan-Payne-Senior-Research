library(readr) 
library(dplyr)

##########################################################################
# Code to import data for use in the analyses and visualizations
# This dataset can be downloaded from kaggle.com at the following URL: 
# https://www.kaggle.com/datasets/nelgiriyewithana/global-youtube-statistics-2023
##########################################################################

youtube <- read_csv("C:/Users/evanp/Dropbox/Evan Documents/CSC 450 Senior Research/YouTubeStats/Global YouTube Statistics.csv")
View(youtube)
colnames(youtube)
nrow(youtube)
ncol(youtube)

##########################################################################
# The dataset had outliers for each variable that made the visualizations
# difficult to see. The following code removed the upper and lower outliers
# using the methods described on the following website:
# https://universeofdatascience.com/how-to-remove-outliers-from-data-in-r/
##########################################################################

#Takes initial dataset and removes upper and lower *Video Views* outliers
quartiles<-quantile(youtube$`video views`, probs = c(.25, .75))
IQR<-IQR(youtube$`video views`)
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
data_no_outlier<-subset(youtube,youtube$`video views`>Lower & youtube$`video views`<Upper)
View(data_no_outlier)
dim(data_no_outlier)

#Takes No Outliers dataset and removes upper and lower *Uploads* outliers
quartiles<-quantile(data_no_outlier$`uploads`, probs = c(.25, .75))
IQR<-IQR(data_no_outlier$`uploads`)
Lower<-quartiles[1]-1.5*IQR
Upper<-quartiles[2]+1.5*IQR
data_no_outlier_uploads<-subset(data_no_outlier,data_no_outlier$`uploads`>Lower & data_no_outlier$`uploads`<Upper)
View(data_no_outlier_uploads)
dim(data_no_outlier_uploads)

#Takes No Outliers dataset and removes upper and lower *Views Last 30 Days* outliers
data_no_outlier2<-subset(data_no_outlier, data_no_outlier$video_views_for_the_last_30_days != "NaN")
View(data_no_outlier2)
dim(data_no_outlier2)
quartiles3<-quantile(data_no_outlier2$video_views_for_the_last_30_days, probs = c(.25, .75))
IQR3<-IQR(data_no_outlier2$video_views_for_the_last_30_days)
Lower3<-quartiles3[1]-1.5*IQR3
Upper3<-quartiles3[2]+1.5*IQR3
data_no_outlier_30_days<-subset(data_no_outlier2,data_no_outlier2$video_views_for_the_last_30_days>Lower3 & data_no_outlier2$video_views_for_the_last_30_days<Upper3)
View(data_no_outlier_30_days)
dim(data_no_outlier_30_days)

##########################################################################
# This code looks at the different categories and how many YouTube
# channels are found in each category from the top 1000 YouTube channels.
# This code was used to create Table 1.
##########################################################################

#HOW MANY CHANNELS ARE IN EACH CATEGORY
youtubecat<-table(youtube$category)
prop.table(youtubecat)

datanooutliercat<-table(data_no_outlier$category)
datanooutliercat
prop.table(datanooutliercat)

outliercat<-table(upper_outliers$category)
outliercat
prop.table(outliercat)

##########################################################################
# Because the Upper and Lower Outliers are of interest
# this code creates subsets for the outliers
##########################################################################

#CREATE SUBSETS FOR UPPER AND LOWER OUTLIERS
upper_outliers<-subset(youtube, youtube$`video views`> Upper)
View(upper_outliers)
dim(upper_outliers)
lower_outliers<-subset(youtube, youtube$`video views`< Lower)
View(lower_outliers)
dim(lower_outliers)

##########################################################################
# The following code finds the mean, median, and standard deviations
# for the main variables in the study
# Video Views, Uploads, Video Views in Last 30 Days, Highest Monthly Earnings,
# Lowest Monthly earnings, Subscribers, and Subscribers in the Last 30 Days
# The mean, median, and sd are found for both the main dataset (data_no_outlier)
# and for the upper outliers. NOTE: There were no lower outliers.
#
# This code was used to create Tables 2, 3, and 4.
##########################################################################

#Mean, Median, SD for Data with Video Views Outliers Removed
mean(data_no_outlier$`video views`)
median(data_no_outlier$`video views`)
sd(data_no_outlier$`video views`)

mean(data_no_outlier$uploads)
median(data_no_outlier$uploads)
sd(data_no_outlier$uploads)

mean(data_no_outlier$video_views_for_the_last_30_days, na.rm = TRUE)
median(data_no_outlier$video_views_for_the_last_30_days, na.rm = TRUE)
sd(data_no_outlier$video_views_for_the_last_30_days, na.rm = TRUE)

mean(data_no_outlier$highest_monthly_earnings)
median(data_no_outlier$highest_monthly_earnings)
sd(data_no_outlier$highest_monthly_earnings)

mean(data_no_outlier$lowest_monthly_earnings)
median(data_no_outlier$lowest_monthly_earnings)
sd(data_no_outlier$lowest_monthly_earnings)

mean(data_no_outlier$subscribers)
median(data_no_outlier$subscribers)
sd(data_no_outlier$subscribers)

mean(data_no_outlier$subscribers_for_last_30_days, na.rm = TRUE)
median(data_no_outlier$subscribers_for_last_30_days, na.rm = TRUE)
sd(data_no_outlier$subscribers_for_last_30_days, na.rm = TRUE)

mean(data_no_outlier$created_year, na.rm = TRUE)
median(data_no_outlier$created_year, na.rm = TRUE)
sd(data_no_outlier$created_year, na.rm = TRUE)

#Mean, Median, SD for Upper Outliers
mean(upper_outliers$`video views`)
median(upper_outliers$`video views`)
sd(upper_outliers$`video views`)

mean(upper_outliers$uploads)
median(upper_outliers$uploads)
sd(upper_outliers$uploads)

mean(upper_outliers$video_views_for_the_last_30_days, na.rm = TRUE)
median(upper_outliers$video_views_for_the_last_30_days, na.rm = TRUE)
sd(upper_outliers$video_views_for_the_last_30_days, na.rm = TRUE)

mean(upper_outliers$highest_monthly_earnings)
median(upper_outliers$highest_monthly_earnings)
sd(upper_outliers$highest_monthly_earnings)

mean(upper_outliers$lowest_monthly_earnings)
median(upper_outliers$lowest_monthly_earnings)
sd(upper_outliers$lowest_monthly_earnings)

mean(upper_outliers$subscribers)
median(upper_outliers$subscribers)
sd(upper_outliers$subscribers)

mean(upper_outliers$subscribers_for_last_30_days, na.rm = TRUE)
median(upper_outliers$subscribers_for_last_30_days, na.rm = TRUE)
sd(upper_outliers$subscribers_for_last_30_days, na.rm = TRUE)

mean(upper_outliers$created_year, na.rm = TRUE)
median(upper_outliers$created_year, na.rm = TRUE)
sd(upper_outliers$created_year, na.rm = TRUE)

##########################################################################
# Main Visualizations for the data
# Using ggplot2 to create side-by-side boxplots
#
# This code was used to create Figures 1, 2, 3, and 4.
##########################################################################

#SIDE BY SIDE BOX PLOTS DATA_NO_OUTLIER
library(ggplot2)

ggplot(data_no_outlier, aes(x=category, y=`video views`, fill=category)) +
  geom_boxplot() +
  ggtitle("Views by Category")+
  labs(x="Category", y="Video Views")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5), legend.position="none")

ggplot(data_no_outlier_30_days, aes(x=category, y=`video_views_for_the_last_30_days`, fill=category)) +
  geom_boxplot() +
  ggtitle("Views by Category")+
  labs(x="Category", y="Video Views Last 30 Days")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5), legend.position="none")

ggplot(data_no_outlier_uploads, aes(x=category, y=`uploads`, fill=category)) +
  geom_boxplot() +
  ggtitle("Views by Category")+
  labs(x="Category", y="Uploads")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5), legend.position="none")

#Creating a factor for the Year of Channel Creation
max(data_no_outlier$created_year, na.rm = TRUE)
data_no_outlier$group <- as.factor(ifelse(data_no_outlier$created_year==2005, '2005', 
                                  ifelse(data_no_outlier$created_year==2006, '2006',
                                  ifelse(data_no_outlier$created_year==2007, '2007',
                                  ifelse(data_no_outlier$created_year==2008, '2008',
                                  ifelse(data_no_outlier$created_year==2009, '2009',
                                  ifelse(data_no_outlier$created_year==2010, '2010',
                                  ifelse(data_no_outlier$created_year==2011, '2011',
                                  ifelse(data_no_outlier$created_year==2012, '2012',
                                  ifelse(data_no_outlier$created_year==2013, '2013',
                                  ifelse(data_no_outlier$created_year==2014, '2014',
                                  ifelse(data_no_outlier$created_year==2015, '2015',
                                  ifelse(data_no_outlier$created_year==2016, '2016',
                                  ifelse(data_no_outlier$created_year==2017, '2017',
                                  ifelse(data_no_outlier$created_year==2018, '2018',
                                  ifelse(data_no_outlier$created_year==2019, '2019',
                                  ifelse(data_no_outlier$created_year==2020, '2020', 
                                  ifelse(data_no_outlier$created_year==2021, '2021','2022'))))))))))))))))))
                                                       
ggplot(data_no_outlier, aes(x=data_no_outlier$group, y=`video views`, fill=data_no_outlier$group)) +
  geom_boxplot() +
  ggtitle("Views by Year Created")+
  labs(x="Year Channel Created", y="Video Views")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5), legend.position="none")

##########################################################################
# Unused code that can create side-by-side boxplots for the Upper Outliers
##########################################################################

#SIDE BY SIDE BOX PLOTS UPPER_OUTLIERS
ggplot(upper_outliers, aes(x=category, y=`video views`, fill=category)) +
  geom_boxplot() +
  ggtitle("Views by Category")+
  labs(x="Category", y="Video Views")+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))


##########################################################################
# Code to create the correlation matrix
# First, a subset is created of the variables that are numerical
# These variables are the main variables for the correlations:
# Video Views, Uploads, Video Views in Last 30 Days, Highest Monthly Earnings,
# Lowest Monthly earnings, Subscribers, and Subscribers in the Last 30 Days
#
# This code was used to create Table 5.
##########################################################################

data_no_outlier.numerical<-select(data_no_outlier, `video views`, `uploads`, `video_views_for_the_last_30_days`, `highest_monthly_earnings`, `lowest_monthly_earnings`, `subscribers`, `subscribers_for_last_30_days` )

#CORRELATION MATRIX

data_no_outlier.cor <- cor(data_no_outlier.numerical, method = "spearman", use = "complete.obs")
View(data_no_outlier.cor)
data_no_outlier_matrix<-as.matrix(data_no_outlier2.cor)
heatmap(data_no_outlier_matrix, Rowv = NA, Colv = NA)
heatmap(data_no_outlier_matrix, scale="row",
          key=TRUE, symkey=FALSE, Rowv = NA, Colv = NA, density.info="none",cexRow=1,cexCol=1,margins=c(12,8),trace="none", srtCol=45)


##########################################################################
# These data get the means and median for each category of Channel.
# These data were used to create Table 6.
##########################################################################

library(dplyr)
df<-data_no_outlier %>% group_by(category) %>% summarise_at(vars(`video views`), list(name=mean))
df
df2<-data_no_outlier %>% group_by(category) %>% summarise_at(vars(`video views`), list(name=median))
df2

df3<-data_no_outlier_30_days %>% filter(data_no_outlier$video_views_for_the_last_30_days != 'NaN') %>% group_by(category) %>% summarise_at(vars(`video_views_for_the_last_30_days`), list(name=mean))
df3
df4<-data_no_outlier_30_days %>% filter(data_no_outlier$video_views_for_the_last_30_days != 'NaN') %>% group_by(category) %>% summarise_at(vars(`video_views_for_the_last_30_days`), list(name=median))
df4

yearsonly <-data_no_outlier %>% filter(data_no_outlier$group != 'NA')
View(yearsonly)

df5<-yearsonly %>% group_by(yearsonly$group) %>% summarise_at(vars(`video views`), list(name=mean))
df5
df6<-yearsonly %>% group_by(yearsonly$group) %>% summarise_at(vars(`video views`), list(name=median))
df6

year.num<-table(data_no_outlier$group)
prop.table(year.num)

##########################################################################
# This code runs an ANOVA on the means for the Video Views by Category
##########################################################################

#ANOVA comparison of means

categories <-aov(`video views` ~ `category`, data = data_no_outlier)
summary(categories)

##########################################################################
# This code is for stuff I am still working on
##########################################################################

#Still working on pairwise comparisons
pwc<- pairwise.t.test(data_no_outlier$`video views`,data_no_outlier$category, p.adjust.method = "holm")
pwc

#Trying to make this ANOVA work, but I have to used the factors created for each year.
years <-aov(`video views` ~ yearsonly$group, data = yearsonly)
years

anova(lm(`video views` ~ yearsonly$group, data = yearsonly))

hist(data_no_outlier$`video views`, main="Histogram of Video Views", xlab="2023 Video Views")

