#' ---
#' title: 'Labor Market Analysis'
#' author: 'From: Mansi Agarwal, 2nd TeamMember, 3rd TeamMember'
#' date: '`r format(Sys.time(), "%B %d, %Y")`'
#' output:
#'   md_document:
#'     variant: markdown_github 
#'     toc: true
#'     toc_depth: '6'
#' ---
#' ***
#' ## Code File Basics
#' 
#' ### Code Header
## ------------------------------------------------------------------------
# Course: ECON 5300
# Title: Labour Market Analysis 
# Purpose: 
# Research Question: 

#' 
#' ### Clear Environment and Load packages 
#' 
## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear working directory (remove all objects)
rm(list=ls(all=TRUE)) 

# Load packages
library(tidyverse)
library(gridExtra)
library(GGally)
library(knitr)
library(grid)
library(reshape)
library(reshape2)
library(psych)
library(corrplot)
library(lmtest)
library(sandwich)

#' 
#' ## Load Data to begin EDA
#' 
## ------------------------------------------------------------------------
# Import LMA data
LMA.Data <- read.csv("ACS Data for LMA Project-2.csv", header = TRUE)


#' 
#' ### Analyse the Structure & Summary of data
## ---- message=FALSE, warning=FALSE---------------------------------------

#Structure of the dataset
str(LMA.Data)

#Summary of the data set ( i.e. Descriptive Statistics of the data)
summary(LMA.Data)

# Check the details of different variables
describe(LMA.Data)

#' 
#' Observations :  
#' 
#' 1. Earnings probably have right skewed distribution.
#' 2. Maximum number of individuals have High School degree .
#' 3. Majority of the individuals in our sample data are White.
#' 4. Age of the individuals in our sample data varies between 18 to 64.
#' 5. Usual Weekly Hours of the individuals in our sample data varies between 1 to 99.  
#' 
#' ### Analyse the Visual Summary of data 
#' 
## ---- fig.width=13, fig.height=6-----------------------------------------
# Visualize numerical variables

par(mfrow=c(3,3))
  hist(LMA.Data$Age, main = "Histogram of Age")
  hist(LMA.Data$Earnings.Past.12.Months, main = "Histogram of Earnings")
  hist(LMA.Data$Usual.Weekly.Hours, main = "Histogram of Usual.Weekly.Hours")
  hist(LMA.Data$Female, main = "Histogram of Gender")
  hist(LMA.Data$Married, main = "Histogram of Marital-Status")
  hist(LMA.Data$Employed, main = "Histogram of Employment-Status")
  hist(LMA.Data$Worked.40..Weeks.During.Past.12.Months, main = "Histogram of Work-duration (40 weeks in a year)")
  hist(LMA.Data$Worked.35..Hours.in.a.Typical.Week, main = "Histogram of Work-duration (35 hours in a week)")

#' 
#' Observations :
#' 
#' 1. It is confirmed that the earning have right skewed distribution. Hence we need to take the log in statistical model to normalise the data.   
#' 2. On an average people usually work for 38 hours.  
#' 
#' ### Plot the Categorical variables
## ----fig.width=12, fig.height=7------------------------------------------

# Plot factor variables
par(mfrow=c(3,2))
  barplot(table(LMA.Data$Educational.Attainment),   main = "Educational Attainment")
  barplot(table(LMA.Data$Race.Ethnicity),           main = "Race/Ethnicity")
  barplot(table(LMA.Data$Married),                  main = "Marital Status")
  barplot(table(LMA.Data$Female),                   main = "Gender")
  barplot(table(LMA.Data$Employed),                 main = "Employed")


#' 
#' Observations :  
#' 
#' 1. Most number of individuals (i.e 17939) just have High School Degree.  
#' 2. Sample Data is equally distributed between male and female.
#' 3. 84.6% of the individuals in the sample data are employed.
#' 
#' 
#' ## Detailed EDA to examine the observations :
#' 
#' ### How different variables are Correlated?
#' 
## ---- message=FALSE, warning=FALSE---------------------------------------
LMA.Data %>%
 select(Age,Earnings.Past.12.Months,Female,Married,Employed) %>%
  ggpairs()

#' 
#' Observations : 
#' 
#' 1. High positive correlation between "Age & Married", "Age & Earnings", "Earnings & Married".  
#' 2. Negative correlation between "Gender & Eanrings".   
#' 
#' 
#' ### Employed/Unemployed Labour Force by Education Attainment
#' 
## ----fig.width=13, fig.height=5------------------------------------------
A<-
LMA.Data %>%
  filter(Employed == 1) %>%
  group_by( Educational.Attainment ) %>%
  summarise(  count = n(),
              Proportion = sum(count) / nrow(LMA.Data),
              PercentEmployed = Proportion * 100) %>%
  select(Educational.Attainment,count,PercentEmployed) %>%
    ggplot(aes(x=reorder(Educational.Attainment, count), y=count, fill="red")) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=FALSE) +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Employed Labour Force by Education Attainment") +
    xlab("Education Attainment") +
    ylab("Count of Employed Individuals") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

B<-
LMA.Data %>%
  filter(Employed == 0) %>%
  group_by( Educational.Attainment ) %>%
  summarise(  count = n(),
              Proportion = sum(count) / nrow(LMA.Data),
              PercentEmployed = Proportion * 100) %>%
  select(Educational.Attainment,count,PercentEmployed) %>%
    ggplot(aes(x=reorder(Educational.Attainment, count), y=count, fill="red")) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=FALSE) +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Unemployed Labour Force by Education Attainment") +
    xlab("Education Attainment") +
    ylab("Count of Unemployed Individuals") +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

grid.arrange(A,B, nrow=1)

#' 
#' Observations :  
#' 
#' 1. Only 27 Doctrate Degree Holders are UnEmployed.
#' 2. Small number of individuals holding post Secondary Degree are  UnEmployed.  
#' 
#' ### Labour Force Participation by Race/Ethnicity
#' 
## ------------------------------------------------------------------------
LMA.Data %>%
  group_by( Race.Ethnicity ) %>%
  summarise(  count = n(),
              Proportion = sum(count) / nrow(LMA.Data),
              PercentEmployed = Proportion * 100) %>%
  select(Race.Ethnicity,count,PercentEmployed) %>%
    ggplot(aes(x=reorder(Race.Ethnicity, count), y=count, fill="red")) +
    geom_bar(stat = "identity", position = "dodge") +
    guides(fill=FALSE) +
    geom_text(aes(label = count) , size = 5, vjust = -0.1, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("Labour Force Participation by Race/Ethnicity") +
    xlab("Race/Ethnicity") +
    ylab("Count of Economically Active Individuals") +
    #theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
    theme(plot.title = element_text(size = 13, face = "bold", hjust= 0.5)) + 
    theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))


#' 
#' Observations : 
#' 
#' 1. Most of the individuals in the sample data are White.  
#' 
#' ### Impact of Higher education on earnings
#' 
## ------------------------------------------------------------------------
kable(
LMA.Data %>%
  filter(Earnings.Past.12.Months > 0 & Employed == 1) %>%
  group_by(Educational.Attainment) %>%
  summarise(Avg.Earning = round(mean(Earnings.Past.12.Months),0),
            Median.Earning = median(Earnings.Past.12.Months),
            Avg.Weekly.Hours = round(mean(Usual.Weekly.Hours),1),
            Median.Weekly.Hours = median(Usual.Weekly.Hours)) %>%
  arrange(desc(Median.Earning)),
align = "l",
format = "html",
caption = "Higher education results in Good Jobs & Higher Earnings")


#' 
#' ### Earnings by Educational Attainment for Employed Individuals
#' 
## ----fig.width=15,fig.height=5-------------------------------------------
G1<-
LMA.Data %>%
  filter(Employed == 1 & Earnings.Past.12.Months > 0) %>%
  #filter(Educational.Attainment == "Bachelors Degree" & Race.Ethnicity != "White") %>%
  ggplot(aes(x=Educational.Attainment, y=Earnings.Past.12.Months, fill = "#4271AE")) + 
  geom_boxplot(color = "#1F3552") +
  scale_y_continuous(name = "Employed Indivuduals Earnings",
                     breaks = seq(4, 577000, 100000)) +
  scale_x_discrete(name = "Educational Attainment") +
  theme_bw() +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
  ggtitle("Individuals with Professional Degree holder Earns the Most",
          subtitle = "Earnings by Education Level")

G2<-   
LMA.Data %>%
  filter(Employed == 1 & Earnings.Past.12.Months > 0) %>%
  ggplot(aes(x=Educational.Attainment, y=Age, fill = "#4271AE")) + 
  geom_boxplot(color = "#1F3552") +
  scale_y_continuous(name = "Employed Indivuduals Age", breaks = seq(10, 70, 5)) +
  scale_x_discrete(name = "Educational Attainment") +
  theme_bw() +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
  ggtitle("Most of the Individuals with Professional or Doctorate or Masters Degree\n are older than other degree holders",
          subtitle = "Education Level by Age")
    
grid.arrange(G1,G2, nrow=1)

#' 
#' ## Subset the Data that satisfies the sample criteria
## ------------------------------------------------------------------------

LMA.Data <- LMA.Data %>%  
          #Filter the number of observations for which employed individuals have < 1000 USD in earnings or the indivuduals are unemployed - 44732
               filter(Earnings.Past.12.Months > 1000 & Employed == 1) %>%
          # Filter the number of individuals those who belongs to either Hawaiian or Other Race
               filter(!(Race.Ethnicity %in% c("Other Race", "Hawaiian","Biracial", "Native American")))


#' 
#' ### How do Earnings vary by Age? Do older people earn more?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5----

# Create Age Groups
lma_age_data <- LMA.Data %>% mutate(Age_Group = cut(Age, 4)) %>% 
                  group_by(Age_Group) %>% 
                  summarise(Num_People      = n(),
                            Median_Earnings = median(Earnings.Past.12.Months),
                            Pop_percentage  = (Num_People/nrow(LMA.Data))*100)

# Plot Number of people by Age
g1 <- lma_age_data %>%
        ggplot(aes(x = Age_Group, y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        xlab("Age") +
        ylab("Percentage Population by Age") +
        ggtitle("How do Population vary by Age?") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))


# Plot Earnings by Age
g2 <- lma_age_data %>%
        ggplot(aes(x = Age_Group, y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        xlab("Age") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Age?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
   
grid.arrange(g1,g2, nrow=1)

#' 
#' Observations : 
#' 
#' 1. Number of Employed Individials belong to the Age between 41 to 52.5. They also have highest median earnings.  
#' 
#' ### How do Earnings vary by Race?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=12, fig.height=5----

lma_race_data <- LMA.Data %>%  
                  group_by(Race.Ethnicity) %>% 
                  summarise(Num_People      = n(),
                            Median_Earnings = median(Earnings.Past.12.Months),
                            Pop_percentage  = (Num_People/nrow(LMA.Data))*100) 

# Plot Earnings by Race
g3 <- lma_race_data %>%
        ggplot(aes(x = reorder(Race.Ethnicity, Num_People), y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Population Percentage") +
        ggtitle("Population Percentage by Race") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=12),
              axis.title.x = element_blank())

# Plot Earnings by Race
g4 <- lma_race_data %>% 
        ggplot(aes(x = reorder(Race.Ethnicity, Median_Earnings), y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Race?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=12),
              axis.title.x = element_blank())
   
grid.arrange(g3,g4, nrow=1)

#' 
#' Observations : 
#' 
#' 1. Asians have the highest median earnings among White, Black & Hispanic Individuals. 
#' 
#' ### How do Earnings vary by Gender? Do Male(s) earn more than Female(s)?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=12, fig.height=5----

lma_gender_data <- LMA.Data %>%  
                  group_by(Female) %>% 
                  summarise(Num_People      = n(),
                            Median_Earnings = median(Earnings.Past.12.Months),
                            Pop_percentage  = round(((Num_People/nrow(LMA.Data))*100), 1)) 

# Plot Earnings by Gender
g5 <- lma_gender_data %>%
        ggplot(aes(x = reorder(Female, Num_People), y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Population Percentage") +
        ggtitle("Population Percentage by Gender") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.title.x = element_blank()) + 
        scale_x_discrete(labels = c("1" = "Female", "0" = "Male"))

# Plot Earnings by Gender
g6 <- lma_gender_data %>% 
        ggplot(aes(x = reorder(Female, Num_People), y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Gender?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.title.x = element_blank()) + 
        scale_x_discrete(labels = c("1" = "Female", "0" = "Male"))
   
grid.arrange(g5,g6, nrow=1)

#' 
#' Observations : 
#' 
#' 1. Males have higher median earnings than Female counterparts. 
#' 
#' ### How do Earnings vary by Educational Attainment? Do people with higher degrees earn more?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=15, fig.height=6----

lma_edu_data <- LMA.Data %>%  
                  group_by(Educational.Attainment) %>% 
                  summarise(Num_People      = n(),
                            Median_Earnings = median(Earnings.Past.12.Months),
                            Pop_percentage  = round(((Num_People/nrow(LMA.Data))*100), 1)) 

# Plot Earnings by Educational Attainment
g7 <- lma_edu_data %>%
        ggplot(aes(x = reorder(Educational.Attainment, Num_People), y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Population Percentage") +
        ggtitle("Population Percentage by Educational Attainment") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=12),
              axis.title.x = element_blank())

# Plot Earnings by Educational Attainment
g8 <- lma_edu_data %>% 
        ggplot(aes(x = reorder(Educational.Attainment, Median_Earnings), y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Educational Attainment?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size=12),
              axis.title.x = element_blank())
   
grid.arrange(g7,g8, nrow=1)

#' 
#' Observations : 
#' 
#' 1. Individuals who hold Professional Degree have the highest median earnings.  
#' 
#' ### How do Earnings vary by Marital Status? Do married people earn more?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=15, fig.height=6----

lma_married_data <- LMA.Data %>%  
                      group_by(Married) %>% 
                      summarise(Num_People      = n(),
                                Median_Earnings = median(Earnings.Past.12.Months),
                                Pop_percentage  = round(((Num_People/nrow(LMA.Data))*100), 1)) 

# Plot Earnings by Marital Status
g9 <- lma_married_data %>%
        ggplot(aes(x = reorder(Married, Num_People), y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Population Percentage") +
        ggtitle("Population Percentage by Marital Status") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.title.x = element_blank()) + 
        scale_x_discrete(labels = c("1" = "Married", "0" = "Not Married"))

# Plot Earnings by Marital Status
g10 <- lma_married_data %>% 
        ggplot(aes(x = reorder(Married, Median_Earnings), y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Marital Status?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.title.x = element_blank()) + 
        scale_x_discrete(labels = c("1" = "Married", "0" = "Not Married"))
   
grid.arrange(g9,g10, nrow=1)

#' 
#' 
#' Observations : 
#' 
#' 1. Married people have higher median earnings than their counterparts.  
#' 
#' ### How does earnings vary by marital Status and Gender?
#' 
## ------------------------------------------------------------------------
LMA.Data %>%
  mutate(Status = as.factor(ifelse(Married == 0 & Female == 0, "UnMarried Male",   
                            ifelse(Married == 0 & Female == 1, "UnMarried Female", 
                            ifelse(Married == 1 & Female == 0, "Married Male", 
                            ifelse(Married == 1 & Female == 1, "Married Female", "No")))))) %>%
  group_by(Status,Educational.Attainment) %>%
  summarise(Mean.Earnings = mean(Earnings.Past.12.Months)) %>%
  ggplot(aes(x=Educational.Attainment, y=Mean.Earnings, fill = Status)) + 
    geom_bar(stat = "identity",position = "dodge") +
    #geom_text(aes(label = Count) , size = 4, vjust = -0.2, position = position_dodge(0.9)) +
    theme_classic() +
    ggtitle("On Average Married Man Earns the Most") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1))
  

#' 
#' ### How do Earnings vary by Number of hours worked? Do people who work more tends to earn more?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=15, fig.height=6----

lma_whours_data <- LMA.Data %>% mutate(WHours_Group = cut(Usual.Weekly.Hours, 5)) %>%
                      group_by(WHours_Group) %>% 
                      summarise(Num_People      = n(),
                                Median_Earnings = median(Earnings.Past.12.Months),
                                Pop_percentage  = round(((Num_People/nrow(LMA.Data))*100), 1)) 

# Plot Earnings by Number of hours worked
g11 <- lma_whours_data %>%
        ggplot(aes(x = WHours_Group, y = Pop_percentage)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Population Percentage") +
        ggtitle("Population Percentage by Hours Worked") + theme_classic() +
        geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

# Plot Earnings by Number of hours worked
g12 <- lma_whours_data %>% 
        ggplot(aes(x = WHours_Group, y = Median_Earnings)) + 
        geom_bar(stat = "identity", fill = "lightblue") +
        ylab("Median Earnings") +
        ggtitle("How do Earnings vary by Hours Worked?") + theme_classic() +
        geom_text(aes(label = Median_Earnings), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) 

grid.arrange(g11,g12, nrow=1)

#' 
#' Observations : 
#' 
#' 1. 63.9% of the individuals in the sample data works weekly between 20.6 and 40.2 hours. 
#' 2. People working between 59.8 and 79.4 weekly hours have highest Median Earnings than all other weekly work hour groups.
#' 
#' ### How does Eductaional Attainment vary by Race?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=25, fig.height=5----

lma_edu_race_data <- LMA.Data %>%
                       group_by(Race.Ethnicity, Educational.Attainment) %>% 
                       summarise(Num_Race_People = n(),
                                 Median_Earnings = median(Earnings.Past.12.Months))

lma_edu_race_data <- lma_edu_race_data %>% 
                       inner_join( (lma_race_data %>% select(Num_People,Race.Ethnicity )), by="Race.Ethnicity") %>%
                       mutate(Pop_percentage = round(((Num_Race_People/Num_People)*100), 1)) 

# Plot Population Percentage by Eductaion Attainment and Race
lma_edu_race_data %>%
  ggplot(aes(x = Race.Ethnicity, y = Pop_percentage, fill = Educational.Attainment)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Population Percentage") +
  ggtitle("Population Percentage by Eductaion Attainment and Race") + theme_classic() +
  geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
  theme(axis.title.x = element_blank()) + theme(legend.position = "bottom") + theme(legend.title = element_text(size = 20),
        legend.text  = element_text(size = 16))

#' 
#' Observations : 
#' 
#' 1. 37.7% of Hispanic individuals in the sample data have just High School Degree.
#' 2. Only 1.8% of Black individuals in the sample data have either a Doctorate or Professional Degree.
#' 3. Only 1.5% of White people in the sample data have Doctorate Degree.
#' 4. 33.4% of Asian people in the sample data have Bachelors Degree.
#' 
#' 
#' ### How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Race?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=25, fig.height=5----
# Plot Earnings by Eductaional Attainment and Race
lma_edu_race_data %>% 
   ggplot(aes(x = Educational.Attainment, y = Median_Earnings/1000, fill=Race.Ethnicity)) + 
   geom_bar(stat = "identity", position = "dodge") +
   ylab("Median Earnings (in 1000 USD)") +
        ggtitle("How do Earnings vary by Eductaional Attainment? \nDoes the premium for higher educational attainment vary by Race?") + theme_classic() +
        geom_text(aes(label = Median_Earnings/1000), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
        theme(axis.title.x = element_blank())

#' 
#' Observations : 
#' 
#' 1. Asians with Doctorate, Bachelors Degree, Masters Degree and Professional Degree have the highest Median Earnings among all other Races.
#' 2. Blacks with Professional Degree earns more than Hispanics with Professional Degrees. The difference is large in comparison to the other degrees.
#' 
#' ### How does Eductaional Attainment vary by Gender?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=20, fig.height=5----

lma_edu_gender_data <- LMA.Data %>%
                         group_by(Female, Educational.Attainment) %>% 
                         summarise(Num_Gender_People = n(),
                                   Median_Earnings = median(Earnings.Past.12.Months))

lma_edu_gender_data <- lma_edu_gender_data %>% 
                       inner_join( (lma_edu_data %>% select(Num_People,Educational.Attainment)), by="Educational.Attainment") %>%
                       mutate(Pop_percentage = round(((Num_Gender_People/Num_People)*100), 1)) 

# Plot Population Percentage by Eductaion Attainment and Race
lma_edu_gender_data %>%
  ggplot(aes(x = Educational.Attainment, y = Pop_percentage, fill = as.factor(Female))) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Population Percentage") +
  ggtitle("Population Percentage by Eductaion Attainment and Gender") + theme_classic() +
  geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
  scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Male", "Female")) + 
  theme(legend.title = element_text(size = 15),
        legend.text  = element_text(size = 12))+ 
  theme(axis.title.x = element_blank())

#' 
#' Observations : 
#' 
#' 1. More Males hold - Doctorate, High School Degree and Professional Degree than their Females counterparts.
#' 
#' 
#' 
#' ### How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Gender?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=25, fig.height=5----
# Plot Earnings by Eductaional Attainment and Gender
lma_edu_gender_data %>% 
   ggplot(aes(x = Educational.Attainment, y = Median_Earnings/1000, fill = as.factor(Female))) + 
   geom_bar(stat = "identity", position = "dodge") +
   ylab("Median Earnings (in 1000 USD") +
   ggtitle("How do Earnings vary by Eductaional Attainment? \nDoes the premium for higher educational attainment vary by Gender?") + theme_classic() +
   geom_text(aes(label = Median_Earnings/1000), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
   theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
   theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
   scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Male", "Female")) + 
   theme(legend.title = element_text(size = 15),
         legend.text  = element_text(size = 12)) + 
   theme(axis.title.x = element_blank()) 
    

#' 
#' Observations : 
#' 
#' 1. Males have higher median earnings than their Female couterparts irrespective of their eductaional degree.
#' 
#' ### How does Race vary by Gender?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=25, fig.height=5----

lma_race_gender_data <- LMA.Data %>%
                          group_by(Race.Ethnicity, Female) %>% 
                          summarise(Num_Race_People = n(),
                                Median_Earnings = median(Earnings.Past.12.Months))

lma_race_gender_data <- lma_race_gender_data %>% 
                          inner_join( (lma_race_data %>% select(Num_People,Race.Ethnicity )), by="Race.Ethnicity") %>%
                          mutate(Pop_percentage = round(((Num_Race_People/Num_People)*100), 1)) 

# Plot Population Percentage by Race and Gender
lma_race_gender_data %>%
  ggplot(aes(x = Race.Ethnicity, y = Pop_percentage, fill = as.factor(Female))) + 
  geom_bar(stat = "identity", position = "dodge") +
  ylab("Population Percentage") +
  ggtitle("Population Percentage by Race and Gender") + theme_classic() +
  geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep="")), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
  scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Male", "Female")) + 
  theme(legend.title = element_text(size = 15),
        legend.text  = element_text(size = 12)) + 
  theme(axis.title.x = element_blank())

#' 
#' ### How do Earnings vary by Race? Does the premium for Race vary by Gender?
## ----echo=TRUE, message=FALSE, warning=FALSE, fig.width=25, fig.height=5----
# Plot Earnings by Race and Gender
lma_race_gender_data %>% 
   ggplot(aes(x = Race.Ethnicity, y = Median_Earnings/1000, fill = as.factor(Female))) + 
   geom_bar(stat = "identity", position = "dodge") +
   ylab("Median Earnings (in 1000 USD)") +
        ggtitle("How do Earnings vary by Race? \nDoes the premium for Race vary by Gender?") + theme_classic() +
        geom_text(aes(label = Median_Earnings/1000), vjust=-0.3, colour="black", position = position_dodge(width = .9),  size = 5) +
        theme(plot.title = element_text(size = 15, face = "bold", hjust= 0.5)) + 
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
   scale_fill_discrete(name="Gender", breaks=c("0", "1"), labels=c("Male", "Female")) + 
   theme(legend.title = element_text(size = 15),
         legend.text  = element_text(size = 12)) + 
   theme(axis.title.x = element_blank())

#' 
#' Observations : 
#' 
#' 1. Males have higher median earnings than their Female couterparts irrespective of their Race.
#' 
#' ### Descriptive Statistics(Mean/Median, Sd) of Earning, Usual Weekly Hours & Age by Education Attainment & Race
## ------------------------------------------------------------------------

# #Count of Individuals by Race & Education Attainment
# kable(
# LMA.Data %>%
# addmargins(xtabs(~ Educational.Attainment + Race.Ethnicity, data = LMA.Data)),
# align = "l",
# format = "html",
# caption = "Count of Individuals by Race & Education Attainment")
# 
# 
# #Percentage of Individuals by Race & Education Attainment
# kable(
# LMA.Data %>%
#   group_by(Educational.Attainment, Race.Ethnicity) %>%
#   summarise(Count = n()) %>%
#   arrange(Race.Ethnicity,Educational.Attainment) %>%
#   group_by(Race.Ethnicity)%>%
#   mutate( LabourForce_Percentage = round((Count/sum(Count))* 100,1)   ) %>%
#   select(-Count) %>%
#   spread(Race.Ethnicity,LabourForce_Percentage) ,
# align = "l",
# format = "html",
# caption = "Percentage of Individuals by Race & Education Attainment")
#   

#Descriptive Statistics

tapply(LMA.Data$Earnings.Past.12.Months, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), median)
tapply(LMA.Data$Earnings.Past.12.Months, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), sd)


tapply(LMA.Data$Usual.Weekly.Hours, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), mean)
tapply(LMA.Data$Usual.Weekly.Hours, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), sd)

tapply(LMA.Data$Age, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), mean)
tapply(LMA.Data$Age, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), sd)


#' 
#' ### Preliminary Model - Remove Weekly hours
## ------------------------------------------------------------------------
#Estimate MR Model
Earnings.Equation = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree + Black + Asian + Hispanic,
          data = LMA.Data)
summary(Earnings.Equation)

bptest(Earnings.Equation)
bptestequation = lm(residuals(Earnings.Equation)*residuals(Earnings.Equation) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree + Black + Asian + Hispanic, data = LMA.Data)
summary(bptestequation)

(sandwich_se1 <- diag(vcovHC(Earnings.Equation, type = "HC"))^0.5)


(exp(-0.3802) - 1)*100
(exp(0.1855) - 1)*100

(exp(-0.5839) - 1)*100

(0.1653 - (2*0.00173*25))*100
(0.1653 - (2*0.00173*55))*100

(exp(-0.2593) - 1)*100 #-48.25367 Asian
(exp(-0.2739) - 1)*100 #-43.41917 Black
(exp(-0.3543233) - 1)*100 #-41.61435 White
(exp(-0.3539) - 1)*100 #-48.54693 Hispanic


       

#' 
#' ### Model Estimation
## ------------------------------------------------------------------------
#Estimate MR Model for Race = White
Earnings.Equation.White = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) +Female + Married + Bachelors.Degree +                                          Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree,              data=subset(LMA.Data, Race.Ethnicity == "White") )
summary(Earnings.Equation.White)

#Estimate MR Model for Race = Black
Earnings.Equation.Black = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) +Female + Married + Bachelors.Degree +                                          Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree,              data=subset(LMA.Data, Race.Ethnicity == "Black") )
summary(Earnings.Equation.Black)

#Estimate MR Model for Race = Asian
Earnings.Equation.Asian = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) +Female + Married + Bachelors.Degree +                                          Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree,              data=subset(LMA.Data, Race.Ethnicity == "Asian") )
summary(Earnings.Equation.Asian)

#Estimate MR Model for Race = Hispanic
Earnings.Equation.Hispanic = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree +  Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree,              data=subset(LMA.Data, Race.Ethnicity == "Hispanic") )
summary(Earnings.Equation.Hispanic)


#Full Model

Earnings.EquationF = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) + Female + Married + Race.Ethnicity*Educational.Attainment, data=LMA.Data)
#summary(Earnings.Equation)

round(summary(Earnings.EquationF)$coefficients[,1:2],7)

Earnings.EquationF1 = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) + Female + Married + Race.Ethnicity*Educational.Attainment, data=subset(LMA.Data, Race.Ethnicity %in% c("White","Black") ))

round(summary(Earnings.EquationF1)$coefficients[,1:2],7)

Earnings.EquationF2 = 
  lm(log(Earnings.Past.12.Months) ~ Age + I(Age*Age) + Female + Married + White*No.High.School.Degree + White*High.School.Degree.or.GED +
       White*Some.College + White*Bachelors.Degree + White*Masters.Degree +White*Professional.Degree + White*Doctorate +White*Associates.Degree , data=subset(LMA.Data, Race.Ethnicity %in% c("White","Black") ))

summary(Earnings.EquationF2)
round(summary(Earnings.EquationF2)$coefficients[,1:2],7)

(exp(9.299e-02 + 8.010e-02) - 1)*100 #18.897


#' 
#' ### Perform BPG Test 
## ------------------------------------------------------------------------

bptest(Earnings.Equation.White)
bptestequationwhite = lm(residuals(Earnings.Equation.White)*residuals(Earnings.Equation.White) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree , data=subset(LMA.Data, Race.Ethnicity == "White"))
summary(bptestequationwhite)

bptest(Earnings.Equation.Black)
bptestequationblack = lm(residuals(Earnings.Equation.Black)*residuals(Earnings.Equation.Black) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree + Black , data=subset(LMA.Data, Race.Ethnicity == "Black"))
summary(bptestequationblack)

bptest(Earnings.Equation.Asian)
bptestequationasian = lm(residuals(Earnings.Equation.Asian)*residuals(Earnings.Equation.Asian) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree + Asian , data=subset(LMA.Data, Race.Ethnicity == "Asian"))
summary(bptestequationasian)

bptest(Earnings.Equation.Hispanic)
bptestequationhispanic = lm(residuals(Earnings.Equation.Hispanic)*residuals(Earnings.Equation.Hispanic) ~ Age + I(Age*Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree + Hispanic , data=subset(LMA.Data, Race.Ethnicity == "Hispanic"))
summary(bptestequationhispanic)

#' 
#' ### Correct Standard Errors by generating robust standard errors
## ------------------------------------------------------------------------
#Generate the Robust standard errors and print them on screen for Whites
(sandwich_se1 <- diag(vcovHC(Earnings.Equation.White, type = "HC"))^0.5)

#Generate the Robust standard errors and print them on screen for Black
(sandwich_se2 <- diag(vcovHC(Earnings.Equation.Black, type = "HC"))^0.5)

#Generate the Robust standard errors and print them on screen for Asian
(sandwich_se3 <- diag(vcovHC(Earnings.Equation.Asian, type = "HC"))^0.5)

#Generate the Robust standard errors and print them on screen for Hispanic
(sandwich_se4 <- diag(vcovHC(Earnings.Equation.Hispanic, type = "HC"))^0.5)


#' 
#' ### Interpretation of age
## ------------------------------------------------------------------------
#optimal age for white
0.173 / (2* 0.001811) #47.76367

(0.173 - (2* 0.001811 * 30)) * 100 #6.434%
(0.173 - (2* 0.001811 * 50)) * 100 #-0.81%

#optimal age for black
0.158 / (2*0.001646) #47.99514

(0.158 - (2*0.001646*30)) * 100 #5.924
(0.158 - (2*0.001646*50)) * 100 #-0.66%

#optimal age for asian
0.195411/ (2* 0.002093) # 46.68204

(0.195411 - (2* 0.002093*30))* 100 # 6.9831
(0.195411 - (2* 0.002093*50))* 100 #-1.3889%

#optimal age for hispanic 
0.1295 / (2*0.001345) # 48.14126
(0.1295 - (2*0.001345*30))* 100 #4.88
(0.1295 - (2*0.001345*50))* 100 #-0.5



#' 
## ------------------------------------------------------------------------
#White Female
(exp(-0.4288) - 1)* 100 #-34.87098
#Black Female
(exp(-0.1719) - 1)* 100 #-15.79366
#Asian Female
(exp(-0.2492512) - 1)* 100 #-22.06158
#Hispanic Female
(exp(-0.3067) - 1)* 100 #-26.41287



#White Married
(exp(0.2072) - 1)* 100 #23.02286
#Black Married
(exp(0.1948) - 1)* 100 #21.50679
#Asian Married
(exp(0.0881647) - 1)* 100 #9.216799
#Hispanic Married
(exp(0.1079) - 1)* 100 #11.39363


#' 
## ------------------------------------------------------------------------
exp(0.161)- 1


#' 
