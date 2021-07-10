#SELECT       5      SELECT 
#FROM         1      GIVE DATAFRAME
#WHERE        2      FILTER
#GROUP BY     3      GROUP_BY
#HAVING BY    4      FILTER
#ORDER BY     6      ARRANGE
#LIMIT        7      HEAD


#Remove everything from my environment
rm(list = ls())

#Installing the package 'rio'
#install.packages('rio')

#Load the library / package 'rio'
library(rio)

#Set up the working directory
#setwd("C:/Users/elias/Dropbox/Desktop/TSOM - Module 2/Jul2021")

#Read the file 'names'
link = 'names.csv'
df <- import(link)

#How many observations do we have?
#6.2M

#How many variables we have?
#5

#What is the data type of the variable 'Count'
#It's a integer

#We want make the variable count a numeric type
df$Count <- as.numeric(df$Count)

#Dimensions
dim(df)

#Let's read the first two observations
head(df,2)

#What is the most recent year in the dataset
max(df$Year)

#What is the least recent year in the dataset
min(df$Year)

#How many years does these dataset cover?
max(df$Year) - min(df$Year) + 1

#I want to know the average count of names
mean(df$Count)

#I want to know the median count of names
median(df$Count)

#What kind of data types we have in the dataset
str(df)

#I want to get an statistical summary of my data
summary(df)

#I want to know the standard deviation of the count of the names
sd(df$Count)

#Installing the 'tidyverse' library
#install.packages("tidyverse")

#Load the library 'tidyverse'
library(tidyverse)

#I want to know how many categories 
#we have in the Gender variable

#Using base R to find out the categories in Gender
unique(df$Gender)

#Find the categories and their freq
round(table(df$Gender)/1e6,2)

#Do I have more names that are male or
#names that are female in my dataset?

#Temporary steps
df1 <- head(df,2)

unique(df1$Gender)

table(df1$Gender)

#How many female names do I have in df1
sum(df1$Count)


#A. Using base R
#1. Slicing only the female names
cond = df$Gender == 'F'   #choose one group
sum(df$Count[cond])/1e6   #Summing operation in the group
#There are 152.17M names that are female

#2. Slicing only the female names
cond = df$Gender == 'M'
sum(df$Count[cond])/1e6
#There are 164.71M names that are male

#B. Using dplyr in tydyverse

#SELECT       5      SELECT 
#FROM         1      GIVE DATAFRAME
#WHERE        2      FILTER
#GROUP BY     3      GROUP_BY
#HAVING BY    4      FILTER
#ORDER BY     6      ARRANGE
#LIMIT        7      HEAD

#Examplw
df %>% #1
  filter(Gender == 'M') %>% #2
  group_by(State) %>% #3
  summarise(Total = sum(Count)/1e6 ) %>% #5
  arrange(desc(Total)) %>% #6
  head(1) %>% #7
  mutate(Total = sprintf("%0.2f", Total)) 

#Count total names per Gender
df %>% 
  group_by(Gender) %>%
  summarise(Total = sum(Count)/1e6) %>%
  mutate(Total = sprintf("%0.2f", Total))

#Count total observations per Gender
df %>% 
  group_by(Gender) %>%
  summarise(Total = n() /1e6)%>%
  mutate(Total = sprintf("%0.2f", Total))

#Base R
table(df$State)

#Count observations per State
df %>% 
  group_by(State) %>%
  summarise(Total = n()/1e3 )%>%
  mutate(Total = sprintf("%0.2f", Total))

#Which state has the most observations
df %>% 
  group_by(State) %>%
  summarise(Total = n()/1e3 ) %>%
  arrange(desc(Total)) %>%
  head(1) %>%
  mutate(Total = sprintf("%0.2f", Total)) 

#Which state has the most male names?
df %>%
  filter(Gender == 'M') %>%
  group_by(State) %>%
  summarise(Total = sum(Count)/1e6 ) %>%
  arrange(desc(Total)) %>%
  head(1) %>%
  mutate(Total = sprintf("%0.2f", Total)) 

#Which state has the most female names?
df %>%
  filter(Gender == 'F') %>%
  group_by(State) %>%
  summarise(Total = sum(Count)/1e6 ) %>%
  arrange(desc(Total)) %>%
  head(1) %>%
  mutate(Total = sprintf("%0.2f", Total)) 

#I want to know the average number of babies
#that were registered in the state of New York in 2020

#SELECT       5      SELECT / SUMMARISE
#FROM         1      GIVE DATAFRAME
#WHERE        2      FILTER
#GROUP BY     3      GROUP_BY
#HAVING BY    4      FILTER
#ORDER BY     6      ARRANGE
#LIMIT        7      HEAD

df %>%
  filter(State == 'NY' & Year == 2020) %>%
  summarise('Average' = round(mean(Count),2))


#1. I want you to find the top 10 states
#that had the most popular names (sum Count)
#in 2018. Show the answers in '000s and rounded to 1
#decimal point

#2. I want to know the top state in which your name is 
#the most popular

#3. The average number of babies registered in 
#'NY' and 'NJ' in 2020'. Which state has the highest mean.
#Round your results to the nearest integer

#4. What is the most popular name of all times?
#What is the most popular male name and the most popular 
#female name

#Retrieving first two values in the dataset
#Using the pipe
df %>% head(2)

#Standard way
head(df, 2)

#Mean records in 2020
df %>%
  filter(Year == 2020) %>%
  group_by(State) %>%
  summarise('Average' = round(mean(Count),2)) -> mean_states

#Rules for searching characters in  a string

# ^      to indicate the start of the search
# $      to indicate the end of the search
# .      to match a single alphanumeric character
# .*     to match multiple alphanumeric characters
# [0-9]  to match a single number
# [a-z]  to match a lowercase letter
# [A-Z]  to match an uppercase letter
# *      to match multiple characters for [0-9],[a-z],[A-Z]


#I want to find any names that start with capital M,
#end with o. 
#So the pattern wil be '^M.*o$' or '^M[a-z]*o$'

#Only Mario
df %>% filter(Name == 'Mario')

#Use the function grepl
grepl(pattern = '^M.*o$', x = Name)

#I want to find any names that start with capital M,
#end with o. 
#So the pattern wil be '^M.*o$' or '^M[a-z]*o$'

#Adding the grepl to filter
df %>% 
  filter(grepl(pattern = '^M.*o$', x = Name)) %>%
  distinct(Name)

df %>% 
  filter(grepl(pattern = '^M[a-z]*o$', x = Name)) %>%
  distinct(Name)

#Now you want any names that start with capital M,
#end with o and have TWO alphanumeric character in between
df %>% 
  filter(grepl(pattern = '^M..o$', x = Name)) %>%
  distinct(Name)

# i want to che witch is the most common name 
df%>%
  group_by(Name)%>%
  summarise(Popularity=sum(Count))%>%
  arrange(desc(Popularity))%>%
  head(1)

# iwant to know how the popularity of the name 
#james has evolve over time 

df%>%
  group_by(Name)%>%
  summarise(Popularity=sum(Count))%>%
  arrange(desc(Popularity))%>%
  head(1)


#SELECT       5      SELECT / SUMMARISE
#FROM         1      GIVE DATAFRAME
#WHERE        2      FILTER
#GROUP BY     3      GROUP_BY
#HAVING BY    4      FILTER
#ORDER BY     6      ARRANGE
#LIMIT        7      HEAD

# creating a table for see the changes over the time 
df%>%
  filter(Name=="James")%>%
  group_by(Year)%>%
  summarise(Popularity=sum(Count))%>%
  with(plot(x=Year, 
            y =Popularity/1e3,
            type="l",
            col="red",
            main="Popularity of name james over the years",
            ylab ='In Thousands',
            xlab = " "
            ))

# i want to figure out what is the year of the peak value and value 



