#This script was created to analyze Tour de France data
#Created by: Daniel Kuhman
#Last updated: 2020-06-01

library(chron)
library(tidyverse)
library(ggplot2)

#Clear workspace
rm(list = ls())

#Load data:
mydata_path <- file.choose(new = FALSE)
mydata <- read.csv(mydata_path)
rm(mydata_path)

#Correct column name error
mydata <- mydata %>% 
  rename(DISTANCE = DISTNACE)

#Convert TIMES to better format:
mydata$TIMES <- as.character(mydata$TIMES)
mydata$TIMES <- str_replace_all(mydata$TIMES, 'H', ':')
mydata$TIMES <- str_replace_all(mydata$TIMES, "'", ':')
mydata$TIMES <- substr(mydata$TIMES, 1, nchar(mydata$TIMES)-2)
mydata$TIMES <- gsub(" ", "", mydata$TIMES, fixed = TRUE)

#Convert GAP to better format
mydata$GAP <- as.character(mydata$GAP)
mydata$GAP[mydata$GAP=='-'] <- NA
mydata$GAP <- str_replace_all(mydata$GAP, 'H', ':')
mydata$GAP <- str_replace_all(mydata$GAP, "'", ':')
mydata$GAP <- gsub(" ", "", mydata$GAP, fixed = TRUE)
mydata$GAP <- substr(mydata$GAP, 2, nchar(mydata$GAP)-2)

#Removed 2006 and 1997 due to issues with data
mydata <- mydata %>% 
  filter(YEAR != 2006 & YEAR != 1997)

#Get total TIME in seconds
mydata$TIME_hr <- as.numeric(substr(mydata$TIMES, 1,
                                    nchar(mydata$TIMES)-6))
mydata$TIME_min <- as.numeric(substr(mydata$TIMES, nchar(mydata$TIMES)-4,
                          nchar(mydata$TIMES)-3))
mydata$TIME_sec <- as.numeric(substr(mydata$TIMES, nchar(mydata$TIMES)-1,
                          nchar(mydata$TIMES)))
mydata <- mydata %>% 
  mutate(TIME_total = ((TIME_hr*60*60) + (TIME_min*60) + TIME_sec)/3600)

#Get total GAP in seconds
mydata$GAP_hr <- as.numeric(substr(mydata$GAP, 1,
                                    nchar(mydata$GAP)-6))
mydata$GAP_min <- as.numeric(substr(mydata$GAP, nchar(mydata$GAP)-4,
                                     nchar(mydata$GAP)-3))
mydata$GAP_sec <- as.numeric(substr(mydata$GAP, nchar(mydata$GAP)-1,
                                     nchar(mydata$GAP)))
mydata <- mydata %>% 
  mutate(GAP_total = ((GAP_hr*60*60) + (GAP_min*60) + GAP_sec)/3600)

#Shorten data
mydata <- mydata %>% 
  select(YEAR, RANK, RIDER, TEAM, DISTANCE, TIMES, GAP, TIME_total,
         GAP_total)

#Get % difference from winner
mydata$YEAR <- as.factor(mydata$YEAR)
mydata <- mydata %>% 
  group_by(YEAR) %>% 
  mutate(GAP_perc = (GAP_total / TIME_total[RANK == 1]) * 100) %>% 
  ungroup()

#Add km per hour
mydata <- mydata %>% 
  mutate(km_per_hr = DISTANCE / TIME_total)

#START PLOTTING
#Average velocity (km/hr) of winners
ggplot(mydata%>%filter(RANK == 1), aes(x=YEAR, y=km_per_hr, fill=RIDER))+
  geom_point(size=5, pch=21, color='black')+
  geom_segment(aes(x=YEAR, xend=YEAR, y=35, yend=km_per_hr, color=RIDER))+
  geom_text(label=plot_data$RIDER, aes(color=RIDER), nudge_y = 1.4)+
  xlab('Year')+
  ylab('Winner Avg Velocity (km/hr)')+
  ylim(35, 45)+
  coord_flip()+
  theme_classic()+
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 20, face = 'bold',
                                margin = margin(t=25, r=0, b=0, l=0)),
    axis.title.y = element_text(size = 20, face = 'bold',
                                margin = margin(t=0, r=25, b=0, l=0)),
    axis.text = element_text(size = 10),
    axis.line = element_line(size=1)
  )

#Difference between winner and: 2nd, 4th, 10th, last
#Get 2nd and 4th place
plot_data1 <- mydata %>% 
  group_by(YEAR) %>% 
  filter(RANK == 2 | RANK == 4)
#Get last place data
plot_data2 <- mydata %>% 
  group_by(YEAR) %>% 
  slice(tail(row_number(), 1))
#Bind DFs
plot_data <- rbind(plot_data1, plot_data2)
plot_data <- plot_data %>% 
  arrange(YEAR)
rm(plot_data1, plot_data2)
#Create Factor Describing Rank
plot_data$RANK <- as.character(plot_data$RANK)
plot_data$RANK[plot_data$RANK == '2'] <- '2nd'
plot_data$RANK[plot_data$RANK == '4'] <- '4th'
plot_data$RANK[plot_data$RANK != '2nd' & 
                 plot_data$RANK != '4th'] <- 'Last'
plot_data$RANK<-as.factor(plot_data$RANK)

ggplot(plot_data, aes(x=YEAR, y=GAP_perc, group=RANK))+
  geom_line(aes(color=RANK), lwd=1.5)+
  geom_point(aes(fill=RANK), pch=21, color='black', size=5)+
  xlab('Year')+
  ylab('Gap To Winner (% Total Race Time)')+
  theme_classic()+
  theme(
    axis.title.x = element_text(size=20, face='bold',
                                margin=margin(t=25, r=0, b=0, l=0)),
    axis.title.y = element_text(size=20, face='bold',
                                margin=margin(t=0, r=25, b=0, l=0)),
    axis.text.x = element_text(size=11, angle=45, hjust = 1),
    axis.text.y = element_text(size=11),
    axis.line = element_line(size=1),
    legend.title = element_text(size=18, face='bold'),
    legend.text = element_text(size=11)
  )

#Same graph as above, but only including 2nd and 4th
plot_data <- plot_data %>% 
  filter(RANK=='2nd' | RANK=='4th')

ggplot(plot_data,aes(x=YEAR, y=GAP_perc, group=RANK))+
  geom_line(aes(color=RANK), lwd=1.5)+
  geom_point(aes(fill=RANK), pch=21, color='black', size=5)+
  xlab('Year')+
  ylab('Gap To Winner (% Total Race Time)')+
  theme_classic()+
  theme(
    axis.title.x = element_text(size=20, face='bold',
                                margin=margin(t=25, r=0, b=0, l=0)),
    axis.title.y = element_text(size=20, face='bold',
                                margin=margin(t=0, r=25, b=0, l=0)),
    axis.text.x = element_text(size=11, angle=45, hjust = 1),
    axis.text.y = element_text(size=11),
    axis.line = element_line(size=1),
    legend.title = element_text(size=18, face='bold'),
    legend.text = element_text(size=11)
  )