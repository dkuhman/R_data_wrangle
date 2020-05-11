#Creator: Daniel Kuhman
#Date Created: 2020-05-10
#Github: https://github.com/dkuhman

#EACH SECTION RUNS INDEPENDENTLY AND REMOVES ANY RESULTING DFS AT SECTION END

library('tidyverse')

#Load data - interactive
mydata_path <- file.choose(new = FALSE)
mydata <- read.csv(mydata_path)
rm(mydata_path)

#SELECT----
#Select specified columns by name
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM)
rm(mydata)


#FILTER----
#Subset rows using conditional filters
mydata$Tm <- trimws(mydata$Tm, which = c('both')) #Strips white space on both sides of the team name
#Returns all rows where Tm = 'DET'
mydata_det <- mydata %>% 
  filter(Tm == 'DET')
#You can add multiple conditions:
#Return all rows where Tm = 'DET' AND GP > 20 AND Season = 2018
mydata_det <- mydata %>% 
  filter(Tm == 'DET' & GP > 20 & Season == 2018)
#Return all rows where Tm = 'CBJ' OR Tm = 'NSH'
mydata_cbj_nsh <- mydata %>%
  filter(Tm == 'CBJ' | Tm == 'NSH')
rm(mydata, mydata_cbj_nsh, mydata_det)

#ARRANGE----
#Arrange data by number of goals scored (most to least)
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM)
mydata$G <- as.numeric(as.character(mydata$G))
mydata_arranged <- mydata %>% 
  arrange(desc(G))
rm(mydata, mydata_arranged)

#MUTATE----
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM)
#Add a column with total PTS (G + A) for each player
#Convert G and A into numerical values
mydata$G <- as.numeric(as.character(mydata$G))
mydata$A <- as.numeric(as.character(mydata$A))
mydata <- mydata %>% 
  mutate(PTS = G + A)
#Add a column that computes goals scored (G) per game played (GP)
mydata<-mydata %>% 
  mutate(GPG = G / GP)
rm(mydata)

#GROUP_BY + MUTATE----
#Group by team and year and rank players based on total PTS
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PTS, PIM)
#Convert Season to factor and PTS to numeric
mydata$Season <- as.factor(mydata$Season)
mydata$PTS <- as.numeric(as.character(mydata$PTS))
mydata <- mydata %>% 
  group_by(Tm, Season) %>% 
  mutate(pt_rank = rank(-PTS, ties.method = 'first')) %>% 
  ungroup()
#Check whether this worked by creating a new DF and arranging by PTS for a specific team in a specific season
mydata_check <- mydata %>% 
  filter(Tm == ' DET ' & Season == 2018) %>% 
  arrange(desc(PTS))
rm(mydata, mydata_check)
  
  
  
  
  

#RENAME----
#You can easily rename a column using rename(new_name = old_name)
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM)
mydata <- mydata %>% 
  rename(Team = Tm, Goals = G)
rm(mydata)

#STRING REPLACE----
#You can replace strings
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM) %>% 
  filter(Tm == ' DET ')
mydata$Tm <- mydata$Tm %>% 
  str_replace(' DET ', 'Detroit')
rm(mydata)


#CONVERT STRING TO...----
mydata <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM) %>% 
  filter(Tm == ' DET ')
#Covert all characters to lower case
mydata$Tm <- mydata$Tm %>% 
  str_to_lower()
#Convert all characters to upper case
mydata$Tm <- mydata$Tm %>% 
  str_to_upper()
rm(mydata)


#RBIND----
#Create two new data frames (both must have the same column headers)
mydata_det <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM) %>% 
  filter(Tm == ' DET ' & Season == 2018)
mydata_cbj <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G, A, PIM) %>% 
  filter(Tm == ' CBJ ' & Season == 2018)
#Bind the two data frames
mydata_bound <- rbind(mydata_det, mydata_cbj)
rm(mydata, mydata_det, mydata_cbj, mydata_bound)

#FULL JOIN----
#Create two new data frames
mydata_1 <- mydata %>% 
  select(Season, Player, Age, Tm, Pos, GP, G) %>% 
  filter(Tm == ' DET ' & Season == 2018)
mydata_2 <- mydata %>% 
  select(Season, Player, Tm, A, PIM) %>% 
  filter(Tm == ' DET ' & Season == 2018)
#Bind the two data frames
mydata_bound <- full_join(mydata_1, mydata_2)
rm(mydata, mydata_1, mydata_2, mydata_bound)






