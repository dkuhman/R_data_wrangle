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
  
  
  
  
  

#RBIND----


#CBIND----