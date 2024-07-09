# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(tidyverse)

# 2024 WAG Olympic Team Selections

# For MAG, the rules are clear and mathematical. For WAG, the winner of Olympic Trials is guaranteed a spot on 
# the team. The rest of the team is up to the discretion of the committee. 
# https://static.usagym.org/PDFs/Pressbox/Selection%20Procedures/w_24olympics.pdf

# The rules include several competitions, difficulty scores, execution scores, consistency, composite strength of
# the team, physical presentation, readiness, and capability to complete required training & competition.

# As these rules are not defined such that a clear team can be chosen mathematically, the rules of MAG will be
# applied in order to provide a comparison to the actual team.
# https://static.usagym.org/PDFs/Pressbox/Selection%20Procedures/m_24olympics.pdf

# It is acknowledged that there are key differences between MAG and WAG. For one, the WAG athletes try to "peak" at 
# the Olympics, so difficulty levels at trials are often higher than at nationals. 

# Two sets will be identified.
# One set includes all four scores from 2 days of nationals and 2 days of trials.
# The other set includes the top 3 of these 4 scores.

# Q1: Would applying the MAG rules result in the actual team? Did discretionary criteria play a role?
# Q2: How much variation is there in the top-scoring teams?
# Q3: Considering only the trials, would the team have been different?
# Q4: Were there any gymnasts that likely to be on the team but did not make it? What happened?

# Scores sources:
# https://static.usagym.org/PDFs/Results/2024/w_24champs_sraa.pdf
# https://static.usagym.org/PDFs/Results/2024/w_24trials_aa.pdf

WAG <- read.csv("https://raw.githubusercontent.com/AK-files/Repository/main/WAG_OLY24.csv")

# Shilese Jones dropped out because of an injury
WAG <- WAG[-14, ] 

# # Step 1: Check & clean data

# Check data types
sapply(WAG, class)

# Add a rank column to use as identifier. Use the all-around score for day 1 of the trials:
WAG <- arrange(WAG, desc(aa.t1)) %>%
  mutate(rank = 1:nrow(WAG))

# On observation, Kaliya Lincoln did not compete at nationals. Given the above-mentioned point regarding peaking, 
# her average scores include only the trials. If she is on prospective teams, this will be considered.

# Step 2: Separate into sets and get average score on each apparatus

# Split into 2 sets: total after 4 scores and best 3 of 4 scores

# Set of all 4 scores
WAG.all4 <- WAG

# Average across apparatus
WAG.all4$vault.mean <- rowMeans(subset(WAG.all4, select = c(vault.n1, vault.n2, vault.t1, vault.t2)), na.rm = TRUE)
WAG.all4$floor.mean <- rowMeans(subset(WAG.all4, select = c(floor.n1, floor.n2, floor.t1, floor.t2)), na.rm = TRUE)
WAG.all4$bars.mean <- rowMeans(subset(WAG.all4, select = c(bars.n1, bars.n2, bars.t1, bars.t2)), na.rm = TRUE)
WAG.all4$beam.mean <- rowMeans(subset(WAG.all4, select = c(beam.n1, beam.n2, beam.t1, beam.t2)), na.rm = TRUE)

# Set of best 3 of 4 scores
WAG.3of4 <- WAG

Top3 <- function(scoredf, aparatus_vector) {
  
  temp1 <- scoredf[ , aparatus_vector]
  
  temp2 <- temp1 %>%
    gather(score , value , -Gymnast) %>%
    arrange(desc(value)) %>%
    group_by(Gymnast) %>%
    slice(1:3) %>%
    mutate(score = 1:3) %>%
    spread(score, value)
  
  temp2 <- as.data.frame(temp2)
  
  temp2 <- temp2 %>% 
    rename(
      high_score = "1",
      mid_score = "2",
      low_score = "3")
  
  temp3 <- mutate(temp2, mean = rowMeans(select(temp2, c(high_score, mid_score, low_score)), na.rm = TRUE)) 
  
  scoredf <- merge(scoredf, temp3[ , c("Gymnast" , "mean" )], by = "Gymnast", all.x=TRUE)
  
}

vault.columns <- c("Gymnast", "vault.n1", "vault.n2", "vault.t1", "vault.t2")
WAG.3of4 <- Top3(WAG.3of4, vault.columns)
WAG.3of4 <- WAG.3of4 %>% rename(vault.mean = "mean")

floor.columns <- c("Gymnast", "floor.n1", "floor.n2", "floor.t1", "floor.t2")
WAG.3of4 <- Top3(WAG.3of4, floor.columns)
WAG.3of4 <- WAG.3of4 %>% rename(floor.mean = "mean")

bars.columns <- c("Gymnast", "bars.n1", "bars.n2", "bars.t1", "bars.t2")
WAG.3of4 <- Top3(WAG.3of4, bars.columns)
WAG.3of4 <- WAG.3of4 %>% rename(bars.mean = "mean")

beam.columns <- c("Gymnast", "beam.n1", "beam.n2", "beam.t1", "beam.t2")
WAG.3of4 <- Top3(WAG.3of4, beam.columns)
WAG.3of4 <- WAG.3of4 %>% rename(beam.mean = "mean")

# Step 3: Make possible team combinations

# Make possible team combinations
Combos <- combn(1:13, 5)
# Change to matrix and transpose, then save as df
Combos <- as.matrix(Combos)
Combos <- t(Combos)
Combos <- as.data.frame(Combos)
# Rename columns
Combos <- Combos %>% 
  rename(
    Person.1 = V1,
    Person.2 = V2,
    Person.3 = V3,
    Person.4 = V4,
    Person.5 = V5
  )

# Delete unnecessry columns
WAG.all4 <- WAG.all4 %>%
  select(Gymnast, rank, vault.mean, floor.mean, bars.mean, beam.mean)

WAG.3of4 <- WAG.3of4 %>%
  select(Gymnast, rank, vault.mean, floor.mean, bars.mean, beam.mean)

# Calculate the sum of the top 3 scores of each possible team on each apparatus
WAGPossTeamScores <- function(Combos, WAG) {
  
  Combos[ , 'vault.total'] = NA
  Combos[ , 'floor.total'] = NA
  Combos[ , 'bars.total'] = NA
  Combos[ , 'beam.total'] = NA
  
  n <- nrow(Combos)
  
  for (i in 1:n) {
    
    # Get subsets
    test <- WAG %>%
      subset(rank %in% Combos[i,])
    
    # Vault
    Combos[i,'vault.total'] <- sum(test$vault.mean[order(-test$vault.mean)[1:3]])
    # Floor
    Combos[i,'floor.total'] <- sum(test$floor.mean[order(-test$floor.mean)[1:3]])
    # Bars
    Combos[i,'bars.total'] <- sum(test$bars.mean[order(-test$bars.mean)[1:3]])
    # Beam
    Combos[i,'beam.total'] <- sum(test$beam.mean[order(-test$beam.mean)[1:3]])
    
  }
  
  Combos$total <- (Combos$vault.total + Combos$floor.total + Combos$bars.total + Combos$beam.total)
  
  Combos <- Combos[order(Combos$total, decreasing = TRUE),]
  
  return(Combos)
  
}

WAGall4scores <- WAGPossTeamScores(Combos, WAG.all4)
WAG3of4scores <- WAGPossTeamScores(Combos, WAG.3of4)

# Step 4: Filter results

# Filter rows that include the winner, who is guaranteed to be on the team
HAS.Winner <- function(df, winner) {
  p1 <- as.numeric(winner == df$Person.1)
  p2 <- as.numeric(winner == df$Person.2)
  p3 <- as.numeric(winner == df$Person.3)
  p4 <- as.numeric(winner == df$Person.4)
  p5 <- as.numeric(winner == df$Person.5)
  
  df <- df %>%
    mutate(HasWinner = (p1 + p2 + p3 + p4 + p5)) 
  
  df <- subset(df, df$HasWinner == 1)
}

winner <- '1'
WAGall4scores <- HAS.Winner(WAGall4scores, winner)
WAG3of4scores <- HAS.Winner(WAG3of4scores, winner)

# As difficulty scores in trials were increased over those in nationals, check only scores in trials

WAGtrials <- WAG

WAGtrials$vault.mean <- rowMeans(subset(WAGtrials, select = c(vault.t1, vault.t2)), na.rm = TRUE)
WAGtrials$floor.mean <- rowMeans(subset(WAGtrials, select = c(floor.t1, floor.t2)), na.rm = TRUE)
WAGtrials$bars.mean <- rowMeans(subset(WAGtrials, select = c(bars.t1, bars.t2)), na.rm = TRUE)
WAGtrials$beam.mean <- rowMeans(subset(WAGtrials, select = c(beam.t1, beam.t2)), na.rm = TRUE)

WAGtrials <- WAGtrials %>%
  select(Gymnast, rank, vault.mean, floor.mean, bars.mean, beam.mean)

WAGtrials <- WAGPossTeamScores(Combos, WAGtrials)
WAGtrials <- HAS.Winner(WAGtrials, winner)

# Step 5: Analyze & discuss results

# The actual team chosen was 1, 2, 3, 4, 7 (Simone Biles, Jordan Chiles, Suni Lee, Jade Carey, and Hezly Rivera).

# Q1: Would applying the MAG rules result in the actual team? Did discretionary criteria play a role?

# This team placed first in both sets, so according to the rules for MAG, this team would have been automoatically
# selected. When considering all scores, it placed .1813 points above the second possible team. When considering
# 3 of 4 scores, it placed .1667 points above. If the teams had been different, discretionary criteria would
# have been used.

# Q2: How much variation is there in the top-scoring teams?

# The combination 1, 2, 3, 4 appeared in 6 of the 10 considered combinations. As predicted by commentators, 
# the last spot on the team was less clear. There is one combination in which gymnast 2 is not a member, but 
# gymnast 3 is on all 10 combinations. Gymnast 4 is on 7 combinations.

# In considering 3 of 4 scores, gymnast 5 appeared 3 times. Her scores appear relatively consistent; it does not
# appear that there were large errors contributing to low scores. She is the gymnast closest to being on the team 
# while not named to it.

# Q3: Considering only the trials, would the team have been different?

# There are some differences if only trial scores are considered. The top team is 1, 3, 4, 5, 7 with 172.475 
# points. The next two teams are 1, 2, 3, 4, 5 and the actual team tied with 172.3875. The difference between 
# the top 2 teams is 0.0875, which is under the threshold of 2 points, meaning that discretionary criteria
# could be used. Upon inspection of the data, gymnast 2 had a low beam score on day 2, which had a greater 
# effect on the average of 2 scores in this analysis, as compared to dropping the score or averaging it in 
# 4 scores. 

# Q4: Were there any gymnasts that likely to be on the team but did not make it? What happened?

# Other hopefuls for the Olympics (Joscelyn Roberson, Leanne Wong, and Tiana Sumanasekera) do not appear to have 
# scores clearly differing from others - unlike the men, it does not appear that a gymnast made 2 mistakes that 
# changed the calculation. Joscelyn Roberson is gymnast 5 discussed above. Leanne Wong was on 1 of the 10 lists,
# but it was the fifth team of that set. Tiana Sumanasekera had consistent scores and appeared on 2 of the 10
# lists. These gymnasts were close, but including them in a team resulted in mathematically lower scores.
