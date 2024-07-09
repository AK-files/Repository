# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(tidyverse)

# 2024 MAG Olympic Team Selections

# https://static.usagym.org/PDFs/Pressbox/Selection%20Procedures/m_24olympics.pdf
# The selection procedures state that 2 sets of possible teams are identified. 
# The committee considers 4 scores from the national championships day 1 (n1) and day 2 (n2) 
# as well as from the Olympic trials day 1 (t1) and day 2 (t2).
# Set 1 contains the top 3 scores of these 4.
# Set 2 contains all 4 scores.

# The winner of the trials, if he places in the top 3 on 3 apparatus at the trials, is guaranteed a spot.

# If any team combination has only 3 gymnasts who can compete on any aparatus, that team score must be .5
# higher than the next team combination in that set.

# If the top-scoring team on Set 1 and Set 2 is the same, that team is selected.
# If they differ and one team score is more than 2 points more than the other, the higher-scoring team 
# is selected.
# If the difference is less than 2 points, discretionary criteria are used. In this scenario, the top 5 teams
# of each set are considered.

# Q1: How was the team chosen given the results? Did discretionary criteria play a role?
# Q2: How much variation is there in the top-scoring teams?
# Q3: After the first day of the trials, how did the teams compare? Did the final day change the standing?
# Q4: Were there any gymnasts that likely to be on the team but did not make it? What happened?

# Score sources:
# https://static.usagym.org/PDFs/Results/2024/m_24champs_sraa.pdf
# https://static.usagym.org/PDFs/Results/2024/m_24trials_aa.pdf

MAG <- read.csv("https://raw.githubusercontent.com/AK-files/Repository/main/MAG_OLY24.csv")

# Step 1: Check & clean data

# Check data types
sapply(MAG, class)

# Add a rank column (based on t.1) to use as identifier
 MAG <- arrange(MAG, desc(aa.t1)) %>%
   mutate(rank = 1:nrow(MAG))
 
# Step 2: Separate into sets and get average score on each apparatus

# Split into 3 sets: total after 3 scores, total after 4 scores, and best 3 of 4 scores

# Set after 3 days of competition
MAG.first3 <- subset(MAG, select = -c(floor.t2, pommel.t2, rings.t2, vault.t2, pbars.t2 , highbar.t2))

# Average scores (ignore missing scores)
# There may be an official manner of handling this, but for the purposes of this analysis, this is sufficient.  
# Instead, make a list of gymnasts with missing values. Should they be members of a potential team, 
# the extent and impact of missing scores can be considered. 

# Find rows that have NA
NA_MAG.first3 <- MAG.first3[rowSums(is.na(MAG.first3)) > 0,]

# Average across aparatus
MAG.first3$floor.mean <- rowMeans(subset(MAG.first3, select = c(floor.n1, floor.n2, floor.t1)), na.rm = TRUE)
MAG.first3$pommel.mean <- rowMeans(subset(MAG.first3, select = c(pommel.n1, pommel.n2, pommel.t1)), na.rm = TRUE)
MAG.first3$rings.mean <- rowMeans(subset(MAG.first3, select = c(rings.n1, rings.n2, rings.t1)), na.rm = TRUE)
MAG.first3$vault.mean <- rowMeans(subset(MAG.first3, select = c(vault.n1, vault.n2, vault.t1)), na.rm = TRUE)
MAG.first3$pbars.mean <- rowMeans(subset(MAG.first3, select = c(pbars.n1, pbars.n2, pbars.t1)), na.rm = TRUE)
MAG.first3$highbar.mean <- rowMeans(subset(MAG.first3, select = c(highbar.n1, highbar.n2, highbar.t1)), na.rm = TRUE)
  
# Replace NaN values with 0
MAG.first3$floor.mean[is.nan(MAG.first3$floor.mean)]<-0
MAG.first3$pommel.mean[is.nan(MAG.first3$pommel.mean)]<-0
MAG.first3$rings.mean[is.nan(MAG.first3$rings.mean)]<-0
MAG.first3$vault.mean[is.nan(MAG.first3$vault.mean)]<-0
MAG.first3$pbars.mean[is.nan(MAG.first3$pbars.mean)]<-0
MAG.first3$highbar.mean[is.nan(MAG.first3$highbar.mean)]<-0

# Set of all 4 scores
MAG.all4 <- MAG

# Average across events
MAG.all4$floor.mean <- rowMeans(subset(MAG.all4, select = c(floor.n1, floor.n2, floor.t1, floor.t2)), na.rm = TRUE)
MAG.all4$pommel.mean <- rowMeans(subset(MAG.all4, select = c(pommel.n1, pommel.n2, pommel.t1, pommel.t2)), na.rm = TRUE)
MAG.all4$rings.mean <- rowMeans(subset(MAG.all4, select = c(rings.n1, rings.n2, rings.t1, rings.t2)), na.rm = TRUE)
MAG.all4$vault.mean <- rowMeans(subset(MAG.all4, select = c(vault.n1, vault.n2, vault.t1, vault.t2)), na.rm = TRUE)
MAG.all4$pbars.mean <- rowMeans(subset(MAG.all4, select = c(pbars.n1, pbars.n2, pbars.t1, pbars.t2)), na.rm = TRUE)
MAG.all4$highbar.mean <- rowMeans(subset(MAG.all4, select = c(highbar.n1, highbar.n2, highbar.t1, highbar.t2)), na.rm = TRUE)

# List gymnasts with missing scores
NA_MAG.all4 <- MAG.all4[rowSums(is.na(MAG.all4)) > 0,]

# Replace NaN values with 0
MAG.all4$floor.mean[is.nan(MAG.all4$floor.mean)]<-0
MAG.all4$pommel.mean[is.nan(MAG.all4$pommel.mean)]<-0
MAG.all4$rings.mean[is.nan(MAG.all4$rings.mean)]<-0
MAG.all4$vault.mean[is.nan(MAG.all4$vault.mean)]<-0
MAG.all4$pbars.mean[is.nan(MAG.all4$pbars.mean)]<-0
MAG.all4$highbar.mean[is.nan(MAG.all4$highbar.mean)]<-0

# Set of best 3 of 4 scores
MAG.3of4 <- MAG

# Get the top 3 scores for each apparatus
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

floor.columns <- c("Gymnast", "floor.n1", "floor.n2", "floor.t1", "floor.t2")
MAG.3of4 <- Top3(MAG.3of4, floor.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(floor.mean = "mean")

pommel.columns <- c("Gymnast", "pommel.n1", "pommel.n2", "pommel.t1", "pommel.t2")
MAG.3of4 <- Top3(MAG.3of4, pommel.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(pommel.mean = "mean")

rings.columns <- c("Gymnast", "rings.n1", "rings.n2", "rings.t1", "rings.t2")
MAG.3of4 <- Top3(MAG.3of4, rings.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(rings.mean = "mean")

vault.columns <- c("Gymnast", "vault.n1", "vault.n2", "vault.t1", "vault.t2")
MAG.3of4 <- Top3(MAG.3of4, vault.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(vault.mean = "mean")

pbars.columns <- c("Gymnast", "pbars.n1", "pbars.n2", "pbars.t1", "pbars.t2")
MAG.3of4 <- Top3(MAG.3of4, pbars.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(pbars.mean = "mean")

highbar.columns <- c("Gymnast", "highbar.n1", "highbar.n2", "highbar.t1", "highbar.t2")
MAG.3of4 <- Top3(MAG.3of4, highbar.columns)
MAG.3of4$mean[is.nan(MAG.3of4$mean)]<-0
MAG.3of4 <- MAG.3of4 %>% rename(highbar.mean = "mean")

# Step 3: Make possible team combinations

# List all possible team combinations using 'rank' as unique identifier
Combos <- combn(1:20, 5)
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

# Delete unnecessary columns
MAG.first3 <- MAG.first3 %>%
  select(Gymnast, rank, floor.mean, pommel.mean, rings.mean, vault.mean, pbars.mean, highbar.mean)

MAG.all4 <- MAG.all4 %>%
  select(Gymnast, rank, floor.mean, pommel.mean, rings.mean, vault.mean, pbars.mean, highbar.mean)

MAG.3of4 <- MAG.3of4 %>%
  select(Gymnast, rank, floor.mean, pommel.mean, rings.mean, vault.mean, pbars.mean, highbar.mean)

# Calculate the sum of the top 3 scores of each possible team on each apparatus
PossTeamScores <- function(Combos, MAG) {
  
  Combos[ , 'floor.total'] = NA
  Combos[ , 'pommel.total'] = NA
  Combos[ , 'rings.total'] = NA
  Combos[ , 'vault.total'] = NA
  Combos[ , 'pbars.total'] = NA
  Combos[ , 'highbar.total'] = NA
 
  n <- nrow(Combos)
  
  for (i in 1:n) {

# Get subsets
   test <- MAG %>%
     subset(rank %in% Combos[i,])

# Floor
   Combos[i,'floor.total'] <- sum(test$floor.mean[order(-test$floor.mean)[1:3]])
# Pommel
   Combos[i,'pommel.total'] <- sum(test$pommel.mean[order(-test$pommel.mean)[1:3]])
# Rings
   Combos[i,'rings.total'] <- sum(test$rings.mean[order(-test$rings.mean)[1:3]])
# Vault
   Combos[i,'vault.total'] <- sum(test$vault.mean[order(-test$vault.mean)[1:3]])
# Pbars
   Combos[i,'pbars.total'] <- sum(test$pbars.mean[order(-test$pbars.mean)[1:3]])
# Highbar
   Combos[i,'highbar.total'] <- sum(test$highbar.mean[order(-test$highbar.mean)[1:3]])

   }
  
Combos$total <- (Combos$floor.total + Combos$pommel.total + Combos$rings.total +
                     Combos$vault.total + Combos$pbars.total + Combos$highbar.total)

Combos <- Combos[order(Combos$total, decreasing = TRUE),]
  
return(Combos)

}
 
MAG3scores <- PossTeamScores(Combos, MAG.first3)
MAGall4scores <- PossTeamScores(Combos, MAG.all4)
MAG3of4scores <- PossTeamScores(Combos, MAG.3of4)

# Step 4: Filter results

# According to rule 1.3.2.a, if any MAG gymnast ranks first at the trials and in the top 3 of any 3 
# apparatus, that person is automatically named to the team. Frederick Richards achieved this on
# floor (3rd), pbars (2nd), and highbar (1st).

# Filter rows that include the winner

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
MAGall4scores <- HAS.Winner(MAGall4scores, winner)

# Filter out teams that have multiple specialists

# The rule states that if there is any apparatus in which only 3 athletes can compete, the team must be .5 higher 
# than the next highest team. As no teams have .5 higher than any other team, pairs of specialists who do not 
# compete any apparatus can be identified and removed 
# These are:
# Floor: 18 & 19, 18 & 20, 19 & 20
# Pommel: 16 & 18
# Rings: 16 & 19, 16 & 20, 19 & 20
# Vault: 18 & 19, 18 & 20, 19 & 20
# Pbars: 18 & 19, 18 & 20, 19 & 20
# Highbar: 18 & 19, 18 & 20, 19 & 20
# So these combos are not allowed: 16 & 18, 16 & 19, 16 & 20, 18 & 19, 18 & 20, 19 & 20

# Check to see what the differences are between possible teams

Diffbetweenteams <- abs(diff(MAGall4scores$total))

# No teams have more than .5 difference between them
# NOTE: This can be rechecked after removal of each pair to verify that it holds.

HAS.Specialists <- function(df, specialist1, specialist2) {
  s1.1 <- as.numeric(specialist1 == df$Person.1)
  s1.2 <- as.numeric(specialist1 == df$Person.2)
  s1.3 <- as.numeric(specialist1 == df$Person.3)
  s1.4 <- as.numeric(specialist1 == df$Person.4)
  s1.5 <- as.numeric(specialist1 == df$Person.5)
  
  HasSpecialist1 = (s1.1 + s1.2 + s1.3 + s1.4 + s1.5)
  
  s2.1 <- as.numeric(specialist2 == df$Person.1)
  s2.2 <- as.numeric(specialist2 == df$Person.2)
  s2.3 <- as.numeric(specialist2 == df$Person.3)
  s2.4 <- as.numeric(specialist2 == df$Person.4)
  s2.5 <- as.numeric(specialist2 == df$Person.5)
  
  HasSpecialist2 = (s2.1 + s2.2 + s2.3 + s2.4 + s2.5)
  
  df <- df %>%
    mutate(Has2Specialists = (HasSpecialist1 + HasSpecialist2)) 
  
  df <- subset(df, df$Has2Specialists < 2)
}

MAGall4scores <- HAS.Specialists(MAGall4scores, 16, 18)
MAGall4scores <- HAS.Specialists(MAGall4scores, 16, 19)
MAGall4scores <- HAS.Specialists(MAGall4scores, 16, 20)
MAGall4scores <- HAS.Specialists(MAGall4scores, 18, 19)
MAGall4scores <- HAS.Specialists(MAGall4scores, 18, 20)
MAGall4scores <- HAS.Specialists(MAGall4scores, 19, 20)

# This leaves 3185 possible teams that meet the criteria.
# The top team is 1, 2, 4, 5, 20 with a total of 258.1000

MAG3of4scores <- HAS.Winner(MAG3of4scores, winner)
Diffbetweenteams <- abs(diff(MAG3of4scores$total))
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 16, 18)
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 16, 19)
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 16, 20)
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 18, 19)
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 18, 20)
MAG3of4scores <- HAS.Specialists(MAG3of4scores, 19, 20)

MAG3scores <- HAS.Winner(MAG3scores, winner)
Diffbetweenteams <- abs(diff(MAG3scores$total))
MAG3scores <- HAS.Specialists(MAG3scores, 16, 18)
MAG3scores <- HAS.Specialists(MAG3scores, 16, 19)
MAG3scores <- HAS.Specialists(MAG3scores, 16, 20)
MAG3scores <- HAS.Specialists(MAG3scores, 18, 19)
MAG3scores <- HAS.Specialists(MAG3scores, 18, 20)
MAG3scores <- HAS.Specialists(MAG3scores, 19, 20)

# Step 5: Analyze & discuss results

# The actual team chosen was 1, 2, 4, 5, 20 (Fred Richard, Brody Malone, Paul Juda, Asher Hong, Stephen Nedoroscik)

# Q1: How was the team chosen given the results? Did discretionary criteria play a role?

# The actual team ranked first when considering both sets, all 4 scores and 3 of 4 scores. According to the rules,
# this determines the team. Discretionary criteria did not affect the team.

# Q2: How much variation is there in the top-scoring teams?

# According to the rules, the committee considers the top 5 combinations of each set. Person 2 is in all of these.
# Of the 10 teams considered, Person 3 appears twice. Despite finishing third in the Olympic Trials, this person is
# not likely on the team. Person 4 appears 7 times, Person 5 appears 6 times, Person 6 appears 2 times, and 
# Person 20 appears 6 times. 

# An interesting observation of both sets is that a specialist occurs on many of the top team combinations. 
# Although the rules disfavor having 2 specialists, it's clear that there is a mathematic advantage to including
# the score from one.

# The team scores are strikingly close. There is less than .1 difference between each team with the difference
# between the top team and the 5th team being about .2. The scoring difference between the top score on each list 
# is 2.5167, which highlights the impact of consistency. If the top team from each set had not been the same, 
# this would have been sufficient to name the higher scoring team (3 of 4 scores, as it allows for dropping a 
# weaker score). For comparison, in the 2020 Olympics, the team scores were 262.500, 262.397, 261.894, 
# 255.760, 254.594 (USA). This shows that small differences can be the difference between places, so small 
# differences of possible teams is an important consideration.

# Q3: After the first day of the trials, how did the teams compare? Did the final day change the standing?

# The top two team combinations had the same score. These were 1, 2, 3, 6, 20 and 1, 2, 4, 6, 20. In fact, 
# in addition to 1 and 2, gymnast 6 appeared on all top 5 team combinations. In looking at his scores, they 
# were lower on average on day 2 of the trials. His total was 81.6 for the final day, in comparison to 
# 82.45, 85.75, and 83.70 on previous days.

# Gymnasts 3 and 4 were interchangeable in this set, so the final day of trials differentiated them. Interestingly,
# gymnast 3 scored an average total score of 84.4625, while gymnast 4 scored 84. Despite being a stronger gymnast
# all around, the combination of scores resulted in gymnast 4 making the Olympic team while gymnast 3 not.

# Like the results of the sets used in the actual calculation, gymnasts 19 and 20 were close with 20 having
# slightly better scores. In fact, gymnast 20 made the team.

# Q4: Were there any gymnasts that likely to be on the team but did not make it? What happened?

# Gymnast 12 appeared twice. This gymnast has performed well and was considered to be a strong candidate for the 
# team. Examining his scores, he received two scores on pommel that were a few points lower than the other two. 
# In a "what if" scenario of having only 1 low score, the code can be rerun with the second lowest score being 
# replaced with the second highest score then rerunning the above code:

MAG[MAG$Gymnast=="Khoi Young", "pommel.n1"] <- 14.25

# If gymnast 12 had not had 2 low scores, he would have been on 7 of 10 considered combinations, including the 
# top team when considering 3 of 4 scores. Of course, this eliminates the second low score, while considering 
# all 4 scores still contains one low score. The top team in each set is different, so the process would have 
# continued to look at the totals. Considering 3 out of 4 scores, the team 1, 2, 9, 12, 20 would have scored 
# 261.0333. Considering all 4 scores, the team 1, 2, 4, 5, 20 (the actual team) would have scored 258.1. The 
# difference is 2.9333, meaning that the team considering 3 of 4 scores would have been named.

# There is an interesting side note about how scores combine. In this scenario, gymnast 9 would have been named to
# the team. Prior to changing this score, gymnast 9 appeared on one possible team and not on the list after the 
# first 3 days.


