library(tidyverse)
library(ggplot2)
library(dplyr)


## Loads NLCB worksheet and selects the needed columns
nlcb <- read_csv("nlcb_cashpot.csv") %>%
  transmute(draw_year,draw_num,jackpot, num_of_wins, 
            jackpot_per_winner = jackpot/ num_of_wins)



#Filters Winners from 2000 to 2020 and summarizes data
winner_trend<- nlcb %>%
  filter(draw_year >= 2000) %>%
  group_by(draw_year)%>%
  summarize(yearly_wins = sum(num_of_wins), avg_wins = mean(num_of_wins),
            median_wins = median(num_of_wins))

write.csv(winner_trend,"winner_trend.csv")
  

#Plots Winner data to view trend of total wins per year 
#Inspects the total number of winner
ggplot(winner_trend, aes(draw_year,yearly_wins))+
  geom_col(colour = "black", fill = "darkblue")+
  theme_classic()+
  ylab(label = "Yearly Wins")+
  xlab(label = "Draw Year")

#Calculates the IQR of the yearly winners variable 
winner_iqr <- (quantile(winner_trend$yearly_wins, 0.75)  - 
  quantile(winner_trend$yearly_wins, 0.25))

#Calculates the upper and lower limit for the winners
winner_lowerlimit <- (quantile(winner_trend$yearly_wins, 0.25) - 
                          (1.5 * winner_iqr))
winnerupperlimit <- (quantile(winner_trend$yearly_wins, 0.75) + 
                       (1.5 *winner_iqr))
#Highlights the outliers in the Winner Trend data
winner_outlier <- winner_trend %>%
  filter( yearly_wins > winnerupperlimit| yearly_wins < winner_lowerlimit)



#Summarizes Jackpot Figure 
jackpot_sum <- nlcb %>%
  filter(draw_year >=2000) %>%
  group_by(draw_year) %>%
  summarize( yearly_avg_jackpot = mean(jackpot),
             yearly_mid_jackpot = median(jackpot)) 

write.csv(jackpot_sum,"jacpot_summary.csv")

#Summarizes Jackpot per Winner
jackpot_per_winner_sum <- nlcb %>%
  filter(draw_year >= 2000)%>%
  filter(num_of_wins >= 1 )%>%
  group_by(draw_year) %>%
  summarize(avg_jackpot_per_winner= mean(jackpot_per_winner))

write.csv(jackpot_per_winner_sum,"jackpot_per_winner_summary.csv")

#Boxplot used to quickly test for outliers
ggplot(jackpot_per_winner_sum, aes(avg_jackpot_per_winner,color= draw_year,y = 1,))+
  geom_boxplot()
  

#Calculates the IQR of the Jackpot variable 
jackpot_iqr <- (quantile(nlcb$jackpot, 0.75)  - quantile(nlcb$jackpot, 0.25))

#Calculates the upper and lower limit for the jackpots
jackpot_lowerlimit <- (quantile(nlcb$jackpot, 0.25) - 
                        (1.5 * jackpot_iqr))
jackpot_upperlimit <- (quantile(nlcb$jackpot, 0.75) + 
                       (1.5 * jackpot_iqr))

#Highlights the outliers in the Jackpots  data
jackpot_outlier <- nlcb %>%
  filter( jackpot > jackpot_upperlimit| jackpot < jackpot_lowerlimit)
  

#Number of Losses per year. This counts each draw that did not have a winner
yearly_loss <- nlcb %>%
  filter(num_of_wins == 0)%>%
  filter(draw_year >= 2000)%>%
  group_by(draw_year)%>%
  count(draw_year, sort = TRUE)%>%
  rename(num_of_draw_losses = n)

#Counts the draws that had a winner and summarizes this data on a yearly scale. 
winner_draw_count <- nlcb %>%
  filter(num_of_wins >= 1)%>%
  filter(draw_year >= 2000)%>%
  group_by(draw_year)%>%
  count(draw_year)%>%
  rename(num_of_draw_wins = n)


#Calculate how much draws happened per year
yearly_draw_amt <- nlcb %>%
  arrange(draw_year)%>%
  count(draw_year, sort = FALSE)

#Joins Losses to  Wins and calculates a percentage

win_loss_percent<- winner_draw_count%>%
  left_join(yearly_loss, "draw_year")%>%
  replace_na(list(num_of_draw_losses = 0))%>%
  transmute(draw_year,num_of_draw_wins, num_of_draw_losses, 
            #Calculates total draws
            total_draws = num_of_draw_wins + num_of_draw_losses, 
            win_percent = num_of_draw_wins / total_draws * 100)

write.csv(win_loss_percent, "win_loss_percent.csv")



 


   
  





  






  

