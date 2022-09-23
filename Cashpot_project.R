library(tidyverse)
library(ggplot2)
library(dplyr)

## Loads NLCB worksheet and selects the needed columns
nlcb <- read_csv("nlcb_cashpot.csv") %>%
  select(draw_year,draw_num,jackpot, num_of_wins)
  

options(scipen=999)



#Filters Winners trend from 2000 to 2020
winner_trend<- nlcb %>%
  filter(draw_year >= 2000) %>%
  count(draw_year) %>%
  rename(yearly_wins = n) %>%
  

#Calculates the statistics for the winner trend. 
winner_trend_stats <-  winner_trend %>%
  summarise(total_winners = sum(yearly_wins),
            avg_winners = mean(yearly_wins), 
            median_winner = median(yearly_wins))

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


#Average Jackpot Per Year 
avg_jackpot <- nlcb %>%
  filter(draw_year >=2000) %>%
  group_by(draw_year) %>%
  summarize( yearly_avg_jackpot = mean(jackpot)) 

#Calculates the IQR of the Average Jackpot variable 
avg_jackpot_iqr <- (quantile(nlcb$jackpot, 0.75)  - 
                 quantile(nlcb$jackpot, 0.25))
#Calculates the upper and lower limit for the jackpots

jackpot_lowerlimit <- (quantile(nlcb$jackpot, 0.25) - 
                        (1.5 * avg_jackpot_iqr))
jackpot_upperlimit <- (quantile(nlcb$jackpot, 0.75) + 
                       (1.5 *avg_jackpot_iqr))
#Highlights the outliers in the Jackpots  data
jackpot_outlier <- nlcb %>%
  filter( jackpot > jackpot_upperlimit| jackpot < jackpot_lowerlimit)

  

colnames(avg_jackpot)
#Scatterplot for Average Jack
ggplot(avg_jackpot, aes(x = draw_year, y = yearly_avg_jackpot, 
                        colour = draw_year))+ 
  geom_point()+
  geom_line()+
  scale_y_continuous( name = "Jackpot Yearly Average",
                      limits = c (0,300000))+
  labs(title = "Average Jackpot per Year from 2000 to 2020")
  


#Number of Losses per year
yearly_loss <- nlcb %>%
  filter(jackpot == 0)%>%
  count(draw_year, sort = FALSE)%>%
  arrange(draw_year) 
  

##Date Loss 
loss_date<- nlcb %>%
  filter(jackpot == 0)

#Calculate how much draws happened per year
yearly_draw_amt <- nlcb %>%
  arrange(draw_year)%>%
  count(draw_year, sort = FALSE)

ggplot(yearly_loss, aes(x = draw_year, y = n))+
  geom_col()
 


   
  





  






  

