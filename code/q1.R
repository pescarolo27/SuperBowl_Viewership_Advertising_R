#load in libraries
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

#import the data
tv <- read_csv("tv.csv", show_col_types=FALSE)
  #59 observations, 9 variables
super_bowls <- read_csv("super_bowls.csv", show_col_types=FALSE)
  #58 observations, 18 variables

###############################################################################################

#Q1 -- Do large point differences result in lost viewers across super bowl games?

q1_data <- tv %>%
  #join the 2 datasets
  full_join(super_bowls, by=c("super_bowl")) %>%
  #adjust "viewers" to millions
  mutate(avg_us_viewers_M = avg_us_viewers/1000000, total_us_viewers_M = total_us_viewers/1000000) %>%
  #select columns of interest
  select(super_bowl, date, network, avg_us_viewers_M, total_us_viewers_M, team_winner, 
         winning_pts, team_loser, losing_pts, combined_pts, difference_pts)
q1_data
  #59 observations, 11 variables

#make a plot -- AVG_US_VIEWERS (millions) vs DIFFERENCE_PTS
q1_plot_I <- ggplot(q1_data, aes(x=difference_pts, y=avg_us_viewers_M)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE) + 
  labs(title="Average US Viewership vs Difference in Final Scores of Super Bowls", 
       x="Difference in Final Scores", 
       y="Avg US Viewership (millions)", subtitle="Before 2025, there have been 58 Super Bowls") +
  theme(plot.title=element_text(size=11), plot.subtitle=element_text(size=9),
        axis.title.x=element_text(size=9), axis.title.y=element_text(size=9))
q1_plot_I

#calculate correlation between the variables
q1_corI <- cor(q1_data$difference_pts, q1_data$avg_us_viewers_M)
q1_corI
  #OUTPUT: -0.2206621 (not considerably meaningful)

###############################################################################################

#make another plot -- TOTAL_US_VIEWERS (millions) vs DIFFERENCE_PTS
q1_plot_II <- ggplot(q1_data, aes(x=difference_pts, y=total_us_viewers_M)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE) +
  labs(title="Total US Viewership vs Difference in Final Scores of Super Bowls", 
       x="Difference in Final Scores", y="Total US Viewership (millions)", 
       subtitle="Before 2025, there have been 58 Super Bowls, 44 of which do not have data for this plot") +
  theme(plot.title=element_text(size=11), plot.subtitle=element_text(size=9),
        axis.title.x=element_text(size=9), axis.title.y=element_text(size=9))
q1_plot_II

#calculate correlation between the variables -- need to filter out 'invalid' values
q1_corII <- q1_data %>%
  filter(!is.na(total_us_viewers_M)) %>%
  summarize(corr = cor(difference_pts, total_us_viewers_M))
q1_corII
  #OUTPUT: -0.376

###############################################################################################

#make another plot -- AVG_US_VIEWERS (millions) vs SUPER_BOWL (over time) w/respect to DIFFERENCE_PTS
q1_plot_III <- ggplot(q1_data, aes(x=super_bowl, y=avg_us_viewers_M, size=difference_pts)) + 
  geom_point(alpha=0.5, color="blue") + 
  labs(title="Average US Viewership & Final Point Differential of Super Bowls over time", 
       x="Super Bowl", y="Avg US Viewership (millions)", size="Final Pt Diff", 
       subtitle="Before 2025, there have been 58 Super Bowls") +
  theme(plot.title=element_text(size=11), plot.subtitle=element_text(size=9),
        axis.title.x=element_text(size=9), axis.title.y=element_text(size=9),
        legend.title=element_text(size=9), legend.text=element_text(size=8))
q1_plot_III


score_impact <- "weak"