#Q2 Part A -- How has the number of viewers and TV ratings trended alongside advertisement costs?

###################################################################################################

#Ad cost vs Avg US Viewership
q2_data_A <- tv %>%
  #merge the 2 datasets
  full_join(super_bowls, by=c("super_bowl")) %>%
  #change the units of some columns: viewers to millions, ad_cost to 10,000's
  mutate(avg_us_viewers_M = avg_us_viewers/1000000, total_us_viewers_M = total_us_viewers/1000000, 
         ad_cost_10000 = ad_cost/10000) %>%
  #select columns of interest
  select(super_bowl, date, network, avg_us_viewers_M, total_us_viewers_M, rating_household, 
         share_household, rating_18_49, share_18_49, ad_cost_10000)
q2_data_A
  #59 observations, 10 variables

#make a plot -- AVG_VIEWERS (Millions) vs AD_COST (10,000's)
q2_plot_I <- ggplot(q2_data_A, aes(x=ad_cost_10000, y=avg_us_viewers_M)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Average Super Bowl Viewership vs Advertisement Cost",
       x="Ad Cost ($10,000)", y="Average US Viewership (millions)") +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10))
q2_plot_I

#calculate correlation
q2_corI <- cor(q2_data_A$ad_cost_10000, q2_data_A$avg_us_viewers_M)
q2_corI
  #OUTPUT: 0.7599804

###############################################################################################

#get same data as "q2_data_A" but without modifying the viewers, ad_cost columns
q2_dataA_alt <- tv %>%
  #merge the 2 datasets
  full_join(super_bowls, by=c("super_bowl")) %>%
  #select columns of interest
  select(super_bowl, date, network, avg_us_viewers, total_us_viewers, 
         rating_household, share_household, rating_18_49, share_18_49, ad_cost)

#Plot LOG10 of AVG_VIEWERS vs LOG10 of AD_COST
q2_plot_II <- ggplot(q2_dataA_alt, aes(x=log10(ad_cost), y=log10(avg_us_viewers))) + 
  geom_point() + geom_smooth(se=FALSE, method="lm") +
  labs(title="Average Super Bowl Viewership vs Advertisement Cost",
       x="Ad Cost (Log10)", y="Average US Viewership (Log10)") +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10))
q2_plot_II

#calculate correlation
q2_corII <- cor(q2_dataA_alt$ad_cost, q2_dataA_alt$avg_us_viewers)
q2_corII
  #OUTPUT: 0.7599804

###############################################################################################

#Ad cost vs TV ratings

#Plot RATING_HOUSEHOLD vs AD_COST (10,000's)
q2_plot_III <- ggplot(q2_data_A, aes(x=ad_cost_10000, y=rating_household)) + geom_point() +
  geom_smooth(se=FALSE, method="lm") + 
  labs(title="Percentage Households that Viewed the Super Bowl vs Ad Cost",
       x="Ad Cost ($10,000)", y="% Households that Watched") +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))
q2_plot_III

#calculate correlation
q2_corIII <- cor(q2_data_A$ad_cost_10000, q2_data_A$rating_household)
q2_corIII
  #OUTPUT: 0.06802466

###############################################################################################
#make a plot using the non-modified data -- Plot RATING HOUSEHOLD vs LOG10 of AD_COST
q2_plot_IV <- ggplot(q2_dataA_alt, aes(x=log10(ad_cost), y=rating_household)) + geom_point() +
  geom_smooth(se=FALSE, method="lm") +
  labs(title="Percentage Households that Viewed the Super Bowl vs Ad Cost",
       x="Ad Cost (Log10)", y="% Households that Watched") +
  theme(plot.title=element_text(size=11), axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))
q2_plot_IV

#calculate correlation
q2_corIV <- cor(q2_dataA_alt$ad_cost, q2_dataA_alt$rating_household)
q2_corIV
  #OUTPUT: 0.06802466