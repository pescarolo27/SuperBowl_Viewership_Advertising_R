#Q2 Part B -- Which variable (viewers, ratings, or ads) increases first?

q2B_data_I <- tv %>%
  #merge the 2 datasets
  full_join(super_bowls, by=c("super_bowl")) %>%
  #filter for the first 10 SB's
  filter(super_bowl <= 10) %>%
  #select columns of interest
  select(super_bowl, avg_us_viewers, rating_household, share_household, ad_cost)
q2B_data_I
  #11 observations (for 10 SB's), 5 variables

#fix SB 1 data (it had data for 2 tv networks)
q2B_SB1 <- q2B_data_I %>%
  #filter for SB 1
  filter(super_bowl == 1) %>%
  #calculate statistics for columns of interest
  summarize(super_bowl = super_bowl, avg_us_viewers = sum(avg_us_viewers), 
            rating_household = mean(rating_household), share_household = mean(share_household), 
            ad_cost = sum(ad_cost))
#remove the duplicate row
q2B_SB1 <- q2B_SB1[-1,]
#q2B_SB1

#impute this new data into the original data for SB 1
q2B_data_II <- q2B_data_I %>%
  #select SB's 2-10
  filter(super_bowl != 1) %>%
  #add new SB 1 data
  bind_rows(q2B_SB1)
q2B_data_II
  #10 observations, 5 variables

############################################################################################
#Q2 conclusion --- assign specified variable

first_to_increase <- "ratings"