# Create data set in wide format
rates <- popData %>%
  select(County, MortRate)

rates$adjSmooth <- gaCounty_df$MortRateSmooth

rates$invDistSmooth <- gaCounty_df$MortRateSmoothDist

# Use the gather() function to transform the data from wide to long format
# Data must be in long format to be used properly in ggplot()
rate2 <- rates %>% 
  gather(key="statistic", value="val",
         MortRate,
         adjSmooth,
         invDistSmooth)

# Specify level order for the "statistic" variable
stat_levels <- c("adjSmooth","MortRate","invDistSmooth")

# Create a variable called statistic that is a factor based on the original
# "statistic" variable which also has levels in the order indicated in 
# stat_levels
rate2$statistic <- factor(rate2$statistic, 
                          levels = stat_levels,
                          labels = c("Adj Smooth", 
                                     "Mortality Raw", 
                                     "Inv Dist Smooth"))


spider <- rate2 %>% 
  arrange(statistic) %>% # This was the key. 
                         # geom_path plots the points in the order which they 
                         # are presented in the data. geom_line plots the 
                         # points in the order which the x values are sorted.
  ggplot(aes(x=val, y=statistic, group=County)) + 
  geom_path(alpha = 1 / 4) + # I added the alpha = argument to make the lines
                             # transparent. The denominator of 1 / x can be 
                             # interpreted as the number of objects you can 
                             # stack before the color is totally opaque.
  scale_x_continuous(breaks = seq(0,max(rate2$val), by = .05)) +
  labs(x = "Mortality Rate Estimate", 
       y = "Smoothing Method",
       title = "Mortality Rate Changes via Smoothing")

spider 

bigVals <- rate2$County[rate2$val > .3]

spider +
  geom_path(data = arrange(filter(rate2, County %in% bigVals), statistic),
         aes(x=val, y=statistic, group=County)) +
  geom_text(data = arrange(filter(rate2,
                                  County %in% bigVals &
                                    grepl("Mort", statistic)),
                           statistic),
            aes(x=val, y=statistic, label = County),
            angle = 45, check_overlap = T,
            alpha = 1 / 1.5) +
  labs(subtitle = "Only the First County Printed in Overplotted Values")
