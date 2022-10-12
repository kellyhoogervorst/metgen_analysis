# test gender differences in trial conf modulation of post self be --------
# first multiple regression with gender and avg confidence influencing updating of beliefs

# make new dataset
library(dplyr)

mreg <- megatable[,c(2, 4, 13, 14, 27:29)]
mreg <- mreg %>%
  mutate(sb_diff = avg_pre - avg_post)

regmod <- lm(sb_diff ~ gender + avg_0 + avg_1, data = mreg)
summary(regmod)0

# average confidence doesn't influence the difference form pre to post
# only confidence for correct trials is slightly significant

# maybe it is influenced by T1, so actual accuracy, women are more aware of it in T2 so could be interesting


# two way mixed anova to see difference of hit and miss trials ------------


