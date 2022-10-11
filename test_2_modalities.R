# test for sb and conf differences between modalities------------------------------------------------------------------


# three separate anovas to check gender and modality differences in self belief (pre and post) and trial confidence


# first, put megatable in long format -------------------------------------
library(tidyr)

mega_long <- megatable[,c(2:12, 23:26)]
mega_long <- gather(mega_long, key="mod", value="conf", mem_pre:avg_cal)


# check assumptions -------------------------------------------------------
library(rstatix)

out <- mega_long %>%
  group_by(mod, gender) %>%
    identify_outliers(conf)

# save which subjects are extreme outliers
exlout <- out$subj[out$is.extreme == T]

# exclude the outliers
mega_long <- filter(mega_long, subj!=exlout)
megatablea <- filter(megatable, subj!=exlout)

# test for normality of data distribution ----------------------------------------------------
# shaprio test the values for each group
library(dplyr)

norm <- mega_long %>%
  group_by(mod,gender) %>%
  shapiro_test(conf)

# any significant columns?
sum(norm$p <= 0.05)

# most groups are significant

# correct normality of the data -------------------------------------------
# apparently it is robust as long as the data is approx normally distributed
#generate histograms
nums <- c(5:12, 23:26)

for (i in nums){
  hist(megatablea[, i])
}

# looks alright to me

# test data for equal variance in standard deviation ---------------------------
# with levene test, all dependent variables for both groups
library(car)

#select correct and relevant columns

for (i in nums){
  print(leveneTest(megatablea[,i], megatablea$gender)$`Pr(>F)`)
  } 

# 8 (calpost) is significant
library(ggplot2)
ggplot(megatablea, aes(x = cal_post, fill = gender)) +
geom_histogram(position = "identity", alpha = 0.2, bins = 50, binwidth = 5) 

# also seems fine to me


# run anova ---------------------------------------------------------------


