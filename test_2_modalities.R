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
# assumption of sphericity is automatically tested

# seperate three tests
# pre values
long_pre <- megatable[,c(2, 4:8)]
long_pre <- gather(long_pre, key="mod", value="conf", mem_pre:cal_pre)
# post values
long_post <- megatable[,c(2, 4, 9:12)]
long_post <- gather(long_post, key="mod", value="conf", mem_post:cal_post)
# trial values
long_trial <- megatable[,c(2, 4, 23:26)]
long_trial <- gather(long_trial, key="mod", value="conf", avg_mem:avg_cal)

# run anovas
library(rstatix)

# pre
pre.aov <- anova_test(data = long_pre, dv = conf, wid = subj, between = gender, within = mod)
get_anova_table(pre.aov)
# post
post.aov <- anova_test(data = long_post, dv = conf, wid = subj, between = gender, within = mod)
get_anova_table(post.aov)
# trial
trial.aov <- anova_test(data = long_trial, dv = conf, wid = subj, between = gender, within = mod)
get_anova_table(trial.aov)

# EVERYTHING IS SIGNIFICANT


# post hoc testing of specific differences --------------------------------

# pre
pre.one <- long_pre %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
pre.one

# post
post.one <- long_post %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
post.one

# trial
trial.one <- long_trial %>%
  group_by(mod) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
trial.one


# visualize results -------------------------------------------------------
library(ggplot2)
library(ggsignif)

ggplot(long_pre, aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0,110) +
  labs(title="Average self belief before task per gender", y="Self belief pre", x = "Gender") +
  theme(legend.position="none") + 
  geom_signif(y_position = 100, xmin = 1.8, xmax = 2.2, annotation = c("***"), tip_length = 0.01, textsize = 7, size = 0.5)

ggplot(long_post, aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0,110) +
  labs(title="Average self belief after task per gender", y="Self belief posrt", x = "Gender") +
  theme(legend.position="none") + 
  geom_signif(y_position = 100, xmin = c(0.8, 1.8, 3.8), xmax = c(1.2, 2.2, 4.2), annotation = c("*", "***", "***"), tip_length = 0.01, textsize = 7, size = 0.5)

ggplot(long_trial, aes(x=mod, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0,8) +
  labs(title="Average confidence during task per gender", y="Confidence", x = "Gender") +
  theme(legend.position="none") + 
  geom_signif(y_position = 7, xmin = c(1.8, 2.8), xmax = c(2.2, 3.2), annotation = c("***", "*"), tip_length = 0.01, textsize = 7, size = 0.5)
