# test differences between hit or miss ----------------------------------

# first, put megatable in long format -------------------------------------
library(tidyr)

mega_acc <- megatable[,c(2, 4, 15:29)]
acc_long <- megatable[,c(2, 4, 15:29)]
acc_long <- gather(acc_long, key="mod", value="conf", cal_0:avg_trial)

# check assumptions -------------------------------------------------------

# first check outliers
library(rstatix)

out <- acc_long %>%
  group_by(mod, gender) %>%
  identify_outliers(conf)

# save which subjects are extreme outliers
exout <- out$subj[out$is.extreme == T]

# 0 , so no outliers have to be removed

# test for normality of data distribution ----------------------------------------------------
# shaprio test the values for each group
library(dplyr)

norm <- acc_long %>%
  group_by(mod,gender) %>%
  shapiro_test(conf)

# any significant columns?
sum(norm$p <= 0.05)

# significant columns, so check m
#generate histograms
nums <- c(3:17)

for (i in nums){
  hist(mega_acc[, i])
}

# doesn't look to bad

# test data for equal variance in standard deviation ---------------------------
# with levene test, all dependent variables for both groups
library(car)

# select correct and relevant columns

for (i in nums){
  print(leveneTest(mega_acc[,i], mega_acc$gender)$`Pr(>F)`)
} 

# 4&5 (gdp_1&mem_0) are significant
library(ggplot2)
ggplot(mega_acc, aes(x = gdp_1, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 10, binwidth = 1) 

ggplot(mega_acc, aes(x = mem_0, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.2, bins = 10, binwidth = 1) 

# also seems fine

# start with average acc anovas -------------------------------------------

# seperate three tests
# pre values
long_avgacc <- mega_acc[,c(1,2, 15:16)]
long_avgacc <- gather(long_avgacc, key="acc", value="conf", avg_0:avg_1)

# run anovas
library(rstatix)

avg.aov <- anova_test(data = long_avgacc, dv = conf, wid = subj, between = gender, within = acc)
get_anova_table(avg.aov)

# post hoc for accuracy
avg.one <- long_avgacc %>%
  group_by(acc) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
avg.one

agen.one <- long_avgacc %>%
  group_by(gender) %>%
  anova_test(dv = conf, wid = subj, within = acc) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
agen.one


# plot
library(ggplot2)
library(ggsignif)

ggplot(long_avgacc, aes(x=acc, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0, 8) +
  labs(title="Average accuracy confidence per gender", y="Confidence", x = "Gender") +
  theme(legend.position="none") + 
  geom_signif(y_position = 7, xmin = c(0.8, 1.8), xmax = c(1.2, 2.2), annotation = c("***", "**"), tip_length = 0.01, textsize = 7, size = 0.5)

# also plot interaction
ggplot(long_avgacc, aes(x = acc, y = conf, group = gender, col=gender))+
  geom_point(shape=1, size = 2, position = position_dodge(width = 0.05)) +
  geom_line() +
  labs(title="Accuracy dependent trial confidence per gender", y="Confidence", x = "Accuracy") +
  ylim(0, 8) +
  geom_signif(y_position = 7, xmin = 1, xmax = 2, annotation = c("**"), tip_length = 0.01, textsize = 7, size = 0.5, col = "black")


# across different modalities ---------------------------------------------


