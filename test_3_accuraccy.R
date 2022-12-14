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

# seperate average scores
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

# separate modality specific scores
long_acc <- mega_acc[,c(1:10)]
long_acc <- gather(long_acc, key="acc", value="conf", cal_0:vis_1)

# run anovas
library(rstatix)

acc.aov <- anova_test(data = long_acc, dv = conf, wid = subj, between = gender, within = acc)
get_anova_table(acc.aov)

# post hoc for accuracy
acc.one <- long_acc %>%
  group_by(acc) %>%
  anova_test(dv = conf, wid = subj, between = gender) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
acc.one

# plot
library(ggplot2)
library(gridExtra)
library(ggsignif)

# split modalities for plotting
long_mem <- mega_acc[,c(1,2,7,8)]
long_mem <- gather(long_mem, key="acc", value="conf", mem_0:mem_1)

long_vis <- mega_acc[,c(1,2,9,10)]
long_vis <- gather(long_vis, key="acc", value="conf", vis_0:vis_1)

long_gdp <- mega_acc[,c(1,2,5,6)]
long_gdp <- gather(long_gdp, key="acc", value="conf", gdp_0:gdp_1)

long_cal <- mega_acc[,c(1:4)]
long_cal <- gather(long_cal, key="acc", value="conf", cal_0:cal_1)

# now the real plotting

membox <- ggplot(long_mem, aes(x=acc, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0, 8) +
  labs(title="Average accuracy confidence for memory per gender", y="Confidence", x = "Accuracy") +
  theme(legend.position="none") + 
  geom_signif(y_position = 7, xmin = 0.8, xmax = 1.2, annotation = "**", tip_length = 0.01, textsize = 7, size = 0.5)

visbox <- ggplot(long_vis, aes(x=acc, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0, 8) +
  labs(title="Average accuracy confidence for vision per gender", y="Confidence", x = "Accuracy") +
  theme(legend.position="none")

gdpbox <- ggplot(long_gdp, aes(x=acc, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0, 8) +
  labs(title="Average accuracy confidence for gdp per gender", y="Confidence", x = "Accuracy") +
  theme(legend.position="none") + 
  geom_signif(y_position = 7, xmin = c(0.8, 1.8), xmax = c(1.2, 2.2), annotation = c("***", "***"), tip_length = 0.01, textsize = 7, size = 0.5)

calbox <- ggplot(long_cal, aes(x=acc, y=conf, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  ylim(0, 8) +
  labs(title="Average accuracy confidence for calories per gender", y="Confidence", x = "Accuracy") +
  theme(legend.position="none") 

grid.arrange(membox, visbox, gdpbox, calbox, nrow=2, ncol=2, bottom = "Accuracy")


# for interaction plots ---------------------------------------------------
mem.aov <- anova_test(data = long_mem, dv = conf, wid = subj, between = gender, within = acc)
vis.aov <- anova_test(data = long_vis, dv = conf, wid = subj, between = gender, within = acc)
gdp.aov <- anova_test(data = long_gdp, dv = conf, wid = subj, between = gender, within = acc)
cal.aov <- anova_test(data = long_cal, dv = conf, wid = subj, between = gender, within = acc)

get_anova_table(mem.aov)
get_anova_table(vis.aov)
get_anova_table(gdp.aov)
get_anova_table(cal.aov)

#plot
intmem <- ggplot(long_mem, aes(x = acc, y = conf, group = gender, col=gender))+
  geom_point(shape=1, size = 2, position = position_dodge(width = 0.05)) +
  geom_line() +
  labs(title="Accuracy dependent memory confidence per gender", y="Confidence", x = "Accuracy") +
  ylim(0, 8) +
  geom_signif(y_position = 7, xmin = 1, xmax = 2, annotation = c("**"), tip_length = 0.01, textsize = 7, size = 0.5, col = "black")

intvis <- ggplot(long_vis, aes(x = acc, y = conf, group = gender, col=gender))+
  geom_point(shape=1, size = 2, position = position_dodge(width = 0.05)) +
  geom_line() +
  labs(title="Accuracy dependent vision confidence per gender", y="Confidence", x = "Accuracy") +
  ylim(0, 8) 

intgdp <- ggplot(long_gdp, aes(x = acc, y = conf, group = gender, col=gender))+
  geom_point(shape=1, size = 2, position = position_dodge(width = 0.05)) +
  geom_line() +
  labs(title="Accuracy dependent gdp confidence per gender", y="Confidence", x = "Accuracy") +
  ylim(0, 8) 

intcal <- ggplot(long_cal, aes(x = acc, y = conf, group = gender, col=gender))+
  geom_point(shape=1, size = 2, position = position_dodge(width = 0.05)) +
  geom_line() +
  labs(title="Accuracy dependent calorie confidence per gender", y="Confidence", x = "Accuracy") +
  ylim(0, 8) +
  geom_signif(y_position = 7, xmin = 1, xmax = 2, annotation = c("*"), tip_length = 0.01, textsize = 7, size = 0.5, col = "black")

grid.arrange(intmem, intvis, intgdp, intcal, nrow=2, ncol=2, bottom = "Accuracy")
