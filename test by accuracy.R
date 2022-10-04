# test by hit or miss


# create new accuracy table -----------------------------------------------
library(dplyr)

# remove na
metaa <- na.omit(metaf)

# couple gender
metaa <- full_join(selfbel[,1:2], metaa, by = "subj")

acctab <- metaa %>%
  group_by(subj, gender, mod, acc) %>%
  summarise(mean(conf))

names(acctab)[names(acctab) == "mean(conf)"] <- "conf"

#delete non-binary person
acctab <- filter(acctab, gender!="non-binary")
#delete non-sub
acctab <- filter(acctab, subj!=996)
#delete na
acctab <- na.omit(acctab)

# plot
library(ggplot2)
acctab[, 4] <- as.character(unlist(acctab[, 4])) # because numbers were integers

acctab %>%
 ggplot(aes(x = acc, y = conf, col = gender))+
  geom_boxplot() +
  facet_grid(. ~ mod)


# statistical testing -----------------------------------------------------
# one continuous dependent variable, three categorical predictive variables
# mixed factorial anova
library(lme4)
library(Matrix)
library(tidyr)
library(afex)
library(performance)
library(emmeans)

# data is already in long format, nice
acctab <- acctab[acctab$subj != 173,]

mixed_anova <- aov_ez(
  id = "subj",
  dv = "conf",
  data = acctab,
  between = "gender",
  within = c("mod", "acc")
)

test_sphericity(mixed_anova)

afex_plot(mixed_anova, x = "mod", trace = "gender")

# alternative code
library(rstatix)
colnames(acctab) <- cbind("subj", "gender", "mod", "acc", "conf")
res.aov <- acctab %>%
            ungroup() %>%
             anova_test(dv = "conf", wid = "subj", between = "gender", within = c("mod", "acc"))

get_anova_table(res.aov)

#post hoc
pwc <- acctab %>%
  ungroup() %>%
  pairwise_t_test(
    conf ~ c(gender, mod, acc), paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-statistic, -df) # Remove details

# accuracy difference -----------------------------------------------------

deltacc <- acctab %>%
  group_by(subj, gender, mod) %>%
  summarise(conf[acc==1]-conf[acc==0])
colnames(deltacc) <- cbind("subj", "gender", "mod", "delta_conf")

deltacc %>%
  ggplot(aes(x = mod, y = delta_conf, fill = gender))+
  geom_boxplot(notch = T)
