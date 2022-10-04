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

