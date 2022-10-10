# test general differences in sb or conf ----------------------------------
# three seperate t-tests to check gender differences in self belief (pre and post) and trial confidence

# check assumptions -------------------------------------------------------

# test for normality of data distribution ----------------------------------------------------
# shaprio test the values for each group

# self belief pre
shapiro.test(megatable$avg_pre[megatable$gender == "Feminin"])
shapiro.test(megatable$avg_pre[megatable$gender == "Masculin"])

# self belief post
shapiro.test(megatable$avg_post[megatable$gender == "Feminin"])
shapiro.test(megatable$avg_post[megatable$gender == "Masculin"])

# average confidence
shapiro.test(megatable$avg_trial[megatable$gender == "Feminin"])
shapiro.test(megatable$avg_trial[megatable$gender == "Masculin"])

# all groups are significant

# teall_datast for equal variance in standard deviation ---------------------------
# with levene test, all dependent variables for both groups
library(car)

# sb pre
leveneTest(megatable$avg_pre, megatable$gender)

# sb post
leveneTest(megatable$avg_post, megatable$gender)

# trial conf
leveneTest(megatable$avg_trial, megatable$gender)

# all of them are non-significant!


# perform Mann-Whitney Test -----------------------------------------------

# sb pre
ppre <- wilcox.test(megatable$avg_pre ~ megatable$gender)$p.value

# sb post
ppost <- wilcox.test(megatable$avg_post ~ megatable$gender)$p.value

# trial conf
ptrial <- wilcox.test(megatable$avg_trial ~ megatable$gender)$p.value

# all are significant wiehoe


# plot respective data ----------------------------------------------------
library(ggplot2)
library(gridExtra)
library(ggpubr)

# pre
preplot <- ggplot(megatable, aes(x=gender, y=avg_pre, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  labs(title="Average self belief before task per gender", y="Self belief pre") +
  theme(legend.position="none", axis.title.x = element_blank()) 

# post
postplot <- ggplot(megatable, aes(x=gender, y=avg_post, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  labs(title="Average self belief after task per gender", y="Self belief post") +
  theme(legend.position="none", axis.title.x = element_blank()) 

# trial
trialplot <- ggplot(megatable, aes(x=gender, y=avg_trial, fill=gender)) +
  geom_boxplot(notch = T, outlier.shape = NA) +
  labs(title="Average confidence during task per gender", y="During trial confidence") +
  theme(legend.position="none", axis.title.x = element_blank()) 

grid.arrange(preplot, postplot, trialplot, nrow=1, ncol=3, bottom = "Gender")
