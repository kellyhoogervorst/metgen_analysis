# test assumptions

# for a t-test,
# test for normality of data distribution ----------------------------------------------------
#shaprio test the values for each group

# accuracy
shapiro.test(all_data$avg_acc[all_data$gender == "Feminin"])
shapiro.test(all_data$avg_acc[all_data$gender == "Masculin"])

# confidence
shapiro.test(all_data$avg_conf[all_data$gender == "Feminin"])
shapiro.test(all_data$avg_conf[all_data$gender == "Masculin"])

# reaction time
shapiro.test(all_data$avg_rt[all_data$gender == "Feminin"])
shapiro.test(all_data$avg_rt[all_data$gender == "Masculin"])

#none of these are non-significant so we will have to do a Mann-Whitney Test 

# test for equal variance in standard deviation ---------------------------
# with levene test, all dependent variables for both groups
library(car)

# accuracy
leveneTest(all_data$avg_acc, all_data$gender)

# confidence
leveneTest(all_data$avg_conf, all_data$gender)

# reaction time
leveneTest(all_data$avg_rt, all_data$gender)

# only the accuracy is significant


# perform Mann-Whitney Test -----------------------------------------------

# accuracy
wilcox.test(all_data$avg_acc ~ all_data$gender)

# confidence
wilcox.test(all_data$avg_conf ~ all_data$gender)

# reaction time
wilcox.test(all_data$avg_rt ~ all_data$gender)

# accuracy and confidence are significant


# plot respective data ----------------------------------------------------
library(ggplot2)

