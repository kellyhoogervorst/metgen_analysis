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
library(gridExtra)

# accuracy
accplot <- ggplot(all_data, aes(x=gender, y=avg_acc, fill=gender)) +
  geom_violin(width=0.4) +
  labs(title="Average accuracy per gender", y="Accuracy", x="Gender") +
  theme(legend.position="none", plot.title=element_text(size=11))

# confidence
confplot <- ggplot(all_data, aes(x=gender, y=avg_conf, fill=gender)) +
  geom_violin(width=0.4) +
  labs(title="Average confidence per gender", y="Confidence score", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

grid.arrange(accplot, confplot, nrow=1, ncol=2)


# Perform MANOVA to see interaction effect --------------------------------

man <- manova(cbind(avg_acc, avg_conf) ~ gender, data = all_data)
summary(man) # is significant

# make new dataset
library(tidyr)

inter <- as.data.frame(cbind(all_data$gender, all_data$avg_acc, all_data$avg_conf), header = T)
colnames(inter) <- c("gender", "acc", "conf")

# scale variables


# into long format
inter_long <- gather(inter, T1, measurement, acc:conf, factor_key=TRUE)
inter_long[, 3] <- as.numeric(unlist(inter_long[, 3])) # because numbers were integers

# visualise interaction effect
inter_long %>% 
  ggplot() +
  aes(x = T1, color = gender, group = gender, y = measurement) +
  scale_y_continuous("accuracy", limits = c(0,1), sec.axis = sec_axis(~ . * 7, name = "Confidence"))
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")

