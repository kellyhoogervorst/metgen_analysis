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
library(scales)
library(ggplot2)

inter <- as.data.frame(cbind(all_data$gender, all_data$avg_acc, all_data$avg_conf), header = T)
colnames(inter) <- c("gender", "acc", "conf")

# scale variables
inter[, 2:3] <- as.numeric(unlist(inter[, 2:3])) # because numbers were characters
scale_conf <- rescale(inter$conf)
inter <- cbind(inter, scale_conf)
inter <- inter[, cbind(1, 2, 4)]

# into long format
inter_long <- gather(inter, key="T1", value="measurement", acc:scale_conf, factor_key=TRUE)

# visualise interaction effect
ggplot(data = inter_long, aes(x = T1, y = measurement, col = gender))+
  geom_boxplot()
