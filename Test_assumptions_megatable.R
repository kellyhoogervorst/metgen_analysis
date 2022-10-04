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
  geom_boxplot() +
  geom_path(aes(group = gender))


# Seperate modalities -----------------------------------------------------
mod_inter <- as.data.frame(cbind(all_data$gender, all_data$mem_acc, all_data$vis_acc, all_data$gdp_acc, all_data$cal_acc, all_data$mem_conf, all_data$vis_conf, all_data$gdp_conf, all_data$cal_conf), header = T)
colnames(mod_inter) <- c("gender", "mem_acc", "vis_acc", "gdp_acc", "cal_acc", "mem_conf", "vis_conf", "gdp_conf", "cal_conf")

# scale variables
mod_inter[, 2:9] <- as.numeric(unlist(mod_inter[, 2:9])) # because numbers were characters
mod_inter[, 6] <- rescale(mod_inter[, 6])
mod_inter[, 7] <- rescale(mod_inter[, 7])
mod_inter[, 8] <- rescale(mod_inter[, 8])
mod_inter[, 9] <- rescale(mod_inter[, 9])

# into long format
mod_long <- gather(mod_inter, key="T1", value="measurement", mem_acc:cal_conf, factor_key=TRUE)

# visualise interaction effect
ggplot(data = mod_long, aes(x = T1, y = measurement, col = gender))+
  geom_boxplot() 
#+ geom_path(aes(group = gender))

# reshape so modalities are visualized next to eachother
mod_eff <- as.data.frame(cbind(all_data$gender, all_data$mem_acc, all_data$mem_conf, all_data$vis_acc, all_data$vis_conf, all_data$gdp_acc, all_data$gdp_conf, all_data$cal_acc, all_data$cal_conf), header = T)
colnames(mod_eff) <- c("gender", "mem_acc", "mem_conf", "vis_acc", "vis_conf", "gdp_acc", "gdp_conf", "cal_acc", "cal_conf")

# scale variables
mod_eff[, 2:9] <- as.numeric(unlist(mod_eff[, 2:9])) # because numbers were characters
mod_eff[, 3] <- rescale(mod_inter[, 3])
mod_eff[, 5] <- rescale(mod_inter[, 5])
mod_eff[, 7] <- rescale(mod_inter[, 7])
mod_eff[, 9] <- rescale(mod_inter[, 9])

# into long format
eff_long <- gather(mod_eff, key="T1", value="measurement", mem_acc:cal_conf, factor_key=TRUE)

# visualise interaction effect
ggplot(data = eff_long, aes(x = T1, y = measurement, col = gender))+
  geom_boxplot() 
