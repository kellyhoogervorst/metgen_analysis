# create megatable

# load data ---------------------------------------------------------------

data_meta <- read.csv(file.choose(),header = T) #select metacognition_TrialData_master
sb_pre <- read.table(file.choose(), sep = ";", header = T) #select self_belief_pre_labels
sb_post <- read.table(file.choose(), sep = ";", header = T) #select self_belief_post_labels


# For self belief data ----------------------------------------------------

# select relevant data -------------------------------------------------------------
library(tidyr)
library(dplyr)

#pre test data
pref <- as.data.frame(cbind(sb_pre$Sid, sb_pre$gender, sb_pre$self_memory_pre, sb_pre$self_visual_pre, sb_pre$self_gdp_pre, sb_pre$self_calories_pre))
colnames(pref) <- c("subj", "gender", "mem_pre", "vis_pre", "gdp_pre", "cal_pre")
pref[, c(1, 3:6)] <- as.numeric(unlist(pref[, c(1, 3:6)])) # because numbers were characters

#post test data
postf <- as.data.frame(cbind(sb_post$Sid, sb_post$self_memory_post, sb_post$self_visual_post, sb_post$self_gdp_post, sb_post$self_calories_post))
colnames(postf) <- c("subj", "mem_post", "vis_post", "gdp_post", "cal_post")
postf[, c(1:5)] <- as.numeric(unlist(postf[, c(1:5)])) # because numbers were integers

#merge the two sets
selfbel <- inner_join(pref, postf, by="subj")


#calculate and add average pre and post scores
selfbel <- cbind(selfbel, rowMeans(subset(selfbel, select = c("mem_pre", "vis_pre", "gdp_pre", "cal_pre"))))
selfbel <- cbind(selfbel, rowMeans(subset(selfbel, select = c("mem_post", "vis_post", "gdp_post", "cal_post"))))
colnames(selfbel)[11:12] <- c("avg_pre", "avg_post")

# for meta data -----------------------------------------------------------
metaf <- data_meta[,c(1, 2, 6:8)]
colnames(metaf) <- c("subj", "mod", "acc", "conf", "rt")

# congregate scores to fit other dataset
metaw <- as.data.frame(matrix(nrow = 329, ncol = 13))
colnames(metaw) <- c("subj", "mem_acc", "mem_conf", "mem_rt", "vis_acc", "vis_conf", "vis_rt", "gdp_acc", "gdp_conf", "gdp_rt", "cal_acc", "cal_conf", "cal_rt")

# loop over all subjects - TAKES A WHILE TO RUN > MAKE MORE EFFICIENT!
for (i in metaf$subj){
  submeta <- metaf[metaf$subj == i,2:5]
  metaw[i,1] <- i
  metaw[i,2] <- mean(submeta[submeta$mod == 'memory', 'acc'], na.rm =T)
  metaw[i,3] <- mean(submeta[submeta$mod == 'memory', 'conf'], na.rm =T)
  metaw[i,4] <- mean(submeta[submeta$mod == 'memory', 'rt'], na.rm =T)
  metaw[i,5] <- mean(submeta[submeta$mod == 'vision', 'acc'], na.rm =T)
  metaw[i,6] <- mean(submeta[submeta$mod == 'vision', 'conf'], na.rm =T)
  metaw[i,7] <- mean(submeta[submeta$mod == 'vision', 'rt'], na.rm =T)
  metaw[i,8] <- mean(submeta[submeta$mod == 'GDP', 'acc'], na.rm =T)
  metaw[i,9] <- mean(submeta[submeta$mod == 'GDP', 'conf'], na.rm =T)
  metaw[i,10] <- mean(submeta[submeta$mod == 'GDP', 'rt'], na.rm =T)
  metaw[i,11] <- mean(submeta[submeta$mod == 'Calories', 'acc'], na.rm =T)
  metaw[i,12] <- mean(submeta[submeta$mod == 'Calories', 'conf'], na.rm =T)
  metaw[i,13] <- mean(submeta[submeta$mod == 'Calories', 'rt'], na.rm =T)
}


# combine the two sets ----------------------------------------------------

all_data <- inner_join(selfbel, metaw, by = "subj")

#delete non-binary person
all_data <- filter(all_data, gender!="non-binary")

#delete non-sub
all_data <- filter(all_data, subj!=996)

#delete duplicates
all_data = cbind(all_data, duplicated(all_data$subj))
all_data <- filter(all_data, all_data[,28]!=TRUE)

#delete na
all_data <- na.omit(all_data)

# add acc, conf and rt averages
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_acc", "vis_acc", "gdp_acc", "cal_acc"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_conf", "vis_conf", "gdp_conf", "cal_conf"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_rt", "vis_rt", "gdp_rt", "cal_rt"))))
colnames(all_data)[25:27] <- c("avg_acc", "avg_conf", "avg_rt")

# save megatable as csv
write.csv(all_data, file = "megatable.csv")
