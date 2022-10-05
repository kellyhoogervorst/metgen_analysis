# create megatable

# load data ---------------------------------------------------------------

data_meta <- read.csv(file.choose(),header = T) #select metacognition_TrialData_master
sb_pre <- read.table(file.choose(), sep = ";", header = T) #select self_belief_pre_labels
sb_post <- read.table(file.choose(), sep = ";", header = T) #select self_belief_post_labels

# check duplicates in data
sum(duplicated(data_meta))
sum(duplicated(sb_pre))
sum(duplicated(sb_post))

# check duplicates in subject number
sum(table(data_meta$subject)!=600)
sum(duplicated(sb_pre$Sid))
sum(duplicated(sb_post$Sid))

# both self belief scores have duplicates, delete them
sb_pre <- sb_pre[-which(duplicated(sb_pre$Sid), arr.ind = FALSE, useNames = TRUE), ]
sb_post <- sb_post[-which(duplicated(sb_post$Sid), arr.ind = FALSE, useNames = TRUE),]

# For self belief data ----------------------------------------------------

# select relevant data -------------------------------------------------------------
library(tidyr)
library(dplyr)

# pre test data
pref <- as.data.frame(cbind(sb_pre$Sid, sb_pre$age, sb_pre$gender, sb_pre$self_memory_pre, sb_pre$self_visual_pre, sb_pre$self_gdp_pre, sb_pre$self_calories_pre))
colnames(pref) <- c("subj","age", "gender", "mem_pre", "vis_pre", "gdp_pre", "cal_pre")
pref[, c(1,2,4:7)] <- as.numeric(unlist(pref[, c(1,2,4:7)])) # because numbers were characters

# post test data
postf <- as.data.frame(cbind(sb_post$Sid, sb_post$self_memory_post, sb_post$self_visual_post, sb_post$self_gdp_post, sb_post$self_calories_post))
colnames(postf) <- c("subj", "mem_post", "vis_post", "gdp_post", "cal_post")
postf[, c(1:5)] <- as.numeric(unlist(postf[, c(1:5)])) # because numbers were integers

# merge the two sets
selfbel <- inner_join(pref, postf, by="subj")

# calculate and add average pre and post scores
selfbel <- cbind(selfbel, rowMeans(subset(selfbel, select = c("mem_pre", "vis_pre", "gdp_pre", "cal_pre"))))
selfbel <- cbind(selfbel, rowMeans(subset(selfbel, select = c("mem_post", "vis_post", "gdp_post", "cal_post"))))
colnames(selfbel)[12:13] <- c("avg_pre", "avg_post")


# For meta data -----------------------------------------------------------
trialdata <- data_meta[,c(1, 2, 6, 7)]
colnames(trialdata) <- c("subj", "mod", "acc", "conf")

# average the confidence scores over similar trials
trialw <- trialdata %>%
  group_by(subj, mod, acc)%>%
  summarise(mean(conf, na.rm = T))
colnames(trialw) <- c("subj", "mod", "acc", "conf")

# from long to wide format
trialf <- trialw %>%
  pivot_wider(id_cols = subj, names_from = c(mod, acc), values_from = conf)
colnames(trialf) <- cbind("subj", "cal_0", "cal_1", "gdp_0", "gdp_1", "mem_0", "mem_1", "vis_0", "vis_1")


# Combine the two sets ----------------------------------------------------
all_data <- inner_join(selfbel, trialf, by = "subj")

# delete non-binary person
all_data <- filter(all_data, gender!="non-binary")

# check duplicates
all_data = cbind(all_data, duplicated(all_data$subj))
all_data <- filter(all_data, all_data[,28]!=TRUE)

#delete na
all_data <- na.omit(all_data)

# add acc, conf and rt averages
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_0", "mem_1"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("vis_0", "vis_1"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("gdp_0", "gdp_1"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("cal_0", "cal_1"))))

all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_0", "vis_0", "gdp_0", "cal_0"))))
all_data <- cbind(all_data, rowMeans(subset(all_data, select = c("mem_1", "vis_1", "gdp_1", "cal_1"))))

all_data <- cbind(all_data, rowMeans(subset(all_data[,14:21])))

colnames(all_data)[22:28] <- cbind( "avg_mem", "avg_vis", "avg_gdp", "avg_cal", "avg_0", "avg_1", "avg_trial")

# save megatable as csv
write.csv(all_data, file = "megatable.csv")
