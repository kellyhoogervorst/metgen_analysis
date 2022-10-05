# testing all metacognition ratings

# converting the accuracy data
library(tidyr)

# mean confidence 
trial_conf <- data_meta[,c(1, 2, 6, 7)]
trial_conf <- trial_conf %>%
  group_by(subject, modality, accuracy) %>%
  summarise(mean(confidence, na.rm=T))
colnames(trial_conf) <- cbind("subj", "mod", "acc", "conf")

# reshape to wide format
trial_data <- trial_conf %>%
  pivot_wider(id_cols = subj, names_from = c(mod, acc), values_from = conf)
colnames(trial_data) <- cbind("subj", "cal_0", "cal_1", "gdp_0", "gdp_1", "mem_0", "mem_1", "vis_0", "vis_1")

# add averages
trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("mem_0", "mem_1"))))
trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("vis_0", "vis_1"))))
trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("gdp_0", "gdp_1"))))
trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("cal_0", "cal_1"))))

trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("mem_0", "vis_0", "gdp_0", "cal_0"))))
trial_data <- cbind(trial_data, rowMeans(subset(trial_data, select = c("mem_1", "vis_1", "gdp_1", "cal_1"))))

trial_data <- cbind(trial_data, rowMeans(subset(trial_data[,2:9])))

colnames(trial_data) <- cbind("subj", "cal_0", "cal_1", "gdp_0", "gdp_1", "mem_0", "mem_1", "vis_0", "vis_1", "avg_mem", "avg_vis", "avg_gdp", "avg_cal", "avg_0", "avg_1", "tot_avg")

#loading in the pre and post confidence scores
meta <- all_data[, 1:12]

# merge the confidence data
meta <- full_join(meta, trial_data, by = "subj")

#delete non-binary person
meta <- filter(meta, gender!="non-binary")
#delete non-sub
meta <- filter(meta, subj!=996)
#delete na
meta <- na.omit(meta)


# plot time difference ----------------------------------------------------


