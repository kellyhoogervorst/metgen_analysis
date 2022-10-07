# testing all metacognition ratings

# converting the accuracy data
library(tidyr)
library(dplyr)

# mean confidence 
trial_conf <- data_meta[,c(1, 2, 6, 7)]
trial_conf <- trial_conf %>%
  group_by(subj, mod, acc) %>%
  summarise(mean(conf, na.rm=T))
colnames(trial_conf) <- cbind("subj", "mod", "acc", "conf")

# rescale trial data
library(scales)
trial_conf$conf <- rescale(trial_conf$conf, to = c(0, 100))

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
meta <- inner_join(meta, trial_data, by = "subj")

#delete non-binary person
meta <- filter(meta, gender!="non-binary")
#delete non-sub
meta <- filter(meta, subj!=996)
#delete na
meta <- na.omit(meta)


# total values in long format for plotting
time <- gather(meta, time, conf, c(avg_pre, tot_avg, avg_post), factor_key = T)
time <- time[,c(1, 2, 25, 26)]

# plot time difference ----------------------------------------------------
time %>%
  ggplot(aes(x = time, y = conf, col = gender))+
  geom_boxplot(outlier.shape = NA, notch = T)

time <- time[time$subj != 173,]

mixed_anova <- aov_ez(
  id = "subj",
  dv = "conf",
  data = time,
  between = "gender",
  within = "time"
)

afex_plot(mixed_anova, x = "time", trace = "gender")


# By modality -------------------------------------------------------------
timacc <- gather(meta, time, conf, c(mem_pre, vis_pre, gdp_pre, cal_pre, avg_mem, avg_vis, avg_gdp, avg_cal, mem_post, vis_post, gdp_post, cal_post), factor_key = T)
timacc <- timacc[,c(1, 2, 16, 17)]

