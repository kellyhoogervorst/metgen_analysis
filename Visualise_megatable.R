# visualise megatable data

# make some visualizations ------------------------------------------------
library(ggplot2)
library(gridExtra)


# first, T1 data ----------------------------------------------------------

accplot <- ggplot(all_data, aes(x=gender, y=avg_acc, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="average accuracy", y="accuracy", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

confplot <- ggplot(all_data, aes(x=gender, y=avg_conf, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="average confidence", y="confidence score", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

rtplot <- ggplot(all_data, aes(x=gender, y=avg_rt, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="average reaction time", y="reaction time", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

grid.arrange(accplot, confplot, rtplot, nrow=1, ncol=3)


# how about modality sensitive

# accuracy 

accmem <- ggplot(all_data, aes(x=gender, y=mem_acc, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  ylim(0, 1) +
  labs(title="accuracy memory", y="accuracy", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
accvis <- ggplot(all_data, aes(x=gender, y=vis_acc, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  ylim(0, 1) +
  labs(title="accuracy visual", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
accgdp <- ggplot(all_data, aes(x=gender, y=gdp_acc, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  ylim(0, 1) +
  labs(title="accuracy gdp", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
acccal <- ggplot(all_data, aes(x=gender, y=cal_acc, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  ylim(0, 1) +
  labs(title="accuracy calorie", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

grid.arrange(accmem, accvis, accgdp, acccal, nrow=1, ncol=4)

# confidence 

confmem <- ggplot(all_data, aes(x=gender, y=mem_conf, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="confidence memory", y="confidence", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
confvis <- ggplot(all_data, aes(x=gender, y=vis_conf, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="confidence visual", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
confgdp <- ggplot(all_data, aes(x=gender, y=gdp_conf, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="confidence gdp", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
confcal <- ggplot(all_data, aes(x=gender, y=cal_conf, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="confidence calorie", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

grid.arrange(confmem, confvis, confgdp, confcal, nrow=1, ncol=4)

# reaction time 

rtmem <- ggplot(all_data, aes(x=gender, y=mem_rt, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="reaction time memory", y="accuracy", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
rtvis <- ggplot(all_data, aes(x=gender, y=vis_rt, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="reaction time visual", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
rtgdp <- ggplot(all_data, aes(x=gender, y=gdp_rt, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="reaction time gdp", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))
rtcal <- ggplot(all_data, aes(x=gender, y=cal_rt, fill=gender)) +
  geom_violin(width=0.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="reaction time calorie", x="Gender")+
  theme(legend.position="none", plot.title=element_text(size=11))

grid.arrange(rtmem, rtvis, rtgdp, rtcal, nrow=1, ncol=4)
