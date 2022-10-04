# test modalities seperately

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


# statistical testing -----------------------------------------------------
# two outcome variables, continuous, two predictive variables, discrete
# factorial manova

# first, testing of assumptions -------------------------------------------
# shapiro for normal distribution
shap_p <- as.data.frame(matrix(nrow =1, ncol = 16))
#colnames(shap_p) <- cbind("F", "M")

shap_p[1] <- shapiro.test(all_data$mem_acc[all_data$gender == "Feminin"])$p.value
shap_p[2] <- shapiro.test(all_data$mem_acc[all_data$gender == "Masculin"])$p.value
shap_p[3] <- shapiro.test(all_data$mem_conf[all_data$gender == "Feminin"])$p.value
shap_p[4] <- shapiro.test(all_data$mem_conf[all_data$gender == "Masculin"])$p.value
shap_p[5] <- shapiro.test(all_data$vis_acc[all_data$gender == "Feminin"])$p.value
shap_p[6] <- shapiro.test(all_data$vis_acc[all_data$gender == "Masculin"])$p.value
shap_p[7] <- shapiro.test(all_data$vis_conf[all_data$gender == "Feminin"])$p.value
shap_p[8] <- shapiro.test(all_data$vis_conf[all_data$gender == "Masculin"])$p.value
shap_p[9] <- shapiro.test(all_data$gdp_acc[all_data$gender == "Feminin"])$p.value
shap_p[10] <- shapiro.test(all_data$gdp_acc[all_data$gender == "Masculin"])$p.value
shap_p[11] <- shapiro.test(all_data$gdp_conf[all_data$gender == "Feminin"])$p.value
shap_p[12] <- shapiro.test(all_data$gdp_conf[all_data$gender == "Masculin"])$p.value
shap_p[13] <- shapiro.test(all_data$cal_acc[all_data$gender == "Feminin"])$p.value
shap_p[14] <- shapiro.test(all_data$cal_acc[all_data$gender == "Masculin"])$p.value
shap_p[15] <- shapiro.test(all_data$cal_conf[all_data$gender == "Feminin"])$p.value
shap_p[16] <- shapiro.test(all_data$cal_conf[all_data$gender == "Masculin"])$p.value

# definitely some significant

# do the manova
#attach(all_data)
man <- manova(cbind(mem_acc, mem_conf, vis_acc, vis_conf, gdp_acc, gdp_conf, cal_acc, cal_conf) ~ gender, data = all_data)
summary(man) # is significant
