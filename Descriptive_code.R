# descriptive statistics
nums <- as.data.frame(meta$subj)
colnames(nums) <- "Sid"
desc <- inner_join(nums, sb_pre[,2:4], by = "Sid")

dup <- duplicated(desc)
desc <- cbind(desc, dup)

desc <- filter(desc, dup!=TRUE)

min(desc$age)
max(desc$age)

mean(desc$age[desc$gender == "Masculin"])
mean(desc$age[desc$gender == "Feminin"])

sd(desc$age[desc$gender == "Masculin"])
sd(desc$age[desc$gender == "Feminin"])

hist(desc$age[desc$gender == "Masculin"])
hist(desc$age[desc$gender == "Feminin"])

sum(duplicated(all_data$subj))
