# descriptive statistics

# number of males vs. females
table(all_data$gender)

# age information
min(all_data$age)
max(all_data$age)

mean(all_data$age[all_data$gender == "Masculin"])
mean(all_data$age[all_data$gender == "Feminin"])

sd(all_data$age[all_data$gender == "Masculin"])
sd(all_data$age[all_data$gender == "Feminin"])



