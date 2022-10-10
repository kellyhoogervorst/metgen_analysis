
# Descriptive statistics --------------------------------------------------

# load in megatable
megatable <- read.csv(file.choose(),header = T) #select megatable


# number of males vs. females
table(megatable$gender)

# age information
min(megatable$age)
max(megatable$age)

mean(megatable$age[megatable$gender == "Masculin"])
mean(megatable$age[megatable$gender == "Feminin"])

sd(megatable$age[megatable$gender == "Masculin"])
sd(megatable$age[megatable$gender == "Feminin"])



