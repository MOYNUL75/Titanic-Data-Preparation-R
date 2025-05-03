library(dplyr)
library(ggplot2)
library(tidyr)

mydata <- read.csv("D:/8th Semester/INTRODUCTION TO DATA SCIENCE [B]/MID/IDS_Project/Midterm_Project.csv", header = TRUE, sep = ",")

View(mydata)
str(mydata)
summary(mydata)
missing_indices <- lapply(mydata, function(x) which(is.na(x)))
print (missing_indices)


mydata$Gender <- ifelse(mydata$Gender == "", NA, mydata$Gender)
View(mydata)


people_2_to_152_with_Gender <- mydata %>%filter(age >= 2 & age <= 152, !is.na(Gender)) %>% select(age, Gender)
print(people_2_to_152_with_Gender)
Gender_counts <- table(people_2_to_152_with_Gender$Gender)
most_common_Gender <- names(Gender_counts)[which.max(Gender_counts)]
print(most_common_Gender)
mydata <- mydata %>%
  mutate(Gender = ifelse(is.na(Gender), most_common_Gender, Gender))
View(mydata)


mydata$Gender <- factor(mydata$Gender,
                        levels = c("male", "female"),
                        labels = c(1, 0))
which(is.na(mydata$Gender))
View (mydata)


mydata$age <- round(mydata$age)
age_median <- round(median(mydata$age, na.rm = TRUE))
mydata$age[is.na(mydata$age)] <- age_median

mydata$age <- round(mydata$age)
age_mean <- round(mean(mydata$age, na.rm = TRUE))
mydata$age[is.na(mydata$age)] <- age_mean

Q1 <- quantile(mydata$age[mydata$age >= 2 & mydata$age <= 70], 0.25)
Q3 <- quantile(mydata$age[mydata$age >= 2 & mydata$age <= 70], 0.75)
IQR_value <- Q3 - Q1

threshold <- 1.5

outlier_condition <- (mydata$age < (Q1 - threshold * IQR_value)) | (mydata$age > (Q3 + threshold * IQR_value))

mydata <- mydata[!outlier_condition, ]
View(mydata)


mydata <- mydata %>%
  filter(!is.na(age)) %>%
  mutate(row_number = row_number())


people_2_to_152_with_sibsp <- mydata %>%filter(age >= 2 & age <= 152, !is.na(sibsp)) %>% select(age, sibsp)
print(people_2_to_152_with_sibsp)
sibsp_counts <- table(people_2_to_152_with_sibsp$sibsp)
most_common_sibsp <- names(sibsp_counts)[which.max(sibsp_counts)]
print(most_common_sibsp)
mydata <- mydata %>%
  mutate(sibsp = ifelse(is.na(sibsp), most_common_sibsp, sibsp))
View(mydata)


people_2_to_152_with_parch <- mydata %>%filter(age >= 2 & age <= 152, !is.na(parch)) %>% select(age, parch)
print(people_2_to_152_with_parch)
parch_counts <- table(people_2_to_152_with_parch$parch)
most_common_parch <- names(parch_counts)[which.max(parch_counts)]
print(most_common_parch)
mydata <- mydata %>%
  mutate(parch = ifelse(is.na(parch), most_common_parch, parch))
View(mydata)


mydata$fare <- ifelse(mydata$fare == "", NA, mydata$fare)
View(mydata)

mydata$fare <- as.numeric(as.character(mydata$fare))
print(mydata$fare)
mydata$fare <- round(mydata$fare)
View(mydata)

people_2_to_152_with_fare <- mydata %>%filter(age >= 2 & age <= 152, !is.na(fare)) %>% select(age, fare)
print(people_2_to_152_with_fare)
fare_counts <- table(people_2_to_152_with_fare$fare)
most_common_fare <- names(fare_counts)[which.max(fare_counts)]
print(most_common_fare)
mydata <- mydata %>%
  mutate(fare = ifelse(is.na(fare), most_common_fare, fare))
View(mydata)

mydata$fare <- as.numeric(as.character(mydata$fare))
print(mydata$fare)
mydata$fare <- round(mydata$fare)
View(mydata)

fare_column <- mydata$fare
normalized_fare <- (fare_column - min(fare_column)) / (max(fare_column) - min(fare_column))
mydata$fare <- normalized_fare
head(mydata)
print(mydata)


mydata$embarked <- ifelse(mydata$embarked == "", NA, mydata$embarked)
View(mydata)

people_2_to_152_with_embarked <- mydata %>%filter(age >= 2 & age <= 152, !is.na(embarked)) %>% select(age, embarked)
print(people_2_to_152_with_embarked)
embarked_counts <- table(people_2_to_152_with_embarked$embarked)
most_common_embarked <- names(embarked_counts)[which.max(embarked_counts)]
print(most_common_embarked)
mydata <- mydata %>%
  mutate(embarked = ifelse(is.na(embarked), most_common_embarked, embarked))
View(mydata)

mydata$embarked <- factor(mydata$embarked,
                          levels = c("S", "C", "Q"),
                          labels = c(1, 2, 3))
which(is.na(mydata$embarked))
View (mydata)


mydata$class <- ifelse(mydata$class == "", NA, mydata$class)
View(mydata)

people_2_to_152_with_class <- mydata %>%filter(age >= 2 & age <= 152, !is.na(class)) %>% select(age, class)
print(people_2_to_152_with_class)
class_counts <- table(people_2_to_152_with_class$class)
most_common_class <- names(class_counts)[which.max(class_counts)]
print(most_common_class)
mydata <- mydata %>%
  mutate(class = ifelse(is.na(class), most_common_class, class))
View(mydata)

mydata$class <- factor(mydata$class,
                       levels = c("First", "Second", "Third"),
                       labels = c(1, 2, 3))
which(is.na(mydata$class))
View (mydata)


unique_values <- unique(mydata$who)
print(unique_values)
mydata$who <- gsub("mannn", "man", mydata$who)

mydata$who <- ifelse(mydata$who == "", NA, mydata$who)
View(mydata)

people_2_to_152_with_who <- mydata %>%filter(age >= 2 & age <= 152, !is.na(who)) %>% select(age, who)
print(people_2_to_152_with_who)
who_counts <- table(people_2_to_152_with_who$who)
most_common_who <- names(who_counts)[which.max(who_counts)]
print(most_common_who)
mydata <- mydata %>%
  mutate(who = ifelse(is.na(who), most_common_who, who))
View(mydata)

mydata$who <- factor(mydata$who,
                     levels = c("man", "woman", "child"),
                     labels = c(1, 2, 3))
which(is.na(mydata$who))
View (mydata)

mydata$alone <- ifelse(mydata$alone == "", NA, mydata$alone)
View(mydata)

people_2_to_152_with_alone <- mydata %>%filter(age >= 2 & age <= 152, !is.na(alone)) %>% select(age, alone)
print(people_2_to_152_with_who)
alone_counts <- table(people_2_to_152_with_alone$alone)
most_common_alone <- names(alone_counts)[which.max(alone_counts)]
print(most_common_alone)
mydata <- mydata %>%
  mutate(alone = ifelse(is.na(alone), most_common_alone, alone))
View(mydata)

mydata$alone <- factor(mydata$alone,
                       levels = c("TRUE", "FALSE"),
                       labels = c(1, 0))
which(is.na(mydata$alone))
View (mydata)

anyNA(mydata$survived)


numerical_columns <- sapply(mydata, is.numeric)
for (col in names(mydata)[numerical_columns]) {
  hist(mydata[[col]], main = paste(col, "Distribution"), xlab = col, col = "red", 
       border = "black", breaks =10, col.axis = "darkblue", density = 20)
}

numerical_columns <- sapply(mydata, is.numeric)
for (col in names(mydata)[numerical_columns]) {
  boxplot(mydata[[col]], main = paste(col, "Box Plot"), ylab = col, col = "skyblue", 
          border = "red", notch = FALSE, horizontal = TRUE)
}


mydata$age_group <- cut(mydata$age, breaks = c(0, 20, 40, Inf),
                        labels = c("1-20", "21-40", "40+"), include.lowest = TRUE)

print(head(mydata))
print(mydata)


age_plot <- ggplot(mydata, aes(x = age_group, fill = age_group)) +
  geom_bar() +
  labs(title = "Distribution of Age Group",
       x = "Age Group",
       y = "Count") +
  scale_fill_manual(values = c("1-20" = "black", "21-40" = "yellow", "40+" = "red")) +
  scale_x_discrete(labels = c("1-20" = "1-20", "21-40" = "21-40", "40+" = "40+"))

print(age_plot)

Gender_plot <- ggplot(mydata, aes(x = factor(Gender), fill = factor(Gender))) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  scale_fill_manual(values = c("1" = "red2", "0" = "purple")) +
  scale_x_discrete(labels = c("1" = "Male", "0" = "Female"))
print(Gender_plot)

sibsp_plot <- ggplot(mydata, aes(x = factor(sibsp), fill = factor(sibsp))) +
  geom_bar() +
  labs(title = "Distribution of Siblings of the Passenger",
       x = "Siblings",
       y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red","2" = "purple","3" = "yellow","4" = "green")) +
  scale_x_discrete(labels = c("0" = "0", "1" = "1","2" = "2","3" = "3","4" = "4" ))
print(sibsp_plot)

parch_plot <- ggplot(mydata, aes(x = factor(parch), fill = factor(parch))) +
  geom_bar() +
  labs(title = "Distribution of Parents/Children abroad of the Titanic",
       x = "Parents/Children",
       y = "Count") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue","2" = "green","3" = "yellow","4" = "purple")) +
  scale_x_discrete(labels = c("0" = "0", "1" = "1","2" = "2","3" = "3","4" = "4" ))
print(parch_plot)

mydata$fare_group <- cut(mydata$fare, breaks = c(0, 0.25, 0.50, 0.75, Inf),
                         labels = c("0-0.25", "0.26-0.50", "0.51-0.75",  "0.76+"), include.lowest = TRUE)

print(head(mydata))
print(mydata)


fare_plot <- ggplot(mydata, aes(x = fare_group, fill = fare_group)) +
  geom_bar() +
  labs(title = "Distribution of Fare",
       x = "Fare Group",
       y = "Count") +
  scale_fill_manual(values = c("0-0.25" = "blue", "0.26-0.50" = "red", "0.51-0.75" = "purple","0.76+" = "black")) +
  scale_x_discrete(labels = c("0-0.25" = "Low", "0.26-0.50" = "Medium", "0.51-0.75" = "High","0.76+" = "Expensive"))

print(fare_plot)

embarked_plot <- ggplot(mydata, aes(x = factor(embarked), fill = factor(embarked))) +
  geom_bar() +
  labs(title = "Distribution of Embarked Station",
       x = "Embarked",
       y = "Count") +
  scale_fill_manual(values = c("1" = "black", "2" = "red","3" = "yellow")) +
  scale_x_discrete(labels = c("1" = "S", "2" = "C","3" = "Q"))
print(embarked_plot)

class_plot <- ggplot(mydata, aes(x = factor(class), fill = factor(class))) +
  geom_bar() +
  labs(title = "Distribution of Class of the Titanic",
       x = "Class Type",
       y = "Count") +
  scale_fill_manual(values = c("1" = "blue", "2" = "yellow2","3" = "black")) +
  scale_x_discrete(labels = c("1" = "1st Class", "2" = "2nd Class","3" = "3rd Class"))
print(class_plot) 

who_plot <- ggplot(mydata, aes(x = factor(who), fill = factor(who))) +
  geom_bar() +
  labs(title = "Distribution of Person Catagory in Titanic",
       x = "Class Type",
       y = "Count") +
  scale_fill_manual(values = c("1" = "black", "2" = "pink","3" = "red")) +
  scale_x_discrete(labels = c("1" = "Man", "2" = "Woman","3" = "Child"))
print(who_plot)

alone_plot <- ggplot(mydata, aes(x = factor(alone), fill = factor(alone))) +
  geom_bar() +
  labs(title = "Distribution of the Company of Person",
       x = "alone Type",
       y = "Count") +
  scale_fill_manual(values = c("1" = "yellow", "0" = "black")) +
  scale_x_discrete(labels = c("1" = "Alone", "0" = "Not Alone"))
print(alone_plot)

survived_plot <- ggplot(mydata, aes(x = factor(survived), fill = factor(survived))) +
  geom_bar() +
  labs(title = "Distribution of Survival of Titanic Passengers",
       x = "Survived Type",
       y = "Count") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red")) +
  scale_x_discrete(labels = c("1" = "Survived", "0" = "Died"))
print(survived_plot)

age_plot <- ggplot(mydata, aes(x = age_group, fill = factor(survived))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribution of Survival by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Survived") +
  scale_fill_manual(values = c("1" = "yellow", "0" = "black"), labels = c("Survived", "Not Survived"))

print(age_plot)

age_plot <- ggplot(mydata, aes(x = age_group, fill = factor(class))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribution of Class by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Class") +
  scale_fill_manual(values = c("1" = "red", "2" = "white", "3" = "blue"), labels = c("First Class", "Second Class", "Third Class"))

print(age_plot)

age_plot <- ggplot(mydata, aes(x = age_group, fill = factor(alone))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribution of Alone by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Alone") +
  scale_fill_manual(values = c("0" = "red", "1" = "black"), labels = c("Alone", "Not Alone"))

print(age_plot)

age_plot <- ggplot(mydata, aes(x = age_group, fill = factor(who))) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribution of the Person is with Age",
       x = "Age Group",
       y = "Count",
       fill = "Who") +
  scale_fill_manual(values = c("1" = "black", "2" = "blue", "3" = "red"), labels = c("Man", "Woman", "Child"))

print(age_plot)

mydata <- subset(mydata, select = -row_number)
rownames(mydata) <- NULL
View(mydata)

numerical_columns <- sapply(mydata, is.numeric)
standard_deviations <- numeric(length = sum(numerical_columns))
names(standard_deviations) <- names(mydata)[numerical_columns]

for (col in names(mydata)[numerical_columns]) {
  standard_deviations[col] <- sd(mydata[[col]], na.rm = TRUE)
}
print("Standard Deviations:")
print(standard_deviations)

mydata <- subset(mydata, select = -row_number)
rownames(mydata) <- NULL
View(mydata)

missing_indices <- list(
  Gender = integer(0),
  age = c(7, 22, 40, 61, 73, 78, 83, 89, 94, 98, 101, 103, 104, 105),
  sibsp = c(104,105),
  parch = c(104,105),
  fare = integer(0),
  embarked = integer(0),
  class = integer(0),
  who = integer(0),
  alone = c(104,105),
  survived = integer(0)
)
missing_matrix <- matrix(0, nrow = 105, ncol = length(missing_indices),
                         dimnames = list(1:105, names(missing_indices)))


for (col_name in names(missing_indices)) {
  missing_matrix[missing_indices[[col_name]], col_name] <- 1
}

heatmap(missing_matrix, Rowv = NA, Colv = NA, col = c("blue", "red"),
        xlab = "", ylab = "Row Numbers",
        main = "Missing Value Heatmap", 
        labRow = 1:105, 
        add.expr = { abline(h = seq(0.5, 105.5), col = "cyan4", lty = 2) })



library(dplyr)
library(ggplot2)
library(tidyr)

mydata <- read.csv("D:/8th Semester/INTRODUCTION TO DATA SCIENCE [B]/MID/IDS_Project/Midterm_Project.csv", header = TRUE, sep = ",")


mydata$Gender <- as.numeric(as.character(mydata$Gender))
print(mydata$Gender)
mydata$Gender <- round(mydata$Gender)
View(mydata)

mydata$sibsp <- as.numeric(as.character(mydata$sibsp))
print(mydata$sibsp)
mydata$sibsp <- round(mydata$sibsp)
View(mydata)

mydata$parch <- as.numeric(as.character(mydata$parch))
print(mydata$parch)
mydata$parch <- round(mydata$parch)
View(mydata)

mydata$embarked <- as.numeric(as.character(mydata$embarked))
print(mydata$embarked)
mydata$embarked <- round(mydata$embarked)
View(mydata)

mydata$class <- as.numeric(as.character(mydata$class))
print(mydata$class)
mydata$class <- round(mydata$class)
View(mydata)

mydata$who <- as.numeric(as.character(mydata$who))
print(mydata$who)
mydata$who <- round(mydata$who)
View(mydata)

mydata$alone <- as.numeric(as.character(mydata$alone))
print(mydata$alone)
mydata$alone <- round(mydata$alone)
View(mydata)

mydata$survived <- as.numeric(as.character(mydata$survived))
print(mydata$survived)
mydata$survived <- round(mydata$survived)
View(mydata)

mydata$row_number <- as.numeric(as.character(mydata$row_number))
print(mydata$row_number)
mydata$row_number <- round(mydata$row_number)
View(mydata)

mean_numeric <- function(x) {
  if (!is.numeric(x)) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}

means <- sapply(mydata, mean_numeric)
medians <- sapply(mydata, function(x) median(x, na.rm = TRUE))
medians <- as.numeric(medians)
summary_df <- data.frame(variable = names(means),
                         mean = means,
                         median = medians)
summary_df_long <- pivot_longer(summary_df, cols = c(mean, median),
                                names_to = "statistic", values_to = "value")

ggplot(summary_df_long, aes(x = variable, y = value, fill = statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(aes(group = statistic, color = statistic), linetype = "dotted", size = 1.2) +
  scale_fill_manual(values = c("mean" = "red", "median" = "blue")) +
  scale_color_manual(values = c("mean" = "skyblue4", "median" = "salmon2")) +
  labs(x = "Variables", y = "Value", fill = "Statistic", color = "Statistic",
       title = "Mean and Median Visualization") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

















mydata<- filter(mydata, Gender=="female")
mydata
num_females <- nrow(mydata)
num_females

mydata$age
mydata$Gender
is.na(mydata)
mydata = select(mydata,  Gender)
mydata

mydata <- read.csv("D:/8th Semester/INTRODUCTION TO DATA SCIENCE [B]/MID/IDS_Project/Midterm_Project.csv", header = TRUE, sep = ",")
arrange(mydata, -age)


filter(mydata, age>60)
is.na(mydata)

