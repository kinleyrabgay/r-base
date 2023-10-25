# install.packages('ggplot2')
# install.packages('dylyr')
library(ggplot2)
library(dplyr)

# Load the Titanic dataset
titanic_data <- read.csv("titani_dataset.csv")  # Replace with the actual file name
# 1 summary(titanic_data)

# 2 Plot histogram (Age)
ggplot(titanic_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution of Passengers", x = "Age", y = "Count")

# 2 Plot histogram (Fare paid)
ggplot(titanic_data, aes(x = fare)) +
  geom_histogram(binwidth = 20, fill = "green", color = "black") +
  labs(title = "Fare Paid Distribution", x = "Fare Paid", y = "Count")

# 3 Stacked Histogram of Age by Sex
ggplot(titanic_data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Age Distribution of Passengers by Sex (Stacked)",
       x = "Age", y = "Count") +
  scale_fill_manual(values = c("blue", "pink"))

# 3 Side-by-Side Histogram of Age by Sex:
ggplot(titanic_data, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, color = "black", position = "dodge") +
  labs(title = "Age Distribution of Passengers by Sex (Side-by-Side)",
       x = "Age", y = "Count") +
  scale_fill_manual(values = c("blue", "pink"))

# 4 Box and Whisker plot of the fare paid by thepassengers.
ggplot(titanic_data, aes(y = fare)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
  labs(title = "Box and Whisker Plot of Fare Paid", y = "Fare")


# Create a new dataframe without rows with missing values in the "age" column
titanic_data_filtered <- titanic_data[complete.cases(titanic_data$age), ]

# 5 Box and Whisker plot with age of the passengers.
ggplot(titanic_data_filtered, aes(y = age)) +
  geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
  labs(title = "Box and Whisker Plot of Age of Passengers (No NaN)", y = "Age")

# 6 Side-by-Side Box and Whisker Plots
# 6 (i) Based on Survival Status
ggplot(titanic_data, aes(x = factor(survived), y = age)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
  labs(title = "Age Distribution by Survival Status", x = "Survived", y = "Age") +
  scale_x_discrete(labels = c("Not Survived", "Survived"))

# 6 (ii)  Based on Alone or Not Alone
ggplot(titanic_data, aes(x = factor(alone), y = age)) +
  geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
  labs(title = "Age Distribution by Alone Status", x = "Alone", y = "Age") +
  scale_x_discrete(labels = c("Not Alone", "Alone"))

# 6 (iii) Based on Gender (Male or Female)
ggplot(titanic_data, aes(x = sex, y = age)) +
  geom_boxplot(fill = "pink", color = "black", width = 0.5) +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")


# 7 Create Side-by-Side Box and Whisker Plots:

# 7 (i) Based on Survival Status
ggplot(titanic_data, aes(x = factor(survived), y = fare)) +
  geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
  labs(title = "Fare Distribution by Survival Status", x = "Survived", y = "Fare") +
  scale_x_discrete(labels = c("Not Survived", "Survived"))

# 7 (ii) Based on Alone or Not Alone:
ggplot(titanic_data, aes(x = factor(alone), y = fare)) +
  geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
  labs(title = "Fare Distribution by Alone Status", x = "Alone", y = "Fare") +
  scale_x_discrete(labels = c("Not Alone", "Alone"))

# 7 (iii) Based on Gender (Male or Female):
ggplot(titanic_data, aes(x = sex, y = fare)) +
  geom_boxplot(fill = "pink", color = "black", width = 0.5) +
  labs(title = "Fare Distribution by Gender", x = "Gender", y = "Fare")


# 8 Create Line Charts:
# 8 (i) Line Chart of Age:
ggplot(titanic_data, aes(x = 1:nrow(titanic_data), y = age)) +
  geom_line(color = "blue") +
  labs(title = "Line Chart of Passenger Age", x = "Passenger ID", y = "Age")

# 8 (ii) Line Chart of Fare Paid:
ggplot(titanic_data, aes(x = 1:nrow(titanic_data), y = fare)) +
  geom_line(color = "green") +
  labs(title = "Line Chart of Fare Paid by Passengers", x = "Passenger ID", y = "Fare Paid")

ggplot(titanic_data, aes(x = age, y = fare)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare Paid",
       x = "Age", y = "Fare Paid")

# 9 Create a scatter plot presenting the relationship between the age and the fare paid by the passengers
ggplot(titanic_data, aes(x = age, y = fare)) +
  geom_point(color = "purple", size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare Paid",
       x = "Age", y = "Fare Paid")

# 10 Count the number of passengers who survived (survived = 342)
survived_passengers <- sum(titanic_data$survived == 1)
cat("Number of passengers who survived: ", survived_passengers, "\n")


# 11 Count the number of passengers who embarked from Southampton (embarked = "S")
southampton_passengers <- sum(titanic_data$embarked == "S")
cat("Number of passengers who embarked from Southampton: ", southampton_passengers, "\n")


# 12
# Create age groups
age_groups <- cut(titanic_data$age, breaks = c(0, 15, 30, 45, 60, max(titanic_data$age, na.rm = TRUE)))

# Count passengers in each age group
# Create age groups
age_groups <- cut(titanic_data$age, breaks = c(0, 15, 30, 45, 60, max(titanic_data$age, na.rm = TRUE)))

# Count passengers in each age group
age_group_counts <- as.data.frame(table(age_groups))

# Create the bar plot
ggplot(age_group_counts, aes(x = age_groups, y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Passengers by Age Groups", x = "Age Group", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########## Question 12 #########

# (i) Bar plot of passengers by age groups
age_groups <- cut(titanic_data$age, breaks = c(0, 15, 30, 45, 60, Inf))
age_group_counts <- as.data.frame(table(age_groups))

# (ii) Bar plot of passengers by class
class_counts <- table(titanic_data$pclass)

# (iii) Bar plot of passengers by embarkation point
embark_counts <- table(titanic_data$embarked)

# Set up a multi-plot layout
par(mfrow = c(2, 2))

# Create the first bar plot (age groups)
barplot(age_group_counts$Freq, names.arg = age_group_counts$age_groups, main = "Passengers by Age Groups", col = "lightblue", xlab = "Age Group", ylab = "Count")

# Create the second bar plot (class)
barplot(class_counts, names.arg = c("1st Class", "2nd Class", "3rd Class"), main = "Passengers by Class", col = "lightgreen", xlab = "Class", ylab = "Count")

# Create the third bar plot (embarkation point)
barplot(embark_counts, names.arg = names(embark_counts), main = "Passengers by Embarkation Point", col = "lightpink", xlab = "Embarkation Point", ylab = "Count")





######### Question 13 ####3
# Filter the data into survivors and non-survivors
survivors <- titanic_data[titanic_data$survived == 1, ]
non_survivors <- titanic_data[titanic_data$survived == 0, ]

# Create the stacked histogram
ggplot() +
  geom_histogram(data = survivors, aes(x = age, fill = "Survived"), color = "black", bins = 20, alpha = 0.6) +
  geom_histogram(data = non_survivors, aes(x = age, fill = "Not Survived"), color = "black", bins = 20, alpha = 0.6) +
  scale_fill_manual(values = c("Survived" = "lightblue", "Not Survived" = "lightcoral")) +
  labs(title = "Stacked Histogram of Passenger Age by Survival Status", x = "Age", y = "Count") +
  theme_minimal()


# Q 14 #
# Create side-by-side histograms for Age by (a) Survival Status and (b) Alone Status
ggplot(titanic_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  facet_grid(. ~ survived + alone) +
  labs(title = "Age Distribution by Survival Status and Alone Status", x = "Age", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Create side-by-side histograms for Fare by (a) Survival Status, (b) Sex, and (c) Alone Status
ggplot(titanic_data, aes(x = fare)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  facet_grid(. ~ survived + sex + alone) +
  labs(title = "Fare Distribution by Survival Status, Sex, and Alone Status", x = "Fare", y = "Count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), axis.ticks.x = element_blank()) 

# Q 15
# Filter the data for passengers who embarked from Cherbourg
cherbourg_passengers <- titanic_data[titanic_data$embarked == "C", ]

# Create side-by-side histograms for passengers from Cherbourg by survival status
ggplot(cherbourg_passengers, aes(x = survived, fill = factor(survived))) +
  geom_histogram(binwidth = 0.2, position = "dodge", color = "black") +
  labs(title = "Passengers from Cherbourg by Survival Status", x = "Survival Status", y = "Count") +
  scale_fill_manual(values = c("0" = "lightcoral", "1" = "lightblue"))


# Q 16
# Create a Kernel Density Estimate (KDE) plot for the age of passengers
ggplot(titanic_data, aes(x = age)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Kernel Density Estimate of Passenger Age", x = "Age", y = "Density")


# Q 17

# 17 (i) Create a data frame with age and survival status
age_survival <- titanic_data %>% select(age, survived)

# Remove rows with missing age values
age_survival <- age_survival[!is.na(age_survival$age), ]

# Sort and calculate cumulative frequencies for age by survival status
age_survival <- age_survival %>% arrange(age) %>%
  group_by(age, survived) %>%
  summarise(CumulativeFreq = n())

# Create an ogive plot for age by survival status
ggplot(age_survival, aes(x = age, y = CumulativeFreq, color = factor(survived))) +
  geom_line(size = 0.5) + 
  labs(title = "Ogive Curve of Passenger Age by Survival Status", x = "Age", y = "Cumulative Frequency") +
  scale_color_manual(values = c("0" = "lightcoral", "1" = "lightblue"))



# 17 (ii) Create a data frame with age and class
age_class <- titanic_data %>% select(age, pclass)

# Remove rows with missing age values
age_class <- age_class[!is.na(age_class$age), ]

# Sort and calculate cumulative frequencies for age by class
age_class <- age_class %>% arrange(age) %>%
  group_by(age, pclass) %>%
  summarise(CumulativeFreq = n())

# Create an ogive plot for age by class
ggplot(age_class, aes(x = age, y = CumulativeFreq, color = factor(pclass))) +
  geom_line(size = 0.5) +
  labs(title = "Ogive Curve of Passenger Age by Class", x = "Age", y = "Cumulative Frequency") +
  scale_color_manual(values = c("1" = "lightblue", "2" = "lightgreen", "3" = "lightcoral"))


# 17 (iii) Create a data frame with age and alone status
age_alone <- titanic_data %>% select(age, alone)

# Remove rows with missing age values
age_alone <- age_alone[!is.na(age_alone$age), ]

# Convert "alone" column to a factor for plotting
age_alone$alone <- factor(age_alone$alone, levels = c("True", "False"))

# Sort and calculate cumulative frequencies for age by alone status
age_alone <- age_alone %>% arrange(age) %>%
  group_by(age, alone) %>%
  summarise(CumulativeFreq = n())

# Create an ogive plot for age by alone status
ggplot(age_alone, aes(x = age, y = CumulativeFreq, color = alone)) +
  geom_line(size = 0.5) + 
  labs(title = "Ogive Curve of Passenger Age by Alone Status", x = "Age", y = "Cumulative Frequency") +
  scale_color_manual(values = c("True" = "lightblue", "False" = "lightgreen"))



# Q 18
# 18 (i) Cumulative Frequency Distribution Curve for Age:
# Create a data frame with age
age_data <- titanic_data %>% select(age)

# Remove rows with missing age values
age_data <- age_data[!is.na(age_data$age), ]

# Sort and calculate cumulative frequencies for age
age_data <- age_data %>% arrange(age) %>%
  mutate(CumulativeFreq = cumsum(1))

# Create a cumulative frequency distribution curve for age
ggplot(age_data, aes(x = age, y = CumulativeFreq)) +
  geom_step(size = 1, direction = "right") + # Step function for cumulative distribution
  labs(title = "Cumulative Frequency Distribution Curve of Age", x = "Age", y = "Cumulative Frequency")

# 18 (ii)
# Create a data frame with fare
fare_data <- titanic_data %>% select(fare)

# Remove rows with missing fare values
fare_data <- fare_data[!is.na(fare_data$fare), ]

# Sort and calculate cumulative frequencies for fare
fare_data <- fare_data %>% arrange(fare) %>%
  mutate(CumulativeFreq = cumsum(1))

# Create a cumulative frequency distribution curve for fare
ggplot(fare_data, aes(x = fare, y = CumulativeFreq)) +
  geom_step(size = 1, direction = "right") + # Step function for cumulative distribution
  labs(title = "Cumulative Frequency Distribution Curve of Fare", x = "Fare", y = "Cumulative Frequency")


# Q 19 
# Plot the Ogive graph on top of the histogram on the age of the passengers
# Create a data frame with age
age_data <- titanic_data %>% select(age)

# Remove rows with missing age values
age_data <- age_data[!is.na(age_data$age), ]

# Create a histogram of age
hist_plot <- ggplot(age_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Passenger Age", x = "Age", y = "Count")

# Calculate cumulative frequencies for age
age_data <- age_data %>% 
  mutate(CumulativeFreq = 1:n())

# Create an Ogive plot on top of the histogram
ogive_hist_plot <- hist_plot +
  geom_line(data = age_data, aes(x = age, y = CumulativeFreq), size = 1, color = "red")

# Print the combined histogram and Ogive plot
print(ogive_hist_plot)


# Q 20
# Create a Box and Whisker plot for Age by Class
boxplot_plot <- ggplot(titanic_data, aes(x = factor(pclass), y = age)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box and Whisker Plot of Passenger Age by Class", x = "Class", y = "Age")

# Print the Box and Whisker plot
print(boxplot_plot)



# Q 21
# Create a scatter plot for the relationship between Age and Fare
scatter_plot <- ggplot(titanic_data, aes(x = age, y = fare)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare", x = "Age", y = "Fare")


# Q 22
# (i) Scatter plot by gender
scatter_gender <- ggplot(titanic_data, aes(x = age, y = fare, color = sex)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare by Gender", x = "Age", y = "Fare")

# (ii) Scatter plot by survival
scatter_survival <- ggplot(titanic_data, aes(x = age, y = fare, color = factor(survived))) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare by Survival", x = "Age", y = "Fare")

# (iii) Scatter plot by whether passengers were alone or not
scatter_alone <- ggplot(titanic_data, aes(x = age, y = fare, color = alone)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare by Alone Status", x = "Age", y = "Fare")

# (iv) Scatter plot by embarking place
scatter_embark <- ggplot(titanic_data, aes(x = age, y = fare, color = embarked)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Age vs. Fare by Embarking Place", x = "Age", y = "Fare")


