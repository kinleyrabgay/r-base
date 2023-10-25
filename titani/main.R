
# Install necessary packages if not already installed
# install.packages('ggplot2')
# install.packages('dplyr')
library(ggplot2)
library(dplyr)

# Load the Titanic dataset
titanic_data <- read.csv("titani_dataset.csv") 

# Function for Question 2
question2 <- function() {
  # Plot histogram (Age)
  ggplot(titanic_data, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black") +
    labs(title = "Age Distribution of Passengers", x = "Age", y = "Count")
}

# Function for Question 3
question3 <- function() {
  # Stacked Histogram of Age by Sex
  ggplot(titanic_data, aes(x = age, fill = sex)) +
    geom_histogram(binwidth = 5, color = "black") +
    labs(title = "Age Distribution of Passengers by Sex (Stacked)",
         x = "Age", y = "Count") +
    scale_fill_manual(values = c("blue", "pink"))
}

# Function for Question 4
question4 <- function() {
  # Box and Whisker plot of the fare paid by the passengers.
  ggplot(titanic_data, aes(y = fare)) +
    geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
    labs(title = "Box and Whisker Plot of Fare Paid", y = "Fare")
}

# Function for Question 5
question5 <- function() {
  # Create a new dataframe without rows with missing values in the "age" column
  titanic_data_filtered <- titanic_data[complete.cases(titanic_data$age), ]

  # Box and Whisker plot with age of the passengers.
  ggplot(titanic_data_filtered, aes(y = age)) +
    geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
    labs(title = "Box and Whisker Plot of Age of Passengers (No NaN)", y = "Age")
}

# Function for Question 6
question6 <- function() {
  # (i) Based on Survival Status
  ggplot(titanic_data, aes(x = factor(survived), y = age)) +
    geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
    labs(title = "Age Distribution by Survival Status", x = "Survived", y = "Age") +
    scale_x_discrete(labels = c("Not Survived", "Survived"))

  # (ii) Based on Alone or Not Alone
  ggplot(titanic_data, aes(x = factor(alone), y = age)) +
    geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
    labs(title = "Age Distribution by Alone Status", x = "Alone", y = "Age") +
    scale_x_discrete(labels = c("Not Alone", "Alone"))

  # (iii) Based on Gender (Male or Female)
  ggplot(titanic_data, aes(x = sex, y = age)) +
    geom_boxplot(fill = "pink", color = "black", width = 0.5) +
    labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")
}

# Function for Question 7
question7 <- function() {
  # Create Side-by-Side Box and Whisker Plots:
  
  # (i) Based on Survival Status
  ggplot(titanic_data, aes(x = factor(survived), y = fare)) +
    geom_boxplot(fill = "lightblue", color = "black", width = 0.5) +
    labs(title = "Fare Distribution by Survival Status", x = "Survived", y = "Fare") +
    scale_x_discrete(labels = c("Not Survived", "Survived"))

  # (ii) Based on Alone or Not Alone:
  ggplot(titanic_data, aes(x = factor(alone), y = fare)) +
    geom_boxplot(fill = "lightgreen", color = "black", width = 0.5) +
    labs(title = "Fare Distribution by Alone Status", x = "Alone", y = "Fare") +
    scale_x_discrete(labels = c("Not Alone", "Alone"))

  # (iii) Based on Gender (Male or Female):
  ggplot(titanic_data, aes(x = sex, y = fare)) +
    geom_boxplot(fill = "pink", color = "black", width = 0.5) +
    labs(title = "Fare Distribution by Gender", x = "Gender", y = "Fare")
}

# Function for Question 8
question8 <- function() {
  # Line Chart of Age
  ggplot(titanic_data, aes(x = 1:nrow(titanic_data), y = age)) +
    geom_line(color = "blue") +
    labs(title = "Line Chart of Passenger Age", x = "Passenger ID", y = "Age")
}

# Function for Question 9
question9 <- function() {
  # Line Chart of Fare Paid
  ggplot(titanic_data, aes(x = 1:nrow(titanic_data), y = fare)) +
    geom_line(color = "green") +
    labs(title = "Line Chart of Fare Paid by Passengers", x = "Passenger ID", y = "Fare")
}

# Function for Question 10
question10 <- function() {
  # Count the number of passengers who survived (survived = 342)
  survived_passengers <- sum(titanic_data$survived == 1)
  cat("Number of passengers who survived: ", survived_passengers, "\n")
}

# Function for Question 11
question11 <- function() {
  # Count the number of passengers who embarked from Southampton (embarked = "S")
  southampton_passengers <- sum(titanic_data$embarked == "S")
  cat("Number of passengers who embarked from Southampton: ", southampton_passengers, "\n")
}

# Function for Question 12
question12 <- function() {
  # (i) Bar plot of passengers by age groups
  age_groups <- cut(titanic_data$age, breaks = c(0, 15, 30, 45, 60, max(titanic_data$age, na.rm = TRUE)))
  age_group_counts <- as.data.frame(table(age_groups))
  ggplot(age_group_counts, aes(x = age_groups, y = Freq)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = "Passengers by Age Groups", x = "Age Group", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Function for Question 13
question13 <- function() {
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
}

# Function for Question 14
question14 <- function() {
  # Create side-by-side histograms for Age by (a) Survival Status and (b) Alone Status
  ggplot(titanic_data, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
    facet_grid(. ~ survived + alone) +
    labs(title = "Age Distribution by Survival Status and Alone Status", x = "Age", y = "Count") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
}

# Function for Question 15
question15 <- function() {
  # Filter the data for passengers who embarked from Cherbourg
  cherbourg_passengers <- titanic_data[titanic_data$embarked == "C", ]

  # Create side-by-side histograms for passengers from Cherbourg by survival status
  ggplot(cherbourg_passengers, aes(x = survived, fill = factor(survived))) +
    geom_histogram(binwidth = 0.2, position = "dodge", color = "black") +
    labs(title = "Passengers from Cherbourg by Survival Status", x = "Survival Status", y = "Count") +
    scale_fill_manual(values = c("0" = "lightcoral", "1" = "lightblue"))
}

# Function for Question 16
question16 <- function() {
  # Create a Kernel Density Estimate (KDE) plot for the age of passengers
  ggplot(titanic_data, aes(x = age)) +
    geom_density(fill = "lightblue", color = "black") +
    labs(title = "Kernel Density Estimate of Passenger Age", x = "Age", y = "Density")
}

# Function for Question 17
question17 <- function() {
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
}

# Function for Question 18
question18 <- function() {
  # 18 (i) Cumulative Frequency Distribution Curve for Age
  # Create a data frame with age
  age_data <- titanic_data %>% select(age)

  # Remove rows with missing age values
  age_data <- age_data[!is.na(age_data$age), ]

  # Sort and calculate cumulative frequencies for age
  age_data <- age_data %>% arrange(age) %>%
    mutate(CumulativeFreq = cumsum(1))

  # Create a cumulative frequency distribution curve for age
  ggplot(age_data, aes(x = age, y = CumulativeFreq)) +
    geom_step(size = 1, direction = "right") +
    labs(title = "Cumulative Frequency Distribution Curve of Age", x = "Age", y = "Cumulative Frequency")
}

# Function for Question 19
question19 <- function() {
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
    geom_step(data = age_data, aes(x = age, y = CumulativeFreq), size = 1, color = "red")

  ogive_hist_plot
}

# Function for Question 20
question20 <- function() {
  # Create a scatter plot of age vs. fare
  ggplot(titanic_data, aes(x = age, y = fare)) +
    geom_point(color = "blue") +
    labs(title = "Scatter Plot of Age vs. Fare", x = "Age", y = "Fare")
}

# Function to call specific question function
call_question_function <- function(q_number) {
  switch(q_number,
         "2" = question2(),
         "3" = question3(),
         "4" = question4(),
         "5" = question5(),
         "6" = question6(),
         "7" = question7(),
         "8" = question8(),
         "9" = question9(),
         "10" = question10(),
         "11" = question11(),
         "12" = question12(),
         "13" = question13(),
         "14" = question14(),
         "15" = question15(),
         "16" = question16(),
         "17" = question17(),
         "18" = question18(),
         "19" = question19(),
         "20" = question20(),
         "default" = cat("Invalid question number.\n")
  )
}

# Main function
main <- function() {
  cat("Data Visualization for Titanic Dataset\n")
  
  while (TRUE) {
    cat("\nSelect a question number (2-20) to visualize or '0' to exit: ")
    q_number <- as.numeric(readline(prompt = ""))
    
    if (q_number == 0) {
      cat("Goodbye!")
      break
    } else if (q_number >= 2 && q_number <= 20) {
      call_question_function(as.character(q_number))
    } else {
      cat("Invalid question number. Please select a number between 2 and 20.\n")
    }
  }
}

# Run the main function
main()
