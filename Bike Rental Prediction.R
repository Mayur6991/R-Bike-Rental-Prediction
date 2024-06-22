#Bike Rental Prediction .

setwd(choose.dir())
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)

Bike_rental_data <- read_excel("day.xlsx")

View(Bike_rental_data)

summary(Bike_rental_data)

str(Bike_rental_data)

Bike_rental_data1 <- Bike_rental_data %>%
  mutate(instant=as.integer(instant),
         dteday=as.Date(dteday),
         season = as.factor(season),
         yr=as.factor(yr),
         mnth=as.factor(mnth),
         holiday=as.factor(holiday),
         weekday=as.factor(weekday),
         workingday=as.factor(workingday),
         weathersit=as.factor(weathersit)
         )

str(Bike_rental_data1)


missing_values <- Bike_rental_data1 %>%
  summarise_all(~sum(is.na(.)))

print(missing_values)

monthly_rentals <- Bike_rental_data1 %>%
  group_by(mnth) %>%
  summarise(total_rentals=sum(cnt))


ggplot(monthly_rentals, aes(x = mnth, y = total_rentals)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(
    aes(label = scales::number_format(scale = 1e-3, accuracy = 1, suffix = "k")(total_rentals)),
    vjust = -0.5,
    size = 3,
    color = "black"
  ) + # Add labels in thousands (k, without decimals) on top of bars
  labs(
    title = "Monthly Distribution of Bike Rentals",
    x = "Month",
    y = "Total Rentals (in k)"
  ) +
  scale_x_discrete(labels = c(
    "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr",
    "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug",
    "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"
  )) +
  scale_y_continuous(
    labels = scales::number_format(scale = 1e-3, accuracy = 1, suffix = "k")
  ) + # Format Y-axis labels in thousands (k, without decimals) with "k" suffix
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5) # Adjust size and center title
  )


Bike_rental_data1 <- Bike_rental_data1 %>%
  mutate(yr = as.numeric(yr))
yearly_rentals <- Bike_rental_data1 %>%
  group_by(yr) %>%
  summarise(total_rentals = sum(cnt))
  
  


ggplot(yearly_rentals, aes(x = yr, y = total_rentals)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = scales::number_format(scale = 1e-3, accuracy = 1, suffix = "k")(total_rentals)),
            vjust = -0.5, size = 3, color = "black") + # Add labels on top of bars
  labs(
    title = "Yearly Distribution of Bike Rentals",
    x = "Year",
    y = "Total Rentals (in k)"
  ) +
  scale_x_continuous(
    labels = c("Year 1", "Year 2"), # Specify custom labels
    breaks = 1:2 # Specify the breaks for the custom labels
  ) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, accuracy = 1, suffix = "k")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5), # Adjust title size and center it
    axis.text.x = element_text(size = 12), # Adjust X-axis label font size
    axis.text.y = element_text(size = 12) # Adjust Y-axis label font size
  )


numeric_variables <- c("temp", "atemp", "hum", "windspeed", "casual", "registered", "cnt")
boxplots <- list()

for (var in numeric_variables) {
  p <- ggplot(Bike_rental_data1, aes(x = "", y = !!sym(var))) +
    geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red") +
    labs(
      title = paste("Outliers Analysis of", var),
      x = "",
      y = var
    ) + 
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, accuracy = 1, suffix = "k")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5), # Adjust title size and center it
      axis.text.x = element_blank(), # Remove X-axis labels
      axis.ticks.x = element_blank(), # Remove X-axis ticks
      axis.text.y = element_text(size = 12) # Adjust Y-axis label font size
    )
  boxplots[[var]] <- p
}
boxplots # Print the boxplots

set.seed(123)

trainIndex <- createDataPartition(Bike_rental_data1$cnt, p = 0.7, list = FALSE)
training_data <- Bike_rental_data1[trainIndex, ]

training_data

test_data <- Bike_rental_data1[-trainIndex, ]

test_data

model <- randomForest(cnt ~ season + yr + mnth + holiday + weekday + workingday 
                      + weathersit + temp + atemp + hum + windspeed + casual 
                      + registered,
                      data = training_data)


predictions <- predict(model, newdata = test_data)

model


test_predictions <- predict(model, newdata = test_data)

rmse <- sqrt(mean((test_data$cnt - test_predictions)^2))
rmse
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


r_squared <- 1 - (sum((test_data$cnt - test_predictions)^2) / sum((test_data$cnt - mean(test_data$cnt))^2))
cat("R-squared (R2):", r_squared, "\n")

plot(test_data$cnt, test_predictions, xlab = "Actual", ylab = "Predicted", main = "Actual vs. Predicted Values")
abline(0, 1, col = "red") # Add a diagonal line for reference