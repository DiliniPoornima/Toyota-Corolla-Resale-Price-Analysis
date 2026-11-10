# Load necessary libraries
library(readr)
library(dplyr)
library(corrr)
library(car)
library(corrplot)

# Load the data set
data <- read.csv("new_data_set4.csv") 
numeric_data <- data %>% select(where(is.numeric))
numeric_data

#Regression Line and plot
# Plot Age vs Price with regression line
plot(df$Age, df$Price,
     main = "Age vs Price",
     xlab = "Age", ylab = "Price", col = "blue", pch = 16)
abline(lm(Price~Age, data = df), col = "red", lwd = 2)

#histogram

hist(df$Price, 
     main = "Histogram of Price", 
     xlab = "Price", 
     col = "skyblue", 
     border = "white")
#box plot
boxplot(df$Price, 
        main = "box of Price", 
        xlab = "Price", 
        col = "skyblue")



# Calculate correlation matrix with respect to Price
cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_with_target <- cor_matrix[, "Price"]
# Filter variables with high correlation (adjust threshold as needed)
high_cor_vars <- names(cor_with_target[abs(cor_with_target) > 0])
# Create a correlation matrix of only those high-correlation variables
final_cor_matrix <- cor_matrix[high_cor_vars,high_cor_vars ]
# Print the result
print(round(final_cor_matrix, 2))

corrplot(final_cor_matrix,
         method = "color", # Use colored squares
         type = "upper", # Show upper triangle only
         addCoef.col = "black", # Add coefficient text in black
         tl.col = "black", # Text label color
         number.cex = 0.6, # Size of the number labels
         tl.cex = 0.8, # Size of variable labels
         diag = FALSE) 
