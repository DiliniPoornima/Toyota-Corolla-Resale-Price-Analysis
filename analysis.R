library(car)
library(dplyr)
library(knitr)
library(olsrr)
data<-read.csv(file="new_data_set4.csv")
df<-data.frame(data)
head(df)

df$Color <-as.factor(df$Color )
df$CC<-as.factor(df$CC )
model <-lm(Price~Age+KM+Color+CC+Weight, data = df)
summary(model)

numeric_data <-df %>%select_if(is.numeric)
correlation_matrix <-round(cor(numeric_data),4)

#cor matrix table
kable(correlation_matrix,
         caption = "Correlation Table of Numeric Variables")
summary_data <-summary(df)
summary_data
#residual plot
plot(model, which = 1)

# Q-Q plot for normality
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

dependent_variable <-df$Price
hist(dependent_variable,
         main = "Histogram of Price",
         xlab = "Price",
         col = "skyblue",
         border = "white")

#Homoscedasticity
# Get the standardized residuals from the model
standardized_residuals <-rstandard(model)
# Get the fitted (predicted) values from the model
fitted_values <-fitted(model)

 # Plot the standardized residuals against the fitted values
plot(fitted_values, standardized_residuals,
         main = "Standardized Residuals vs Fitted Values",
         xlab = "Fitted (Predicted) Values",
        ylab = "Standardized Residuals")

 # Add a horizontal line at y=0 for reference
abline(h = 0, col = "red", lty = 2)

 #full model
model <-lm(Price~Age+KM+Color+CC+Weight, data = df)
anova(model)
#forward_method

ols_step_forward_p(model,pent=0.05,details=T)

forward_ml<-ols_step_forward_p(model,pent=0.05,details=F)
forward_ml

#reduced model
model_2<-lm(Price~Age+KM+Color+Weight,data=df)
model_2
summary(model_2)
#VIF Values of the model
vif(model)

# Define the parameters
alpha <-0.05 # The significance level (alpha)
v1 <-3 # Numerator degrees of freedom
v2 <-235 # Denominator degrees of freedom

# Calculate the critical F-value using the qf() function
critical_F_value <-qf(1- alpha, v1, v2)

# Print the result
print(critical_F_value)