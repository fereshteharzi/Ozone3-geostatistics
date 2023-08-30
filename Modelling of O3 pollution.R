# Load required libraries
install.packages("gstat")
library(gstat)
library(sp)


# Step 1: Read Data from CSV
input_csv_file <- "C:\\Users\\navid.tavakoli\\Desktop\\data.csv"

# Read the CSV file into a data frame
df <- read.csv(input_csv_file, header = TRUE)

# Step 2: Calculate the distances between points using the Euclidean distance formula

distances <- list()
for (i in 1:(nrow(df) - 1)) {
  for (j in 1:(nrow(df) - 1)) {
    d <- sqrt((df$X[i] - df$X[j])^2 + (df$Y[i] - df$Y[j])^2)
    if (d > 0) { 
      distances <- c(distances, d)
      print(paste("Length of distances list:", length(distances)))
    }
    
  }
}






#Step 1: Read Data from CSV
my_data <- read.csv("C:\\Users\\navid.tavakoli\\Desktop\\flat_distances.csv")
# Flatten the list of distances into a numeric vector
flat_distances <- unlist(my_data)


# Calculate the minimum, maximum, average, and variance of flat_distances
min_distance <- round(min(flat_distances, na.rm = TRUE), digit = 0)
max_distance <- round(max(flat_distances, na.rm = TRUE), digit = 0)
average_distance <- mean(flat_distances, na.rm = TRUE)
variance_distance <- round(var(flat_distances, na.rm = TRUE), digit = 0)
# Calculate the standard deviation (square root of the variance)
standard_deviation <- round(sqrt(variance_distance), digit = 0)

# Step 2: Calculate and print the maximum and minimum values of X and Y
max_X <- max(df$X)
min_X <- min(df$X)
max_Y <- max(df$Y)
min_Y <- min(df$Y)
# Step 3: Calculate and print the dimensions of X and Y (range)
dimension_X <- max_X - min_X
dimension_Y <- max_Y - min_Y
# Step 4: Calculate and print half of the field
half_of_field <- round(max(dimension_X, dimension_Y) / 2, digit = 0)
# Step 2: Calculate and print the maximum and minimum values of X and Y
max_X <- max(df$X)
min_X <- min(df$X)
max_Y <- max(df$Y)
min_Y <- min(df$Y)



# Step 2: Calculate and print the variance of the "Density" column
var_density <- round(var(df$Density), digit = 0)

# Step 6: Calculate lag separation based on the minimum distance
lag_separation <- 6


# Step 11: Calculate number of lags
number_of_lags <- round(half_of_field / lag_separation, digits = 0)

# Step 12: Print the number of lags
print("Number of Lags:")
print(number_of_lags)

# Step 11: Calculate lag tolerance
lag_tolerance <- (lag_separation / 2 )

# Print the results
print("Minimum Distance:")
print(min_distance)

print("Maximum Distance:")
print(max_distance)

print("Average Distance:")
print(average_distance)

print("Variance of Distance:")
print(variance_distance)

print("Standard Deviation of Distance:")
print(standard_deviation)


print("Maximum X:")
print(max_X)
print("Minimum X:")
print(min_X)
print("Maximum Y:")
print(max_Y)
print("Minimum Y:")
print(min_Y)


print("Dimension X:")
print(dimension_X)
print("Dimension Y:")
print(dimension_Y)


print("Half of the Field:")
print(half_of_field)


print("Variance of Density:")
print(var_density)

# Step 7: Print the lag separation
print("Lag Separation:")
print(lag_separation)


print("Number of Lags:")
print(number_of_lags)

print("lag tolerance:")
print(lag_tolerance)






















#variogram model





# Step 2: Check for missing or invalid values in the "Density" column
valid_density <- !is.na(df$Density) & is.numeric(df$Density) & df$Density >= 0
df <- df[valid_density, ]

# ... (Rest of the distance calculation, calculations, and variogram code)

# Step 10: Create a SpatialPointsDataFrame object
sp_points <- SpatialPointsDataFrame(coords = df[, c("X", "Y")], data = df)

# Step 12: Set the maximum distance for the variogram calculation using cutoff
cutoff_distance <- 1200

# Step 15: Add the first variogram with azimuth=0, dip=0, tolerance=90, and bandwidth=1000
v_azimuth0 <- variogram(Density ~ 1, sp_points, alpha = c(0, 0), 
                        cutoff = cutoff_distance, width = lag_separation)

# Step 16: Add the second variogram with azimuth=90, dip=0, tolerance=22.5, and bandwidth=1000
v_azimuth90 <- variogram(Density ~ 1, sp_points, alpha = c(90), 
                         cutoff = cutoff_distance, width = lag_separation)

# Save the plots in PNG format with increased width
png("variogram_azimuth0.png", width = 1000, height = 600, res = 150)
plot(v_azimuth0, col = "red", lty = 2, lwd = 2, main = "Variogram: Azimuth 0")
dev.off()

png("variogram_azimuth90.png", width = 1000, height = 600, res = 150)
plot(v_azimuth90, col = "green", lty = 3, lwd = 2, main = "Variogram: Azimuth 90")
dev.off()



# Step 15: Fit the variogram model - Spherical
model_sph1 <- fit.variogram(v_azimuth0, model = vgm("Sph", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Exponential
model_exp1 <- fit.variogram(v_azimuth0, model = vgm("Exp", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Gaussian
model_gauss1 <- fit.variogram(v_azimuth0, model = vgm("Gau", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Linear
model_lin1 <- fit.variogram(v_azimuth0, model = vgm("Lin", psill = 70, range = 100, nugget = 48))

# Step 16: Print the fitted model parameters for all the models
print("Fitted Variogram Model Parameters - Spherical:")
print(model_sph1)

print("Fitted Variogram Model Parameters - Exponential:")
print(model_exp1)

print("Fitted Variogram Model Parameters - Gaussian:")
print(model_gauss1)

print("Fitted Variogram Model Parameters - Linear:")
print(model_lin1)


# Step 17: Open a new plotting device
plot.new()

# Step 18: Plot the experimental and fitted variograms together for each model

# Spherical variogram
plot(v_azimuth0, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Spherical",
     xlab = "Distance", ylab = "Semivariance",
     model = model_sph1, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Spherical:")
print(model_sph1)

# Exponential variogram
plot(v_azimuth0, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Exponential",
     xlab = "Distance", ylab = "Semivariance",
     model = model_exp1, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Exponential:")
print(model_exp1)

# Gaussian variogram
plot(v_azimuth0, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Gaussian",
     xlab = "Distance", ylab = "Semivariance",
     model = model_gauss1, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Gaussian:")
print(model_gauss1)

# Linear variogram
plot(v_azimuth0, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Linear",
     xlab = "Distance", ylab = "Semivariance",
     model = model_lin1, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Linear:")
print(model_lin1)










# Step 15: Fit the variogram model - Spherical
model_sph2 <- fit.variogram(v_azimuth90, model = vgm("Sph", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Exponential
model_exp2 <- fit.variogram(v_azimuth90, model = vgm("Exp", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Gaussian
model_gauss2 <- fit.variogram(v_azimuth90, model = vgm("Gau", psill = 70, range = 100, nugget = 48))

# Step 15: Fit the variogram model - Linear
model_lin2 <- fit.variogram(v_azimuth90, model = vgm("Lin", psill = 70, range = 100, nugget = 48))

# Step 16: Print the fitted model parameters for all the models
print("Fitted Variogram Model Parameters - Spherical:")
print(model_sph2)

print("Fitted Variogram Model Parameters - Exponential:")
print(model_exp2)

print("Fitted Variogram Model Parameters - Gaussian:")
print(model_gauss2)

print("Fitted Variogram Model Parameters - Linear:")
print(model_lin2)


# Step 17: Open a new plotting device
plot.new()

# Step 18: Plot the experimental and fitted variograms together for each model

# Spherical variogram
plot(v_azimuth90, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Spherical",
     xlab = "Distance", ylab = "Semivariance",
     model = model_sph2, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Spherical:")
print(model_sph2)

# Exponential variogram
plot(v_azimuth90, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Exponential",
     xlab = "Distance", ylab = "Semivariance",
     model = model_exp2, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Exponential:")
print(model_exp2)

# Gaussian variogram
plot(v_azimuth90, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Gaussian",
     xlab = "Distance", ylab = "Semivariance",
     model = model_gauss2, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Gaussian:")
print(model_gauss2)

# Linear variogram
plot(v_azimuth90, pch = 16, cex = 0.5, col = "blue",
     main = "Experimental vs. Fitted Variogram - Linear",
     xlab = "Distance", ylab = "Semivariance",
     model = model_lin2, lwd = 2, col.fitted = "red")
legend("topright", legend = c("Experimental", "Fitted"),
       col = c("blue", "red"), lty = 1, lwd = 2)
print("Fitted Variogram Model Parameters - Linear:")
print(model_lin2)




# Perform cross-validation using krige.cv
kriging_cv_model_gauss1 <- krige.cv(Density ~ 1, sp_points, model = model_gauss1)
kriging_cv_model_exp1 <- krige.cv(Density ~ 1, sp_points, model = model_exp1)
kriging_cv_model_sph1 <- krige.cv(Density ~ 1, sp_points, model = model_sph1)
kriging_cv_model_lin1 <- krige.cv(Density ~ 1, sp_points, model = model_lin1)

# Perform cross-validation using krige.cv
kriging_cv_model_gauss2 <- krige.cv(Density ~ 1, sp_points, model = model_gauss2)
kriging_cv_model_exp2 <- krige.cv(Density ~ 1, sp_points, model = model_exp2)
kriging_cv_model_sph2 <- krige.cv(Density ~ 1, sp_points, model = model_sph2)
kriging_cv_model_lin2 <- krige.cv(Density ~ 1, sp_points, model = model_lin2)



# Save kriging_cv to a CSV file named "kriging_cv_results.csv"
write.csv(kriging_cv_model_gauss1, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_gauss1.csv", row.names = FALSE)
write.csv(kriging_cv_model_exp1, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_exp1.csv", row.names = FALSE)
write.csv(kriging_cv_model_sph1, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_sph1.csv", row.names = FALSE)
write.csv(kriging_cv_model_lin1, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_lin1.csv", row.names = FALSE)




# Save kriging_cv to a CSV file named "kriging_cv_results.csv"
write.csv(kriging_cv_model_gauss2, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_gauss2.csv", row.names = FALSE)
write.csv(kriging_cv_model_exp2, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_exp2.csv", row.names = FALSE)
write.csv(kriging_cv_model_sph2, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_sph2.csv", row.names = FALSE)
write.csv(kriging_cv_model_lin2, file = "C:\\Users\\navid.tavakoli\\Desktop\\kriging_cv_model_lin2.csv", row.names = FALSE)




# Function to calculate Mean Squared Error (MSE)
calculate_mse <- function(observed, predicted) {
  mean((observed - predicted)^2, na.rm = TRUE)
}

# Function to calculate Root Mean Squared Error (RMSE)
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}

# Function to calculate Mean Absolute Error (MAE)
calculate_mae <- function(observed, predicted) {
  mean(abs(observed - predicted), na.rm = TRUE)
}

# Function to calculate Mean of Normalized Error
calculate_mean_normalized_error <- function(residuals, observed) {
  normalized_error <- residuals / observed
  mean(normalized_error, na.rm = TRUE)
}

# Function to calculate Variance of Normalized Error
calculate_variance_normalized_error <- function(residuals, observed) {
  normalized_error <- residuals / observed
  var(normalized_error, na.rm = TRUE)
}

# Calculate and print MSE, RMSE, MAE, Mean of Normalized Error, and Variance of Normalized Error for each kriging model

# kriging_cv_model_exp1
mse_exp1 <- calculate_mse(kriging_cv_model_exp1$observed, kriging_cv_model_exp1$var1.pred)
rmse_exp1 <- calculate_rmse(kriging_cv_model_exp1$observed, kriging_cv_model_exp1$var1.pred)
mae_exp1 <- calculate_mae(kriging_cv_model_exp1$observed, kriging_cv_model_exp1$var1.pred)
mean_normalized_error_exp1 <- calculate_mean_normalized_error(kriging_cv_model_exp1$residual, kriging_cv_model_exp1$observed)
var_normalized_error_exp1 <- calculate_variance_normalized_error(kriging_cv_model_exp1$residual, kriging_cv_model_exp1$observed)

cat("kriging_cv_model_exp1:\n")
cat("MSE:", mse_exp1, "\n")
cat("RMSE:", rmse_exp1, "\n")
cat("MAE:", mae_exp1, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_exp1, "\n")
cat("Variance of Normalized Error:", var_normalized_error_exp1, "\n\n")

# kriging_cv_model_sph1
mse_sph1 <- calculate_mse(kriging_cv_model_sph1$observed, kriging_cv_model_sph1$var1.pred)
rmse_sph1 <- calculate_rmse(kriging_cv_model_sph1$observed, kriging_cv_model_sph1$var1.pred)
mae_sph1 <- calculate_mae(kriging_cv_model_sph1$observed, kriging_cv_model_sph1$var1.pred)
mean_normalized_error_sph1 <- calculate_mean_normalized_error(kriging_cv_model_sph1$residual, kriging_cv_model_sph1$observed)
var_normalized_error_sph1 <- calculate_variance_normalized_error(kriging_cv_model_sph1$residual, kriging_cv_model_sph1$observed)

cat("kriging_cv_model_sph1:\n")
cat("MSE:", mse_sph1, "\n")
cat("RMSE:", rmse_sph1, "\n")
cat("MAE:", mae_sph1, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_sph1, "\n")
cat("Variance of Normalized Error:", var_normalized_error_sph1, "\n\n")

# kriging_cv_model_gauss1
mse_gauss1 <- calculate_mse(kriging_cv_model_gauss1$observed, kriging_cv_model_gauss1$var1.pred)
rmse_gauss1 <- calculate_rmse(kriging_cv_model_gauss1$observed, kriging_cv_model_gauss1$var1.pred)
mae_gauss1 <- calculate_mae(kriging_cv_model_gauss1$observed, kriging_cv_model_gauss1$var1.pred)
mean_normalized_error_gauss1 <- calculate_mean_normalized_error(kriging_cv_model_gauss1$residual, kriging_cv_model_gauss1$observed)
var_normalized_error_gauss1 <- calculate_variance_normalized_error(kriging_cv_model_gauss1$residual, kriging_cv_model_gauss1$observed)

cat("kriging_cv_model_gauss1:\n")
cat("MSE:", mse_gauss1, "\n")
cat("RMSE:", rmse_gauss1, "\n")
cat("MAE:", mae_gauss1, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_gauss1, "\n")
cat("Variance of Normalized Error:", var_normalized_error_gauss1, "\n\n")

# kriging_cv_model_lin1
mse_lin1 <- calculate_mse(kriging_cv_model_lin1$observed, kriging_cv_model_lin1$var1.pred)
rmse_lin1 <- calculate_rmse(kriging_cv_model_lin1$observed, kriging_cv_model_lin1$var1.pred)
mae_lin1 <- calculate_mae(kriging_cv_model_lin1$observed, kriging_cv_model_lin1$var1.pred)
mean_normalized_error_lin1 <- calculate_mean_normalized_error(kriging_cv_model_lin1$residual, kriging_cv_model_lin1$observed)
var_normalized_error_lin1 <- calculate_variance_normalized_error(kriging_cv_model_lin1$residual, kriging_cv_model_lin1$observed)

cat("kriging_cv_model_lin1:\n")
cat("MSE:", mse_lin1, "\n")
cat("RMSE:", rmse_lin1, "\n")
cat("MAE:", mae_lin1, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_lin1, "\n")
cat("Variance of Normalized Error:", var_normalized_error_lin1, "\n\n")

# Repeat the above calculations for kriging_cv_model_exp2, kriging_cv_model_sph2, kriging_cv_model_gauss2, and kriging_cv_model_lin2
# (Use the corresponding objects and function calls)

# Calculate and print MSE, RMSE, MAE, Mean of Normalized Error, and Variance of Normalized Error for each kriging model

# kriging_cv_model_exp2
mse_exp2 <- calculate_mse(kriging_cv_model_exp2$observed, kriging_cv_model_exp2$var1.pred)
rmse_exp2 <- calculate_rmse(kriging_cv_model_exp2$observed, kriging_cv_model_exp2$var1.pred)
mae_exp2 <- calculate_mae(kriging_cv_model_exp2$observed, kriging_cv_model_exp2$var1.pred)
mean_normalized_error_exp2 <- calculate_mean_normalized_error(kriging_cv_model_exp2$residual, kriging_cv_model_exp2$observed)
var_normalized_error_exp2 <- calculate_variance_normalized_error(kriging_cv_model_exp2$residual, kriging_cv_model_exp2$observed)

cat("kriging_cv_model_exp2:\n")
cat("MSE:", mse_exp2, "\n")
cat("RMSE:", rmse_exp2, "\n")
cat("MAE:", mae_exp2, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_exp2, "\n")
cat("Variance of Normalized Error:", var_normalized_error_exp2, "\n\n")

# kriging_cv_model_sph2
mse_sph2 <- calculate_mse(kriging_cv_model_sph2$observed, kriging_cv_model_sph2$var1.pred)
rmse_sph2 <- calculate_rmse(kriging_cv_model_sph2$observed, kriging_cv_model_sph2$var1.pred)
mae_sph2 <- calculate_mae(kriging_cv_model_sph2$observed, kriging_cv_model_sph2$var1.pred)
mean_normalized_error_sph2 <- calculate_mean_normalized_error(kriging_cv_model_sph2$residual, kriging_cv_model_sph2$observed)
var_normalized_error_sph2 <- calculate_variance_normalized_error(kriging_cv_model_sph2$residual, kriging_cv_model_sph2$observed)

cat("kriging_cv_model_sph2:\n")
cat("MSE:", mse_sph2, "\n")
cat("RMSE:", rmse_sph2, "\n")
cat("MAE:", mae_sph2, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_sph2, "\n")
cat("Variance of Normalized Error:", var_normalized_error_sph2, "\n\n")

# kriging_cv_model_gauss2
mse_gauss2 <- calculate_mse(kriging_cv_model_gauss2$observed, kriging_cv_model_gauss2$var1.pred)
rmse_gauss2 <- calculate_rmse(kriging_cv_model_gauss2$observed, kriging_cv_model_gauss2$var1.pred)
mae_gauss2 <- calculate_mae(kriging_cv_model_gauss2$observed, kriging_cv_model_gauss2$var1.pred)
mean_normalized_error_gauss2 <- calculate_mean_normalized_error(kriging_cv_model_gauss2$residual, kriging_cv_model_gauss2$observed)
var_normalized_error_gauss2 <- calculate_variance_normalized_error(kriging_cv_model_gauss2$residual, kriging_cv_model_gauss2$observed)

cat("kriging_cv_model_gauss2:\n")
cat("MSE:", mse_gauss2, "\n")
cat("RMSE:", rmse_gauss2, "\n")
cat("MAE:", mae_gauss2, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_gauss2, "\n")
cat("Variance of Normalized Error:", var_normalized_error_gauss2, "\n\n")

# kriging_cv_model_lin2
mse_lin2 <- calculate_mse(kriging_cv_model_lin2$observed, kriging_cv_model_lin2$var1.pred)
rmse_lin2 <- calculate_rmse(kriging_cv_model_lin2$observed, kriging_cv_model_lin2$var1.pred)
mae_lin2 <- calculate_mae(kriging_cv_model_lin2$observed, kriging_cv_model_lin2$var1.pred)
mean_normalized_error_lin2 <- calculate_mean_normalized_error(kriging_cv_model_lin2$residual, kriging_cv_model_lin2$observed)
var_normalized_error_lin2 <- calculate_variance_normalized_error(kriging_cv_model_lin2$residual, kriging_cv_model_lin2$observed)

cat("kriging_cv_model_lin2:\n")
cat("MSE:", mse_lin2, "\n")
cat("RMSE:", rmse_lin2, "\n")
cat("MAE:", mae_lin2, "\n")
cat("Mean of Normalized Error:", mean_normalized_error_lin2, "\n")
cat("Variance of Normalized Error:", var_normalized_error_lin2, "\n\n")



# Create a data frame to store the results
error_results <- data.frame(
  Model = c("kriging_cv_model_exp1", "kriging_cv_model_sph1", "kriging_cv_model_gauss1", "kriging_cv_model_lin1",
            "kriging_cv_model_exp2", "kriging_cv_model_sph2", "kriging_cv_model_gauss2", "kriging_cv_model_lin2"),
  MSE = c(mse_exp1, mse_sph1, mse_gauss1, mse_lin1, mse_exp2, mse_sph2, mse_gauss2, mse_lin2),
  RMSE = c(rmse_exp1, rmse_sph1, rmse_gauss1, rmse_lin1, rmse_exp2, rmse_sph2, rmse_gauss2, rmse_lin2),
  MAE = c(mae_exp1, mae_sph1, mae_gauss1, mae_lin1, mae_exp2, mae_sph2, mae_gauss2, mae_lin2),
  Mean_of_Normalized_Error = c(mean_normalized_error_exp1, mean_normalized_error_sph1,
                               mean_normalized_error_gauss1, mean_normalized_error_lin1,
                               mean_normalized_error_exp2, mean_normalized_error_sph2,
                               mean_normalized_error_gauss2, mean_normalized_error_lin2),
  Variance_of_Normalized_Error = c(var_normalized_error_exp1, var_normalized_error_sph1,
                                   var_normalized_error_gauss1, var_normalized_error_lin1,
                                   var_normalized_error_exp2, var_normalized_error_sph2,
                                   var_normalized_error_gauss2, var_normalized_error_lin2)
)

# Save the data frame to a CSV file
write.csv(error_results, file = "C:\\Users\\navid.tavakoli\\Desktop\\Error_Result.csv", row.names = FALSE)

# Display the saved data frame
error_results

