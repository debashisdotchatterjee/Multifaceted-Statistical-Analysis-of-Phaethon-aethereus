
#This R code should Run after the R code for Linear Statistical Analysis ................


# Load necessary libraries
library(ggplot2)
library(minpack.lm)

# Define the non-linear regression model using nlsLM with increased iterations
nls_model <- nlsLM(weight ~ beta0 + beta1 * exp(beta2 * Bill_length) +
                     beta3 * exp(beta4 * bill_depth) +
                     beta5 * exp(beta6 * wing_length),
                   data = dataset,
                   start = list(beta0 = 0, beta1 = 0.1, beta2 = 0.1,
                                beta3 = 0.1, beta4 = 0.1, beta5 = 0.1,
                                beta6 = 0.1),
                   control = nls.lm.control(maxiter = 200))

# Summary of the non-linear model
nls_summary <- summary(nls_model)

# Save the summary
sink(file.path(output_dir, "nls_model_summary.txt"))
print(nls_summary)
sink()

# Generate and save the plot for the non-linear model
plot1 <- ggplot(dataset, aes(x = Bill_length, y = weight)) +
  geom_point() +
  stat_function(fun = function(x) coef(nls_model)["beta0"] +
                  coef(nls_model)["beta1"] * exp(coef(nls_model)["beta2"] * x),
                color = "blue") +
  ggtitle("Non-linear Regression: Weight vs Bill Length")
ggsave(file.path(output_dir, "nls_model_plot_bill_length.png"), plot1)

plot2 <- ggplot(dataset, aes(x = bill_depth, y = weight)) +
  geom_point() +
  stat_function(fun = function(x) coef(nls_model)["beta0"] +
                  coef(nls_model)["beta3"] * exp(coef(nls_model)["beta4"] * x),
                color = "blue") +
  ggtitle("Non-linear Regression: Weight vs Bill Depth")
ggsave(file.path(output_dir, "nls_model_plot_bill_depth.png"), plot2)

plot3 <- ggplot(dataset, aes(x = wing_length, y = weight)) +
  geom_point() +
  stat_function(fun = function(x) coef(nls_model)["beta0"] +
                  coef(nls_model)["beta5"] * exp(coef(nls_model)["beta6"] * x),
                color = "blue") +
  ggtitle("Non-linear Regression: Weight vs Wing Length")
ggsave(file.path(output_dir, "nls_model_plot_wing_length.png"), plot3)

#############################


# Load necessary libraries
library(segmented)
library(ggplot2)

#Summary Statistics to Choose Initial Breakpoints
#First, let's examine the summary statistics for the predictors:
summary(dataset$Bill_length)
summary(dataset$bill_depth)
summary(dataset$wing_length)

# Load necessary libraries
library(segmented)
library(ggplot2)

# Define the initial linear models
initial_model_length <- lm(weight ~ Bill_length, data = dataset)
initial_model_depth <- lm(weight ~ bill_depth, data = dataset)
initial_model_wing <- lm(weight ~ wing_length, data = dataset)

# Fit the piecewise regression models with adjusted breakpoints
piecewise_model_length <- segmented(initial_model_length, seg.Z = ~Bill_length, psi = list(Bill_length = c(50, 63)))
piecewise_model_depth <- segmented(initial_model_depth, seg.Z = ~bill_depth, psi = list(bill_depth = c(19.5, 21.8)))
piecewise_model_wing <- segmented(initial_model_wing, seg.Z = ~wing_length, psi = list(wing_length = c(30, 32)))

# Summaries of the piecewise models
piecewise_summary_length <- summary(piecewise_model_length)
piecewise_summary_depth <- summary(piecewise_model_depth)
piecewise_summary_wing <- summary(piecewise_model_wing)

# Save the summaries
sink(file.path(output_dir, "piecewise_model_length_summary.txt"))
print(piecewise_summary_length)
sink()

sink(file.path(output_dir, "piecewise_model_depth_summary.txt"))
print(piecewise_summary_depth)
sink()

sink(file.path(output_dir, "piecewise_model_wing_summary.txt"))
print(piecewise_summary_wing)
sink()

# Generate and save the plots for the piecewise models
plot_piecewise_length <- ggplot(dataset, aes(x = Bill_length, y = weight)) +
  geom_point() +
  geom_line(aes(y = fitted(piecewise_model_length)), color = "blue") +
  ggtitle("Piecewise Polynomial Regression: Weight vs Bill Length") +
  theme_minimal()
ggsave(file.path(output_dir, "piecewise_model_plot_bill_length.png"), plot_piecewise_length)

plot_piecewise_depth <- ggplot(dataset, aes(x = bill_depth, y = weight)) +
  geom_point() +
  geom_line(aes(y = fitted(piecewise_model_depth)), color = "blue") +
  ggtitle("Piecewise Polynomial Regression: Weight vs Bill Depth") +
  theme_minimal()
ggsave(file.path(output_dir, "piecewise_model_plot_bill_depth.png"), plot_piecewise_depth)

plot_piecewise_wing <- ggplot(dataset, aes(x = wing_length, y = weight)) +
  geom_point() +
  geom_line(aes(y = fitted(piecewise_model_wing)), color = "blue") +
  ggtitle("Piecewise Polynomial Regression: Weight vs Wing Length") +
  theme_minimal()
ggsave(file.path(output_dir, "piecewise_model_plot_wing_length.png"), plot_piecewise_wing)
