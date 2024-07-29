# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(car)
library(mgcv)
library(rstanarm)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(CCA)
library(glmnet)
library(cluster)
library(xgboost)
library(ggplot2)
library(bayesplot)

# Load the dataset
dataset <- read_excel("C:/Users/User/Desktop/Debashis 2024/PeerJ (Bird Health 11 Statistical Analysis)/ALL_RBTB.xlsx", col_types = "text")

# Convert columns to appropriate types, handling 'N/A' and date columns
numeric_columns <- c("istat_number", "Year", "Bill_length", "bill_depth", "bill_width",
                     "wing_length", "tarsus", "weight", "temperature", "heart_rate",
                     "respiratory_weight", "Handling_time", "Heterophils", "Monocytes",
                     "Eosinophils", "Lymphocytes", "Basophils", "WBC_Estimates",
                     "Lactate_mmol/L_mg/dL", "Na", "K", "Cl-", "iCa", "TCO2", "Glucose",
                     "BUN", "CREA", "PCV_%", "Hematocrite_%PCV", "Hemoglobine", "pH",
                     "PCO2", "PO2", "Beecf", "HCO3")

dataset[, numeric_columns] <- lapply(dataset[, numeric_columns], as.numeric)

# Remove rows with too many NAs or convert NAs to means or other appropriate values
dataset <- dataset %>%
  mutate(across(where(is.numeric), ~ replace_na(., mean(., na.rm = TRUE))))

# Set up output directory for saving plots and tables
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)

# 1. Mixed-Effects Linear Model
mixed_model <- lmer(weight ~ Bill_length + bill_depth + wing_length + (1|istat_number), data = dataset)
summary(mixed_model)

# Save summary
sink(file.path(output_dir, "mixed_model_summary.txt"))
print(summary(mixed_model))
sink()

# Generate and save plot
plot1 <- ggplot(dataset, aes(x = Bill_length, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Mixed-Effects Linear Model: Weight vs Bill Length")
ggsave(file.path(output_dir, "mixed_model_plot.png"), plot1)

# 2. MANOVA
manova_model <- manova(cbind(pH, PCO2, PO2) ~ Year + istat_number, data = dataset)
summary(manova_model, test = "Pillai")

# Save summary
sink(file.path(output_dir, "manova_summary.txt"))
print(summary(manova_model, test = "Pillai"))
sink()

# Generate and save plot (alternative approach)
# Plotting individual dependent variables for a MANOVA
manova_plot1 <- ggplot(dataset, aes(x = as.factor(Year), y = pH)) +
  geom_boxplot() +
  ggtitle("Boxplot of pH by Year")
ggsave(file.path(output_dir, "manova_plot1.png"), manova_plot1)

manova_plot2 <- ggplot(dataset, aes(x = as.factor(istat_number), y = PCO2)) +
  geom_boxplot() +
  ggtitle("Boxplot of PCO2 by istat_number")
ggsave(file.path(output_dir, "manova_plot2.png"), manova_plot2)

manova_plot3 <- ggplot(dataset, aes(x = as.factor(Year), y = PO2)) +
  geom_boxplot() +
  ggtitle("Boxplot of PO2 by Year")
ggsave(file.path(output_dir, "manova_plot3.png"), manova_plot3)

# 3. Generalized Additive Model (GAM)
gam_model <- gam(heart_rate ~ s(temperature) + s(weight), data = dataset)
summary(gam_model)

# Save summary
sink(file.path(output_dir, "gam_summary.txt"))
print(summary(gam_model))
sink()

# Generate and save plot
plot(gam_model, pages = 1)
ggsave(file.path(output_dir, "gam_plot.png"))

# 4. Bayesian Hierarchical Model
bayesian_model <- stan_glmer(heart_rate ~ (1|istat_number), data = dataset, family = gaussian(), chains = 4, iter = 2000)
summary(bayesian_model)

# Save summary
sink(file.path(output_dir, "bayesian_model_summary.txt"))
print(summary(bayesian_model))
sink()

# Inspect parameter names in the Bayesian model
parameter_names <- dimnames(as.array(bayesian_model))$parameters
print(parameter_names)

# Select appropriate parameters for diagnostic plot
# Adjust based on the actual parameter names in the model
selected_parameters <- parameter_names[grepl("\\(Intercept\\)|sigma", parameter_names)]

# Generate and save MCMC pairs plot for the Bayesian model
mcmc_pairs(as.array(bayesian_model), pars = selected_parameters)
ggsave(file.path(output_dir, "bayesian_model_pairs.png"))

# 5. Random Forest Regression
rf_model <- randomForest(weight ~ pH + PCO2 + PO2 + heart_rate + temperature, data = dataset)
print(rf_model)

# Save summary
sink(file.path(output_dir, "rf_model_summary.txt"))
print(rf_model)
sink()

# Generate and save plot
varImpPlot(rf_model)
ggsave(file.path(output_dir, "rf_model_varimp.png"))

# 6. Principal Component Analysis (PCA)
pca_model <- PCA(dataset[, c("pH", "PCO2", "PO2", "heart_rate", "temperature")], graph = FALSE)
fviz_pca_biplot(pca_model)

# Save summary
sink(file.path(output_dir, "pca_model_summary.txt"))
print(pca_model)
sink()

# Generate and save plot
fviz_pca_biplot(pca_model, repel = TRUE)
ggsave(file.path(output_dir, "pca_model_biplot.png"))

# 7. Canonical Correlation Analysis (CCA)
cca_model <- cancor(dataset[, c("pH", "PCO2", "PO2")], dataset[, c("heart_rate", "temperature")])
print(cca_model)

# Save summary
sink(file.path(output_dir, "cca_model_summary.txt"))
print(cca_model)
sink()

# Generate and save plot
# CCA plots can be tricky, so here's a basic scatter plot
cca_plot <- ggplot(dataset, aes(x = pH, y = heart_rate)) +
  geom_point() +
  ggtitle("CCA: pH vs Heart Rate")
ggsave(file.path(output_dir, "cca_model_plot.png"), cca_plot)

# 8. Lasso Regression
x <- as.matrix(dataset[, c("pH", "PCO2", "PO2", "heart_rate", "temperature")])
y <- dataset$weight
lasso_model <- cv.glmnet(x, y, alpha = 1)
print(lasso_model)

# Save summary
sink(file.path(output_dir, "lasso_model_summary.txt"))
print(lasso_model)
sink()

# Generate and save plot
plot(lasso_model)
ggsave(file.path(output_dir, "lasso_model_plot.png"))

# 9. Cluster Analysis
distance_matrix <- dist(scale(dataset[, c("pH", "PCO2", "PO2", "heart_rate", "temperature")]))
cluster_model <- hclust(distance_matrix, method = "ward.D2")
plot(cluster_model)

# Save plot
ggsave(file.path(output_dir, "cluster_model_plot.png"))

# 10. XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(dataset[, c("pH", "PCO2", "PO2", "heart_rate", "temperature")]), label = dataset$weight)
params <- list(booster = "gbtree", objective = "reg:squarederror")
xgboost_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
print(xgboost_model)

# Save summary
sink(file.path(output_dir, "xgboost_model_summary.txt"))
print(xgboost_model)
sink()

# Generate and save plot
xgb.plot.importance(xgb.importance(model = xgboost_model))
ggsave(file.path(output_dir, "xgboost_model_importance.png"))

# Save R script
saveRDS(list(mixed_model = mixed_model,
             manova_model = manova_model,
             gam_model = gam_model,
             bayesian_model = bayesian_model,
             rf_model = rf_model,
             pca_model = pca_model,
             cca_model = cca_model,
             lasso_model = lasso_model,
             cluster_model = cluster_model,
             xgboost_model = xgboost_model),
        file.path(output_dir, "models.rds"))
