# 1. Setting up the environment
setwd("C:/Users/hanof/OneDrive/Desktop/STA202_TPs") # Set working directory
rm(list=objects()); graphics.off() # Clear workspace and close graphics windows


# Read the CSV file
df <- read.csv2("mtcars.csv", sep=",", dec=".", header=TRUE) 

# Rename columns for clarity
colnames(num) <- c( 
  "Miles_per_Gallon",
  "Cylinders",
  "Displacement_cuin",
  "Horsepower",
  "Rear_Axle_Ratio",
  "Weight_lb_per_1000",
  "Quarter_Mile_Time",
  "Engine_Shape",
  "Transmission",
  "Forward_Gears",
  "Carburetors"
)


head(df) # Show the first rows
dim(df) # Dimensions of the data
str(df) # Structure of the data
names(df) # Column names
summary(num) # Descriptive statistics


# 2. EDA (Exploratory Data Analysis)
# Univariate analysis
num <- df[,-1] # Select numeric columns (excluding the first column)
# We notice the presence of both discrete categorical and continuous variables, so we separate them
num_cont <- num[, c("Miles_per_Gallon", "Displacement_cuin", "Horsepower", "Rear_Axle_Ratio",
                     "Weight_lb_per_1000", "Quarter_Mile_Time")] 

num_dis <- num[, c("Miles_per_Gallon","Cylinders","Engine_Shape","Transmission","Forward_Gears","Carburetors")]



# Histograms for continuous variables
# Set up plotting area: 2 rows, 3 columns for original histograms
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))  

# Define variables and colors for original histograms
vars <- c("mpg", "disp", "hp", "wt", "qsec", "drat")
titles <- c("Miles per Gallon", "Displacement", "Horsepower", "Weight", "Quarter Mile Time", "Rear Axle Ratio")
colors <- c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lavenderblush")

# Plot histograms for original variables
for(i in seq_along(vars)) {
  hist(df[[vars[i]]],
       main = paste("Histogram of", titles[i]),
       xlab = titles[i],
       col = colors[i],
       border = "black")
}

# Set up plotting area: 2 rows, 2 columns for log-transformed histograms
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))  

# Variables to plot log-histograms for
log_vars <- c("disp", "hp", "wt", "qsec")
log_titles <- paste("Log of", c("Displacement", "Horsepower", "Weight", "Quarter Mile Time"))
log_colors <- c("lightgreen", "lightcoral", "lightyellow", "lightpink")

# Plot histograms for log-transformed variables
for(i in seq_along(log_vars)) {
  hist(log(df[[log_vars[i]]]),
       main = paste("Histogram of", log_titles[i]),
       xlab = log_titles[i],
       col = log_colors[i],
       border = "black")
}

# Reset plotting area to default
par(mfrow = c(1, 1))


# Apply logarithmic transformation to the specified columns
num$Displacement_log <- log(num$Displacement_cuin)
num$Horsepower_log <- log(num$Horsepower)
num$Weight_log <- log(num$Weight_lb_per_1000)




#Boxplots for continuous variables
# Set up plotting area: 2 rows, 3 columns for boxplots
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))

# Define the variables, titles, and colors for the boxplots
vars <- c("Miles_per_Gallon", "Displacement_cuin", "Horsepower",
          "Rear_Axle_Ratio", "Weight_lb_per_1000", "Quarter_Mile_Time")

colors <- c("lightblue", "lightgreen", "lightcoral", 
            "lavenderblush", "lightyellow", "lightpink")

# Loop through each variable and create a boxplot
for(i in seq_along(vars)) {
  boxplot(num_cont[[vars[i]]],
          main = paste("Boxplot of", vars[i]),
          xlab = vars[i],
          col = colors[i],
          las = 2,           # Rotate axis labels for better readability
          border = "black",
          notch = TRUE)      # Add notch to display confidence interval around median
}

# Reset plotting parameters to default
par(mfrow = c(1,1))



#Barplots for categorical variables  
# Set up plotting area: 2 rows, 3 columns
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))

# Define variables, titles, x-axis labels, and colors for barplots
vars <- c("cyl", "vs", "am", "gear", "carb", "model")
titles <- c("Distribution of Cylinders", "Engine Shape", "Transmission Type", 
            "Number of Forward Gears", "Number of Carburetors", "Car Models Frequency")
xlabels <- c("Number of Cylinders", 
             "Type (0 = V-shaped, 1 = Straight)", 
             "Transmission (0 = Automatic, 1 = Manual)", 
             "Forward Gears", 
             "Carburetors", 
             "Frequency")
colors <- c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lavenderblush")
horiz <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)  # Make last barplot horizontal

# Loop through variables to create barplots
for(i in seq_along(vars)) {
  barplot(table(df[[vars[i]]]),
          main = titles[i],
          xlab = ifelse(horiz[i], "", xlabels[i]),
          ylab = ifelse(horiz[i], xlabels[i], "Frequency"),
          col = colors[i],
          border = "black",
          horiz = horiz[i],
          las = ifelse(horiz[i], 1, 0))  # Rotate labels only for horizontal plot
}

# Reset plotting parameters
par(mfrow = c(1,1))



# Bivariate analysis: Scatterplot matrix of all numeric variables
pairs(num, main = "Scatterplot Matrix of Numeric Variables")

# Load corrplot library for correlation matrix visualization
library(corrplot)

# Compute correlation matrix of numeric variables
cor_matrix <- cor(num)

# Display the correlation matrix (upper triangle only)
corrplot(cor_matrix, type = "upper", 
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Text label rotation angle
         addCoef.col = "gray30", # Add correlation coefficients in the plot
         number.cex = 0.7)       # Size of correlation coefficients

# Remove columns whose names contain "log" (exclude log-transformed variables)
num_no_log <- num[, !grepl("log", colnames(num))]


# 3. Multiple Linear Regression Models

# Model 1: Linear regression with all variables (excluding log-transformed)
model1 <- lm(Miles_per_Gallon ~ ., data = num_no_log) 
summary(model1)
anova(model1)
plot(model1, 2)  # QQ-plot to check normality of residuals

# Model 2: Linear regression with selected variables (excluding log)
model2 <- lm(Miles_per_Gallon ~ Weight_lb_per_1000 + Cylinders + Displacement_cuin + Horsepower + Transmission, 
             data = num_no_log) 
summary(model2)
anova(model2) 
plot(model2, 2)

# Model 3: Linear regression using log-transformed variables for some predictors
model3 <- lm(Miles_per_Gallon ~ Weight_log + Cylinders + Displacement_log + Horsepower_log + Transmission, 
             data = num) 
summary(model3)
anova(model3)
plot(model3, 2)

# Model 4: Linear regression with significant variables only (using log-transformed variables)
model4 <- lm(Miles_per_Gallon ~ Weight_log + Cylinders + Horsepower_log, data = num) 
summary(model4)
anova(model4)
plot(model4, 2)

# Model 5: Full model with all numeric variables, then variable selection by AIC
library(MASS)
full_model_aic <- lm(Miles_per_Gallon ~ ., data = num)
model5 <- stepAIC(full_model_aic, direction = "both")  # Stepwise selection based on AIC
summary(model5)

# Display the formula of the best selected model
formula(model5)

# Model 6: Linear regression with a mix of log and non-log variables (based on prior insight)
model6 <- lm(Miles_per_Gallon ~ Weight_lb_per_1000 + Displacement_cuin + Horsepower_log, data = num) 
summary(model6)


# Effect of adding new observations on the regression model

# Example 1: Adding a typical new observation
typical_new_row <- data.frame(
  Miles_per_Gallon = 20,
  Cylinders = 6,
  Displacement_cuin = 200,
  Horsepower = 150,
  Rear_Axle_Ratio = 3.5,       # Typical value
  Weight_lb_per_1000 = 3.0,
  Quarter_Mile_Time = 17.0,    # Typical value
  Engine_Shape = 0,            # Assume 0 = V-shaped (common)
  Transmission = 0,            # Assume 0 = automatic (common)
  Forward_Gears = 3,           # Typical for automatic
  Carburetors = 2,             # Frequent value
  Displacement_log = log(200),
  Horsepower_log = log(150),
  Weight_log = log(3.0)
)

# Append the typical observation to the dataset
num_plus1_typical <- rbind(num, typical_new_row)

# Fit the model on the extended dataset and summarize
model_plus1_typical <- lm(formula(model4), data = num_plus1_typical)
summary(model_plus1_typical)


# Example 2: Adding an outlier observation
outlier_new_row <- data.frame(
  Miles_per_Gallon = 10,
  Cylinders = 4,
  Displacement_cuin = 50,
  Horsepower = 500,            # Unusually high horsepower (outlier)
  Rear_Axle_Ratio = 4.5,       # Plausible value
  Weight_lb_per_1000 = 5.0,
  Quarter_Mile_Time = 12.0,    # Plausible for high performance
  Engine_Shape = 1,            # 1 = straight (less common)
  Transmission = 1,            # 1 = manual (less common)
  Forward_Gears = 5,           # Plausible for manual
  Carburetors = 4,             # Plausible for high performance
  Displacement_log = log(50),
  Horsepower_log = log(500),
  Weight_log = log(5.0)
)

# Append the outlier observation to the dataset
num_plus1_outlier <- rbind(num, outlier_new_row)

# Fit the model on the dataset including the outlier
model_plus1_outlier <- lm(formula(model4), data = num_plus1_outlier)
summary(model_plus1_outlier)


# Example 3: Adding a new observation in an extended variable range
extended_new_row <- data.frame(
  Miles_per_Gallon = 12,
  Cylinders = 8,
  Displacement_cuin = 450,
  Horsepower = 400,
  Rear_Axle_Ratio = 3.0,       # Plausible value
  Weight_lb_per_1000 = 6.0,
  Quarter_Mile_Time = 13.5,    # Plausible for a large powerful car
  Engine_Shape = 0,            # V-shaped
  Transmission = 0,            # Automatic
  Forward_Gears = 3,           # Automatic
  Carburetors = 4,             # Powerful car
  Displacement_log = log(450),
  Horsepower_log = log(400),
  Weight_log = log(6.0)
)

# Append the extended-range observation to the dataset
num_plus1_extended <- rbind(num, extended_new_row)

# Fit the model on the dataset including the extended-range observation
model_plus1_extended <- lm(formula(model4), data = num_plus1_extended)
summary(model_plus1_extended)



# 4. PCA (Principal Component Analysis)
# --- 1. Data Preparation for PCA ---

# Names of numeric variables (excluding potential target variable)
noms_variables_numeriques <- c("Cylinders", "Displacement_cuin", "Horsepower",
                               "Rear_Axle_Ratio", "Weight_lb_per_1000", "Quarter_Mile_Time",
                               "Engine_Shape", "Transmission", "Forward_Gears", "Carburetors") # List of variable names for PCA

# Subset the data frame to keep only the selected numeric variables
variables_acp <- num_no_log[, noms_variables_numeriques]

# Standardize the data (important for PCA when variables have different scales)
variables_standardisees <- scale(variables_acp)

# --- 2. Performing Principal Component Analysis (PCA) ---
acp_result <- prcomp(variables_standardisees, center = FALSE, scale = FALSE)

# --- 3. Exploration of PCA Results ---

# 3.1. Summary of PCA (explained variance, importance of components, etc.)
summary(acp_result)

# 3.2. Display loadings (weights of variables on principal components)
print(acp_result$rotation)

# 3.3. Scree plot (plot of eigenvalues / variances explained by each component)
plot(acp_result$sdev^2, type = "b",
     main = "Scree Plot",
     xlab = "Principal Component",
     ylab = "Explained Variance")

# 3.4. Biplot (visualization of both individuals and variables on the first two PCs)
biplot(acp_result, choices = 1:2, cex = 0.8,
       main = "Biplot of First Two Principal Components")

# --- 4. Analysis of Principal Component Scores and Clustering ---

# 4.1. Extract scores (coordinates) of the first two principal components
scores_acp <- as.data.frame(acp_result$x[, 1:2])
colnames(scores_acp) <- c("Comp.1", "Comp.2")

# 4.2. Clustering using k-means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(variables_standardisees, centers = 3)
scores_acp$cluster <- as.factor(kmeans_result$cluster)

# 4.4. Visualize clusters in PCA space with ggplot2
library(ggplot2)
ggplot(scores_acp, aes(x = Comp.1, y = Comp.2)) +
  theme_classic() +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_point(aes(color = cluster), alpha = 0.55, size = 3) +
  xlab("First Principal Component (PC1)") +
  ylab("Second Principal Component (PC2)") +
  ggtitle("Projection of Cars on First Two Principal Components (Clusters)") +
  scale_color_discrete(name = "Cluster")

# --- 5. Use of factoextra package for advanced visualizations ---

# Install and load factoextra (do this once outside script)
if(!require(factoextra)) install.packages("factoextra")
library(factoextra)

# 5.1. Scree plot with variance percentages labeled
fviz_eig(acp_result,
         main = "Scree Plot",
         xlab = "Principal Component",
         ylab = "Explained Variance (%)",
         addlabels = TRUE)

# 5.2. Correlation circle (variables plot colored by quality of representation)
fviz_pca_var(acp_result,
             col.var = "cos2",                     # Color by squared cosine (quality)
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,                        # Avoid label overlap
             title = "Correlation Circle of Variables")

# --- 6. Linear Regression on Principal Components ---

# Prepare data:
X <- variables_standardisees              # Standardized predictor variables
y <- num$Miles_per_Gallon                 # Target variable

# Create data frame of scores for all principal components
scores_cp <- as.data.frame(acp_result$x)
colnames(scores_cp) <- paste0("Comp.", 1:ncol(scores_cp))

# 6.1. Regression model using all principal components
modele_rcp_complet <- lm(y ~ ., data = scores_cp)
summary(modele_rcp_complet)

# 6.2. Regression model using only the first two principal components
modele_rcp_2cp <- lm(y ~ Comp.1 + Comp.2, data = scores_cp)
summary(modele_rcp_2cp)

# 6.3. Visualize relationship between target variable and first two PCs
ggplot(scores_cp, aes(x = Comp.1, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Miles per Gallon vs. First Principal Component (PC1)",
       x = "First Principal Component (PC1)",
       y = "Miles per Gallon") +
  theme_minimal()

ggplot(scores_cp, aes(x = Comp.2, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Miles per Gallon vs. Second Principal Component (PC2)",
       x = "Second Principal Component (PC2)",
       y = "Miles per Gallon") +
  theme_minimal()

