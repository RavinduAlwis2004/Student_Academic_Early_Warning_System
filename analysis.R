# Student Academic Early Warning System: complete teaching script
# Group project: Alwis A.R.K., Rajapaksha A.G.S., Gunarathna G.A.M.A.D.
# Requires base R only. Run from the project folder containing data/.
# In RStudio, open this script and click Source.
# The public/synthetic data support a prototype, not operational deployment.

options(stringsAsFactors = FALSE, scipen = 6)
if (!file.exists("data/StudentPerformanceFactors.csv")) {
  stop("Set the working directory to the project folder containing data/ before running this script.")
}
dir.create("results", recursive = TRUE, showWarnings = FALSE)
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

students <- read.csv(
  "data/StudentPerformanceFactors.csv",
  na.strings = c("", "NA")
)

dim(students)

head(students)

str(students)

colSums(is.na(students))

# Count exact duplicate rows
sum(duplicated(students))

# Check the exam score range
range(students$Exam_Score, na.rm = TRUE)

# Inspect scores outside the assumed 0-100 scale
invalid_scores <- students[
  !is.na(students$Exam_Score) &
    (students$Exam_Score < 0 | students$Exam_Score > 100),
]

invalid_scores

# Count rows containing at least one missing value
sum(!complete.cases(students))

# Calculate the missing percentage for each column
round(colMeans(is.na(students)) * 100, 2)

# Create a working copy and preserve the original dataset
students_clean <- students

# Add a row identifier for tracing records
students_clean$record_id <- seq_len(nrow(students_clean))

# Identify scores outside the assumed 0-100 scale
invalid_score <- !is.na(students_clean$Exam_Score) &
  (students_clean$Exam_Score < 0 |
     students_clean$Exam_Score > 100)

# Keep invalid records separately for documentation
quarantined_records <- students_clean[invalid_score, ]

# Exclude invalid scores from the working dataset
students_clean <- students_clean[!invalid_score, ]

# Check the cleaned dataset
nrow(students_clean)
range(students_clean$Exam_Score, na.rm = TRUE)

# Inspect the quarantined record
quarantined_records

# Define the illustrative academic support outcome
students_clean$Risk <- as.integer(
  students_clean$Exam_Score < 65
)

# Count records in each outcome group
table(students_clean$Risk)

# Calculate the percentage in each outcome group
round(prop.table(table(students_clean$Risk)) * 100, 2)

# --------------------------------------------------
# Step 3: Descriptive analysis
# --------------------------------------------------

# Select the numeric variables for descriptive analysis
numeric_variables <- c(
  "Hours_Studied",
  "Attendance",
  "Sleep_Hours",
  "Previous_Scores",
  "Tutoring_Sessions",
  "Physical_Activity",
  "Exam_Score"
)

# Calculate summary statistics for each numeric variable
descriptive_summary <- data.frame(
  Variable = numeric_variables,
  N = sapply(students_clean[numeric_variables],
             function(x) sum(!is.na(x))),
  Mean = sapply(students_clean[numeric_variables],
                function(x) mean(x, na.rm = TRUE)),
  SD = sapply(students_clean[numeric_variables],
              function(x) sd(x, na.rm = TRUE)),
  Minimum = sapply(students_clean[numeric_variables],
                   function(x) min(x, na.rm = TRUE)),
  Median = sapply(students_clean[numeric_variables],
                  function(x) median(x, na.rm = TRUE)),
  Maximum = sapply(students_clean[numeric_variables],
                   function(x) max(x, na.rm = TRUE)),
  row.names = NULL
)

# Round numeric results for easier reading
descriptive_summary[-1] <- lapply(
  descriptive_summary[-1],
  round,
  digits = 2
)

# Display the summary table
print(descriptive_summary)

# Create a folder and save the summary table
dir.create("results", showWarnings = FALSE)

write.csv(
  descriptive_summary,
  "results/descriptive_summary.csv",
  row.names = FALSE
)

# Plot the distribution of valid exam scores
hist(
  students_clean$Exam_Score,
  breaks = 25,
  main = "Distribution of Exam Scores",
  xlab = "Exam Score",
  ylab = "Number of Records",
  col = "#2A9D8F",
  border = "white"
)

# Mark the illustrative teacher-review outcome cutoff
abline(
  v = 65,
  col = "#C0392B",
  lwd = 2,
  lty = 2
)

# Explain the cutoff line
legend(
  "topright",
  legend = "Illustrative cutoff: 65",
  col = "#C0392B",
  lwd = 2,
  lty = 2,
  bty = "n"
)

# --------------------------------------------------
# Step 4: Relationships with exam score
# --------------------------------------------------

# Calculate Pearson correlations with exam score
relationship_variables <- c(
  "Attendance",
  "Hours_Studied",
  "Previous_Scores",
  "Tutoring_Sessions"
)

correlation_summary <- data.frame(
  Variable = relationship_variables,
  Correlation = sapply(
    students_clean[relationship_variables],
    function(x) cor(
      x,
      students_clean$Exam_Score,
      use = "complete.obs",
      method = "pearson"
    )
  ),
  row.names = NULL
)

# Display rounded correlations
correlation_summary$Correlation <- round(
  correlation_summary$Correlation,
  digits = 3
)

print(correlation_summary)

# Save the correlation table
write.csv(
  correlation_summary,
  "results/score_correlations.csv",
  row.names = FALSE
)

# Show two scatterplots side by side
par(mfrow = c(1, 2))

# Plot attendance against exam score
plot(
  students_clean$Attendance,
  students_clean$Exam_Score,
  main = "Attendance vs Exam Score",
  xlab = "Attendance",
  ylab = "Exam Score",
  pch = 16,
  cex = 0.5,
  col = adjustcolor("#2A9D8F", alpha.f = 0.2)
)

# Add an unadjusted linear trend
abline(
  lm(Exam_Score ~ Attendance, data = students_clean),
  col = "#C0392B",
  lwd = 2
)

# Plot study hours against exam score
plot(
  students_clean$Hours_Studied,
  students_clean$Exam_Score,
  main = "Study Hours vs Exam Score",
  xlab = "Hours Studied",
  ylab = "Exam Score",
  pch = 16,
  cex = 0.5,
  col = adjustcolor("#2A9D8F", alpha.f = 0.2)
)

# Add an unadjusted linear trend
abline(
  lm(Exam_Score ~ Hours_Studied, data = students_clean),
  col = "#C0392B",
  lwd = 2
)

# Restore the single-plot layout
par(mfrow = c(1, 1))

# --------------------------------------------------
# Step 5A: Compare mean scores by internet access
# --------------------------------------------------

# Check the number of records in each group
table(students_clean$Internet_Access, useNA = "ifany")

# Display the mean score in each group
internet_means <- aggregate(
  Exam_Score ~ Internet_Access,
  data = students_clean,
  FUN = mean
)

print(internet_means)

# Extract valid scores for each group
internet_yes <- with(
  students_clean,
  Exam_Score[
    !is.na(Internet_Access) & Internet_Access == "Yes"
  ]
)

internet_no <- with(
  students_clean,
  Exam_Score[
    !is.na(Internet_Access) & Internet_Access == "No"
  ]
)

# Run Welch's two-sample t-test
# The estimated difference is Yes minus No
internet_test <- t.test(
  x = internet_yes,
  y = internet_no,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)

print(internet_test)

# Calculate the mean difference
internet_difference <- mean(internet_yes) - mean(internet_no)

print(internet_difference)

# Save the complete test output
capture.output(
  internet_test,
  file = "results/internet_welch_test.txt"
)

# --------------------------------------------------
# Step 5B: Compare scores across resource-access groups
# --------------------------------------------------

# Display the number of records in each group
table(students_clean$Access_to_Resources, useNA = "ifany")

# Calculate the mean score in each group
resource_means <- aggregate(
  Exam_Score ~ Access_to_Resources,
  data = students_clean,
  FUN = mean
)

print(resource_means)

# Run Welch's one-way ANOVA
resource_test <- oneway.test(
  Exam_Score ~ Access_to_Resources,
  data = students_clean,
  var.equal = FALSE
)

print(resource_test)

# Save the test output
capture.output(
  resource_test,
  file = "results/resource_welch_anova.txt"
)

# Save the group means
write.csv(
  resource_means,
  "results/resource_group_means.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 5C: Compare below-65 proportions by attendance
# --------------------------------------------------

# Define the two attendance groups
low_attendance <- students_clean$Attendance < 75

# Count below-65 outcomes in each group
outcome_counts <- c(
  sum(students_clean$Risk[low_attendance]),
  sum(students_clean$Risk[!low_attendance])
)

# Count all records in each group
group_totals <- c(
  sum(low_attendance),
  sum(!low_attendance)
)

# Summarise the groups
attendance_summary <- data.frame(
  Group = c("Attendance < 75", "Attendance >= 75"),
  Total = group_totals,
  Below_65 = outcome_counts,
  Percentage = round(
    outcome_counts / group_totals * 100,
    digits = 2
  )
)

print(attendance_summary)

# Run the two-sample proportion test
# The difference is low attendance minus higher attendance
attendance_test <- prop.test(
  x = outcome_counts,
  n = group_totals,
  alternative = "two.sided",
  correct = FALSE
)

print(attendance_test)

# Save the summary and test output
write.csv(
  attendance_summary,
  "results/attendance_outcome_summary.csv",
  row.names = FALSE
)

capture.output(
  attendance_test,
  file = "results/attendance_proportion_test.txt"
)

# --------------------------------------------------
# Adjust p-values across the three exploratory tests
# --------------------------------------------------

# Collect the original p-values
inference_summary <- data.frame(
  Test = c(
    "Internet access: Welch t-test",
    "Resource access: Welch ANOVA",
    "Attendance: two-proportion test"
  ),
  Raw_p_value = c(
    internet_test$p.value,
    resource_test$p.value,
    attendance_test$p.value
  )
)

# Apply Holm adjustment for multiple testing
inference_summary$Holm_p_value <- p.adjust(
  inference_summary$Raw_p_value,
  method = "holm"
)

print(inference_summary)

# Save the adjusted results
write.csv(
  inference_summary,
  "results/hypothesis_test_summary.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 6: Stratified training, validation and test split
# --------------------------------------------------

# Set the seed immediately before sampling for reproducibility
set.seed(24100903)

# Split row positions within one outcome group
split_outcome_group <- function(row_positions) {

  shuffled_rows <- sample(row_positions)
  total_rows <- length(shuffled_rows)

  train_count <- floor(0.60 * total_rows)
  validation_count <- floor(0.20 * total_rows)

  list(
    train = shuffled_rows[seq_len(train_count)],

    validation = shuffled_rows[
      seq.int(
        train_count + 1,
        train_count + validation_count
      )
    ],

    test = shuffled_rows[
      seq.int(
        train_count + validation_count + 1,
        total_rows
      )
    ]
  )
}

# Separate row positions by the binary outcome
rows_by_outcome <- split(
  seq_len(nrow(students_clean)),
  students_clean$Risk
)

# Split each outcome group independently
group_splits <- lapply(
  rows_by_outcome,
  split_outcome_group
)

# Combine row positions for each partition
partition_indices <- lapply(
  c("train", "validation", "test"),
  function(partition_name) {
    sort(
      unlist(
        lapply(group_splits, function(group) {
          group[[partition_name]]
        }),
        use.names = FALSE
      )
    )
  }
)

names(partition_indices) <- c(
  "train",
  "validation",
  "test"
)

# Verify that every row appears in exactly one partition
all_indices <- unlist(
  partition_indices,
  use.names = FALSE
)

stopifnot(
  length(all_indices) == nrow(students_clean),
  !anyDuplicated(all_indices),
  setequal(all_indices, seq_len(nrow(students_clean)))
)

# Create the three datasets before preprocessing
train_raw <- students_clean[partition_indices$train, ]
validation_raw <- students_clean[partition_indices$validation, ]
test_raw <- students_clean[partition_indices$test, ]

# Summarise partition sizes and outcome proportions
split_summary <- data.frame(
  Partition = c("Training", "Validation", "Test"),

  Records = c(
    nrow(train_raw),
    nrow(validation_raw),
    nrow(test_raw)
  ),

  Below_65 = c(
    sum(train_raw$Risk),
    sum(validation_raw$Risk),
    sum(test_raw$Risk)
  )
)

split_summary$Below_65_Percentage <- round(
  split_summary$Below_65 / split_summary$Records * 100,
  digits = 2
)

print(split_summary)

# Save the partition summary
write.csv(
  split_summary,
  "results/data_split_summary.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 7: Training-only imputation and factor conversion
# --------------------------------------------------

# Define category levels using the dataset codebook
# The first level becomes the reference category in regression
factor_levels <- list(
  Parental_Involvement = c("Low", "Medium", "High"),
  Access_to_Resources = c("Low", "Medium", "High"),
  Extracurricular_Activities = c("No", "Yes"),
  Motivation_Level = c("Low", "Medium", "High"),
  Internet_Access = c("No", "Yes"),
  Family_Income = c("Low", "Medium", "High"),
  Teacher_Quality = c("Low", "Medium", "High"),
  School_Type = c("Public", "Private"),
  Peer_Influence = c("Negative", "Neutral", "Positive"),
  Learning_Disabilities = c("No", "Yes"),
  Parental_Education_Level = c(
    "High School", "College", "Postgraduate"
  ),
  Distance_from_Home = c("Near", "Moderate", "Far"),
  Gender = c("Female", "Male")
)

categorical_variables <- names(factor_levels)

# Find the most frequent non-missing category
get_mode <- function(x) {
  counts <- table(x, useNA = "no")

  if (length(counts) == 0) {
    stop("A categorical training column has no observed values.")
  }

  names(counts)[which.max(counts)]
}

# Learn replacement values from training data only
training_modes <- sapply(
  train_raw[categorical_variables],
  get_mode
)

# Display the replacement values for columns with missing data
missing_columns <- categorical_variables[
  sapply(
    students_clean[categorical_variables],
    function(x) anyNA(x)
  )
]

imputation_summary <- data.frame(
  Variable = missing_columns,
  Training_Mode = unname(training_modes[missing_columns])
)

print(imputation_summary)

# Apply the same preprocessing to any partition
prepare_data <- function(data) {

  for (variable in categorical_variables) {

    values <- as.character(data[[variable]])

    # Reject unexpected categories instead of silently creating NAs
    unexpected_values <- setdiff(
      unique(values[!is.na(values)]),
      factor_levels[[variable]]
    )

    if (length(unexpected_values) > 0) {
      stop(paste("Unexpected category in:", variable))
    }

    # Fill missing values using the training mode
    values[is.na(values)] <- training_modes[[variable]]

    # Apply consistent category levels across all partitions
    data[[variable]] <- factor(
      values,
      levels = factor_levels[[variable]]
    )
  }

  data
}

# Create model-ready datasets
train_data <- prepare_data(train_raw)
validation_data <- prepare_data(validation_raw)
test_data <- prepare_data(test_raw)

# Check for remaining missing values
remaining_missing <- c(
  Training = sum(is.na(train_data)),
  Validation = sum(is.na(validation_data)),
  Test = sum(is.na(test_data))
)

print(remaining_missing)

# Confirm that preprocessing removed all missing values
stopifnot(all(remaining_missing == 0))

# Inspect the factor levels of an example variable
levels(train_data$Access_to_Resources)

# Save the imputation summary
write.csv(
  imputation_summary,
  "results/training_imputation_summary.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 8: Fit and compare score prediction models
# --------------------------------------------------

# Define the four core academic predictors
simple_predictors <- c(
  "Hours_Studied",
  "Attendance",
  "Previous_Scores",
  "Tutoring_Sessions"
)

# Define all 19 predictors explicitly
# Exclude Exam_Score, Risk and the generated record ID
all_predictors <- names(factor_levels)
all_predictors <- c(
  "Hours_Studied",
  "Attendance",
  "Sleep_Hours",
  "Previous_Scores",
  "Tutoring_Sessions",
  "Physical_Activity",
  all_predictors
)

stopifnot(
  length(all_predictors) == 19,
  !anyDuplicated(all_predictors)
)

# Fit the four-predictor linear model using training data
lm_simple <- lm(
  reformulate(simple_predictors, response = "Exam_Score"),
  data = train_data
)

# Fit the full linear model using training data
lm_full <- lm(
  reformulate(all_predictors, response = "Exam_Score"),
  data = train_data
)

# Inspect the simple model
summary(lm_simple)

# Define evaluation metrics
regression_metrics <- function(actual, predicted) {
  data.frame(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    R2 = 1 -
      sum((actual - predicted)^2) /
      sum((actual - mean(actual))^2)
  )
}

# Calculate the baseline prediction from training data only
training_mean_score <- mean(train_data$Exam_Score)

# Generate predictions for the validation set
baseline_predictions <- rep(
  training_mean_score,
  nrow(validation_data)
)

simple_predictions <- predict(
  lm_simple,
  newdata = validation_data
)

full_predictions <- predict(
  lm_full,
  newdata = validation_data
)

# Compare validation performance
validation_results <- rbind(
  data.frame(
    Model = "Mean baseline",
    regression_metrics(
      validation_data$Exam_Score,
      baseline_predictions
    )
  ),
  data.frame(
    Model = "Four-predictor linear",
    regression_metrics(
      validation_data$Exam_Score,
      simple_predictions
    )
  ),
  data.frame(
    Model = "Full linear",
    regression_metrics(
      validation_data$Exam_Score,
      full_predictions
    )
  )
)

# Display rounded results without changing stored precision
validation_display <- validation_results
validation_display[-1] <- lapply(
  validation_display[-1],
  round,
  digits = 3
)

print(validation_display)

# Save validation results and model summaries
write.csv(
  validation_results,
  "results/linear_validation_results.csv",
  row.names = FALSE
)

capture.output(
  summary(lm_simple),
  file = "results/simple_linear_model_summary.txt"
)

capture.output(
  summary(lm_full),
  file = "results/full_linear_model_summary.txt"
)

# --------------------------------------------------
# Step 9: Compare ridge regression on validation data
# --------------------------------------------------

# Extract the encoded predictor matrix without the intercept
ridge_train_matrix <- model.matrix(lm_full)[, -1, drop = FALSE]

# Learn scaling parameters from training data only
ridge_centres <- colMeans(ridge_train_matrix)
ridge_scales <- apply(ridge_train_matrix, 2, sd)

stopifnot(all(ridge_scales > 0))

# Standardise training predictors
ridge_train_scaled <- scale(
  ridge_train_matrix,
  center = ridge_centres,
  scale = ridge_scales
)

# Centre the training outcome
ridge_outcome_mean <- mean(train_data$Exam_Score)
ridge_outcome_centred <- (
  train_data$Exam_Score - ridge_outcome_mean
)

# Fit ridge coefficients for a given penalty
# The intercept is not penalised
fit_ridge <- function(lambda) {
  as.vector(
    solve(
      crossprod(ridge_train_scaled) +
        diag(lambda, ncol(ridge_train_scaled)),
      crossprod(ridge_train_scaled, ridge_outcome_centred)
    )
  )
}

# Predict using training-derived encoding and scaling
predict_ridge <- function(new_data, coefficients) {

  predictor_matrix <- model.matrix(
    delete.response(terms(lm_full)),
    data = new_data,
    contrasts.arg = lm_full$contrasts,
    xlev = lm_full$xlevels
  )[, -1, drop = FALSE]

  predictor_matrix <- predictor_matrix[
    , colnames(ridge_train_matrix), drop = FALSE
  ]

  scaled_matrix <- scale(
    predictor_matrix,
    center = ridge_centres,
    scale = ridge_scales
  )

  as.vector(
    ridge_outcome_mean + scaled_matrix %*% coefficients
  )
}

# Compare candidate penalties using validation RMSE
ridge_penalties <- c(0.01, 0.1, 1, 10, 100)

ridge_validation <- do.call(
  rbind,
  lapply(ridge_penalties, function(lambda) {

    coefficients <- fit_ridge(lambda)

    predictions <- predict_ridge(
      validation_data,
      coefficients
    )

    data.frame(
      Lambda = lambda,
      regression_metrics(
        validation_data$Exam_Score,
        predictions
      )
    )
  })
)

print(ridge_validation)

# Select the penalty with the lowest validation RMSE
best_lambda <- ridge_validation$Lambda[
  which.min(ridge_validation$RMSE)
]

ridge_coefficients <- fit_ridge(best_lambda)

print(best_lambda)

# Add the selected ridge model to the comparison
score_model_comparison <- rbind(
  validation_results,
  data.frame(
    Model = "Full ridge",
    regression_metrics(
      validation_data$Exam_Score,
      predict_ridge(validation_data, ridge_coefficients)
    )
  )
)

# Select using full-precision validation RMSE
selected_score_model <- score_model_comparison$Model[
  which.min(score_model_comparison$RMSE)
]

print(score_model_comparison)
print(selected_score_model)

# Save the validation results
write.csv(
  ridge_validation,
  "results/ridge_validation_grid.csv",
  row.names = FALSE
)

write.csv(
  score_model_comparison,
  "results/score_model_validation_comparison.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 10A: Visual diagnostics for the full linear model
# --------------------------------------------------

# Display the four standard diagnostic plots
par(mfrow = c(2, 2))

plot(
  lm_full,
  which = c(1, 2, 3, 5),
  ask = FALSE
)

# Restore the single-plot layout
par(mfrow = c(1, 1))

# Save the diagnostic plots as an image
dir.create(
  "results/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  "results/figures/full_linear_diagnostics.png",
  width = 1600,
  height = 1200,
  res = 150
)

par(mfrow = c(2, 2))

plot(
  lm_full,
  which = c(1, 2, 3, 5),
  ask = FALSE
)

dev.off()

# Restore the plotting layout
par(mfrow = c(1, 1))

# --------------------------------------------------
# Step 10B: Check multicollinearity and influence
# --------------------------------------------------

# Extract the full model's encoded design matrix
design_matrix <- model.matrix(lm_full)

# Calculate VIF for each encoded predictor column
# These are column-level VIFs, not whole-factor GVIFs
vif_values <- sapply(
  seq.int(2, ncol(design_matrix)),
  function(column_index) {

    predictor_values <- design_matrix[, column_index]

    other_predictors <- design_matrix[
      , -c(1, column_index),
      drop = FALSE
    ]

    auxiliary_model <- lm(
      predictor_values ~ other_predictors
    )

    auxiliary_r2 <- summary(auxiliary_model)$r.squared

    1 / (1 - auxiliary_r2)
  }
)

vif_summary <- data.frame(
  Predictor = colnames(design_matrix)[-1],
  VIF = unname(vif_values)
)

# Display the largest VIF values first
vif_summary <- vif_summary[
  order(vif_summary$VIF, decreasing = TRUE),
]

print(head(vif_summary, 10))
print(max(vif_summary$VIF))

# Calculate Cook's distance for training records
cook_values <- cooks.distance(lm_full)
cook_threshold <- 4 / nobs(lm_full)

influence_summary <- data.frame(
  Training_records = nobs(lm_full),
  Cook_threshold = cook_threshold,
  Records_above_threshold = sum(
    cook_values > cook_threshold
  ),
  Maximum_Cook_distance = max(cook_values)
)

print(influence_summary)

# Save diagnostic summaries
write.csv(
  vif_summary,
  "results/full_linear_vif.csv",
  row.names = FALSE
)

write.csv(
  influence_summary,
  "results/full_linear_influence.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 10C: Numerical diagnostics and HC3 uncertainty
# --------------------------------------------------

# Extract training residuals and the design matrix
model_errors <- residuals(lm_full)
X <- model.matrix(lm_full)
training_n <- nobs(lm_full)

# Calculate the studentised Breusch-Pagan LM statistic
# H0: Error variance is constant across the predictors
variance_model <- lm(
  I(model_errors^2) ~ X[, -1, drop = FALSE]
)

bp_statistic <- training_n * summary(variance_model)$r.squared
bp_df <- ncol(X) - 1

bp_p_value <- pchisq(
  bp_statistic,
  df = bp_df,
  lower.tail = FALSE
)

# Calculate the Jarque-Bera normality statistic
# H0: Residual skewness and kurtosis match a normal distribution
scaled_errors <- model_errors / sqrt(mean(model_errors^2))

residual_skewness <- mean(scaled_errors^3)
residual_kurtosis <- mean(scaled_errors^4)

jb_statistic <- training_n / 6 * (
  residual_skewness^2 +
    (residual_kurtosis - 3)^2 / 4
)

jb_p_value <- pchisq(
  jb_statistic,
  df = 2,
  lower.tail = FALSE
)

diagnostic_tests <- data.frame(
  Test = c(
    "Studentised Breusch-Pagan",
    "Jarque-Bera"
  ),
  Statistic = c(bp_statistic, jb_statistic),
  DF = c(bp_df, 2),
  P_value = c(bp_p_value, jb_p_value)
)

print(diagnostic_tests)

# Calculate the HC3 robust covariance matrix
leverage_values <- hatvalues(lm_full)
inverse_xtx <- solve(crossprod(X))

hc3_weights <- (
  model_errors / (1 - leverage_values)
)^2

hc3_covariance <- inverse_xtx %*%
  crossprod(X, X * hc3_weights) %*%
  inverse_xtx

# Calculate robust standard errors and approximate 95% intervals
coefficient_estimates <- coef(lm_full)
hc3_standard_errors <- sqrt(diag(hc3_covariance))
critical_value <- qt(0.975, df = df.residual(lm_full))

robust_coefficients <- data.frame(
  Term = names(coefficient_estimates),
  Estimate = unname(coefficient_estimates),
  HC3_SE = unname(hc3_standard_errors),
  CI_low = unname(
    coefficient_estimates -
      critical_value * hc3_standard_errors
  ),
  CI_high = unname(
    coefficient_estimates +
      critical_value * hc3_standard_errors
  )
)

# Display the four core predictors
print(
  robust_coefficients[
    robust_coefficients$Term %in% simple_predictors,
  ]
)

# Save the numerical checks and robust coefficient intervals
write.csv(
  diagnostic_tests,
  "results/linear_diagnostic_tests.csv",
  row.names = FALSE
)

write.csv(
  robust_coefficients,
  "results/linear_coefficients_HC3.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 11: Fit candidate early warning models
# --------------------------------------------------

# Define the expanded set of 10 predictors
expanded_predictors <- c(
  simple_predictors,
  "Sleep_Hours",
  "Physical_Activity",
  "Access_to_Resources",
  "Internet_Access",
  "Motivation_Level",
  "Teacher_Quality"
)

# Fit the four-predictor logistic model
logistic_simple <- glm(
  reformulate(simple_predictors, response = "Risk"),
  data = train_data,
  family = binomial(link = "logit")
)

# Fit the expanded logistic model
logistic_expanded <- glm(
  reformulate(expanded_predictors, response = "Risk"),
  data = train_data,
  family = binomial(link = "logit")
)

# Check whether both models converged
convergence_summary <- data.frame(
  Model = c("Four-predictor", "Expanded"),
  Converged = c(
    logistic_simple$converged,
    logistic_expanded$converged
  )
)

print(convergence_summary)

stopifnot(
  logistic_simple$converged,
  logistic_expanded$converged
)

# Generate validation probabilities
simple_validation_probability <- predict(
  logistic_simple,
  newdata = validation_data,
  type = "response"
)

expanded_validation_probability <- predict(
  logistic_expanded,
  newdata = validation_data,
  type = "response"
)

# Define probability-based evaluation metrics
probability_metrics <- function(actual, probability) {

  # Clip probabilities only when calculating logarithms
  clipped_probability <- pmin(
    pmax(probability, 1e-12),
    1 - 1e-12
  )

  data.frame(
    Log_loss = -mean(
      actual * log(clipped_probability) +
        (1 - actual) * log(1 - clipped_probability)
    ),
    Brier_score = mean((probability - actual)^2)
  )
}

# Compare candidate models using validation data
logistic_validation_results <- rbind(
  data.frame(
    Model = "Four-predictor",
    probability_metrics(
      validation_data$Risk,
      simple_validation_probability
    )
  ),
  data.frame(
    Model = "Expanded",
    probability_metrics(
      validation_data$Risk,
      expanded_validation_probability
    )
  )
)

print(logistic_validation_results)

# Select the model with the lowest validation log loss
selected_risk_model_name <- logistic_validation_results$Model[
  which.min(logistic_validation_results$Log_loss)
]

risk_models <- list(
  "Four-predictor" = logistic_simple,
  "Expanded" = logistic_expanded
)

selected_risk_model <- risk_models[[selected_risk_model_name]]

print(selected_risk_model_name)

# Inspect the selected model
summary(selected_risk_model)

# Save validation results and the selected model summary
write.csv(
  logistic_validation_results,
  "results/logistic_validation_results.csv",
  row.names = FALSE
)

capture.output(
  summary(selected_risk_model),
  file = "results/selected_logistic_model_summary.txt"
)

# --------------------------------------------------
# Step 12: Select the alert threshold using validation data
# --------------------------------------------------

# Generate validation probabilities from the selected model
validation_probability <- predict(
  selected_risk_model,
  newdata = validation_data,
  type = "response"
)

stopifnot(
  all(is.finite(validation_probability)),
  all(validation_probability >= 0),
  all(validation_probability <= 1)
)

# Calculate alert metrics for one threshold
alert_metrics <- function(actual, probability, threshold) {

  alert <- as.integer(probability >= threshold)

  tp <- sum(alert == 1 & actual == 1)
  fp <- sum(alert == 1 & actual == 0)
  tn <- sum(alert == 0 & actual == 0)
  fn <- sum(alert == 0 & actual == 1)

  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  specificity <- if (tn + fp > 0) tn / (tn + fp) else NA_real_

  data.frame(
    Threshold = threshold,
    TP = tp,
    FP = fp,
    TN = tn,
    FN = fn,
    Alerts = tp + fp,
    Recall = recall,
    Precision = precision,
    Specificity = specificity,
    Accuracy = (tp + tn) / length(actual)
  )
}

# Evaluate all distinct validation probabilities as thresholds
candidate_thresholds <- sort(
  unique(c(0, validation_probability, 0.5, 1))
)

threshold_search <- do.call(
  rbind,
  lapply(candidate_thresholds, function(threshold) {
    alert_metrics(
      validation_data$Risk,
      validation_probability,
      threshold
    )
  })
)

# Apply the illustrative minimum recall requirement
target_recall <- 0.85

eligible_thresholds <- threshold_search[
  threshold_search$Recall >= target_recall,
]

# Select the largest threshold meeting the requirement
selected_threshold <- max(eligible_thresholds$Threshold)

print(selected_threshold)

# Compare the selected policy with the default threshold
validation_policy_comparison <- rbind(
  data.frame(
    Policy = "Default threshold",
    alert_metrics(
      validation_data$Risk,
      validation_probability,
      0.5
    )
  ),
  data.frame(
    Policy = "Recall-target threshold",
    alert_metrics(
      validation_data$Risk,
      validation_probability,
      selected_threshold
    )
  )
)

print(validation_policy_comparison)

# Save the threshold search and policy comparison
write.csv(
  threshold_search,
  "results/validation_threshold_search.csv",
  row.names = FALSE
)

write.csv(
  validation_policy_comparison,
  "results/validation_alert_policy.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 13: Final evaluation on the held-out test set
# --------------------------------------------------

# Predict test probabilities using the selected training model
test_probability <- predict(
  selected_risk_model,
  newdata = test_data,
  type = "response"
)

stopifnot(
  all(is.finite(test_probability)),
  all(test_probability >= 0),
  all(test_probability <= 1)
)

# Compare the frozen selected threshold with the default policy
test_policy_comparison <- rbind(
  data.frame(
    Policy = "Default threshold",
    alert_metrics(
      test_data$Risk,
      test_probability,
      0.5
    )
  ),
  data.frame(
    Policy = "Selected threshold",
    alert_metrics(
      test_data$Risk,
      test_probability,
      selected_threshold
    )
  )
)

print(test_policy_comparison)

# Create the final binary alerts
test_alert <- as.integer(
  test_probability >= selected_threshold
)

# Display the confusion matrix
# Rows are actual outcomes and columns are predicted alerts
confusion_matrix <- table(
  Actual = factor(
    test_data$Risk,
    levels = c(0, 1),
    labels = c("Score >= 65", "Score < 65")
  ),
  Predicted = factor(
    test_alert,
    levels = c(0, 1),
    labels = c("No alert", "Review alert")
  )
)

print(confusion_matrix)

# Calculate ROC AUC using ranks, with average ranks for ties
calculate_auc <- function(actual, probability) {

  positive_n <- sum(actual == 1)
  negative_n <- sum(actual == 0)

  if (positive_n == 0 || negative_n == 0) {
    return(NA_real_)
  }

  (
    sum(rank(probability)[actual == 1]) -
      positive_n * (positive_n + 1) / 2
  ) / (positive_n * negative_n)
}

# Evaluate probability quality independently of the alert threshold
test_probability_results <- data.frame(
  Model = selected_risk_model_name,
  AUC = calculate_auc(test_data$Risk, test_probability),
  probability_metrics(test_data$Risk, test_probability)
)

print(test_probability_results)

# Evaluate the already selected score model
score_test_predictions <- switch(
  selected_score_model,
  "Mean baseline" = rep(training_mean_score, nrow(test_data)),
  "Four-predictor linear" = predict(lm_simple, newdata = test_data),
  "Full linear" = predict(lm_full, newdata = test_data),
  "Full ridge" = predict_ridge(test_data, ridge_coefficients),
  stop("Unknown selected score model.")
)

score_test_results <- data.frame(
  Model = selected_score_model,
  regression_metrics(
    test_data$Exam_Score,
    score_test_predictions
  )
)

print(score_test_results)

# Save final evaluation tables
write.csv(
  test_policy_comparison,
  "results/final_test_alert_metrics.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame.matrix(confusion_matrix),
  "results/final_test_confusion_matrix.csv",
  row.names = TRUE
)

write.csv(
  test_probability_results,
  "results/final_test_probability_metrics.csv",
  row.names = FALSE
)

write.csv(
  score_test_results,
  "results/final_test_score_metrics.csv",
  row.names = FALSE
)# --------------------------------------------------
# Step 14: Evaluate test-set calibration
# --------------------------------------------------

# Clip probabilities to avoid infinite log-odds
clipped_test_probability <- pmin(
  pmax(test_probability, 1e-12),
  1 - 1e-12
)

# Estimate the joint calibration intercept and slope
# This is an evaluation step, not a model update
calibration_model <- glm(
  test_data$Risk ~ qlogis(clipped_test_probability),
  family = binomial()
)

calibration_summary <- data.frame(
  Measure = c("Calibration intercept", "Calibration slope"),
  Value = unname(coef(calibration_model))
)

print(calibration_summary)

# Group predictions into fixed probability intervals
probability_bins <- cut(
  test_probability,
  breaks = seq(0, 1, by = 0.1),
  include.lowest = TRUE
)

# Summarise predicted and observed outcomes in non-empty bins
calibration_bins <- do.call(
  rbind,
  lapply(
    split(seq_along(test_probability), probability_bins),
    function(indices) {

      if (length(indices) == 0) {
        return(NULL)
      }

      data.frame(
        Records = length(indices),
        Mean_predicted = mean(test_probability[indices]),
        Observed_proportion = mean(test_data$Risk[indices])
      )
    }
  )
)

print(calibration_bins)

# Draw the descriptive calibration curve
plot(
  calibration_bins$Mean_predicted,
  calibration_bins$Observed_proportion,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "Mean Predicted Probability",
  ylab = "Observed Below-65 Proportion",
  main = "Test-Set Calibration",
  type = "b",
  pch = 16,
  col = "#2A9D8F",
  lwd = 2
)

# Add the ideal calibration line
abline(a = 0, b = 1, lty = 2, col = "grey50")

# Save calibration results
write.csv(
  calibration_summary,
  "results/test_calibration_summary.csv",
  row.names = FALSE
)

write.csv(
  calibration_bins,
  "results/test_calibration_bins.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 15: Evaluate PCA using training predictors only
# --------------------------------------------------

# Exclude both outcomes and the generated record ID
pca_variables <- c(
  "Hours_Studied",
  "Attendance",
  "Sleep_Hours",
  "Previous_Scores",
  "Tutoring_Sessions",
  "Physical_Activity"
)

# Standardise predictors because their scales differ
pca_model <- prcomp(
  train_data[pca_variables],
  center = TRUE,
  scale. = TRUE
)

# Calculate the variance explained by each component
component_variance <- pca_model$sdev^2
variance_proportion <- (
  component_variance / sum(component_variance)
)

pca_summary <- data.frame(
  Component = paste0("PC", seq_along(variance_proportion)),
  Variance_percent = variance_proportion * 100,
  Cumulative_percent = cumsum(variance_proportion) * 100
)

print(pca_summary)

# Find the number of components needed for 95% variance
components_for_95 <- which(
  cumsum(variance_proportion) >= 0.95
)[1]

print(components_for_95)

# Save the PCA summary
write.csv(
  pca_summary,
  "results/pca_variance_summary.csv",
  row.names = FALSE
)


# --------------------------------------------------
# Step 16: Bootstrap uncertainty for test performance
# --------------------------------------------------

# Make bootstrap sampling reproducible
set.seed(162410)

bootstrap_repetitions <- 600

# Resample test records while keeping predictions paired
bootstrap_results <- replicate(
  bootstrap_repetitions,
  {
    sampled_rows <- sample(
      seq_len(nrow(test_data)),
      size = nrow(test_data),
      replace = TRUE
    )

    sampled_actual <- test_data$Risk[sampled_rows]
    sampled_probability <- test_probability[sampled_rows]

    sampled_metrics <- alert_metrics(
      sampled_actual,
      sampled_probability,
      selected_threshold
    )

    c(
      AUC = calculate_auc(
        sampled_actual,
        sampled_probability
      ),
      Recall = sampled_metrics$Recall,
      Precision = sampled_metrics$Precision
    )
  }
)

# Calculate percentile-based 95% confidence intervals
bootstrap_limits <- t(
  apply(
    bootstrap_results,
    1,
    quantile,
    probs = c(0.025, 0.975),
    na.rm = TRUE
  )
)

selected_test_metrics <- alert_metrics(
  test_data$Risk,
  test_probability,
  selected_threshold
)

bootstrap_summary <- data.frame(
  Metric = c("AUC", "Recall", "Precision"),
  Estimate = c(
    calculate_auc(test_data$Risk, test_probability),
    selected_test_metrics$Recall,
    selected_test_metrics$Precision
  ),
  CI_low = bootstrap_limits[, 1],
  CI_high = bootstrap_limits[, 2],
  row.names = NULL
)

print(bootstrap_summary)

# Save the confidence intervals
write.csv(
  bootstrap_summary,
  "results/test_bootstrap_intervals.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 18A: Evaluate performance by income group
# --------------------------------------------------

# Evaluate the frozen model within each test income group
income_performance <- do.call(
  rbind,
  lapply(levels(test_data$Family_Income), function(group_name) {

    group_rows <- which(
      test_data$Family_Income == group_name
    )

    data.frame(
      Income_group = group_name,
      Records = length(group_rows),
      Below_65 = sum(test_data$Risk[group_rows]),
      alert_metrics(
        test_data$Risk[group_rows],
        test_probability[group_rows],
        selected_threshold
      )
    )
  })
)

print(
  income_performance[
    , c("Income_group", "Records", "Below_65",
        "Recall", "Precision", "FP", "FN")
  ]
)

write.csv(
  income_performance,
  "results/test_income_group_performance.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Step 18B: Check sensitivity to influential training rows
# --------------------------------------------------

# Identify potentially influential training records
influential_rows <- cook_values > cook_threshold

# Refit the same formula excluding flagged training rows
# Keep the original full model as the primary model
lm_influence_check <- lm(
  formula(lm_full),
  data = train_data[!influential_rows, ]
)

# Evaluate the sensitivity model on the unchanged test set
influence_predictions <- predict(
  lm_influence_check,
  newdata = test_data
)

influence_comparison <- rbind(
  data.frame(
    Analysis = "Primary full linear model",
    Excluded_training_rows = 0,
    regression_metrics(
      test_data$Exam_Score,
      score_test_predictions
    )
  ),
  data.frame(
    Analysis = "Influence sensitivity analysis",
    Excluded_training_rows = sum(influential_rows),
    regression_metrics(
      test_data$Exam_Score,
      influence_predictions
    )
  )
)

print(influence_comparison)

write.csv(
  influence_comparison,
  "results/linear_influence_sensitivity.csv",
  row.names = FALSE
)

# --------------------------------------------------
# Save and verify the selected prototype
# --------------------------------------------------

# Ensure that the results folder exists
dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

# Bundle the selected models and preprocessing settings
prototype_bundle <- list(
  project = "Student Academic Early Warning System",
  outcome_definition = "Exam_Score < 65",
  selected_score_model_name = selected_score_model,
  score_model = lm_full,
  selected_risk_model_name = selected_risk_model_name,
  risk_model = selected_risk_model,
  risk_predictors = expanded_predictors,
  score_predictors = all_predictors,
  alert_threshold = selected_threshold,
  training_modes = training_modes,
  factor_levels = factor_levels,
  usage = "Demonstration prototype requiring local validation"
)

# Save the bundle to disk
prototype_path <- file.path(
  "results",
  "student_early_warning_prototype.rds"
)

saveRDS(prototype_bundle, file = prototype_path)

# Verify that the file exists and can be read
stopifnot(file.exists(prototype_path))

saved_prototype <- readRDS(prototype_path)

stopifnot(
  isTRUE(all.equal(
    saved_prototype$alert_threshold,
    selected_threshold
  ))
)

# Display the actual saved location
print(normalizePath(prototype_path, winslash = "/"))

cat("Saved prototype verified successfully.\n")

# --------------------------------------------------
# Step 19: Demonstrate scoring with the saved prototype
# --------------------------------------------------

# Load the saved model and preprocessing settings
demo_bundle <- readRDS(
  "results/student_early_warning_prototype.rds"
)

# Select three example records without their outcomes
demo_records <- test_raw[
  1:3,
  demo_bundle$score_predictors,
  drop = FALSE
]

# Apply preprocessing using saved training settings
prepare_saved_data <- function(data, bundle) {

  for (variable in names(bundle$factor_levels)) {

    values <- as.character(data[[variable]])

    unexpected_values <- setdiff(
      unique(values[!is.na(values)]),
      bundle$factor_levels[[variable]]
    )

    if (length(unexpected_values) > 0) {
      stop(paste("Unexpected category in:", variable))
    }

    # Fill missing categories using saved training modes
    values[is.na(values)] <- bundle$training_modes[[variable]]

    # Apply the original factor levels
    data[[variable]] <- factor(
      values,
      levels = bundle$factor_levels[[variable]]
    )
  }

  data
}

demo_ready <- prepare_saved_data(
  demo_records,
  demo_bundle
)

# Predict scores and below-65 probabilities
demo_scores <- predict(
  demo_bundle$score_model,
  newdata = demo_ready
)

demo_probabilities <- predict(
  demo_bundle$risk_model,
  newdata = demo_ready,
  type = "response"
)

# Apply the saved threshold without rounding probabilities
demo_alerts <- (
  demo_probabilities >= demo_bundle$alert_threshold
)

demo_results <- data.frame(
  Record_ID = test_raw$record_id[1:3],
  Predicted_score = as.numeric(demo_scores),
  Below_65_probability = as.numeric(demo_probabilities),
  Decision = ifelse(
    demo_alerts,
    "Teacher review",
    "No model alert"
  )
)

print(demo_results)

# Verify consistency with the original test predictions
stopifnot(
  isTRUE(all.equal(
    as.numeric(demo_probabilities),
    as.numeric(test_probability[1:3])
  ))
)

# Save the demonstration results
write.csv(
  demo_results,
  "results/saved_model_demo.csv",
  row.names = FALSE
)

# Record the analysis environment
capture.output(
  sessionInfo(),
  file = "results/R_session_info.txt"
)

cat("Saved-model scoring demonstration completed successfully.\n")

# --------------------------------------------------
# Step 20: Supporting analyses used in the final report
# --------------------------------------------------
# Keep these checks separate from the frozen primary model and policy.
# Short aliases below map the teaching objects to report table names.
local({
  d <- students_clean
  raw <- students
  raw$record_id <- seq_len(nrow(raw))
  tr <- train_data; va <- validation_data; te <- test_data
  tr0 <- train_raw; va0 <- validation_raw; te0 <- test_raw
  simple <- simple_predictors; expanded <- expanded_predictors
  predictors <- all_predictors
  categorical <- names(factor_levels)
  numeric_vars <- numeric_variables
  g <- selected_risk_model
  g_simple <- logistic_simple; g_expanded <- logistic_expanded
  threshold <- selected_threshold
  tp <- test_probability
  modes <- training_modes
  prepare <- prepare_data
  f <- function(response, vars) reformulate(vars, response)
  reg_metrics <- regression_metrics
  ridge_pred <- function(x, b) predict_ridge(x, b)
  rb <- ridge_coefficients; lambda <- best_lambda
  ym <- training_mean_score
  selected_reg <- selected_score_model
  chosen <- "Expanded"
  out <- "results"
  write_table <- function(x, name) {
    write.csv(x, file.path(out, paste0(name, ".csv")),
              row.names = FALSE, na = "")
  }

  # Preserve audit evidence, including quarantined records and IQR flags.
  write_table(quarantined_records, "quarantined_records")
  missing <- data.frame(
    variable = predictors,
    missing = vapply(raw[predictors], function(x) sum(is.na(x)), integer(1))
  )
  missing$percent <- 100 * missing$missing / nrow(raw)
  write_table(missing, "missing_values")
  audit <- data.frame(
    metric = c("Raw rows", "Columns excluding generated ID", "Exact duplicate rows",
               "Rows with missing values", "Exam scores outside assumed 0 to 100 scale"),
    value = c(nrow(raw), ncol(raw)-1,
              sum(duplicated(students)), sum(!complete.cases(students)),
              nrow(quarantined_records))
  )
  write_table(audit, "data_audit")
desc <- do.call(rbind,lapply(numeric_vars,function(v) {
  x<-d[[v]]; q<-quantile(x,c(.25,.5,.75)); lo<-q[1]-1.5*IQR(x); hi<-q[3]+1.5*IQR(x)
  data.frame(variable=v,n=sum(!is.na(x)),mean=mean(x),sd=sd(x),min=min(x),q1=q[1],median=q[2],q3=q[3],max=max(x),iqr_flags=sum(x<lo|x>hi),row.names=NULL)
}))
write_table(desc,'descriptive_statistics')
cats<-do.call(rbind,lapply(categorical,function(v){t<-table(raw[[v]],useNA='ifany');data.frame(variable=v,category=names(t),n=as.integer(t))}))
write_table(cats,'category_counts')
prevalence<-do.call(rbind,lapply(c(50,60,64,65,66,70),function(t)data.frame(cutoff=t,n=sum(d$Exam_Score<t),total=nrow(d),percent=100*mean(d$Exam_Score<t))))
write_table(prevalence,'outcome_cutoffs')
cors<-data.frame(variable=setdiff(numeric_vars,'Exam_Score'),r=sapply(d[setdiff(numeric_vars,'Exam_Score')],function(x)cor(x,d$Exam_Score)))
write_table(cors,'score_correlations')
nc<-cor(d[setdiff(numeric_vars,'Exam_Score')]);write_table(data.frame(variable=rownames(nc),nc,row.names=NULL),'numeric_predictor_correlations')

# 2. Inference. These analyses are exploratory; associations do not establish causation.
# H0: Mean score is equal in Internet_Access Yes and No groups.
yes<-d$Exam_Score[d$Internet_Access=='Yes']; no<-d$Exam_Score[d$Internet_Access=='No']
tt<-t.test(yes,no,var.equal=FALSE)
sp<-sqrt(((length(yes)-1)*var(yes)+(length(no)-1)*var(no))/(length(yes)+length(no)-2))
hedges_g<-(mean(yes)-mean(no))/sp*(1-3/(4*(length(yes)+length(no))-9))
# H0: All resource-access groups have equal mean scores.
welch<-oneway.test(Exam_Score~Access_to_Resources,data=d,var.equal=FALSE)
am<-aov(Exam_Score~Access_to_Resources,data=d); aa<-summary(am)[[1]]
eta2<-aa[1,'Sum Sq']/sum(aa[,'Sum Sq'])
# H0: Support-risk proportions are equal for attendance below 75 and at least 75.
# Attendance 75 is a pre-specified illustrative policy split, not an optimised cutoff.
low<-d$Attendance<75; xt<-c(sum(d$Risk[low]),sum(d$Risk[!low])); nt<-c(sum(low),sum(!low))
pr<-prop.test(xt,nt,correct=FALSE)
inf<-data.frame(test=c('Welch t test internet Yes minus No','Welch ANOVA resource access','Two proportion test attendance below 75 minus at least 75'),
  statistic=c(unname(tt$statistic),unname(welch$statistic),unname(pr$statistic)),
  df1=c(unname(tt$parameter),welch$parameter[1],unname(pr$parameter)),df2=c(NA,welch$parameter[2],NA),
  p=c(tt$p.value,welch$p.value,pr$p.value),
  effect=c(mean(yes)-mean(no),eta2,diff(rev(xt/nt))),
  ci_low=c(tt$conf.int[1],NA,pr$conf.int[1]),ci_high=c(tt$conf.int[2],NA,pr$conf.int[2]),
  effect_definition=c('Mean score difference','Classical descriptive eta squared','Risk proportion difference'))
inf$p_holm<-p.adjust(inf$p,'holm');write_table(inf,'inference')
write_table(data.frame(hedges_g=hedges_g,yes_n=length(yes),no_n=length(no),yes_mean=mean(yes),no_mean=mean(no)),'internet_effect')
write_table(data.frame(group=c('Attendance <75','Attendance >=75'),n=nt,risk=xt,proportion=xt/nt),'attendance_risk')
pairs<-pairwise.t.test(d$Exam_Score,d$Access_to_Resources,p.adjust.method='holm',pool.sd=FALSE)
write_table(as.data.frame(as.table(pairs$p.value)), 'resource_posthoc_holm')
group_resource<-aggregate(Exam_Score~Access_to_Resources,d,function(x)c(n=length(x),mean=mean(x),sd=sd(x)))
write_table(data.frame(group=group_resource[,1],group_resource[,2]),'resource_group_summary')


  # Reproduce report partition tables from the existing split.
  write_table(data.frame(
    partition = c("train", "validation", "test"),
    n = c(nrow(tr), nrow(va), nrow(te)),
    risk = c(sum(tr$Risk), sum(va$Risk), sum(te$Risk)),
    risk_percent = c(mean(tr$Risk), mean(va$Risk), mean(te$Risk)) * 100
  ), "partitions")
  partition_labels <- rep(NA_character_, nrow(d))
  for (k in names(partition_indices)) partition_labels[partition_indices[[k]]] <- k
  write_table(data.frame(record_id=d$record_id, partition=partition_labels), "partition_membership")
  write_table(data.frame(variable=names(modes),training_mode=unname(modes)), "imputation_values")
  reg_test <- rbind(
    data.frame(model="Training mean baseline",reg_metrics(te$Exam_Score,rep(ym,nrow(te)))),
    data.frame(model="Four variable linear",reg_metrics(te$Exam_Score,predict(lm_simple,te))),
    data.frame(model="Full linear",reg_metrics(te$Exam_Score,predict(lm_full,te))),
    data.frame(model="Full ridge",reg_metrics(te$Exam_Score,ridge_pred(te,rb))))
  write_table(reg_test,"regression_test")
  reg_val <- score_model_comparison
  names(reg_val)[1] <- "model"
  reg_val$model <- c("Training mean baseline", "Four variable linear", "Full linear", "Full ridge")
  write_table(reg_val,"regression_validation")
  robust <- robust_coefficients
  robust$p_HC3 <- 2 * pt(-abs(robust$Estimate / robust$HC3_SE), df.residual(lm_full))
  write_table(robust,"linear_coefficients_HC3")
  write_table(data.frame(term=vif_summary$Predictor,design_column_VIF=vif_summary$VIF),"vif")
  write_table(data.frame(metric=c("Breusch Pagan LM","Breusch Pagan df","Breusch Pagan p",
                                "Jarque Bera","Jarque Bera p","Max design column VIF",
                                "Cook D above 4 over n","Maximum Cook D","Training residual SD"),
                         value=c(bp_statistic,bp_df,bp_p_value,jb_statistic,jb_p_value,
                                 max(vif_values),sum(influential_rows),max(cook_values),sigma(lm_full))),
              "linear_diagnostics")
  write_table(data.frame(analysis="Excluding Cook D above 4/n from training only",
                         excluded_train_n=sum(influential_rows),
                         reg_metrics(te$Exam_Score,influence_predictions)),"influence_sensitivity")

  # Fit the full logistic model as a declared sensitivity model only.
  # It never replaces the operational candidate based on test performance.
  g_full <- glm(f("Risk", predictors), tr, family=binomial())
clip<-function(p)pmin(pmax(p,1e-12),1-1e-12)
auc<-function(y,p){n1<-sum(y==1);n0<-sum(y==0);if(n1==0||n0==0)return(NA_real_);(sum(rank(p)[y==1])-n1*(n1+1)/2)/(n1*n0)}
average_precision<-function(y,p){o<-order(p,decreasing=TRUE); yy<-y[o]; pp<-p[o]; ends<-which(!duplicated(pp,fromLast=TRUE)); tp<-cumsum(yy)[ends];rec<-tp/sum(y);prec<-tp/ends;sum(diff(c(0,rec))*prec)}
class_metrics<-function(y,p,t=.5){pred<-as.integer(p>=t);tp<-sum(pred==1&y==1);fp<-sum(pred==1&y==0);tn<-sum(pred==0&y==0);fn<-sum(pred==0&y==1);pre<-if(tp+fp)tp/(tp+fp) else 0;rec<-tp/(tp+fn)
 data.frame(threshold=t,AUC=auc(y,p),AP=average_precision(y,p),Brier=mean((p-y)^2),log_loss=-mean(y*log(clip(p))+(1-y)*log(1-clip(p))),accuracy=(tp+tn)/length(y),precision=pre,recall=rec,specificity=tn/(tn+fp),F1=if(pre+rec)2*pre*rec/(pre+rec)else 0,TP=tp,FP=fp,TN=tn,FN=fn,alert_n=tp+fp)}
models <- list(Four_variable=g_simple, Expanded=g_expanded, Full_sensitivity=g_full)
write_table(do.call(rbind,lapply(names(models),function(n){m<-models[[n]];data.frame(model=n,converged=m$converged,iterations=m$iter,max_abs_coefficient=max(abs(coef(m))),min_training_probability=min(fitted(m)),max_training_probability=max(fitted(m)))})),'glm_diagnostics')

  cl_val <- do.call(rbind,lapply(names(models),function(n) {
    data.frame(model=n,class_metrics(va$Risk,predict(models[[n]],va,type="response")))
  }))
  cl_test <- do.call(rbind,lapply(names(models),function(n) {
    data.frame(model=n,class_metrics(te$Risk,predict(models[[n]],te,type="response"),
                                  if(n==chosen) threshold else .5))
  }))
  basep <- mean(tr$Risk)
  cl_test <- rbind(data.frame(model="Prevalence_baseline",
                             class_metrics(te$Risk,rep(basep,nrow(te)),.5)),cl_test)
  write_table(cl_val,"classification_validation")
  write_table(cl_test,"classification_test")
  write_table(rbind(data.frame(policy="Default 0.5",class_metrics(te$Risk,tp,.5)),
                    data.frame(policy="Validation recall target",class_metrics(te$Risk,tp,threshold))),
              "threshold_test_comparison")
# Unpenalised expanded GLM coefficients describe training associations.
cg<-summary(g_expanded)$coefficients
or<-data.frame(term=rownames(cg),log_odds=cg[,1],SE=cg[,2],OR=exp(cg[,1]),OR_low=exp(cg[,1]-1.96*cg[,2]),OR_high=exp(cg[,1]+1.96*cg[,2]),p=cg[,4],row.names=NULL)
write_table(or,'logistic_odds_ratios')
lr<-anova(g_simple,g_expanded,test='Chisq');write_table(data.frame(lr),'logistic_nested_LR')
g_square<-update(g_expanded,.~.+I(Hours_Studied^2)+I(Attendance^2)+I(Previous_Scores^2)+I(Tutoring_Sessions^2))
write_table(data.frame(anova(g_expanded,g_square,test='Chisq')),'logistic_nonlinearity_LR')

  cal <- data.frame(metric=c("Calibration intercept","Calibration slope"),value=coef(calibration_model))
  write_table(cal,"test_calibration")
  calibration <- data.frame(n=calibration_bins$Records,
                            predicted=calibration_bins$Mean_predicted,
                            observed=calibration_bins$Observed_proportion)
  write_table(calibration,"calibration_bins")
test_predictions<-data.frame(record_id=te$record_id,Exam_Score=te$Exam_Score,Risk=te$Risk,predicted_risk=tp,alert=as.integer(tp>=threshold),predicted_score=predict(lm_full,te))
write_table(test_predictions,'test_predictions')

# Sampling uncertainty within this dataset. Bootstrap does NOT measure external validity.
set.seed(162410)
boot<-replicate(600,{ii<-sample(seq_along(tp),replace=TRUE);as.numeric(class_metrics(te$Risk[ii],tp[ii],threshold)[1,c('AUC','AP','Brier','precision','recall')])})
ci<-t(apply(boot,1,quantile,c(.025,.975),na.rm=TRUE));write_table(data.frame(metric=c('AUC','AP','Brier','precision','recall'),low=ci[,1],high=ci[,2]),'test_bootstrap_CI')
groups<-do.call(rbind,lapply(c('Gender','Learning_Disabilities','Family_Income'),function(v)do.call(rbind,lapply(levels(te[[v]]),function(l){ii<-te[[v]]==l;data.frame(variable=v,group=l,n=sum(ii),risk_n=sum(te$Risk[ii]),class_metrics(te$Risk[ii],tp[ii],threshold))}))))
write_table(groups,'subgroup_audit')

# 6. Outcome-definition sensitivity. Same partitions, no test-based policy selection.
sensitivity<-do.call(rbind,lapply(c(60,64,65,66),function(cutoff){a<-tr;b<-va;c<-te;a$Risk<-as.integer(a$Exam_Score<cutoff);b$Risk<-as.integer(b$Exam_Score<cutoff);c$Risk<-as.integer(c$Exam_Score<cutoff);m<-glm(formula(g),a,family=binomial());pv<-predict(m,b,type='response');ts<-sort(unique(c(0,pv,1)));rec<-sapply(ts,function(t)mean(pv[b$Risk==1]>=t));t<-max(ts[rec>=.85]);data.frame(cutoff=cutoff,train_events=sum(a$Risk),test_events=sum(c$Risk),class_metrics(c$Risk,predict(m,c,type='response'),t))}))
write_table(sensitivity,'outcome_sensitivity')
# Missing-value strategy sensitivity for expanded GLM on complete-case partitions.
ct<-tr0[complete.cases(tr0[expanded]),];cv<-va0[complete.cases(va0[expanded]),];ce<-te0[complete.cases(te0[expanded]),]
ct<-prepare(ct);cv<-prepare(cv);ce<-prepare(ce);cm<-glm(f('Risk',expanded),ct,family=binomial());cp<-predict(cm,cv,type='response');ts<-sort(unique(c(0,cp,1)));cth<-max(ts[sapply(ts,function(t)mean(cp[cv$Risk==1]>=t))>=.85]);write_table(data.frame(strategy='Complete case expanded',train_n=nrow(ct),test_n=nrow(ce),class_metrics(ce$Risk,predict(cm,ce,type='response'),cth)),'missing_strategy_sensitivity')

# 7. PCA on six NUMERIC PREDICTORS ONLY, standardised using training data.
numeric_predictors<-setdiff(numeric_vars,'Exam_Score');pc<-prcomp(tr[numeric_predictors],center=TRUE,scale.=TRUE)
pvar<-pc$sdev^2/sum(pc$sdev^2);pca<-data.frame(component=paste0('PC',seq_along(pvar)),proportion=pvar,cumulative=cumsum(pvar))
write_table(pca,'pca_variance');write_table(data.frame(variable=rownames(pc$rotation),pc$rotation,row.names=NULL),'pca_loadings')


  # Verify the saved primary bundle again after supporting analyses.
  saved_check <- readRDS("results/student_early_warning_prototype.rds")
  stopifnot(isTRUE(all.equal(as.numeric(predict(saved_check$risk_model,te,type="response")),
                            as.numeric(tp))))
  # Figures below use the same frozen predictions as the report.
  save_plot <- function(name, draw, w=1400, h=900) {
    png(file.path(out,"figures",paste0(name,".png")),width=w,height=h,res=150)
    par(mar=c(5,5,3,1),family="sans",col.axis="#334155",col.lab="#0f172a")
    draw()
    dev.off()
  }
# 9. Reusable figures for report. Native charts in the PPTX use the same CSV values.
save_plot('score_distribution',function(){hist(d$Exam_Score,breaks=seq(54.5,100.5,1),col='#0d9488',border='white',main='Exam score distribution',xlab='Exam score',ylab='Number of records');abline(v=65,col='#be123c',lwd=2,lty=2);legend('topright','Support outcome: score <65',lty=2,col='#be123c',bty='n')})
save_plot('attendance_score',function(){smoothScatter(d$Attendance,d$Exam_Score,xlab='Attendance (%)',ylab='Exam score',main='Attendance and exam score');abline(lm(Exam_Score~Attendance,d),col='#be123c',lwd=2)})
save_plot('resource_boxplot',function(){boxplot(Exam_Score~factor(Access_to_Resources,levels=c('Low','Medium','High')),d,col=c('#cbd5e1','#5eead4','#0d9488'),main='Scores by resource access',xlab='Access to resources',ylab='Exam score')})
save_plot('linear_diagnostics',function(){par(mfrow=c(2,2),mar=c(4,4,3,1));plot(lm_full,which=c(1,2,3,5),ask=FALSE)},1600,1200)
save_plot('roc_curve',function(){o<-order(tp,decreasing=TRUE);tpr<-c(0,cumsum(te$Risk[o])/sum(te$Risk));fpr<-c(0,cumsum(1-te$Risk[o])/sum(1-te$Risk));plot(fpr,tpr,type='s',lwd=3,col='#0d9488',xlab='False positive rate',ylab='Recall',main=sprintf('Test ROC curve  AUC %.3f',auc(te$Risk,tp)));abline(0,1,lty=2,col='gray')})
save_plot('precision_recall',function(){o<-order(tp,decreasing=TRUE);rec<-cumsum(te$Risk[o])/sum(te$Risk);pre<-cumsum(te$Risk[o])/seq_along(o);plot(rec,pre,type='s',col='#0d9488',lwd=2,xlab='Recall',ylab='Precision',main=sprintf('Test precision recall curve  AP %.3f',average_precision(te$Risk,tp)));abline(h=mean(te$Risk),lty=2,col='#be123c')})
save_plot('calibration',function(){plot(calibration$predicted,calibration$observed,xlim=c(0,1),ylim=c(0,1),pch=19,cex=1.3,col='#0d9488',xlab='Mean predicted probability',ylab='Observed support outcome rate',main='Test probability calibration');abline(0,1,lty=2,col='gray');text(calibration$predicted,calibration$observed,labels=calibration$n,pos=3,cex=.8)})
save_plot('pca_scree',function(){barplot(pvar,names.arg=paste0('PC',1:6),col='#0d9488',ylim=c(0,max(pvar)*1.2),ylab='Share of predictor variance',main='PCA on six standardised numeric predictors')})
score_freq<-as.data.frame(table(d$Exam_Score));names(score_freq)<-c('score','n');write_table(score_freq,'score_frequency')
attband<-cut(d$Attendance,c(59,69,79,89,100),labels=c('60 to 69','70 to 79','80 to 89','90 to 100'))
ab<-aggregate(d$Exam_Score,list(attendance=attband),mean);names(ab)[2]<-'mean_score';write_table(ab,'attendance_bands')
summary<-data.frame(metric=c('Selected regression','Ridge lambda','Selected classifier','Probability alert threshold','Outcome score cutoff','Clean rows','Internet Hedges g','PCA components for 95 percent'),value=c(selected_reg,lambda,chosen,threshold,65,nrow(d),hedges_g,which(cumsum(pvar)>=.95)[1]))
write_table(summary,'analysis_summary')
capture.output(sessionInfo(),file=file.path(out,'sessionInfo.txt'))

  cat("\nREPORT SUPPORTING CHECKS COMPLETE\n")
  print(cl_test)
  print(summary)
})
cat("\nCOMPLETE: primary analysis, saved-model demo and report supporting checks.\n")
