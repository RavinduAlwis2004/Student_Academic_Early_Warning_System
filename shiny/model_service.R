# Saved-model prediction service. This file requires base R only.
# No coefficients are copied into the interface and no model is retrained.

profile_numeric_ranges <- list(
  Hours_Studied = c(1, 44), Attendance = c(60, 100),
  Previous_Scores = c(50, 100), Tutoring_Sessions = c(0, 8),
  Sleep_Hours = c(4, 10), Physical_Activity = c(0, 6)
)
profile_categories <- list(
  Access_to_Resources = c("Low", "Medium", "High"),
  Internet_Access = c("No", "Yes"),
  Motivation_Level = c("Low", "Medium", "High"),
  Teacher_Quality = c("Low", "Medium", "High")
)

load_prototype <- function(path) {
  if (!file.exists(path)) stop("Saved model file is missing: ", path)
  bundle <- readRDS(path)
  if (!inherits(bundle$risk_model, "glm") ||
      bundle$risk_model$family$family != "binomial" ||
      bundle$risk_model$family$link != "logit") {
    stop("The saved bundle must contain the fitted binomial logistic model.")
  }
  if (!isTRUE(bundle$risk_model$converged) ||
      any(!is.finite(coef(bundle$risk_model)))) {
    stop("The saved logistic model is not suitable for scoring.")
  }
  threshold <- bundle$alert_threshold
  if (length(threshold) != 1 || !is.finite(threshold) ||
      threshold < 0 || threshold > 1) stop("Invalid saved alert threshold.")
  expected <- c(names(profile_numeric_ranges), names(profile_categories))
  if (!setequal(bundle$risk_predictors, expected)) {
    stop("This dashboard expects the project's expanded ten-input model.")
  }
  bundle
}

prepare_student_profile <- function(profile, bundle) {
  expected <- c(names(profile_numeric_ranges), names(profile_categories))
  if (!all(expected %in% names(profile))) stop("Complete all ten profile inputs.")
  values <- list()
  for (name in names(profile_numeric_ranges)) {
    raw <- profile[[name]]
    if (length(raw) != 1 || is.na(raw) || is.logical(raw) ||
        !nzchar(trimws(as.character(raw)))) {
      stop("Please enter ", gsub("_", " ", name), ".")
    }
    value <- suppressWarnings(as.numeric(raw))
    bounds <- profile_numeric_ranges[[name]]
    if (!is.finite(value) || value < bounds[1] || value > bounds[2] ||
        value != floor(value)) {
      stop(gsub("_", " ", name), " must be a whole number from ",
           bounds[1], " to ", bounds[2], ".")
    }
    values[[name]] <- value
  }
  for (name in names(profile_categories)) {
    raw <- profile[[name]]
    if (length(raw) != 1 || is.na(raw) ||
        !as.character(raw) %in% bundle$factor_levels[[name]]) {
      stop("Please select a valid ", gsub("_", " ", name), ".")
    }
    values[[name]] <- factor(as.character(raw),
                             levels = bundle$factor_levels[[name]])
  }
  as.data.frame(values, check.names = FALSE)
}

score_student_profile <- function(profile, bundle) {
  prepared <- prepare_student_profile(profile, bundle)
  probability <- as.numeric(predict(bundle$risk_model,
                                    newdata = prepared, type = "response"))
  if (length(probability) != 1 || !is.finite(probability) ||
      probability < 0 || probability > 1) stop("Model returned an invalid probability.")
  list(probability = probability, threshold = bundle$alert_threshold,
       alert = probability >= bundle$alert_threshold,
       model = bundle$selected_risk_model_name)
}
