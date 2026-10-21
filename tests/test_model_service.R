# Run from the repository root. No Shiny dependency is required.
source("shiny/model_service.R")
source("shiny/demo_profiles.R")
bundle <- load_prototype("shiny/models/student_early_warning_prototype.rds")
review <- score_student_profile(example_review, bundle)
no_alert <- score_student_profile(example_no_alert, bundle)
stopifnot(review$alert, !no_alert$alert,
          abs(review$probability - 0.718461590939934) < 1e-12,
          abs(no_alert$probability - 0.120420871121205) < 1e-12,
          abs(review$threshold - 0.360444876165715) < 1e-12)
for (change in list(list(Attendance = 59), list(Hours_Studied = ""),
                    list(Hours_Studied = "abc"), list(Attendance = Inf),
                    list(Sleep_Hours = 7.5), list(Teacher_Quality = "Unknown"),
                    list(Internet_Access = NA_character_))) {
  invalid <- modifyList(example_review, change)
  stopifnot(inherits(try(score_student_profile(invalid, bundle), silent = TRUE), "try-error"))
}
cat("Model integration checks passed.\n")
