# Install Shiny from CRAN into a project-local library when needed.
app_library <- file.path(getwd(), ".R_library")
dir.create(app_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(normalizePath(app_library, winslash = "/"), .libPaths()))
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", lib = app_library,
                   repos = "https://cloud.r-project.org")
}
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Shiny setup did not complete. Review the installation output.")
}
cat("Shiny is ready.\n")
