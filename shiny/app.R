# R Shiny dashboard backed by the actual saved .rds model.
local_library <- file.path(getwd(), '.R_library')
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))
if (!requireNamespace('shiny', quietly = TRUE)) {
  stop('Run setup.R once, then run this app again.')
}
library(shiny)
source('model_service.R', local = TRUE)
source('demo_profiles.R', local = TRUE)
prototype <- load_prototype('models/student_early_warning_prototype.rds')

profile_defaults <- list(
  Hours_Studied = 20, Attendance = 80, Previous_Scores = 75,
  Tutoring_Sessions = 1, Sleep_Hours = 7, Physical_Activity = 3,
  Access_to_Resources = 'Medium', Internet_Access = 'Yes',
  Motivation_Level = 'Medium', Teacher_Quality = 'Medium'
)
profile_fields <- names(profile_defaults)

number_field <- function(id, label) {
  bounds <- profile_numeric_ranges[[id]]
  div(class = 'field',
      numericInput(id, label, value = profile_defaults[[id]],
                   min = bounds[1], max = bounds[2], step = 1, width = '100%'),
      div(class = 'range', paste('Recorded range:', bounds[1], '–', bounds[2])))
}
category_field <- function(id, label) {
  div(class = 'field', selectInput(id, label,
      choices = profile_categories[[id]], selected = profile_defaults[[id]],
      width = '100%', selectize = FALSE))
}
percent_label <- function(p) {
  if (p < .0001) return('<0.01%')
  if (p > .9999) return('>99.99%')
  sprintf('%.2f%%', 100 * p)
}
empty_result <- function() {
  div(class = 'empty',
      div(class = 'empty-mark', '◎', `aria-hidden` = 'true'),
      h3('Ready to check a profile'),
      p('Complete the inputs and select “Check review alert”, or try an example above.'))
}

ui <- fluidPage(
  tags$head(
    tags$title('Student Academic Early Warning — R Shiny'),
    tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'dashboard.css'),
    tags$script(HTML("document.addEventListener('keydown',function(e){if(e.key==='Enter'&&e.target.tagName==='INPUT'){e.preventDefault();document.getElementById('check_profile').click();}});"))
  ),
  tags$header(div(class = 'header-inner',
    div(class = 'brand', 'IT3081 · Statistical Modelling'),
    div(class = 'header-row',
      div(h1('Student Academic Early Warning'),
          p(class = 'subhead', 'A teacher-led academic support demonstration')),
      span(class = 'offline', 'Saved R model connected')))),
  tags$main(
    div(class = 'intro',
      p('Enter a student profile to estimate the below-65 outcome and check whether the model recommends teacher review.'),
      div(class = 'example-actions',
        actionButton('example_review', 'Example: review', class = 'sample'),
        actionButton('example_no_alert', 'Example: no alert', class = 'sample'))),
    div(class = 'layout',
      tags$section(class = 'panel input-panel', `aria-labelledby` = 'profile-heading',
        h2(id = 'profile-heading', 'Student profile'),
        p(class = 'panel-note', 'Use the same measurement units as the project dataset.'),
        div(class = 'section-label', 'Academic records'),
        div(class = 'fields',
          number_field('Hours_Studied', 'Study hours'),
          number_field('Attendance', 'Attendance (%)'),
          number_field('Previous_Scores', 'Previous score'),
          number_field('Tutoring_Sessions', 'Tutoring sessions')),
        div(class = 'section-label', 'Learning context'),
        div(class = 'fields',
          number_field('Sleep_Hours', 'Sleep hours'),
          number_field('Physical_Activity', 'Physical activity'),
          category_field('Access_to_Resources', 'Access to resources'),
          category_field('Internet_Access', 'Internet access'),
          category_field('Motivation_Level', 'Motivation level'),
          category_field('Teacher_Quality', 'Teacher quality')),
        div(class = 'field reference', textInput('student_reference',
            'Student reference (optional)', placeholder = 'e.g. DEMO-01', width = '100%')),
        div(class = 'actions',
          actionButton('check_profile', 'Check review alert', class = 'primary'),
          actionButton('reset_profile', 'Reset', class = 'secondary'))),
      tags$aside(
        tags$section(class = 'panel result-panel', `aria-labelledby` = 'result-heading',
          div(class = 'result-header', h2(id = 'result-heading', 'Review result'),
              div(class = 'small', 'Estimated outcome, followed by a teacher decision')),
          div(class = 'result-content', tagAppendChildren(uiOutput('review_result',
              `aria-live` = 'polite', `aria-atomic` = 'true'), empty_result()))),
        p(class = 'method', strong('Demonstration limits'), br(),
          'Public/synthetic data; performance at your institute is unverified. A model alert is not a failure label. Inputs are processed by this local R session.')))),
  tags$footer(
    span('Alwis A.R.K. · Rajapaksha A.G.S. · Gunarathna G.A.M.A.D.'),
    span('Student Academic Early Warning · Group consultancy project'))
)

server <- function(input, output, session) {
  last_result <- reactiveVal(NULL)
  current_profile <- reactive({
    setNames(lapply(profile_fields, function(name) input[[name]]), profile_fields)
  })

  # Every check calls predict() on the fitted model loaded from the RDS file.
  evaluate_profile <- function(profile) {
    tryCatch({
      prediction <- score_student_profile(profile, prototype)
      last_result(list(profile = profile, prediction = prediction))
    }, error = function(error) {
      last_result(NULL)
      showNotification(conditionMessage(error), type = 'error', duration = 8)
    })
  }

  observeEvent(input$check_profile, {
    evaluate_profile(current_profile())
  }, ignoreInit = TRUE)

  set_profile <- function(profile, reference = '') {
    for (name in names(profile_numeric_ranges)) {
      updateNumericInput(session, name, value = profile[[name]])
    }
    for (name in names(profile_categories)) {
      updateSelectInput(session, name, selected = profile[[name]])
    }
    updateTextInput(session, 'student_reference', value = reference)
  }

  observeEvent(input$example_review, {
    set_profile(example_review, 'DEMO-REVIEW')
    evaluate_profile(example_review)
  }, ignoreInit = TRUE)
  observeEvent(input$example_no_alert, {
    set_profile(example_no_alert, 'DEMO-NO-ALERT')
    evaluate_profile(example_no_alert)
  }, ignoreInit = TRUE)
  observeEvent(input$reset_profile, {
    last_result(NULL)
    set_profile(profile_defaults)
  }, ignoreInit = TRUE)

  output$review_result <- renderUI({
    snapshot <- last_result()
    # Hide any previous prediction while the displayed input values differ.
    if (is.null(snapshot) || !identical(snapshot$profile, current_profile())) {
      return(empty_result())
    }
    scored <- snapshot$prediction
    reference <- trimws(if (is.null(input$student_reference)) '' else input$student_reference)
    threshold_text <- sprintf('%.2f%%', scored$threshold * 100)
    div(class = if (scored$alert) 'review' else 'clear',
      div(class = 'status', if (scored$alert) 'Above the review threshold' else 'Below the review threshold'),
      h3(class = 'result-title', if (scored$alert) 'Teacher review recommended' else 'No model alert'),
      if (nzchar(reference)) p(class = 'reference-line', paste('Student reference:', reference)),
      div(class = 'prob-number', percent_label(scored$probability)),
      p(class = 'prob-label', 'Estimated probability of an exam score below 65'),
      div(class = 'prob-track', role = 'img',
        `aria-label` = paste('Estimated probability', percent_label(scored$probability),
                            'Review threshold', threshold_text),
        div(class = 'prob-fill', style = sprintf('width:%.12f%%;', 100 * scored$probability)),
        span(class = 'threshold-mark', style = sprintf('left:%.12f%%;', 100 * scored$threshold))),
      div(class = 'track-labels', span('0%'), span('100%')),
      p(class = 'threshold-text', 'Review threshold: ', strong(threshold_text)),
      div(class = 'next-step',
        strong(if (scored$alert) 'Suggested next step' else 'Continue routine monitoring'),
        p(if (scored$alert)
            'Discuss the student’s learning context with a teacher. Consider suitable support after that review.'
          else 'The profile does not trigger this model’s alert. Other concerns can still justify teacher review.')),
      p(class = 'result-foot',
        'A score below 65 is an illustrative review outcome. Teacher assessment determines whether support is needed.'))
  })
  outputOptions(output, "review_result", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
