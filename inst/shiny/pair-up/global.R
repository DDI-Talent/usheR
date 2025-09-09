create_pair_divs <- function(pairs, gr_num) {
  pairs <- strsplit(pairs, '~~')[[1]]

  g <- htmltools::p(class = 'pair-group', paste('Group', gr_num))
  ul <- htmltools::tags$ul(class = 'individuals', lapply(pairs, tags$li))

  # paste(
  #   htmltools::tags$div(class = 'pair-group-container', g, '\n', ul)
  # ) |>
  #   htmltools::HTML()


  column(
    width = 4,
    class = "col-sm-12 col-md-6 col-lg-4 col-xl-2",  # Responsive breakpoints
    div(
      class = 'pair-group-container',
      #style = 'background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 15px;',
      g, ul
    )
  )
}

prep_for_copy <- function(d) {
  d[d$week != 0, 'name'] |>
    strsplit('~~') |>
    unname() |>
    purrr::imap_chr(
      ~ sprintf('Group %02i\n%s\n', .y, paste0('  - ', .x, collapse = '\n'))
    )
}

validate_class_data <- function(d) {
  names(d) <- tolower(trimws(names(d)))
  validate(need(
    hasName(d, 'name'),
    'Ensure there is a column specifically named `name`'
  ))
  
  required_cols <- c('week', 'session', 'name')
  d <- d[names(d) %in% required_cols]
  if (!hasName(d, 'week')) d$week <- 0
  if (!hasName(d, 'session')) d$session <- NA
  
  d[required_cols]
}

split_data_concerns <- function(d) {
  if (all(d$week == 0)) {
    list(pairs_df = NULL, class_list = d)
  } else {
    list(pairs_df = d[d$week != 0, ], class_list = d[d$week == 0, ])
  }
}

forceReactiveEval <- function(expr) {
  tryCatch({
      force(expr)
      list(truthy = TRUE)
    },
    error = function(e) {
      list(truthy = FALSE, message = conditionMessage(e))
    }
  )
}

validateClassData <- function(file_info, class_list) {
  req(
    file_info(),
    hasName(class_list(), 'name'),
    nrow(class_list()) > 0,
    !any(grepl('~~', class_list()$name))
  )
  TRUE
}


filter_student_selection <- function(input, class_list) {
  filter_by_selection <- if ('everyone' %in% tolower(input$select_students)) {
    TRUE
  } else {
    expr(class_list()$name %in% input$select_students)
  }

  if (isTruthy(input$tgl_present_absent %% 2)) {
    filter_by_selection <- expr(!(!!filter_by_selection))
  }

  class_list() |>
    select(name) |>
    filter(!!filter_by_selection)
}


tgl_select_btn_visibility <- function(input, input_id = 'tgl_present_absent') {
  click_count <- input[[input_id]] %% 2 + 1
  class_in_ex <- c('INcluded', 'EXcluded')
  btn_lab <- sprintf(
    'Students selected below will be <span class=%s>%s</span>',
    class_in_ex[click_count],
    class_in_ex[click_count]
  )

  shinyjs::runjs(sprintf('
    var selectize_el = document.getElementsByClassName("selectize-input")[1];
    selectize_el.classList.remove("%s");
    selectize_el.classList.add("%s");
    ',
    class_in_ex[3 - click_count],
    class_in_ex[click_count]
  ))

  updateActionButton(inputId = input_id, label = btn_lab)
}


tgl_pair_btn_visibility <- function(attendance_data) {
  has_students <- tryCatch({
      length(attendance_data()$available$name) > 0
    },
    error = function(e) FALSE
  )

  if (has_students) {
    shinyjs::enable('btn_pair')
    shinyjs::enable('group_size')
  } else {
    shinyjs::disable('btn_pair')
    shinyjs::disable('group_size')
  }
}
