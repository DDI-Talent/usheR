#' Flter admissions xlsx spreadsheat
#'
#' Reads the spreadsheet at the provided path. The spreadsheet should be stored
#' in your personal OneDrive space. The column names are changed to be
#' syntactic. The data are filtered to include only applications to "Data
#' Science for Health and Social Care" programmes (see [](programme_code_lookup).) and only those with a `Qualification Status Decision` equal to "`(SB) Ready for Decision`".
#'
#' No columns are removed, so the function should always work as intended, as long as the `Programme Code` column is correct, and always called `Programme Code`.
#'
#' @param spreadsheet_path Character; Path to admissions spreadsheet
#'
#' @returns Data frame filtered for DSHSC applications. programme applications.
#' @export
#'
#' @examples
#'
#' filter_admissions('~/../OneDrive - University of Edinburgh/.../PG00x PG applicant communications report-online 01-Jan-00.xlsx')
#'
filter_admissions <- function(new_admissions_spreadsheet,
                              actioned_spreadsheet) {

  admissions_data <- readxl::read_xlsx(new_admissions_spreadsheet)

  names(admissions_data) <- gsub('\\W+', '_', tolower(names(admissions_data)))

  in_progress_dshsc <- admissions_data |>
    dplyr::filter(qualification_status_description == '(SB) Ready for Decision') |>
    dplyr::semi_join(programme_code_lookup,
                   by = dplyr::join_by(programme_code)) |>
    dplyr::filter(tolower(status) == 'in progress')

  # already_actioned <-  readxl::read_xlsx(actioned_spreadsheet)
  in_progress_dshsc
}

