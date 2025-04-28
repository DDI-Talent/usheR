#' Flter admissions xlsx spreadsheat
#'
#' Reads the spreadsheet at the provided path. The spreadsheet should be stored
#' in your personal OneDrive space. The column names are changed to be
#' syntactic. The data are filtered to include only applications to "Data
#' Science for Health and Social Care" programmes.
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
filter_admissions <- function(spreadsheet_path) {
  admissions_data <- readxl::read_xlsx(spreadsheet_path)
  names(admissions_data) <- gsub('\\s+', '_', tolower(names(admissions_data)))
  dplyr::semi_join(admissions_data, programme_code_lookup,
                   by = dplyr::join_by(programme_code))
}

