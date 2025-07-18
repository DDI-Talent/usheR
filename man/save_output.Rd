% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attendance_function.R
\name{save_output}
\alias{save_output}
\title{Save a data frame to CSV}
\usage{
save_output(dataframe, file_path)
}
\arguments{
\item{dataframe}{A data frame to be saved.}

\item{file_path}{A character string specifying the CSV file path.}
}
\value{
Returns the (optionally saved) data frame.
}
\description{
This helper function saves a data frame to a specified \code{.csv} file path.
If the file already exists, the new data is automatically appended by merging
on the \code{name} column. This allows multiple attendance sessions to be saved
into a single growing log file. If \code{file_path} is \code{NULL}, the data frame is
returned without saving.
}
\examples{
# Load USJudgeRatings dataset and use as full class list
class_list <- data.frame(id = 1:nrow(USJudgeRatings),
                         name = rownames(USJudgeRatings))

# print full class list to view row indices
print(class_list$name)

# Below is an example for saving
\dontrun{
# Week 1: take attendance
attendance <- take_attendance(full_class = class_list,
                         present_students = c(1, 2, 4),
                         file_path = "attendance.csv")  # saved using save_output()

# Week 2: take attendance and append to same file
attendance <- take_attendance(full_class = class_list,
                         present_students = c(1, 3, 4, 5, 7, 10, 15, 17),
                         file_path = "attendance.csv")

}
}
