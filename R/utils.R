#  create local “misc” package


# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

# Remove NULLs from a list
compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}


#' Show data.frame in excel
#' create temp file
#' @param .data
#'
#' @return  data.frame
#' @export
#'
#' @examples
#'
#' mtcars |>
#'  show_in_excel() |>
#'  dplyr::select(1:2)#'
#'
#'
show_in_excel <- function(.data) {
  if (interactive()) { # avoid unvanted Excel execution
    tmp <- tempfile(fileext = ".csv")
    readr::write_excel_csv(.data,tmp,delim=";")
    fs::file_show(tmp)
    browseURL(tmp)
  }
}
