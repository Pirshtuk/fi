# returns pivoted data.frame for DT

#

# DT with child rows ----------------------------------------------------------------------

#' datatable with child rows
#'
#' @param x
#' @param vars столбцы для child rows
#' @param opts
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
datatable2 <- function(x, vars = NULL, opts = NULL, ...) {

  names_x <- names(x)
  if (is.null(vars)) stop("'vars' must be specified!")
  pos <- match(vars, names_x)
  if (any(purrr::map_chr(x[, pos], typeof) == "list"))
    stop("list columns are not supported in datatable2()")

  pos <- pos[pos <= ncol(x)] + 1
  rownames(x) <- NULL
  if (nrow(x) > 0) x <- cbind(' ' = '&oplus;', x)

  # options
  opts <- c(
    opts,
    list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, pos)),
        list(orderable = FALSE, className = 'details-control', targets = 1),
        list(className = 'dt-left', targets = 1:3),
        list(className = 'dt-right', targets = 4:ncol(x))
      )
    ))

  DT::datatable(
    x,
    ...,
    escape = -2,
    options = opts,
    callback = DT::JS(.callback2(x = x, pos = c(0, pos)))
  )
}

#' Внутренняя
#'
#' @param x
#' @param pos
#'
#' @return
#'
#' @examples
.callback2 <- function(x, pos = NULL) {

  part1 <- "table.column(1).nodes().to$().css({cursor: 'pointer'});"

  part2 <- .child_row_table2(x, pos = pos)

  part3 <-
    "
   table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&ominus;');
    }
  });"

  paste(part1, part2, part3)
}

#' Внутренняя
#'
#' @param x
#' @param pos
#'
#' @return
#'
#' @examples
.child_row_table2 <- function(x, pos = NULL) {

  names_x <- paste0(names(x), ":")
  text <- "
  var format = function(d) {
    text = '<div><table >' +
  "

  for (i in seq_along(pos)) {
    text <- paste(text, glue::glue(
      "'<tr>' +
          '<td>' + '{names_x[pos[i]]}' + '</td>' +
          '<td>' + d[{pos[i]}] + '</td>' +
        '</tr>' + " ))
  }

  paste0(text,
         "'</table></div>'
      return text;};"
  )
}
library(purrr)
datatable2_test <- function(x) {
  datatable2(
    x = coa,
    vars = c("source_id", "is_quantity", "is_currency"),
    opts = list(pageLength = 5)
  )
}

# Возвращает объект DT, готовый для отображения -------------------------------

# поиск, перемещение столбцов, кнопки, заголовок

#' Заполнить таблицу DT данными в колонках
#'
#'
#' @param tbl таблица с колонками to_date,forecast,diff
#' @param dt_caption
#'
#' @return
#' @export
#'
#' @examples
#'  # dt |> showDT('')
showDT <- function(tbl, dt_caption) {
  tbl |>
    rename('Факт'=revenue_cum,
           "План"=forecast_cum,
           "Откл"=diff,
           'Рекламная кампания'=campaign_name) |>
    pivot_wider(names_from = life_day,values_from = c(Факт,План,Откл)) -> df_wide
  df_wide |>
    #filter(to_date_7>0.01) %>%
    datatable(
      rownames = F,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        dt_caption),
      filter = list(position = 'top', clear = FALSE),
      extensions = c('Buttons','ColReorder'),
      options = list(
        #  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Russian.json'),
        colReorder = TRUE,
        scrollX = TRUE,
        scrollY = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'excel'),
        initComplete = JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Calibri'});",
          "}"
        )
      )) |>
    formatStyle(grep('План',colnames(df_wide)),backgroundColor='Beige') |>
    formatStyle(grep('Откл',colnames(df_wide)),backgroundColor='Azure')
}


#' Заполнить таблицу DT данными в колонках с названиями ...
#'
#' @param tbl
#' @param dt_caption
#'
#' @return
#' @export
#'
#' @examples
#' # dt |> showDT_avg('')
showDT_avg <- function(tbl, dt_caption) {
  tbl |>rename(revenue_cum=rev_user_cum,
               forecast_cum=forecast_user_cum,
               diff=diff_user) |>
    showDT(dt_caption)
}
