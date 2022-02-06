# set of common visualization function “primitives”
# returns ggplot

my_org_theme <- function(variables) {

}

#'  basic visualization function that wraps ggplot2 code
#'  but allows users to input their preferred x-axis, y-axis and grouping variables to draw cohort curves.
#'
#' @param data
#' @param time
#' @param metric
#' @param group
#'
#' @return
#' @export
#'
#' @examples viz_cohort(my_org_data)
#'           viz_cohort(my_org_data, color = COHORT, linetype = COHORT)
#'
#'           my_org_data %>%
#'             mutate(COHORT_QUARTER = quarter(DATE_COHORT_START)) %>%
#'             viz_cohort(my_org_data) +
#'             facet_grid(rows = vars(COHORT_QUARTER))


viz_cohort <- function(data,
                       metric = "IND_ACTIVE",
                       time = "MONTHS_SUBSCRIBED",
                       group = "COHORT",
                       ...) {
  .data <- NULL

  gg <-
    ggplot2::ggplot(data) +
    ggplot2::aes(x = .data[[time]],
        y = .data[[metric]],
        group = .data[[group]]) +
    ggplot2::geom_line(...) +
    my_org_theme()

  return(gg)

}



#' Returns ggproto objects in a list and that list plays nicely with +.
#' data %>%
#'    ggplot(aes(blah blah)) +
#'    geom_something_else() +
#'    layer_cohort(data = other_data, tweak1, tweak2)
#'
#' @param data
#' @param time
#' @param metric
#' @param group
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

layer_cohort <- function(data,
                         time = "MONTHS_SUBSCRIBED",
                         metric = "IND_ACTIVE",
                         group = "COHORT",
                         ...) {
  .data <- NULL

  list(
    ggplot2::aes(x = .data[[time]],
        y = .data[[metric]],
        group = .data[[group]]),
    ggplot2::geom_line(ggplot2::aes(...)),
    my_org_theme()
  )
}


#' catch potential errors caused by missing keywords
#'
#' @param vbl_names
#'
#' @return
#' @export
#'
#' @examples
validate_mypackage_data <- function(vbl_names) {

  # validate variable names ----
  required <- c("TIME_SUBSCRIBED", "CUSTOMER_COHORT", "CUSTOMER_SEGMENT")
  not_present <- setdiff(required, vbl_names)

  # report missing / required names ----
  if (length(not_present) == 0) {return(TRUE)}
  message("The following required variable(s) are missing: ",
          paste(not_present, collapse = ", "))
  return(FALSE)

}

#' Статичный график двух линий ggplot2. Например: Органик/Неорганик.
#'
#' @param dt
#' @param x
#' @param y
#' @param z
#' @param title
#' @param title_x
#'
#' @return
#' @export
#'
#' @examples
viz_lines <- function(dt, x, y, z,title="",title_x="") {
  dt |>
    ggplot() +
    aes(x = x, y = y, fill = z) +
    geom_area(size = 0.5) +
    scale_fill_hue(direction = 1) +
    labs(
      y = "",
      x = title_x,
      title = title,
      fill=""
    ) +
    theme_classic() +
    theme(legend.position = "top")
}

#' Статичный график двух и более переменных (столбики ggplot)
#'
#' @param dt
#' @param x
#' @param y
#' @param z
#'
#' @return
#' @export
#'
#' @examples
viz_bar <- function(dt, x, y, z) {
  dt  |>
    ggplot() +
    aes(x = x, fill = z, weight = y) +
    geom_bar() +
    labs(y = "",x =
           #"Дата выручки (показа рекламы)"
           "Revenue date (impression showed)"
         ,fill = " ")+
    scale_fill_hue(direction = 1) +
    theme_minimal()
}

# возвращает echarts - для интерактивных визуализаций ---------------------

#' Интерактивный график двух линий Органик/Неорганик, e_charts, JavaScript
#'
#'
#' @param dt - табличка с легендой из group_by
#' @param x - название столбца дат
#' @param y - название столбца выручка
#' @param title
#' @param title_x
#'
#' @return
#' @export
#'
#' @examples
viz_lines_js <- function(dt, x, y, title="",title_x="") {
  #print(deparse(substitute(x)))
  dt |>
    e_charts_(x) |>
    e_line_(y,symbol ="none") |>
    e_datazoom(type = "slider") |>
    e_tooltip(trigger = "axis") |>
    e_title(title, "USD")
}

viz_lines_wide_js <- function(dt, x, y1,y2, title="",title_x="") {
  #print(deparse(substitute(x)))
  dt |>
    e_charts_(x) |>
    e_line_(y1,symbol ="none") |>
    e_line_(y2,symbol ="none") |>
    e_datazoom(type = "slider") |>
    e_tooltip(trigger = "axis") |>
    e_title(title, "USD")
}

#' Vizualisation with JavaScript: 4 lines from wide table with 4 columns for each measure
#'
#' @param dt
#' @param x  X axis
#' @param y1 column name for first measure
#' @param y2
#' @param y3
#' @param y4
#' @param title title for Vizualisation
#' @param title_x title for X axis
#'
#' @return
#' @export
#'
#' @examples
viz_lines_wide4_js <- function(dt, x, y1,y2,y3,y4, title="",title_x="") {
  #print(deparse(substitute(x)))
  dt |>
    echarts4r::e_charts_(x) |>
    echarts4r::e_line_(y1,symbol ="none") |>
    echarts4r::e_line_(y2,symbol ="none") |>
    echarts4r::e_line_(y3,symbol ="none") |>
    echarts4r::e_line_(y4,symbol ="none") |>
    echarts4r::e_datazoom(type = "slider") |>
    echarts4r::e_tooltip(trigger = "axis") |>
    echarts4r::e_title(title, "USD")
}

#' График факта, прогноза и прогнозного интервала
#'
#' @param dt
#' @param x
#' @param y1
#' @param y2
#' @param lwr
#' @param upr
#' @param title
#' @param title_x
#'
#' @return
#' @export
#'
#' @examples
viz_lines_bands_js <- function(dt, x, y1,y2,lwr,upr, title="",title_x="") {
  dt |>
    e_charts_(x) |>
    e_line_(y1,symbol ="none") |>
    e_line_(y2,symbol ="none") |>
    e_band_(lwr, upr) |>
    e_datazoom(type = "slider") |>
    e_tooltip(trigger = "axis") |>
    e_title(title, "USD")
}
