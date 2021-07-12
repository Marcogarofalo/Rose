#' color for html and pdf output
#' function copied from https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html
#' @param x string
#' @param color e.g. "red", "blue"
#' @details  call with inline r
#' @examples `r colorize("$\\alpha$ is red", "red")` and `r colorize("$\\beta$ is blue", "blue")`

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}
colorise <- function(x, color) {
  colorize(x, color)
}
