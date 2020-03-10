#' Apply common style to plots
#'
#' @param subplot Plotly subplot object
#' @param title Character
#'
#' @return
#' @export
#'
#' @examples
style_subplot <- function(subplot, plot_title){

  legend_style <- list(
    "font" = list(
      "family" = "sans-serif",
      "size" = 12,
      "color" = "white"
    )
  )

  plot_title <- list(
    "text" = plot_title,
    "font" = list(
      "family" = "sans-serif",
      "size" = 16,
      "color" = "white"
    )
  )

  plotly::layout(subplot,
    plot_bgcolor = "#252525",
    paper_bgcolor = "#252525",
    xaxis = list("showticklabels" = F, showline = F),
    legend = legend_style, title = plot_title
  )

}

#' Apply common style to individual plots
#'
#' @param plot Plotly object
#' @param axis_title Character
#'
#' @return
#' @export
#'
#' @examples
style_plot <- function(plot, y_axis_title = NULL, x_axis_title = NULL, show_legend = TRUE){

  plotly::layout(plot,
                 showLegend = show_legend,
    yaxis = list("title" = y_axis_title, color = "white"),
    xaxis = list("title" = x_axis_title,  "type" = "category")
  )

}


