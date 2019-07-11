timeseries_plot <- function(df, x = dateTime, y){

  x_var <- enquo(x)
  y_var <- enquo(y)

  p <- df %>%
    ggplot(aes(x = !!x_var, y = !!y_var)) +
    geom_line() +
    bdscale::scale_x_bd(business.dates = df[['dateTime']], max.major.breaks = 10)+
    theme_minimal()

  plotly::ggplotly(p)

}
