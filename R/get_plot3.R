#' Prints a scatter plot of ages and the year the professor received the last degree
#' @examples
#' library(ggplot2)
#' data(allprofs)
#' analyse_data(allprofs)
#' get_plot3()
#' @export
get_plot3 <- function(){

  # Uses geom_point from ggplot2 to plot the age of each professor, and the year he/she received the
  # last degree. Each point is also colored by the division, and a smooth fit for each division is also
  # drawn. The diagonal line shows professors whose last degree was the BA (then the year the professor
  # received the last degree would correspond to the year the BA was received, which was used to
  # estimate the age)

  ggplot(data_of_profs, aes( last_degree, ages )) + geom_point( aes( color = division ) ) + geom_smooth(
    aes( color= division )) + geom_abline(slope= -1, intercept= 2036 )

}
