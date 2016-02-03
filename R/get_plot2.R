#' Prints a graph showing age distributions in each department
#' @examples
#' library
#' data(allprofs)
#' analyse_data(allprofs)
#' get_plot2()
#' @export
get_plot2 <- function(){

  # Uses geom_boxplot from ggplot2 to show the age distribution in each department. The boxes are colored
  # by the division, so that analysis from get_plot1 about the divisions can be continued. The dep_label
  # is a list of abbreviations for the names of the departments, used so that the graph can be drawn nicely.
  # The names are also rotated by 90 degrees so that they can fit in without overlapping.

  ggplot(data_of_profs, aes(department, ages)) + geom_boxplot( aes( fill = division ) ) + scale_x_discrete(
    labels= dep_label) + theme( axis.text.x = element_text(angle=90, vjust=1))

  }
