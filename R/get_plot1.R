#' Prints a histogram with the divisions that the professors belong to
#' @examples
#' library(ggplot2)
#' data(allprofs)
#' analyse_data(allprofs)
#' get_plot1()
#' @export
get_plot1 <- function(){

  # Uses geom_histogram from ggplot2 to create a histogram of the age distribution. Each professor is also
  # colored by the division, so a comparison among different divisions can be made. The binwidth is set to 1
  # so that a bar is used for one age. The div_label is simply a list of abbreviations for the names of the
  # division, used for the legend.

  ggplot(data_of_profs, aes( ages ) ) + geom_histogram(
    binwidth = 1, aes( fill = division ) ) + scale_fill_discrete(labels= div_label)

  }
