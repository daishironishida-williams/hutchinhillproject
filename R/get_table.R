#' Prints out a table of all professors, ordered by age
#' @examples
#' data(allprofs)
#' analyse_data(allprofs)
#' get_table()
#' @export
get_table <- function(){

  # First, all professors are ordered according to the year they received their BA
  year_order <- order( data_of_profs$year_of_BA )

  # A data frame is created for the five oldest members of the faculty.
  # Only five are needed since the paper seeks to find the oldest member.
  top_five <- year_order[1:5]

  # Another data frame for the five youngest members of the faculty.
  bottom_five <- year_order[387:391]

  # Both data frames are now defined globally so that they can be accessed from the vignette.
  assign("table_top", data_of_profs[top_five, c(2,4,3,7)], envir = .GlobalEnv)
  assign("table_bottom", data_of_profs[bottom_five, c(2,4,3,7)], envir = .GlobalEnv)

}
