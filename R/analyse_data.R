#' Reads the necessary data and defines variables used in later functions
#' @examples
#' data(allprofs)
#' analyse_data(allprofs)
#' @export

# This function takes a data frame as an argument. The data frame should contain information about
# the professor's name, department, division, the year he/she received the BA and the year he\she received
# the last degree.
analyse_data <- function( profs_data ) {

  # The input is assigned to a variable
  data_of_profs <- profs_data

  # The age of each professor is calculated from the year he\she received the BA.
  # The assumption is that all professors received their BA at the age of 22, and the estimated ages are
  # as of the year 2014. The ages are incorporated into the data frame with all the information.
  ages <- 2014 - data_of_profs$year_of_BA + 22
  data_of_profs <- cbind(data_of_profs, ages)

  # The data frame is defined globally so that it can be accessed from the vignette
  assign("data_of_profs", data_of_profs, envir = .GlobalEnv)

  # Now the mean and the standard deviation of all ages are calculated.
  mean_age <- mean(ages)
  deviation <- sd(ages)

  # Again, the variables are defined globally so that they can be accessed from the vignette
  assign("mean_age", mean_age, envir = .GlobalEnv)
  assign("deviation", deviation, envir = .GlobalEnv)

  # The mean and the standard deviation are printed out
  cat("Mean = ", mean_age, "\n")
  cat("Standard Deviation = ", deviation, "\n")

  # This is a list of labels for the names of the departments. These are abbreviations that will be
  # used when the data is plotted on a graph. This is defined globally since it will be used in several
  # different functions
  assign("dep_label", c("AFR", "AMST", "ANTH", "ARAB", "ARTS", "ARTH", "ASTR", "BIOL", "CHEM", "CHIN",
                 "CLAS", "COMP", "CSCI", "DANC", "ECON", "ENGL", "ENVI", "GEOS", "GERM", "HIST",
                 "JAPN", "LATS", "LEAD", "MAST", "MATH", "MUS", "PHIL", "PHED", "PHYS", "PSCI",
                 "PSYC", "REL", "RL", "RUSS", "SOC", "STAT", "THEA", "WGSS"), envir = .GlobalEnv )

  # This is a list of labels for the names of the divisions
  assign("div_label", c("Div. I", "Div. II", "Div. III", "PE"), envir = .GlobalEnv )
}








