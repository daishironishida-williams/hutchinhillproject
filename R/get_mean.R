#' Prints the mean age in each division
#' @examples
#' library(ggplot2)
#' data(allprofs)
#' analyse_data(allprofs)
#' get_mean()
#' @export
get_mean <- function() {

  # First, a data frame is created for each division; the professors are separated into four data frames
  # according to their division.
  div1_profs <- data_of_profs[data_of_profs$division == " 1",]
  div2_profs <- data_of_profs[data_of_profs$division == " 2",]
  div3_profs <- data_of_profs[data_of_profs$division == " 3",]
  pe_profs <- data_of_profs[data_of_profs$division == " PE",]

  # Now the mean age of each division is calculated. Each variable is defined globally so that it can be
  # accessed from the vignette. Then a line prints out the mean age of the division.

  div1_mean <- mean( div1_profs$ages )
  assign("div1_mean", div1_mean, envir = .GlobalEnv)
  cat("Div.I Mean = ", div1_mean, "\n")

  div2_mean <- mean( div2_profs$ages )
  assign("div2_mean", div2_mean, envir = .GlobalEnv)
  cat("Div.II Mean = ", div2_mean, "\n")

  div3_mean <- mean( div3_profs$ages )
  assign("div3_mean", div3_mean, envir = .GlobalEnv)
  cat("Div.III Mean = ", div3_mean, "\n")

  pe_mean <- mean( pe_profs$ages )
  assign("pe_mean", pe_mean, envir = .GlobalEnv)
  cat("PE Mean = ", pe_mean, "\n")

}
