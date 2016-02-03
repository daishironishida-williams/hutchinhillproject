#' Reads the necessary data and defines variables used in later functions
#' @examples
#' read_data()
#' @export
analyse_data <- function( profs_data ) {

  data_of_profs <- profs_data

  ages <- 2014 - data_of_profs$year_of_BA + 22
  data_of_profs <- cbind(data_of_profs, ages)

  assign("data_of_profs", data_of_profs, envir = .GlobalEnv)

  mean_age <- mean(ages)
  deviation <- sd(ages)

  assign("mean_age", mean_age, envir = .GlobalEnv)
  assign("deviation", deviation, envir = .GlobalEnv)

  cat("Mean = ", mean_age, "\n")
  cat("Standard Deviation = ", deviation, "\n")


  assign("dep_label", c("AFR", "AMST", "ANTH", "ARAB", "ARTS", "ARTH", "ASTR", "BIOL", "CHEM", "CHIN",
                 "CLAS", "COMP", "CSCI", "DANC", "ECON", "ENGL", "ENVI", "GEOS", "GERM", "HIST",
                 "JAPN", "LATS", "LEAD", "MAST", "MATH", "MUS", "PHIL", "PHED", "PHYS", "PSCI",
                 "PSYC", "REL", "RL", "RUSS", "SOC", "STAT", "THEA", "WGSS"), envir = .GlobalEnv )

  assign("div_label", c("Div. I", "Div. II", "Div. III", "PE"), envir = .GlobalEnv )
}

#' Prints out a table of all professors, ordered by age
#' @examples
#' read_data()
#' get_table()
#' @export
get_table <- function(){

  year_order <- order( data_of_profs$year_of_BA )
  top_five <- year_order[1:5]
  bottom_five <- year_order[387:391]

  assign("table_top", data_of_profs[top_five, c(2,4,3,7)], envir = .GlobalEnv)
  assign("table_bottom", data_of_profs[bottom_five, c(2,4,3,7)], envir = .GlobalEnv)

}

#' Prints the mean age in each division
#' @examples
#' library(ggplot2)
#' read_data()
#' get_mean()
#' @export
get_mean <- function() {

  div1_profs <- data_of_profs[data_of_profs$division == " 1",]
  div2_profs <- data_of_profs[data_of_profs$division == " 2",]
  div3_profs <- data_of_profs[data_of_profs$division == " 3",]
  pe_profs <- data_of_profs[data_of_profs$division == " PE",]

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

#' Prints a histogram with the divisions that the professors belong to
#' @examples
#' library(ggplot2)
#' read_data()
#' get_plot1()
#' @export
get_plot1 <- function(){

  ggplot(data_of_profs, aes( ages ) ) + geom_histogram(
    binwidth = 1, aes( fill = division ) ) + scale_fill_discrete(labels= div_label)
}

#' Prints a graph showing age distributions in each department
#' @examples
#' library(ggplot2)
#' read_data()
#' get_plot2()
#' @export
get_plot2 <- function(){
  ggplot(data_of_profs, aes(department, ages)) + geom_boxplot( aes( fill = division ) ) + scale_x_discrete(
    labels= dep_label) + theme( axis.text.x = element_text(angle=90, vjust=1))
}

#' Prints a scatter plot of ages and the year the professor received the last degree
#' @examples
#' library(ggplot2)
#' read_data()
#' get_plot3()
#' @export
get_plot3 <- function(){
  ggplot(data_of_profs, aes( last_degree, ages )) + geom_point( aes( color = division ) ) + geom_smooth(
    aes( color= division )) + geom_abline(slope= -1, intercept= 2036 )
}
