#' Reads the necessary data and defines variables used in later functions
#' @examples
#' read_data()
#' @export
read_data <- function() {

  allprofs <- read.csv( file.path("data","Data.csv") )

  ages <- 2014 - allprofs$year_of_BA + 22
  allprofs <- cbind(allprofs, ages)

  assign("allprofs", allprofs, envir = .GlobalEnv)

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
#' get_table()
#' @export
get_table <- function(){

  year_order <- order( allprofs$year_of_BA )
  top_five <- year_order[1:5]
  bottom_five <- year_order[387:391]

  assign("table_top", allprofs[top_five, c(2,4,3,7)], envir = .GlobalEnv)
  assign("table_bottom", allprofs[bottom_five, c(2,4,3,7)], envir = .GlobalEnv)

}

#' Prints a histogram with the divisions that the professors belong to
#' @examples
#' get_plot1()
#' @export
get_plot1 <- function(){

  ggplot(allprofs, aes( ages ) ) + geom_histogram(
    binwidth = 1, aes( fill = division ) ) + scale_fill_discrete(labels= div_label)
}

#' Prints a graph showing age distributions in each department
#' @examples
#' get_plot2()
#' @export
get_plot2 <- function(){
  ggplot(allprofs, aes(department, ages)) + geom_boxplot() + scale_x_discrete(labels= dep_label) + theme(
    axis.text.x = element_text(angle=90, vjust=1))
}

#' Prints a scatter plot of ages and the year the professor received the last degree
#' @examples
#' get_plot3()
#' @export
get_plot3 <- function(){
  ggplot(allprofs, aes( last_degree, ages )) + geom_point( aes( color = division ) ) + geom_smooth(
    aes( color= division )) + geom_abline(slope= -1, intercept= 2036 )
}
