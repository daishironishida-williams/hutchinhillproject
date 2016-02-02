#' Reads the necessary data and defines variables used in later functions
#' @examples
#' read_data()
read_data <- function() {

  allprofs <- read.csv( file.path("data","Data.csv") )

  ages <- 2014 - allprofs$year_of_BA + 22
  allprofs <- cbind(allprofs, ages)

  assign("allprofs", allprofs, envir = .GlobalEnv)

  year_order <- order( allprofs$year_of_BA )
  department_order <- order( allprofs$department )

  mean_age <- mean(ages)
  print(mean_age)

  assign("dep_label", c("AFR", "AMST", "ANTH", "ARAB", "ARTS", "ARTH", "ASTR", "BIOL", "CHEM", "CHIN",
                 "CLAS", "COMP", "CSCI", "DANC", "ECON", "ENGL", "ENVI", "GEOS", "GERM", "HIST",
                 "JAPN", "LATS", "LEAD", "MAST", "MATH", "MUS", "PHIL", "PHED", "PHYS", "PSCI",
                 "PSYC", "REL", "RL", "RUSS", "SOC", "STAT", "THEA", "WGSS"), envir = .GlobalEnv )

  assign("div_label", c("Div. I", "Div. II", "Div. III", "PE"), envir = .GlobalEnv )
}

#' Prints a histogram with the divisions that the professors belong to
#' @examples
#' get_plot1()
get_plot1 <- function(){

  ggplot(allprofs, aes( ages ) ) + geom_histogram(
    binwidth = 1, aes( fill = division ) ) + scale_fill_discrete(labels= div_label)
}

#' Prints a graph showing age distributions in each department
#' @examples
#' get_plot2()
get_plot2 <- function(){
  ggplot(allprofs, aes(department, ages)) + geom_boxplot() + scale_x_discrete(labels= dep_label) + theme(
    axis.text.x = element_text(angle=90, vjust=1))
}

#' Prints a scatter plot of ages and the year the professor received the last degree
#' @examples
#' get_plot3()
get_plot3 <- function(){
  ggplot(allprofs, aes( last_degree, ages )) + geom_point( aes( color = division ) ) + geom_smooth(
    aes( color= division )) + geom_abline(slope= -1, intercept= 2036 )
}
