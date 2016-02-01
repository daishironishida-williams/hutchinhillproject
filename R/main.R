allprofs <- read.csv("Data.csv")

year_order <- order( allprofs$year_of_BA )
print(allprofs[year_order,])

department_order <- order( allprofs$department )
print(allprofs[department_order,])

ages <- 2014 - allprofs$year_of_BA + 22

allprofs <- cbind(allprofs, ages)

print(allprofs)

mean_age <- mean(ages)
print(mean_age)

dep_label <- c("AFR", "AMST", "ANTH", "ARAB", "ARTS", "ARTH", "ASTR", "BIOL", "CHEM", "CHIN",
               "CLAS", "COMP", "CSCI", "DANC", "ECON", "ENGL", "ENVI", "GEOS", "GERM", "HIST",
               "JAPN", "LATS", "LEAD", "MAST", "MATH", "MUS", "PHIL", "PHED", "PHYS", "PSCI",
               "PSYC", "REL", "RL", "RUSS", "SOC", "STAT", "THEA", "WGSS")

div_label <- c("Div. I", "Div. II", "Div. III", "PE")

ggplot(allprofs, aes( ages ) ) + geom_histogram(
  binwidth = 1, aes( fill = division ) ) + scale_fill_discrete(labels= div_label)
ggplot(allprofs, aes(department, ages)) + geom_boxplot() + scale_x_discrete(labels= dep_label) + theme(
  axis.text.x = element_text(angle=90, vjust=1))
