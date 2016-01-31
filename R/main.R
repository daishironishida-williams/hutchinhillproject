allprofs <- read.csv("Data.csv")

year_order <- order( allprofs$year_of_grad )
print(allprofs[year_order,])

department_order <- order( allprofs$department )
print(allprofs[department_order,])

grad_years <- allprofs$year_of_grad
mean_grad_year <- mean(grad_years)
mean_age <- 2014 - mean(grad_years) + 22
print(mean_age)
