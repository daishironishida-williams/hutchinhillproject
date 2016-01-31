allprofs <- read.csv("Data.csv")

yearorder <- order( allprofs$year_of_grad )
print(allprofs[yearorder,])

departmentorder <- order( allprofs$department )
print(allprofs[departmentorder,])

gradyears <- allprofs$year_of_grad
meangradyear <- mean(gradyears)
meanage <- 2014 - mean(gradyears) + 22
print(meanage)
