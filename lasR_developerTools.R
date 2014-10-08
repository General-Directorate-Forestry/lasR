library(roxygen2)
roxygenize(package.dir="D:\\github\\lasR\\")
library(tools)
resaveRdaFiles("D:\\github\\lasR\\data\\las.rda")


data(las)
#Density metrics
dens <- laser.density(las$dz,las$ID,gtv=2)
#Height distibution metrics
dist <- laser.distribution(las$dz,las$ID,gtv=2)
X <- laser.metrics(las$dz,las$ID,gtv=2)

#Metrics from only first of many and single echoes
first <- laser.metrics(las$dz[las$r==1],las$ID[las$r==1],gtv=2,suffix=".F")
names(first)
#Merics from last of many and single echoes
last <- laser.metrics(las$dz[las$r==las$n],las$ID[las$r==las$n],gtv=2,suffix=".L")
names(last)
