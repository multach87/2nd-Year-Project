#meh
library(devtools)
install_github("mrxiaohe/WRScpp")
library(WRScpp)
?`WRScpp-package`

source(file.choose())
ghdist


#Create 2 normal populations --> draw 1 sample of size n from each --> check normality
##--> group comparisons --> repeat * 10000

#Create 1 normal population --> draw 2 samples of size n from each --> check normality
##--> group comparisons --> repeat * 10000