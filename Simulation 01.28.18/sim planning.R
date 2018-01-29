#meh
library(devtools)
install_github("mrxiaohe/WRScpp")
library(WRScpp)
?`WRScpp-package`

source(file.choose())
ghdist

#One-sample version????
#create 1 normal 100k population

#Create 2 normal 100k populations --> draw 1 sample of size n from each --> check normality
##--> group comparisons --> repeat * 10000

#Vary: Number tests? 

#nrows = 10,000
##for each repetition
#ncols = # of tests
##cell [i,j] corresponds to a binary 0/1 for whether test j for iteration i incorrectly rejected
###null hypothesis of no difference

#n's = 25 , 50 , 100 , 250 , 500 , 1000 , 2000 , 5000 , 10000
##9 simulations, 9 datasets

#tests: two-sample t test, Cliff's WMW, 10% Trim, 20% trim, Median