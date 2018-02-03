#meh
library(devtools)
install_github("mrxiaohe/WRScpp")
library(WRScpp)
?`WRScpp-package`

source(file.choose())

#dat.25
dat.25 <- matrix(ncol = 10000 , nrow = 50)
for(i in 1:10000) {
       dat.25[1:25 , i] <- rnorm(25)
       dat.25[26:50 , i] <- rnorm(25)
}
View(dat.25)

#loop.25
tests.25 <- matrix(ncol = 9 , nrow = 10000)
colnames(tests.25) <- c("pair.t.p" , "cliffwmw.p" , "yuen.trim" , "pb.med" , "pair.t.rej" , "cliffwmw.rej" , "yuen.trim.rej" , "pb.med.rej" , "error rate")
for(i in 1:nrow(tests.25)) {
       tests.25[i,1] <- (t.test(dat.25[1:25,i] , dat.25[26:50,i] , var.equal=T, conf.level=0.95))$p.value
       tests.25[i,2] <- (cidv2(dat.25[1:25,i] , dat.25[26:50,i] , plotit=F))$p.value
       tests.25[i,3] <- (yuen(dat.25[1:25,i] , dat.25[26:50,i] , alpha=0.05, tr=0.2))$p.value
       tests.25[i,4] <- (medpb2(dat.25[1:25,i] , dat.25[26:50,i] , alpha=0.05, nboot=2000))$p.value
       tests.25[i,5:9] <- 0
       if(tests.25[i,1] <= .05) tests.25[i,5] <- 1
       if(tests.25[i,2] <= .05) tests.25[i,6] <- 1
       if(tests.25[i,3] <= .05) tests.25[i,7] <- 1
       if(tests.25[i,4] <= .05) tests.25[i,8] <- 1
       tests.25[i,9] <- sum(tests.25[i , 5:8]) / 4
       #cat(tests.25[i , 1:4] , "\n")
       cat(dat.25[3 , i] , "\n")
       cat(dat.25[27 , i] , "\n")
       cat("i = " , i, "\n" , "p = ")
       cat(mean(tests.25[1:i , 9]) , "\n")
}

#dat.100
dat.100 <- matrix(ncol = 10000 , nrow = 200)
for(i in 1:10000) {
       dat.100[1:100 , i] <- rnorm(100)
       dat.100[101:200 , i] <- rnorm(100)
}

#loop.100
tests.100 <- matrix(ncol = 9 , nrow = 10000)
colnames(tests.100) <- c("pair.t.p" , "cliffwmw.p" , "yuen.trim" , "pb.med" , "pair.t.rej" , "cliffwmw.rej" , "yuen.trim.rej" , "pb.med.rej" , "error rate")
for(i in 1:nrow(tests.100)) {
       tests.100[i,1] <- (t.test(dat.100[1:100,i] , dat.100[101:200,i] , var.equal=T, conf.level=0.95))$p.value
       tests.100[i,2] <- (cidv2(dat.100[1:100,i] , dat.100[101:200,i] , plotit=F))$p.value
       tests.100[i,3] <- (yuen(dat.100[1:100,i] , dat.100[101:200,i] , alpha=0.05, tr=0.2))$p.value
       tests.100[i,4] <- (medpb2(dat.100[1:100,i] , dat.100[101:200,i] , alpha=0.05, nboot=2000))$p.value
       tests.100[i,5:9] <- 0
       if(tests.100[i,1] <= .05) tests.100[i,5] <- 1
       if(tests.100[i,2] <= .05) tests.100[i,6] <- 1
       if(tests.100[i,3] <= .05) tests.100[i,7] <- 1
       if(tests.100[i,4] <= .05) tests.100[i,8] <- 1
       tests.100[i,9] <- sum(tests.100[i , 5:8]) / 4
       #cat(tests.100[i , 1:4] , "\n")
       #cat(dat.100[3 , i] , "\n")
       #cat(dat.100[27 , i] , "\n")
       cat("i = " , i, "\n" , "p = ")
       cat(mean(tests.100[1:i , 9]) , "\n")
}



#One-sample version????
#create 1 normal 100k population

#tests: (4)
##two-sample t test
##Cliff's WMW (192)
##20% trim: Yuen's Method (166)
##Median: pb method 5.4.2 (185)?

####Vary: Number tests? 
#####Add: 10% trim?
#####Add: bootstrap-t tests?
#####Add: Welch's?
#####Add: compare variances/measures of scale?

##cell [i,j] corresponds to a p-value for whether test j for iteration i incorrectly rejected
###null hypothesis of no difference

#n's = 25 , 50 , 100 , 250 , 500 , 1000 , 2000 , 5000 , 10000
##9 simulations, 9 datasets


#Testing matrix filling
testing <- matrix(ncol = 9 , nrow = 5)
dat.blep <- matrix(ncol = 5 , nrow = 50)
dat.blep
for(i in 1:5) {
       dat.blep[1:25 , i] <- rnorm(25)
       dat.blep[26:50 , i] <- rnorm(25)
}


for(i in 1:5) {
       dat.blep[1:25 , i] <- rnorm(25)
       dat.blep[26:50 , i] <- rnorm(25)
       testing[i,1] <- (t.test(dat.blep[1:25 , i] , dat.blep[2:50 , i] , var.equal=T, conf.level=0.95))$p.value
       testing[i,2] <- (cidv2(dat.blep[1:25 , i] , dat.blep[2:50 , i] , plotit=F))$p.value
       testing[i,3] <- (yuen(dat.blep[1:25 , i] , dat.blep[2:50 , i] , alpha=0.05, tr=0.2))$p.value
       testing[i,4] <- (medpb2(dat.blep[1:25 , i] , dat.blep[2:50 , i] , alpha=0.05, nboot=2000))$p.value
       testing[i,5:9] <- 0
       if(testing[i,1] <= .05) testing[i,5] <- 1
       if(testing[i,2] <= .05) testing[i,6] <- 1
       if(testing[i,3] <= .05) testing[i,7] <- 1
       if(testing[i,4] <= .05) testing[i,8] <- 1
       testing[i,9] <- sum(testing[i,5:8]) / 4
}

dat.blep

testing[1,1] <- (t.test(dat.blep[,1] , dat.blep[,2] , var.equal=T, conf.level=0.95))$p.value
testing[1,2] <- (cidv2(dat.blep[,1] , dat.blep[,2] , plotit=F))$p.value
testing[1,3] <- (yuen(dat.blep[,1] , dat.blep[,2] , alpha=0.05, tr=0.2))$p.value
testing[1,4] <- (medpb2(dat.blep[,1] , dat.blep[,2] , alpha=0.05, nboot=2000))$p.value
testing[1,5:9] <- 0
testing
testing[1,2] <- .049
if(testing[1,1] <= .05) testing[1,5] <- 1
if(testing[1,2] <= .05) testing[1,6] <- 1
if(testing[1,3] <= .05) testing[1,7] <- 1
if(testing[1.4] <= .05) testing[1,8] <- 1
testing[1,9] <- sum(testing[1,5:8]) / 4
testing

