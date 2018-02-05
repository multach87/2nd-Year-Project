tests.simple <- function(n , dat.name , tests.name) {
       dat.name <- matrix(ncol = 10000 , nrow = n * 2)
       for(i in 1:10000) {
              dat.name[1:n , i] <- rnorm(n)
              dat.name[(n+1):(2*n) , i] <- rnorm(n)
       } 
       tests.name <- matrix(ncol = 9 , nrow = 10000)
       colnames(tests.name) <- c("pair.t.p" , "cliffwmw.p" , "yuen.trim" , "pb.med" , "pair.t.rej" , "cliffwmw.rej" , "yuen.trim.rej" , "pb.med.rej" , "error rate")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(dat.name[1:n , i] , dat.name[(n+1):(2*n) , i] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(dat.name[1:n , i] , dat.name[(n+1):(2*n) , i] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(dat.name[1:n , i] , dat.name[(n+1):(2*n) , i] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(dat.name[1:n , i] , dat.name[(n+1):(2*n) , i] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5:9] <- 0
              if(tests.name[i,1] <= .05) tests.name[i,5] <- 1
              if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,9] <- sum(tests.name[i , 5:8]) / 4
              #cat(tests.name[i , 1:4] , "\n")
              cat(dat.name[3 , i] , "\n")
              cat(dat.name[27 , i] , "\n")
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 9]) , "\n")
       }
}
tests.simple(n = 25 , dat.name = dat.25 , tests.name = tests.25)

tests.simple2 <- function(n , dat.name , tests.name) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:10000){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 25 , nrow = 10000)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "error rate")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:24] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
              }
              #if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              #if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              #if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,25] <- sum(tests.name[i , 5:8]) / 12
              #cat(tests.name[i , 1:4] , "\n")
              #cat(x1[[i]])
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 25]) , "\n")
       }
}
tests.simple2(n = 25 , dat.name = dat.25 , tests.name = tests.25)

tests.simple3 <- function(n , reps = 2000 , dat.name , tests.name , dat.output) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 26 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "error rate" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:26] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
              }
              #if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              #if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              #if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,25] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,26] <- sort(tests.name[i , 1:12])[1]
              #cat(tests.name[i , 1:4] , "\n")
              #cat(x1[[i]])
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 25]) , "\n")
              cat(sort(tests.name[1:i , 26])[(.05*i)] , "\n")
       }
       dat.output <- tests.name
       colnames(dat.output) <- colnames(tests.name)
       return(dat.output)
}
tests.simple3(n = 25 , reps = 2000 , dat.name = dat.25 , tests.name = tests.25)

#saves matrix
tests.simple4 <- function(n , reps = 2000 , dat.name , tests.name , dat.output) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 26 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "error rate" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:26] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
              }
              #if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              #if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              #if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,25] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,26] <- sort(tests.name[i , 1:12])[1]
              #cat(tests.name[i , 1:4] , "\n")
              #cat(x1[[i]])
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 25]) , "\n")
              cat(sort(tests.name[1:i , 26])[(.05*i)] , "\n")
       }
       #dat.output <- tests.name
       #colnames(dat.output) <- colnames(tests.name)
       return(tests.name)
}
test.25.2000 <- tests.simple4(n = 25 , reps = 2000 , dat.name = dat.25 , tests.name = tests.25)

#add benjamini-hochberg
tests.simple5 <- function(n , a = .05 , reps = 2000 , dat.name , tests.name) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 39 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "pair.t13.rej.BH" , "cliffwmw13.rej.BH" , "yuen.trim13.rej.BH" , "pb.med13.rej.BH" ,
                                 "pair.t23.rej.BH" , "cliffwmw23.rej.BH" , "yuen.trim23.rej.BH" , "pb.med23.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:39] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
                     if(sort(tests.name[i, 1:12] , decreasing = TRUE)[k] <= ((12 - k + 1)*a / 12)) tests.name[i , k + 24] <- 1
              }
              tests.name[i,37] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,38] <- sum(tests.name[i , 25:36]) / 12
              tests.name[i,39] <- sort(tests.name[i , 1:12])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 37]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 38]) , "\n")
       }
       return(tests.name)
}

test.25.100.BH <- tests.simple5(n = 25 , reps = 100 , dat.name = dat.25 , tests.name = tests.25)
sort(test.25.100.BH[ , "low.p"])[5]

test.100.100.BH <- tests.simple5(n = 100 , reps = 100 , dat.name = dat.100 , tests.name = tests.100)
sort(test.100.100.BH[ , "low.p"])[5]

#delete extra arguments
tests.simple6 <- function(n , a = .05 , reps = 2000) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 39 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "pair.t13.rej.BH" , "cliffwmw13.rej.BH" , "yuen.trim13.rej.BH" , "pb.med13.rej.BH" ,
                                 "pair.t23.rej.BH" , "cliffwmw23.rej.BH" , "yuen.trim23.rej.BH" , "pb.med23.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:39] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
                     if(sort(tests.name[i, k] , decreasing = TRUE)[k] <= ((12 - k + 1)*a / 12)) tests.name[i , k + 24] <- 1
              }
              tests.name[i,37] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,38] <- sum(tests.name[i , 25:36]) / 12
              tests.name[i,39] <- sort(tests.name[i , 1:12])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 37]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 38]) , "\n")
       }
       return(tests.name)
}

#only 4 tests
tests.simple7 <- function(n , a = .05 , reps = 2000) {
       x1 <- list()
       x2 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 15 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5:15] <- 0
              for (k in 1:4) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+4)] <- 1
                     if(sort(tests.name[i, 1:4] , decreasing = TRUE)[k] <= ((4 - k + 1)*a / 4)) tests.name[i , k + 8] <- 1
              }
              tests.name[i,13] <- sum(tests.name[i , 5:8]) / 4
              tests.name[i,14] <- sum(tests.name[i , 9:12]) / 4
              tests.name[i,15] <- sort(tests.name[i , 1:4])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 13]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 14]) , "\n")
       }
       return(tests.name)
}
test.4.25.100 <- tests.simple7(n = 25 , reps = 100)
sort(test.4.25.100[ , "low.p"])[5]

tests.4.25.2000 <- tests.simple7(n = 25 , reps = 2000)
sort(tests.4.25.2000[ , "low.p"])[5]

tests.4.100.2000 <- tests.simple7(n = 100 , reps = 2000)
sort(tests.4.100.2000[ , "low.p"])[5]

#Fix bh?
tests.simple5B <- function(n , a = .05 , reps = 2000 , dat.name , tests.name) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 39 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "pair.t13.rej.BH" , "cliffwmw13.rej.BH" , "yuen.trim13.rej.BH" , "pb.med13.rej.BH" ,
                                 "pair.t23.rej.BH" , "cliffwmw23.rej.BH" , "yuen.trim23.rej.BH" , "pb.med23.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:39] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
              }
              for(m in 1:12) {
                     sorted <- sort(tests.name[i, 1:12] , decreasing = TRUE)
                     if(sorted[m] <= ((12 - m + 1)*a / 12)) tests.name[i , m + 24] <- 1
              }
              tests.name[i,37] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,38] <- sum(tests.name[i , 25:36]) / 12
              tests.name[i,39] <- sort(tests.name[i , 1:12])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 37]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 38]) , "\n")
       }
       return(tests.name)
}

tests.simple7B <- function(n , a = .05 , reps = 2000) {
       x1 <- list()
       x2 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 15 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5:15] <- 0
              for (k in 1:4) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+4)] <- 1
              }
              for(m in 1:4) {
                     sorted <- sort(tests.name[i, 1:4] , decreasing = TRUE)
                     if(sorted[m] <= ((4 - m + 1)*a / 4)) tests.name[i , m + 8] <- 1
              }
              tests.name[i,13] <- sum(tests.name[i , 5:8]) / 4
              tests.name[i,14] <- sum(tests.name[i , 9:12]) / 4
              tests.name[i,15] <- sort(tests.name[i , 1:4])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 13]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 14]) , "\n")
       }
       return(tests.name)
}

#run tests
tests.4.25.2000 <- tests.simple7(n = 25 , reps = 2000)
tests.12.25.2000 <- tests.simple5(n = 25 , reps = 2000)
tests.4.50.2000 <- tests.simple7(n = 50 , reps = 2000)
tests.12.50.2000 <- tests.simple5(n = 50 , reps = 2000)
tests.4.100.2000 <- tests.simple7(n = 100 , reps = 2000)
tests.12.100.2000 <- tests.simple5(n = 100 , reps = 2000)
tests.4.250.2000 <- tests.simple7(n = 250 , reps = 2000)
tests.12.250.2000 <- tests.simple5(n = 250 , reps = 2000)
tests.4.500.2000 <- tests.simple7(n = 500 , reps = 2000)
tests.12.500.2000 <- tests.simple5(n = 500 , reps = 2000)




#compare results
mean(test.25.100.BH[ , "error rate"])
mean(test.25.100.BH[ , "error rate BH"])
sort(test.25.100.BH[ , "low.p"])[5]
mean(test.100.100.BH[ , "error rate"])
mean(test.100.100.BH[ , "error rate BH"])
sort(test.100.100.BH[ , "low.p"])[5]
mean(test.4.25.100[ , "error rate"])
mean(test.4.25.100[ , "error rate BH"])
sort(test.4.25.100[ , "low.p"])[5]
mean(tests.4.25.2000[ , "error rate"])
mean(tests.4.25.2000[ , "error rate BH"])
sort(tests.4.25.2000[ , "low.p"])[5]
mean(tests.4.100.2000[ , "error rate"])
mean(tests.4.100.2000[ , "error rate BH"])
sort(tests.4.100.2000[ , "low.p"])[5]

#testing save matrix
tests.simple4 <- function(n , reps = 2000 , dat.name , tests.name , dat.output) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 26 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t13.rej" , "cliffwmw13.rej" , "yuen.trim13.rej" , "pb.med13.rej" ,
                                 "pair.t23.rej" , "cliffwmw23.rej" , "yuen.trim23.rej" , "pb.med23.rej" ,
                                 "error rate" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5] <- (t.test(x1[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,6] <- (cidv2(x1[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,7] <- (yuen(x1[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,8] <- (medpb2(x1[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,9] <- (t.test(x2[[i]] , x3[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,10] <- (cidv2(x2[[i]] , x3[[i]] , plotit=F))$p.value
              tests.name[i,11] <- (yuen(x2[[i]] , x3[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,12] <- (medpb2(x2[[i]] , x3[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,13:26] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k+12)] <- 1
              }
              #if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              #if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              #if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,25] <- sum(tests.name[i , 13:24]) / 12
              tests.name[i,26] <- sort(tests.name[i , 1:12])[1]
              #cat(tests.name[i , 1:4] , "\n")
              #cat(x1[[i]])
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 25]) , "\n")
              cat(sort(tests.name[1:i , 26])[(.05*i)] , "\n")
       }
       #dat.output <- tests.name
       #colnames(dat.output) <- colnames(tests.name)
       return(tests.name)
}
test1 <- tests.simple4(n = 25 , reps = 20 , dat.name = dat.25 , tests.name = tests.25 , dat.output = test25.100)
test1[ , "low.p"]

data <- c(5 , 10 , 3 , 13 , 21 , 1)
sort(data[3:6])[1]
sort(data)[6]

#parallel stuff
detectCores()
cl = makeCluster(4)
parSapply(cl, 1:10000 , tests.simple3(n = 25 , reps = 2 , dat.name = dat.25 , tests.name = tests.25))



