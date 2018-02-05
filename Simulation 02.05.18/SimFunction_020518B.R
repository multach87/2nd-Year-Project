source(file.choose())

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

#Fix bh?
tests.12.BH <- function(n , a = .05 , reps = 2000 , dat.name , tests.name) {
       x1 <- list()
       x2 <- list()
       x3 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
              x3[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 51 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t13.p" , "cliffwmw13.p" , "yuen.trim13.p" , "pb.med13.p" ,
                                 "pair.t23.p" , "cliffwmw23.p" , "yuen.trim23.p" , "pb.med23.p" ,
                                 "pair.t12.p.BH" , "cliffwmw12.p.BH" , "yuen.trim12.p.BH" , "pb.med12.p.BH" ,
                                 "pair.t13.p.BH" , "cliffwmw13.p.BH" , "yuen.trim13.p.BH" , "pb.med13.p.BH" ,
                                 "pair.t23.p.BH" , "cliffwmw23.p.BH" , "yuen.trim23.p.BH" , "pb.med23.p.BH" ,
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
              tests.name[i,13:24] <- p.adjust(tests.name[i , 1:12] , method = "BH")
              tests.name[i , 25:51] <- 0
              for (k in 1:12) {
                     if(tests.name[i,k] <= .05) tests.name[i,(k + 24)] <- 1
              }
              for(m in 13:24) {
                     if(tests.name[i , m] <= .05) tests.name[i , m + 24] <- 1
              }
              tests.name[i,49] <- sum(tests.name[i , 25:36]) / 12
              tests.name[i,50] <- sum(tests.name[i , 37:48]) / 12
              tests.name[i,51] <- sort(tests.name[i , 1:12])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 49]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 50]) , "\n")
       }
       return(tests.name)
}

tests.4.BH <- function(n , a = .05 , reps = 2000) {
       x1 <- list()
       x2 <- list()
       for(j in 1:reps){
              x1[[j]] <- ghdist(n , g = 0 , h = 0)
              x2[[j]] <- ghdist(n , g = 0 , h = 0)
       }
       tests.name <- matrix(ncol = 19 , nrow = reps)
       colnames(tests.name) <- c("pair.t12.p" , "cliffwmw12.p" , "yuen.trim12.p" , "pb.med12.p" ,
                                 "pair.t12.p.BH" , "cliffwmw12.p.BH" , "yuen.trim12.p.BH" , "pb.med12.p.BH" ,
                                 "pair.t12.rej" , "cliffwmw12.rej" , "yuen.trim12.rej" , "pb.med12.rej" ,
                                 "pair.t12.rej.BH" , "cliffwmw12.rej.BH" , "yuen.trim12.rej.BH" , "pb.med12.rej.BH" ,
                                 "error rate" , "error rate BH" , "low.p")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5:8] <- p.adjust(tests.name[i , 1:4] , method = "BH")
              tests.name[i,9:19] <- 0
              for (k in 1:4) {
                     if(tests.name[i , k] <= .05) tests.name[i,(k + 8)] <- 1
              }
              for(m in 5:8) {
                     if(tests.name[i , m] <= .05) tests.name[i , m + 8] <- 1
              }
              tests.name[i,17] <- sum(tests.name[i , 9:12]) / 4
              tests.name[i,18] <- sum(tests.name[i , 13:16]) / 4
              tests.name[i,19] <- sort(tests.name[i , 1:4])[1]
              cat("i = " , i, "\n")
              cat("error = " , mean(tests.name[1:i , 17]) , "\n")
              cat("BH error = " , mean(tests.name[1:i , 18]) , "\n")
       }
       return(tests.name)
}



tests.4.25.2000B <- tests.4.BH(n = 25 , reps = 2000)
tests.12.25.2000B <- tests.12.BH(n = 25 , reps = 2000)
tests.4.50.2000B <- tests.4.BH(n = 50 , reps = 2000)
tests.12.50.2000B <- tests.12.BH(n = 50 , reps = 2000)
tests.4.100.2000B <- tests.4.BH(n = 100 , reps = 2000)
tests.12.100.2000B <- tests.12.BH(n = 100 , reps = 2000)
tests.4.250.2000B <- tests.4.BH(n = 250 , reps = 2000)
tests.12.250.2000B <- tests.12.BH(n = 250 , reps = 2000)
tests.4.500.2000B <- tests.4.BH(n = 500 , reps = 2000)
tests.12.500.2000B <- tests.12.BH(n = 500 , reps = 2000)

#old tests
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



#testing new bh
tests <- c(0.1 , 0.032 , .310 , 0.101 , 0.23 , 0.0012 , 0.0513)
p.adjust(tests , method = "BH")
mtx <- matrix(nrow = 2 , ncol = 8)
mtx[ , ] <- 0
mtx[1 , 2:8] <-p.adjust(tests , method = "BH")
mtx


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

