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

tests.simple2 <- function(n , dat.name , tests.name) {
       x1 <- list()
       x2 <- list()
       for(j in 1:10000){
              x1[[j]] <- ghdist(n , g=0 , h=0)
              x2[[j]] <- ghdist(n , g=0 , h=0)  
       }
       tests.name <- matrix(ncol = 9 , nrow = 10000)
       colnames(tests.name) <- c("pair.t.p" , "cliffwmw.p" , "yuen.trim" , "pb.med" , "pair.t.rej" , "cliffwmw.rej" , "yuen.trim.rej" , "pb.med.rej" , "error rate")
       for(i in 1:nrow(tests.name)) {
              tests.name[i,1] <- (t.test(x1[[i]] , x2[[i]] , var.equal=T, conf.level=0.95))$p.value
              tests.name[i,2] <- (cidv2(x1[[i]] , x2[[i]] , plotit=F))$p.value
              tests.name[i,3] <- (yuen(x1[[i]] , x2[[i]] , alpha=0.05, tr=0.2))$p.value
              tests.name[i,4] <- (medpb2(x1[[i]] , x2[[i]] , alpha=0.05, nboot=2000))$p.value
              tests.name[i,5:9] <- 0
              if(tests.name[i,1] <= .05) tests.name[i,5] <- 1
              if(tests.name[i,2] <= .05) tests.name[i,6] <- 1
              if(tests.name[i,3] <= .05) tests.name[i,7] <- 1
              if(tests.name[i,4] <= .05) tests.name[i,8] <- 1
              tests.name[i,9] <- sum(tests.name[i , 5:8]) / 4
              #cat(tests.name[i , 1:4] , "\n")
              #cat(x1[[i]])
              cat("i = " , i, "\n" , "p = ")
              cat(mean(tests.name[1:i , 9]) , "\n")
       }
}




tests.simple(n = 25 , dat.name = dat.25 , tests.name = tests.25)
tests.simple2(n = 25 , dat.name = dat.25 , tests.name = tests.25)

