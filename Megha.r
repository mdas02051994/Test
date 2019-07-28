library('readxl')
dat <- read_excel("STR.xlsx")



Nk <- function(dat){
  for(k in 1:nrow(dat)){
    if(dat[k,1]==""||is.na(dat[k,1])){
      dat[k,1] <- dat[k-1,1]
    }
  }
  dat <- dat[!is.na(dat[,2]),]
  N <- length(unique(dat[,1]))
  Nknum <- length(unique(dat[,2]))
  dat <- dat[!duplicated(dat),]
  list(N,t(as.matrix(table(dat[,2]))))
}



em<-function(Nk,N){
  
  # Initial values
  
  eps <- 10^-9
  lt <- 2.5
  lt0 <- 5
  n <- length(Nk)
  pkt <- array(1/n,n)
  t <- 0
  pkt0 <- pkt
  
  while(abs(lt-lt0)+sqrt(sum((pkt-pkt0)^2))>eps && t<100){
    t <- t+1
    pkt0 <- pkt
    lt0 <- lt
    lpkt <- lt*pkt
    Ak <- Nk*lpkt/(1 - exp(-lpkt))
    A <- sum(Ak)
    
    #  new pk's
    pkt <- Ak/A
    
    # new lt
    ltau <- lt
    
    ltau0 <- ltau + 1
    s <- 1
    while(abs(ltau0-ltau)>eps && s<100){
      s <- s+1
      ltau0 <- ltau
      el <- exp(ltau)
      el1 <- el-1
      
      ltau <- ltau - el1*(N*ltau-(1 - 1/el)*A)/(N*(el1-ltau)) 
      #print(ltau)
    }
    lt <- ltau
    print(lt)
  
    
  }
  ml <- sum(Ak*log(lpkt)) - N*log(exp(lt)-1)
  out<-list(ml,pkt,lt,N)
  out
}