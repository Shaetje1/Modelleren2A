library("readxl")
Data=read.csv("Battery_train.csv")
x=seq(1,2058)
y=rep(0.88,2058)
plot(x,y,type='l',lty=2, col='red',ylim=c(0.85,1.1),xlab='Cycle #', ylab='Capacity (Ah)')
text(1800,0.85,'Threshold: 0.88',col="red")
for (i in seq(2,80)){
  lines(x,Data[[i]])
  
}

TotalCycles=c()
for(i in seq(2,80)){
  for(j in seq(1,2059)){
    if(is.na(Data[[i]][j])){
      TotalCycles=append(TotalCycles,j-1)
      break
    }
  }
  
}
PlotData=function(){
plot(Data[[2]],seq(2058,1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
for(i in seq(3,80)){
  lines(Data[[i]],append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1])))
}
}



#Making the data 1 big list
TransformedData=c()
TransformedData$Capacity=c()
TransformedData$RUL     =c()
for (i in seq(2,80)){
  TransformedData$Capacity=append(TransformedData$Capacity,Data[[i]])
  TransformedData$RUL=append(TransformedData$RUL,append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1])))
}

NthRegression=function(n,SM=FALSE){
  my.lm=lm(TransformedData$RUL ~ poly(TransformedData$Capacity, n, raw = TRUE))
  sm=summary(my.lm)

  PlotData()
  x=seq(from=0.8,to=1.1,by=0.001)
  y=rep(my.lm$coefficients[[1]],length(x))
  for (i in seq(2,n+1)){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
  }


  lines(x,y ,col='red', lwd=2.5)
  
  if (SM==TRUE){
    print(sm)
  }
  return(c(n,mean(sm$residuals**2),median(sm$residuals)))
  
  
}

fileBlaBla=file("output.txt")
write('',fileBlaBla)
for (i in seq(1,100)){
  
  write(NthRegression(i,FALSE),"output.txt",sep='\n',append=TRUE)
}








#Test data
Data2=read.csv("Battery_test.csv")
x=seq(1,1696)
y=rep(0.88,1696)
plot(x,y,type='l',lty=2,col='red',ylim=c(0.85,1.1),xlab="t (Cycles)", ylab="Capacity (Ah)")
text(1200,0.85,"Threshold: 0.88", col='red')
for(i in seq(2,11)){
  lines(x,Data2[[i]])
}

