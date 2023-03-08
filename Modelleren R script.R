library("readxl")
library("boot")
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
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
  }}


  lines(x,y ,col='red', lwd=2.5)
  
  if (SM==TRUE){
    print(sm)
  }
  return(c(n,mean(sm$residuals**2),median(sm$residuals)))
  
  
}

#fileBlaBla=file("output.txt")
#write('',fileBlaBla)

#for (i in seq(1,100)){
  
  
#  write(NthRegression(i,FALSE),"output.txt",sep='\n',append=TRUE)
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

#Transforming the test data!
TotalCycles2=c()
for(i in seq(2,11)){
  for(j in seq(1,1697)){
    if(is.na(Data2[[i]][j])){
      TotalCycles2=append(TotalCycles2,j-1)
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

PlotData2=function(){
  plot(Data2[[11]],seq(1696,1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
  for(i in seq(2,10)){
    lines(Data2[[i]],append(seq(TotalCycles2[i-1],1),rep(0,1696-TotalCycles2[i-1])))
  }
}

#Making the data 1 big list
TransformedData2=c()
TransformedData2$Capacity=c()
TransformedData2$RUL     =c()
for (i in seq(2,11)){
  TransformedData2$Capacity=append(TransformedData2$Capacity,Data2[[i]])
  TransformedData2$RUL=append(TransformedData2$RUL,append(seq(TotalCycles2[i-1],1),rep(0,1696-TotalCycles2[i-1])))
}

NthRegPoly=function(n,x){
  my.lm=lm(TransformedData$RUL ~ poly(TransformedData$Capacity, n, raw = TRUE))
  y=my.lm$coefficients[[1]]
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
    }
  }
  return(y)
  
}

#Plots comparing degrees
x=c()
y1=c()
y2=c()
for (i in seq(1,100)){
                 x=append(x,i)
                 y1=append(y1,NthRegression(i)[2])
                 y2=append(y2,NthRegression(i)[3])
}
plot(x,y1)
plot(x,y2)

MSEforecast=function(n){
  my.lm=lm(TransformedData$RUL ~ poly(TransformedData$Capacity, n, raw = TRUE))
  Residuals=c()
  for (i in seq(1,16960)){
  
    Residuals = append(Residuals, TransformedData2$RUL[i]-NthRegPoly(n,TransformedData2$Capacity[i]))
  Residuals=Residuals**2
  return(mean(Residuals))
    
  }

  
}

PlotForecast=function(n){
  my.lm=lm(TransformedData$RUL ~ poly(TransformedData$Capacity, n, raw = TRUE))
  sm=summary(my.lm)
  

  x=seq(from=0.8,to=1.1,by=0.001)
  y=rep(my.lm$coefficients[[1]],length(x))
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
  }}
  
  
  lines(x,y ,col='red', lwd=2.5)
}



#Bootstrap 
Transform = function(idx){
  Indices=sort(idx)
  Transformed=c()
  Transformed$RUL=c()
  Transformed$Capacity=c()
  for (i in seq(1,79)){
    Transformed$Capacity=append(Transformed$Capacity,Data[[idx[i]+1]])
    Transformed$RUL=append(Transformed$RUL,append(seq(TotalCycles[idx[i]],1),rep(0,2058-TotalCycles[idx[i]])))
  }
  return (Transformed)
}
CoeffIdx= function(idx){

  Transformed=Transform(idx)
  my.lm=lm(Transformed$RUL ~ poly(Transformed$Capacity, 2, raw = TRUE))
  return(c(my.lm$coefficients[[1]],my.lm$coefficients[[2]],my.lm$coefficients[[3]]))
}

Evaluate= function(x,Coeff){
  return (sum(Coeff[1],x*Coeff[2],x**2*Coeff[3]))
  
}
ListOfIdx=c()
for (i in seq(1,100)){
  ListOfIdx[[i]]=sample(seq(1,79),79,replace=TRUE)
  
}
ListOfCoeff=c()
for (i in seq(1,100)){
 
   ListOfCoeff[[i]]=CoeffIdx(ListOfIdx[[i]])
}

ListOfRUL=c()
for (i in seq(1,100)){


  ListOfRUL[[i]]=1
  for(j in seq(1,162582)){
  
    
      
    
    
    ListOfRUL[[i]][j]=Evaluate(TransformedData$Capacity[j],ListOfCoeff[[i]])
  
  }
  
}

