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
  my.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, n, raw = TRUE))
  sm=summary(my.lm)

  PlotData()
  x=seq(from=0.8,to=1.1,by=0.001)
  y=rep(my.lm$coefficients[[1]],length(x))
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
  }}
  y=exp(y)-1

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
#}








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
PlotLogData=function(){
  plot(Data[[2]],log(seq(2058,1)+1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
  for(i in seq(3,80)){
    lines(Data[[i]],log(append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1]))+1))
  }
}

PlotData2=function(){
  plot(Data2[[11]],seq(1696,1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
  for(i in seq(2,10)){
    lines(Data2[[i]],append(seq(TotalCycles2[i-1],1),rep(0,1696-TotalCycles2[i-1])))
  }
}
PlotLogData2=function(){
  plot(Data2[[11]],log(seq(1696,1)+1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
  for(i in seq(2,10)){
    lines(Data2[[i]],log(append(seq(TotalCycles2[i-1],1),rep(0,1696-TotalCycles2[i-1]))+1))
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
  if (is.na(x)){
    return(0)
  }
  my.lm=lm(log(TransformedData$RUL) ~ poly(TransformedData$Capacity, n, raw = TRUE))
  y=my.lm$coefficients[[1]]
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
    }
  }
  y=exp(y)-1
  return(y)
  
}

my1stDegree.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, 1, raw = TRUE))
my2ndDegree.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, 2, raw = TRUE))
my3rdDegree.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, 3, raw = TRUE))
my4thDegree.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, 4, raw = TRUE))
my5thDegree.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, 5, raw = TRUE))

Poly = function(my.lm,n,x){
  if (is.na(x)){
    return(0)
  }

  y=my.lm$coefficients[[1]]
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
      y=y + my.lm$coefficients[[i]]*x**(i-1)
    }
  }
  y=exp(y)-1
  return(y)
  }
#Plots comparing degrees
#x=c()
#y1=c()
#y2=c()
#for (i in seq(1,100)){
#                 x=append(x,i)
#                 y1=append(y1,NthRegression(i)[2])
#                 y2=append(y2,NthRegression(i)[3])
#}
#plot(x,y1)
#plot(x,y2)

MSEforecast=function(my.lm,n){
  Residuals=c()
  for (i in seq(1,162582)){
    
    Residuals[i] = TransformedData$RUL[i]-Poly(my.lm,n,TransformedData$Capacity[i])
  }
  Residuals=Residuals**2
  return(mean(Residuals))
    
}

  


PlotForecast=function(n){
  my.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, n, raw = TRUE))
  sm=summary(my.lm)
  

  x=seq(from=0.8,to=1.1,by=0.001)
  y=rep(my.lm$coefficients[[1]],length(x))
  for (i in seq(2,n+1)){
    if (is.na(my.lm$coefficients[[i]])==FALSE){
    y=y + my.lm$coefficients[[i]]*x**(i-1)
  }}
  y=exp(y)-1
  
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
  my.lm=lm(log(Transformed$RUL+1) ~ poly(Transformed$Capacity, 1, raw = TRUE))
  return(c(my.lm$coefficients[[1]],my.lm$coefficients[[2]])
)}

Evaluate= function(x,Coeff){
  if (is.na(x)==FALSE){
  return (exp(sum(Coeff[1],x*Coeff[2]))-1)}
  else{
    return(0)
  }
  
}
ListOfIdx=c()
for (i in seq(1,200)){
  ListOfIdx[[i]]=sample(seq(1,79),79,replace=TRUE)
  
}
ListOfCoeff=c()
for (i in seq(1,200)){
 
   ListOfCoeff[[i]]=CoeffIdx(ListOfIdx[[i]])
}

ListOfRUL=c()
for (i in seq(1,200)){


  ListOfRUL[[i]]=1
  for(j in seq(1,162582)){
  
    
      
    
    
    ListOfRUL[[i]][j]=Evaluate(TransformedData$Capacity[j],ListOfCoeff[[i]])
  
  }
  
}
TransformList=c()
for (i in seq(1,162582)){
  TransformList[[i]]=1
  for (j in seq(1,200)){

     TransformList[[i]][j]=Evaluate(TransformedData$Capacity[i],ListOfCoeff[[j]])
  }
  TransformList[[i]]=sort(TransformList[[i]])

}
LowerQuantile=c()
UpperQuantile=c()
for (i in seq(1,162582)){
  LowerQuantile[i]=TransformList[[i]][10]
  UpperQuantile[i]=TransformList[[i]][191]
}
PlotData2()
lines(TransformedData$Capacity,LowerQuantile,col='blue',type='p',cex=0.2)
lines(TransformedData$Capacity,UpperQuantile,col='blue',type='p',cex=0.2)
PlotForecast(1)


CoeffLwr=c(confint(my1stDegree.lm)[1],confint(my1stDegree.lm)[2])
CoeffUpr=c(confint(my1stDegree.lm)[3],confint(my1stDegree.lm)[4])
LwrR=c()
UprR=c()
for (i in seq(1,162582)){
  LwrR[i]=Evaluate(TransformedData$Capacity[i],CoeffLwr)
  UprR[i]=Evaluate(TransformedData$Capacity[i],CoeffUpr)
  if (i%%1000==0){
    print(i)
  }
}
PlotData2()
lines(TransformedData$Capacity,LwrR,col='blue',type='p',cex=0.2)
lines(TransformedData$Capacity,UprR,col='blue',type='p',cex=0.2)
PlotForecast(1)





#Task 4
CostsxD=function(AmountOfTests,MaintenanceTime=200){
Cr = 3
Cm = 0.5
Cp = 1

Battery = sample(seq(2,80),1)
ListOfCosts=c()
for (j in seq(1,AmountOfTests)){
  Costs = 0
  k=1
while(k<=2000){
  i=1
  while (k<=2000){


  if (Data[[Battery]][i]<=0.88){
    Costs = Costs + Cr + Cm + Cp

    Battery = sample(seq(2,80),1)
    k=k+1

    break
  
   
  
  }
  if (i%%MaintenanceTime==0){
    Costs = Costs + Cm


    if (Poly(my1stDegree.lm,1,Data[[Battery]][i])<MaintenanceTime){
      Costs = Costs + Cr
      Battery = sample(seq(2,80),1)
      k=k+1
     
      break
      
    }

  }
  i=i+1
  k=k+1
}

}
  ListOfCosts[j]=Costs
}
print("Mean:")
print(mean(ListOfCosts))
print("sd:")
print(sd(ListOfCosts))
return(ListOfCosts)
}
lol=CostsxD(100)

Means=c()
for (i in seq(50,500)){
  Means[i]=mean(CostsxD(10,i))
  
}
plot(seq(1,500),Means,type='l')




<<<<<<< Updated upstream
=======





# Transformed_Data = c()
# Transformed_Data$Capacity = c()
# Transformed_Data$RUL = c()
# 
# k = 1
# for(i in seq(2,80)){
#   j = 1
#   Transformed_Data$RUL = append(Transformed_Data$RUL, seq(TotalCycles[i-1],1))
#   while(is.na(Data[[i]][j]) == FALSE){
#     print(TRUE)
#     Transformed_Data$Capacity[k] = Data[[i]][j]
#     k = k + 1
#     j = j + 1
#   }
# }
# 
# k = 1
# 
# for(i in seq(2058,1)){
#   
#   for(j in seq(2,80)){
#     
#     if (is.na(Data[[j]][i]) == FALSE){
#       Transformed_Data$RUL[k] = TotalCycles[j-1] - i
#       Transformed_Data$Capacity[k] = Data[[j]][i]
#       k = k + 1
#     }
#   }
# }
# 
# 
# 
# mytest.lm = lm(log(Transformed_Data$RUL+1) ~ poly(Transformed_Data$Capacity, 1, raw = TRUE))
# mytest.lm = lm(Transformed_Data$RUL ~ Transformed_Data$Capacity)
# 
# 
# 
# 
# 
# newx = seq(0.88+0.22/length(Transformed_Data$Capacity),1.1,by=0.22/length(Transformed_Data$Capacity))
# conf_interval = predict(mytest.lm, newdata = data.frame(newx), interval = 'confidence', level = 0.95)
# PlotData()
# lines(newx,conf_interval[,2],col= 'blue')
# lines(newx,exp(conf_interval[,3]),col = 'blue')



x = seq(0.01,3,by=0.01)
y = c()

for (i in seq(0.01,3,by=0.01)){
  y[i*100] = VariableCost(i,50,20)
}
plot(x,y)



Counter=0
for (i in seq(1,162582)){
  
  x=TransformedData$RUL[i]
  if (x != 0){
    if (LowerQuantile[i]>x){
      Counter=Counter+1
    }
    if (UpperQuantile[i]<x){
      Counter=Counter+1
    }
  }
}


>>>>>>> Stashed changes
