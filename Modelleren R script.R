library("readxl")
library("boot")

#Obtain and plot the data in its most basic form
Data=read.csv("Battery_train.csv")
x=seq(1,2058)
y=rep(0.88,2058)
plot(x,y,type='l',lty=2, col='red',ylim=c(0.85,1.1),xlab='Cycle #', ylab='Capacity (Ah)')
text(1800,0.85,'Threshold: 0.88',col="red")
for (i in seq(2,80)){
  lines(x,Data[[i]])
  
}

#For each battery, calculate the total amount of cycles it has (usefull for calculating RUL)
TotalCycles=c()
for(i in seq(2,80)){
  for(j in seq(1,2059)){
    if(is.na(Data[[i]][j])){
      TotalCycles=append(TotalCycles,j-1)
      break
    }
  }
  
}




#Transforming the data in capacity and RUL, and putting it all in 1 big list, no longer seperated by battery
TransformedData=c()
TransformedData$Capacity=c()
TransformedData$RUL     =c()
for (i in seq(2,80)){
  TransformedData$Capacity=append(TransformedData$Capacity,Data[[i]])
  TransformedData$RUL=append(TransformedData$RUL,append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1])))
}



#Function which takes in an N, performs an nth order regression based on the log of the RUL, then returns the coefficients of the polynomial 
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
  return(my.lm$coefficients)
  
  
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


#Some convenient functions to quickly make plots of data

PlotData=function(){
  plot(Data[[2]],seq(2058,1),type='l', xlab='Capacity (Ah)',ylab="RUL (Cycles)",xlim=c(0.86999,1.1))
  for(i in seq(3,80)){
    lines(Data[[i]],append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1])))
  }
}
PlotLogData=function(){
  plot(Data[[2]],log(seq(2058,1)+1),type='l', xlab='Capacity (Ah)',ylab="Log(RUL (Cycles)+1)",xlim=c(0.86999,1.1))
  for(i in seq(3,80)){
    lines(Data[[i]],log(append(seq(TotalCycles[i-1],1),rep(0,2058-TotalCycles[i-1]))+1))
  }
}

PlotData2=function(){
  plot(Data2[[11]],seq(1696,1),type='l', xlab='Capacity (Ah)',ylab="Log(RUL (Cycles)+1)",xlim=c(0.86999,1.1))
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

#Transforming the test data into RUL and Capacity, and making it into 1 list, no longer seperated by batteries
TransformedData2=c()
TransformedData2$Capacity=c()
TransformedData2$RUL     =c()
for (i in seq(2,11)){
  TransformedData2$Capacity=append(TransformedData2$Capacity,Data2[[i]])
  TransformedData2$RUL=append(TransformedData2$RUL,append(seq(TotalCycles2[i-1],1),rep(0,1696-TotalCycles2[i-1])))
}

#A function which evaluates the polynomial of the nth order regression at point x
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

#A function which evaluates the polynomial of the nth order regression at point x, but with the regression pre-loaded, for easier computations
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

#Calculating the MSE for any nth order regression based on the log of RUL
MSEforecast=function(my.lm,n){
  Residuals=c()
  for (i in seq(1,162582)){
    
    Residuals[i] = TransformedData$RUL[i]-Poly(my.lm,n,TransformedData$Capacity[i])
  }
  Residuals=Residuals**2
  return(mean(Residuals))
  
}



#Convenient function to plot the forecast
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

PlotLogForecast=function(n){
  my.lm=lm(log(TransformedData$RUL+1) ~ poly(TransformedData$Capacity, n, raw = TRUE))
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

#Function that takes as input a list of indices, then returns dataset using only the batteries with those indices
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

#Returns the coeff of the 1st degree model for any given set of batteries
CoeffIdx= function(idx){
  
  Transformed=Transform(idx)
  my.lm=lm(log(Transformed$RUL+1) ~ poly(Transformed$Capacity, 1, raw = TRUE))
  return(c(my.lm$coefficients[[1]],my.lm$coefficients[[2]])
  )}

#Evaluates the e power, given x coordinate and coefficients
Evaluate= function(x,Coeff){
  if (is.na(x)==FALSE){
    return (exp(sum(Coeff[1],x*Coeff[2]))-1)}
  else{
    return(0)
  }
  
}
#Performs the bootstrap
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
#After sorting the final product, we simply take the empirical 5th and 96th quantiles
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


#Use R-'s confint function to determine a confidence interval
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
CostsxD=function(AmountOfTests,MaintenanceTime=356){
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
  #print("Mean:")
  #print(mean(ListOfCosts))
  #print("sd:")
  #print(sd(ListOfCosts))
  return(ListOfCosts)
}
lol=CostsxD(100)

#DUURT LANG OM TE RUNNEN MET DE FOR LOOP
for (s in seq(1,20)){
BestT=c()
Means=c()
SampleSize = 100
plot(seq(1,500),seq(10,29.96,1/25),type="n",ylab="Means")
Means[49]=mean(CostsxD(SampleSize,49))
for (i in seq(50,500)){
  Means[i]=mean(CostsxD(SampleSize,i))
  #lines(c(i-1,i),c(Means[i-1],Means[i]),type="l") #voor de leuk :D
  
}
plot(seq(1,500),Means,type='l')
print(which.min(Means))
print("s is")
print(s)
BestT[s]=which.min(Means)
}


VariableCost = function(varTime = 0.8,minRUL = 50 ,AmountOfTests = 1){
  
  #Beginwaarden
  ListOfCosts=c()
  Cr = 3
  Cm = 0.5
  Cp = 1
  
  #pak random eerste batterij
  Battery = sample(seq(2,80),1)
  
  #loop door aantal tests
  for (test_ in seq(1,AmountOfTests)){
    Costs = 0
    k = 1
    
    
    while (k < 2000){
      i = 1
      j = 1
      Maintenance = min(500,max(as.integer(varTime*Poly(my1stDegree.lm,1,Data[[Battery]][i])),minRUL))
      while (k < 2000){
        
        
        
        #Battery dead
        if (Data[[Battery]][i]<=0.88){
          Costs = Costs + Cr + Cm + Cp
          
          Battery = sample(seq(2,80),1)
          k=k+1
          
          break 
        }
        
        
        #Maintenance
        if(j == Maintenance){
          Costs = Costs + Cm
          
          if (Poly(my1stDegree.lm,1,Data[[Battery]][i])<minRUL){
            Costs = Costs + Cr
            Battery = sample(seq(2,80),1)
            k=k+1
            
            break
            
          }
          
          j = 1
          Maintenance = min(500,max(as.integer(varTime*Poly(my1stDegree.lm,1,Data[[Battery]][i])),minRUL))
          
        }
        k = k + 1
        i = i + 1
        j = j + 1
        
      }
    }
    
    ListOfCosts[test_] = Costs
  }
  return(mean(ListOfCosts))
}







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



x = seq(0.01,1,by=0.01)
y = c()

for (i in seq(0.01,1,by=0.01)){
  y[i*100] = VariableCost(i,50,500)
}
plot(x,y,xlab='lambda',ylab='Mean costs')



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





#Implementing spline regression, with k = 4


#Partitioning up the data
Partition1=c()
Partition2=c()
Partition3=c()
Partition4=c()
Partition5=c()

for (i in seq(1,162582)){
  x=TransformedData$Capacity[i]
  if (is.na(x)==FALSE){
    if (x >0.85 && x <0.9){
      Partition1$Capacity[i]=x
      Partition1$RUL[i]=TransformedData$RUL[i]
    
    }
    if (x >=0.9 && x <0.95){
      Partition2$Capacity[i]=x
      Partition2$RUL[i]=TransformedData$RUL[i]
      
    }
    if (x >=0.95 && x <1){
      Partition3$Capacity[i]=x
      Partition3$RUL[i]=TransformedData$RUL[i]
      
    }
    if (x >=1 && x <1.05){
      Partition4$Capacity[i]=x
      Partition4$RUL[i]=TransformedData$RUL[i]
      
    }
    if (x >=1.05 && x <1.1){
      Partition5$Capacity[i]=x
      Partition5$RUL[i]=TransformedData$RUL[i]
      
    }
  }
}

#Making 5 models based on the log of the RUL
my.lm1=lm(log(Partition1$RUL+1) ~ poly(Partition1$Capacity, 1, raw = TRUE))
my.lm2=lm(log(Partition2$RUL+1) ~ poly(Partition2$Capacity, 1, raw = TRUE))
my.lm3=lm(log(Partition3$RUL+1) ~ poly(Partition3$Capacity, 1, raw = TRUE))
my.lm4=lm(log(Partition4$RUL+1) ~ poly(Partition4$Capacity, 1, raw = TRUE))
my.lm5=lm(log(Partition5$RUL+1) ~ poly(Partition5$Capacity, 1, raw = TRUE))
Coeff1=c(my.lm1$coefficients[[1]],my.lm1$coefficients[[2]])
Coeff2=c(my.lm2$coefficients[[1]],my.lm2$coefficients[[2]])
Coeff3=c(my.lm3$coefficients[[1]],my.lm3$coefficients[[2]])
Coeff4=c(my.lm4$coefficients[[1]],my.lm4$coefficients[[2]])
Coeff5=c(my.lm5$coefficients[[1]],my.lm5$coefficients[[2]])


SplineLine=function(my.lm,start,end){
  
  
  x=seq(from=start,to=end,by=0.001)
  y=rep(my.lm$coefficients[[1]],length(x))
  if (is.na(my.lm$coefficients[[2]])==FALSE){
      y=y + my.lm$coefficients[[2]]*x**(1)
  }
  lines(x,y ,col='red', lwd=4)

  
}
PlotLogData()
SplineLine(my.lm1,0.85,0.9)
SplineLine(my.lm2,0.9,0.95)
SplineLine(my.lm3,0.95,1)
SplineLine(my.lm4,1,1.05)
SplineLine(my.lm5,1.05,1.1)
PlotLogForecast(1)
SplinePoly=function(x,k){
  if (is.na(x)==TRUE){
    return(0)
  }
  h=paste("Partition",as.character(k),sep='')
  j=paste("my.lm",as.character(k),sep='')
  Partition = get(h)
  my.lm=get(j)

  return(exp(my.lm$coefficients[[1]]+x*my.lm$coefficients[[2]])-1)
}
MSESplineFit=function(k){
  h=paste("Partition",as.character(k),sep='')
  j=paste("my.lm",as.character(k),sep='')
  Partition = get(h)
  my.lm=get(j)
  Residuals=c()
  for (i in seq(1,162582)){
    x=Partition$Capacity[i]
    if (is.na(x)==FALSE){
      Residuals[i] = Partition$RUL[i]-SplinePoly(x,k)

    
    }
    else{
      Residuals[i] = 0
    }
     
    
  }
  Residuals=Residuals**2
  return(Residuals)
  
}
SquareResidualsFit=MSESplineFit(1)+MSESplineFit(2)+MSESplineFit(3)+MSESplineFit(4)+MSESplineFit(5)
mean(SquareResidualsFit)


MSESplinePredict=function(k){
  #Need to redo partition of x-axis for the test data
  Partition1=c()
  Partition2=c()
  Partition3=c()
  Partition4=c()
  Partition5=c()
  
  for (i in seq(1,162582)){
    x=TransformedData2$Capacity[i]
    if (is.na(x)==FALSE){
      if (x >0.85 && x <0.9){
        Partition1$Capacity[i]=x
        Partition1$RUL[i]=TransformedData2$RUL[i]
        
      }
      if (x >=0.9 && x <0.95){
        Partition2$Capacity[i]=x
        Partition2$RUL[i]=TransformedData2$RUL[i]
        
      }
      if (x >=0.95 && x <1){
        Partition3$Capacity[i]=x
        Partition3$RUL[i]=TransformedData2$RUL[i]
        
      }
      if (x >=1 && x <1.05){
        Partition4$Capacity[i]=x
        Partition4$RUL[i]=TransformedData2$RUL[i]
        
      }
      if (x >=1.05 && x <1.1){
        Partition5$Capacity[i]=x
        Partition5$RUL[i]=TransformedData2$RUL[i]
        
      }
    }
  }
  h=paste("Partition",as.character(k),sep='')
  j=paste("my.lm",as.character(k),sep='')
  Partition = get(h)
  my.lm=get(j)
  Residuals=c()
  for (i in seq(1,33920)){
    x=Partition$Capacity[i]
    if (is.na(x)==FALSE){
      Residuals[i] = Partition$RUL[i]-SplinePoly(x,k)
      
      
    }
    else{
      Residuals[i] = 0
    }}
  Residuals=Residuals**2
  return(Residuals)}

SquareResidualsPredict=MSESplinePredict(1)+MSESplinePredict(2)+MSESplinePredict(3)+MSESplinePredict(4)+MSESplinePredict(5)
mean(SquareResidualsPredict)


#Now to construct a confidence interval we will use the method by R
CoeffLwr1=c(confint(my.lm1)[1],confint(my.lm1)[2])
CoeffUpr1=c(confint(my.lm1)[3],confint(my.lm1)[4])
CoeffLwr2=c(confint(my.lm2)[1],confint(my.lm2)[2])
CoeffUpr2=c(confint(my.lm2)[3],confint(my.lm2)[4])
CoeffLwr3=c(confint(my.lm3)[1],confint(my.lm3)[2])
CoeffUpr3=c(confint(my.lm3)[3],confint(my.lm3)[4])
CoeffLwr4=c(confint(my.lm4)[1],confint(my.lm4)[2])
CoeffUpr4=c(confint(my.lm4)[3],confint(my.lm4)[4])
CoeffLwr5=c(confint(my.lm5)[1],confint(my.lm5)[2])
CoeffUpr5=c(confint(my.lm5)[3],confint(my.lm5)[4])

CoeffLine=function(Coeff,start,end){
  x=seq(from=start,to=end,by=0.001)
  y=rep(Coeff[1],length(x))
  
  y=y + Coeff[2]*x
  lines(x,y,col='blue')

  
}
PlotLogData()
SplineLine(my.lm1,0.85,0.9)
SplineLine(my.lm2,0.9,0.95)
SplineLine(my.lm3,0.95,1)
SplineLine(my.lm4,1,1.05)
SplineLine(my.lm5,1.05,1.1)
CoeffLine(CoeffUpr1,0.85,0.9)
CoeffLine(CoeffLwr1,0.85,0.9)
CoeffLine(CoeffUpr2,0.9,0.95)
CoeffLine(CoeffLwr2,0.9,0.95)
CoeffLine(CoeffUpr3,0.95,1.0)
CoeffLine(CoeffLwr3,0.95,1.0)
CoeffLine(CoeffUpr4,1.0,1.05)
CoeffLine(CoeffLwr4,1.0,1.05)
CoeffLine(CoeffUpr5,1.05,1.1)
CoeffLine(CoeffLwr5,1.05,1.1)

