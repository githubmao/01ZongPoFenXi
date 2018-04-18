#本程序脚本包含了多个异常行为标准计算的函数
library(ggplot2)
library(plyr)

#1. Order.dis 对道路编号相同的数据按照整桩号筛选，并排序-------------------
Order.dis <- function(data, step=1) 
{ # data为数据集，step为排列间距     
  data$Station<- as.numeric(data$Station)%/%step*step 
  end=length(data$Station)
  order <- c()
  for(i in 1:end)
  {
    if(i==1)
    {
      k=1
      order[1]=1
    }
    else if(data$Station[i]!=data$Station[i-1])
    {k=k+1
    order[k]=i
    }      
  }
  return(data[order,])
}


#2. 计算所有数据中速度最低点，以及对应的桩号----
Station.minSpeed<-function(data,IDname)
{
  Len<-length(IDname)
  result.out<-data.frame()
  for(i in 1:Len)
  {
    Temp_dataframe<-subset(data,Speed.begin ==IDname[i] & Station<=10000)
    minSpeed<-min(Temp_dataframe$Speed)
    Temp_dataframe<-subset(Temp_dataframe,Speed==minSpeed)
    grade.Len<-min(Temp_dataframe$Station)-100
    ID.result<-data.frame(as.double(IDname[i]),minSpeed,grade.Len)
    result.out<-rbind(result.out,ID.result)
  }
  colnames(result.out)<-c("Speed.begin","min.Speed","Grade.Length")
  return(result.out)
}


#3. 计算货车上坡行驶时的稳定速度以及达到稳定速度的坡长
CalcGradeSpeed <- function(data,
                           kLength = 50){
  # data:计算数据集
  # kLength:用于计算稳定速度的区间长度
  data <- Order.dis(data)
  
  for(i in 1: (length(data$Station) - kLength)){
    
    if(abs(data$Speed[i] - data$Speed[i+kLength]) <= 1){
      return(c(data$Speed[i], data$Station[i]))
      break
    } else if(i == (length(data$Station) - kLength)){
      return(c(data$Speed[i], data$Station[i]))
    }
  }
}







