
setwd("C:/Grad Case Studies/Leads Data")

leadsken2015 = read.csv("Dayton FY2016 Leads.csv")
new = read.csv("All Leads Kentucky FY2015.csv")

colnames(leadsken2015)=colnames(new)

add = leadsken2015$address
add = as.character(add)

for ( i in 1:length(add) ) {
  
  n = nchar(add[i])
  
  add[i] = substring(add[i],n-5,n-1)
  
}

Pincode = add

leads2015 = cbind(leadsken2015,Pincode)

UniqueId = paste(leads2015$accountnum,"-",leads2015$estimatetype)



leads2015 = cbind(UniqueId,leads2015)


dup = duplicated(leads2015$estimateid)

leads2015 = leads2015[!dup,]



m=length(leads2015[,1])

leads2015 = with(leads2015, leads2015[order(leads2015$accountnum,leads2015$statusflag,leads2015$recieveddate),])

leads2015 = leads2015[1:m,]

leads2015$statusflag = as.numeric(as.character(leads2015$statusflag))



for (i in 1:length(leads2015[,1])){
  if(is.na(leads2015$statusflag[i])){
    
    leads2015$statusflag[i]=-1
    leads2015$UniqueId=NA
    
  }
  
  if (leads2015$statusflag[i]==1){
    
    leads2015$statusflag[i]=0
    
  }
  else if (leads2015$statusflag[i]==0){
    
    leads2015$statusflag[i]=1
    
  }
}

n=0

for (i in 2:length(leads2015[,1])){
  
  
  if (leads2015$accountnum[i]==leads2015$accountnum[i-1]){
    
    n = n+1
    
    if(leads2015$statusflag[i]>max(leads2015$statusflag[i-(1:n)])){
      
      leads2015$UniqueId[i-1] = "delete" 
      
    }
    
    else {
      
      leads2015$UniqueId[i] = "delete"
      
    }
    
  }
  
  else {
    
    n = 0
    
  }
  
}



write.csv(leads2015,file="2016 Dayton Leads.csv",row.names = FALSE)










