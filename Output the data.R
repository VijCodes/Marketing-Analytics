
setwd("C:/Grad Case Studies/Leads Data")

leads2015 = read.csv("2015 Columbus Leads.csv")
leads2016 = read.csv("2016 Columbus Leads.csv")

del=is.na(leads2015$UniqueId)

leads2015 = leads2015[!del,]

leads2015$UniqueId = NULL

del=is.na(leads2016$UniqueId)

leads2016 = leads2016[!del,]

leads2016$UniqueId = NULL

write.csv(leads2015,file="2015 Columbus Leads final.csv",row.names = FALSE)
write.csv(leads2016,file="2016 Columbus Leads final.csv",row.names = FALSE)

merged = rbind(leads2015,leads2016)

dup = duplicated(merged$accountnum)

merged = merged[!dup,]


write.csv(merged,file="Columbusmerged.csv",row.names = FALSE)