
hypso<- read.csv("hypsocurve.csv",header=T, sep=";")


hypso[,1]<- as.double(gsub(",", ".", hypso[,1]))


plot(hypso$depth, hypso$volume,   type="l", lwd=2, xlim=c(0, -70),
     main="Stechlin depth ~ volume", ylab="volume", xlab= "depth")



pcm<- st_physchemmet
colnames(pcm)[1]<- "date"
pcm$date<-  as.POSIXct(pcm$date, tz = "UTC", format = "%d.%m.%Y")
pcm<- pcm[with(pcm, order(date, depth)),]


pcm1<- subset(pcm, pcm$date== pcm$date[20000])


interprofile(hypso[,1:2], pcm1$depth, hypso$depth, min.valid.meas=0, date.column = T)







