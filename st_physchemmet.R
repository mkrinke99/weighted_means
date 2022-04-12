library(ggplot2)
library(plotly)

st<- read.csv(file = "st_physchemmet.csv", header= T, sep= ";")
st$time<- as.POSIXct(st$time, format= "%d.%m.%Y", tz= "UTC")

#checking NA values-------------

#observations per year

valuesdepth<-  table(substr(subset(st$time, !is.na(st$depth)),1,4))
table(!is.na(st[,2]))   #30342 measured values for depth, 5732 NAs

valueswt<-  table(substr(subset(st$time, !is.na(st$wt)),1,4))
table(!is.na(st[,3]))   #16077 measured values for water temperature

valueso2<-  table(substr(subset(st$time, !is.na(st$o2)),1,4))
table(!is.na(st[,4]))   #15647 measured values for O2

valuesso2<-  table(substr(subset(st$time, !is.na(st$so2)),1,4))
table(!is.na(st[,5]))   #15641 measured values for SO2

valuesph<-  table(substr(subset(st$time, !is.na(st$ph)),1,4))
table(!is.na(st[,6]))   #15136 measured values for pH 

table(complete.cases(st[,2:6])) 
#14553 measurements with depth, water temperature, O2, SO2 and pH data

valuestp<-  table(substr(subset(st$time, !is.na(st$tp)),1,4))
table(!is.na(st[,11])) #4885 measured values for total phosphorus

valuessrp<-  table(substr(subset(st$time, !is.na(st$srp)),1,4))
table(!is.na(st[,12])) #4929 measured values for soluble reactive phosphorus

valuestn<-  table(substr(subset(st$time, !is.na(st$tn)),1,4)) #1993-2020!
table(!is.na(st[,13])) #3759 measured values for total nitrogen

valuesno2<-  table(substr(subset(st$time, !is.na(st$no2)),1,4))
table(!is.na(st[,14])) #4002 measured values for NO2

valuesno3<-  table(substr(subset(st$time, !is.na(st$no3)),1,4))
table(!is.na(st[,15])) #4394 measured values for NO3

valuesnh4<-  table(substr(subset(st$time, !is.na(st$nh4)),1,4))
table(!is.na(st[,16])) #4376 measured values for NH4

valuessi<-  table(substr(subset(st$time, !is.na(st$si)),1,4))
table(!is.na(st[,17])) #2962 measured values for Si

valuespar<-  table(substr(subset(st$time, !is.na(st$PAR)),1,4)) #1997-2020!
table(!is.na(st[,18])) #6589 measured values for PAR 

plot(valuesdepth, las=1, ylim=c(0,100*ceiling(max(valuesdepth)/100)),
     type="b", pch=16, lwd= 2, col= "forestgreen")
abline(h=seq(0,100*ceiling(max(valuesdepth)/100),100), col= "grey82", lty=2)
abline(h=seq(0,100*ceiling(max(valuesdepth)/100),500), col= "grey52", lty=2)
lines(valueswt, type="b", pch=16, lwd= 2, col= "cyan4", lty=2)
lines(valueso2, type="b", pch=16, lwd= 2, col= "red3", lty=3)
lines(valuesso2, type="b", pch=16, lwd= 2, col= "orange", lty=4)
lines(valuesph, type="b", pch=16, lwd= 2, col= "grey12", lty=3)
lines(valuespar, type="b", pch=16, lwd= 2, col= "limegreen", lty=3)
legend("topleft", legend= c("depth","water temperature","O2","SO2","pH","PAR"),
       fill=c("forestgreen","cyan4","red3","orange","grey12","limegreen"))


plot(valuestp, las=1, ylim=c(0,100*ceiling(max(valuestp)/100)),
     type="b", pch=16, lwd= 2, col= "forestgreen")
abline(h=seq(0,100*ceiling(max(valuestp)/100),50), col= "grey82", lty=2)
abline(h=seq(0,100*ceiling(max(valuestp)/100),200), col= "grey52", lty=2)
lines(valuessrp, type="b", pch=16, lwd= 2, col= "cyan4", lty=2)
lines(valuesno2, type="b", pch=16, lwd= 2, col= "orange", lty=4)
lines(valuesno3, type="b", pch=16, lwd= 2, col= "grey12", lty=3)
lines(valuesnh4, type="b", pch=16, lwd= 2, col= "limegreen", lty=3)
lines(valuessi,  type="b", pch=16, lwd= 2, col= "purple", lty=3)
lines(valuestn, type="b", pch=16, lwd= 2, col= "red3", lty=3)
legend("topleft", legend= c("tp","srp","tn","NO2","NO3","NH4","Si"),
       fill=c("forestgreen", "cyan4","red3","orange","grey12","limegreen","purple"))




st05<- subset(st, st$depth<  5 & st$depth>=  0)
st10<- subset(st, st$depth< 10 & st$depth>=  5)
st15<- subset(st, st$depth< 15 & st$depth>= 10)
st20<- subset(st, st$depth< 20 & st$depth>= 15)
st25<- subset(st, st$depth< 25 & st$depth>= 20)
st30<- subset(st, st$depth< 30 & st$depth>= 25)
st35<- subset(st, st$depth< 35 & st$depth>= 30)
st40<- subset(st, st$depth< 40 & st$depth>= 35)
st45<- subset(st, st$depth< 45 & st$depth>= 40)
st50<- subset(st, st$depth< 50 & st$depth>= 45)
st55<- subset(st, st$depth< 55 & st$depth>= 50)
st60<- subset(st, st$depth< 60 & st$depth>= 55)
st65<- subset(st, st$depth< 65 & st$depth>= 60)
st70<- subset(st, st$depth< 70 & st$depth>= 65)


stdepth<- list(st05, st10, st15, st20, st25, st30, st35, st40, st45,
                 st50, st55, st60, st65, st70)

#MAF-----------
modulo<-function(x,a){		# gibt den Rest der Division x/a zur�ck
  mod_dummy<-x-round(x/a)*a	#round() geht bei Rundung von 5 zur n�chsten geraden Zahl!
  if (mod_dummy<0) mod_dummy+a else mod_dummy
}

is.even<-function(x){
  if(modulo(x,2)==0) TRUE else FALSE
}
MAF_even<-function(ord,reihe){		# moving average filter, argumente: ordnung "ord" (gerade zahlen!), zeitreihe "reihe"
  n<-ord/2	# funktioniert nur für gerade Zahlen
  result<-reihe
  leng<-length(reihe)
  for(i in 1:n)result[i]<-mean(reihe[1:(2*n)])	# setze die ersten n Werte auf Mittelwert der ersten 2*n Werte
  for(i in (leng-n+1):leng)result[i]<-mean(reihe[(leng-(2*n)+1):leng])	#letzte n Werte: Mittelwert der letzten ord Werte
  for(i in (n+1):(leng-n)){
    result[i]<-(sum(reihe[(i-n+1):(i+n-1)])+0.5*(reihe[i+n]+reihe[i-n]))/(2*n)	#Gleitender Durchschnitt
  }
  result		#Ausgabe
}
MAF_uneven<-function(ord,reihe){		# moving average filter, argumente: ordnung "ord" (ungerade zahlen!), zeitreihe "reihe"
  n<-(ord-1)/2	# funktioniert nur f�r gerade Zahlen
  result<-reihe
  leng<-length(reihe)
  for(i in 1:n)result[i]<-mean(reihe[1:(2*n)])	# setze die ersten n Werte auf Mittelwert der ersten 2*n Werte
  for(i in (leng-n+1):leng)result[i]<-mean(reihe[(leng-(2*n)+1):leng])	#letzte n Werte: Mittelwert der letzten ord Werte
  for(i in (n+1):(leng-n)){
    result[i]<-(sum(reihe[(i-n):(i+n)]))/(2*n+1)	#Gleitender Durchschnitt
  }
  result		#Ausgabe
}
MAF<-function(ord,reihe){
  if (is.even(ord)==TRUE) result<-MAF_even(ord,reihe) else result<-MAF_uneven(ord,reihe) 
  result
}



#depth-----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
plot(stdepth[[i]]$time, stdepth[[i]]$depth, col= "steelblue3", pch= 16, cex= 0.7,
     xlab= "time", ylab= "depth", las=1,
     main= paste0("Measuring depth ", 5*i-5,"-",5*i,"m-layer"),
     ylim= c(5*i-5,5*i), xlim= c(min(st$time), max(st$time)))
}
#depth of observed data, until 25m in 1-m-steps, mostly 5-m-steps in deeper sections

#water temperature---------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$wt))
  plot(stdepth[[i]]$time, stdepth[[i]]$wt, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "water temperature", las=1,
       main= paste0("Water temperature in ", 5*i-5,"-",5*i,"m-layer"),
       ylim= c(min(st$wt, na.rm= T), max(st$wt, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(365,  stdepth[[i]]$wt), col="red3")
}
#no surprisingly high or low values

#O2 concentration-----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  plot(stdepth[[i]]$time, stdepth[[i]]$o2, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "O2 in mg/l", las=1,
       main= paste0("O2 concentration in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$o2, na.rm= T), max(st$o2, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$o2), col="red3", lwd= 2)
}
#since 2010: O2 concentration declined in 10-70 meter deepness

#SO2 concentration-----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  plot(stdepth[[i]]$time, stdepth[[i]]$so2, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "SO2 in mg/l", las=1,
       main= paste0("SO2 concentration in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$so2, na.rm= T), max(st$so2, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$so2), col="red3", lwd= 2)
}
#since 2010: sO2 concentration declined

#pH-----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$ph))
  plot(stdepth[[i]]$time, stdepth[[i]]$ph, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "pH", las=1,
       main= paste0("pH in ", 5*i-5,"-",5*i,"m-layer"),
       ylim= c(min(st$ph, na.rm= T), max(st$ph, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$ph), col="red3", lwd= 2)
}



# Secchi ---------------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$secchi))
  plot(stdepth[[i]]$time, stdepth[[i]]$secchi, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "secchi depth in m", las=1,
       main= paste0("Secchi depth in ", 5*i-5,"-",5*i,"m-layer in m"),
       ylim= c(15,0),
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$secchi), col="red3", lwd= 2)
}

#total phosphorus----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
   stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$tp))
  plot(stdepth[[i]]$time, stdepth[[i]]$tp, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "total phosphorus in mg/l", las=1,
       main= paste0("Total phosphorus in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$tp, na.rm= T), max(st$tp, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(15,  stdepth[[i]]$tp), col="red3", lwd= 2)
}
#increasing concentration begining in 2010

#soluble reactive phosphorus----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$srp))
  plot(stdepth[[i]]$time, stdepth[[i]]$srp, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "soluble reactive phosphorus in mg/l", las=1,
       main= paste0("Soluble reactive in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$srp, na.rm= T), max(st$srp, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$srp), col="red3", lwd= 2)
}
#increasing concentration begining in 2010

#total nitrogen-----------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$tn))
  plot(stdepth[[i]]$time, stdepth[[i]]$tn, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "total nitrogen in mg/l", las=1,
       main= paste0("Total nitrogen in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$tn, na.rm= T), max(st$tn, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$tn), col="red3", lwd= 2)
}
#no data before 1995, some layers just have data from 2010 onwards

#NO2 concentration---------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$no2))
  plot(stdepth[[i]]$time, stdepth[[i]]$no2, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "NO2 concentration in mg/l", las=1,
       main= paste0("NO2 in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$no2, na.rm= T), max(st$no2, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$no2), col="red3", lwd= 2)
}

#NO3 concentration---------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$no3))
  plot(stdepth[[i]]$time, stdepth[[i]]$no3, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "NO3 concentration in mg/l", las=1,
       main= paste0("NO3 in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$no3, na.rm= T), max(st$no3, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(60,  stdepth[[i]]$no3), col="red3", lwd= 2)
}

#NH4 concentration--------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  stdepth[[i]]<- subset( stdepth[[i]], !is.na(stdepth[[i]]$no3))
  plot(stdepth[[i]]$time, stdepth[[i]]$nh4, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "NH4 concentration in mg/l", las=1,
       main= paste0("NH4 in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$nh4, na.rm= T), max(st$nh4, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
  lines(stdepth[[i]]$time, MAF(20,  stdepth[[i]]$no3), col="red3", lwd= 2)
}

#silicate concentration---------
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  plot(stdepth[[i]]$time, stdepth[[i]]$si, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "Si concentration in mg/l", las=1,
       main= paste0("Si concentration in ", 5*i-5,"-",5*i,"m-layer in mg/l"),
       ylim= c(min(st$si, na.rm= T), max(st$si, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
}
#lack of data for 6 layers, 2 layers with very limited data

#PAR(Photosynthetically active radiation)---------
stdepth<- list(st05, st10, st15, st20, st25, st30, st35, st40, st45,
               st50, st55, st60, st65, st70)
par(mfrow= c(3,5), bg= "azure")
for(i in 1:14){
  plot(stdepth[[i]]$time, stdepth[[i]]$PAR, col= "steelblue3", pch= 16, cex= 0.7,
       xlab= "time", ylab= "PAR in micromol*m/s", las=1,
       main= paste0("Photosynthetically active radiation in ", 5*i-5,"-",5*i,"m-layer"),
       ylim= c(min(st$PAR, na.rm= T), max(st$PAR, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))
}

#ice cover, snow cover, ice thickness, snow thickness----------
par(mfrow= c(2,2), bg= "azure")
  plot(st$time, st$ice.cover, col= "steelblue3", pch= 16,
       xlab= "time", ylab= "ice cover in %", las=1, cex= 0.8,
       main= "Ice cover in %", type= "b",
       ylim= c(0,100), 
       xlim= c(min(st$time), max(st$time)))

  plot(st$time, st$snow.cover, col= "steelblue3", pch= 16,
       xlab= "time", ylab= "snow cover in %", las=1, cex= 0.8,
       main= "Snow cover in %", type= "b",
       ylim= c(0,100), 
       xlim= c(min(st$time), max(st$time)))

  plot(st$time, st$ice.thickness, col= "steelblue3", pch= 16,
       xlab= "time", ylab= "ice thickness in m", las=1, cex= 0.8,
       main= "Ice thickness in m", 
       ylim= c(0, max(st$ice.thickness, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))

  plot(st$time, st$snow.thickness, col= "steelblue3", pch= 16,
       xlab= "time", ylab= "snow thickness in m", las=1, cex= 0.8,
       main= "Snow thickness in m", 
       ylim= c(0, max(st$snow.thickness, na.rm= T)), 
       xlim= c(min(st$time), max(st$time)))



plot(c(0,70), c(0,25))
lines(st$depth[st$time== "2001-11-01 UTC"], st$wt[st$time= "2001-11-01 UTC"])


st$snow.thickness[!is.na(st$snow.thickness) & st$snow.thickness >0]


#depth ~ water temperature-------

wtdays<-unique(st$time[!is.na(st$wt)])
wtwinter<- subset(wtdays, substr(wtdays, 6,7)== "12" | substr(wtdays, 6,7)== "01" |
                  substr(wtdays, 6,7)== "02") 
wtspring<- subset(wtdays, substr(wtdays, 6,7)== "03" | substr(wtdays, 6,7)== "04" |
                    substr(wtdays, 6,7)== "05") 
wtsummer<- subset(wtdays, substr(wtdays, 6,7)== "06" | substr(wtdays, 6,7)== "07" |
                    substr(wtdays, 6,7)== "08") 
wtautumn<- subset(wtdays, substr(wtdays, 6,7)== "09" | substr(wtdays, 6,7)== "10" |
                    substr(wtdays, 6,7)== "11") 

depth<- list()
temp<-  list()
plot(NA,NA, xlim= c(0,25), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:200){
  depth[[i]]<- st$depth[st$time== wtdays[i]]
  temp[[i]] <- st$wt[st$time== wtdays[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}

#winter
plot(NA,NA, xlim= c(0,10), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:length(wtwinter)){
  depth[[i]]<- st$depth[st$time== wtwinter[i]]
  temp[[i]] <- st$wt[st$time== wtwinter[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}

#spring
plot(NA,NA, xlim= c(0,22), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:length(wtspring)){
  depth[[i]]<- st$depth[st$time== wtspring[i]]
  temp[[i]] <- st$wt[st$time== wtspring[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}

#summer
plot(NA,NA, xlim= c(0,22), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:length(wtsummer)){
  depth[[i]]<- st$depth[st$time== wtsummer[i]]
  temp[[i]] <- st$wt[st$time== wtsummer[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}

#autumn
plot(NA,NA, xlim= c(0,22), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:length(wtautumn)){
  depth[[i]]<- st$depth[st$time== wtautumn[i]]
  temp[[i]] <- st$wt[st$time== wtautumn[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}




depth<- list()
temp<-  list()
plot(NA,NA, xlim= c(0,15), ylim= c(0,70), ylab="depth", xlab="water temperature",
     las=1, main= "depth ~ water temperature")
for(i in 1:1000){
  depth[[i]]<- st$depth[st$time== wtdays[i]]
  temp[[i]] <- st$secchi[st$time== wtdays[i]]
  lines( temp[[i]][order(depth[[i]])],sort(depth[[i]]),
         col= "grey23")
}


#secchi 2----------------
secchi<- aggregate(st$secchi, list(st$time), mean, na.rm= T)
secchi<- subset(secchi, !is.na(secchi$x))
secchi[,3]<- MAF(21, secchi$x)

#plot
plot(secchi$Group.1, secchi$x, type= "h", ylim= c(15,0), col= "lightskyblue3",
     xlab= "time", ylab= "Secchi depth", main= "Secchi depth \n Lake Stechlin", las= 1)
points(secchi$Group.1, secchi$x, pch= 15, cex= 0.7, col= "lightskyblue4")
lines(secchi$Group.1 ,MAF(21, secchi$x), col= "red3", lwd= 2)
abline(h= c(seq(0,15,1)), col="grey75", lty= 2)
abline(h= c(seq(0,15,5)), col="grey45", lty= 2)

aggregate(secchi$x, list(substr(secchi$Group.1, 1, 3)), mean)

theme_update(plot.title = element_text(hjust = 0.5))


#ggplot
ggp<- ggplot(data = secchi, aes(x = Group.1 , y = x ))+ ylim (15, 0) + 
  labs(x= "time", y= "secchi depth") + ggtitle("Secchi depth \nLake Stechlin") +
  geom_point(col= "lightskyblue4")+
  geom_line(data= secchi ,aes(x= Group.1, y= V3, 
            colour= "21 measures moving average") ,size=2)+
  geom_hline(yintercept=c(1,2,3,4,6,7,8,9,11,12,13,14) ,color= "grey75", lty= 2)+
  geom_linerange(aes(x = Group.1 , ymax = x, ymin=0), colour= "lightskyblue3")+
  scale_color_manual(name = "legend", 
                     values = c("21 measures moving average" = "red3"))

ggp<- ggp + theme(plot.title = element_text(size = 20))   
ggp



#contour----------
stwt<- subset(st, !is.na(st$wt))
t1<- c(10000:15809)

hmwt<- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$wt[t1], type= "contour")%>%
       layout(title = "Water temperature: October 2018 - September 2020",
              xaxis = list(title = "time"),
              yaxis=list(title = "depth [m]", range= c(70,0)))
hmwt <- hmwt %>% colorbar(title = "[°C]")
hmwt

hmo2 <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$o2[t1], type= "contour")%>%
        layout(title = "O2 concentration: October 2018 - September 2020", 
               xaxis = list(title = "time"),
         yaxis=list(title = "O2 concentration [mg/l]", range= c(70,0)))
hmo2 <- hmo2 %>% colorbar(title = "mg/l]")
hmo2

hmSO2 <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$so2[t1], type= "contour")%>%
  layout(title = "SO2 concentration: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "SO2 concentration [mg/l]", range= c(70,0)))
hmSO2 <- hmSO2 %>% colorbar(title = "mg/l")
hmSO2

hmph <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$ph[t1], type= "contour")%>%
  layout(title = "pH value: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "pH value", range= c(70,0)))
hmph <- hmph %>% colorbar(title = "pH")
hmph

hmtp <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$tp[t1], type= "contour")%>%
  layout(title = "total phosphorus: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "total phosphorus concentration [mg/l]", range= c(70,0)))
hmtp <- hmtp %>% colorbar(title = "mg/l")
hmtp

hmsrp <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$srp[t1], type= "contour")%>%
  layout(title = "Soluble reactive phosphorus: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "Soluble reactive phosphorus [mg/l]", range= c(70,0)))
hmsrp <- hmsrp %>% colorbar(title = "mg/l")
hmsrp

hmtn <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$tn[t1], type= "contour")%>%
  layout(title = "Total nitrogen: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "Total nitrogen [mg/l]", range= c(70,0)))
hmtn <- hmtn %>% colorbar(title = "mg/l")
hmtn

hmno2 <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$no2[t1], type= "contour")%>%
  layout(title = "NO2 concentration: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "NO2 concentration [mg/l]", range= c(70,0)))
hmno2 <- hmno2 %>% colorbar(title = "mg/l")
hmno2

hmno3 <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$no3[t1], type= "contour")%>%
  layout(title = "NO3 concentration: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "NO3 concentration [mg/l]", range= c(70,0)))
hmno3 <- hmno3 %>% colorbar(title = "mg/l")
hmno3

hmnh4 <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$nh4[t1], type= "contour")%>%
  layout(title = "NH4 concentration: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "NH4 concentration [mg/l]", range= c(70,0)))
hmnh4 <- hmnh4 %>% colorbar(title = "mg/l")
hmnh4

hmsi <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$si[t1], type= "contour")%>%
  layout(title = "Si concentration: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "Si concentration [mg/l]", range= c(70,0)))
hmsi <- hmsi %>% colorbar(title = "mg/l")
hmsi

hmPAR <- plot_ly(x= stwt$time[t1], y=stwt$depth[t1], z= stwt$PAR[t1], type= "contour")%>%
  layout(title = "Photosynthetically active radiation: October 2018 - September 2020", 
         xaxis = list(title = "time"),
         yaxis=list(title = "PAR [micromole*m/s]", range= c(70,0)))
hmPAR <- hmPAR %>% colorbar(title = "micromole*m/s")
hmPAR


#ggplots 1994-2020-----------
stcomp[,length(stcomp)+1]<- MAF(21, stcomp$wt)

ggp<- ggplot(data = stcomp, aes(x = time , y = wt )) + 
   labs(x= "time", y= "water temperature") +
   ggtitle("Water temperature Lake Stechlin") +
   geom_point(col= "lightskyblue3") +
   geom_line(data= secchi ,aes(x= Group.1, y= V40, 
         colour= "21 measures moving average") ,size=2)+
   geom_hline(yintercept=c(1:4, 6:9, 11:14, 16:19, 21:25) ,color= "grey75", lty= 2)+
   geom_linerange(aes(x = Group.1 , ymax = x, ymin=0), colour= "lightskyblue3")+
   scale_color_manual(name = "legend", 
                     values = c("21 measures moving average" = "red3"))

ggp<- ggp + theme(plot.title = element_text(size = 20))   
ggp








