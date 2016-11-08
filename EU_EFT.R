# Natura 2000 Analysis, spatial panel data and mapping #
# author: nils.droste@ufz.de #

setwd("C:/Users/droste/Dropbox/Dokumente/doctorate/fiscal transfers/EFT-EU/data")

# data -------------------------------------------------------------------
# raw
  #Natura 2000
  require(gdata)
  perl <-  gdata:::findPerl("perl")
    df.natura2000.2014 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2014.xls', sep=""), sheet = 1, header = T, perl = perl)  
    df.natura2000.2013 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2013.xls', sep=""), sheet = 1, header = T, perl = perl) 
    df.natura2000.2012 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2012.xls', sep=""), sheet = 1, header = T, perl = perl) 
    df.natura2000.2011 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2011.xls', sep=""), sheet = 1, header = T, perl = perl) 
    df.natura2000.2010 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2010.xls', sep=""), sheet = 1, header = T, perl = perl) 
    df.natura2000.2009 <- read.xls(paste(getwd(), "/stats/natura2000/", 'Natura2000_end2009.xls', sep=""), sheet = 1, header = T, perl = perl) 
    df.natura2000<-cbind.data.frame(NUTS_ID=df.natura2000.2013$NUTS_ID, natura2000.2013=df.natura2000.2013$PERCENTAGE, natura2000.2012=df.natura2000.2012$PERCENTAGE, natura2000.2011=df.natura2000.2011$PERCENTAGE)
    df.natura2000<-merge(df.natura2000, df.natura2000.2010, by="NUTS_ID", all.x = TRUE)
    df.natura2000<-merge(df.natura2000, df.natura2000.2009, by="NUTS_ID", all.x = TRUE)
    df.natura2000<-df.natura2000[,c(1:4,8,12)]
    colnames(df.natura2000)<-c("NUTS_ID","natura2000.2013","natura2000.2012","natura2000.2011","natura2000.2010","natura2000.2009")
    df.natura2000<-df.natura2000[, c(1,6,5,4,3,2) ]   #View(df.natura2000)
    detach(package:gdata)

  #area "km2"
  require(stringr)
    df.area <- read.table(paste(getwd(),"/stats/area/",'tgs00002.tsv', sep=""), header=TRUE, sep="\t", na.strings=": ", fileEncoding="windows-1252")             
    df.area <- df.area[227:540, c(1,8:12)] # subsetting for total area and from 2009:2013
    colnames(df.area) <- c("NUTS_ID", "area.2009", "area.2010", "area.2011", "area.2012", "area.2013")
    df.area$NUTS_ID<-as.factor(str_sub(df.area$NUTS_ID, -4)) 
    #View(df.area)
    
  #BIP.cap "Regional gross domestic product (PPS per inhabitant)"
    df.GDPcap <- read.table(paste(getwd(),"/stats/BIPcap/",'tgs00005.tsv', sep=""), header=TRUE, sep="\t", na.strings=": ",fileEncoding="windows-1252")         
    df.GDPcap <- df.GDPcap[, c(1,9:13)] # subsetting for 2009:2014
    colnames(df.GDPcap) <- c("NUTS_ID", "BIP.cap.2009", "BIP.cap.2010", "BIP.cap.2011", "BIP.cap.2012", "BIP.cap.2013")
    df.GDPcap$NUTS_ID<-as.factor(str_sub(df.GDPcap$NUTS_ID, -4))
    #View(df.GDPcap)
    
  #pop.density "Persons per km2"
    df.pop <- read.table(paste(getwd(),"/stats/pop/",'tgs00024.tsv', sep=""), header=TRUE, sep="\t", na.strings=": ",fileEncoding="windows-1252")               #View(df.pop)
    df.pop <- df.pop[, c(1,9:13)] # subsetting for 2009:2014
    colnames(df.pop) <- c("NUTS_ID", "pop.2009", "pop.2010", "pop.2011", "pop.2012", "pop.2013")
    #View(df.pop)
    
  #tourism  "Nights spent at tourist accommodation establishments"
    df.tourism <- read.table(paste(getwd(),"/stats/tourism/",'tgs00111.tsv', sep=""), header=TRUE, sep="\t", na.strings=": ",fileEncoding="windows-1252")       
    df.tourism <- df.tourism[599:897, c(1, 8:12)] #subsetting for total stays and 2009:2013
    colnames(df.tourism) <- c("NUTS_ID", "tour.2009", "tour.2010", "tour.2011", "tour.2012", "tour.2013")
    df.tourism$NUTS_ID<-as.factor(str_sub(df.tourism$NUTS_ID, -4))
    df.tourism<-df.tourism[c(1:92, 102:256, 259:299),] # getting rid of empty NUTS2 2013 for which values are reported for their NUTS2010 names
    #View(df.tourism) 
    
  #unemployment "unemployment rate %" 
    df.unemploy <- read.table(paste(getwd(),"/stats/unemployment/",'tgs00010.tsv', sep=""), header=TRUE, sep="\t", na.strings=": ",fileEncoding="windows-1252") #View(df.unemploy)
    df.unemploy <- df.unemploy[633:948, c(1, 8:12)] #subsetting for total unemploym. and 2009:2013
    colnames(df.unemploy) <- c("NUTS_ID", "unemp.2009", "unemp.2010", "unemp.2011", "unemp.2012", "unemp.2013")
    df.unemploy$NUTS_ID<-as.factor(str_sub(df.unemploy$NUTS_ID, -4))
    #View(df.unemploy)
    detach(package:stringr)
    
  #get rid of letters
      for (i in 2:6) {df.area[,i] <- as.numeric(as.character(sub("b", "", df.area[,i])))}
      for (i in 2:6) {df.GDPcap[,i] <- as.numeric(sub("e", "", df.GDPcap[,i]))} #NA introduced by coercion regard where there was no number
      for (i in 2:6) {df.tourism[,i] <- as.numeric(sub("e", "", df.tourism[,i]))} #NA introduced by coercion regard where there was no number
      for (i in 2:6) {df.unemploy[,i] <- as.numeric(sapply(strsplit(as.character(df.unemploy[,i]), " "), "[[", 1))} #NA introduced by coercion regard where there was no number
    
  #correcting for NUTS reform 2013
    to.be.substituted<-c("EL11","EL12","EL13","EL14","EL21","EL22","EL23","EL24","EL25","SI01","SI02")
    to.substitute<-c("EL51","EL52","EL53","EL61","EL54","EL62","EL63","EL64","EL65","SI03","SI04")
    for (i in 1:length(to.be.substituted)) {
        df.area<-as.data.frame(within(df.area, levels(NUTS_ID)[levels(NUTS_ID) == to.be.substituted[i]] <- to.substitute[i]))
    }
    for (i in 1:length(to.be.substituted)) {
      df.pop<-as.data.frame(within(df.pop, levels(NUTS_ID)[levels(NUTS_ID) == to.be.substituted[i]] <- to.substitute[i]))
    }
    for (i in 1:length(to.be.substituted)) {
      df.GDPcap<-as.data.frame(within(df.GDPcap, levels(NUTS_ID)[levels(NUTS_ID) == to.be.substituted[i]] <- to.substitute[i]))
    }
    for (i in 1:length(to.be.substituted)) {
      df.tourism<-as.data.frame(within(df.tourism, levels(NUTS_ID)[levels(NUTS_ID) == to.be.substituted[i]] <- to.substitute[i]))
    }
    for (i in 1:length(to.be.substituted)) {
      df.unemploy<-as.data.frame(within(df.unemploy, levels(NUTS_ID)[levels(NUTS_ID) == to.be.substituted[i]] <- to.substitute[i]))
    }
    
  #merging dfs
    df<-Reduce(function(x, y) merge(x, y, by="NUTS_ID", all.x = TRUE), list(df.natura2000, df.area, df.pop, df.GDPcap, df.tourism, df.unemploy))
    
  #subsetting EU-27 minus overseas
    to.match<-c("CH01","CH02","CH03","CH04","CH05","CH06","CH07","ES70","FRA1","FRA2","FRA3","FRA4","FRA5","HR03", "HR04", "IS00","LI00","ME00","MK00","NO01","NO02","NO03","NO04","NO05","NO06","NO07","PT20","PT30","TR10","TR21","TR22","TR31","TR32","TR33","TR41","TR42","TR51","TR52","TR61","TR62","TR63","TR71","TR72","TR81","TR82","TR83","TR90","TRA1","TRA2","TRB1","TRB2","TRC1","TRC2", "TRC3")
    df<-subset(df, grepl(paste(to.match,collapse="|"), as.character(df$NUTS_ID))==F)
    
# tidying up <- resformatting data for 1 multivariate obs per ID & year 
    df<-reshape(df, direction='long', varying=list(2:6, 7:11, 12:16, 17:21, 22:26, 27:31), v.names=c("natura2000", "area", "pop", "GDPcap", "tour", "unemp"), idvar="NUTS_ID", timevar = "year", times=c(2009, 2010, 2011, 2012, 2013))
    df<- df[order(df$NUTS_ID, df$year),] 
    
#adding UKI1 and UKI2 data for UKI3, UKI4, UKI5, UKI6 and UKI7 
    #area
    UKI3.area<-c(2.9+21.8+21.48+12.13+16.40+34.26,108.97,108.97,108.97,108.97)
    UKI4.area<-c(19.06+36.22+19.77+29.59+14.86+35.15+28.85+26.82,210.32,210.32,210.32,210.32)
    UKI5.area<-c(60.56+47.35+36.09+112.27+56.41+38.82+82.20,433.7,433.7,433.7,433.7)
    UKI6.area<-c(150.15+87+37.61+37.25+43.85,355.86,355.86,355.86,355.86)
    UKI7.area<-c(86.74+43.24+55.53+50.47+115.70+55.98+57.41,465.07,465.07,465.07,465.07)
    UKI.area<-c(UKI3.area,UKI4.area,UKI5.area,UKI6.area,UKI7.area)
    #BIP
    UKI3.GDPcap<-as.numeric(df.GDPcap[265,2:6])
    UKI4.GDPcap<-as.numeric(df.GDPcap[265,2:6])
    UKI5.GDPcap<-as.numeric(df.GDPcap[266,2:6])
    UKI6.GDPcap<-as.numeric(df.GDPcap[266,2:6])
    UKI7.GDPcap<-as.numeric(df.GDPcap[266,2:6])
    UKI.GDPcap<-c(UKI3.GDPcap,UKI4.GDPcap,UKI5.GDPcap,UKI6.GDPcap,UKI7.GDPcap)
    #pop
    UKI3.pop<-as.numeric(df.pop[300,2:6])
    UKI4.pop<-as.numeric(df.pop[300,2:6])
    UKI5.pop<-as.numeric(df.pop[301,2:6])
    UKI6.pop<-as.numeric(df.pop[301,2:6])
    UKI7.pop<-as.numeric(df.pop[301,2:6])
    UKI.pop<-c(UKI3.pop,UKI4.pop,UKI5.pop,UKI6.pop,UKI7.pop)
    #tourism
    UKI3.tour<-as.numeric(df.tourism[272,2:6]/2)
    UKI4.tour<-as.numeric(df.tourism[272,2:6]/2)
    UKI5.tour<-as.numeric(df.tourism[273,2:6]/3)
    UKI6.tour<-as.numeric(df.tourism[273,2:6]/3)
    UKI7.tour<-as.numeric(df.tourism[273,2:6]/3)
    UKI.tour<-c(UKI3.tour,UKI4.tour,UKI5.tour,UKI6.tour,UKI7.tour)
    #unemployment
    UKI3.unemploy<-as.numeric(df.unemploy[300,2:6])
    UKI4.unemploy<-as.numeric(df.unemploy[300,2:6])
    UKI5.unemploy<-as.numeric(df.unemploy[301,2:6])
    UKI6.unemploy<-as.numeric(df.unemploy[301,2:6])
    UKI7.unemploy<-as.numeric(df.unemploy[301,2:6])
    UKI.unemploy<-c(UKI3.unemploy,UKI4.unemploy,UKI5.unemploy,UKI6.unemploy,UKI7.unemploy)
    UKI<-as.data.frame(cbind(UKI.area,UKI.pop,UKI.GDPcap,UKI.tour,UKI.unemploy))
    df[1241:1265,4:8]<-UKI

  #natura 2000 favourable conservation status
    require(gdata)
    df.cs<- read.xls(paste(getwd(),"/stats/natura2000/quality/",'NUTS_RG_01M_2013_levl2_prop_CS_fav_improved_TableToExcel.xls', sep=""), sheet = 1, header = T, perl = perl) #View(df.cs)
    df.cs <- subset(df.cs, select=c(NUTS_ID, propFV_TOT))
    df.cs$year <- rep(2013, nrow(df.cs))
    detach(package:gdata)    
    
  #bioregions" 
    require(gdata)
    bioregions<- read.xls(paste(getwd(),"/stats/bioregions/",'bioregions.xls', sep=""), sheet = 1, header = T, perl = perl)
    bioregions[c(185,187),6]<-0
    bioregions[c(186,188),6]<-100
    bioregions$PERCENTAGE <- round(bioregions$PERCENTAGE, digits = 0)
    bioregions$PERCENTAGE[bioregions$PERCENTAGE>=99]<-100
    bioregions$PERCENTAGE[bioregions$PERCENTAGE<=1]<-0
    bioregions <- bioregions[bioregions$PERCENTAGE>0,c(2,3,6)]
    library(dplyr)
    library(tidyr)
    bioregions<-bioregions %>%
      group_by(NUTS_ID) %>%
      spread(ABBRE, PERCENTAGE)
    bioregions[is.na(bioregions)] <- 0
    bioregions <- bioregions[c(1,2,5:8, 10:ncol(bioregions))]
    detach(package:gdata)
    detach(package:dplyr)
    detach(package:tidyr)  
    
    df <- merge(df, df.cs[1:2], by=c("NUTS_ID"))
    df[690, 9] <- NA
    df[695, 9] <- NA
    df <- merge(df, bioregions, by=c("NUTS_ID"), all.x = TRUE)

#adding area data HR03 & HR04
#     HR03.area<-c(24705,24705,24705,24705,24705)
#     HR04.area<-c(31889,31889,31889,31889,31889)
#     HR.area<-c(HR03.area,HR04.area)
#     df[686:695,4]<-HR.area
    
# imputing missing values via bayesian bootstrapping
  set.seed(123)  
  require(Amelia)    
    bds.natura2000 <- matrix(c(3,0.000001, sort(df$natura2000, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds.area <- matrix(c(4,0.000001, sort(df$area, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds.pop <- matrix(c(5,0.000001, sort(df$pop, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds.GDPcap <- matrix(c(6,0.000001, sort(df$GDPcap, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds.tour <- matrix(c(7,0.000001, sort(df$tour, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds.unemp <- matrix(c(8,0.000001, sort(df$unemp, decreasing = T)[1]*1.2), nrow = 1, ncol = 3)
    bds = rbind(bds.natura2000,bds.area, bds.pop, bds.GDPcap, bds.tour, bds.unemp) 
    df.imp<-amelia(df, 100, 1, cs="NUTS_ID", ts="year",lags =c("natura2000", "area", "pop", "GDPcap", "tour", "unemp"), leads = c("natura2000", "area", "pop", "GDPcap", "tour", "unemp"), bounds = bds , splinetime=1, intercs=F, max.resample=100)
    #df.imp2<-amelia(df, 10, 1, cs="NUTS_ID", ts="year",lags =c("natura2000", "area", "pop", "GDPcap", "tour", "unemp"), leads = c("natura2000", "area", "pop", "GDPcap", "tour", "unemp"), bounds = bds , polytime=1, max.resample=100)
    #View(df.imp$imputations$imp1)
    plot(df.imp)
    missmap(df.imp)

#combine imputations by averaging variables over the multiple imputed data sets
    n<-1:nrow(df.imp$imputations$imp1)    
    df.imp$imputations <- lapply(df.imp$imputations, function(df) {
      df <- cbind(df, n)
    })
    require(dplyr)
    require(tidyr)    
    df.imp.avg <- rbind_all(df.imp$imputations)
    df.imp.avg<- df.imp.avg %>%
      arrange(n, NUTS_ID, year) %>%
      group_by(n, NUTS_ID) %>%
      summarise_each(funs(mean))
    df.imp.avg <- df.imp.avg[,-1]
    View(df.imp.avg)    
    length(unique(df.imp.avg$NUTS_ID))
    write.csv(df.imp.avg, "spatialpanel.csv", row.names = F)
    detach(package:Amelia)
    
# EFT for 2013
    s.df <- read.csv("spatialpanel.csv")
    #s.df$propFV_TOT[c(546, 547, 548, 549, 550, 690, 695)]<-mean(s.df$propFV_TOT[-c(546, 547, 548, 549, 550, 690, 695)])
    s.df$CF<-s.df$natura2000 + s.df$propFV_TOT*100
    s.df$EFT<-rep(0,nrow(s.df))
    for (i in 2009:2013){
      s.df[s.df$year==i,max(ncol(s.df))]<-(s.df[s.df$year==i,max(ncol(s.df))-1]/sum(s.df[s.df$year==i,max(ncol(s.df))-1]))*1000000
    }
    sum(s.df$EFT)
    sum(s.df[s.df$year==2013,max(ncol(s.df))])
    write.csv(s.df, "spatialpanel.csv", row.names = F)
    
    rm(list = ls())

# mapping --------------------------------------------------------------------
setwd("C:/Users/droste/Dropbox/Dokumente/doctorate/fiscal transfers/EFT-EU/data/")
spatialpanel <- read.csv("spatialpanel.csv")    
par(mfrow=c(1,1))
#loading spatial data
  require(sp)
  require(maps)                   
  require(maptools)
  require(latticeExtra)  
  # require(rgeos)
  # require(rgdal)
  # require(raster)
  # require(latticeExtra)

#get NUTS2013regions.shp
  NUTS2<-readShapeSpatial(paste(getwd(),"/shapefiles/NUTS2013/data/","NUTS_RG_01M_2013.shp", sep=""), proj4string = CRS("+init=epsg:4258"))
    # plot(NUTS) 
    # NUTS@proj4string
  # head(NUTS@data, max.level=2)
    # length(NUTS@data$NUTS_ID[grepl("ZZ", as.character(NUTS@data$NUTS_ID))==F & NUTS@data$STAT_LEVL_==2])
    # length(df.area$NUTS2_ID)

  #subsetting for plotting (excluding non EU-27 and overseas regions from NUTS data, also: only NUTS 2 regions)
  to.match<-c("CH01","CH02","CH03","CH04","CH05","CH06","CH07","ES70","FRA1","FRA2","FRA3","FRA4","FRA5","HR03", "HR04","IS00","LI00","ME00","MK00","NO01","NO02","NO03","NO04","NO05","NO06","NO07","PT20","PT30","TR10","TR21","TR22","TR31","TR32","TR33","TR41","TR42","TR51","TR52","TR61","TR62","TR63","TR71","TR72","TR81","TR82","TR83","TR90","TRA1","TRA2","TRB1","TRB2","TRC1","TRC2", "TRC3")
  NUTS2<-subset(NUTS2, NUTS2@data$STAT_LEVL_==2 &  grepl(paste(to.match,collapse="|"), as.character(NUTS2@data$NUTS_ID))==F)
  
  #attaching data from panel for 2013
  NUTS2<-merge(NUTS2, spatialpanel[spatialpanel$year==2013,], by="NUTS_ID", all.x=T)
  
  #tranform according to natura2000 projection
  NUTS2<-spTransform(NUTS2, CRS("+init=EPSG:3035")) 
  
#background image
  require(rworldmap)
    world <- getMap(resolution = "low")
    world <- spTransform(world, CRS("+init=EPSG:3035"))
    # plot(world)

#plotting
    #natura2000
    natura2000.cols<-colorRampPalette(c("orange", "yellow", "green", "green2", "darkgreen")) 
    natura2000.plot<-spplot(NUTS2, c("natura2000"), col.regions=natura2000.cols(20), main="Natura 2000 coverage in per cent") +
      layer(sp.polygons(world, fill="lightgrey"), under = TRUE)
    natura2000.plot
#     tiff("Fig_N2k.tif", width = 6, height = 6, units = 'in', res = 300, compression = "lzw+p")
#     natura2000.plot
#     dev.off()
    
    #natura2000 quality
    natura2000.cs.cols<-colorRampPalette(c("orangered", "yellow", "lightgreen", "green2", "green4","darkgreen")) 
    natura2000.cs.plot<-spplot(NUTS2, c("propFV_TOT"), col.regions=natura2000.cs.cols(100), main="Proportion of habitats in favorable status") +
      layer(sp.polygons(world,fill="lightgrey"), under=TRUE)
    natura2000.cs.plot
#     tiff("Fig_fvCS.tif", width = 6, height = 6, units = 'in', res = 300, compression = "lzw+p")
#     natura2000.cs.plot
#     dev.off()
    
    #control variables
    oopt <- lattice.options()
    lattice.options(layout.widths = list(key.right = list(x = .66, units = "in", data = NULL)))
    pop.cols<-colorRampPalette(c("forestgreen","gold","sienna1","darkred"))
    NUTS2@data$lpop <- log(NUTS2@data$pop)
    pop.plot<-spplot(NUTS2, c("lpop"), col.regions=pop.cols(20), ylab="ln of population density") +
      layer(sp.polygons(world.t,fill="lightgrey"), under=TRUE)
    pop.plot
    NUTS2@data$lGDPcap <- log(NUTS2@data$GDPcap)
    GDPcap.cols<-colorRampPalette(c("yellow", "darkorange","darkmagenta")) 
    BIP.plot<-spplot(NUTS2, c("lGDPcap"), col.regions=GDPcap.cols(20), ylab="ln of GDP per capita") +
      layer(sp.polygons(world.t,fill="lightgrey"), under=TRUE) +
      mapLegendGrob(obj, widths = unit(1, "cm"), heights = unit(1, "cm"), fill = "black", just = "right")
    BIP.plot
    ##NUTS2@data$ltour <- log(NUTS2@data$tour)
    tour.cols<-colorRampPalette(c( "paleturquoise","purple","midnightblue")) 
    tour.plot<-spplot(NUTS2, c("tour"), col.regions=tour.cols(20), ylab="tourist overnight stays") +
      layer(sp.polygons(world.t,fill="lightgrey"), under=TRUE)
    tour.plot
    unemp.cols<-colorRampPalette(c("royalblue","red")) 
    unemp.plot<-spplot(NUTS2, c("unemp"), col.regions=unemp.cols(20), ylab="unemployment in per cent") +
      layer(sp.polygons(world.t,fill="lightgrey"), under=TRUE)
    unemp.plot
    
    #printing plots
    require(gridExtra)
    tiff("Fig_3.tif", width = 6, height = 5, units = 'in', res = 300, compression = "lzw+p")
    grid.arrange(pop.plot, BIP.plot, tour.plot, unemp.plot, nrow=2, ncol=2)
    dev.off()
    lattice.options(oopt)
    detach(package:gridExtra)
    
  
    #bioregions
    #bioreg.poly<-readShapeSpatial(paste(getwd(),"/shapefiles/bioregions/","BiogeoRegions2011.shp", sep=""), proj4string = CRS("+init=epsg:3035"))      # require(rgeos)
    require(raster)
    # breg <- gIntersection(bioreg.poly, NUTS2, byid=F)
    #     breg2 <- crop(bioreg.poly, NUTS2)
    #     breg2@data$NAME<-factor(breg2@data$NAME)
    #     breg2@data$ABBRE<-factor(breg2@data$ABBRE)
    #     breg2@data$code<-factor(breg2@data$code)
    #     breg2@data$label<-factor(breg2@data$label)
    #     writeSpatialShape(breg2, paste(getwd(),"/shapefiles/bioregions/","bioreg.shp", sep=""))
    bioreg <- readShapeSpatial(paste(getwd(),"/shapefiles/bioregions/","bioreg.shp", sep=""), proj4string = CRS("+init=epsg:3035"))
    require(colorspace)
    breg.cols<-terrain_hcl(8)
    bioreg.plot<-spplot(bioreg, c("ABBRE"), col.regions=breg.cols, col="NA", ylab="bio-geographical regions", colorkey = T) +
      layer(sp.polygons(NUTS2, col.regions=0, col="black"), under=F) +
      layer(sp.polygons(world,fill="lightgrey"), under=T)
    bioreg.plot
    #print
    tiff("Fig_4.tif", width = 6, height = 6, units = 'in', res = 300, compression = "lzw+p")
    bioreg.plot
    dev.off()
    detach(package:raster)
    
    #EFT
    EFT.cols<-colorRampPalette(c("yellow", "green", "darkgreen")) 
    EFT.plot<-spplot(NUTS2, c("EFT"), col.regions=EFT.cols(18), main="EFT fund of 1 M distruted in EU-27") +
      layer(sp.polygons(world,fill="lightgrey"), under=TRUE)
    EFT.plot
#     tiff("Fig_EFT.tif", width = 6, height = 6, units = 'in', res = 300, compression = "lzw+p")
#     EFT.plot
#     dev.off()
    
    hist(s.df.2013$EFT, main="Histogramm of EFT in EU-27", xlab="EFT flows of a 1 M fund")
    lines(x=c(mean(s.df.2013$EFT),mean(s.df.2013$EFT)), y=c(0,mean(s.df.2013$EFT)), col="black", lwd=3, lty=2)
    
    require(gridExtra)
    tiff("Fig_1.tif", width = 18, height = 6, units = 'in', res = 300, compression = "lzw+p")
    grid.arrange(natura2000.plot, natura2000.cs.plot, EFT.plot, nrow=1, ncol=3)
    dev.off()
    detach(package:gridExtra)
    
  detach(package:rworldmap)
  detach(package:maps)                   
  detach(package:maptools)
  detach(package:sp)
  detach(package:latticeExtra)  
  rm(list = ls())

# spatial regressions -----------------------------------------------------

  s.df <- read.csv("spatialpanel.csv")     
  #subsetting by excluding those with no neighbours
  #to.match2<-c("CY00", "EL41", "EL42", "EL43", "EL62", "ES53", "ES63", "ES64", "FI20", "FR83", "ITG1", "ITG2", "MT00")
  #NUTS2.n0n<-subset(NUTS2, grepl(paste(to.match2,collapse="|"), as.character(NUTS2@data$NUTS_ID))==F)    
  
  #compute spatial weights matrix  
  require(spdep)
#   nbs<-poly2nb(NUTS2, row.names = NUTS2@data$NUTS_ID, queen = T)
#   summary(nbs)
#   #   write.nb.gal(nbs, "nbs")
#   #W/o 0 neigbours
#   nbs.n0n<-poly2nb(NUTS2.n0n, row.names = NUTS2.n0n@data$NUTS_ID, queen = T)
#   summary(nbs.n0n)
#   #   write.nb.gal(nbs.n0n, "nbs.n0n")
  
  nbs <- read.gal("nbs")
  nbs.n0n <- read.gal("nbs.n0n")
  
  # spoint<-SpatialPoints(NUTS2, proj4string=CRS("+init=EPSG:3035"))
  # summary(spoint)
  # dlist<-nbdists(nbs, spoint)
  # summary(dlist)
  # nbl<-nb2listw(nbs, style="B",zero.policy = TRUE)
  # summary(nbl,  zero.policy = TRUE)
  nbm<-nb2mat(nbs, style="W", zero.policy = TRUE)
  dim(nbm)
  nbm.n0n<-nb2mat(nbs.n0n, style="W", zero.policy = TRUE)
  dim(nbm.n0n)
  colnames(nbm)<-rownames(nbm)
  colnames(nbm)
  colnames(nbm.n0n)<-rownames(nbm.n0n)
  colnames(nbm.n0n)
  #can.be.simmed(nbl)
  
  #s.df<-read.csv("spatialpanel.csv") #View(s.df)
  l<-mat2listw(nbm)
  l0<-mat2listw(nbm.n0n)
  
  moran.test(s.df$natura2000[s.df$year==2013], l, zero.policy = T)
  moran.plot(s.df$natura2000[s.df$year==2013], l, zero.policy = T)
  
  moran.test(s.df.sub$natura2000[s.df.sub$year==2013], l0, zero.policy = T)
  moran.plot(s.df.sub$natura2000[s.df.sub$year==2013], l0, zero.policy = T)  
  
  
#   #alternative way to get spatial weight matrix
#   require(McSpatial)
#   ?makew
#   smat<-makew(shpfile = NUTS2.n0n, method = "queen")
#   #View(smat$wmat)
#   dim(smat$wmat)
#   # colnames(smat$wmat)
#   # rownames(smat$wmat)
  
  require(splm)
  require(plm)
  require(lmtest)
  require(sandwich)
  
  #do for and without those with 0 neighbours
  s.df<-read.csv("spatialpanel.csv") #View(s.df)
#   s.df.sub<-subset(s.df, grepl(paste(to.match2,collapse="|"), as.character(s.df$NUTS_ID))==F)
#   write.csv(s.df.sub, "s.df.sub", row.names = F)
  s.df.sub<-read.csv("s.df.sub") #n0ns
  length(levels(s.df$NUTS_ID))
  length(levels(s.df.sub$NUTS_ID))
  
#   pairs(s.df)
#   source("C:/Users/droste/Dropbox/Dokumente/doctorate/R/scripts/panel_cor.R")
#   pairs(natura2000~area+pop+GDPcap+tour+unemp, data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
#   pairs(log(natura2000)~log(area)+log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  pairs(natura2000~log(area)+log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  pairs(natura2000~area+pop+I(pop^2)+GDPcap+I(GDPcap^2)+tour+unemp, data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  # exclude area
  pairs(log(natura2000)~log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  pairs(natura2000~log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  pairs(natura2000~log(pop)+log(GDPcap)+tour+unemp, data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  
  #functions for without 0 neigbour polygons
  reg1<-spml(natura2000~area+pop+GDPcap+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T)
  summary(reg1)
  #coeftest(reg1, vcov=vcovSCC)
  reg2<-spml(natura2000~area+pop+GDPcap+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="twoways",lag=T)
  summary(reg2)
  reg3<-spml(natura2000~area+pop+GDPcap+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="random", effect="individual",lag=T, spatial.error = "kkp") 
  summary(reg3)
  reg4<-spml(natura2000~pop+GDPcap+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="twoways",lag=T)
  summary(reg4)

  #non-linear
  reg.nl1<-spml(natura2000~area+pop+I(pop^2)+GDPcap+I(GDPcap^2)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T)
  summary(reg.nl1)
  reg.nl2<-spml(natura2000~area+pop+I(pop^2)+GDPcap+I(GDPcap^2)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="twoways",lag=T)
  summary(reg.nl2)
  # reg.nl3<-spml(natura2000~area+pop+I(pop^2)+GDPcap+I(GDPcap^2)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="random", effect="individual",lag=T, spatial.error = "kkp")  #doesn't work
  # summary(reg.nl3) 
  reg.nl4<-spml(natura2000~log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T) #doesn't work
    summary(reg.nl4)
  reg.nl5<-spml(natura2000~log(pop)+log(GDPcap)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T, method = "eigen", tol.solve = 1e-10, control = list(), legacy = FALSE)
  summary(reg.nl5)
  reg.nl6<-spml(natura2000~log(pop)+log(GDPcap)+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data=s.df.sub, listw=mat2listw(nbm.n0n), model="random", effect="individual",lag=T, tol.solve = 1e-10, control = list(), legacy = FALSE, spatial.error = "kkp")
  summary(reg.nl6)
  reg.nl7<-spml(natura2000~log(pop)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T, tol.solve = 1e-10, control = list(), legacy = FALSE, spatial.error = "kkp")
  summary(reg.nl7)
  reg.nl8<-spml(natura2000~log(pop)+log(GDPcap)+tour+unemp, data=s.df.sub, listw=mat2listw(nbm.n0n), model="within", effect="individual",lag=T, tol.solve = 1e-10, control = list(), legacy = FALSE, spatial.error = "kkp")
  summary(reg.nl8)

  #inkl. no-neighbour islands
#   sp.reg1<-spml(natura2000~area+pop++GDPcap+tour+unemp, data=s.df, listw=mat2listw(nbm), model="within", effect="twoway", lag=T, method = "eigen", zero.policy=T, tol.solve = 1e-10, control = list(), legacy = FALSE)
#   summary(sp.reg1)
#   effects(sp.reg1)
  
  #require(stargazer)
  #stargazer(reg1, reg2, reg.nl1, reg.nl2, type="html", single.row = T)
  
  
#plm way
  pairs(natura2000~area+pop+GDPcap+tour+unemp,data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  library(car)
  pairs(natura2000~log(area)+log(pop)+GDPcap+tour+unemp,data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  
  require(plm)#   scatterplot(natura2000~area, s.df)
#   scatterplot(natura2000~log(area), s.df)
#   scatterplot(natura2000~pop, s.df)
#   scatterplot(natura2000~log(pop), s.df)
#   scatterplot(natura2000~GDPcap, s.df)
#   scatterplot(natura2000~log(GDPcap), s.df)
#   scatterplot(natura2000~tour, s.df)
#   scatterplot(natura2000~log(tour), s.df)
#   scatterplot(natura2000~unemp, s.df)
  fixed1<-plm(natura2000~area+pop+GDPcap+tour+unemp, data=s.df, effect="individual", model="within")
  summary(fixed1)
  coeftest(fixed1, vcov=vcovSCC)
  #(fixed1)
  fixed2<-plm(natura2000~area+log(pop)+GDPcap+tour+unemp, data=s.df, effect="individual", model="within")
  summary(fixed2)
  fixed3<-plm(natura2000~area+log(pop)+log(GDPcap)+tour+unemp, data=s.df, effect="individual", model="within")
  summary(fixed3)
  fixed.tw1<-plm(natura2000~area+pop+GDPcap+tour+unemp, data=s.df, effect="twoways", model="within")
  summary(fixed.tw1)
  
  fixed<-plm(natura2000~pop+GDPcap+tour+unemp, data=s.df, effect="individual", model="within")
  summary(fixed)
  coeftest(fixed, vcov=vcovSCC)
  
  random1<-plm(natura2000~area+pop+GDPcap+tour+unemp, data=s.df, effect="individual", model="random")
  summary(random1)
  coeftest(random1, vcov=vcovSCC)
  
  random<-plm(natura2000~pop+tour+unemp, data=s.df, effect="individual", model="random")
  summary(random)
  coeftest(random, vcov=vcovSCC)
  
  #source("C:/Users/droste/Dropbox/Dokumente/doctorate/R/scripts/panel_cor.R")
  pairs(natura2000~pop+tour+unemp, data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  pairs(natura2000~log(pop)+tour+log(unemp), data=s.df, upper.panel = panel.cor, lower.panel=panel.smooth)
  
  #non-lin
  fixed.nl1<-plm(log(natura2000)~log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, effect="individual", model="within")
  summary(fixed.nl1)
  coeftest(fixed.nl1, vcov=vcovSCC)
  
  random.nl1<-plm(log(natura2000)~log(pop)+log(GDPcap)+log(tour)+log(unemp), data=s.df, effect="individual", model="random")
  summary(random.nl1)
  coeftest(random.nl1, vcov=vcovSCC)
  
  fixed.nl2<-plm(natura2000~log(pop)+log(GDPcap)+tour+unemp, data=s.df, effect="individual", model="within")
  summary(fixed.nl2)
  coeftest(fixed.nl2, vcov=vcovSCC)
  
  random.nl2<-plm(natura2000~log(pop)+log(GDPcap)+tour+unemp, data=s.df, effect="individual", model="random")
  summary(random.nl2)
  coeftest(random.nl2, vcov=vcovSCC)
  
  random.nl<-plm(natura2000~log(pop)+tour+unemp, data=s.df, effect="individual", model="random")
  summary(random.nl)
  coeftest(random.nl, vcov=function(x) vcovSCC(x, type="HC0"))
  
#install.packages("geoRglm")  
#   require(geoRglm)
#   ?geoRglm
  
  

# CARTS----
  library(rpart)
  library(rpart.plot)
  library(caret)
  s.df <- read.csv("spatialpanel.csv")
  
  s.df.2013<-s.df[s.df$year==2013,]
  sum(s.df.2013$EFT)  
  
  
  # Cross Validation
  set.seed(1234)  
  numFolds = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp = seq(0.0001,0.1,0.0001))
  tree.EFT <- train(EFT~area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data = s.df.2013, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
  #tree.EFT = rpart(EFT ~ area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data=s.df.2013, cp = cp.get$bestTune)
  par(mfrow=c(1,1))
  prp(tree.EFT$finalModel)
  summary(tree.EFT$finalModel)
  tiff("Fig_2.tif", width = 7, height = 4, units = 'in', res = 300, compression = "lzw+p")
  prp(tree.EFT$finalModel)
  dev.off()
  
  require(randomForest)
  set.seed(1234)
  rf.get <- train(EFT ~ area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data = s.df.2013, method = "rf", trControl=trainControl(method="cv",number=10), tuneGrid=(expand.grid(mtry = c(3,13))), num.trees = 10000, prox=TRUE, allowParallel=TRUE)
  par(mfrow=c(1,1))
  varImpPlot(rf.get$finalModel, main="Variable Importance in Random Forest Model")
  tiff("Fig_6.tif", width = 7, height = 4, units = 'in', res = 300, compression = "lzw+p")
  varImpPlot(rf.get$finalModel, main="Variable Importance in Random Forest Model")
  dev.off()

  library(forestFloor)  
  rf <- randomForest(EFT ~ area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE,data = s.df.2013, sampsize=250, ntree=100, keep.inbag =T)
  ff = forestFloor(rf,s.df.2013)
  plot(ff,1:18,col=fcol(ff))
  
  tiff("Fig_5.tif", width = 7, height = 4, units = 'in', res = 300, compression = "lzw+p")
  hist(s.df.2013$EFT, main = "Histogram of EFT payments", xlab = "EFT payments")
  lines(c(mean(s.df.2013$EFT), mean(s.df.2013$EFT)), c(0, 45) , col = "black", lty=2, lwd = 2)
  dev.off()

#   library(REEMtree)
#   library(rpart.plot)
#   REEM.tree0= REEMtree(natura2000 ~ area+pop+GDPcap+tour+unemp, data=s.df, random=~1|NUTS_ID, method="REML")
#   prp(tree(REEM.tree0))
#   logLik(REEM.tree0)
#   
#   REEM.tree= REEMtree(natura2000 ~ area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data=s.df, random=~1|NUTS_ID, method="REML")
#   prp(tree(REEM.tree))
#   logLik(REEM.tree)
#   
#   REEM.tree1= REEMtree(natura2000 ~ propFV_TOT+area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data=s.df, random=~1|NUTS_ID, method="REML")
#   prp(tree(REEM.tree1))
#   logLik(REEM.tree1)
#   
#   REEM.tree.qual= REEMtree(propFV_TOT ~ natura2000+area+pop+GDPcap+tour+unemp+ALP+ATL+BLK+BOR+CON+MED+PAN+STE, data=s.df, random=~1|NUTS_ID, method="ML")
#   prp(tree(REEM.tree.qual))
#   
  
#   trial <- glm(propFV_TOT ~ natura2000+area+pop+GDPcap+tour+unemp, data=s.df[s.df$year==2013,], family = "gaussian")
#   summary(trial)
  
  #require(vcrpart)
  #m1 <- olmm(natura2000 ~ area+pop+GDPcap+tour+unemp+propFV_TOT + re(1|NUTS_ID), data = marijuana, family = cumulative())

  
#summary table ----
  require(stargazer)
  stargazer(s.df.2013[c("EFT","area","pop","GDPcap","tour","unemp","ALP","ATL","BLK","BOR","CON","MED","PAN","STE")], type = "html", title="Descriptive statistics", digits=1, out="summary.html",covariate.labels=c("Ecological fiscal transfer payments (EFT)","area in square km (area)","population density (pop)","GDP per capita (GDPcap)", "tourist overnight stays (tour)","unemployment rate (unemp)","Alpine region (ALP)","Atlantic region","Black Sea region (BLK)","Boreal region (BOR)","Continental region (CON)","Meditarranean region (MED)","Pannonian region (PAN)","Steppic region (STE)"), notes = "Source: authors computation based on European Environment Agency (2015) and Eurostat (2015), monetary values are in purchasing power standards (PPS) per inhabitant, except for EFT payments which are based on an arbitrary fund size and rather stand for distributive patterns.")
  
  