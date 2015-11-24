setwd("~/Dropbox/CSI/CSI-LIMNO_DATA/LAGOSData/Version1.054.1")

data.lake.specific = read.table("lagos_lakes_10541.txt", 
                                header = TRUE, 
                                sep = "\t", 
                                quote = "", 
                                dec = ".", 
                                strip.white = TRUE,
                                comment.char = "")

lakehu12<-data.lake.specific[,c(1,20)]


modern.15.h12id<-merge(modern.15, lakehu12, by="lagoslakeid", all.x=TRUE, all.y=FALSE)

limnohu12s<-modern.15.h12id$hu12_zoneid
limnohu12s<-as.vector(limnohu12s)

hu12list<-unique(limnohu12s)


setwd("~/Dropbox/CSI/CSI_LIMNO_Manuscripts-presentations/CSI_Nitrogen MSs/Time series/GeoTS")
write.csv(hu12list, "hu12withlimnodata.csv")
