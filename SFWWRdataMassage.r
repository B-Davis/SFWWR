input<-"C:/Users/brdavis/Documents/Projects/Walla_Walla/SFWWR/bulltrout2002-2015 5.21.2016.csv" # path to data set
output<-"C:/Users/brdavis/Documents/Projects/Walla_Walla/SFWWR/sfwwrDATA.RData" # not yet created file called "SFWWRbarker.inp" in specified folder
### Data Grab
d<-read.table(input,fill=T,sep=",",header=T,stringsAsFactors = F,na.strings = c("","NA"))[c(1:4,6,9)]
d$dt<-as.POSIXct(d$dt,"","%F %T")
x1<-split(d,d[[1]]) # data frame to list
out.sites<-unique(d[[4]])[-c(1:2)]
f<-function(x) sapply(1:nrow(x),function(y) any(x[x$dt<=x[y,"dt"],"site"] %in% (out.sites)))
add<-lapply(x1,f)
d<-Map(cbind,x1,migrant=add)# add to list
# At large Since last fl measurement, could be tagging date or recap
tmp<-lapply(d,function(x) cumsum(!is.na(x$fl.tag))) # temporary dummy variable representing time since last measured
d<-Map(cbind,d,tmp=tmp)
d<-lapply(d,function(x) split(x,x$tmp)) # nested list
p<-function(x) lapply(1:length(d[[x]]),function(y) as.numeric(round(difftime(d[[x]][[y]]$dt,d[[x]][[y]]$dt[1],units = "days")/365.25,3)))
tmp2<-lapply(1:length(d),p) 
d<-lapply(seq_along(d),function(x) Map(cbind,d[[x]],yrs.at.large=tmp2[[x]]))
m<-function(x) ifelse(x$migrant==T,
  round(((668-x[1,"fl.tag"])*(1-exp(-0.33*x$yrs.at.large))) + x[1,"fl.tag"]), # migrant growth function
  round(((578-x[1,"fl.tag"])*(1-exp(-0.15*x$yrs.at.large))) + x[1,"fl.tag"])  # non-migrant growth function
)
fl.esti<-lapply(d,function(x) lapply(x,m))
d<-lapply(seq_along(d),function(x) Map(cbind,d[[x]],fl.est=fl.esti[[x]]))
##############################
d<-do.call(rbind,lapply(d,function(x) do.call(rbind,x)))
rm(list=ls()[ls() %nin% c("output","d")])
d<-with(d,d[order(dt),-c(8,9)]) # remove some columns here
rownames(d)<-NULL
# save(d,file=output)

########################
########################
########################
########################
########################


# dd<-data.table(d)
# dd[,new:=cummax(fl.est),tag]
# dd[,new2:=fl.est-new,tag]
# unique(dd[new2 < 0,tag])
# [1] "3D9.1BF15A17D2" "3D9.1BF1B2B368" "3D9.1BF1FDA2BB" "3D9.1BF15A1710" "3D9.1BF1FDDE7E" "3D9.1BF1FCAB50" "3D9.1BF1B2A652" "3D9.1C2C54FC8C" "3D9.1C2C5504F1" "3D9.1C2C54FB75" "3D9.1C2C55038B"
# [12] "3D9.1C2C54FCE8" "3D9.1C2C54FE76" "3D9.1C2C550613" "3D9.1BF1FDCC4B" "3D9.1BF1FDD3D1" "3D9.1C2C54FB9C" "3D9.1C2C55059E" "3D9.1C2C690FC5" "3D9.1C2C68785B" "3D9.1C2C6CC3FA" "3D9.1C2C3BA833"
# [23] "3D9.1C2C6CCC9D" "3D9.1C2CBE1628" "3D9.1BF1FD968D" "3D9.1C2CC9FF35" "3D9.1C2CCE44D3" "3D9.1C2C54F7D4" "3D9.1BF1FDE69A" "3D9.1C2CBE5032" "3D9.1BF1FD05FF" "384.1B795B266B" "3D9.1C2C391EFF"
# [34] "3D9.257C69841C" "3D9.1BF1B2F1CF" "3D9.1BF1B2FAE3" "3D9.1BF1FD8EE9" "384.1B795B2701" "3D9.1C2C6C2031" "3D9.239F834BA3" "3D9.1C2C6CC93A" "384.1B795B266D" "384.1B795B26AD" "384.1B795B2662"
# [45] "384.1B795B2665" "3D9.1C2C6C2190" "3D9.1C2CBE0ED1" "3D9.1C2C68793C" "3D9.1C2CBE475B" "384.1B795B26F4" "3D9.1C2C687C7A" "384.1B795B26C6" "3D9.1C2C550444" "384.1B795B25FB" "3D9.1C2CBE3FA5"
# [56] "384.1B795B2684" "384.1B795B19B9" "384.1B795B1991" "384.1B795B191D"

