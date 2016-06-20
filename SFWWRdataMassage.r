input<-"C:/Users/brdavis/Documents/Projects/Walla_Walla/ProgMark/bulltrout2002-2015 5.21.2016.csv" # path to data set
output<-"C:/Users/brdavis/Documents/Projects/Walla_Walla/ProgMark/sfwwrDATA.RData" # not yet created file called "SFWWRbarker.inp" in specified folder
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
save(d,file=output)