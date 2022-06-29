library(sas7bdat)
#x0<-read.sas7bdat("h18cogmode_20220506.sas7bdat")
x<-read.sas7bdat("h18cogmode_20220613.sas7bdat")

##adding numeracy: Numeracy is alt-wave. So 2014 & 2018 (O & Q). Pay attention to "OD180S" "QD180S" "OD180M" "QD180M"
x$RD180S<-NA
x$RD180M<-NA
x$RD178S<-NA
x$RD179S<-NA
x$PD180S<-NA
x$PD180M<-NA
x$PD178S<-NA
x$PD179S<-NA
x$MD180S<-NA
x$MD180M<-NA
x$MD178S<-NA
x$MD179S<-NA
x$ND180S<-NA
x$ND180M<-NA
x$ND178S<-NA
x$ND179S<-NA
##
x$QIWMODE<-NA
x$RIWMODE<-NA

waves<-c("M","N","O","P","Q","R")

nms<-list()
for (l in waves) {
    ii<-grep(paste0("^",l),names(x))
    z<-names(x)[ii]
    n<-nchar(z)
    nms[[l]]<-substr(z,2,n)
}
tab<-table(unlist(nms))
vars<-names(tab[tab==6])

keep<-c("HHID","PN","BIRTHYR","DEGREE","GENDER","HISPANIC","RACE","QWEBCONTROL","QPROXY","QDMODE","EFTFASSIGN","RDMODE")
L<-list()
for (l in waves) {
    vars0<-vars
    test<-FALSE
    z<-x[,c(keep,paste0(l,vars0))]
    names(z)<-c(keep,vars0)
    z$wave<-l
    L[[l]]<-z
}
df<-data.frame(do.call("rbind",L))

##year
yr<-list(M=2010,N=2012,O=2014,P=2016,Q=2018,R=2020)
i<-match(df$wave,names(yr))
df$year<-unlist(yr[i])
##tidying
names(df)<-tolower(names(df))
df$hhidpn<-paste(df$hhid,df$pn,sep='')


## mode 2010-2016: miwmode niwmode oiwmode piwmode
## 1. Face-to-face
## 2. Telephone
mod<-c("f2f","tel")
df$iwmode<-mod[df$iwmode]
##
## In 2018 & 2020, we have section-specific mode indicators: qdmode rdmode WITH OPPOSITE CODING
## 1. PHONE
## 2. FTF
## 3. WEB
## 4. WEB-SMALL
mod<-c("tel","f2f","web","webS")
df$mode<-ifelse(df$wave=="Q",df$qdmode,NA)
df$mode<-ifelse(df$wave=="R",df$rdmode,df$mode)
df$mode<-mod[df$qdmode]
##
df$mode<-df$mode<-ifelse(df$wave %in% c("Q","R"),df$mode,df$iwmode)


##add pattern
L<-split(df,df$hhidpn)
f<-function(x) {
    x<-x[x$year %in% c(2014,2016,2018),]
    x<-x[order(x$year),]
    pat<-paste(x$mode,collapse=' ')
    c(unique(x$hhidpn),pat,nrow(x))
}
pat<-lapply(L,f)
pat<-data.frame(do.call("rbind",pat))
names(pat)<-c("hhidpn","pat","npat")
df<-merge(df,pat,all.x=TRUE)


save(df,file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")



