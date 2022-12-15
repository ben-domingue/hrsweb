load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
## N = 3,632 (QWEBCONTROL==1 or QWEBCONTROL==2)
## To limit to 2018 self-respondents (n=2869), select cases QIWTYPE==1 and QPROXY==5
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
df<-df[df$qdmode %in% c("tel","web"),]

##no attriters
df$attrit<-ifelse(df$qwebcontrol==1 & !(df$qdmode %in% c("web","webS")),1,0)
df<-df[df$attrit==0,]
##

######################################################
ff<-function(df) {
    m<-lm(std~ss+qdmode,df)
    x1<-summary(m)$coef[3,1:2]
    x2<-mean(df$std[df$qdmode=="web"],na.rm=TRUE)-mean(df$std[df$qdmode=="tel"],na.rm=TRUE)
    c(x2,x1)
}
z<-df[,nms]
df$ss<-rowMeans(z,na.rm=TRUE)
L<-list()
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
##dlr
y<-df[,paste("dlrc",1:4,sep='')]
f<-function(x) {
    x<-x[!is.na(x)]
    if (length(x)==0) NA else x
}
df$dlr<-apply(y,1,f)
df$std<-std(df$dlr)
L$dlr<-ff(df)
##imr
y<-df[,paste("imrc",1:4,sep='')]
f<-function(x) {
    x<-x[!is.na(x)]
    if (length(x)==0) NA else x
}
df$imr<-apply(y,1,f)
df$std<-std(df$dlr)
L$imr<-ff(df)
##ser7
df$std<-std(df$ser7)
L$ser7<-ff(df)
##
## df$std<-std(df$d178s)
## L$d178s<-lm(std~ss+mode,df)
## df$std<-std(df$d179s)
## L$d179s<-lm(std~ss+mode,df)
## df$std<-std(df$d180s)
## L$d180s<-lm(std~ss+mode,df)
y<-df[,c("d178s","d179s","d180s")]
df$ds<-rowSums(y,na.rm=TRUE)
df$std<-std(df$ds)
L$d<-ff(df)
##
tab1<-do.call("rbind",L)

######################################################
library(mirt)
irt<-function(z,nms0) {
    resp<-z[,nms0]
    z$rs<-rowSums(!is.na(resp))
    resp<-resp[z$rs>0,]  
    cs<-colSums(!is.na(resp))
    resp<-resp[,cs>0]
    I<-ncol(resp)
    mm<-paste('F=1
FIXED = (1-',I,',a1)
START=(1-',I,',a1,1.0)',sep='')
    mm<-mirt.model(mm)
    mg<-multipleGroup(resp,mm,itemtype="gpcmIRT",group=z$qdmode[z$rs>0],invariance=c("slopes","intercepts","free_means","free_var"),technical=list(NCYCLES=5000))
    coef(mg)$web$GroupPars
}
L<-list()
#nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
nms<-c("imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
L$dlr<-irt(df,nms0=nms)
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "ser7","d178s", "d179s", "d180s")
L$imr<-irt(df,nms0=nms)
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4","d178s", "d179s", "d180s")
L$ser7<-irt(df,nms0=nms)
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7")
L$d<-irt(df,nms0=nms)
tab2<-do.call("rbind",L)

tab<-cbind(tab1,tab2)
xtable(tab)
