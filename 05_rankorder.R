library(foreign)
xxx<-read.dta("randhrs1992_2018v2.dta",convert.factors=FALSE)
x0<-xxx[,c("hhid","pn","r13cogtot")]


load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
## N = 3,632 (QWEBCONTROL==1 or QWEBCONTROL==2)
## To limit to 2018 self-respondents (n=2869), select cases QIWTYPE==1 and QPROXY==5
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
length(unique(df$hhidpn))
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
#df$focal<-ifelse(df$pat %in% c("tel f2f tel","tel f2f web"),1,0)
#df<-df[df$focal==1,]
df<-df[df$qdmode %in% c("tel","web"),]

df<-merge(df,x0)
dim(df)


z<-df[,nms]
f<-function(x) sum(is.na(x))/length(x)
L<-split(z,df$qdmode)
lapply(L,function(x) apply(x,2,f))


library(mirt)
irt<-function(z) {
    resp<-z[,nms]
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
    fscores(mg)
}
df$th<-irt(df)[,1]

L<-split(df,df$qdmode)
ll<-lapply(L,function(x) cor(x$r13cogtot,x$th,method='spearman',use='p'))


pdf("/home/bd/Dropbox/Apps/Overleaf/HRS Module D/qq.pdf",width=7,height=3)
par(mfrow=c(1,2),mar=c(3,3,1.2,1),mgp=c(2,1,0))
qqplot(L$tel$r13cogtot,L$tel$th,ylim=range(df$th,na.rm=TRUE),
       xlab="2016 Cogtot",ylab="2018 theta, phone",
       xlim=range(df$r13cogtot,na.rm=TRUE),pch=19,col='gray',cex=.5)
legend("topleft",bty='n',title="Spearman correlation",paste(round(ll$tel,2)))
qqplot(L$web$r13cogtot,L$web$th,ylim=range(df$th,na.rm=TRUE),
       xlab="2016 Cogtot",ylab="2018 theta, web",
       xlim=range(df$r13cogtot,na.rm=TRUE),pch=19,col='red',cex=.5)
legend("topleft",bty='n',title="Spearman correlation",paste(round(ll$web,2)))
dev.off()

df$gr<-cut(df$age,c(50,60,70,80,Inf))
f<-function(df) {
    L<-split(df,df$qdmode)
    ll<-sapply(L,function(x) cor(x$r13cogtot,x$th,method='spearman',use='p'))
    c(nrow(df),ll)
}
L<-split(df,df$gr)
z<-lapply(c(list(df),L),f)
tab<-do.call("rbind",z)
library(xtable)
xtable(tab)
