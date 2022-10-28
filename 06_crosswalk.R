load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
length(unique(df$hhidpn))
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
df<-df[df$qdmode %in% c("tel","web"),]

irt<-function(z) {
    library(mirt)
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

z<-df[df$qdmode=='tel',]
z<-z[!is.na(z$cogtot27_imp2018),]
#sum(z$cogtot27_imp2018<=6)/nrow(z)
zz<-sum(z$cogtot27_imp2018<=11)/nrow(z)
tmp<-by(z$th,z$cogtot27_imp2018,mean,na.rm=TRUE)
x<-data.frame(ss=as.numeric(names(tmp)),ph=as.numeric(tmp))
x$cs.ph<-cumsum(table(z$cogtot27_imp2018))/nrow(z)
tab<-table(z$cogtot27_imp2018)/nrow(z)
tmp<-data.frame(ss=names(tab),n=as.numeric(tab))
x<-merge(x,tmp)
##
z<-df[df$qdmode=='web',]
z<-z[!is.na(z$cogtot27_imp2018),]
qu<-quantile(z$th,zz)
tmp<-by(z$th,z$cogtot27_imp2018,mean,na.rm=TRUE)
x2<-data.frame(ss=names(tmp),web=as.numeric(tmp))
x2$cs.web<-cumsum(table(z$cogtot27_imp2018))/nrow(z)
x<-merge(x,x2)

pdf("/home/bd/Dropbox/Apps/Overleaf/HRS Module D/crosswalk.pdf",width=7,height=3)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
##
col<-ifelse(x$ss==11,'red','black')
plot(x$ph,x$web,cex=2.5*x$n/max(x$n),pch=19,xlab="Phone",ylab="Web",col=col)
abline(0,1)
##
plot(x$ss,x$cs.ph,col='gray',type='l',xlab="Sum score",ylab="ECDF")
lines(x$ss,x$cs.web,col='red',type='l')
axis(side=1,col='blue',at=11:14,labels=FALSE)
legend("topleft",bty='n',fill=c("red","gray"),c("Web","Phone"))
##
plot(x$ss,x$cs.ph,col='gray',type='l',xlab="Sum score",ylab="ECDF",xlim=c(10,14),ylim=c(0,.3))
axis(side=1,col='blue',at=11:15,labels=FALSE)
lines(x$ss,x$cs.web,col='red',type='l')
xv<-which(x$ss==11)
segments(x$ss[xv],0,x$ss[xv],zz,col='gray')
segments(0,zz,x$ss[xv],zz,col='gray')
xv<-which(x$ss==12)
segments(x$ss[xv],0,x$ss[xv],x$cs.web[xv],col='red')
segments(0,x$cs.web[xv],x$ss[xv],x$cs.web[xv],col='red')
xv<-which(x$ss==13)
segments(x$ss[xv],0,x$ss[xv],x$cs.web[xv],col='red',lty=2)
segments(0,x$cs.web[xv],x$ss[xv],x$cs.web[xv],col='red',lty=2)
dev.off()

