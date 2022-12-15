load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
## N = 3,632 (QWEBCONTROL==1 or QWEBCONTROL==2)
## To limit to 2018 self-respondents (n=2869), select cases QIWTYPE==1 and QPROXY==5
##
df<-df[!(df$year<2018 & is.na(df$iwmode)),]
##
tmp<-df[df$wave=="P",]
ids<-tmp$hhidpn[tmp$iwmode=="f2f"]
df<-df[df$hhidpn %in% ids,]
##
tmp<-df[df$wave=="N",]
ids<-tmp$hhidpn[tmp$iwmode=="f2f"]
df<-df[df$hhidpn %in% ids,]
##
tmp<-df[df$wave=="O",]
ids<-tmp$hhidpn[tmp$iwmode=="tel"]
df<-df[df$hhidpn %in% ids,]
##
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
df<-df[df$qdmode %in% c("tel","web"),]
##no attriters
df$attrit<-ifelse(df$qwebcontrol==1 & !(df$qdmode %in% c("web","webS")),1,0)
df<-df[df$attrit==0,]
##
ids<-df$hhidpn
load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
df<-df[df$hhidpn %in% ids,]
df<-df[df$wave %in% c("N","O","P","Q"),]
table(df$wave,df$iwmode,df$qdmode)

nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
df$gr<-ifelse(df$year<2018,df$year,df$qdmode)

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
    mg<-multipleGroup(resp,mm,itemtype="gpcmIRT",group=z$gr[z$rs>0],invariance=c("slopes","intercepts","free_means","free_var"),technical=list(NCYCLES=5000))
    print(names(coef(mg)))
    mg
}
z<-irt(df)

z<-lapply(coef(z),function(x) x$GroupPars)
z1<-sapply(z,"[",1)

######################################################
##2020
##
load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
tmp<-df[df$wave=="R",]
idsR<-tmp$hhidpn[tmp$rdmode %in% c("tel")]
idsR<-idsR[!is.na(idsR)] #the 2018 ids
##
load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
df<-df[df$qdmode %in% c("tel","web"),]
##no attriters
df$attrit<-ifelse(df$qwebcontrol==1 & !(df$qdmode %in% c("web","webS")),1,0)
df<-df[df$attrit==0,]
##
ids<-df$hhidpn #the 2020 ids
##
load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
df<-df[df$hhidpn %in% intersect(ids,idsR),]
df<-df[df$wave %in% c("Q","R"),]
table(paste(df$qdmode,df$rdmode))
df$gr<-paste(df$qdmode,df$year)

nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")

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
    mg<-multipleGroup(resp,mm,itemtype="gpcmIRT",group=z$gr[z$rs>0],invariance=c("slopes","intercepts","free_means","free_var"),technical=list(NCYCLES=5000))
    print(names(coef(mg)))
    #list(table(z$mode[z$rs>0]),coef(mg)[[2]]$GroupPars)
    mg
}
z<-irt(df)

z<-lapply(coef(z),function(x) x$GroupPars)
z2<-sapply(z,"[",1)


######################################################
pdf("/home/bd/Dropbox/Apps/Overleaf/HRS Module D/trajectory.pdf",width=7,height=3)
par(mfrow=c(1,2),mar=c(3,3,1.2,1),mgp=c(2,1,0))
##
z<-z1
plot(NA,xlim=c(.8,4.2),ylim=c(-.1,.5),xaxt='n',xlab="Year",ylab="Cognitive Functioning")
mtext(side=3,adj=0,line=0.2,"A")
axis(side=1,at=1:4,c(2012,2014,2016,2018))
points(1:4,z[1:4],pch=19,type='b',col='darkgray')
text(1,z[1],pos=3,'f2f')
text(2,z[2],pos=3,'tel')
text(3,z[3],pos=1,'f2f')
text(4,z[4],pos=1,'tel')
points(4,z[5],pch=19,col='red')
text(4,z[5],pos=3,'web')
lines(3:4,c(z[3],z[5]),col='red',pch=NA,type='b')
##
z<-z2
plot(NA,xlim=c(.8,2.2),ylim=c(-.1,.5),xaxt='n',xlab="Year",ylab="Cognitive Functioning")
mtext(side=3,adj=0,line=0.2,"B")
axis(side=1,at=1:2,c(2018,2020))
points(1:2,z[1:2],pch=19,type='b',col='darkgray')
text(1,z[1],pos=3,'tel')
text(2,z[2],pos=3,'tel')
points(1:2,z[3:4],pch=19,type='b',col='red')
text(1,z[3],pos=3,'web')
text(2,z[4],pos=3,'tel')
##
dev.off()
