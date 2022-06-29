load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
## N = 3,632 (QWEBCONTROL==1 or QWEBCONTROL==2)
## To limit to 2018 self-respondents (n=2869), select cases QIWTYPE==1 and QPROXY==5
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
length(unique(df$hhidpn))
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
#df$focal<-ifelse(df$pat %in% c("tel f2f tel","tel f2f web"),1,0)
#df<-df[df$focal==1,]
df<-df[df$mode %in% c("tel","web"),]

dim(df)
load(file="/home/bd/Dropbox/projects/hrs/web/data/wt.Rdata")
df<-merge(df,wt)
dim(df)


z<-df[,nms]
f<-function(x) sum(is.na(x))/length(x)
L<-split(z,df$mode)
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
    mg<-multipleGroup(resp,mm,itemtype="gpcmIRT",group=z$mode[z$rs>0],invariance=c("slopes","intercepts","free_means","free_var"),technical=list(NCYCLES=5000))
    list(table(z$mode[z$rs>0]),coef(mg)[[2]]$GroupPars)
}


ir<-list()
ir$base<-irt(df[df$attrit==0,])

##by(df$wt,paste(df$mode,df$attrit),summary)
for (Q in c(.01,.05,.1,.2)) {
    qu<-quantile(df$wt[df$attrit==1],c(1-Q))
    df2<-df[df$mode=="web" | df$wt<=qu,]
    ir[[as.character(1-Q)]]<-irt(df2[df2$attrit==0,])
}

L<-list()
for (i in 1:10) {
    k<-rbinom(nrow(df),1,df$wt)
    L[[i]]<-irt(df[k==0 & df$attrit==0,])
}
tmp<-do.call("rbind",lapply(L,unlist))

tab<-do.call("rbind",lapply(ir,unlist))
tab<-rbind(tab,colMeans(tmp))
library(xtable)
xtable(tab)
