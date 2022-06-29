load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")

z<-df[df$year==2018,]
z$attrit<-ifelse(z$qwebcontrol==1 & z$mode=="tel",1,0)
table(z$attrit,z$mode)
z<-z[,c("hhidpn","attrit")]
df<-merge(df,z)


## N = 3,632 (QWEBCONTROL==1 or QWEBCONTROL==2)
## To limit to 2018 self-respondents (n=2869), select cases QIWTYPE==1 and QPROXY==5
df<-df[df$wave=="Q" & df$qwebcontrol %in% 1:2,]
df<-df[df$iwtype==1 & df$proxy==5,]
length(unique(df$hhidpn))
nms<-c("dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
#df$focal<-ifelse(df$pat %in% c("tel f2f tel","tel f2f web"),1,0)
#df<-df[df$focal==1,]
hold<-df[df$mode %in% c("tel","web"),]

##get pre-2018 scores
load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")
nms<-c("hhidpn","dlrc1", "dlrc2", "dlrc3", "dlrc4", "imrc1", "imrc2", "imrc3", "imrc4", "ser7","d178s", "d179s", "d180s")
tmp<-df[df$year==2016,]
tmp<-tmp[,nms]
tmp<-tmp[,colMeans(!is.na(tmp))>0]
tmp<-tmp[rowMeans(!is.na(tmp[,-1]))>0,]
library(mirt)
m<-mirt(tmp[,-1],1)
th<-data.frame(hhidpn=tmp$hhidpn,th2016=fscores(m)[,1])
## tmp<-df[df$year==2014,]
## tmp<-tmp[,nms]
## tmp<-tmp[,colMeans(!is.na(tmp))>0]
## tmp<-tmp[rowMeans(!is.na(tmp[,-1]))>0,]
## library(mirt)
## m<-mirt(tmp[,-1],1)
## tmp<-data.frame(hhidpn=tmp$hhidpn,th2014=fscores(m)[,1])
## th<-merge(th,tmp,all=TRUE)
df<-merge(hold,th,all.x=TRUE)

library(splines)
tmp<-bs(df$age)
df$age1<-tmp[,1]
df$age2<-tmp[,2]
df$age3<-tmp[,3]
m<-list()
m[["baseline"]]<-glm(attrit~1,family="binomial",df)
m[["demo"]]<-glm(attrit~age1+age2+age3+factor(degree)+factor(gender)+factor(race),family="binomial",df)
m[["cog"]]<-glm(attrit~th2016,family="binomial",df)
m[["all"]]<-glm(attrit~age1+age2+age3+factor(degree)+factor(gender)+factor(race)+th2016,family="binomial",df)

imv<-function(y,p1,p2) {
    ##
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(x)
        exp(z)
    }    
    loglik1<-ll(y,p1)
    loglik2<-ll(y,p2)
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c1<-getcoins(loglik1)
    c2<-getcoins(loglik2)
    ew<-function(p1,p0) (p1-p0)/p0
    imv<-ew(c2,c1)
    imv
}
pred<-data.frame(hhidpn=df$hhidpn,attrit=df$attrit,p0=predict(m[[1]],type='response'))
au<-N<-om<-numeric()
for (i in 2:length(m)) {
    p<-predict(m[[i]],type='response')
    index<-as.numeric(names(p))
    p<-data.frame(hhidpn=df$hhidpn[index],p1=p)
    tmp<-merge(pred,p)
    N[i-1]<-length(m[[i]]$resid)
    om[i-1]<-imv(tmp$attrit,tmp$p0,tmp$p1)
    library(pROC)
    #ro<-roc(y,p1)
    au[i-1]<-pROC::auc(response=tmp$attrit,predictor=tmp$p1)
}
tab<-cbind(N,au,om) #export as table

library(xtable)
xtable(tab,digits=3)

p<-predict(m$demo,type='response')
index<-as.numeric(names(p))
p<-data.frame(hhidpn=df$hhidpn[index],p1=p,qwebcontrol=df$qwebcontrol)
tmp<-merge(p,df[,c("hhidpn","attrit")])
tmp<-tmp[tmp$qwebcontrol==1,]
plot(density(tmp$p1[tmp$attrit==0]))
lines(density(tmp$p1[tmp$attrit==1]),col='red')



wt<-data.frame(hhidpn=df$hhidpn,attrit=df$attrit,wt=p)
#save(wt,file="/home/bd/Dropbox/projects/hrs/web/data/wt.Rdata")
