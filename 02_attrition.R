load(file="/home/bd/Dropbox/projects/hrs/web/data/df.Rdata")

z<-df[df$year==2018,]
z$attrit<-ifelse(z$qwebcontrol==1 & !(z$qdmode %in% c("web","webS")),1,0)
table(z$attrit,z$qdmode)
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
#hold<-df[df$mode %in% c("tel","web"),]
hold<-df

table(df$attrit,df$qwebcontrol)

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
df<-merge(hold[hold$year==2018,],th,all.x=TRUE)

table(df$qdmode)
df<-df[df$qdmode %in% c("tel","web"),]


#############################################################################
##attrition descriptives
library(foreign)
xxx<-read.dta("randhrs1992_2018v2.dta",convert.factors=FALSE)
dim(xxx)

x0<-xxx[,c("hhid","pn","r14shlt","r14conde","r14mstat")]
df<-merge(df,x0,all.x=TRUE)
dim(df)


df$female<-ifelse(df$gender==2,1,0)
df$partner<-ifelse(df$r14mstat %in% c(1:3),1,0)

df$modemode<-ifelse(df$qwebcontrol==2,'phone','web')
df$modemode<-ifelse(df$attrit==0 & df$qwebcontrol==1,'complier',df$modemode)
df$modemode<-ifelse(df$attrit==1 & df$qwebcontrol==1,'non-complier',df$modemode)
df$modemode<-factor(df$modemode,ordered=TRUE,levels=c("phone","complier","non-complier"))

L<-list()
f<-function(nm,df) {
    x<-df[!is.na(df[[nm]]),]
    z<-x[[nm]]
    M<-mean(z,na.rm=TRUE)
    x[[nm]]<-(z-mean(z))/sd(z)
    ##z test
    zz<-x[x$modemode!="phone",]
    pv<-t.test(zz[[nm]][zz$modemode=="complier"],zz[[nm]][zz$modemode=="non-complier"])$p.value
    ##
    c(nrow(x),M,by(x[[nm]],x$modemode,mean,na.rm=TRUE),pv)
    }
L$age<-f('age',df)
L$female<-f('female',df)
L$degree<-f('degree',df)
L$th2016<-f('th2016',df)
L$r14shlt<-f('r14shlt',df)
L$r14conde<-f('r14conde',df)
L$partner<-f('partner',df)
tab<-do.call("rbind",L)
library(xtable)
#xtable(tab,)
print(xtable(tab,digits=2,display=c("f","d","f","f","f","f","e")))
            
#############################################################################

library(splines)
tmp<-bs(df$age)
df$age1<-tmp[,1]
df$age2<-tmp[,2]
df$age3<-tmp[,3]
m<-list()
df0<-df[df$qwebcontrol==1,]
##
df0<-df0[df0$race>0,]
##
m[["baseline"]]<-glm(attrit~1,family="binomial",df0)
m[["demo"]]<-glm(attrit~age1+age2+age3+factor(degree)+factor(female)+factor(race),family="binomial",df0)
m[["cog"]]<-glm(attrit~th2016,family="binomial",df0)
m[["all"]]<-glm(attrit~age1+age2+age3+factor(degree)+factor(female)+factor(race)+th2016+r14shlt+r14conde+partner,family="binomial",df0)
m[["nocog"]]<-glm(attrit~age1+age2+age3+factor(degree)+factor(female)+factor(race)+r14shlt+r14conde+partner,family="binomial",df0)

library(pROC)
library(imv)
pred<-data.frame(hhidpn=df$hhidpn,attrit=df$attrit,p0=predict(m[[1]],df,type='response'))
per<-au<-N<-om<-numeric()
for (i in 2:length(m)) {
    p<-predict(m[[i]],type='response')
    index<-as.numeric(names(p))
    p<-data.frame(hhidpn=df$hhidpn[index],p1=p)
    tmp<-merge(pred,p)
    tmp<-tmp[!is.na(tmp$p1),]
    #N0<-length(m[[i]]$resid)
    N[i-1]<-nrow(tmp)
    om[i-1]<-mean(imv0glm(m[[i]]))#imv(tmp$attrit,tmp$p0,tmp$p1)
    #ro<-roc(y,p1)
    au[i-1]<-pROC::auc(response=tmp$attrit,predictor=tmp$p1)
    per[i-1]<-sum(m[[i]]$y,na.rm=TRUE)
}
tab<-cbind(N,per,au,om) #export as table

library(xtable)
xtable(tab,digits=3)

#########################
p<-predict(m$nocog,df,type='response')
index<-as.numeric(names(p))
p<-data.frame(hhidpn=df$hhidpn[index],p1=p,qwebcontrol=df$qwebcontrol)
tmp<-merge(p,df[,c("hhidpn","attrit")])


wt<-tmp[,c("hhidpn","attrit")]
wt$wt<-tmp$p1
##data.frame(hhidpn=df$hhidpn,attrit=df$attrit,wt=p$p1)
save(wt,file="/home/bd/Dropbox/projects/hrs/web/data/wt.Rdata")
