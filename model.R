# found quadratic term in 'year'
year<-substr(total[,2],2,5)
year<-as.data.frame(year)
typescore<-cbind(hh,score,year=year-1980)
fit1<-lm(score~.-score-历史+I(year^2),data=typescore)
summary(fit1)
ddd<-sort(fit1$coefficient,decreasing = T)
quartz()
barplot(ddd[-1],xlab="类别",ylab="系数",family="STKaiti",cex.axis = 0.5)

yearf<-as.numeric(substr(film[,2],2,5))
yearff<-as.data.frame(yearf)
typescoref<-cbind(hhf,scoref,yearff=yearff-1980)
fit2<-lm(scoref~.-scoref-科幻纪录-历史传记-惊悚喜剧+I(yearff^2),data=typescoref)
summary(fit2)
dddf<-sort(fit2$coefficient,decreasing = T)
quartz()
barplot(dddf[-1],xlab="类别",ylab="系数",family="STKaiti",cex.axis = 0.5)

typescoretv<-cbind(hhtv,scoretv)
fit3<-lm(scoretv~.-scoretv-犯罪-科幻-喜剧,data=typescoretv)
summary(fit3)
dddtv<-sort(fit3$coefficient,decreasing = T)
barplot(dddtv[-1],xlab="类别",ylab="系数",family="STKaiti",cex.axis = 0.5)

regscore[,1]<-as.numeric(as.character(regscore[,1]))
aovregscore<-aov(V1~factor(V2),data=regscore)
summary(aovregscore)
TukeyHSD(aovregscore,ordered=T)

regscorename<-names(table(regscore[,2]))
regsscore<-as.data.frame(matrix(rep(1,length(regscorename)),1))
for(i in 1:nrow(regscore)){
  k<-which(regscorename==as.character(regscore[i,2]))
  K<-rep(0,8);K[k]=1
  regsscore<-rbind(regsscore,K)
}
colnames(regsscore)<-regscorename
regsscore<-regsscore[-1,]
reggscore<-cbind(regsscore,regscore[,1])
colnames(reggscore)<-c(regscorename,'score')
lmreg<-lm(regscore[,1]~.-regscore[,1]-中国大陆,data=reggscore)
summary(lmreg)

# anova for region, Tukey HSD to find different level in region
aovtypescore<-aov(scorepro~factor(V2),data=dalei)
summary(aovtypescore)
TukeyHSD(aovtypescore,ordered=T)

daleiname<-names(table(dalei[,2]))
daleiscore<-as.data.frame(matrix(rep(1,length(daleiname)),1))
for(i in 1:nrow(dalei)){
  k<-which(daleiname==as.character(dalei[i,2]))
  K<-rep(0,8);K[k]=1
  daleiscore<-rbind(daleiscore,K)
}
colnames(daleiscore)<-daleiname
daleiscore<-daleiscore[-1,]
dlscore<-cbind(daleiscore,dalei[,1])
colnames(dlscore)<-c(daleiname,'score')
lmdl<-lm(dalei[,1]~.-dalei[,1]-综艺-score,data=dlscore)
summary(lmdl)

pjrs<-c()
pjrs<-str_extract_all(total[,4],'(.*人)')
pjrs<-gsub('人','',pjrs)
pjrs<-gsub('评价','',pjrs)
pjrsdf<-as.data.frame(pjrs)
scoredf<-as.data.frame(score)
rsscore<-cbind(pjrs,score)
rsscore<-as.data.frame((rsscore))
lmrs<-lm(score~pjrs,data = rsscore)

rsscore[,1]<-as.numeric(as.character(rsscore[,1]))
rsscore[,2]<-as.numeric(as.character(rsscore[,2]))
lmrs<-lm(score~pjrs,data = rsscore)

year<-substr(total[,2],2,5)
year<-as.data.frame(year)

score<-as.numeric(as.character(score))

totallm<-cbind(hh,score,year)
tlm<-lm(score~.-score-动作爱情-历史,data=totallm)
summary(tlm)
sum(totallm[,19])

score<-data.frame(score)#
region01<-cbind(score,rep(0,length(nrow(score))),rep(0,length(nrow(score))),
                rep(0,length(nrow(score))),rep(0,length(nrow(score))),
                rep(0,length(nrow(score))),rep(0,length(nrow(score))),
                rep(0,length(nrow(score))),rep(0,length(nrow(score))),
                rep(0,length(nrow(score))),rep(0,length(nrow(score))),rep(0,length(nrow(score))))

colnames(region01)<-c('score','chn','kor','eng','ameri',
                      'hk','jap','fran','other','program','tvseries','film')
other<-(1:nrow(total))[-guojia]
region01[chn,2]<-1;region01[kor,3]<-1;region01[eng,4]<-1;region01[ameri,5]<-1
region01[hk,6]<-1;region01[jap,7]<-1;region01[fran,8]<-1;region01[other,9]<-1
region01[program[,8],10]<-1;region01[tvseries[,8],11]<-1;region01[film[,8],12]<-1



### regression
ttttotal<-cbind(totallm[,-13],region01)
totalnarm<-ttttotal[-which(is.na(ttttotal$score)),]
totalnarm<-totalnarm[-which(is.na(totalnarm$year)),]
totalnarm$score<-as.numeric(as.character(totalnarm$score))
lmt<-lm(score~.-score-film-历史-chn+I(year^2),data=totalnarm)
summary(lmt)

totalseen<-cbind(ttttotal,seendf)
totalseen<-totalseen[-which(is.na(totalseen$score)),]
totalseen<-totalseen[-which(is.na(totalseen$year)),]
totalseen$seen<-log(totalseen$seen)
lmseen<-lm(seen~.-score-seen-film-历史-chn+I(year^2),data=totalseen)
summary(lmseen)

totalpjrs<-cbind(ttttotal,pjrsdf)
totalpjrs$pjrs<-as.numeric(as.character(totalpjrs$pjrs))
totalpjrs$pjrs<-log(totalpjrs$pjrs)
totalpjrs<-totalpjrs[-which(is.na(totalpjrs$score)),]
totalpjrs<-totalpjrs[-which(is.na(totalpjrs$year)),]
lmpj<-lm(pjrs~.-score-pjrs-film-历史-chn+I(year^2),data=totalpjrs)
summary(lmpj)

info<-cbind(scoredf,seendf,wantdf,pjrsdf)
info$score<-as.numeric(as.character(info$score))
info$want<-as.numeric(as.character(info$want))
info$want<-log(info$want)
info$pjrs<-as.numeric(as.character(info$pjrs))
info$pjrs<-log(info$pjrs)
info$seen<-log(info$seen)
lminfo<-lm(score~.-score,data=info)
summary(lminfo)

quantile(totalnarm$score)


totalnarm[,30]<-cut(totalnarm$score,breaks=c(2.3,6.7,7.5,8.2,9.9),labels=c('1','2','3','4'))
colnames(totalnarm)[30]<-'good'

####### tree based model
totalnarm[,30]<-factor(totalnarm[,30])
yi.for=as.formula("good~.")
fittree <- rpart(yi.for,data=totalnarm, method="class",
                 split = "information")
tree.carseats=tree(good~.-score,totalnarm)

######

totalnarm1 = totalnarm[sample(1:nrow(totalnarm),length(1:nrow(totalnarm))),1:ncol(totalnarm)]

totalnarm1 = totalnarm[sample(1:nrow(totalnarm),length(1:nrow(totalnarm))),1:ncol(totalnarm)]
Values= totalnarm1[,(1:29)[-18]]
Targets = decodeClassLabels(totalnarm[,30])
totalnarm2 = splitForTrainingAndTest(Values, Targets, ratio=0.15)
totalnarm3 = normTrainingAndTestSet(totalnarm2)
model = mlp(totalnarm2$inputsTrain, totalnarm2$targetsTrain, size=5, 
            learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),
            maxit=100, inputsTest=totalnarm2$inputsTest, targetsTest=totalnarm2$targetsTest) 

predictions = predict(model,totalnarm2$inputsTest)
confusionMatrix(totalnarm2$targetsTest,predictions)

# 
fit1<-lm(score~.-good-program-chn-动作爱情-历史+I(year^2),data=totalnarm)
summary(fit1)

score<-as.numeric(score)
quartz()
hist(score)

lm2<-lm(pjrs~.-pjrs-动作爱情-历史-1,data=totallm)
summary(lm2)

Fact<-factpc(t,m=8,rotation='none')
Fact
Fact1<-Factpc(t,8,rot="varimax")
Fact1$vars
Fact1$loadings
Fact1$scores
Fact1$Rank
plot.text(Fact1$scores)

seen<-c()
seen<-str_extract_all(total[,6],'(.*看过)')
seen<-gsub('人看过','',seen)
seendf[,1]<-as.numeric(as.character(log(seendf[,1])))
seenlm<-cbind(seendf,totallm)
seenlm1<-lm(seen~.-seen-动作爱情-历史,data=seenlm)
summary(seenlm1)

want<-c()
want<-str_extract_all(total[,6],'(.*想看)')
want<-gsub('人想看','',want)
wantdf<-as.data.frame(want)
wantdf[,1]<-as.numeric(as.character(log(wantdf[,1])))
wantlm<-cbind(wantdf,totallm)
wantlm1<-lm(want~.-want-历史-动作爱情,data=wantlm)
summary(wantlm1)



