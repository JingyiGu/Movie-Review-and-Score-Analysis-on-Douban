########### import data
name<-c()         
for (i in 1:36)   {
  name[i]<-paste0('a',i)   
}
for (i in 1:36){ 
  k<-paste0('douban',i,'.txt')  
  assign(name[i],read.csv(k,header=F))
}
totall<-c()
for(i in 1:36){
  totall<-rbind(totall,get(name[i]))
  }
total<-unique(totall)
score<-substr(total[,4],9,11)

# histogram for score
summary(score)
m<-seq(2,10,by=2)
table(cut(score,m))
mm<-table(cut(score,m))
barplot(mm)



#################### region factor

# extract region by regex
region<-c()
region<-str_extract_all(total[,3],'(制片国家/地区.*)')
kk<-str_extract_all(total[,3],'制片国家/地区: .*')
region<-gsub('制片国家/地区: ','',kk)
seqregion<-list()
region1<-as.character(region)
for ( i in 1:length(region1)){
  k<-unlist(strsplit(region1[i],' / '))
  seqregion<-c(seqregion,list(k))
}
head(seqregion)

 # score against region
guojia<-grep('法国|韩国|美国|日本|香港|英国|中国大陆',total[,3])
totalgj<-as.data.frame(total[guojia,])
othergj<-as.data.frame(total[-guojia,])
otherscoregj<-as.data.frame(score[-guojia,])

score<-as.numeric(score)
chn<-grep('中国大陆',total[,3])
chn1<-cbind(dbscore[chn],rep('中国大陆',length(chn)))
fran<-grep('法国',total[,3])
fran1<-cbind(score[fran],rep('法国',length(fran)))
kor<-grep('韩国',total[,3])
kor1<-cbind(score[kor],rep('韩国',length(kor)))
ameri<-grep('美国',total[,3])
ameri1<-cbind(score[ameri],rep('美国',length(ameri)))
jap<-grep('日本',total[,3])
jap1<-cbind(score[jap],rep('日本',length(jap)))
hk<-grep('香港',total[,3])
hk1<-cbind(score[hk],rep('香港',length(hk)))
eng<-grep('英国',total[,3])
eng1<-cbind(score[eng],rep('英国',length(eng)))
other1<-cbind(score[-guojia],rep('其他',2411))
regscore<-rbind(fran1,ameri1,kor1,jap1,hk1,eng1,chn1,other1)

# box plot for region
quartz()
regscore<-as.data.frame(regscore)
regscore[,1]<-as.numeric(as.character(regscore[,1]))
regscore[,2]<-as.character(regscore[,2],levels=)
nation<-data.frame(score=regscore[,1],nation=regscore[,2])
medi<-tapply(regscore[,1], regscore[,2], median,na.rm=T)
regscore[,2]<-factor(regscore[,2],levels=names(sort(medi)),ordered=T)
quartz()
p<-ggplot(nation,aes(x=nation,y=score))
p+geom_boxplot(varwidth=T)+
  theme(text=element_text(family="STKaiti",size=14))+
  labs(x='制片地区',y='豆瓣评分',title="制片地区对评分的影响")

#################### type factor

# extract type by regex
library(stringr)
type<-c()
type<-str_extract_all(total[,3],'(类型.*)')
kk<-str_extract_all(total[,3],'类型: .*')
type<-gsub('类型: ','',kk)
seqtype<-list()
type1<-as.character(type)
for ( i in 1:length(type1)){
  k<-unlist(strsplit(type1[i],' / '))
  seqtype<-c(seqtype,list(k))
}
head(seqtype)


# transform type to categorical variable
# build design matrix
getdesign<-function(data,words){
  x=data;y=words
  k<-table(x)
  m<-as.data.frame(matrix(rep(0,1*length(y)),1,length(y)))
  colnames(m)<-y
  for(i in 1:length(k)){
    k1<-which(colnames(m)==names(k)[i])
    m[1,k1]=k[i]}
  return(m)
}

designmatrix<-function(datalist,words){ #datalist是分词之后的列表，words是所有的分词向量
  x=datalist;y=words;ld<-c()
  for(i in 1:length(x)){
    l<-getdesign(x[[i]],y)
    ld<-rbind(ld,l)
  }
  return(ld)
}

movtype<-names(table(unlist(seqtype)))
t<-designmatrix(seqtype,movtype)
t<-t[,-ncol(t)]
t<-t[,-c(14,18,40,41,42,43,44)]
t<-t[,-17]

#sparse pca to reduce dimension
library(elasticnet)
spcat<-spca(t,K=15,type="predictor",sparse="penalty",trace=FALSE,para=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
tmmat<-as.matrix(t)
tscore<-tmmat%*%spcat$loadings
rowmax<-function(x){
  k<-rep(0,length(x))
  k[which.max(x)]<-1
  return(k)
}
hh<-as.data.frame(t(apply(tscore,1,rowmax)))
str(hh)
hh[,8]<-hh[,8]+hh[,9]
hh<-hh[,-9]
hh[,9]<-hh[,9]+hh[,10]
hh<-hh[,-10]
hh[,3]<-hh[,3]+hh[,4]
hh<-hh[,-4]

# boxplot for type
newtype<-c('喜剧',
           '动作',
           '爱情',
           '动画',
           '犯罪',
          '冒险',
           '纪录片',
           '科幻',
         '微电影',
          '恐怖',
           '剧情',
           '历史'
)

colnames(hh)<-newtype

hhnames<-c()
for(i in 1:nrow(hh)){
  hhnames[i]<-newtype[which(hh[i,]>0)]
}

typedata<-data.frame(score,hhnames)
med<-tapply(typedata[,1], typedata[,2], median,na.rm=T)
typedata[,2]<-factor(typedata[,2],levels=names(sort(med)),ordered=T)
quartz()
pp<-ggplot(typedata,aes(x=hhnames,y=score))
pp+geom_boxplot(varwidth=T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='作品类型',y='豆瓣评分',title="作品类型对评分的影响")


############## classify data into 3 groups, movie, talk show, tv series
# extract group information
total[,8]<-1:nrow(total)
zongyi<-grep('综艺',total[,5])
jishu<-grep('集数',total[,3])

tvseries<-total[jishu,]
film<-total[-jishu,]
program<-total[zongyi,]
filmzy<-film
zongyifilm<-grep('综艺',filmzy[,5])
film<-filmzy[-zongyifilm,]
tvzy<-tvseries
zongyitv<-grep('综艺',tvzy[,5])
tvseries<-tvzy[-zongyitv,]

# boxplot in each groups
scorepro<-substr(program[,4],9,11)
scoretv<-substr(tvseries[,4],9,11)
scoref<-substr(film[,4],9,11)
scorepro<-as.numeric(scorepro)
scorezy<-cbind(scorepro,rep('综艺',length(scorepro)))
scoretv<-as.numeric(scoretv)
scoreds<-cbind(scoretv,rep('电视剧',length(scoretv)))
scoref<-as.numeric(scoref)
scorefm<-cbind(scoref,rep('电影',length(scoref)))
dalei<-rbind(scorezy,scoreds,scorefm)
dalei<-as.data.frame(dalei)
str(dalei)
dalei[,1]<-as.numeric(as.character(dalei[,1]))
three<-data.frame(score=dalei[,1],ptf=dalei[,2])
quartz()
ptf<-ggplot(three,aes(x=ptf,y=score))
ptf+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='作品形式',y='豆瓣评分',title="作品形式对评分的影响")

##### region analysis in talk show group
# extract data
aaa<-c()
aaa<-str_extract_all(program[,3],'(制片国家/地区.*)')
kk<-str_extract_all(program[,3],'制片国家/地区: .*')
aaa<-gsub('制片国家/地区: ','',kk)
seqaaa<-list()
aaa1<-as.character(aaa)
for ( i in 1:length(aaa1)){
  k<-unlist(strsplit(aaa1[i],' / '))
  seqaaa<-c(seqaaa,list(k))
}
head(seqaaa)

# boxplot for region against score in talk show groups
korzy<-grep('韩国',program[,3])
korzy1<-cbind(scorepro[korzy],rep('韩国',length(korzy)))
amerizy<-grep('美国',program[,3])
amerizy1<-cbind(scorepro[amerizy],rep('美国',length(amerizy)))
chnzy<-grep('中国大陆',program[,3])
chnzy1<-cbind(scorepro[chnzy],rep('中国大陆',length(chnzy)))
regscorezy<-rbind(amerizy1,korzy1,chnzy1)
regscorezy<-as.data.frame(regscorezy)
str(regscorezy)
regscorezy[,1]<-as.numeric(as.character(regscorezy[,1]))
proreg<-data.frame(score=regscorezy[,1],zyreg=regscorezy[,2])
quartz()
proooreg<-ggplot(proreg,aes(x=zyreg,y=score))
proooreg+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='地区',y='豆瓣评分',title="综艺作品中地区对评分的影响")

##### region analysis in movie group
# extract
bbb<-c()
bbb<-str_extract_all(film[,3],'(制片国家/地区.*)')
kk<-str_extract_all(film[,3],'制片国家/地区: .*')
bbb<-gsub('制片国家/地区: ','',kk)
seqbbb<-list()
bbb1<-as.character(bbb)
for ( i in 1:length(bbb1)){
  k<-unlist(strsplit(bbb1[i],' / '))
  seqbbb<-c(seqbbb,list(k))
}
head(seqbbb)

# boxplot for region against score in movie groups
table(unlist(seqbbb))
hkf<-grep('香港',film[,3])
hkf1<-cbind(scoref[hkf],rep('香港',length(hkf)))
amerif<-grep('美国',film[,3])
amerif1<-cbind(scoref[amerif],rep('美国',length(amerif)))
chnf<-grep('中国大陆',film[,3])
chnf1<-cbind(scoref[chnf],rep('中国大陆',length(chnf)))
franf<-grep('法国',film[,3])
franf1<-cbind(scoref[franf],rep('法国',length(franf)))
japf<-grep('日本',film[,3])
japf1<-cbind(scoref[japf],rep('日本',length(japf)))
engf<-grep('英国',film[,3])
engf1<-cbind(scoref[engf],rep('英国',length(engf)))
guojiaf<-grep('法国|美国|日本|香港|英国|中国大陆',film[,3])
otherf1<-cbind(scoref[-guojiaf],rep('其他',2779))
regscoref<-rbind(amerif1,hkf1,chnf1,japf1,franf1,engf1,otherf1)
regscoref<-as.data.frame(regscoref)
str(regscoref)
regscoref[,1]<-as.numeric(as.character(regscoref[,1]))
regscoref[,2]<-as.character(regscoref[,2])
medf<-tapply(regscoref[,1], regscoref[,2], median,na.rm=T)
regscoref[,2]<-factor(regscoref[,2],levels=names(sort(medf)),ordered=T)
filmreg<-data.frame(score=regscoref[,1],freg=regscoref[,2])
quartz()
fmreg<-ggplot(filmreg,aes(x=freg,y=score))
fmreg+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='地区',y='豆瓣评分',title="电影作品中地区对评分的影响")

##### region analysis in tv series group
# extract
ccc<-c()
ccc<-str_extract_all(tvseries[,3],'(制片国家/地区.*)')
kk<-str_extract_all(tvseries[,3],'制片国家/地区: .*')
ccc<-gsub('制片国家/地区: ','',kk)
seqccc<-list()
ccc1<-as.character(ccc)
for ( i in 1:length(ccc1)){
  k<-unlist(strsplit(ccc1[i],' / '))
  seqccc<-c(seqccc,list(k))
}
head(seqccc)

# boxplot for region against score in tv series groups
table(unlist(seqccc))
kortv<-grep('韩国',tvseries[,3])
kortv1<-cbind(scoretv[kortv],rep('韩国',length(kortv)))
ameritv<-grep('美国',tvseries[,3])
ameritv1<-cbind(scoretv[ameritv],rep('美国',length(ameritv)))
chntv<-grep('中国大陆',tvseries[,3])
chntv1<-cbind(scoretv[chntv],rep('中国大陆',length(chntv)))
japtv<-grep('日本',tvseries[,3])
japtv1<-cbind(scoretv[japtv],rep('日本',length(japtv)))
engtv<-grep('英国',tvseries[,3])
engtv1<-cbind(scoretv[engtv],rep('英国',length(engtv)))
guojiatv<-grep('韩国|美国|日本|英国|中国大陆',tvseries[,3])
othertv1<-cbind(scoretv[-guojiatv],rep('其他',589))
regscoretv<-rbind(ameritv1,kortv1,chntv1,japtv1,engtv1,othertv1)
regscoretv<-as.data.frame(regscoretv)
str(regscoretv)
regscoretv[,1]<-as.numeric(as.character(regscoretv[,1]))
regscoretv[,2]<-as.character(regscoretv[,2])
medrtv<-tapply(regscoretv[,1], regscoretv[,2], median,na.rm=T)
regscoretv[,2]<-factor(regscoretv[,2],levels=names(sort(medrtv)),ordered=T)
tvsreg<-data.frame(score=regscoretv[,1],treg=regscoretv[,2])
quartz()
tvreg<-ggplot(tvsreg,aes(x=treg,y=score))
tvreg+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='地区',y='豆瓣评分',title="电视剧作品中地区对评分的影响")

##### type analysis in movie group
# extract
fff<-c()
fff<-str_extract_all(film[,3],'(类型*)')
kk<-str_extract_all(film[,3],'类型: .*')
fff<-gsub('类型: ','',kk)
seqfff<-list()
fff1<-as.character(fff)
for ( i in 1:length(fff1)){
  k<-unlist(strsplit(fff1[i],' / '))
  seqfff<-c(seqfff,list(k))
}
head(seqfff)

# transform type to 0-1 variables and build design matrix
filmtype<-names(table(unlist(seqfff)))
f<-designmatrix(seqfff,filmtype)
f<-f[,-ncol(f)]
f<-f[,-14]
f<-f[,-c(17,18,42,41,40,39)]
spcaf<-spca(f,K=15,type="predictor",sparse="penalty",trace=FALSE,para=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
fmmaf<-as.matrix(f)
fscore<-fmmaf%*%spcaf$loadings
hhf<-as.data.frame(t(apply(fscore,1,rowmax)))
hhf[,3]<-hhf[,3]+hhf[,4]
hhf<-hhf[,-4]
hhf[,7]<-hhf[,7]+hhf[,8]
hhf<-hhf[,-8]
hhf[,6]<-hhf[,6]+hhf[,8]
hhf<-hhf[,-8]

ftype<-c('喜剧',
         '动作',
         '爱情',
         '动画',
         '犯罪',
         '冒险',
         '纪录',
         '科幻',
         '恐怖',
         '微电影',
         '剧情',
         '历史')
colnames(hhf)<-ftype
hhfnames<-c()
for(i in 1:nrow(hhf)){
  hhfnames[i]<-ftype[which(hhf[i,]>0)]
}
typedataf<-data.frame(scoref,hhfnames)
typedataf[,2]<-as.character(typedataf[,2])
medf<-tapply(typedataf[,1], typedataf[,2], median,na.rm=T)
typedataf[,2]<-factor(typedataf[,2],levels=names(sort(medf)),ordered=T)
quartz()
fmtype<-ggplot(typedataf,aes(x=hhfnames,y=scoref))
fmtype+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='类型',y='豆瓣评分',title="电影作品中类型对评分的影响")

##### type analysis in tv series group
# extract
ttt<-c()
ttt<-str_extract_all(tvseries[,3],'(类型*)')
kk<-str_extract_all(tvseries[,3],'类型: .*')
ttt<-gsub('类型: ','',kk)
seqttt<-list()
ttt1<-as.character(ttt)
for ( i in 1:length(ttt1)){
  k<-unlist(strsplit(ttt1[i],' / '))
  seqttt<-c(seqttt,list(k))
}
head(seqttt)

# transform type to 0-1 variables and build design matrix
tvtype<-names(table(unlist(seqttt)))
tv<-designmatrix(seqttt,tvtype)
tv<-tv[,-ncol(tv)]
tv<-tv[,-14]
tv<-tv[,-c(14,37,36,35,34)]
spcatv<-spca(tv,K=12,type="predictor",sparse="penalty",trace=FALSE,para=c(1,1,1,1,1,1,1,1,1,1,1,1))
tmmav<-as.matrix(tv)
tvscore<-tmmav%*%spcatv$loadings
hhtv<-as.data.frame(t(apply(tvscore,1,rowmax)))
tvtype<-c('喜剧',
         '动画',
         '悬疑',
         '爱情',
         '动作',
         '犯罪',
         '奇幻',
         '纪录',
         '古装',
         '惊悚',
         '家庭',
         '科幻')
colnames(hhtv)<-tvtype
hhtvnames<-c()
for(i in 1:nrow(hhtv)){
  hhtvnames[i]<-tvtype[which(hhtv[i,]>0)]
}
typedatatv<-data.frame(scoretv,hhtvnames)
guzhuang<-grep('古装|纪录',typedatatv[,2])
typedatatv<-typedatatv[-guzhuang,]
typedatatv[,2]<-as.character(typedatatv[,2])
medtv<-tapply(typedatatv[,1], typedatatv[,2], median,na.rm=T)
typedatatv[,2]<-factor(typedatatv[,2],levels=names(sort(medtv)),ordered=T)
quartz()
tvstype<-ggplot(typedatatv,aes(x=hhtvnames,y=scoretv))
tvstype+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='类型',y='豆瓣评分',title="电视剧类型对评分的影响")  

##### type analysis in talk show group
# extract
ttp<-c()
ttp<-str_extract_all(program[,3],'(类型*)')
kkp<-str_extract_all(program[,3],'类型: .*')
ttp<-gsub('类型: ','',kkp)
seqttp<-list()
ttp1<-as.character(ttp)
for ( i in 1:length(ttp1)){
  k<-unlist(strsplit(ttp1[i],' / '))
  seqttp<-c(seqttp,list(k))
}
head(seqttp)

# transform type to 0-1 variables and build design matrix
ptype<-names(table(unlist(seqttp)))
pg<-designmatrix(seqttp,ptype)
pg<-pg[,-ncol(pg)]
spcap<-spca(pg,K=6,type="predictor",sparse="penalty",trace=FALSE,para=c(1,1,1,1,1,1))
tmmap<-as.matrix(pg)
pgscore<-tmmap%*%spcap$loadings
hhp<-as.data.frame(t(apply(pgscore,1,rowmax)))
ptype<-c('真人秀',
          '音乐',
          '脱口秀',
          '喜剧',
          '纪录',
          '儿童')
          colnames(hhp)<-ptype
          hhpnames<-c()
          for(i in 1:nrow(hhp)){
            hhpnames[i]<-ptype[which(hhp[i,]>0)]
          }
typedatap<-data.frame(scorepro,hhpnames)
typedatatv[,2]<-as.character(typedatatv[,2])
medtv<-tapply(typedatatv[,1], typedatatv[,2], median,na.rm=T)
typedatatv[,2]<-factor(typedatatv[,2],levels=names(sort(medtv)),ordered=T)
quartz()
pstype<-ggplot(typedatap,aes(x=hhpnames,y=scorepro))
pstype+geom_boxplot(varwidth = T)+
  theme(text=element_text(family="STKaiti",size=10))+
  labs(x='综艺类型',y='豆瓣评分',title="综艺类型对评分的影响") 


##### plots for quantity in each type in 5-year perod from 1980 to 2015
y1980<-grep('1962|1975|1976|1977|1978|1979|1980',total[,2])
assign('yy1980',typedata[y1980,])
yr1980<-ggplot(yy1980,aes(x=hhnames,y =..count..))
yr1980<-yr1980+geom_bar()+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='类型',y="个数",title='1980年及以前 作品类型个数')
yr1980
y1981<-grep('1981',total[,2])
yy1981<-typedata[y1981,]
yr1981<-ggplot(yy1981,aes(x=hhnames,y =..count..))
yr1981<-yr1981+geom_bar()+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='类型',y="个数",title='1981年作品类型个数')
yr1981
y1982<-grep('1982',total[,2])
yy1982<-typedata[y1982,]
yr1982<-ggplot(yy1982,aes(x=hhnames,y =..count..))
yr1982<-yr1982+geom_bar()+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='类型',y="个数",title='1982年作品类型个数')
yr1982

y2000<-grep('2000',total[,2])
yy2000<-typedata[y2000,]
yr2000<-ggplot(yy2000,aes(x=hhnames,y =..count..))
yr2000<-yr2000+geom_bar()+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='类型',y="个数",title='2000年作品类型个数')
yr2000

y2015<-grep('2015',total[,2])
yy2015<-typedata[y2015,]
yr2015<-ggplot(yy2015,aes(x=hhnames,y =..count..))
yr2015<-yr2015+geom_bar(fill='violet')+
  theme(text=element_text(family="STKaiti",size=12))+
  labs(x='类型',y="个数",title='2015年作品类型个数')
yr2015 

yyyyear<-c();yyyear<-c()
for(i in 1:35){
  
  yyyyear[i]<-paste0('y',1980+i);yyyear[i]<-paste0('yy',1980+i)
  assign(yyyyear[i],grep(as.character(1980+i),total[,2]))
  assign(yyyear[i],typedata[get(yyyyear[i]),])
} 
yyyear<-c('yy1980',yyyear)


library(animation)
oopt = ani.options(interval = 0.4, nmax =36)

# generate into animation plots

quartz()
for (i in 1:36){
  p<-ggplot(data=get(yyyear[i]),aes(x=hhnames,y=..count..))+coord_cartesian(ylim=c(0,500))+geom_bar(fill='red')+theme(text=element_text(family="STKaiti",size=12))+
    labs(x='类型',y="作品个数",title=paste0(1979+i,'年作品类型个数'))
         # waiting time for the settings
  print(p)
  ani.pause()
}

scoredf<-as.data.frame(score)
View(scoredf)
for(i in 1:36){
}

plot(density(score))





