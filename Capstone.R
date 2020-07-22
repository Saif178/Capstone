suppressWarnings(library(stringr))
suppressPackageStartupMessages(library(dplyr)) 
library(caret)
suppressWarnings(library(tau))
library(data.table)

create_item<-function(filepath){
  connect<-file(filepath,'r')
  item<-readLines(connect)
  close(connect)
  rm('connect')
  
  item<-tolower(item)
  itemc<-str_replace_all(item,"([iu]n)-([a-z])","\\1\\2")
  itemc<-str_replace_all(itemc,"([0-9])(st|nd|rd|th)","\\1")
  itemc<-str_replace_all(itemc,"\\'|\\'"," ")
  itemc<-str_replace_all(itemc,"[^a-z.' ]"," ")
  itemc<-str_replace_all(itemc,"([abiep])\\.([cdegm])\\.","\\1\\2")
  itemc<-str_replace_all(itemc,"([a-z])\\.([a-z])","\\1 \\2")
  itemc<-str_replace_all(itemc,"( [a-z])\\."," \\1 ")
  itemc<-str_replace_all(itemc,"(dr|st|rd|av|ave|blvd|ct)\\."," \\1 ")
  itemc<-str_replace_all(itemc,"\\.$","")
  itemc<-str_replace_all(itemc,"^ +| +$|","")
  itemc<-str_replace_all(itemc," {2,}"," ")
  itemc<-str_replace_all(itemc," *\\.*","\\.")
  iteml<-str_split(itemc,'\\.')
  iteml<-unlist(iteml)
  iteml<-iteml[iteml!=""]
  rm('item','itemc')
  return(iteml)
}

bl<-create_item('./final/en_US/en_US.blogs.txt')
nl<-create_item('./final/en_US/en_US.news.txt')
suppressWarnings(tl<-create_item('./final/en_US/en_US.twitter.txt'))
items<-c(bl,nl,tl)
rm('bl','nl','tl')

set.seed(42)
inLines<-createDataPartition(y=1:length(items),p=0.15,list=FALSE)
train_items<-items[inLines]
rm('items','inLines')

base1<-textcnt(train_items,method='string',split='[[:space:]]',n=1L,decreasing=TRUE)
base2<-textcnt(train_items,method='string',split='[[:space:]]',n=2L,decreasing=TRUE)
base3<-textcnt(train_items,method='string',split='[[:space:]]',n=3L,decreasing=TRUE)
rm('train_items')

unigram_dt<-data.table(text=names(base1),as.matrix(base1))
setnames(unigram_dt,'V1','count')
setnames(unigram_dt,'text','n0')
tot<-sum(unigram_dt$count)
unigram_dt<-mutate(unigram_dt,freq=round(count/tot,7))
unigram_dt$count<-NULL
setkeyv(unigram_dt,c('n0','freq'))
saveRDS(unigram_dt,'unigram_dt.RDS')

bibase_dt<-data.table(text=names(base2),as.matrix(base2))
setnames(bibase_dt,'V1','count')
bigram_dt<-bibase_dt
bigram_dt[,c('n1','n0') :=do.call(Map,c(f=c,strsplit(text," ")))]
bigram_dt<-mutate(bigram_dt,freq=round(count/base1[n1][[1]],7))
bigram_dt$text<-NULL; bigram_dt$count<-NULL
setkey(bigram_dt, n1)
bigram_dt<-bigram_dt[,lapply(.SD, function(x) head(x, 5)), by = key(bigram_dt)]
setkeyv(bigram_dt, c("n1", "freq", "n0"))
saveRDS(bigram_dt,'bigram_dt.rds')

tribase_dt<-data.table(text=names(base3),as.matrix(base3))
setnames(tribase_dt,'V1','count')
trigram_dt<-subset(tribase_dt,count>1)
trigram_dt[,c('n2','n1','n0') :=do.call(Map,c(f=c,strsplit(text," ")))]
trigram_dt<-mutate(trigram_dt,freq=round(count/base2[paste(n2, n1)][[1]],7))
trigram_dt$text<-NULL; trigram_dt$count<-NULL
setkeyv(trigram_dt, c("n2", "n1"))
trigram_dt <- trigram_dt[,lapply(.SD, function(x) head(x, 5)),by = key(trigram_dt)]
setkeyv(trigram_dt, c("n2", "n1", "freq", "n0"))
saveRDS(trigram_dt,'trigram_dt.rds')
