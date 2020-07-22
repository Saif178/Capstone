suppressWarnings(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressWarnings(library(openNLP))
suppressWarnings(library(NLP))

unigram_dt<-readRDS('unigram_dt.rds')
bigram_dt<-readRDS('bigram_dt.rds')
trigram_dt<-readRDS('trigram_dt.rds')
suppressWarnings(badwords<-readLines('badWords.txt'))

sent_token<-Maxent_Sent_Token_Annotator()
word_token<-Maxent_Word_Token_Annotator()
pos_tag<-Maxent_POS_Tag_Annotator()

parse_text<-function(text1){
  tmp<-unlist(str_split(text1,' '))
  tmp<-tmp[tmp!='']
  return(tmp)
}

filter_text<-function(text1){
  tmp<-text1
  if (length(tmp)>0){
    words1<-parse_text(tmp)
    num_words<-length(words1)
    if(num_words>0){
      for(i in 1:num_words){
        if(words1[i]%in%badwords) words1[i]<-paste(substring(
                                                    words1[i],1,1),'***',sep='')
      }
      tmp_w<-paste(words1[1])
      if(num_words>1){
        for(i in 2:num_words) tmp_w<-paste(tmp_w,words1[i])
      }
      return(tmp_w)
    }
  }
  return(tmp)
}

get_word<-function(text1){
  if(text1!=' '){
    words1<-parse_text(tolower(text1))
    num_words<-length(words1)
    filter1<-paste('^',words1[num_words],sep='')
    tmp_dt<-unigram_dt[n0%like%filter1]
    pred_word<-dim(tmp_dt)[1]
    if(pred_word>0){
      tmp_dt<-tmp_dt[order(rank(-freq))]
      pred<-tmp_dt[1]$n0
      if(num_words>2){
        tmp_w<-paste(words1[1])
        for(i in 2:(num_words-1)) tmp_w<-paste(tmp_w,words1[i])
        return(paste(tmp_w,filter_text(pred)))
      }else if(num_words>1){
        tmp_w<-paste(words1[1])
        return(paste(tmp_w,filter_text(pred)))
      }
    }
  }
  return(text1)
}

clean_text <- function(text1) {
  input_str <- tolower(text1)    
  input_str <- str_replace_all(input_str, "([iu]n)-([a-z])", "\\1\\2")
  input_str <- str_replace_all(input_str, "([0-9])(st|nd|rd|th)", "\\1")
  input_str <- str_replace_all(input_str, "[^a-z.' ]", " ")
  input_str <- str_replace_all(input_str, "www\\.[a-z]+\\.[a-z]+", "")
  input_str <- str_replace_all(input_str, "\\.", " ")
  input_str <- str_replace_all(input_str, " ([a-z])\\1+ |^([a-z])\\1+ | 
                               ([a-z])\\1+$|^([a-z])\\1+$", " ")
  input_str <- str_replace_all(input_str, "([a-z])\\1{2,}", "\\1\\1")
  input_str <- str_replace_all(input_str, "\\'+([a-z]+)\\'+", "\\1")
  input_str <- str_replace_all(input_str, "\\'+ \\'+", " ")
  input_str <- str_replace_all(input_str, "(\\'+ )+|( \\'+)+|^\\'+|\\'+$",
                               " ")
  input_str <- str_replace_all(input_str, "^[a-z]+$", "")
  input_str <- str_replace_all(input_str, "( [^ai])+ |^([^ai] )+|( [^ai])+$",
                               " ")
  input_str <- str_replace_all(input_str, "^ +| +$|", "")
  input_str <- str_replace_all(input_str, " {2,}", " ")
  input_str <- str_replace_all(input_str, " +$|^ +", "")
  return(input_str)
}

get_default <- function(text) {
  if (length(text) > 0) {
    a2 <- annotate(as.String(text), list(sent_token, word_token))
    a3 <- annotate(as.String(text), pos_tag, a2)
    a3w <- subset(a3, type == "word")
    tags <- sapply(a3w$features, `[[`, "POS")
    if (tags %like% "NN") {
      return("in")
    } else if (tags %like% "VB") {
      return("a")
    } else if (tags %like% "JJ") {
      return("time")
    } else if (tags %like% "PRP") {
      return("first")
    } else if (tags %like% "CC") {
      return("i")
    } else if (text == "the") {
      return("first")
    }
  }
  return("the")
}

get_pred<-function(text1){
  if(text1!=' '){
    input_words<-parse_text(clean_text(text1))
    len<-length(input_words)
    if(len>1){
      w1<-input_words[len]
      w2<-input_words[len-1]
    }else if(len>0){
      w1<-input_words[len]
      w2<-'NA'
    }else return('the')
    l1<-0.95;l2<-0.04;l3<-0.01
    len3<-length(subset(trigram_dt,n2==w2&n1==w1)$freq)
    len2<-length(subset(bigram_dt,n1==w1)$freq)
    matches<-matrix(nrow=len3+len2,ncol=2)
    matches[,1]<-'';matches[,2]<-0
    if(len3>0){
      for(i in 1:len3){
        matches[i,1]<-subset(trigram_dt,n2==w2&n1==w1)$n0[i]
        cnt2<-length(subset(bigram_dt,n1==w1&n0==matches[i,1])$freq)
        cnt1<-length(subset(unigram_dt,n0==matches[i,1])$freq)
        freq2<-ifelse(cnt2>0,subset(bigram_dt,n1==w1&n0==matches[i,1])$freq,0)
        freq1<-ifelse(cnt1>0,subset(unigram_dt,n0==matches[i,1])$freq,0)
        matches[i,2]<-subset(trigram_dt,n2==w2&n1==w1)$freq[i]*l1
                      +freq2*l2+freq1*l3
      }
    }
    match_len<-length(matches[which.max(matches[,2])])
    if(match_len>0) return(matches[which.max(matches[,2])])
    return(get_default(w1))
  }
  return(' ')
}

