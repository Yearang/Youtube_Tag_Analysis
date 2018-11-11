rm(list=ls())

#### 카테고리별 태그분석 -> Howto & Style 예시 ####
library(readxl)
# excel에서 date 에 자동필터걸어서 공백인 열 제거
youtube <- read.csv("C:/Users/고예랑/Desktop/유투브 태그분석/USvideos_save.csv", header=T, quote = "") 

library(jsonlite)
category <- fromJSON("C:/Users/고예랑/Desktop/유투브 태그분석/US_category_id.json")
category <- data.frame(category)
View(category)

# 카테고리 생성
category_t <- as.data.frame(table(youtube$category_id))
library(dplyr)
cname <- table(youtube$category_id) %>% names
cna <- c()
for(i in 1:length(cname)){
  cna[i] <- category$items.snippet$title[category$items.id == cname[i]]
}
# category

category_t$cname <- cna
category_order <- category_t[order(category_t$Freq,decreasing = T),]

cid <- category_order$Var1
categ_dat <- list()
for(i in 1:length(cid)){
  categ_dat[[i]] <- youtube[youtube$category_id == cid[i],]
}

categ_dat[[1]]$category_id

# 유투브 테그 분석
tags <- as.character(categ_dat[[4]]$tags)
head(tags)

tags <- tolower(tags)
tag <- strsplit(tags, '/')
tag[[2]]

# tag를 단어로 쪼개서 저장
tag_word <- list()
for(i in 1:length(tag)){
  tag_word[[i]] <- unlist(strsplit(tag[[i]], " "))
}

# tag의 빈도수 저장
for(i in 1:length(tag_word)){
  tag_word[[i]]<-as.data.frame(table(tag_word[[i]]))
}
tag_word[[1]][]
for(i in 1:length(tag_word)) {
  categ_dat$reptag1[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[1],1])
  categ_dat$reptag2[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[2],1])
  categ_dat$reptag3[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[3],1])
}

# 태그를 한줄로 만들기
r1 <- categ_dat$reptag1
r2 <- categ_dat$reptag2
r3 <- categ_dat$reptag3
tagbind <- rbind(r1,r2,r3)

library(stringr)
## 불필요한 문자제거
tagbind <- gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", tagbind) 
tagbind <- str_replace_all(tagbind, "[^[:alnum:]]", " ") 

head(tagbind)
tablereptag <- sort(table(tagbind), decreasing = T)

library(SnowballC)
library(RColorBrewer)
library(RCurl)
library(XML)
library(wordcloud)
library(RColorBrewer)
# install.packages("RColorBrewer")
wordcloud(rownames(tablereptag), tablereptag, min.freq =4, scale=c(5, .2), random.order = FALSE, 
          colors = "grey1")#colors= c("indianred1","indianred2","indianred3","indianred"))

# 이름붙이기
title <- text(x=0.5, y=1.0, category_order$cname[4])
