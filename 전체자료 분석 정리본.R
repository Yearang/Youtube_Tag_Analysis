###################################################
# 유투브 테그 분석
library(readxl)
# excel에서 date 에 자동필터걸어서 공백인 열 제거
youtube <- read.csv("C:/Users/고예랑/Desktop/유투브 태그분석/USvideos_save.csv", header=T, quote = "") 
# View(youtube)
dim(youtube)
str(youtube)
tags <- as.character(youtube$tags)
head(tags)
tags <- tolower(tags)
tag <- strsplit(tags, '/')
head(tag)
str(tag)


# tag를 단어로 쪼개서 저장
tag_word <- list()
for(i in 1:length(tag)){
  tag_word[[i]] <- unlist(strsplit(tag[[i]], " "))
}

# tag의 빈도수 저장
for(i in 1:length(tag_word)){
  tag_word[[i]]<-as.data.frame(table(tag_word[[i]]))
}

# 각 동영상별로 가장 많이언급한 테그 3개를 대표태그로 저장
for(i in 1:length(tag_word)) {
  youtube$reptag1[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[1],1])
  youtube$reptag2[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[2],1])
  youtube$reptag3[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[3],1])
}

# 태그를 한줄로 만들기
r1 <- youtube$reptag1
r2 <- youtube$reptag2
r3 <- youtube$reptag3

tagbind <- rbind(r1,r2,r3)

length(tagbind)


# 불필요한 문자제거
tagbind <- gsub("[[]", "//", tagbind)
tagbind <- gsub("[none]]","//",tagbind)
tagbind <- gsub("non","//",tagbind)
tagbind <- gsub("the","//",tagbind)
tagbind <- gsub("#", "//", tagbind)
tagbind <- gsub("())","//",tagbind)
tagbind <- gsub("'(')","//",tagbind)
tagbind <- gsub("\"" , "//" ,tagbind)
tagbind <- gsub("//","",tagbind)
tagbind <- gsub("\uAC00-\uD7A3xfe a-zA-Z0-9\\s]","",tagbind)
head(table(tagbind),30)

# 특수문자 제거
tagbind <- str_replace_all(tagbind, "\\W","")
tail(tagbind,1000)

# 태그 빈도
tablereptag <- sort(table(tagbind), decreasing = T)
head(tablereptag,15)

class(tablereptag)

library(wordcloud)
# install.packages("tm")
library("tm")


#wordcloud load
library("wordcloud")
# 워드클라우드 그리기
wordcloud(tagbind, min.freq = 1, 
          colors=brewer.pal(8, "Dark2"),  
          random.color= TRUE, random.order = FALSE, max.words = 70)

wordcloud(names(tablereptag),freq=tablereptag, 
          scale=c(4,1), rot.per=0.25, min.freq = 1, max.words=70,
          colors=brewer.pal(8, "Dark2"), 
          random.color= TRUE, random.order = FALSE)

library(wordcloud2)
# install.packages("wordcloud2")
library(RColorBrewer)
my<- brewer.pal(7,"Reds")
# table(tablereptag)
head(tablereptag,30)

wordcloud2(tablereptag, figPath = "C:/Users/고예랑/Desktop/유투브 태그분석/unnamed.png",
           color= c(my,"#bb0000"))










#### 연관성 분석 ####

#테그를 한줄한줄 읽기
tag_unlist<- unlist(tag)
class(tag_unlist)
head(tag_unlist,10)



tag_list <- as.list(tag_unlist)

tag_list <- str_replace_all(tag_list, "\\W"," ")
tag_list <- gsub("[[]", "//", tag_list)
tag_list <- gsub("[none]]","//",tag_list)
tag_list <- gsub("non","//",tag_list)
tag_list <- gsub("the","//",tag_list)
tag_list <- gsub("#", "//", tag_list)
tag_list <- gsub("())","//",tag_list)
tag_list <- gsub("'(')","//",tag_list)
tag_list <- gsub("\"" , "//" ,tag_list)
tag_list <- gsub("//", "", tag_list)

tag_list_word <- list()
for(i in 1:length(tag_list)){
  tag_list_word[[i]] <- unlist(strsplit(tag_list[[i]], " "))
}

head(tag_list_word)


filter1 <- function(x){
  nchar(x) <= 10
}

filter2 <- function(x){
  Filter(filter1, x)
}


tag_list_word1 <- sapply(tag_list_word, filter2)

tolower(tag_list_word1)

head(tag_list_word1)

tag_list_word1 <- unique(tag_list_word1)
tag_list_word1 <- sapply(tag_list_word1, unique)

# arules 패키지 설치
# install.packages("arules")
library(arules)

wordtran <- as(tag_list_word1, "transactions")
head(wordtran)

wordtable <- crossTable(wordtran)
head(wordtable)

# 단어간 연관규칙 산출
tranrule <- apriori(wordtran, parameter=list(supp=0.002, conf=0.05))
inspect(sort(tranrule))

#install.packages("arulesViz")
library(arulesViz)

# 시각화
plot(tranrule, method="graph", control = list(type="items"),
     vertex.labes.cex=1,
     edge.arrow.size=0.7,
     edge.arrow.width=1.5, max=60)

