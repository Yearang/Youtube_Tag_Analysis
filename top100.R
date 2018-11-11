#######################################################################################
#### 조회순기준 정렬 top 100 #####
library(readxl)
# excel에서 date 에 자동필터걸어서 공백인 열 제거
youtube <- read.csv("C:/Users/고예랑/Desktop/유투브 태그분석/USvideos_save.csv", header=T, quote = "") 
#View(youtube)
dim(youtube)
str(youtube)
head(youtube$views)

# 조회수 기준 100위까지만 저장
view_sort <-youtube[c(order(youtube$views,decreasing = T)[1:100]), ]
head(view_sort)
#View(view_sort)
dim(view_sort)

# 유투브 테그 분석
top100tag <- as.character(view_sort$tags)
head(top100tag)
top100tag <- tolower(top100tag)
tag_top100 <- strsplit(top100tag, '/')
head(tag_top100)


# tag를 단어로 쪼개서 저장
tag_word_top100 <- list()
for(i in 1:length(tag_top100)){
  tag_word_top100[[i]] <- unlist(strsplit(tag_top100[[i]], " "))
}

# tag의 빈도수 저장
for(i in 1:length(tag_word_top100)){
  tag_word_top100[[i]]<-as.data.frame(table(tag_word_top100[[i]]))
}

# 각 동영상별로 가장 많이언급한 테그 3개를 대표태그로 저장
for(i in 1:length(tag_word_top100)) {
  view_sort$reptag1[i]<- as.character((tag_word_top100[[i]])[order(tag_word_top100[[i]]$Freq, decreasing = T)[1],1])
  view_sort$reptag2[i]<- as.character((tag_word_top100[[i]])[order(tag_word_top100[[i]]$Freq, decreasing = T)[2],1])
  view_sort$reptag3[i]<- as.character((tag_word_top100[[i]])[order(tag_word_top100[[i]]$Freq, decreasing = T)[3],1])
}

# 태그를 한줄로 만들기
r1 <- view_sort$reptag1
r2 <- view_sort$reptag2
r3 <- view_sort$reptag3
tagbind_top100 <- rbind(r1,r2,r3)

length(tagbind)
# 특수문자 제거
tagbind_top100 <- str_replace_all(tagbind_top100, "\\W"," ")
# 불필요한 문자제거
tagbind_top100 <- gsub("[[]", "//", tagbind_top100)
tagbind_top100 <- gsub("[none]]","//",tagbind_top100)
tagbind_top100 <- gsub("non","//",tagbind_top100)
tagbind_top100 <- gsub("the","//",tagbind_top100)
tagbind_top100 <- gsub("#", "//", tagbind_top100)
tagbind_top100 <- gsub("())","//",tagbind_top100)
tagbind_top100 <- gsub("'(')","//",tagbind_top100)
tagbind_top100 <- gsub("\"" , "//" ,tagbind_top100)
tagbind_top100 <- gsub("//","",tagbind_top100)
tagbind_top100 <- gsub("[\U4E00-\U9FFF\U3000-\U303F]", "", tagbind_top100) 
tagbind_top100 <- str_replace_all(tagbind_top100, "[^[:alnum:]]", " ") 
tagbind_top100 <- str_replace_all(tagbind_top100, "[가-핳]", " ") 
head(table(tagbind_top100),30)



# 태그 빈도
tablereptag <- sort(table(tagbind_top100), decreasing = T)
head(tablereptag,15)
class(tablereptag)

# 워드클라우드
# install.packages("tm")
library("tm")
library("wordcloud")

# 워드클라우드 그리기
wordcloud(tagbind_top100, min.freq = 1, 
          colors=brewer.pal(8, "Dark2"),  
          random.color= TRUE, random.order = FALSE, max.words = 70)

wordcloud(names(tablereptag),freq=tablereptag, 
          scale=c(4,1), rot.per=0.25, min.freq = 1, max.words=70,
          colors=brewer.pal(8, "Dark2"), 
          random.color= TRUE, random.order = FALSE)

