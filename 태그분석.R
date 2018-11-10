##### iphone 시간에 따라 #####
d13 <- youtube[youtube$date==13.09,]
d14 <- youtube[youtube$date==14.09,]
d15 <- youtube[youtube$date==15.09,]
d16 <- youtube[youtube$date==16.09,]
d17 <- youtube[youtube$date==17.09,]
d18 <- youtube[youtube$date==18.09,]
d19 <- youtube[youtube$date==19.09,]
d20 <- youtube[youtube$date==20.09,]
d21 <- youtube[youtube$date==21.09,]
d22 <- youtube[youtube$date==22.09,]
d23 <- youtube[youtube$date==23.09,]
d24 <- youtube[youtube$date==24.09,]


 <- d13$tags

# 유투브 테그 분석
bb <- as.character(d24$tags)
head(bb)
bb <- tolower(bb)
tag <- strsplit(bb, '/')
head(tag)
str(tag)


# tag를 단어로 쪼개서 저장
# tag_word <- unlist(strsplit(tag[[1]], " "))

tag_word <- list()
for(i in 1:length(tag)){
  tag_word[[i]] <- unlist(strsplit(tag[[i]], " "))
}

# tag의 빈도수 저장
for(i in 1:length(tag_word)){
  tag_word[[i]]<-as.data.frame(table(tag_word[[i]]))
}


tag_word[[1]]
# as.character((tag_word[[1]])[order(tag_word[[1]]$Freq, decreasing = T)[1:3],1])

# View(youtube)
# 각 동영상별로 가장 많이언급한 테그 3개를 대표태그로 저장
for(i in 1:length(tag_word)) {
  d13$reptag1[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[1],1])
  d13$reptag2[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[2],1])
  d13$reptag3[i]<- as.character((tag_word[[i]])[order(tag_word[[i]]$Freq, decreasing = T)[3],1])
}

# 태그를 한줄로 만들기
r1 <- d13$reptag1

r2 <- d13$reptag2
r3 <- d13$reptag3

tagbind <- rbind(r1,r2,r3)


# 모두 소문자로 통일 -> 처음에 함
# tagbind <- tolower(tagbind)

length(tagbind)

# 불필요한 문자제거
tagbind <- gsub("[[]", "//", tagbind)
tagbind <- gsub("[none]]","//",tagbind)
tagbind <- gsub("non","//",tagbind)
tagbind <- gsub("the","//",tagbind)
# tagbind <- gsub("a","//",tagbind)
tagbind <- gsub("#", "//", tagbind)
tagbind <- gsub("())","//",tagbind)
tagbind <- gsub("'(')","//",tagbind)
tagbind <- gsub("\"" , "//" ,tagbind)
tagbind <- gsub("//","",tagbind)
tagbind <- gsub("\uAC00-\uD7A3xfe a-zA-Z0-9\\s]","",tagbind)
# head(table(tagbind),30)

# 특수문자 제거
# install.packages("stringr")
# library(stringr)
tagbind <- str_replace_all(tagbind, "\\W","")
# tail(tagbind,1000)

# 태그 빈도
tablereptag <- sort(table(tagbind), decreasing = T)
# tabletag <- list()

tagbind <- str_replace_all(tagbind, 'emmys', 'emmy')
# tagbind#####여기바꿔라아아ㅏ#######


tabletag[[12]] <- sort(table(tagbind),decreasing = T)[1:10]
head(tablereptag,15)
tablereptag["none"]

# makeup <-c()
# emmy <- c()
# maxico <-c()
length(iphone)
iphone <- c(10,10,11,11,11,11,11,11,11,3,3,4)
iphone[12] <- tablereptag["iphone"]
makeup[12] <- tablereptag["makeup"]
  emmy[12] <- tablereptag["emmy"]
#maxico[12] <- tablereptag["maxico"]

top4 <- cbind(iphone,makeup,emmy)
class(top4)
plot(top4)

row.names(top4) <- c("09.13","09.14","09.15","09.16","09.17","09.18","09.19","09.20","09.21","09.22","09.23","09.24")
top4$date <-  c("09.13","09.14","09.15","09.16","09.17","09.18","09.19","09.20","09.21","09.22","09.23","09.24")

plot(top4,ylab="execution time(seconds)",xlab="the number of attributes",ylim=c(0,0.57),pch=4,type="o",col="#228B22")
par(new=T)
plot(data$attr_20,time_m_enc,ylab="",xlab="",ylim=c(0,0.57),pch=0,type="o",col="#FF3030")
par(new=T)
plot(data$attr_20,time_total_enc,ylab="",xlab="",ylim=c(0,0.57),pch=2,type="o",col="#009ACD")
legend(x=16,y=0.57, c("total encryption","generation M","encypt data"), cex=0.7, pch=c(2,0,4),col=c("#009ACD","#FF3030","#228B22"))
 top4 <- as.data.frame(top4)

#출처: http://comscience.tistory.com/entry/R-하나의-그래프-영역에-여러-개의-그래프를-겹쳐서-그리기 [Five Layers]

 library(ggplot2)

 library(tidyr)
 
 View(top4)
 top3 <- top4 %>% 
   gather(key="term", value="freq","iphone", "emmy", "makeup")
 con=top3$term
 to
 ggplot(top3, aes(x = date ,y= freq, colour=term)) +
   geom_point()+
   geom_line(aes(colour=con, group=con)) #+ geom_smooth()


class(tablereptag)
# sort(as.data.frame(table(tag_word[[1]]))),decreasing = T, by="Freq")[1,]
# tag_word[order(tag_word[[1]])$Freq, decreasing = T),1]

library(wordcloud)
# install.packages("tm")
library("tm")
#create corpus
# tagbind_b <- Corpus(VectorSource(tagbind))
#clean up by removing stop words
# tweets.text.corpus <- tm_map(tagbind, function(x)removeWords(x,stopwords()))

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


tagbind

as.data.frame(tagbind[1])
t24 <- as.data.frame(tabletag[[12]])
tbind <- cbind(t13, t14, t15, t16, t17, t18, t19, t20, t21, t22,t23,t24)
install.packages("xlsx")
library(xlsx)
write.xlsx(tbind,                # R데이터명
           file="C:/Users/고예랑/Desktop/youtube/aa.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=TRUE) 
