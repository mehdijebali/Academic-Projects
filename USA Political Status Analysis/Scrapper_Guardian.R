library(tm)
library(rvest)
library(XML)
library(igraph)
library(ggraph)
library(tidytext)
library(ggplot2)
library(tidyr)
library(dplyr)
library(wordcloud)
library(stringr)
library(reshape2)
library(widyr)
########### WEB SCRAPING  ############
strtail <- function(s,n=1) {
if(n<0) 
substring(s,1-n) 
else 
substring(s,nchar(s)-n+1)
}
titles<-""
for (j in c("jan","feb","mar","apr","may","jun","jul","aug","sep")) {
if ( j %in% c("jan","mar","may","jul","aug","oct","dec")){m<-31}
else if(j %in% c("apr","jun","sep","nov")){m<-30}
else {m<-28}
for (i in 1:m){
if (i<10) {
link<-paste('https://www.theguardian.com/world/2017/',j,sep="")
link<-paste(link,'/0',sep="")
link<-paste(link,toString(i),sep="")
link<-paste(link,'/all',sep="")
}
else {link<-paste('https://www.theguardian.com/world/2017/',j,sep="")
link<-paste(link,'/',sep="")
link<-paste(link,toString(i),sep="")
link<-paste(link,'/all',sep="")
}
page<-read_html(link)
t<-html_attr(html_nodes(page, "a"), "href")
titles<-rbind(titles,t)
}
}
i<-1
while (i<=length(titles)){
if (is.na(titles[i])){
titles<-titles[-c(i)]
i<-i-1
}
else if (titles[i] %in% c("https://www.facebook.com/theguardian","https://www.theguardian.com/global-development","","https://www.theguardian.com/cities","https://advertising.theguardian.com","https://www.theguardian.com/info/tech-feedback","https://securedrop.theguardian.com/","https://www.theguardian.com/info/complaints-and-corrections","https://www.theguardian.com/help/contact-us","https://www.theguardian.com/info/privacy","https://www.theguardian.com/preference/edition/uk","#maincontent","https://www.theguardian.com/help/terms-of-service","https://twitter.com/guardian","https://www.theguardian.com/index/contributors","https://www.theguardian.com/preference/edition/au","https://www.theguardian.com/preference/edition/us","https://theguardian.newspapers.com","https://www.theguardian.com/index/subjects/a","https://membership.theguardian.com/supporter?INTCMP=mem_int_web_newheader&acquisitionData=%7B\"source\":\"GUARDIAN_WEB\",\"componentType\":\"ACQUISITIONS_HEADER\",\"componentId\":\"mem_int_web_newheader\",\"campaignCode\":\"mem_int_web_newheader\"%7D","https://www.theguardian.com/international","https://www.theguardian.com/info/cookies","https://workforus.theguardian.com","https://www.theguardian.com/uk/environment","#top","https://www.theguardian.com/crosswords/series/speedy","https://jobs.theguardian.com?INTCMP=jobs_int_web_newheader","https://contribute.theguardian.com?INTCMP=gdnwb_copts_memco_dotcom_footer&acquisitionData=%7B\"source\":\"GUARDIAN_WEB\",\"componentType\":\"ACQUISITIONS_FOOTER\",\"componentId\":\"gdnwb_copts_memco_dotcom_footer\",\"campaignCode\":\"gdnwb_copts_memco_dotcom_footer\"%7D","https://www.theguardian.com/crosswords/series/azed","https://www.theguardian.com/science","https://www.theguardian.com/world","https://www.theguardian.com/video","https://www.theguardian.com/crosswords/series/cryptic","https://www.theguardian.com/video","https://www.theguardian.com/uk/business","https://www.theguardian.com/tone/obituaries","https://www.theguardian.com/crosswords/series/genius","https://subscribe.theguardian.com?INTCMP=subs_int_web_newheader&acquisitionData=%7B\"source\":\"GUARDIAN_WEB\",\"componentType\":\"ACQUISITIONS_HEADER\",\"componentId\":\"subs_int_web_newheader\",\"campaignCode\":\"subs_int_web_newheader\"%7D","https://membership.theguardian.com/supporter?INTCMP=NGW_FOOTER_INT_GU_MEMBERSHIP&acquisitionData=%7B\"source\":\"GUARDIAN_WEB\",\"componentType\":\"ACQUISITIONS_FOOTER\",\"componentId\":\"NGW_FOOTER_INT_GU_MEMBERSHIP\",\"campaignCode\":\"NGW_FOOTER_INT_GU_MEMBERSHIP\"%7D","https://www.theguardian.com/crosswords","https://www.theguardian.com/crosswords/series/everyman","https://www.theguardian.com/podcasts","https://www.theguardian.com/uk-news","https://www.theguardian.com/uk/technology","https://www.theguardian.com/crosswords/series/weekend-crossword","https://www.theguardian.com/crosswords/series/quick","https://www.theguardian.com/membership","https://www.theguardian.com/crosswords/series/quiptic","https://www.theguardian.com/crosswords/series/crossword-editor-update","https://www.theguardian.com/theobserver/magazine","https://www.theguardian.com/crosswords/series/prize","https://www.theguardian.com/crosswords/crossword-blog","https://www.theguardian.com/theobserver/new-review","https://www.theguardian.com/theobserver/news/comment","https://www.theguardian.com/theobserver","https://www.theguardian.com/theguardian/guardianreview","https://www.theguardian.com/theguardian/theguide","https://www.theguardian.com/theguardian/weekend","https://www.theguardian.com/theguardian/g2","https://www.theguardian.com/theguardian","https://www.theguardian.com/observer","https://www.theguardian.com/guardian-professional","https://www.theguardian.com/money/work-and-careers","https://www.theguardian.com/money/debt","https://www.theguardian.com/money/pensions","https://www.theguardian.com/money/savings","https://www.theguardian.com/money/property","https://www.theguardian.com/uk/money","https://www.theguardian.com/travel/usa","https://www.theguardian.com/travel/europe","https://www.theguardian.com/travel/uk")) {
titles<-titles[-c(i)]
i<-i-1
}
else if (titles[i] %in% c("https://www.theguardian.com/travel/uk","https://www.theguardian.com/environment/pollution","https://www.theguardian.com/uk/travel","https://www.theguardian.com/environment/energy","https://www.theguardian.com/environment/wildlife","https://www.theguardian.com/environment/climate-change","https://www.theguardian.com/fashion","https://www.theguardian.com/lifeandstyle/home-and-garden","https://www.theguardian.com/lifeandstyle/women","https://www.theguardian.com/lifeandstyle/family","https://www.theguardian.com/lifeandstyle/love-and-sex","https://www.theguardian.com/lifeandstyle/health-and-wellbeing","https://www.theguardian.com/tone/recipes","https://www.theguardian.com/lifeandstyle/food-and-drink","https://www.theguardian.com/business/eurozone","https://www.theguardian.com/business/stock-markets","https://www.theguardian.com/business/retail","https://www.theguardian.com/uk/lifeandstyle","https://www.theguardian.com/business/banking","https://www.theguardian.com/business/economics","https://www.theguardian.com/music/classicalmusicandopera","https://www.theguardian.com/stage","https://www.theguardian.com/artanddesign","https://www.theguardian.com/books","https://www.theguardian.com/technology/games","https://www.theguardian.com/music","https://www.theguardian.com/music","https://www.theguardian.com/uk/film","https://www.theguardian.com/uk/tv-and-radio","https://www.theguardian.com/uk/culture","https://www.theguardian.com/tone/editorials","https://www.theguardian.com/tone/letters","https://www.theguardian.com/uk/commentisfree","https://www.theguardian.com/football/teams","https://www.theguardian.com/football/fixtures","https://www.theguardian.com/football/results","https://www.theguardian.com/football/competitions","https://www.theguardian.com/football/tables","https://www.theguardian.com/football/live","https://www.theguardian.com/football","https://www.theguardian.com/sport/rugbyleague","https://www.theguardian.com/sport/horse-racing","https://www.theguardian.com/sport/boxing","https://www.theguardian.com/sport/cycling","https://www.theguardian.com/sport/golf","https://www.theguardian.com/sport/tennis","https://www.theguardian.com/sport/formulaone","https://www.theguardian.com/sport/rugby-union","https://www.theguardian.com/sport/cricket","https://www.theguardian.com/uk/sport","https://www.theguardian.com/world/middleeast","https://www.theguardian.com/world/africa","https://www.theguardian.com/australia-news","https://www.theguardian.com/world/asia","https://www.theguardian.com/world/americas","https://www.theguardian.com/us-news","https://www.theguardian.com/world/europe-news","https://www.theguardian.com/uk/northernireland","https://www.theguardian.com/uk/wales","https://www.theguardian.com/uk/scotland","https://www.theguardian.com/law","https://www.theguardian.com/society","https://www.theguardian.com/uk/media","https://www.theguardian.com/education","https://www.theguardian.com/business/technology","https://www.theguardian.com/technology/internet","https://www.theguardian.com/technology/alphabet","https://www.theguardian.com/technology/google", "https://www.theguardian.com/us-news/san-bernardino-shooting","https://www.theguardian.com/world/privacy","https://www.theguardian.com/technology/2016/feb/12/altdate","https://www.theguardian.com/technology/apple","https://www.theguardian.com/technology/2016/feb/10/all","https://www.theguardian.com/technology/encryption","https://www.theguardian.com/technology/self-driving-cars","https://www.theguardian.com/technology/twitter","https://www.theguardian.com/technology/smartphones","https://www.theguardian.com/technology/internet","https://www.theguardian.com/technology/mobilephones","https://www.theguardian.com/technology/yahoo","https://www.theguardian.com/technology/2016/feb/19/altdate","https://www.theguardian.com/technology/ces","https://www.theguardian.com/us-news/california","https://app.adjust.com/f8qm1x_8q69t7?campaign=NewHeader&amp;adgroup=Mobile&amp;creative=generic?INTCMP=apps_int_web_newheader","#footer-nav")){
titles<-titles[-c(i)]
i<-i-1
}
else if (strtail(titles[i],7)=="altdate"){
titles<-titles[-c(i)]
i<-i-1
}
else if (strtail(titles[i],3)=="all"){
titles<-titles[-c(i)]
i<-i-1
}

i<-i+1
}
i<-1
j<-2
while (i <length(titles)){
j<-i+1
while (j <=length(titles)){
if (titles[i]==titles[j]){
titles<-titles[-c(j)]
j<-j-1
}
j<-j+1
}
i<-i+1
}
for (i in 1:20){
titles<-titles[-c(i)]}
i<-1
Corp<-""
for(i in 1:length(titles)){
link<-titles[i]
page<-read_html(link)
pagetext<- page %>% html_nodes("p") %>% html_text()
Corp<-rbind(Corp,pagetext)
}
Encoding(Corp)<-"UTF-8"
write(Corp,file="text3.txt")
############## TEXT MINING##########
text<-readLines("text3.txt")
docs<-Corpus(VectorSource(text))
motssupprimes =c("monday","tuesday","wednesday","thursday","friday","saturday","sunday","januari","februari","march","april","may","june","july","august","september","october",
"november","december","gmt","bst","modifi","the")
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs <- tm_map(docs,stemDocument,language="english")
docs<-tm_map(docs,removeWords,motssupprimes)
write(docs,file="text5.txt")
dtm <- TermDocumentMatrix(docs)
writeLines(as.character(docs), con="mycorpus.txt")
text<-readLines("mycorpus.txt")
trump<-data_frame(line=1:length(text),text=text)
trump1<- trump %>% unnest_tokens(word,text)
afinn <- trump1 %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(trump1 %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          trump1 %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = line %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
bing_word_counts <- trump1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
custom_stop_words <- bind_rows(data_frame(word = c("trump"), 
                                          lexicon = c("custom")), 
                               stop_words)
trump1 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
trump1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
trump2 <- trump %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
trump2 %>%
  count(bigram, sort = TRUE)
bigrams_separated <- trump2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
bigrams_filtered %>%
  filter(word2 == "trump") %>%
  count(word1, sort = TRUE)
kaka<-bigrams_filtered %>%
  filter(word1 == "trump") %>%
  count(word2, sort = TRUE)
AFINN <- get_sentiments("afinn")
bigrams_separated %>%
  filter(word1 == "trump") %>%
  count(word1, word2, sort = TRUE)
trump_words <- bigrams_separated %>%
  filter(word1 == "trump") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

trump_words
trump_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"trump\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

trump4 <- trump %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

trump4
word_pairs <- trump4 %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs
word_pairs %>%
  filter(item1 == "trump")
word_cors <- trump4 %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_
word_cors %>%
  filter(item1 == "trump")
word_cors %>%
  filter(item1 %in% c("trump", "kim", "korea")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()
word_cors %>%
  filter(item1 == "trump",item2=="fire")
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
