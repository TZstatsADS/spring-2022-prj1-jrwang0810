f.clean.corpus=function(docs){
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, rm_words)
  return(docs)
}


f.make.tdm=function(docs){
  tdm.all <- TermDocumentMatrix(docs) 
  #matrix <- as.matrix(tdm.all) 
  tdm.tidy=tidy(tdm.all)
  #kable(tdm.tidy[1:10,])
  tdm.overall=summarise(group_by(tdm.tidy, term), sum(count))
  #kable(tdm.overall[101:110,])
  return(tdm.overall)
}