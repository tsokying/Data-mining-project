library(dplyr)
library(jiebaR)
library(tidytext)
library(stringr)
library(ggplot2)
library(lda)
library(gsubfn)

indata <- function (dir, stopword) {
  xdir <- dir
  fnames <- list.files(path=xdir, pattern="*.txt")

  xtext <- NULL
  for (f in fnames) {
    fpath <- paste(xdir, f, sep="/")
    t0 <- readLines(fpath, warn=FALSE)
    t0 <- gsub('[0-9]+', '', t0)
    t1 <- paste(t0, collapse=" ")
    xtext <- c(xtext, t1)
  }
  xseg <- worker(stop_word=stopword)
  
  xtext2 <- NULL
  for (i in 1:length(xtext)){
    t0 <- xtext[i]
    t1 <- xseg <= t0
    xtext2 <- c(xtext2, paste0(t1, collapse=" "))
  }

  text_df <- data_frame(doc_id = 1:length(xtext2), text = xtext2)
  return(text_df)
}

wordcount <- function(text_df) {
  tok99 <- function(t) str_split(t, "[ ]{1,}")
  td1 <- unnest_tokens(text_df, word, text, token=tok99)
  td_count <- dplyr::count(td1, word, sort = TRUE)
  return(td_count)
}

tfidf_ng <-function(text_df) {
  tok99 <- function(t) str_split(t, "[ ]{1,}")
  td1 <- unnest_tokens(text_df, word, text, token=tok99)
  td2 <- td1 %>%
  dplyr::count(doc_id, word, sort=TRUE) %>%
  bind_tf_idf(word, doc_id, n) 
  td_tfidf = arrange(td2, desc(tf_idf))
  return(td_tfidf)
}

tfidf <-function(text_df) {
  tok99 <- function(t) str_split(t, "[ ]{1,}")
  td1 <- unnest_tokens(text_df, word, text, token=tok99)
  td2 <- td1 %>%
  dplyr::count(doc_id, word, sort=TRUE) %>%
  bind_tf_idf(word, doc_id, n) 
  td_tfidf = arrange(td2, desc(tf_idf))

  plot <- td_tfidf %>%
  group_by(doc_id) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  theme(text=element_text(family="Hei Regular", size=16)) +
  facet_wrap(~doc_id, scales = "free") +
  coord_flip()
  
  print(plot)
  return(td_tfidf)
}

topword <- function(text_df, K, tp){
  body <- lexicalize(text_df, lower=TRUE)
  model <- lda.collapsed.gibbs.sampler(documents = body$documents, K, body$vocab, num.iterations = 500, alpha = 50/K, eta = 0.1)
  #α: per document topic distribution; β: per topic word distribution. High α = doc more similar to each other; High β = topic more similar to each other
  #Griffiths and Steyvers (2004) suggest a value of 50/k (where k is number of topics) for α and 0.1 for δ. (They use δ rather than beta to refer to the other hyperparameter).
  #Griffiths TL, Steyvers M (2004). “Finding Scientific Topics.” Proceedings of the National Academy of Sciences of the United States of America, 101, 5228–5235.
  topwords <- top.topic.words(model$topics, num.words = tp, by.score = T)
  return(topwords)
} 

run0 <-function(name, dir){
  xtext <- indata(dir, "                                      ")
  xtfidf <- tfidf_ng (xtext)
  assign(sprintf("tdidf_%s", name), xtfidf, envir = globalenv())
}

run1 <-function(name, dir){
  xtext <- indata(dir, "                                      ")
  xcount <- wordcount(xtext) 
  xtfidf <- tfidf (xtext)
  assign(sprintf("text_%s", name), xtext, envir = globalenv())
  assign(sprintf("count_%s", name), xcount, envir = globalenv())
  assign(sprintf("tdidf_%s", name), xtfidf, envir = globalenv())
}

run2 <-function(name, dir){
  xtext <- indata(dir, "/Users/ling/Downloads/2_tm/stop.txt")
   for (n in 5:50){
      tp <- topword(xtext, n, 10)
      filename = paste("topwords", name, "_Topic", n, ".csv", sep = "")
      write.csv(data.frame(tp), file = filename)
  }
}

run1("area", "                                      ")
run2("area", "                                      ")
run1("year", "                                      ")
run2("year", "                                      ")
run0("para", "                                      ")
run2("para", "                                      ")
