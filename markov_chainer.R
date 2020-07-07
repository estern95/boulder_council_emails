# Build Text Generator 

# Based on Daniel Shiffman
# http://shiffman.net/a2z
# https://github.com/shiffman/A2Z-F16

# Array.prototype.choice = function() {
#   var i = floor(random(this.length));
#   return this[i];
# }

library(magrittr)
library(stringr)
library(purrr)
# testing_corpus <- "A Markov process is a stochastic process that satisfies the Markov property[1] (sometimes characterized as memorylessness). In simpler terms, a Markov process is a process for which one can make predictions for its future based solely on its present state just as well as one could knowing the process's 
# full history.[11] In other words, conditional on the present state of the system, its future and past states 
# are independent. Markov chain is a type of Markov process that has either a discrete state space or a discrete 
# index set (often representing time), but the precise definition of a Markov chain varies.[12] For example, it 
# is common to define a Markov chain as a Markov process in either discrete or continuous time with a countable 
# state space (thus regardless of the nature of time),[13][14][15][16] but it is also common to define a Markov 
# chain as having discrete time in either countable or continuous state space (thus regardless of the state space)."
# 
link <- 'https://www.gutenberg.org/files/11/11-pdf.pdf'

testing_corpus2 <- pdftools::pdf_text(link) %>%
  purrr::discard(~str_detect(.x, 'Free eBooks at Planet eBook.com [0-9+]')) %>%
  map(~str_remove_all(.x, '[0-9+]') %>%
        str_remove_all("Free eBooks at Planet eBook\\.com") %>%
        str_replace_all("\\r\\n", " ") %>%
        #str_remove_all("\\s{4,}Aesop’s Fables") %>%
        str_replace_all("\\s{2,}", " ")) %>%
  paste(collapse = '\n\n')

# this generates a marcov chain based on a set size of ngrams, max length, and a corpus
marcov_generator <- function(seeder, max, corpus) {
  n = length(strsplit(seeder, "\\s")[[1]]) # strsplit returns a nested object idk
  grams = ngrammer(n, corpus)
  
  chainer <- function(this_gram = seeder, length = max, dict = grams, chain = c()) {
    if (length(chain) == length) {
      return(chain)
    }
    possibles <- dict[which(dict == this_gram) + n]
    
    if (all(is.na(possibles))) {
      return(chain)
    }
    next_gram <- base::sample(possibles, 1) # randomly sample from all next possible instances
    
    chain = c(chain, next_gram)
    
    chainer(this_gram = next_gram, length = max, dict = grams, chain = chain)
  }
  
  return(paste(c(seeder, chainer()), collapse = " "))
}
   
# this is an ngram generator. It takes a length of ngram n and a corpus to create ngrams. Returns a list of ngrams

ngrammer <- function(n, corpus) {
   # unnest_tokens(corpus, unigrams, test, token = 'ngrams', n = n)$unigrams 
  list_tokens <- strsplit(corpus, "\\s")[[1]]
  grams <- character(length = length(list_tokens))
  for (i in seq_along(list_tokens)) {
    grams <- c(grams, paste(list_tokens[i:(i+n-1)], collapse = " "))  
  }
  return(grams)
}

marcov_generator(seeder = "she was", max = 200, corpus = testing_corpus2) %>% cat()
