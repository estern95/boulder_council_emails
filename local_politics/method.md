## Dashboard

This is a shiny dashboard built using open source tools from Rstudio and the greater R community. Shiny lets data scientists and analysts quickly build out high quality, code based dashboards with a limited amount of boilerplate code. The dashboard came together very quickly in roughly 15 hours of effort.

## Topic Modelling 

The topic model is a Latent Dirichlet Allocation (LDA) model implemented in the `topicmodels` libary in R. This is a very popular unsupervised language model as it can it does a decent job of disaggregating topics without massive computing power. It is a bag of words approach, meaning that we are purely looking at word counts by document without looking at how the words are in series. [More information can be found here](http://jmlr.org/papers/volume3/blei03a/blei03a.pdf).

## Data Preparation and Cleaning 

The data was provided by the city of Boulder in csv by email. Thankfully, email addresses have been scrubbed prior to being openly distibuted.  Here are the basic steps I did for cleaning:

* Remove exact duplicates.
* Remove as much spam as possible.
* Remove common words that don't provide value in a bag-of-words approach (to, is, as, am, etc.)
* Stemmed words. 
* Removed extremely unique words and extremely common words.
* Removed emails with no body text.