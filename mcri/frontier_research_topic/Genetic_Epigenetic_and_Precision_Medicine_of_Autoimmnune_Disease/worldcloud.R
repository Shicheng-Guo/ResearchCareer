install.packages("devtools")
require("devtools")

install.packages("Rcpp")  # for text mining

install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
install_github("lchiffon/wordcloud2")
library("wordcloud2")
setwd("//mcrfnas2/bigdata/Genetic/Projects/shg047/cv/frontier_research_topic/Genetic_Epigenetic_and_Precision_Medicine_of_Autoimmnune_Disease")

data<-read.table("worldcloud.txt",sep="\t")
head(data)
demoFreq=data.frame(word=data[,1],freq=data[,2])
wordcloud2(data = demoFreq)

data<-read.table("worldcloud.txt",sep="\t",head=T)
head(data)

wordcloud2(data)


