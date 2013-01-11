##################################################################################
#  
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni 8,50145,Italia                              
# mail: a.crisci@ibimet.cnr.it
# file: sem_analitics_mclass_code.r
# github: https://github.com/alfcrisci/socialgeosensing 
# Semantics analisys
##################################################################################

###############################################################################################
### Load the package or install if not present

if (!require("RColorBrewer")) {install.packages("RColorBrewer");library(RColorBrewer)}
if (!require("XML")) {install.packages("XML");library(XML)}
if (!require("tm")) {install.packages("tm");library(tm)}
if (!require("stringr")) {install.packages("stringr");library(stringr)}
if (!require("gdata")) {install.packages("gdata");library(gdata)}

###############################################################################################
# Setup working directory 


setwd("C:\\Users\\Alf\\Documents\\GitHub\\socialgeosensing")

###############################################################################################
# Load funtions
 
source("sem_functions.r")


###############################################################################################
# Importing data streams

full_tweets <- read.csv("data_semantics/twitter_streams.csv",header=T)

# subetting by period

heat_tweets<- subset(full_tweets,critical_day=="heat") 
noheat_tweets <- subset(full_tweets,critical_day=="noheat")  

###############################################################################################
# Creating tm italian corpus object. for english use function: generateCorpus_eng
# fill your stopwords if necessary
tw_table=table(full_tweets$data_excel)
png(file = "pie_tweet_volume_date.png",width = 1024, height = 768, bg = "transparent")
pie(table(full_tweets$data_excel), labels=paste(levels(full_tweets$data_excel),tw_table,sep="\n"), main="DNKT volume by date", col=brewer.pal(length(levels(full_tweets$data_excel)),"RdYlGn"))
dev.off()

table_tweets=as.data.frame(table(full_tweets$data_excel))
write.csv(table_tweets,file="table_tweets_to_be_sorted_by_date.csv")

###############################################################################################



###############################################################################################
# Creating tm italian corpus object. for english use function: generateCorpus_eng
# fill your stopwords if necessary example my_stopword=c("ciao")
# Full corpus tweets in two documents

noheat_tw=paste(noheat_tweets$MSG,collapse=" ")
heat_tw=paste(heat_tweets$MSG,collapse=" ")
heat_tw=clean.text(heat_tw)
noheat_tw=clean.text(noheat_tw)
heat_tw = removeWords(heat_tw,c(stopwords("italian"), c("setenossao","peo","hahaha","chega","rsrs","rose","afightersoul","batti","rangelagre","aprovado","perto","setenossa","caridade","luzia","rangel","miojo","lonerinlove","afighersouls","adooooooooro")))
noheat_tw = removeWords(noheat_tw,c(stopwords("italian"),c("setenossao","peo","hahaha","chega","rsrs","rose","afightersoul","batti","rangelagre","aprovado","perto","setenossa","caridade","luzia","rangel","miojo","lonerinlove","afighersouls","adooooooooro")))


allwave = c(heat_tw,noheat_tw)

heat.wave.corpus <- Corpus(VectorSource(allwave))
heat.wave.corpus <- tm_map(heat.wave.corpus, tolower)

# TermDocumentMatrix

heat.wave.dtm <- TermDocumentMatrix(heat.wave.corpus)
heat.wave.dtm <-removeSparseTerms(heat.wave.dtm, 0.6)
heat.wave.term.matrix <- as.matrix(heat.wave.dtm)
colnames(heat.wave.term.matrix) <- c("HEAT_DAYS","NO_HEAT_DAYS")


pdf("comparison_wordcloud.pdf")
comparison.cloud(heat.wave.term.matrix, random.order=T, colors = c("red","blue"), max.words=10)
dev.off()

pdf("commonality_wordcloud.pdf")
commonality.cloud(heat.wave.term.matrix, random.order=T, colors = c( "red","blue"), max.words=10)
dev.off()



###############################################################################################
# Removing noising/rare terms inspecting most popular words

heat_tw_corpus <- generateCorpus_ita(heat_tweets$MSG)
noheat_tw_corpus <- generateCorpus_ita(noheat_tweets$MSG)

# TermDocumentMatrix
heat_tw_corpus.dtm <- TermDocumentMatrix(heat_tw_corpus)
noheat_tw_corpus.dtm <- TermDocumentMatrix(noheat_tw_corpus)

# high frequency words

frequent_heat_60=findFreqTerms(heat_tw_corpus.dtm, lowfreq=60)
frequent_noheat_60=findFreqTerms(noheat_tw_corpus.dtm, lowfreq=60)
frequent_heat_60
frequent_noheat_60


# high & low  frequency words

frequent_heat_30=findFreqTerms(heat_tw_corpus.dtm, lowfreq=30)
frequent_noheat_30=findFreqTerms(noheat_tw_corpus.dtm, lowfreq=30)


###########################
# noheat_tw_corpus.dtm   term-document matrix 6682 terms 2608 tweets
# heat_tw_corpus.dtm     term-document matrix 7209 terms 3461 tweets 3 days 


# lexical_widening_heat
length(frequent_heat_30)/7209 

#lexical_widening_noheat

length(frequent_noheat_30)/6682 

#lexical_heat

length(frequent_heat_30)/3461 

# lexical_noheat

length(frequent_noheat_30)/2608

###############################################################################################
# Filtering 

heat_tw_corpus.dtm.strict <- heat_tw_corpus.dtm[frequent_heat_60]
noheat_tw_corpus.dtm.strict <- noheat_tw_corpus.dtm[frequent_noheat_60]


###############################################################################################
# HEAT DAYS Preparing distance matrix for clustering.

ncol(heat_tw_corpus.dtm.strict)
nrow(heat_tw_corpus.dtm.strict)
heat.df <- as.data.frame(inspect(heat_tw_corpus.dtm.strict))
heat.scale <- scale(heat.df)
dheat <- dist(heat.scale, method = "euclidean") # distance matrix


###############################################################################################
# HEAT DAYS clustering and draw dendogram with red borders around the 5 clusters


fit <- hclust(dheat, method="ward")
png(file = "heat_word_cluster.png",width = 1024, height = 768, bg = "transparent")
plot(fit,main="Clustering HEAT") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")
dev.off()


################################################################################################################
# HEAT DAYS  wordcloud min.freq=15
heat_tw_corpus <- generateCorpus_ita(heat_tweets$MSG)
heat_tw_corpus_w= tm_map(heat_tw_corpus, removeWords, c("caldo","sete","afa"))

pdf("heat_wordcloud.pdf")
par(bg = 'black')
print(wordcloud.generate(heat_tw_corpus_w,min.freq=30,palette=brewer.pal(3, "YlOrRd")))
dev.off() 
################################################################################################################


###############################################################################################
# NOHEAT DAYS Preparing distance matrix for clustering.

ncol(noheat_tw_corpus.dtm.strict)
nrow(noheat_tw_corpus.dtm.strict)
noheat.df <- as.data.frame(inspect(noheat_tw_corpus.dtm.strict))
noheat.scale <- scale(noheat.df)
dnoheat <- dist(noheat.scale, method = "euclidean") # distance matrix


###############################################################################################
# NOHEAT DAYS clustering and draw dendogram with red borders around the 5 clusters

par(bg = 'trasparent')
fit <- hclust(dnoheat, method="ward")

png(file = "noheat_word_cluster.png",width = 1024, height = 768, bg = "transparent")
plot(fit,main="Clustering NOHEAT") # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red")
dev.off()

################################################################################################################
# NOHEAT  DAYS wordcloud min.freq=15

noheat_tw_corpus <- generateCorpus_ita(noheat_tweets$MSG)

noheat_tw_corpus_w= tm_map(noheat_tw_corpus , removeWords, c("caldo","sete","afa"))

pdf("noheat_wordcloud.pdf")

par(bg = 'black')
print(wordcloud.generate(noheat_tw_corpus_w,min.freq=30,palette=brewer.pal(3, "YlOrRd")))
dev.off() 

################################################################################################################
# HEAT and NOHEAT DAYS hashtags wordcloud min.freq=3

hashtags_heat<- as.vector(na.exclude(str_extract(heat_tweets$MSG, "#\\S+")))
hashtags_noheat<- as.vector(na.exclude(str_extract(noheat_tweets$MSG, "#\\S+")))

png(file = "hashtags_heat.png",width = 1024, height = 768, bg = "black")
par(bg = 'black')
print(wordcloud.generate(generateCorpus_hashtag(hashtags_heat),3,brewer.pal(9, "Reds")))
dev.off()


png(file = "hashtags_noheat.png",width = 1024, height = 768, bg = "black")
print(wordcloud.generate(generateCorpus_hashtag(hashtags_noheat),3,brewer.pal(9, "Reds")))
dev.off()

################################################################################################################
# References

# Clustering texts http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/
# Wordcloud http://blog.fellstat.com/?p=101
# https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud