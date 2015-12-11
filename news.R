###############################################################
#             Invoke required library                         #
###############################################################

require("tm")||install.packages("tm")
require("rJava")||install.packages("rJava")
require("wordcloud")||install.packages("wordcloud")
require("textir")||install.packages("textir")
require("RWeka")||install.packages("RWeka")
require("qdap")||install.packages("qdap")
require("maptpx")||install.packages("maptpx")

library("tm")
library("rJava")
library("wordcloud")
library("textir")
library("RWeka")
library("qdap")
library("maptpx")

#############################################################
#         Attach userdefined functions                      #
#############################################################

source(file.choose())  #    Select text_functions.R

############################################################
#           Read the Text Data in R                        # 
############################################################
text  = readLines (file.choose())     # Select txt file you want to analyse
head(text)

Doc.id=seq(1:length(text))            # Assign Document no for each Document 
calib=data.frame(Doc.id,text)         # Create a dataframe for text documents with document ID

stpw = readLines(file.choose())      # Select stopwords.txt file
stpw1 = stopwords('english')         # tm package stop word list
comn  = unique(c(stpw, stpw1))       # Union of two list
stopwords = unique(c(gsub("'","",comn),comn)) # final stop word lsit after removing punctuation
head (stopwords)


#############################################################
#                        Text Cleaning                      #
#############################################################

test = text.clean(text)                         # basic HTML Cleaning etc
test  =  removeWords(test,stopwords)            # removing stopwords created above
head(test)                                      # print top documents

clean_text = test

########################################################
#             Create Document Term Matrix              #
########################################################

x1 = Corpus(VectorSource(test))          # Create the corpus
x1 = n.gram(x1,"bi",3)                   # Encoding bi-gram with atleast frequency 3 as uni-gram

dtm1 = custom.dtm(x1,"tf")               # Document Term Frequency 
dtm2 = custom.dtm(x1,"tfidf")            # Term Frequency Inverse Document Frequency Scheme


######################################################
#         Basic Analysis                             #

#   1- Using Term frequency(tf)             

freq1 = (sort(apply(dtm1,2,sum), decreasing =T)) # Calcualte term frequency
freq1[1:80]                                     # View top 80 terms 

windows()  # New plot window
wordcloud(names(freq1), freq1, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency - Wordcloud")


#   2- UsingTerm Frequency Inverse Document Frequency (tfidf)             
freq2 = (sort(apply(dtm2,2,sum), decreasing =T)) # Calcualte term frequency
freq2[1:80]                                     # View top 80 terms 

windows()  # New plot window
wordcloud(names(freq2), freq2, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency Inverse Document Frequency - Wordcloud")

###########################################################
#         Sentiment Analysis                              #
###########################################################

clean_text0 = clean_text[clean_text != ""]    # Remove Blank Document for sentiment Analysis

pol = polarity(clean_text0)       # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

print(positive_words)       # Print results
print(negative_words)       # Print results

########################################################
# Create Postive Words wordcloud                      #
########################################################

tdm_temp = t(TermDocumentMatrix(Corpus(VectorSource(clean_text0))))
pos.tdm = tdm_temp[,(match(positive_words,colnames(tdm_temp)))]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=50,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")


########################################################
# Create Negative Words wordcloud                      #
########################################################

neg.tdm = tdm_temp[,(match(negative_words,colnames(tdm_temp)))]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,0.5),1, max.words=50,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")


#######################################################
#        Positive words vs Negative Words plot        #
#######################################################

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}


pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)
