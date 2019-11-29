#########################
#######################
# Section 1 Find a Topic to Focus on Sentiment Analysis
######################
######################

library(twitteR)   # Version: 0.2.2
library(tidytext)  # Version: 0.2.2
library(ggplot2)   # Version: 3.2.1  
library(stringr)   # Version: 1.4.0
library(wordcloud) # Version: 2.6

##### Step 1.1: Twitter API application and download tweets
#Apply for a developer account on Twitter
#Apply for a new webpage to have a webpage address
#WRite application for api_key that sort of information

api_key<-'Please insert your own key'
api_secret<-'Please insert your secret codes'
access_token<-'Please insert your token' 
access_secret<-'Please insert your secret access codes' 


# Search term: "#"('#',n=18000,lang = 'en',since='2019-11-13',until = '2019-11-17',
# geocode = '43.6532,-79.3832,60km')
# latitude and altitude of Toronto, with a 60km radius

tweetsBig1<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweetsBig1, file = "tweetsBig1.RData")
tweetsBig2<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-15',until = '2019-11-16',geocode = '43.6532,-79.3832,60km')
save(tweetsBig2, file = "tweetsBig2.RData")
tweetsBig3<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-14',until = '2019-11-15',geocode = '43.6532,-79.3832,60km')
save(tweetsBig3, file = "tweetsBig3.RData")
tweetsBig4<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweetsBig4, file = "tweetsBig4.RData")
tweetsBig5<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweetsBig5, file = "tweetsBig5.RData")
tweetsBig6<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-13',until = '2019-11-14',geocode = '43.6532,-79.3832,60km')
save(tweetsBig6, file = "tweetsBig6.RData")
tweetsBig7<-searchTwitter('#',n=18000,lang = 'en',since='2019-11-17',until = '2019-11-18',geocode = '43.6532,-79.3832,60km')
save(tweetsBig7, file = "tweetsBig7.RData")


# Convert raw data into dataframes
df1<-twListToDF(tweetsBig1)
df2<-twListToDF(tweetsBig2)
df3<-twListToDF(tweetsBig3)
df4<-twListToDF(tweetsBig4)
df5<-twListToDF(tweetsBig5)
df6<-twListToDF(tweetsBig6)
df7<-twListToDF(tweetsBig7)


# Combine 5 dataframes, #4 and #5 are duplicated
# and therefore are not included
# Tweets are listed in ascending order of date
DF_big<-rbind(df6,df3,df2,df1,df7)

# Remove is"Retweet"
dfbig<-dfbig[dfbig$isRetweet!=T,] # to remove the isRetweet


# Take only the text column form the combined frame
# Change the column's name to text
dfbig2<-as.data.frame(dfbig[,1])
colnames(dfbig1)<-"text"



######  Step 1.2: Cleaning

#Remove RT and username, with a space between them
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("(^RT\\sw*)(@\\w+)","",dfbig2$text[i])
}

#Remove RT@username (no space between them)
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("(^RT@\\w+)","",dfbig2$text[i])
}


#Remove RT in the beginning of sentence
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("^RT\\s","",dfbig2$text[i])
}


# Remove the mentions, only the first one
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("^@\\w+","",dfbig2$text[i])
}

# Remove ":"
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("^:\\s","",dfbig2$text[i])
}

# Clean the "&amp"
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("&amp","",dfbig2$text[i])
}

# Remove website at the beginning of text
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*\\s","",dfbig2$text[i])
}


# Remove website at the end of text
for (i in 1:nrow(dfbig2)){
  dfbig2$text[i]<-gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*","",dfbig2$text[i])
}


# Take away the digits
for (i in 1:nrow(testDF_clean)){
  dfbig2$text[i]<-gsub("[[:digit:]]+","",testDF_clean$text[i])
}



##### Step 1.3: Tokenization

df_clean<-dfbig2

# Tokenization and introduce stopwords

data(stop_words)
df_token<-df_clean %>%
  unnest_tokens(word, text, to_lower = T)  %>%
  anti_join(stop_words)


##### Step 1.4: Further cleaning 
#Step 1.4.1: Take away foreign character from the token bag

library(stringi)

length(which(stri_enc_isascii(df_token$word)==F))

df_token_new<-as.data.frame(df_token[stri_enc_isascii(df_token$word)==T,])
colnames(df_token_new)<-"text"
df_token_new$text<-as.character(df_token_new$text)


# Step 1.4.2: Take away punctuation, some columns only contains punctuation marks
for (i in 1:nrow(df_token_new)){
  df_token_new$text[i]<-gsub("[[:punct:]]","",df_token_new$text[i])
}
which(df_token_new$text=="")
length(which(df_token_new$text==""))
emp<-which(df_token_new$text=="")

# there are 16 cells that contain empty string
# make a newer dataframe without empty string
df_token_2<-as.data.frame(df_token_new[-(which(df_token_new$text=="")),1])
colnames(df_token_2)<-"word"
df_token_2$text<-as.character(df_token_2$text)


# Step 1.4.3: Find most frequent words
# Filter out the word "lol" (high frequency)
df_token_2_freq<-df_token_2 %>%
  filter(word != "lol") %>%
  count(word, sort = TRUE)


# Plot the words of top 25 frequencies
top_n(df_token_2,25) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n))+
  geom_col(aes(fill=word))+
  coord_flip()+
  labs(x="word",y="frequency of the words used",
       title="Top 25 words used in the downloaded tweets")



##### Step 1.5: Find topics through hashtag research
library(stringr)
library(wordcloud)

# Reload dfbig2 file from Step 1.1
load("dfbig2.RData")

#Lower cases
testDF_clean2<-dfbig2
testDF_clean2$text<-tolower(testDF_clean2$text)

# Get the hashtags
hashedtext = str_extract_all(testDF_clean2$text, "#\\w+")
hashedtext # a large list
str(hashedtext)

# This generates a list of 45170, list
# contains the #words from document 1
# put tags in a data frame and count 
# the frequency
hashedvector = as.data.frame(unlist(hashedtext))
colnames(hashedvector)<-"words"
hashedvector$words<-as.character(hashedvector$words)



#####  Step 1.6: Tokenize the words according to hashtags
hashed_freq <- hashedvector %>%
  group_by(words) %>%
  count(words,sort = T)


# Step 1.6.1 Plot hashtags wordcloud
wordcloud(hashed_freq$words, hashed_freq$n,
          min.freq=1, random.order=FALSE, 
          colors="#D95F02")
title("\n\nHashtags in downloaded tweets",
      cex.main=1.5, col.main="gray50")


# Step 1.6.2: Plot the hashtags frequency
# New data frame is needed because the frame with hash tagged words
# are grouped and the mutate function will not work

hashed_freq_2<-as.data.frame(hashed_freq)

top_n(hashed_freq_2 ,20) %>%
  mutate(words=reorder(words,n)) %>%
  ggplot(aes(words,n))+
  geom_col(aes(fill=words),show.legend = F)+
  coord_flip()+
  labs(x="word",y="frequency of the words used",
       title="Top 20 topics mentioned in the downloaded tweets")


# Step 1.7: Find all tweets with the 
#           #toronto/#leafsforever/#firejessallen/#leafs from 
#           the cleaned test frame

testDF_clean3<-df_clean # from testDF_clean from 
# beginnning of step 1.3

# Clean the punctuations
for (i in 1:nrow(testDF_clean3)){
  testDF_clean3$text[i]<-gsub("[[:punct:]]","",testDF_clean3$text[i])
}


#Check for cell containing empty string
which(testDF_clean3$text==" ")
is.na(testDF_clean3[29524,1])
testDF_clean3[29524,1]

# After investifation, there is one cell containing empty string
# Remove empty cell
testDF_clean3<-as.data.frame(testDF_clean3[-(which(testDF_clean3$text==" ")),1])
colnames(testDF_clean3)<-"word"
testDF_clean3$word<-as.character(testDF_clean3)


# Lower case all the words
testDF_clean3a <-as.data.frame(tolower(testDF_clean3$text))
testDF_clean3a$text<-as.character(testDF_clean3a$text)
colnames(testDF_clean3a)<-"text"


# Find from the 45170 tweets that only 285 tweets
# contain one of the thest 4 specific # words. Proof need new data
testDF_clean3b<-testDF_clean3a %>%
  filter(grepl('(\\snfl\\s)|(\\sleafs\\s)|(firejessallen)|(leafsforever)',text))




################################################
###############################################################
##### Section 2: Sentimental Analysis According to Specific Hashtags #####
###############################################################
###############################################




# RStudio Version: 1.2.1335 

library(dplyr)     # Version: 0.8.3
library(tidytext)  # Version: 0.2.2
library(textdata)  # Textdata: 0.3.0
library(tidyr)     # Version: 0.8.3
library(ggplot2)   # Version 3.2.1
library(ggerpel)   # Version 0.8.1
library(stringi)   # Version: 1.4.3
library(stringr)   # Version: 1.4.0
library(syuzhet)   # Version: 1.0.4
library(twitteR)   # Verson: 1.1.9
library(wordcloud) # Version: 2.6
library(quanteda)  # Version: 1.5.1
library(caret)     # Version: 6.0-84



##Step 2.1.1

# Download tweets according to specific hashtags, though the number was set on 5000 for each tweet
# Only few tweets returned

# LeafsForever  (Total: 7686 tweets)
tweets_sent_leafs_1<-searchTwitter('#LeafsForever',n=10000,lang = 'en',since='2019-11-13',until = '2019-11-14',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_1, file = "tweets_sent_leafs_1.RData")

tweets_sent_leafs_2<-searchTwitter('#LeafsForever',n=5000,lang = 'en',since='2019-11-14',until = '2019-11-15',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_2, file = "tweets_sent_leafs_2.RData")

tweets_sent_leafs_3<-searchTwitter('#LeafsForever',n=5000,lang = 'en',since='2019-11-15',until = '2019-11-16',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_3, file = "tweets_sent_leafs_3.RData")

tweets_sent_leafs_4<-searchTwitter('#LeafsForever',n=5000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_4, file = "tweets_sent_leafs_4.RData")

tweets_sent_leafs_5<-searchTwitter('#LeafsForever',n=5000,lang = 'en',since='2019-11-17',until = '2019-11-18',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_5, file = "tweets_sent_leafs_5.RData")

#### mapleleafs (total: 399 tweets retrieved)

tweets_sent_leafs_6<-searchTwitter('#mapleleafs',n=10000,lang = 'en',since='2019-11-13',until = '2019-11-14',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_6, file = "tweets_sent_leafs_6.RData")

tweets_sent_leafs_7<-searchTwitter('#mapleleafs',n=5000,lang = 'en',since='2019-11-14',until = '2019-11-15',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_7, file = "tweets_sent_leafs_7.RData")

tweets_sent_leafs_8<-searchTwitter('#mapleleafs',n=5000,lang = 'en',since='2019-11-15',until = '2019-11-16',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_8, file = "tweets_sent_leafs_8.RData")

tweets_sent_leafs_9<-searchTwitter('#mapleleafs',n=5000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_9, file = "tweets_sent_leafs_9.RData")

tweets_sent_leafs_10<-searchTwitter('#mapleleafs',n=5000,lang = 'en',since='2019-11-17',until = '2019-11-18',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_10, file = "tweets_sent_leafs_10.RData")

#### #Leafs (Total: 2862 tweets retrieved)

tweets_sent_leafs_11<-searchTwitter('#Leafs',n=10000,lang = 'en',since='2019-11-13',until = '2019-11-14',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_11, file = "tweets_sent_leafs_11.RData")

tweets_sent_leafs_12<-searchTwitter('#Leafs',n=5000,lang = 'en',since='2019-11-14',until = '2019-11-15',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_12, file = "tweets_sent_leafs_12.RData")

tweets_sent_leafs_13<-searchTwitter('#Leafs',n=5000,lang = 'en',since='2019-11-15',until = '2019-11-16',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_13, file = "tweets_sent_leafs_13.RData")

tweets_sent_leafs_14<-searchTwitter('#Leafs',n=5000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_14, file = "tweets_sent_leafs_14.RData")

tweets_sent_leafs_15<-searchTwitter('#Leafs',n=5000,lang = 'en',since='2019-11-17',until = '2019-11-18',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_15, file = "tweets_sent_leafs_15.RData")


#### FireJessAllen (Total: 2344 tweets retrieved)

tweets_sent_leafs_16<-searchTwitter('#FireJessAllen',n=10000,lang = 'en',since='2019-11-13',until = '2019-11-14',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_16, file = "tweets_sent_leafs_16.RData")

tweets_sent_leafs_17<-searchTwitter('#FireJessAllen',n=5000,lang = 'en',since='2019-11-14',until = '2019-11-15',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_17, file = "tweets_sent_leafs_17.RData")

tweets_sent_leafs_18<-searchTwitter('#FireJessAllen',n=5000,lang = 'en',since='2019-11-15',until = '2019-11-16',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_18, file = "tweets_sent_leafs_18.RData")

tweets_sent_leafs_19<-searchTwitter('#FireJessAllen',n=5000,lang = 'en',since='2019-11-16',until = '2019-11-17',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_19, file = "tweets_sent_leafs_19.RData")

tweets_sent_leafs_20<-searchTwitter('#FireJessAllen',n=5000,lang = 'en',since='2019-11-17',until = '2019-11-18',geocode = '43.6532,-79.3832,60km')
save(tweets_sent_leafs_20, file = "tweets_sent_leafs_20.RData")


##### Turn the downloaded into a dataframe
##########

dfs1<-twListToDF(tweets_sent_leafs_1)
dfs2<-twListToDF(tweets_sent_leafs_2)
dfs3<-twListToDF(tweets_sent_leafs_3)
dfs4<-twListToDF(tweets_sent_leafs_4)
dfs5<-twListToDF(tweets_sent_leafs_5)
dfs6<-twListToDF(tweets_sent_leafs_6)
dfs7<-twListToDF(tweets_sent_leafs_7)
dfs8<-twListToDF(tweets_sent_leafs_8)
dfs9<-twListToDF(tweets_sent_leafs_9)
dfs10<-twListToDF(tweets_sent_leafs_10)
dfs11<-twListToDF(tweets_sent_leafs_11)
dfs12<-twListToDF(tweets_sent_leafs_12)
dfs13<-twListToDF(tweets_sent_leafs_13)
dfs14<-twListToDF(tweets_sent_leafs_14)
dfs15<-twListToDF(tweets_sent_leafs_15)
dfs16<-twListToDF(tweets_sent_leafs_16)
dfs17<-twListToDF(tweets_sent_leafs_17)
dfs18<-twListToDF(tweets_sent_leafs_18)
dfs19<-twListToDF(tweets_sent_leafs_19)
dfs20<-twListToDF(tweets_sent_leafs_10)


##### Put tags after each tweet (the hashtag's name)
##########  Combine the Raw data into a big frame

New_DFS_Big1<-rbind(dfs1,dfs2,dfs3,dfs4,dfs5)
New_DFS_Big1$Tag<-"Leafsforever"

New_DFS_Big2<-rbind(dfs6,dfs7,dfs8,dfs9,dfs10)

New_DFS_Big2$Tag<-"Mapleleafs"

New_DFS_Big3<-rbind(dfs11,dfs12,dfs13,dfs14,dfs15)

New_DFS_Big3$Tag<-"Leafs"


New_DFS_Big4<-rbind(dfs16,dfs17,dfs18,dfs19,dfs20)

New_DFS_Big4$Tag<-"Firejessallen"

save(New_dfsb,file="New_dfsb.RData")



##### 2.1.2: Investigate the raw data

# Arrange all data with time in ascending order from early to late
# (early entry on top)

new_dfsb_order<-New_dfsb[order(as.POSIXct.Date(New_dfsb$created,format="%d/%m/%Y %H:/%M")),]
dim(new_dfsb_order) #13291;17


# Check for missing value 
length(which(!complete.cases(new_dfsb_order$text))) # Ans: no missing values in text column
summary(new_dfsb_order$favoriteCount)
boxplot(new_dfsb_order$favoriteCount,border ="dark green",main="Boxplot of favorited")


# check # of empty cells in this column
length(which(is.na(new_dfsb_order$favoriteCount))) # no empty cells
length(which(new_dfsb_order$favoriteCount!=0)) # 2959 tweets receive many favorites
favorited<-length(which(new_dfsb_order$favoriteCount!=0))/nrow(new_dfsb_order) #ANS:0.22

# Check for top 10 tweets 
favorited<-subset(new_dfsb_order,new_dfsb_order$favoriteCount>400)
favor_count<-hist(favorited$favoriteCount,breaks=30,col="dark blue",border="white",
                  xlab="Track of FavoriteCount in the tweets",main="Histogram of tweets receiving favorite",freq=T)
favor<-favorited[order(favorited$favoriteCount,decreasing = T),]
Table_for_favorite_count<-favor[1:10,c(1,3)] # none are a reply
write.csv(Table_for_favorite_count,"text with most favorite counts.csv")

# investigate the tweets that are retweets 
retweet<-new_dfsb_order[new_dfsb_order$isRetweet==T,]
nrow(retweet) ## of retweets: 8533

# find the number of tweets that are a reply
replyingtweet<-retweet<-new_dfsb_order[new_dfsb_order$replyToUID==T,]
nrow(replyingtweet)  #12280 tweets are replying tweets to others (UID);
                     # use USID: assign for every user


# Using graph to show the proportion of retweet in the dataset
library("ggplot2")
non_retweet<-(nrow(new_dfsb_order))-(nrow(retweet))
raw<-data.frame(Category=c("Original","Retweet"),Count=c(non_retweet,retweetnum))
raw$fraction = round((raw$Count / sum(raw$Count)),digits=4)
raw$percentage = round((raw$Count / sum(raw$Count) * 100),digits = 4)
raw$ymax = cumsum(raw$fraction)
raw$ymin = c(0, head(raw$ymax, n=-1))

# Rounding the data to two decimal points
raw <- round_df(raw, 2)
# Specify what the legend should say
Tweet_proportion <- paste(raw$Category, raw$percentage, "%")
ggplot(raw, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tweet_proportion)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

### Take Away the retweets

new_dfsb_order<-new_dfsb_order[new_dfsb_order$isRetweet!=T,]
dim(new_dfsb_order)  #4758 rows with 17 columns
newdf_sent<-new_dfsb_order[,c(1,5,17)]  # 4758 rows with 3 columns




#########################  Data Processing: Data cleaning #############
###########################(preliminary step)###########

###Step 2.2.1: Data Cleaning

# Remove RT and username with a space between them. EX: RT @ 
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("(^RT\\sw*)(@\\w+)","",newdf_sent$text[i])
}

#Remove RT@username (no space between them)
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("(^RT@\\w+)","",newdf_sent$text[i])
}

#Remove RT in the beginning of sentence
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("^RT\\s","",newdf_sent$text[i])
}

# Remove the mentions, only the first one 
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("^@\\w+","",newdf_sent$text[i])
}

# Remove ":" in the beginning (this will work if a tweet starts with RT@username :)
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("^:\\s","",newdf_sent$text[i])
}

# Clean the "&amp"
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("&amp","",newdf_sent$text[i])
}

# Remove website at the beginning of text. 
# To ensure the text content is remained of such tweets
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*\\s","",newdf_sent$text[i])
}

# Remove website address that are at the end of text
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+?(/)[[:alnum:]]*","",newdf_sent$text[i])
}

# Remove digits
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("[[:digit:]]+","",newdf_sent$text[i])
}

# Remove punctuation
# This step will not clean the ounctuation in the words such as (it's)
# (Yur're), (i'm), (I'm)

for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-gsub("[[:punct:]]","",newdf_sent$text[i])
}

# Lower case all words. Though many package offers lowering cases 
# upon tokenization, but just in case....
# 
for (i in 1:nrow(newdf_sent)){
  newdf_sent$text[i]<-tolower(newdf_sent$text[i])
}

# Take away duplicated text. 
newdf_sent<-newdf_sent[!duplicated(newdf_sent$text),]  # 4006 rows now

# Then check if there is empty string
which(newdf_sent$text==" ")  #return: integer(0)
which(!complete.cases((newdf_sent$text)))  #return: integer(0)



# Check if there is any text that only contains foreign characters.
 library(stringi)
 length(which(stri_enc_isascii(newdf_sent$word)==F)) ## Resurn: 0 
                                             
 ##### Function to remove the tweets that are all written in 
 ##### characters not in English
 # library(stringi)
 # dfsb_sm2<-dfsb_sm2[stri_enc_isascii(testDF3_1$word)==T,]
 
 
# Remove Emoticons
 for (i in 1:nrow(newdf_sent)){
   newdf_sent$text[i]<-gsub("[^\x01-\x7F]","",newdf_sent$text[i])  # removes codes as \u23f0,\u0001f4Fa
                                                               # which is none ASCII codes
   newdf_sent$text[i]<-gsub("[\r\n]+", "", newdf_sent$text[i]) #removes line break (for Windows)
 }

de_clean<-newdf_sent # Assign to a simplier name

                 #################End of preliminary cleaning.################




### Step: 2.2.2: A look at the cleaned text

# To know average word length of the tweets
for (i in 1:nrow(df_clean)){
df_clean$words[i]<-sapply(strsplit(df_clean$text[i]," "), length)
}

summary(df_clean$words) 
boxplot(df_clean$words,border ="dark green",
         col="dark green", 
        main="Boxplot of word length in each tweet")
hist(df_clean$words,border="dark red",density=c(15,30,60,70,50) , 
     xlab = "Distribution of word length",
     angle=c(0,45,90,11,36),col=c("dark red"),
     main="distribution of word length in the tweets")


# Find our what the longest text is about
print(which.max(df_clean$words))


# Transform the date into the name of weekdays as new column for later work as well
df_clean$days<-format(df_clean$created,'%b %d') #turn date into Month and day


# Count Frequency of tweet numbers per day
tweet_freq<-df_clean %>% group_by(days) %>%
                       summarise(tweets_number=n())
paste(tweet_freq)
tweets_number <- data.frame(
                    Day=c("Nov_13","Nov_14","Nov_15","Nov_16","Nov_17"),
                    Total_tweets=c(390,1156,325,948,1187)
                     )
barplot(height = tweets_number$Total_tweets, names=tweets_number$Day ,
        density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="dark blue",
        main = "Boxplot for total tweets from Wednesday to Sunday")

save(df_clean,file="df_clean_length.RData")



##### Step: 2.2.3: Unto Tokenization 

# To tokenize the texts require following library 
library(tidytext)
library(dplyr)


# Introduce Stop_words
data("stop_words")
tidy_stop<-stop_words$word # used for later



# Download the dictionaries, stored as data frame for word look-up

###   A NOTE ahead: 
###   In order to get dictionary for sentiment evaluations, do the following
###   "IN THE CONSOLE"
###   type:
###   get_sentiments("afinn"), then a question will come
###   up for downloading the dictionary
###   do the same thing for the other two dictionaries ("nrc" and "bing") 
###

afinn<-get_sentiments("afinn")  # get dictionary "afinn" as a data frame
nrc<-get_sentiments("nrc")  # get dictionary "nrc" as a data frame
bing<-get_sentiments("bing")



#tokenization
# PS: I keep the function mutate with hashtags because I learned that if working with different
#     computers, sometimes the word "it's" will not be taken out even though it's a stop word
#     due to the codings of eaach platform. In some computers, the tidytext stopword is 
#     recognized to uses an apostrophe, not
#     the right single quotation mark. This happened once when I worked with different computer, not with all 
#     computers.

df_clean_token<- df_clean %>%
                   unnest_tokens(word, text, to_lower = T) %>% 
                      mutate(word = gsub("\u2019", "'", word)) %>% # original text uses right single quotation mark
                                                          # stop_words in tidytext uses an apostrophe
                        anti_join(stop_words,by="word")   # 27058 tokens in the bag


# Count frequency of all the words
sentiment_freq<-df_clean_token %>%
                count(word, sort = T) #6365 


# Count frequency of each tokens on each day

fre_days<- df_clean_token %>% 
                         group_by(days) %>% 
                             summarise(totalwords_tweet=n())
glimpse(fre_days)

Word_freq <- data.frame(
  TweetDay=c("Nov_13","Nov_14","Nov_15","Nov_16","Nov_17"),
  Total_tweet_word=c(2837,7806,2327,6375,7761)
)
barplot(height = Word_freq$Total_tweet_word, names=Word_freq $TweetDay ,
        density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="dark red",
        main = "Boxplot for total tweeted words from Wednesday 
        to Sunday")


### The most frequent word

df_clean_token %>% count(word) %>%
                   top_n(30) %>%  mutate(word=reorder(word,n)) %>%
                      ggplot(aes(word,n))+ geom_col(aes(fill=word))+
                           coord_flip()+
                              labs(x="word",y="frequency of the words used",
                                   title="Top 30 words used in the tweets regards to Hockey fans in Toronto")



##### Step 2.2.4: Customize additional stopwords

###################### Data Cleaning:Second Stage #########
###################### ####Custom additional stopwords#####

# Assign the unwanted words from top 30 word region, add other common stopwords
# to a vector

unwant_word<-c("leafsforever","leafs","firejessallen","game","team","hockey","leafsnation","mapleleafs","tonight","play",
               "time","goal","nhl","white","pp","aint","hey","tmltalk","people","islanders",
               "im","coach","matthews","dont","whats","ppl","dubas","jess","st","barrie","goleafsgo","pittsburgh",
               "firejessicaallen","tomorrow","frankdangelo","kerfoot","kyle","ceci","isnt","lot","miss","andersen",
               "john","arent","wont","isnt","didnt","havent","wasnt","doesnt","wouldnt","theyre","theyve","theyll","weve","werent",
               "youve","youre","youll","youd","youve","idk","were","werent")   # cucstomed list for stopwords


# Combine two dictionaries as a vector

affinword<-c(afinn$word)
nrcword<-c(nrc$word)
combined<-c(affinword,nrcword)  # combine 2 dictionaries

#PS: take away the word "john" to be included in the sentiment analysis
# Because in nrc, this is a word represent"disgust"
# john appeared twice in the "nrc" dictionary. Therefore, has to removed
# this word twice form the nrc dictionary.

which(combined =="john")
combined<-combined[-c(9760,9761),]


######## More stopwords needed to be customed.
#### But upon inspection of word distribution, it was realized
### 5469 tokens are in the region of frequency <=4 (total tokens: 6356)
### Therefore, no stopwords should be included form the tokens with low frequency

###############################Custom stopwords list process done##############



##### Step 2.2.5: Transform the original data frame 

# Function for transformation, transform the words appeared in the tweet to the word 
# that will be counted as a sentiment

transform<-function(sentecnce){
  sentecnce<-gsub("(buttfucked)|(chantsfucking)|(fuckboston)|(fucks)|(wtfff)","fuck",sentecnce)
  sentecnce<-gsub("(yeai)|(yeahhhh)|(yeaaah)|(yeaaaah)|(yeaaaa)|(ye)|(yassssss)","yeah",sentecnce)
  sentecnce<-gsub("unfuckingbelievable","fucking",sentecnce)
  sentecnce<-gsub("sofuckingfrusterating","frustrating",sentecnce)
  sentecnce<-gsub("underachieving","fail",sentecnce)
  sentecnce<-gsub("crippling","cripple",sentecnce)
  sentecnce<-gsub("(discriminatory)|(discriminateany)","discriminating",sentecnce)
  sentecnce<-gsub("hypocri","hypocrite",sentecnce)
  sentecnce<-gsub("championship","champion",sentecnce)
  sentecnce<-gsub("enjoyable","enjoy",sentecnce)
  sentecnce<-gsub("achievers","achieve",sentecnce)
  sentecnce<-gsub("hypocri","hypocrite",sentecnce)
  sentecnce<-gsub("(buttfucked)|(chantsfucking)|(fuckboston)|(fucks)|(wtfff)","fuck",sentecnce)
  sentecnce<-gsub("shity","shit",sentecnce,fixed = T)
  sentecnce<-gsub("shithole","shit",sentecnce,fixed=T)
  sentecnce<-gsub("(happier)|(makemehappyboys)","happy",sentecnce)
  sentecnce<-gsub("sacrifice","sacrifices",sentecnce)
  sentecnce<-gsub("yeam","yeaming",sentecnce)
  sentecnce<-gsub("challenges","challenge",sentecnce)
  sentecnce<-gsub("dangerously","dangerous",sentecnce)
  sentecnce<-gsub("cringed","cringe",sentecnce)
  sentecnce<-gsub("diligently","diligence",sentecnce)
  sentecnce<-gsub("acknowledged","success",sentecnce)
  sentecnce<-gsub("affectionately","affection",sentecnce)
  sentecnce<-gsub("antiwhite","racist",sentecnce)
  sentecnce<-gsub("apolog","apology",sentecnce)
  sentecnce<-gsub("anticipate","anticipation",sentecnce)
}


# Apply transformation
df_clean_transformed<-df_clean
df_clean_transformed$text<-sapply(df_clean_transformed$text,transform)

#### Result 1: have a custom stopword list
#### REsult 2: The original dataframe 
####           now being "cleaned" and "transformed"

############################### Transformation Finished##############
############ Tokenization part 1 finished #######
############ Start a new path and undergo tokenization again#######



###### SAve the file for future use ##########
save(df_clean_transformed,file="df_clean_transformed.RData")
###############################################



##### Step 2.3: Make a customized dictionary and assign each tweet to 
#####           class (positive or negative or neutral)
#####            according to a: only "nrc", b) the customed dictionary

##### Step 2.3.1: Obtain the third dictionary from Github

######################################
######################################
##### 1: Codes of function to evaluate sentiment are taken and modified 
#####   from jeffreybreen's Github site, together with the provided dictionary
#####   (below are the links)
#####
##### link for dictionaries for positive words
##### https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/data/opinion-lexicon-English/positive-words.txt

##### link for negative words
##### https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/data/opinion-lexicon-English/positive-words.txt

######################################
#####################################

# Import positive and negative words downloaded from Github
pos.words = readLines("positive-words.txt")
neg.words = readLines("negative-words.txt")

# Add words from two other dictionaries ("nrc" and "afinn)
# into the positive and negative word lists
afinn<-get_sentiments("afinn")
nrc<-get_sentiments("nrc")



# Step 2.3.2: Add nrc words to the positive word list

# Assign positive emotions as "positive" emotions
nrcpos<-c()
for(i in 1:nrow(nrc)){
  if(nrc$sentiment[i]=="trust" || nrc$sentiment[i]=="positive" ||
     nrc$sentiment[i]=="joy")
    nrcpos<-c(nrcpos,nrc$word[i])
}
length(which(!duplicated(nrcpos)))
nrcpos<-nrcpos[which(!duplicated(nrcpos))] #remove duplicated words

# Add extra words from nrc dictionary into the original positive words text file

nrcpos<-as.data.frame(nrcpos)
colnames(nrcpos)<-"word"
addnrcpos<- nrcpos %>% filter(! word %in% pos.words) #add this into the text file
addpos<-as.character(addnrcpos$word)
pos_new<-c(pos.words,addpos)
save(pos_new,file="pos_new_nrc.txt")
pos.words<-pos_new


# Step 2.3.3: Add nrc words to the original negative word text file list
# ASsign negative emotions as "negative" from "nrc"

nrcneg<-c()
for(i in 1:nrow(nrc)){
  if(nrc$sentiment[i]=="fear" || nrc$sentiment[i]=="negative" ||
     nrc$sentiment=="sadness" || nrc$sentiment[i]=="anger")
    nrcneg<-c(nrcneg,nrc$word[i])
}

length(which(!duplicated(nrcneg))) # check if there are duplicated values
nrcneg<-nrcneg[which(!duplicated(nrcneg))] # get only the non duplicated values


# Add extra words from nrc dictionary 
# into the original negative word text file
# 
nrcneg<-as.data.frame(nrcneg)
colnames(nrcneg)<-"word"
addnrcneg<- nrcneg %>% filter(! word %in% neg.words) #add this into the text file
addneg<-as.character(addnrcneg$word)
neg_new<-c(neg.words,addneg)
save(neg_new,file="neg_new_nrc.txt")
neg.words<-neg_new


# Step 2.3.4: Add afinn words to new positive word list

afinpos<-c()
afinn<-data.frame(afinn)
afinn$newvalue<-as.numeric(as.character(afinn$value)) # Get "afinn values"

# Assign words to "positive" category according to afinn value system
for(i in 1:nrow(afinn)){
  if(afinn$newvalue[i]==5 | afinn$newvalue[i]==4 |
     afinn$newvalue[i]==3|afinn$newvalue[i]==2|afinn$newvalue[i]==1)
    afinpos<-c(afinpos,afinn$word[i])
}

length(which(!duplicated(afinpos)))

#remove duplicated words
afinpos<-afinpos[which(!duplicated(afinpos))]
str(afinpos)


# Add extra words from afinn dictionary 
# into the extended positive word list
# 
afinpos1<-as.data.frame(afinpos)
colnames(afinpos1)<-"word"
afinpos2<- afinpos1 %>% filter(! word %in% pos_new) #add this into the text file
str(afinpos2)
afinpos2$word<-as.character(afinpos2$word)
pos_new2<-c(pos.words,afinpos2$word)
save(pos_new2,file="pos_new2_nrc and afinn.txt")
pos.words<-pos_new2


# Step 2.3.5: Add afinn words to the new negative word list

afinneg<-c()

# Assign words to "negative" category according to afinn value system
for(i in 1:nrow(afinn)){
  if(afinn$newvalue[i]==-5 | afinn$newvalue[i]==-4 |
     afinn$newvalue[i]==-3|afinn$newvalue[i]==-2|afinn$newvalue[i]==-1)
    afinneg<-c(afinneg,afinn$word[i])
}   

length(which(!duplicated(afinneg))) # check for duplicated values
afinneg<-afinneg[which(!duplicated(afinneg))] # get only the non duplicated values

# Add extra words from afinn dictionary 
# into the extended negative word list
afinneg<-as.data.frame(afinneg)
colnames(afinneg)<-"word"
addafinneg<- afinneg %>% filter(! word %in% neg_new) #add this into the text file, 
                                                    #but this is actually "0" in length
addafinneg$word<-as.character(addafinneg$word)
neg_new2<-c(neg_new,addafinneg$word)
save(neg_new2,file="neg_new2_nrc and afinnneg_new2.txt")
neg_new<-neg_new2

#Reassign variable for replacing the old text file with the new one
pos.words<-pos_new2
neg.words<-neg_new2


##### Step 2.3.6:Use the extended version of text to 
# assign values for each tweet

######################################
########## Note: the function of evaluate sentiment for each tweet
##########       are Codes are learned and modified from jeffreybreen's Github site
#####################################

# Function for sentiment evaluation

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words,.progress='none') {
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Recall the dataframe that is cleaned and has words at lower frequency transformed
load("df_clean_transformed")
df_clean<-df_clean_transformed

# Get the scores of positive and negative to the transformed clean data frame
scores <- score.sentiment(df_clean$text, pos.words, neg.words, .progress='text')

# Assign the scores to the clean data frame
df_clean$scores<-scores$score

# Assign whether this is a negative or positive comment
for(i in 1:nrow(df_clean)){
  if(df_clean$scores[i]>0) {df_clean$class[i]="pos"} 
  else if (df_clean$scores[i]<0) {df_clean$class[i]="neg"}
  else {df_clean$class[i]="neutral"}
}


##################################################
######## Save this data frame (cleaned/transformed/assigned with 3 lexicons)
##################################################

df_clean_3dict<-df_clean
save(df_clean_3dict,file="df_clean_3dict.RData")
load("df_clean_3dict.RData") # load the transformed clean data assigned with 3 dictionaries


# A quick look into the distribution
library(dplyr)
df_clean_3_sum<-df_clean_3dict %>%
  select(class) %>%
  group_by(class) %>%
  tally()
print(df_clean_3_sum)  

df_clean_3_sum %>%
  ggplot(aes(x=class,y=n))+
  theme(legend.position="right")+
  geom_bar(aes(fill=n),stat="identity",fill=c("dark orange","blue","dark green"))+
  labs(x="Class",y="Total count",
       title="Sentiment of the tweets According to three dictionaries")



##### Step 2.3.7: Assign class according to only one dictionary ("nrc")

# Load the clean and transformed dataframe ("df_clean_transformed")
load("df_clean_transformed.RData")
df_clean<-df_clean_transformed

# Get positive words assigned by nrc 
nrc_pos<- get_sentiments("nrc") %>% 
  filter(sentiment == "positive")  # lisr of nrc positive words
glimpse(nrc_pos)

nrc_pos<-data.frame(nrc_pos$word)  # transform tbl frame into data frame
# because tbl frame is only a local frame
glimpse(nrc_neg)
nrc_pos$word<-as.character(nrc_pos$nrc_pos.word)
nrc_poslist<-nrc_pos$word



## Get negative words assigned by nrc
nrc_neg<-get_sentiments("nrc")  %>% 
  filter(sentiment == "negative")  # list of nrc negative words
nrc_neg<-data.frame(nrc_neg$word)  # transform tbl frame into data frame

# because tbl frame is only a local frame
nrc_neg$word<-as.character(nrc_neg$nrc_neg.word)
nrc_neglist<-nrc_neg$word  # all together nrc recognize 5635


## Set the positive and negative words from nrc as the 
## variables used by function
pos.words<-nrc_poslist
neg.words<-nrc_neglist


# Get the scores of positive and negative value in the original clean data frame
scores <- score.sentiment(df_clean$text, pos.words, neg.words, .progress='text')


# Assign the scores to the clean & transformed data frame
df_clean$scores<-scores$score


# Assign negative or positive class to the dataframe
for(i in 1:nrow(df_clean)){
  if(df_clean$scores[i]>0) {df_clean$class[i]="pos"} 
  else if (df_clean$scores[i]<0) {df_clean$class[i]="neg"}
  else {df_clean$class[i]="neutral"}
}


##############################
# Save this clean data frame as a new data frame for predictive analysis
df_clean_nrc<-as.data.frame(df_clean)
save(df_clean_nrc,file="df_clean_nrc.RData")
load("df_clean_nrc.RData")
###############################

# A quick look into the distribution
library(dplyr)
df_clean_nrc_sum<-df_clean_nrc %>%
  select(class) %>%
  group_by(class) %>%
  tally()
print(df_clean_nrc_sum)  

df_clean_nrc_sum %>%
  ggplot(aes(x=class,y=n))+
  theme(legend.position="right")+
  geom_bar(aes(fill=n),stat="identity",fill=c("dark orange","blue","dark green"))+
  labs(x="Class",y="Total count",
       title="Sentiment of the tweets According to nrc dictionary")


##### Step 2.4: Tokenization and TF_IDF with clean/transformed dataframe

library(dplyr)
library(tidytext)


###### Step 2.4.1 Tokenize the dataframe assigned by ONLY "nrc"
##Load the claned and transformed nrc dataframe from step 2.3.7
load("df_clean_nrc.RData")


# And obtain TF-IDF values 
df_clean_nrc<-df_clean_nrc[,c(1,7)]
nrc_tokens <- df_clean_nrc %>%
                unnest_tokens(word,text) %>% 
                  dplyr::count(class,word,sort=TRUE) %>%
                  bind_tf_idf(word, class, n)
glimpse(nrc_tokens)
nrc_tokens

# Arrange the tokens according to TF-IDF
nrc_tokens %>%
  select(-n) %>%
  arrange(desc(tf_idf))

# Top 15 words in the bag (not according to the class)
nrc_tokens %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  top_n(15) %>% 
  ggplot(aes(word, tf_idf, fill = class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#293352","#52854C"))+
  labs(x = NULL, y = "tf-idf", title="Top 15 words tweeted in this set of tweet data") +
  coord_flip()


# Top 10 words in the bag according to class
nrc_tokens %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(class) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#D16103","#C3D7A4","#52854C"))+
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~class, ncol = 2, scales = "free") +
  coord_flip()


nrc_emotions<-nrc_tokens %>%
                          inner_join(get_sentiments("nrc")) %>%
                              group_by(sentiment) %>%
                                dplyr::count(word, sort = TRUE) %>%
                                     arrange(desc(n)) %>%
                                          slice(seq_len(8)) %>% #consider top_n() from dplyr also
                                                ungroup()


# Plot the tokens according to "nrc" 10 categories
#### (A technique leanred from 
### https://www.datacamp.com/community/tutorials/sentiment-analysis-R#lexiconsandlyrics)

# Require library(ggerpel)

set.seed(42)
nrc_emotions %>%
      ggplot(aes(word, 1, label = word, fill = sentiment )) +
                geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = 0.5,  # for the label not overlapping
                   direction = "y",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Hockey NRC Sentiment") +
  coord_flip()
  



##### Step 2.4.2 : tokenization of the datafrmane defined by three lexicons

library(dplyr)
library(tidytext)

# Load the dataframe that is cleaned and transformed and 
# being assigned with a class according to three lexicsons (abbrev: tl)
# from Step 2.3.6
load(file="df_clean_3dict.RData")


# Tokenize the dataframe assigned by tl and obtain TF-IDF values 

df_clean_3dict<-df_clean_3dict[,c(1,7)]
df_clean_tl_tokens <- df_clean_3dict %>%
  unnest_tokens(word,text) %>% 
  dplyr::count(class,word,sort=TRUE) %>%
  bind_tf_idf(word, class, n)
glimpse(df_clean_tl_tokens)
df_clean_tl_tokens

# Arrange the tokens according to TF-IDF
df_clean_tl_tokens %>%
  select(-n) %>%
  arrange(desc(tf_idf))

# Top 15 words in the bag (not according to the class)
df_clean_tl_tokens %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  top_n(15) %>% 
  ggplot(aes(word, tf_idf, fill = class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#0072B2","#52854C","#E69F00"))+
  labs(x = NULL, y = "tf-idf", title="Top 15 words tweeted in this set of tweet data") +
  coord_flip()


# Top 10 words in the bag according to class
df_clean_tl_tokens %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(class) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = class)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#D16103","#0072B2","#52854C"))+
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~class, ncol = 2, scales = "free") +
  coord_flip()


# Plot the defined sentiment according to nrc's emotion category

tl_emotions<-df_clean_tl_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(sentiment) %>%
  dplyr::count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup() 
tl_emotions%>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = 0.5,  # for the label not overlapping
                   direction = "y",
                   box.padding = 0.02,
                   segment.color = "transparent",
                   size = 4) +
  facet_grid(~sentiment) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Hockey Sentiment, defined by three dictionaries") +
  coord_flip()




############################################
################################################################

##### Section 3: Predictive model
##### Random Forest

################################################################
############################################


library(caret)         # Version: 6.0-84
library(quanteda)      # Version: 1.5.1
library(e1071)         # Version: 1.7-2
library(irlba)         # Version: 4.6-14
library(randomForest)  # Version: 4.6-14
library(dplyr)         # Version: 0.8.3

# Step3.1.1: Set the index for training and test dataset

# Recall the cleaned and transformed frame that has each tweet assigned 
# to a sentiment
load("df_clean_nrc.RData")
nrc<-df_clean_nrc [,c(1,7)]
prop.table(table(nrc$class))

library(ggplot2)
nrc$leng <- nchar(nrc$text)
summary(nrc$leng)
ggplot(nrc, aes(x = leng, fill = class)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Tweet Count", x = "Length of tweet",
       title = "Distribution of tweet lengths in different class")

# Convert  class label into a factor.
nrc$class <- as.factor(nrc$class)

##### Step 3.1.2: Make TF-IDF matrix for training set and test set

# Set an index for training and test set
set.seed(35757)
indexes <- createDataPartition(nrc$class, times = 1,
                               p = 0.7, list = FALSE)
train <- nrc[indexes,]
test <- nrc[-indexes,]

# To see proportion in a table
prop.table(table(train$class)) #0.23(neg): 0.48(neutral): 0.277 (pos)

prop.table(table(test$class))  # 0.23(neg): 0.48(neutral): 0.277 (pos)


### 3.1.2.1 Tokenize training set
# Introduce stopwords in the training token bags
# 1:
# Recall stop_words list from step 2.2.3, use this stopwords vector
# (vector: tidy_stop)
# 2:
# Recall the customized stopword from 2.2.4 and apply it as well
# (vector: unwant_word)

train_tokens <- tokens(train$text, what = "word")
train_tokens<-tokens_select(train_tokens,tidy_stop,selection="remove")
train_tokens<-tokens_select(train_tokens,unwant_word,selection="remove") #2805 tokens

save(train_tokens,file="nrc_train_tokens_afterstop.RData")
load("nrc_train_tokens_afterstop.RData")


### 3.1.2.2 Tokenize test set for nrc frame
# Introduce stopwords in the training token bags
# 1:
# Recall stop_words list from step 2.2.3, use this stopwords vector
# (vector: tidy_stop)
# 2:
# Recall the customized stopword from 2.2.4 and apply it as well
# (vector: unwant_word)

test_tokens <- tokens(test$text, what = "word")
test_tokens<-tokens_select(test_tokens,tidy_stop,selection="remove")
test_tokens<-tokens_select(test_tokens,unwant_word,selection="remove") #1201 tokens

save(test_tokens,file="nrc_test_tokens_afterstop.RData")
load("nrc_test_tokens_afterstop.RData")


### 3.1.2.3 Register TF IDF function

# Our function for calculating relative term frequency (TF)
term.frequency <- function(row) {
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(x, idf) {
  x * idf
}



### 3.1.2.4  Build TF-IDF frame for training set

# create the DTF (document Term Frame) for training set
train_tokens_dfm <- dfm(train_tokens)

# turn to matrix for future step
train_tokens_matrix <- as.matrix(train_tokens_dfm) 


# Normalize all documents via TF for training set
train_tokens_df <- apply(train_tokens_matrix, 1, term.frequency)
dim(train_tokens_df) #5030 2805 now the document is inversed


# Second step, calculate the IDF vector for training set
train_tokens_idf <- apply(train_tokens_matrix, 2, inverse.doc.freq)
str(train_tokens_idf)


# Calculate TF-IDF for training corpus.
train_tokens_tfidf <- apply(train_tokens_df, 2, tf.idf, idf = train_tokens_idf)
dim(train_tokens_tfidf) #5030 2805


# Transpose the matrix
train_tokens_tfidf <- t(train_tokens_tfidf)
dim(train_tokens_tfidf)  # 2805 5030

# Check for incopmlete cases
incomplete <- which(!complete.cases(train_tokens_tfidf))
train$text[incomplete] # 41 vectors

# Fix incomplete cases
train_tokens_tfidf[incomplete,] <- rep(0.0, ncol(train_tokens_tfidf))
dim(train_tokens_tfidf)
sum(which(!complete.cases(train_tokens_tfidf)))

# make a clean frame
train_tokens_tfidf_df <- cbind(Label = train$class, data.frame(train_tokens_tfidf))
names(train_tokens_tfidf_df) <- make.names(names(train_tokens_tfidf_df))

################# TF-IDF for training set finished#######




##### 3.1.2.5 Dimension Reduction by SVD

# Use doSNOW to distribute the word to 3 cores to reduce time
library(irlba)
library(doSNOW)

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
start.time <- Sys.time()
#Applying SVD to reduce features to 300
train.irlba <- irlba(t(train_tokens_tfidf), nv = 300, maxit = 600) 
total.time <- Sys.time() - start.time #1.1 minutes
total.time


# Project new data (e.g., the test data) into the SVD semantic space.
sigma_inverse <- 1 / train.irlba$d
u.transpose <- t(train.irlba$u)
document <- train_tokens_tfidf[1,]
document_hat <- sigma_inverse * u.transpose %*% document


# Create new feature data frame using our document semantic space of 300
# features (i.e., the V matrix from our SVD) for classification
nrc_train_svd <- data.frame(class = train$class, train.irlba$v)



##### UNTO Machine Learning
##### 3.1.2.6 Machine learning for classification by Random Forest

# load doSNOW to distribute the work to 3 cores
library(doSNOW)
library(randomForest)
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)


# Build cross fold for training set
set.seed(32984)
cv.folds <- createMultiFolds(train$class, k = 10, times = 3) # set 10 foleds, just iterate 3 time

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv.folds)

# Time the code execution
start.time <- Sys.time()

# Classification according to random forest
train_nrc <- train(class ~ ., data = nrc_train_svd, method = "rf", 
                  trControl = cv.cntrl, tuneLength = 3)

# Processing is done, stop cluster.
stopCluster(cl)

# Total time of execution on workstation was 
total.time <- Sys.time() - start.time #19.25648 minutes
total.time

# Check results.
train_nrc  
confusionMatrix(nrc_train_svd$class, train_nrc$finalModel$predicted)


############################
##### Because the result was very bad ######
### I want to see if I get the same result from the
### data that is assigned by customized dicitonaries
### just for test part
###########################



# Step3.2.1: Investigate the raw data

# Recall the cleaned and transformed frame that has each tweet assigned 
# to a sentiment (df_clean_3dict from step 2.3.6 )
load("df_clean_3dict.RData")
tl<-df_clean_3dict [,c(1,7)]
prop.table(table(tl$class))
library(ggplot2)
tl$leng <- nchar(tl$text)
summary(tl$leng)
ggplot(tl, aes(x = leng, fill = class)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Tweet Count", x = "Length of tweet",
       title = "Distribution of tweet lengths in different class
       ,using customized lexicon")

# Convert  class label into a factor.
tl$class <- as.factor(tl$class)

##### Step 3.2.2: Make TF-IDF matrix for training set and test set

# Set an index for training and test set
set.seed(35757)
indexes <- createDataPartition(tl$class, times = 1,
                               p = 0.7, list = FALSE)
train <- tl[indexes,]
test <- tl[-indexes,]

# To see proportion in a table
prop.table(table(train$class)) #0.28(neg): 0.32(neutral): 0.39 (pos)

prop.table(table(test$class))  # 0.28(neg): 0.32(neutral): 0.39 (pos)


### 3.2.2.1 Tokenize training set
# Introduce stopwords in the training token bags
# 1:
# Recall stop_words list from step 2.2.3, s this stopwords vector
# (vector: tidy_stop)
# 2:
# Recall the customized stopword from 2.2.4 and apply it as well
# (vector: unwant_word)

train_tokens <- tokens(train$text, what = "word")
train_tokens<-tokens_select(train_tokens,tidy_stop,selection="remove")
train_tokens<-tokens_select(train_tokens,unwant_word,selection="remove") #2805 tokens

save(train_tokens,file="tl_train_tokens_afterstop.RData")
load("tl_train_tokens_afterstop.RData")



### 3.2.2.2 Build TF-IDF frame for training set with tl frame

# create the DTF (document Term Frame) for training set
train_tokens_dfm <- dfm(train_tokens)

# turn to matrix for future step
train_tokens_matrix <- as.matrix(train_tokens_dfm) 


# Normalize all documents via TF for training set
train_tokens_df <- apply(train_tokens_matrix, 1, term.frequency)
dim(train_tokens_df) #5030 2805 now the document is inversed


# Second step, calculate the IDF vector for training set
train_tokens_idf <- apply(train_tokens_matrix, 2, inverse.doc.freq)
str(train_tokens_idf)


# Calculate TF-IDF for training corpus.
train_tokens_tfidf <- apply(train_tokens_df, 2, tf.idf, idf = train_tokens_idf)
dim(train_tokens_tfidf) 


# Transpose the matrix
train_tokens_tfidf <- t(train_tokens_tfidf)
dim(train_tokens_tfidf)  

# Check for incopmlete cases
incomplete <- which(!complete.cases(train_tokens_tfidf))
train$text[incomplete] 

# Fix incomplete cases
train_tokens_tfidf[incomplete,] <- rep(0.0, ncol(train_tokens_tfidf))
dim(train_tokens_tfidf) 
sum(which(!complete.cases(train_tokens_tfidf)))

# make a clean frame
train_tokens_tfidf_df <- cbind(Label = train$class, data.frame(train_tokens_tfidf))
names(train_tokens_tfidf_df) <- make.names(names(train_tokens_tfidf_df))

################# TF-IDF for training set finished#######




##### 3.2.2.3 Dimension Reduction by SVD

# Use doSNOW to distribute the word to 3 cores to reduce time
library(irlba)
library(doSNOW)

cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)
start.time <- Sys.time()
#Applying SVD to reduce features to 300
train.irlba <- irlba(t(train_tokens_tfidf), nv = 300, maxit = 600) 
total.time <- Sys.time() - start.time #59 sec
total.time


# Project new data (e.g., the test data) into the SVD semantic space.
sigma_inverse <- 1 / train.irlba$d
u.transpose <- t(train.irlba$u)
document <- train_tokens_tfidf[1,]
document_hat <- sigma_inverse * u.transpose %*% document


# Create new feature data frame using our document semantic space of 300
# features (i.e., the V matrix from our SVD) for classification
tl_train_svd <- data.frame(class = train$class, train.irlba$v)



##### UNTO Machine Learning
##### 3.2.2.4 Machine learning for classification by Random Forest

# load doSNOW to distribute the work to 3 cores
library(doSNOW)
library(randomForest)
cl <- makeCluster(3, type = "SOCK")
registerDoSNOW(cl)


# Build cross fold for training set
set.seed(32984)
cv.folds <- createMultiFolds(train$class, k = 10, times = 3) # set 10 foleds, just iterate 3 time

cv.cntrl <- trainControl(method = "repeatedcv", number = 10,
                         repeats = 3, index = cv.folds)

# Time the code execution
start.time <- Sys.time()

# Classification according to random forest
train_tl <- train(class ~ ., data = tl_train_svd, method = "rf", 
                   trControl = cv.cntrl, tuneLength = 3)

# Processing is done, stop cluster.
stopCluster(cl)

# Total time of execution on workstation was 
total.time <- Sys.time() - start.time #19.25648 minutes
total.time

# Check results.
train_tl  
confusionMatrix(tl_train_svd$class, train_tl$finalModel$predicted)

























