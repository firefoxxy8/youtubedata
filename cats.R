library("textcat")
library("tuber")
library("syuzhet")
library("dplyr")
library("tidyr")
library("ggplot2")


# see https://github.com/soodoku/tuber readme for authentification

# please note I only keep comments in English!
# and you can get only 100 comments per video

# comments for Maru
maru <- "bXtHwvp7jYE"

maruComments <- tbl_df(get_comments(video_id=maru, maxResults = 100, textFormat = "plainText"))
maruComments <- maruComments %>% 
  filter(textcat(textDisplay) == "english") %>%
  mutate(textDisplay = iconv(unlist(textDisplay), "UTF-8", "ASCII", sub="")) %>%
  mutate(textDisplay = gsub("&#39;", "\'", textDisplay)) %>%
  mutate(publishedAt = lubridate::ymd_hms(unlist(publishedAt)))%>%
  mutate(updatedAt = lubridate::ymd_hms(unlist(updatedAt))) %>%
  select(publishedAt, updatedAt, textDisplay)

# sentiment analysis for Maru
maruComments <- tbl_df(cbind(maruComments, get_nrc_sentiment(maruComments$textDisplay)))

maruComments2 <- maruComments %>%
  gather(sentiment, score, anger:positive) %>%
  group_by(sentiment) %>%
  summarize(mean(score))

# comments for keyboard cat
keyboardCat <- "J---aiyznGQ"

keyboardCatComments <- tbl_df(get_comments(video_id=keyboardCat, maxResults = 100, textFormat = "plainText"))
keyboardCatComments <- keyboardCatComments %>% 
  filter(textcat(textDisplay) == "english") %>%
  mutate(textDisplay = iconv(unlist(textDisplay), "UTF-8", "ASCII", sub="")) %>%
  mutate(textDisplay = gsub("&#39;", "\'", textDisplay)) %>%
  mutate(publishedAt = lubridate::ymd_hms(unlist(publishedAt)))%>%
  mutate(updatedAt = lubridate::ymd_hms(unlist(updatedAt))) %>%
  select(publishedAt, updatedAt, textDisplay)

# sentiment analysis for keyboard cat
keyboardCatComments <- tbl_df(cbind(keyboardCatComments, get_nrc_sentiment(keyboardCatComments$textDisplay)))

keyboardCatComments2 <- keyboardCatComments %>%
  gather(sentiment, score, anger:positive) %>%
  group_by(sentiment) %>%
  summarize(mean(score))

# prepare the data to plot
dataCats <- tbl_df(data.frame(maru = maruComments2,
                              keyboardCat = keyboardCatComments2)) %>%
  mutate(sentiment = maru.sentiment) %>%
  select(sentiment, everything()) %>%
  select(- maru.sentiment, - keyboardCat.sentiment) 
names(dataCats) <- c("sentiment", "maru", "keyboardCat")

dataCats <- dataCats %>%
  gather(cat, score, 2:3) %>%
  mutate(sentiment = factor(sentiment, levels = unique(sentiment), ordered = TRUE))

# get the patterns

catColors <- c("#57C27E", "#4967A3")

# !! new version of ggplot2 for using subtitles

p <- ggplot(dataCats) +
  geom_bar(aes(x = sentiment,
               y = score,
               fill = cat),
           stat = "identity",
           position="dodge",
           width = 0.8)+
  scale_fill_manual(values = catColors) +
  theme(text = element_text(size=25))

p <- p+ labs( y="Mean score",
         title=paste0("Battle of two Youtube stars"),
         subtitle=paste0("Sentiment analysis of ",
                         nrow(keyboardCatComments), " comments on a Keyboard Cat and ",
                         nrow(maruComments), " comments on a Maru video, all in English"))

ggsave(p, file = "catswar.png")