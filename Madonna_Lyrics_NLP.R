####################################################################
# Music sentiment analysis and topic modelling of Madonna's lyrics #
####################################################################

library(tidyverse)
library(httr)
library(stringr)
library(lubridate)
library(corrplot)

library(dplyr)

#library(devtools)
#install_github("tiagomendesdantas/Rspotify")
library(Rspotify)

artist_info <- searchArtist("Madonna")
artist_info <- artist_info[1,]

getAlbumsYear <- function (id, type = "album", market = "US") 
{
  total <- jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/", 
                                     id, "/albums??&album_type=album"))$total
  req <- jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/", 
                                   id, "/albums??offset=0&limit=", total, "&album_type=", 
                                   type, "&market=", market))
  i = 1
  date_release <- album_img <- vector(length = length(req$items$artists))
  
  for (album_id in req$items[,"id"])
    {
    date_release[i] <- jsonlite::fromJSON(paste0("https://api.spotify.com/v1/albums/", 
                                              album_id))$release_date
    album_img[i] <- jsonlite::fromJSON(paste0("https://api.spotify.com/v1/albums/",
                                              album_id))$images[[2]][2]
    i = i+1
  }
  
  res <- req$items[, c("id", "name", "album_type", "available_markets")]
  res$album_date <- date_release
  res$album_img <- album_img
  return(res)
}

album_info <- getAlbumsYear(artist_info$id)
album_info <- album_info[c(-2,-4,-8,-10),]
album_info$album_date <- as.Date(album_info$album_date)
album_info$album_year <- format(album_info$album_date,'%Y')

keys <- spotifyOAuth("musicAnalysis",
                     "KEY1",
                     "KEY2")

album_songs <- data.frame(matrix(NA, nrow = 0, ncol = 9))

for (album_id in album_info$id){
  tmp1 <- getAlbum(album_id)
  tmp2 <- rep(album_id, length(tmp1$id))
  album_songs <- rbind(album_songs, cbind(tmp1, tmp2))
  
}
names(album_songs) <- c(names(getAlbum(album_info[1,]$id)), "album_id")

#album_songs <- as.data.frame(lapply(album_songs, function(x) unlist(x)), stringsAsFactors = F)

album_songs$album_name <- sapply(album_songs$album_id, 
                                 function(x) subset(album_info, id == x)$name)
album_songs$album_year <- sapply(album_songs$album_id, 
                                 function(x) subset(album_info, id == x)$album_year)
album_songs$album_img <- sapply(album_songs$album_id, 
                                 function(x) subset(album_info, id == x)$album_img)

song_feat <- data.frame(matrix(NA, nrow = 0, ncol = 16))
for (song_id in album_songs[315:348,]$id){
  song_feat <- rbind(song_feat, getFeatures(as.character(song_id), keys))
}

#song_feat <- rbind(song_feat,song_feat2)

song_feat <- cbind(album_songs, song_feat[,c(2:12, 14:16)])
#song_feat$album_img <- sapply(song_feat$album_id, 
#                                function(x) subset(album_info, id == x)$album_img)

#song_feat$album_img <- sapply(song_feat$album_id, 
#                              function(x) subset(album_info, id == x)$album_img)
song_feat$id <- unlist(song_feat$id)
song_feat$name <- unlist(song_feat$name)
song_feat$duration_ms <- unlist(song_feat$duration_ms)
song_feat$track_number <- unlist(song_feat$track_number)
song_feat$disc_number <- unlist(song_feat$disc_number)
song_feat$preview_url <- unlist(song_feat$preview_url)

song_feat %>% 
  select(valence, danceability, name) %>%
  arrange(-valence) %>% 
  head(10)

#### Visualising Valence per album ####

library(ggplot2)

ggplot(song_feat, aes(track_number)) + 
  geom_line(aes(y = valence, colour = "Valence")) + 
  geom_line(aes(y = danceability, colour = "Danceability")) +
  geom_line(aes(y = liveness, colour = "Liveness")) +
  facet_wrap( ~ album_name, ncol=3) +
  scale_y_continuous(name="values")

#### Visualisation of Music Features #### 

library(RColorBrewer)
library(highcharter)

song_feat$uri2 <- sapply(song_feat$uri, function(x) paste0("https://embed.spotify.com/?uri=", x))

plot_df <- song_feat %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', name
                          #,'<br><iframe width="250" height="80" fframeborder="0" allowtransparency="true" 
                          #src=', uri2, '></iframe>'
                          )) %>% 
  ungroup

avg_line <- plot_df %>% 
  group_by(album_year, album_name, album_img) %>% 
  summarise(avg = mean(danceability)) %>% 
  ungroup %>% 
  transmute(x = as.numeric(as.factor(album_year)), 
            y = avg,
            tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Year:</b> ', album_year, 
                             '<br><b>Average Danceability:</b> ', round(avg, 2),
                             '</a>'))

plot_track_df <- plot_df %>% 
  mutate(tooltip = paste0(tooltip, '<br><b>Danceability:</b> ', danceability, '</a>'),
         album_number = as.numeric(as.factor(album_year))) %>% 
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_year)), y = danceability, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(song_feat$album_name), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 1, title = list(text = 'Danceability')) %>% 
  hc_title(text = 'Data Driven Danceability') %>% 
  hc_subtitle(text = 'Madonna song danceability by album') %>% 
  hc_add_theme(hc_theme_smpl())

album_chart$x$hc_opts$series[[10]]$name <- 'Album Averages'
album_chart


###### COLLECTING LYRICS DATA ######

token <- 'TOKEN'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('Madonna')
genius_artists <- genius_artists[c(-1,-2),]


baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

length(track_lyric_urls) #312

summary(track_lyric_urls[[1]])

library(rvest)

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('lyrics') %>% 
    html_text
}

genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  # add in error handling
  lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
  if (class(lyrics) != 'try-error') {
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- tolower(str_trim(lyrics))
  } else {
    lyrics <- NA
  }
  
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  
  return(tots)
})

genius_df <- genius_df %>% 
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>% 
  filter(!duplicated(track_name_join)) %>% 
  select(-track_name)

track_df <- song_feat %>%
  mutate(track_name_join = tolower(str_replace(name, '[[:punct:]]', ''))) %>%
  left_join(genius_df, by = 'track_name_join') %>%
  select(name, duration_ms, track_number, danceability, energy, loudness, 
         speechiness, acousticness, instrumentalness, liveness, valence, tempo,
         lyrics, album_name, album_year, album_img)


library(tidytext)

negative_words <- sentiments %>% 
  filter(lexicon == 'nrc', sentiment %in% c('sadness')) %>% 
  select(word) %>% 
  mutate(negative = T)

positive_words <- sentiments %>% 
  filter(lexicon == 'nrc', sentiment %in% c('joy')) %>% 
  select(word) %>% 
  mutate(positive = T)

sent_neg_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(negative_words, by = 'word') %>%
  group_by(name) %>% 
  summarise(pct_neg = round(sum(negative, na.rm = T) / n(), 4),
            word_count = n()) %>% 
  ungroup

require(qdap)
sent_all_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  mutate(pos = pos(word)$POStagged$POStags) %>%
  left_join(positive_words, by = 'word') %>%
  left_join(negative_words, by = 'word') %>%
  group_by(name)

sent_all_df$test <- sapply(sent_all_df$pos, 
                           function(x) ifelse(length(x[[1]]) == 1, unlist(x), "check"))
sent_all_df$noun <- sapply(sent_all_df$test,
                           function(x) ifelse(x %in% c("NN", "NNS"), TRUE, NA))
sent_all_df$adj <- sapply(sent_all_df$test,
                           function(x) ifelse(x %in% c("JJ", "JJS", "JJR"), TRUE, NA))
sent_all_df$word_len <- sapply(sent_all_df$word,
                               function(x) str_length(x))

sent_all_df$album_year <- as.numeric(sent_all_df$album_year)
sent_all_df$album_decade <- ifelse(sent_all_df$album_year<1990, "1980s", 
                                ifelse(sent_all_df$album_year<2000, "1990s",
                                       ifelse(sent_all_df$album_year<2010, "2000s", "2010s")))

decade_word_freq <- sent_all_df %>%
  group_by(album_decade, word, noun, adj) %>%
  summarise(word_freq = n()) 
top80s <- subset(decade_word_freq, noun==T & album_decade=="1980s" & word != "chorus") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top90s <-subset(decade_word_freq, noun==T & album_decade=="1990s" & word != "chorus") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top00s <-subset(decade_word_freq, noun==T & album_decade=="2000s" & word != "chorus") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top10s <-subset(decade_word_freq, noun==T & album_decade=="2010s" & word != "chorus") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top_nouns <- rbind(top80s, top90s, top00s, top10s)

top80s <- subset(decade_word_freq, adj==T & album_decade=="1980s") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top90s <-subset(decade_word_freq, adj==T & album_decade=="1990s") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top00s <-subset(decade_word_freq, adj==T & album_decade=="2000s") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top10s <-subset(decade_word_freq, adj==T & album_decade=="2010s") %>%
  arrange(desc(word_freq)) %>%
  head(10)
top_adj <- rbind(top80s, top90s, top00s, top10s)

word_freq <- sent_all_df %>%
  group_by(name, word, noun, adj) %>%
  summarise(word_freq = n()) %>%
  group_by(name) %>%
  summarise(avg_word_freq = mean(word_freq),
            noun_uniq_count = sum(noun, na.rm = T),
            adj_uniq_count = sum(adj, na.rm = T)) 

pos_freq <- sent_all_df %>%
  group_by(name) %>%
  summarise(noun_notuniq_count = sum(noun, na.rm = T),
            adj_notuniq_count = sum(adj, na.rm = T),
            word_len = mean(word_len, na.rm=T))

sent_pos_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(positive_words, by = 'word') %>%
  group_by(name) %>% 
  summarise(pct_pos = round(sum(positive, na.rm = T) / n(), 4),
            word_count = n()) %>% 
  ungroup

sent_neg_df %>% 
  select(pct_neg, name) %>%
  arrange(-pct_neg) %>% 
  head(10)

sent_pos_df %>% 
  select(pct_pos, name) %>%
  arrange(-pct_pos) %>% 
  head(10)

sent_df <- cbind(sent_pos_df[,1:3],sent_neg_df[,2])
#names(sent_df) <- c("name", "pct_pos", "pos_word_count", "pct_neg", "neg_word_count")
sent_df$avg_word_freq <- word_freq$avg_word_freq
sent_df <- cbind(sent_df, word_freq[,3:4])
sent_df <- cbind(sent_df, pos_freq[,2:4])

library(scales)
track_df <- track_df[,1:16]

track_df <- track_df %>% 
  left_join(sent_df, by = 'name') %>% 
  #mutate_at(c('pct_neg', 'neg_word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
  #mutate_at(c('pct_pos', 'pos_word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(lyrical_density = word_count / duration_ms * 1000,
         gloom_index = round(rescale(1 - ((1 - valence) + (pct_neg * (1 + lyrical_density))) / 2, to = c(1, 100)), 2),
         cheer_index = round(rescale(1 - ((1 - valence) + (pct_pos * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

track_df$album_year <- as.numeric(track_df$album_year)
track_df$album_decade <- ifelse(track_df$album_year<1990, "1980s", 
                                ifelse(track_df$album_year<2000, "1990s",
                                       ifelse(track_df$album_year<2010, "2000s", "2010s")))

track_df %>%
  select(cheer_index, name, album_name) %>%
  arrange(-cheer_index) %>%
  head(10)

track_df %>%
  select(gloom_index, name, album_name) %>%
  arrange(-gloom_index) %>%
  head(10)

##### Visualising Gloom and Cheer Indexes #######
plot_df <- track_df %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>% 
  ungroup

avg_line <- plot_df %>% 
  group_by(album_release_year, album_name, album_img) %>% 
  summarise(avg = mean(gloom_index)) %>% 
  ungroup %>% 
  transmute(x = as.numeric(as.factor(album_release_year)), 
            y = avg,
            tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                             '</a>'))
plot_track_df <- plot_df %>% 
  mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', gloom_index, '</a>'),
         album_number = as.numeric(as.factor(album_release_year))) %>% 
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = gloom_index, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 100, title = list(text = 'Gloom Index')) %>% 
  hc_title(text = 'Data Driven Depression') %>% 
  hc_subtitle(text = 'Radiohead song sadness by album') %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[10]]$name <- 'Album Averages'
album_chart

#### Word clouds per decades ####

library(tm)
library(SnowballC)
library(wordcloud)
require(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")

par(mfrow=c(2,2))
wordcloud(subset(track_df, album_decade == "1980s")$lyrics, 
          min.freq = 30, colors=pal2)
wordcloud(subset(track_df, album_decade == "1990s")$lyrics, 
          min.freq = 30, colors=pal2)
wordcloud(subset(track_df, album_decade == "2000s")$lyrics, 
          min.freq = 30, colors=pal2)
wordcloud(subset(track_df, album_decade == "2010s")$lyrics, 
          min.freq = 30, colors=pal2)
par(mfrow=c(1,1))


library(qdap)

track_df_compl <- track_df[complete.cases(track_df[,13]),]

require(stringr)
library(stringi)

track_df$n.char <- sapply(track_df$lyrics, 
                                function(x) stri_length(x) - stri_count_fixed(x, " "))
#track_df$word_len <- track_df$n.char/track_df$word_count
track_df$word_count_uniq <- sapply(track_df$lyrics,
                                         function(x) length(unique(trimws(unlist(strsplit(x, "[^[:alnum:]]"))))))
track_df$lexic_diversity <- track_df$word_count_uniq/track_df$word_count


#### Visualisation of Music Features #### 

#### 1. Word repetitions ####

track_df$album_year <- as.factor(track_df$album_year)
ggplot(track_df, aes(x = album_year, y = avg_word_freq))+
  geom_boxplot(aes(as.factor(album_year))) +
  #geom_point(position = position_jitter(h = 0))+
  #geom_smooth(method = "lm")
  ylim(0,10) +
  xlab("Album release year")+
  ylab("Repetition Score (Average repetitions per word)")

track_df$album_year <- as.numeric(as.character(track_df$album_year))  
ggplot(track_df, aes(x = album_year, y = avg_word_freq))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  ylim(0,10) +
  xlab("Album release year")+
  ylab("Repetition Score (Average repetitions per word)")

avg_repeat <- track_df %>%
  group_by(album_year) %>%
  summarise(avg_rep = mean(avg_word_freq, na.rm = T))
avg_repeat <- avg_repeat[complete.cases(avg_repeat),]
ggplot(avg_repeat, aes(x = album_year, y = avg_rep))+
  geom_line()+
  xlab("Album release year")+
  ylab("Repetition Score (Average repetitions per word)")

#### 2. Lexical Diversity ####

track_df$album_year <- as.factor(track_df$album_year)
ggplot(track_df, aes(x = album_year, y = lexic_diversity))+
  geom_boxplot(aes(as.factor(album_year))) +
  xlab("Album release year")+
  ylab("Lexical Diversity (Unique words/Total words)")

track_df$album_year <- as.numeric(as.character(track_df$album_year))  
ggplot(track_df, aes(x = album_year, y = lexic_diversity))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Lexical Diversity (Unique words/Total words)")

track_df$album_year <- as.factor(track_df$album_year)
avg_lex_diversity <- track_df %>%
  group_by(album_year) %>%
  summarise(avg_lex_div = mean(lexic_diversity, na.rm = T))
avg_lex_diversity <- avg_lex_diversity[complete.cases(avg_lex_diversity),]
ggplot(avg_lex_diversity, aes(x = album_year, y = avg_lex_div))+
  geom_line()+
  xlab("Album release year")+
  ylab("Lexical Diversity (Unique words/Total words)")

#### 3. Word Counts ####

track_df$album_year <- as.numeric(as.character(track_df$album_year))  
ggplot(track_df, aes(x = album_year, y = word_count))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Word count (all words)")
ggplot(track_df, aes(x = album_year, y = word_count_uniq))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Word count (unique words)")
ggplot(track_df, aes(x = album_year, y = noun_uniq_count))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Unique Noun count")
ggplot(track_df, aes(x = album_year, y = adj_uniq_count))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Unique Adjective count")

track_df$album_year <- as.factor(track_df$album_year)
tot_wc <- track_df %>%
  group_by(album_year) %>%
  summarise(avg_wc = mean(word_count, na.rm = T),
            avg_wc_uniq = mean(word_count_uniq, na.rm = T),
            noun_wc = mean(noun_notuniq_count, na.rm = T),
            noun_wc_uniq = mean(noun_uniq_count, na.rm = T),
            adj_wc = mean(adj_notuniq_count, na.rm = T),
            adj_wc_uniq = mean(adj_uniq_count, na.rm = T))
tot_wc <- tot_wc[complete.cases(tot_wc),]
ggplot(tot_wc, aes(x = album_year))+
  geom_line(aes(y = avg_wc, group = 1, color = "All words"))+
  #geom_smooth(aes(y = avg_wc))+
  geom_line(aes(y = avg_wc_uniq, group = 1, color = "Unique words"))+
  #geom_smooth(aes(y = avg_wc_uniq))+
  geom_line(aes(y = noun_wc_uniq, group = 1, color = "Unique nouns"))+
  #geom_smooth(aes(y = noun_wc_uniq))+
  geom_line(aes(y = adj_wc_uniq, group = 1, color = "Unique adjectives"))+
  xlab("Album release year")+
  ylab("Word count")

#### 4. Adjectives/nouns popular by decades ####
library(dplyr)

top_nouns <- top_nouns %>%
  arrange(album_decade, desc(word_freq)) %>%
  mutate(order = row_number(album_decade))
top_adj <- top_adj %>%
  arrange(album_decade, desc(word_freq)) %>%
  mutate(order = row_number(album_decade))

ggplot(top_nouns, aes(x=order, y=word_freq))+
  geom_bar(stat="identity")+
  facet_wrap(~album_decade, ncol = 1, scales="free_x")+
  scale_x_continuous(
    breaks = top_nouns$order,
    labels = top_nouns$word,
    expand = c(0,0))+
  xlab("Vocabulary")+
  ylab("Word frequency")

ggplot(top_adj, aes(x=order, y=word_freq))+
  geom_bar(stat="identity")+
  facet_wrap(~album_decade, ncol = 1, scales="free_x")+
  scale_x_continuous(
    breaks = top_adj$order,
    labels = top_adj$word,
    expand = c(0,0))+
  xlab("Vocabulary")+
  ylab("Word frequency")

#### 5. Correlation between musical and lexical features #####

corrplot::corrplot(cor(track_df[,c(2,4:12,17:28,30:32)], use="complete.obs"),
                         #lower = "circle", upper = "number", 
                         type = "lower",
                         #main = "WBM", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))

#### 6. Musical features ####
track_df$album_year <- as.factor(track_df$album_year)
mus_feat <- track_df %>%
  group_by(album_year) %>%
  summarise(danc = mean(danceability, na.rm = T),
            enrg = mean(energy, na.rm = T),
            speech = mean(speechiness, na.rm = T),
            acst = mean(acousticness, na.rm = T),
            instr = mean(instrumentalness, na.rm = T),
            live = mean(liveness, na.rm = T),
            val = mean(valence, na.rm = T))
mus_feat <- mus_feat[complete.cases(mus_feat),]

ggplot(mus_feat, aes(x = album_year))+
  geom_line(aes(y = danc, group = 1, color = "Danceability"))+
  geom_smooth(aes(y = danc)) +
  geom_line(aes(y = enrg, group = 1, color = "Energy"))+
  geom_smooth(aes(y = enrg)) +
  xlab("Album release year")+
  ylab("Value")
ggplot(mus_feat, aes(x = album_year))+
  geom_line(aes(y = instr, group = 1, color = "Instrumentalness"))+
  geom_smooth(aes(y=instr)) +
  geom_line(aes(y = val, group = 1, color = "Valence"))+
  geom_smooth(aes(y=val)) +
  xlab("Album release year")+
  ylab("Value")
ggplot(mus_feat, aes(x = album_year))+
  geom_line(aes(y = live, group = 1, color = "Liveness"))+
  #geom_smooth(aes(y = live)) +
  geom_line(aes(y = val, group = 1, color = "Valence"))+
  #geom_smooth(aes(y = val)) +
  xlab("Album release year")+
  ylab("Value")


ggplot(track_df, aes(x = album_year, y = tempo))+
  geom_point(position = position_jitter(h = 0))+
  geom_smooth()+ # method = "lm" for a linear trend
  xlab("Album release year")+
  ylab("Tempo")


##### Latent Dirichlet Analysis. Topic Modelling #####

#Create document-term matrix
library(tm)
docs <- Corpus(VectorSource(track_df$lyrics))
#remove punctuation
docs <- tm_map(docs, removePunctuation)
#Strip digits
docs <- tm_map(docs, removeNumbers)
#remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
#remove whitespace
docs <- tm_map(docs, stripWhitespace)
#additional stop words
myStopwords <- c("dont", "chorus", "youre", "just", "gonna", "ill", "cant",
                 "ive", "thats", "going", "theres", "pre", "ooh", "wont", "shes",
                 "youll", "mmm", "wanna", "will", "got", "get", "yeah", "doesnt",
                 "hes", "oooh", "can", "youve", "till", "else")
docs <- tm_map(docs, removeWords, myStopwords)



dtm <- DocumentTermMatrix(docs)

freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
a<- as.data.frame(freq[ord])

library(topicmodels)

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

#Number of topics
k <- 2

#Run LDA using Gibbs sampling
rowTotals <- apply(dtm , 1, sum)  #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]  #remove docs without words

ldaOut <-LDA(dtm.new, k, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, 
                          best=best, burnin = burnin, 
                          iter = iter, thin=thin))

#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut, 50))

pal2 <- brewer.pal(8,"Dark2")

par(mfrow=c(1,2))
wordcloud(ldaOut.terms[,1], min.freq = 1, colors = "red")
wordcloud(ldaOut.terms[,2], min.freq = 1, colors = "blue")
par(mfrow=c(1,1))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)

library(tidytext)

ap_topics <- tidy(ldaOut, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


