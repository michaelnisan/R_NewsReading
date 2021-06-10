# Financial News reading and sentiment Analysis
# Lazy Trading Course: Read news and Sentiment Analysis
# (C) 2018 Vladimir Zhbanko
# https://www.udemy.com/forex-news-and-sentiment-analysis/?couponCode=LAZYTRADE1-10

# Purpose: Read headers of the yahoo news and perform their sentiment analysis

# libraries
library(rvest)
#library(qdap)
library(syuzhet)
# you should also: install.packages("openssl")

# url of websites (choose your own) and variables
urlUS <- "https://www.yahoo.com/news/us/"
urlCA <- "https://ca.news.yahoo.com/canada/"
urlGB <- "https://uk.news.yahoo.com/uk/"

# These urls, I added based on my search
urlAU <- "https://au.news.yahoo.com/latest/"
urlJP <- "https://mainichi.jp/english/japan/"
urlNZ <- "https://nz.news.yahoo.com/"
urlSW <- "https://www.swissinfo.ch/eng/latest-news/"
urlEU <- "https://www.euronews.com/programs/realeconomy/"

# modified pairs
pairs <- c("USDCAD", "GBPCAD", "GBPUSD", "AUDUSD", "EURUSD", "NZDUSD", "USDCHF", "USDJPY",
		   "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD", "GBPCHF",
		   "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY", "AUDNZD", "CADJPY", "CHFJPY",
		   "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")

# get the headers into the vectors

heads_US <- urlUS %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_CA <- urlCA %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_UK <- urlGB %>% read_html() %>% html_nodes("h3") %>% html_text()

#heads extra to URLs
heads_AU <- urlAU %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_JP <- urlJP %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_NZ <- urlNZ %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_SW <- urlSW %>% read_html() %>% html_nodes("h3") %>% html_text()
heads_EU <- urlEU %>% read_html() %>% html_nodes("h3") %>% html_text()


# Code is altered from here
# get the polarity scores
pol_US <- syuzhet::get_sentiment(
  char_v = heads_US,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)
pol_CA <- syuzhet::get_sentiment(
  char_v = heads_CA,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)
pol_UK <- syuzhet::get_sentiment(
  char_v = heads_UK,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)

# polarity for other countries
pol_AU <- syuzhet::get_sentiment(
  char_v = heads_AU,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)

pol_JP <- syuzhet::get_sentiment(
  char_v = heads_JP,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)
pol_NZ <- syuzhet::get_sentiment(
  char_v = heads_NZ,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)
pol_SW <- syuzhet::get_sentiment(
  char_v = heads_SW,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)
pol_EU <- syuzhet::get_sentiment(
  char_v = heads_EU,
  method = "syuzhet",
  path_to_tagger = NULL,
  cl = NULL,
  language = "english",
  lexicon = NULL,
  regex = "[^A-Za-z']+",
  lowercase = TRUE
)

# extract average values
ave_US <- mean(pol_US)
ave_CA <- mean(pol_CA)
ave_UK <- mean(pol_UK)
ave_AU <- mean(pol_AU)
ave_JP <- mean(pol_JP)
ave_NZ <- mean(pol_NZ)
ave_SW <- mean(pol_SW)
ave_EU <- mean(pol_EU)


#modified table
table(ave_US, ave_CA, ave_UK, ave_AU, ave_JP, ave_NZ, ave_SW, ave_EU)

# modified summary
summmary_df <- data.frame(day = Sys.time(),
                          country = c('US', 'CA', 'UK', 'AU', 'JP', 'NZ', 'SW', 'EU'),
                          ave_pol = c(ave_US, ave_CA, ave_UK, ave_AU, ave_JP, ave_NZ, ave_SW, ave_EU))

# hash of the time
id_hash <- Sys.time() %>% as.character.POSIXt() %>% openssl::sha1() %>% substr(1, 4)

# write the log
write.csv(summmary_df,
          paste0("C:/Users/Admin/Documents/000_TradingRepo/R_NewsReading/log/s_log-",
                 Sys.Date(), "-", id_hash, ".csv"),
          row.names = F)

# decision function
write_news_sentiment_decision <- function(pair_string,
                                          base_curr_polarity,
                                          quot_curr_polarity,
                                          diff_thresh,
                                          sand_box_path){
  # evaluate direction (0 - buy, 1 - sell, -1 - do nothing)
  # pair_string <- "GBPUSD"
  # base_curr_polarity <- ave_UK
  # quot_curr_polarity <- ave_US
  # diff_thresh <- 0.02
  # sand_box_path <- getwd()
  if(base_curr_polarity > quot_curr_polarity) flag <- 0 #buy
  if(base_curr_polarity < quot_curr_polarity) flag <- 1
  # evaluate difference (too little will mean no effect)
  if(abs(abs(base_curr_polarity)- abs(quot_curr_polarity)) < diff_thresh) flag <- -1
  # write to the file
  write.csv(flag, paste0(sand_box_path, "/Sentiment_", pair_string, ".csv"),row.names = F)
}



# # decisions test (writing to the working directory)
# write_news_sentiment_decision("GBPUSD", ave_UK, ave_US, 0.02, getwd())
# write_news_sentiment_decision("USDCAD", ave_US, ave_CA, 0.02, getwd())
# write_news_sentiment_decision("GBPCAD", ave_UK, ave_CA, 0.02, getwd())

# decision to write in the sandbox of all Terminals
# path_T1 <- "C:/Program Files (x86)/MT4 Terminal 1/MQL4/Files",

paths <- c("C:/Program Files (x86)/MT4 Terminal 1/MQL4/Files",
    		   "C:/Program Files (x86)/MT4 Terminal 2/MQL4/Files",
		       "C:/Program Files (x86)/MT4 Terminal 3/MQL4/Files",
		       "C:/Program Files (x86)/MT4 Terminal 4/MQL4/Files")

for(PATH in paths){
# PATH <- "C:/Program Files (x86)/MT4 Terminal 1/MQL4/Files"
# decisions writing to the sandbox
write_news_sentiment_decision("GBPUSD", ave_UK, ave_US, 0.1, PATH)
write_news_sentiment_decision("USDCAD", ave_US, ave_CA, 0.1, PATH)
write_news_sentiment_decision("GBPCAD", ave_UK, ave_CA, 0.1, PATH)

# added decisions
write_news_sentiment_decision("AUDUSD", ave_AU, ave_US, 0.1, PATH)
write_news_sentiment_decision("EURUSD", ave_EU, ave_US, 0.1, PATH)
write_news_sentiment_decision("NZDUSD", ave_NZ, ave_US, 0.1, PATH)
write_news_sentiment_decision("USDCHF", ave_US, ave_SW, 0.1, PATH)
write_news_sentiment_decision("USDJPY", ave_US, ave_JP, 0.1, PATH)
write_news_sentiment_decision("EURGBP", ave_EU, ave_UK, 0.1, PATH)
write_news_sentiment_decision("EURJPY", ave_EU, ave_JP, 0.1, PATH)
write_news_sentiment_decision("EURCHF", ave_EU, ave_SW, 0.1, PATH)
write_news_sentiment_decision("EURNZD", ave_EU, ave_NZ, 0.1, PATH)
write_news_sentiment_decision("EURCAD", ave_EU, ave_CA, 0.1, PATH)
write_news_sentiment_decision("EURAUD", ave_EU, ave_AU, 0.1, PATH)
write_news_sentiment_decision("GBPAUD", ave_UK, ave_AU, 0.1, PATH)
write_news_sentiment_decision("GBPCHF", ave_UK, ave_SW, 0.1, PATH)
write_news_sentiment_decision("GBPJPY", ave_UK, ave_JP, 0.1, PATH)
write_news_sentiment_decision("GBPNZD", ave_UK, ave_NZ, 0.1, PATH)
write_news_sentiment_decision("AUDCAD", ave_AU, ave_CA, 0.1, PATH)
write_news_sentiment_decision("AUDCHF", ave_AU, ave_SW, 0.1, PATH)
write_news_sentiment_decision("AUDJPY", ave_AU, ave_JP, 0.1, PATH)
write_news_sentiment_decision("AUDNZD", ave_AU, ave_NZ, 0.1, PATH)
write_news_sentiment_decision("CADJPY", ave_CA, ave_JP, 0.1, PATH)
write_news_sentiment_decision("CHFJPY", ave_SW, ave_JP, 0.1, PATH)
write_news_sentiment_decision("NZDJPY", ave_NZ, ave_JP, 0.1, PATH)
write_news_sentiment_decision("NZDCAD", ave_NZ, ave_CA, 0.1, PATH)
write_news_sentiment_decision("NZDCHF", ave_NZ, ave_SW, 0.1, PATH)
write_news_sentiment_decision("CADCHF", ave_CA, ave_SW, 0.1, PATH)
}
# write them to T1 (modification)

# Result: DSS generates the file in the Sandbox. This file will be used in MQL by the Trading System
