library(polite)
library(rvest)
library(purrr)
library(janitor)

library(tidyverse)

# Setting up variables -------

rdata <- read.csv("Final Project/streaming_service.csv")
# get the streaming data
stream <- rdata |>
  # you have to use paste0 because default paste adds a space in between
  mutate(date=as.POSIXct(paste0("01-",date),format='%d-%b-%Y'))|> 
  mutate(service = as.factor(service))

# get the days that we need to search through
days <- seq.Date(as.Date(min(stream$date)),as.Date(max(stream$date)),by="month")

# https://web.archive.org/web/20160201231417/https://thepiratebay.se/top/48hall
host <- "https://archive.org/"

extension <- c("https://thepiratebay.org/top/48hall","https://thepiratebay.org/search.php?q=top100:all")
# https://thepiratebay.org/top/48hall from around 2021 to 2024
# https://thepiratebay.org/search.php?q=top100:all from before 2021

piracy = data.frame()


# Querying Internet Archive --------------------------------------------------



# using the polite package to read and obey the robots.txt
# use the session for rvest stuff, use polite_GET instead of 
# httr::GET, we will use both eventually

polite_GET <- politely(httr::GET,delay=1, verbose=TRUE) 
polite_json_GET <- politely(jsonlite::read_json,delay=1,verbose=T)

dayframe <- data.frame(days, old_url=NA,new_url=NA)

# create a shorter test dataset to test with
sdayframe <- head(dayframe,20)

# example of how to use rvest with polite
# current_page <- nod(session, "https://archive.org/wayback/available?url=https://thepiratebay.org/search.php?q=top100:all&timestamp=20250329181544")|>
#   scrape()

# example of how to use httr::GET politely (we defined polite_GET above)
# test <- polite_GET("https://archive.org/wayback/available?url=https://thepiratebay.org/search.php?q=top100:all&timestamp=20250329181544")

# how to extract the content from the GET into human legible format
# print(rawToChar(test$content))

# grab closest timestamp
# check to see if timestamp is within target month
# get request

base_url = "https://archive.org/wayback/available?url="

# get responses back from the internet archive, trying to see the closest
# available timestamp
# TAKES TIME

responses <- pmap_df(dayframe,\(days,old_url,new_url){
  my_timestamp <- paste0(format(days,'%Y%m%d'),"000000")
  monthly_timestamp_old <- paste0(base_url,extension[1],"&timestamp=",my_timestamp)
  monthly_timestamp_new <- paste0(base_url,extension[2],"&timestamp=",my_timestamp)
  response_old <- rawToChar(polite_GET(monthly_timestamp_old)$content)
  response_new <- rawToChar(polite_GET(monthly_timestamp_new)$content)
  data.frame(
    day = days,
    # we need to do this all messy like because 
    old = response_old,
    new = response_new
  )
})

save(responses, file="Final Project/Initial Responses.Robj")
load("Final Project/Initial Responses.Robj")

# extracting the timestamp from the json responses
unwrapped_responses <- responses |>
  # the backslash is how you create a lambda function in R
  # we want the df variant of pmap because it returns a dataframe
  # I don't remember at this point why we want pmap instead of the other ones
  pmap_df(\(day,old,new){
    unwrapped_old <- rjson::fromJSON(old)$archived_snapshots
    unwrapped_new <- rjson::fromJSON(new)$archived_snapshots
    
    data.frame(
      day = day,
      old_timestamp = if (is.null(unwrapped_old$closest)) NA else unwrapped_old$closest$timestamp,
      new_timestamp = if (is.null(unwrapped_new$closest)) NA else unwrapped_new$closest$timestamp
    )
  })

# doing some type conversion, then we have to split the data frame in half
# so we can perform the filters on the halves
# we then rejoin the dataframes together, allowing for the creation of a master
# list of dates and timestamps to query

old_timed_responses <- unwrapped_responses |>
  mutate(
    time_old_timestamp = old_timestamp |> as.POSIXct(format='%Y%m%d%H%M%S'),
    time_new_timestamp = new_timestamp |> as.POSIXct(format='%Y%m%d%H%M%S'),
    month_old = month(time_old_timestamp),
    month_new = month(time_new_timestamp),
    month = month(day),
    year_old = year(time_old_timestamp),
    year_new = year(time_new_timestamp),
    year = year(day)
  )|>
  dplyr::filter(month==month_old & year==year_old)|>
  select(day,old_timestamp)
  

new_timed_responses <- unwrapped_responses |>
  mutate(
    time_old_timestamp = old_timestamp |> as.POSIXct(format='%Y%m%d%H%M%S'),
    time_new_timestamp = new_timestamp |> as.POSIXct(format='%Y%m%d%H%M%S'),
    month_old = month(time_old_timestamp),
    month_new = month(time_new_timestamp),
    month = month(day),
    year_old = year(time_old_timestamp),
    year_new = year(time_new_timestamp),
    year = year(day)
  )|>
  dplyr::filter(month==month_new & year==year_new)|>
  select(day,new_timestamp)

full_join(new_timed_responses,old_timed_responses) |>
  arrange(day)|>
  # did the last few steps a few times in order to get more data
  write_csv("Final Project/Timestamps10.csv")

timestamps1 <- read.csv("Final Project/Timestamps1.csv")
timestamps2 <- read.csv("Final Project/Timestamps2.csv")
timestamps3 <- read.csv("Final Project/Timestamps3.csv")
timestamps4 <- read.csv("Final Project/Timestamps4.csv")
timestamps5 <- read.csv("Final Project/Timestamps5.csv")
timestamps6 <- read.csv("Final Project/Timestamps6.csv")
timestamps7 <- read.csv("Final Project/Timestamps7.csv")
timestamps8 <- read.csv("Final Project/Timestamps8.csv")
timestamps9 <- read.csv("Final Project/Timestamps9.csv")
timestamps10 <- read.csv("Final Project/Timestamps10.csv")


master_timestamps <- full_join(timestamps1,timestamps2)|>
  full_join(timestamps3)|>
  full_join(timestamps4)|>
  full_join(timestamps5)|>
  full_join(timestamps6)|>
  full_join(timestamps7)|>
  full_join(timestamps8)|>
  full_join(timestamps9)|>
  full_join(timestamps10)|>
  mutate(
    old_timestamp = as.character(old_timestamp),
    new_timestamp = as.character(new_timestamp)
         )|>
  arrange(day)

# example url
# https://web.archive.org/web/20200526060433/https://thepiratebay.org/search.php?q=top100:all

base_url <- "https://web.archive.org/web/"

master_url <- master_timestamps |>
  mutate(
    old_url = case_when(is.na(old_timestamp) ~ NA,
                        .default = paste0(base_url,old_timestamp,"/",extension[1])),
    new_url = case_when(is.na(new_timestamp) ~ NA,
                        .default = paste0(base_url,new_timestamp,"/",extension[2]))
  )

# not working right now, probably need to use either httr more or use
# rvest in order to properly scrape the webpage
# it keeps on saying that the response I have can't be coerced to a dataframe, but it
# was just fine previously, so idk

# turl <- "https://web.archive.org/web/20220809222124/https://thepiratebay.org/search.php?q=top100:all"
# 
# read_html(turl) |>
#   html_elements(".list-item")



# bow(turl) |>
#   scrape(content="text/html; charset=UTF-8") |>
#   html_elements(".item-leech , .item-seed")#|>
#   html_text()
# leachers <- bow(turl) |>
#   scrape(content="text/plain; charset=UTF-8") |>
#   html_elements(".item-leech")|>
#   html_text()

# test <- jsonlite::read_json("https://web.archive.org/web/20220819142144/https://apibay.org/precompiled/data_top100_all.json") 
 
# map_vec(test,\(element){element$seeders})
 
# as.data.frame(test)

real_responses <- master_url |> 
  pmap_df(\(day,new_timestamp,old_timestamp,old_url,new_url){
  # if (!is.na(old_url)) response_old <- polite_GET(old_url) else response_old <- NA
  # if (!is.na(new_url)) response_new <- polite_GET(new_url) else response_new <- NA
  if (!is.na(old_url)){
    html_response <- bow(old_url) |>
      scrape(content="text/html; charset=UTF-8")
    seeders <- html_response |>
      html_elements("td:nth-child(6)")|>
      html_text()
    leechers <- html_response |>
      html_elements("td:nth-child(7)")|>
      html_text()
    # data <- data.frame(seeders=seeders,leechers=leechers)
  }
  if (!is.na(new_url)){
    json_url = paste0(base_url,new_timestamp,"/https://apibay.org/precompiled/data_top100_all.json")
    json_response <- polite_json_GET(json_url)
    seeders=map_vec(json_response,\(element){element$seeders})
    leechers=map_vec(json_response,\(element){element$leechers})
    # data <- list(seeders,leechers)
  }
  
  leechers <- sum(as.numeric(leechers))
  seeders <- sum(as.numeric(seeders))
  pirates = leechers + seeders
  
  
  print(pirates)
    
  data.frame(
    day = day,
    leechers = leechers,
    seeders = seeders
  )
})

# both responses 1 and 2 look the same, so I'm assuming that the data we got is
# all we're ever going to get, at least using current techniques
# maybe all the zeros we're receiving are redirects, in which case, we probably
# ought to follow them with a true headless browser, but I *really* don't want
# to do that: OOP gives me the runs

real_responses |>
  filter(leechers!=0)|>
  write_csv("Final Project/Responses_Final.csv")

View(real_responses)
# to sum up the Seeders and Leachers just run purrr::reduce(sum(whatever))
# on a list of dataframes, to reduce the list to a list of seeders and leachers






