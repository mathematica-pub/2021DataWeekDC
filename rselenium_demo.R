#################################################################################
#' This demo shows how to systematically scrape public comments from web        #
#'                                                                              #
#' Link: https://www.federalregister.gov/documents/2021/04/15/2021-07762/       #
#' ensuring-access-to-equitable-affordable-client-centered-quality-             #
#' family-planning-services                                                     #
#'                                                                              #
#' Program developed by Mathematica                                             #
#' Last update: 4/30/2021                                                       #
#                                                                               #
#################################################################################

# load packages -----------------------------------------------------------

library(tidyverse)
library(RSelenium) # to download this pkg, try devtools::install_github("ropensci/RSelenium")
library(jsonlite)
library(lubridate)


# load urls ---------------------------------------------------------------

# landing page of the public comments
main_url <- "https://www.regulations.gov/document/HHS-OS-2021-0010-0001/comment?" 

# tips to successfully load the chrome driver
# 1. check chrome version and set the closest version to chromever:
# use binman::list_versions("chromedriver") to check chrome driver version
# more to read: https://github.com/ropensci/RSelenium/issues/203

# start a selenium server and browser (try other port number if error returns with msg
# "Selenium server signals port = 1236 is already in use.")
rD <- rsDriver(browser = "chrome", port = 1330L, geckover = NULL, 
               chromever =  "91.0.4472.101", iedrver = NULL, 
               phantomver = NULL)

remDr <- rD[["client"]] 

# scrape public comments --------------------------------------------------

query_date <- ymd("2021-05-08")
first_id <- 31307
last_id <- 31317

# tmp file saves all the metadata as well as raw public comments from webscraping
tmp <- data.frame(
  post_date = as.character(query_date),
  receive_date = NA,
  id = first_id:last_id,
  comments_raw = NA,
  attachments = NA
)

# scrape specific public comment ------------------------------------------

tic <- Sys.time()

for (i in 1:dim(tmp)[1]){

  url_pc <- str_c("https://www.regulations.gov/comment/HHS-OS-2021-0010-", tmp$id[i])
  
  remDr$navigate(url_pc)
  
  # get raw public comments
  pc_content <- remDr$findElement(using = "class", "px-2")
  tmp[["comments_raw"]][i] <- pc_content$getElementText()[[1]]
  
  # print an error if we reach the website's rate limit so we know which comments to re-do.
  if(grepl("an error has occurred",  tmp[["comments_raw"]][i])){
    print(str_c("An error has occurred on ", tmp$id[i]))
  }
  
  # Get attachments if exists
  attachment_exist <- grepl("Download Icon", remDr$findElement(using='css selector',"body")$getElementText()[[1]])
  tmp[["attachments"]][i] <- ifelse(attachment_exist, "Y", NA)
  
  # Get receive date
  tmp[["receive_date"]][i] <- remDr$findElement(using='css selector',"body")$getElementText()[[1]] %>% gsub(".*Received Date\\\n(.*)\\\nPage Count.*", "\\1", .)
  
}

    
toc <- Sys.time()
toc - tic

# save scraped public comments to a new dataframe and clean up a little bit
dat_pc <- tmp %>%
  mutate(id = as.character(id),
         receive_date = mdy(receive_date)) %>% 
  mutate(comments_clean = comments_raw %>% str_trim(side = "both") %>% str_squish()) 



# close the selenium server once complete
remDr$close()
rD$server$stop() 
rm(rD, remDr)
gc()
