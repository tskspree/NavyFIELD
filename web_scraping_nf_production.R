# Get NF player data from https://www.navyfield.com

library(Rcrawler)
library(foreach)
library(doParallel)

Sys.setlocale("LC_CTYPE", locale="Chinese")

# Ranking Data

pattern <- c(".content_table_R a", 
             ".content_table_R td:nth-child(4)",
             ".content_table_R td:nth-child(5)",
             ".content_table_R td:nth-child(6)",
             ".content_table_R td:nth-child(7)")

pattern_names <- c("id", 
                   "wins",
                   "battles",
                   "fleet",
                   "squad")

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

scraper <- function(page){
  target_url <- paste("https://www.navyfield.com/Community/Ranking/List.aspx?page=",
                      toString(page),"&server=1&category=4&searchType=1&searchValue=", sep = "")
  web_content <- Rcrawler::ContentScraper(Url=target_url,
                                CssPatterns = pattern,
                                PatternsName = pattern_names, 
                                ManyPerPattern = TRUE, 
                                astext = TRUE, 
                                asDataFrame = FALSE)
  output <- as.data.frame(web_content)
  return(output)
}

rank_data <- foreach(i=1:500, .combine=rbind) %dopar% {
  tempMatrix = scraper(i) 
  tempMatrix
}

# Hall of Fame Data

hof_pattern <- c(".rnum", 
             ".content_table_H td:nth-child(4)",
             ".content_table_H td:nth-child(5)",
             ".content_table_H td:nth-child(6)")

hof_pattern_names <- c("id", 
                   "wins_total",
                   "fleet",
                   "squad")

hof_scraper <- function(page){
    target_url <- paste("https://www.navyfield.com/Community/HallofFame/List.aspx?page=",
                        toString(page),"&server=&sort=1&searchType=1&searchValue=", sep = "")
    web_content <- Rcrawler::ContentScraper(Url=target_url,
                                  CssPatterns = hof_pattern,
                                  PatternsName = hof_pattern_names, 
                                  ManyPerPattern = TRUE, 
                                  astext = TRUE, 
                                  asDataFrame = FALSE)
    output <- as.data.frame(web_content)
    return(output)
}

hof_data <- foreach(i=1:7, .combine=rbind) %dopar% {
  tempMatrix = hof_scraper(i) 
  tempMatrix 
}

hof_data <- as.data.frame(lapply(hof_data, function(x) {
  gsub("\u00A0", "", x) 
})) 

keyword1 <- "/.*"
hof_data$wins <- sub(keyword1, "", hof_data$wins_total)
keyword2 <- ".*/"
hof_data$battles <- sub(keyword2, "", hof_data$wins_total)

hof_clean <- subset(hof_data, select = c(id, fleet:battles))

# Combine Ranking and HoF Data and Clean

nf_data <- rbind(rank_data, hof_clean)
nf_data$id <- trimws(nf_data$id, which = c("both"))
nf_data$fleet <- trimws(nf_data$fleet, which = c("both"))
nf_data$squad <- trimws(nf_data$squad, which = c("both"))

nf_data$wins <- as.numeric(gsub(",", "", as.character(nf_data$wins)))
nf_data$battles <- as.numeric(gsub(",", "", as.character(nf_data$battles)))

# Compute Winrate

nf_data$win_rate <- nf_data$wins/nf_data$battles
