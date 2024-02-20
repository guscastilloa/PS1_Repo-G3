##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#   Gustavo Castilllo
# DESCRIPTION:
#   Scrape the dataset for PS1 from Ignacio's website.
##############################################################################-

# Prepare workspace
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Web scraping ----
############################################################################-

# With RSelenium
remrD$setTimeout(type = "page load",
                 milliseconds = 20000)


myurl <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

l <- list()
for (i in 1:10){
  the_url <- paste0(myurl, "page",i,".html")
  print(the_url)
  
  rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
  remrD <- rD$client
  remrD$open()
  # Scraping
  # Making the code sleep for 5 seconds
  
  remrD$navigate(the_url)
  Sys.sleep(20)
  
  page_source <- remrD$getPageSource()
  
  webpage <- read_html(page_source[[1]])
  table_nodes <- rvest::html_nodes(webpage, xpath="/html/body/div/div/div[2]/div/table")
  
  cat("Nodos: \n\n ******************")
  print(table_nodes)
  
  l[[i]] <- html_table(table_nodes)
  
  # Close the remote driver
  remrD$close()
  rD$server$stop()
  
  cat("............................ End iteration................... \n\n")
}

# Export temporal list
write_rds(l, file = "stores/geih_list.rds")

# Scrape and save leve labels and variable labels
theurl <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html"
site <- read_html(theurl)
level_labels <-  site %>% html_table() %>% .[[1]]
write_rds(level_labels, file = "stores/level_labels.rds")

theurl <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html"
site <- read_html(theurl)
variable_labels <-  site %>% html_table() %>% .[[1]]
write_rds(variable_labels, file = "stores/variable_labels.rds")

############################################################################-
# 2. Build dataset and export----
############################################################################-
t <- 0
for (i in 1:10){
  t <- t+nrow(l[[i]][[1]])
  print(t)
}
t
b <- bind_rows(l, .id = "column_label")

# Check row binds and export
nrow(b)==t
arrow::write_parquet(b, sink = "stores/geih.parquet")


