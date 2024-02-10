##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#   Gustavo Castilllo
# DESCRIPTION:
#   Scrape the dataset for PS1 from Ignacio's website.
##############################################################################-

source("scripts/00_packages.R")



# With RSelenium
remrD$setTimeout(type = "page load",
                 milliseconds = 20000)


myurl <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

# l <- list()
for (i in 6:10){
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
  
  cat("\n\n")
}



write_rds(l, file = "stores/geih_list.rds")


