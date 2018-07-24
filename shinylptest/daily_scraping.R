library(RMySQL)
library(jsonlite)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

options(mysql = list(
  "host" = "202.52.146.25",
  "port" = 3306,
  "user" = "isiindon",
  "password" = "pNo9rM05i1"
))
databaseName <- "isiindon_evote"
table <- "dataharga"
table_komoditas <- "komoditas"

db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                port = options()$mysql$port, user = options()$mysql$user, 
                password = options()$mysql$password)

get_komoditas <- function() {
  query <- sprintf("SELECT * FROM `komoditas` ", table_komoditas)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  data
}
data_komoditas<-get_komoditas()

for(i in 1:nrow(data_komoditas)){
  if(data_komoditas$id_situs[i] == 1){
    mydata <- fromJSON(data_komoditas$link[i])
    harga<-gsub("Rp ", "",mydata$data$avg_price$value) 
    harga<- gsub("/.*","",harga)
    harga<-as.numeric(harga)*1000
    query3 <- sprintf(
      "INSERT INTO %s (ds, %s) VALUES ('%s', %s) ON DUPLICATE KEY UPDATE %s = %s;",
      table,
      data_komoditas$alias[i],
      #format(Sys.Date(), format="%Y-%m-%d"),
      "2018-03-20",
      harga,
      data_komoditas$alias[i],
      harga
    )
    print(query3)
    dbGetQuery(db, query3)
    
  } else if(data_komoditas$id_situs[i] == 2){
    webpage <- read_html(data_komoditas$link[i])
    results <- webpage %>% html_nodes(".prod-det-crnt-price ") %>% html_text(trim = TRUE)
    harga<-gsub("Rp.*?Rp.", "",results)
    harga<-gsub("Rp.", "",harga)
    harga<-as.numeric(harga)*1000
    query3 <- sprintf(
      "INSERT INTO %s (ds, %s) VALUES ('%s', %s) ON DUPLICATE KEY UPDATE %s = %s;",
      table,
      data_komoditas$alias[i],
      #format(Sys.Date(), format="%Y-%m-%d"),
      "2018-03-20",
      harga,
      data_komoditas$alias[i],
      harga
    )
    print(query3)
    dbGetQuery(db, query3)
  } else if(data_komoditas$id_situs[i]==3){
    webpage <- read_html(data_komoditas$link[i])
    results <- webpage %>% html_nodes(".desc-top > h2") %>% html_text()
    harga<-gsub("0R.*", "",results)
    harga<-gsub("Rp", "",harga)
    harga<-as.numeric(harga)*1000
    query3 <- sprintf(
      "INSERT INTO %s (ds, %s) VALUES ('%s', %s) ON DUPLICATE KEY UPDATE %s = %s;",
      table,
      data_komoditas$alias[i],
      #format(Sys.Date(), format="%Y-%m-%d"),
      "2018-03-20",
      harga,
      data_komoditas$alias[i],
      harga
    )
    print(query3)
    dbGetQuery(db, query3)
  }
}

dbDisconnect(db)

mydata <- fromJSON("https://api.happyfresh.com/api/stock_locations/401/products/search?q=Mila Multipurpose Wheat Flour&corrections=true&popular=true&per_page=240")
records <- vector("list", length = mydata$count)
for(i in 1:mydata$count){
  nama_produk <- mydata$products$name[i]
  harga <- mydata$products$price[i]
  link<-str_c("https://www.happyfresh.id/stores/carrefour/locations/401/products/",mydata$products$id[i])
  records[[i]] <- data_frame(nama = nama_produk,harga = harga,link=link)
}

df <- bind_rows(records)
data_scraper$data<-df

mydata <- fromJSON("https://api.happyfresh.com/api/stock_locations/401/products/27590")

time<-"06:00:00"

if((time > "05:30:00") && (time < "06:30:00")){
  print("halo")
}
