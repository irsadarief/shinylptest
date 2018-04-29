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
      format(Sys.Date(), format="%Y-%m-%d"),
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
      format(Sys.Date(), format="%Y-%m-%d"),
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
      format(Sys.Date(), format="%Y-%m-%d"),
      harga,
      data_komoditas$alias[i],
      harga
    )
    print(query3)
    dbGetQuery(db, query3)
  }
}

dbDisconnect(db)

library(jsonlite)
mydata <- fromJSON("http://infopangan.jakarta.go.id/api/price/top?cid=1&type=market")
harga<-gsub("Rp ", "",mydata$data$avg_price$value) 
harga<- gsub("/.*","",harga)
harga<-as.numeric(harga)*1000

library(jsonlite)
mydata <- fromJSON("https://api.happyfresh.com/api/stock_locations/401/products/search?q=terigu&corrections=true&popular=true&per_page=240")
harga<-gsub("Rp ", "",mydata$data$avg_price$value) 
harga<- gsub("/.*","",harga)


webpage <- read_html("http://www.klikindomaret.com/product/tepung-terigu-1")
results <- webpage %>% html_nodes(".prod-det-crnt-price ") %>% html_text(trim = TRUE)
harga<-gsub("Rp.*?Rp.", "",results)
harga<-gsub("Rp.", "",harga)
harga<-as.numeric(harga)*1000

webpage <- read_html("https://shop.hypermart.co.id/hypermart/product/MARJAN-SYRUP-BOUDOIN-PSG-SUSU-460ML-37407860")
results <- webpage %>% html_nodes(".desc-top > h2") %>% html_text()
harga<-gsub("0R.*", "",results)
harga<-gsub("Rp", "",harga)
harga<-as.numeric(harga)*1000

if((format(Sys.time(), "%H:%M:%S") < "11:00:00") && (format(Sys.time(), "%H:%M:%S") > "10:00:00")){
  print("halo")
} else {
  print("nope")
}

