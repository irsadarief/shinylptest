library(rtweet)
library(httpuv)
library(prophet)
library(plotly)

Sys.setenv("plotly_username" = "Irsad.arief")
Sys.setenv("plotly_api_key" = "VFBsbrrTjuwFY7EqFoSr")

token <- create_token(
  app = "Scraper_Harga",
  consumer_key = "sm8HwpaGbhVVz2RCSbKp2xF8l",
  consumer_secret = "SJOUUqwYBbUSLsM9QHPW4tgaVzLPkI0mMJouzyG4Z76ovkQlTX")

file_name <- file.path("/twitter_token.rds")

## save token to home directory
saveRDS(token, file = file_name)

cat(paste0("TWITTER_PAT=", file_name),
    file = file.path(".Renviron"),
    append = TRUE)
library(gridExtra)
for(j in 1:27){
  i<-30-j
  testprophet<-cbind(data[,1],as.numeric((data[,i])))
  colnames(testprophet) <- c("ds","y")
  testprophet <- data.frame(testprophet)
  testprophet$y <- as.numeric(as.character(testprophet$y))
  testprophet$ds<-(as.Date(testprophet$ds, format = "%Y-%m-%d"))
  m <- prophet(testprophet, yearly.seasonality = TRUE)
  future <- make_future_dataframe(m, periods = 5)
  forecast <- predict(m, future)
  p<- plot_utama(forecast[(nrow(forecast)-60):nrow(forecast),],testprophet[(nrow(testprophet)-55):nrow(testprophet),])
  plotly_IMAGE(p, format = "png", out_file = "output.png")
  tanggal<-format(Sys.Date(), format="%d %b %Y")
  teks<-paste0("Update ", tanggal ," prediksi harga komoditas ",data_komoditas$nama[i-2]," DKI Jakarta Rp",round(forecast$yhat[nrow(forecast)],digits=0)," https://tinyurl.com/scraperharga #BigData #Forecasting #KomoditasJakarta #HargaPanganDKI #Harga")
  post_tweet(teks, media = "output.png")
  file.remove("output.png")
}

posting_twitter<-function(gambar_twitter){
  
}