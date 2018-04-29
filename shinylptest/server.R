
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(RMySQL)
library(prophet)
library(DT)
library(plotly)
library(data.table)
library(pastecs)
library(stringr)
library(rvest)
library(dplyr)
library(lubridate)
library(readr)
library(jsonlite)

#retrieve data dari database
options(mysql = list(
  "host" = "202.52.146.25",
  "port" = 3306,
  "user" = "isiindon",
  "password" = "pNo9rM05i1"
))
databaseName <- "isiindon_evote"
table <- "dataharga"
table_komoditas <- "komoditas"
loadData <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM `dataharga` ", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  
  #dbDisconnect(db)
  data
}

loadData_komoditas<-function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query_komoditas <- sprintf("SELECT * FROM `komoditas` ", table_komoditas)
  # Submit the fetch query and disconnect
  data_komoditas<-dbGetQuery(db, query_komoditas)
  
  #dbDisconnect(db)
  data_komoditas
}

get_komoditas <- function() {
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  query <- sprintf("SELECT * FROM `komoditas` ", table_komoditas)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  data
}
data<-data


#list variabel
listvar<-c("Beras IR. I (IR 64)"="BerasIR1",
           "Beras IR. II (IR 64) Ramos"="BerasIR2Ramos",
           "Beras IR. III (IR 64)"="BerasIR3",
           "Beras Muncul .I"="BerasMuncul",
           "Beras IR 42/Pera"="BerasIR42Pera",
           "Beras Setra I/Premium"="BerasSetraIPremium",
           "Cabe Merah Keriting"="CabeMerahKeriting",
           "Cabe Merah Besar (TW)"="CabeMerahBesar",
           "Cabe Rawit Merah"="CabeRawitMerah",
           "Cabe Rawit Hijau"="CabeRawitHijau",
           "Bawang Merah"="BawangMerah",
           "Minyak Goreng (Kuning/Curah)"="MinyakGorengCurah",
           "Bawang Putih"="BawangPutih",
           "Garam Dapur"="GaramDapur",
           "Ayam Broiler/Ras"="AyamBroiler",
           "Telur Ayam Ras"="TelurAyamRas",
           "Gula Pasir"="GulaPasir",
           "Tepung Terigu"="TepungTerigu",
           "Kentang (sedang)"="Kentangsedang",
           "Tomat Buah"="TomatBuah",
           "Kelapa Kupas"="KelapaKupas",
           "Semangka"="Semangka",
           "Jeruk Medan"="JerukMedan",
           "Ikan Mas"="IkanMas",
           "Daging Sapi Has (Paha Belakang)"="DagingSapiHasPahaBelakang",
           "Daging Sapi Murni (Semur)"="DagingSapiMurniSemur",
           "Daging Kambing"="DagingKambing",
           "Daging Babi Berlemak"="DagingBabiBerlemak",
           "Ikan Bandeng (sedang)"="IkanBandengsedang",
           "Susu Bubuk Dancow 400gr"="SusuBubukDancow400gr",
           "Susu Kental Bendera 200gr"="SusuKentalBendera200gr",
           "Susu Kental Enak 200gr"="SusuKentalEnak200gr",
           "Ikan Lele"="IkanLele",
           "Gas Elpiji 3kg"="GasElpiji3kg",
           "Susu Bubuk Bendera 400gr"="SusuBubukBendera400gr",
           "Margarin Blueband Sachet"="MargarinBluebandSachet",
           "Margarin Blueband Cup"="MargarinBluebandCup", "")


#close all connections
killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())

  for(con in all_cons)
    +  dbDisconnect(con)
}
killDbConnections()

#fungsi mengubah style css
#sumber : https://stackoverflow.com/questions/31425841/css-for-each-page-in-r-shiny
modifyStyle <- function(selector, ...) {
  values <- as.list(substitute(list(...)))[-1L]
  parameters <- names(values)
  args <- Map(function(p, v) paste0("'", p,"': '", v,"'"), parameters, values)
  jsc <- paste0("$('",selector,"').css({", paste(args, collapse = ", "),"});")
  shinyjs::runjs(code = jsc)
}


shinyServer(function(input, output, session) {
  #ambil data
  data_scraper <- reactiveValues(data = NULL)
    
  data<-loadData()
  data_komoditas<-loadData_komoditas()
  #matikan koneksi
  killDbConnections()
  
  data_komoditas<-rbind(data_komoditas,"")
  list_komoditas<-setNames(unlist(data_komoditas[2]),unlist(data_komoditas[1]))
  
  #untuk menaruh navbar jadi align kanan
  shinyjs::addClass(id = "action", class = "navbar-right")
  shinyjs::addClass(id = "menus", class = "navbar-right")
  
  #select input variabel
  output$dropdown_variabel <- renderUI({
    selectizeInput("country", "",list_komoditas , selected = "", options = list(placeholder = "Pilih Komoditas"))
  })
  output$dropdown_variabel2 <- renderUI({
    selectInput("komoditas", "Pilih Komoditas", list_komoditas)
  })
  
  #otomatis tutup aplikasi jika session berakhir
  session$onSessionEnded(stopApp)
  
  #menampilkan input yang ter hidden
  onclick("prophet_advanced", toggle(id = "advanced", anim = TRUE))
  #menampilkan detail tabel yang terhidden
  onclick("tabel_advanced", toggle(id = "plot_advanced", anim = TRUE))
  
  observeEvent(input$menus, {
    currentTab <- input$menus # Name of the current tab
    if (currentTab != "Beranda") {
      modifyStyle("body", background = "white")
    }else if (currentTab == "Beranda") {
      #tags$style("body {background: url(city.jpeg) no-repeat fixed; 
      #        background-size: auto 95%;
      #          background-color: rgb(245, 248, 250);}")
      modifyStyle("body", background = "url(asd.jpg) center bottom / auto 95% no-repeat fixed")
    }
    
  })
  
  observeEvent(input$country, {
    if (input$country != "") {
      showElement(selector = "#menus li a[data-value=Dashboard]", anim = TRUE)
      updateTabsetPanel(session, inputId = "menus", selected = "Dashboard")
      hideElement(selector = "#menus li a[data-value=Beranda]", anim = TRUE)
    }else {
      showElement(selector = "#menus li a[data-value=Beranda]", anim = TRUE)
      updateTabsetPanel(session, inputId = "menus", selected = "Beranda")
      hideElement(selector = "#menus li a[data-value=Dashboard]", anim = TRUE)
    }
  })
    
  observeEvent(input$country, {
    updateSelectInput(session, inputId =  "komoditas", selected = input$country)
  })
  
  renderplotly <-function(forecast,testprophet){
    renderPlotly({
    input$komoditas
    input$update_model
    Sys.sleep(2)
    #plot_ly(x = forecast$ds, y =forecast$yhat)
    x <- list(
      title = "Tanggal"
    )
    y <- list(
      title = "Harga (Rupiah"
    )
    plot_ly(x = ~forecast$ds, y = ~forecast$yhat,
            
            type = 'scatter',
            
            mode = 'lines',
            
            line = list(color='rgb(255,127,14)'),
            
            showlegend = TRUE,
            
            name = 'Prediksi') %>%
      add_trace(x = ~forecast$ds, y = ~forecast$yhat_lower,
                
                type = 'scatter',
                
                mode = 'lines',
                
                fill = 'tonexty',
                
                fillcolor='rgba(255,127,14,0.2)',
                
                line = list(color = 'transparent'),
                
                showlegend = FALSE,
                
                name = 'Batas Bawah') %>%
      add_trace(x = ~forecast$ds, y = ~forecast$yhat_upper,
                
                type = 'scatter',
                
                mode = 'lines',
                fill = 'tonexty',
                
                fillcolor='rgba(255,127,14,0.2)',
                
                line = list(color = 'transparent'),
                
                showlegend = FALSE,
                
                name = 'Batas Atas') %>%
      add_trace(x = ~testprophet$ds, y = ~testprophet$y,
                
                type = 'scatter',
                
                mode = 'lines',
                
                line = list(color='rgb(31,119,180)'),
                
                showlegend = TRUE,
                
                name = 'Nilai Asli') %>%
      layout(legend = list(x = 0.80, y = 0.90), xaxis = x, yaxis=y)
    
  })
  }
  
  renderstat_deskriptif<- function(index){
    renderPlotly({
      input$komoditas
      Harga<-data[,index]
      plot_ly(y = ~Harga, type = "box", name = input$komoditas)
    })
  }
  
  render_teks_deskriptif <- function(i){
    renderText({ 
    paste(sep = "\n",
    paste0("Nilai Tengah : ", stat.desc(data[,i], basic=F)[1]),
    paste0("Rata - Rata : ",round(stat.desc(data[,i], basic=F)[2]),digits=2),
    paste0("Varians : ",round(stat.desc(data[,i], basic=F)[5]),digits=2),
    paste0("Standar Deviasi : ", round(stat.desc(data[,i], basic=F)[6]),digits=2)
    )
    })
  }
  
  renderplot2<- function(forecast,m){
    renderPlot({
      input$update_model
      Sys.sleep(2)
      prophet_plot_components(m, forecast)
    })
  }
  rendertabelprophet<- function(forecast){
    DT::renderDataTable({
      input$update_model
      Sys.sleep(2)
      forecast<-setcolorder(forecast,c("ds","yhat",names(forecast)[2:(length(forecast)-1)]))
      DT::datatable(forecast, 
                    options = list(scrollX = TRUE, pageLength = 10, order = list(1, 'desc'))) %>% formatRound(columns=2:(length(forecast)-2),digits=4)
    })
  }
  
  rendertabelhasil<- function(forecast,periode){
    DT::renderDataTable({
      input$update_model
      Sys.sleep(2)
      forecast<-forecast[c("ds","yhat","yhat_upper","yhat_lower")]
      forecast$ds<- as.Date(forecast$ds,"%d-%b-%Y")
      DT::datatable(forecast[(nrow(forecast)-periode):nrow(forecast),], 
                    options = list(scrollX = TRUE, pageLength = 10)) %>% formatRound(columns=2:4,digits=4)
    })
  }
  
  observeEvent(input$komoditas, {
    print(input$komoditas)
    terpilih <- as.numeric(input$komoditas)
    #prophet function
    index<-0
    for(i in 3:ncol(data)){
      if(input$komoditas == names(data[i])){
        testprophet<-cbind(data[,1],as.numeric((data[,i])))
        index<-i
      }
    }
    
    colnames(testprophet) <- c("ds","y")
    testprophet <- data.frame(testprophet)
    testprophet$y <- as.numeric(as.character(testprophet$y))
    testprophet$ds<-(as.Date(testprophet$ds, format = "%Y-%m-%d"))
    m <- prophet(testprophet, yearly.seasonality = TRUE)
    future <- make_future_dataframe(m, periods = 10)
    forecast <- predict(m, future)
    updateSliderInput(session,inputId = "tanggal",
                      value=c(as.Date(data$ds[1]),as.Date(data$ds[length(data$ds)])))
    updateSliderInput(session,inputId = "periode",value = 10)
    output$ts_plot <- renderplotly(forecast= forecast,testprophet)
    output$ts_plot2 <-renderplot2(forecast,m)
    output$tabelprophet <- rendertabelprophet(forecast)
    output$tabel_hasil <- rendertabelhasil(forecast,10)
    output$stat_deskriptif <- renderstat_deskriptif(index)
    output$teks_stat_deskriptive <- render_teks_deskriptif(index)
  })
  
  observeEvent(input$update_model,{
    print(input$update_model)
    for(i in 3:ncol(data)){
      if(input$komoditas == names(data[i])){
        testprophet<-cbind(data[,1],as.numeric((data[,i])))
      }
    }
    colnames(testprophet) <- c("ds","y")
    testprophet <- data.frame(testprophet)
    testprophet$ds<-(as.Date(testprophet$ds, format = "%Y-%m-%d"))
    tanggal1<-0
    tanggal2<-0
    for(i in 1:length(testprophet$ds)){
      if(testprophet$ds[i]==input$tanggal[1]){
        tanggal1<-i
      } else if(testprophet$ds[i]==input$tanggal[2]){
        tanggal2<-i
      }
    }
    print(paste0(tanggal1,tanggal2))
    testprophet$y <- as.numeric(as.character(testprophet$y))
    testprophet<-testprophet[tanggal1:tanggal2,]
    m <- prophet(testprophet,
                  yearly.seasonality = input$yearly,
                  weekly.seasonality = input$monthly,
                  seasonality.prior.scale = input$seasonality_scale,
                  changepoint.prior.scale = input$changepoint_scale,
                  mcmc.samples = input$mcmc.samples,
                  interval.width = input$interval.width,
                  uncertainty.samples = input$uncertainty.samples,
                  fit = TRUE)
    future <- make_future_dataframe(m, periods = input$periode)
    forecast <- predict(m, future)
    print(tail(forecast$yhat))
    output$ts_plot <- renderplotly(forecast= forecast,testprophet)
    output$ts_plot2 <-renderplot2(forecast,m)
    output$tabelprophet <- rendertabelprophet(forecast)
    output$tabel_hasil <- rendertabelhasil(forecast,input$periode)
    print("Done")
  })
    
  output$tabel1 <- DT::renderDataTable({
    DT::datatable(data[, -2], 
                  colnames = c("Tanggal",data_komoditas$nama), 
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-',Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    })
  
  rendertabelscraper<- function(data_scraper){
    DT::renderDataTable({
      input$start_scraper
      Sys.sleep(2)
      DT::datatable(data_scraper, 
                    options = list(scrollX = TRUE, pageLength = 10))
    })
  }
  
  observeEvent(input$start_scraper,{
    showElement(selector = "#hasil_scraper", anim = TRUE)
    if(input$Website == 1){
      link<-str_c("http://www.klikindomaret.com/search/?key=",input$kata_kunci,sep = "")
      webpage <- read_html(link)
      
      # Extract records info
      results <- webpage %>% html_nodes(".list-unstyled")
      
      #untuk klikindomaret
      results<-results[6] %>% html_nodes(".comparable")
      # Building the dataset
      records <- vector("list", length = length(results))
      for (i in seq_along(results)) {
        nama_produk <- str_c(results[i] %>% html_nodes("h5") %>% html_text(trim = TRUE))
        harga <- results[i] %>% html_nodes(".price > b") %>% html_text(trim = TRUE)
        harga<-gsub("Rp.", "",harga)
        link<-str_c("http://www.klikindomaret.com",results[i] %>% html_attr("href"))
        records[[i]] <- data_frame(nama = nama_produk,harga = harga,link=link)
      }
      df <- bind_rows(records)
      data_scraper$data<-df
    }else {
      link<-str_c("https://shop.hypermart.co.id/hypermart/product-list.php?q=",input$kata_kunci,"&sz=100",sep = "")
      webpage <- read_html(link)
      
      # Extract records info
      results <- webpage %>% html_nodes(".row-three > .col")
      # Building the dataset
      records <- vector("list", length = length(results))
      for (i in seq_along(results)) {
        nama_produk <- str_c(results[i] %>% html_nodes("h5") %>% html_text(trim = TRUE))
        harga <- results[i] %>% html_nodes("a > p") %>% html_text(trim = TRUE)
        harga<-gsub("Rp.*?Rp.", "",harga)
        harga<-gsub("Rp.", "",harga)
        link<-str_c("https://shop.hypermart.co.id",(results[i] %>% html_nodes("a") %>% html_attr("href"))[1])
        records[[i]] <- data_frame(nama = nama_produk,harga = harga,link = link)
      }
      df <- bind_rows(records)
      data_scraper$data<-df
    }
    output$tabel_scraper <- rendertabelscraper(data_scraper$data)
  })
  
  observeEvent(input$add_database,{
    
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    alias<-gsub(" ","",data_scraper$data$nama)
    alias<-gsub("-","",alias)
    if(id_situs == 2){
      nama_situs = 'KlikIndomaret'
    } else if (id_situs == 3){
      nama_situs = 'Hypermart'
    }
    alias<-paste(nama_situs,alias,sep = "_")
    id_situs<-as.integer(input$Website) + 1
    for(i in 1:length(alias)){
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table_komoditas, 
      paste("nama","alias","id_situs","link",sep = ", "),
      paste(data_scraper$data$nama[i],alias[i],id_situs,data_scraper$data$link[i], sep = "', '")
    )
    dbGetQuery(db, query)
    query2 <- sprintf(
      "ALTER TABLE %s ADD %s mediumint(9);",
      table,
      str_c(alias[i])
    )
    dbGetQuery(db, query2)
    query3 <- sprintf(
      "INSERT INTO %s (ds, %s) VALUES ('%s', %s)
      ON DUPLICATE KEY UPDATE %s = %s;",
      table,
      alias[i],
      format(Sys.Date(), format="%Y-%m-%d"),
      as.numeric(data_scraper$data$harga[i])*1000,
      alias[i],
      as.numeric(data_scraper$data$harga[i])*1000
    )
    print(query3)
    dbGetQuery(db, query3)
    }
    dbDisconnect(db)
    toggleModal(session, "sukses_simpan", toggle = "open")
    hideElement(selector = "#hasil_scraper", anim = TRUE)
    data_scraper$data<-NULL
  })
  
    print(format(Sys.time(), "%H:%M:%S"))
    if((format(Sys.time(), "%H:%M:%S") > "05:30:00") && (format(Sys.time(), "%H:%M:%S") < "06:30:00")){
    data_komoditas<-get_komoditas()
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
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
        #print(query3)
        dbGetQuery(db, query3)
      }
    }
    
    dbDisconnect(db)
    }
})
