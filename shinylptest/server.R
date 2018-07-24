
library(shiny)
library(shinyjs)
library(DBI)
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
library(rtweet)
library(digest)
library(randomcoloR)
#library(webshot)

#retrieve data dari database
options(mysql = list(
  "host" = "sql12.freesqldatabase.com",
  "port" = 3306,
  "user" = "sql12248019",
  "password" = "zRJVNFYemy"
))
databaseName <- "sql12248019"
table <- "dataharga"
table_komoditas <- "komoditas"
loadData <- function() {
  # Connect to the database
  library(DBI)
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
  query_komoditas <- sprintf("SELECT * FROM `komoditas` ORDER BY auto_inc ASC", table_komoditas)
  # Submit the fetch query and disconnect
  data_komoditas<-dbGetQuery(db, query_komoditas)
  
  #dbDisconnect(db)
  data_komoditas
}

loadData_user <- function() {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM `user` ", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  
  #dbDisconnect(db)
  data
}
#data<-data


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
  shinyjs::addClass(id = "menus", class = "navbar-right")
  shinyjs::addClass(id = "login_trigger", class = "navbar-right")
  shinyjs::addClass(id = "logout_trigger", class = "navbar-right")
  hideElement("logout_trigger")
  showElement("group_komoditas")
  
  
  #select input variabel
  output$dropdown_variabel <- renderUI({
    selectizeInput("country", "",list_komoditas , selected = "", options = list(placeholder = "Pilih Komoditas"))
  })
  
  output$tanggal_dashboard <- renderUI({
    sliderInput("tanggal",label = "Waktu",
                min = as.Date(data$ds[1],"%Y-%m-%d"),
                max = as.Date(data$ds[length(data$ds)],"%Y-%m-%d"),
                value=c(as.Date(data$ds[1]),as.Date(data$ds[length(data$ds)])),
                timeFormat="%Y-%m-%d")
  })
  
  output$tanggal_data <- renderUI({
    sliderInput("tanggal",label = "Waktu",
                min = as.Date(data$ds[1],"%Y-%m-%d"),
                max = as.Date(data$ds[length(data$ds)],"%Y-%m-%d"),
                value=c(as.Date(data$ds[1]),as.Date(data$ds[length(data$ds)])),
                timeFormat="%Y-%m-%d")
  })
  
  output$dropdown_variabel_data <- renderUI({
    select2Input("data_komoditas","Pilih Komoditas",list_komoditas,selected="")
  })
  
  observeEvent(input$clearAllTop,{
    updateSelect2Input(session,"data_komoditas","Pilih Komoditas",list_komoditas,selected="")
  })
  
  output$dropdown_variabel2 <- renderUI({
    selectInput("komoditas", "Pilih Komoditas", list_komoditas)
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['komoditas']])) {
      updateSelectizeInput(session, "country", value = query[['komoditas']])
    }  
  })
  
  #otomatis tutup aplikasi jika session berakhir
  session$onSessionEnded(stopApp)
  
  #menampilkan input yang ter hidden
  onclick("prophet_advanced", toggle(id = "advanced", anim = TRUE))
  #menampilkan detail tabel yang terhidden
  onclick("tabel_advanced", toggle(id = "plot_advanced", anim = TRUE))
  onclick("pilih_tambahan",toggle(id = "pilihan_tambahan", anim = TRUE))
  #menampilkan tampilan variabel yang terhidden
  onclick("data_all_komoditas", toggle(id = list("all_komoditas","group_komoditas"), anim = TRUE))
  
  #pengaturan token twitter
  appname <- "scrapper_test"
  
  ## api key (example below is not a real key)
  key <- "pQj1KTVRpn6445W1sPLuX21em"
  
  ## api secret (example below is not a real key)
  secret <- "UNORlS4exR1bTWr8C6BHCSXMvVHytobxfvHewFD5by7EUSmtRc"
  
  # create token named "twitter_token"
  # twitter_token <- create_token(
  #  app = appname,
  #  consumer_key = key,
  #  consumer_secret = secret)
  
  #inisiasi plotly
  Sys.setenv("plotly_username" = "Irsad.arief")
  Sys.setenv("plotly_api_key" = "VFBsbrrTjuwFY7EqFoSr")
  
  #inisiasi data tabel
  data_tabel <-reactiveValues(data=NULL)
  data_tabel$data<- NULL
  data_tabel$tabel_data<-NULL
  data_tabel$tabel_data<- c(1:37)
  data_tabel$data<-cbind(data[,1],data[,3:39])
  
  #inisiasi nilai awal user
  user<-reactiveValues(Logged = FALSE)
  #load data user
  data_user<-loadData_user()
  #print(data_user)
  for (i in 1 : nrow(data_user)){
    if(data_user$level[i] == 0){
      data_user$status[i] <- "Admin"
    } else if(data_user$level[i] == 1){
      data_user$status[i] <- "Pengguna Level 1"
    } else {
      data_user$status[i] <- "Pengguna Level 2"
    }
  }
  #buat variabel username + password
  list_username<-setNames(data_user$password,data_user$username)
  
  #hide beberapa tab sebelum login 
  hideElement(selector = "#menus li a[data-value=Scraper]", anim = FALSE)
  hideElement(selector = "#menus li a[data-value=Admin]", anim = FALSE)
  
  #JANGAN LUPA DI UNCOMMENT
  #hideElement(selector = "#menus li a[data-value=Data]", anim = TRUE)
  
  observeEvent(input$login,{
    if(input$username !="" && input$password !=""){
      hash_password<-digest(input$password,"md5",serialize = FALSE)
      username<-input$username
      if (isTRUE(list_username[[input$username]]==hash_password)){
        user$Logged <- TRUE
        print("sukses login")
        toggleModal(session, modalId = "login_modal",toggle = "close")
        hideElement(selector =  "#login_trigger", anim = TRUE)
        showElement(selector = "#logout_trigger", anim = TRUE)
        showElement(selector = "#menus li a[data-value=Data]", anim = TRUE)
        print(nrow(data_user))
        for(i in 1:nrow(data_user)){
          print(data_user$username[i])
          if(username == data_user$username[i]){
            if(data_user$level[i] == 0){
              updateTextInput(session,"username",value = "",placeholder = "")
              updateTextInput(session,"password",value = "",placeholder = "")
              showElement(selector = "#menus li a[data-value=Admin]", anim = TRUE)
            }else if(data_user$level[i]!=1){
              showElement(selector = "#menus li a[data-value=Scraper]", anim = TRUE)
            }
            break()
          }
        }
      } else {
        #print(hash_password)
        toggleModal(session, "gagal_login", toggle = "open")
      }
    }else{
      toggleModal(session, "gagal_login_kosong", toggle = "open")
    }
  })
  
  observeEvent(input$signup_trigger,{
    toggleModal(session, modalId = "login_modal",toggle = "close")
  })
  
  observeEvent(input$signup,{
    if(input$username_daftar !="" && input$email !="" && input$password_daftar !="" && 
      input$first_name !="" && input$last_name !=""){
      username<- input$username_daftar
      email<-input$email
      passwords<-input$password_daftar
      passwords_def<-digest(passwords,"md5",serialize = FALSE)
      first_name<-input$first_name
      last_name<-input$last_name
      db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
      query <- sprintf(
        "INSERT INTO user (username, email,password,first_name,last_name) VALUES ('%s');",
        paste(username,email,passwords_def,first_name,last_name, sep = "', '")
      )
      print(query)
      dbGetQuery(db, query)
      user$Logged <- TRUE
      print("sukses sign up")
      toggleModal(session, modalId = "signup_modal",toggle = "close")
      toggleModal(session, "sukses_daftar", toggle = "open")
      hideElement(selector =  "#login_trigger", anim = TRUE)
      showElement(selector = "#logout_trigger", anim = TRUE)
      #showElement(selector = "#menus li a[data-value=Scraper]", anim = TRUE)
      showElement(selector = "#menus li a[data-value=Admin]", anim = TRUE)
      showElement(selector = "#menus li a[data-value=Data]", anim = TRUE)
    }else{
      toggleModal(session, modalId = "gagal_signup",toggle = "open")
    }
  })
  
  observeEvent(input$logout_trigger,{
    user$Logged <- FALSE
    print("sukses logout")
    showElement(selector =  "#login_trigger", anim = TRUE)
    hideElement(selector = "#logout_trigger", anim = TRUE)
    hideElement(selector = "#menus li a[data-value=Scraper]", anim = TRUE)
    hideElement(selector = "#menus li a[data-value=Data]", anim = TRUE)
    hideElement(selector = "#menus li a[data-value=Admin]", anim = TRUE)
  })
  
  observeEvent(input$pilih_data_cek,{
    #tabeldata<- switch(input$kelompok_komoditas,
    #       beras = c(1:6),
    #       cabe = c(7:10)
    #       )
    #data_tabel$data<- NULL
    #data_tabel$tabel_data<-tabeldata
    tabel_data<-NULL
    for (i in 1: length(input$kelompok_komoditas)){
      if(input$kelompok_komoditas[i] == "beras"){
        tabel_data<-cbind(tabel_data,c(1:6))
      }
      if(input$kelompok_komoditas[i] == "cabe"){
        tabel_data<-cbind(tabel_data,c(7:10))
      }
      if(input$kelompok_komoditas[i] == "daging"){
        tabel_data<-cbind(tabel_data,c(25:28,15:16))
      }
      if(input$kelompok_komoditas[i] == "sayur"){
        tabel_data<-cbind(tabel_data,c(19:21))
      }
      if(input$kelompok_komoditas[i] == "buah"){
        tabel_data<-cbind(tabel_data,c(22:23))
      }
      if(input$kelompok_komoditas[i] == "bawang"){
        tabel_data<-cbind(tabel_data,c(11,13))
      }
    }
    updateSelect2Input(session,"data_komoditas","Pilih Komoditas",list_komoditas,selected=colnames(data[,tabel_data+2]))
    data_tabel$data<-data[,tabel_data+2]
    data_tabel$tabel_data<-tabel_data
    #print(input$data_komoditas)
    #data_tabel$data<-cbind(data[,1],data[,tabeldata + 2])
  })
  observeEvent(input$update_pilih_data,{
    data_tabel$data<- data[,input$data_komoditas[1]]
    data_tabel$tabel_data<-which(data_komoditas$alias == input$data_komoditas[1])
    if(length(input$data_komoditas) > 1){
      for(i in 2:length(input$data_komoditas)){
        data_tabel$data<-cbind(data_tabel$data,data[,input$data_komoditas[i]])
        data_tabel$tabel_data<-cbind(data_tabel$tabel_data,which(data_komoditas$alias == input$data_komoditas[i]))
      }
    }
    print(data_tabel$tabel_data)
  })
  
  
  output$tabel_data <- DT::renderDataTable({
    #DT::datatable(data[, input$data_options, drop = FALSE])
    DT::datatable(data_tabel$data,
                  colnames = c("Tanggal",data_komoditas$nama[data_tabel$tabel_data]),
                  options = list(scrollX = TRUE))
  })
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      #inputs[i] <- as.character(FUN(paste0(id, i), label = data_user$username[i] , ...))
      inputs[i] <- as.character(FUN(paste0(id,i), label = "Ubah Status" , ...))
    }
    inputs
  }
  shinyInputSelectize <- function(FUN, len, id, terpilih , ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      #inputs[i] <- as.character(FUN(paste0(id, i), label = data_user$username[i] , ...))
      inputs[i] <- as.character(FUN(paste0(id, i), "",c("Admin" = 0,"Pengguna Level 1" = 1, "Pengguna Level 2" = 2), selected = terpilih[i] , ...))
    }
    inputs
  }
  
  data_user_admin <- reactiveValues(data = data.frame(
    username = data_user$username,
    first_name = data_user$first_name,
    last_name = data_user$last_name,
    email = data_user$email,
    #status = data_user$status
    status = shinyInputSelectize(selectizeInput,nrow(data_user), 'status_',terpilih = data_user$level),
    aksi = shinyInput(actionButton, nrow(data_user), 'button_', class = "btn-info", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
    #aksi = shinyInput(actionButton, nrow(data_user), 'button_', class = "btn-info"),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$select_button, {
    s <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    status_terpilih<-data_user_admin$data$status[s]
    status_terpilih<-strtoi(substr(a,regexpr('\" selected', a) [1]-1,regexpr('\" selected', a) [1]-1))
    print(s)
    data_user$level[s]<-status_terpilih
    print(data_user$level[s])
    data_user_admin$data$status = shinyInputSelectize(selectizeInput,nrow(data_user), 'status_',terpilih = data_user$level)
    output$tabel_admin<-render_tabel_admin()
    print(data_user$level)
    toggleModal(session, "sukses_ubah_status", toggle = "open")
  })
  
  output$tabel_admin <- DT::renderDataTable({
    DT::datatable(data_user_admin$data, escape = FALSE, selection = 'none',colnames = c("Username","Nama Depan","Nama Belakang","Email","status","Aksi"))
  })
  
  render_tabel_admin<-function(){
    DT::renderDataTable({
      DT::datatable(data_user_admin$data, escape = FALSE, selection = 'none',colnames = c("Username","Nama Depan","Nama Belakang","Email","status","Aksi"))
    })
  }
  
  
  observe({
    print(format(Sys.time(), "%H:%M:%S"))
    if((format(Sys.time(), "%H:%M:%S") > "14:30:00") && (format(Sys.time(), "%H:%M:%S") < "16:30:00")){
      data_komoditas<-loadData_komoditas() 
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
          dbGetQuery(db, query3)
        } else if (data_komoditas$id_situs[i] == 4){
          mydata <- fromJSON(data_komoditas$link[i])
          harga<-mydata$products$price[1]
          query3 <- sprintf(
            "INSERT INTO %s (ds, %s) VALUES ('%s', %s) ON DUPLICATE KEY UPDATE %s = %s;",
            table,
            data_komoditas$alias[i],
            format(Sys.Date(), format="%Y-%m-%d"),
            harga,
            data_komoditas$alias[i],
            harga
          )
          dbGetQuery(db, query3)
        }
      }
      
      dbDisconnect(db)
    }  
    print("Done")
  })
  
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
    updateSelectInput(session, inputId = "dashboard_website", selected = 1)
  })
  
  output$data_plot <-
    renderPlotly({
      #input$
      Sys.sleep(2)
      x <- list(
        title = "Tanggal"
      )
      y <- list(
        title = "Harga (Rupiah)"
      )
      
      #p<-plot_ly(x = ~data$ds, y = ~data[,3],          
      #        type = 'scatter',
      #        mode = 'lines',
      #        line = list(color=randomColor()),
      #        showlegend = TRUE,
      #        name = 'Beras IR 1')
      p<-plot_ly()
      tanggal_data<-(as.Date(data$ds, format = "%Y-%m-%d"))
      options(warn = -1) 
      for(i in 1:length(data_tabel$tabel_data)){
         index<-data_tabel$tabel_data[i]
         p<- add_trace(p, x = tanggal_data, y = data[,index+2],
                     type = 'scatter',
                     mode = 'lines',
                     line = list(color=randomColor()),
                     showlegend = TRUE,
                     name = data_komoditas$nama[index],
                     evaluate = TRUE)
      }
      p %>%
        layout(xaxis = x, yaxis=y)
    })
  
  renderplotly <-function(forecast,testprophet){
    renderPlotly({
    input$komoditas
    input$update_model
    Sys.sleep(2)
    p<- plot_utama(forecast,testprophet)
    p
  })
  } 
  
  plot_utama<- function(forecast,testprophet){
    x <- list(
      title = "Tanggal"
    )
    y <- list(
      title = "Harga (Rupiah)"
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
                    colnames = c("Tanggal", "Prediksi Harga", "Batas Atas", "Batas Bawah"),
                    options = list(scrollX = TRUE, pageLength = 10)) %>% formatRound(columns=2:4,digits=4)
    })
  }
  
  observeEvent(input$dashboard_website,{
    
  })
  
  observeEvent(input$komoditas, {
    print(input$komoditas)
    #browser()
    terpilih <- as.numeric(input$komoditas)
    #prophet function
    index<-0
    for(i in 3:ncol(data)){
      if(input$komoditas == names(data[i])){
        updateSelectInput(session, inputId = "dashboard_website", selected = data_komoditas$id_situs[i-2])
        testprophet<-cbind(data[,1],as.numeric((data[,i])))
        index<-i
        break()
      }
    }
    colnames(testprophet) <- c("ds","y")
    testprophet <- data.frame(testprophet)
    testprophet$y <- as.numeric(as.character(testprophet$y))
    testprophet$ds<-(as.Date(testprophet$ds, format = "%Y-%m-%d"))
    m <- prophet(testprophet, yearly.seasonality = TRUE,daily.seasonality = FALSE)
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
    print("Peramalan sukses")
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
  })
    
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-',Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_tabel$data, file, row.names = FALSE,col.names = c("Tanggal",data_komoditas$nama[data_tabel$tabel_data]))
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
    kata_kunci<-gsub(" ", "%20",input$kata_kunci)
    if(input$Website == 1){
      link<-str_c("http://www.klikindomaret.com/search/?key=",kata_kunci,sep = "")
      webpage <- read_html(link)
      results <- webpage %>% html_nodes(".clearfix > .item")
      records <- vector("list", length = length(results))
      for (i in seq_along(results)) {
        nama_produk <- str_c(results[i] %>% html_nodes(".title") %>% html_text(trim = TRUE))
        harga <- results[i] %>% html_nodes(".price-value") %>% html_text(trim = TRUE)
        harga<-gsub("Rp ", "",harga)
        link<-str_c("http://www.klikindomaret.com",results[i] 
                    %>% html_nodes("a") 
                    %>% html_attr("href"))
        records[[i]] <- data_frame(nama = nama_produk,harga = harga,link=link)
      }
      df <- bind_rows(records)
      data_scraper$data<-df
      
    }else if(input$Website == 2) {
      link<-str_c("https://shop.hypermart.co.id/hypermart/product-list.php?q=",
                  kata_kunci,"&sz=100",sep = "")
      webpage <- read_html(link)
      results <- webpage %>% html_nodes(".row-three > .col")
      records <- vector("list", length = length(results))
      for (i in seq_along(results)) {
        nama_produk <- str_c(results[i] %>% html_nodes("h5") %>% html_text(trim = TRUE))
        harga <- results[i] %>% html_nodes("a > p") %>% html_text(trim = TRUE)
        harga<-gsub("Rp.*?Rp.", "",harga)
        harga<-gsub("Rp.", "",harga)
        link<-str_c("https://shop.hypermart.co.id",(results[i] 
                                                    %>% html_nodes("a") 
                                                    %>% html_attr("href"))[1])
        records[[i]] <- data_frame(nama = nama_produk,harga = harga,link = link)
      }
     df <- bind_rows(records)
      data_scraper$data<-df
      
    } else {
      mydata <- fromJSON(str_c("https://api.happyfresh.com/api/stock_locations/401/products/search?q=",
                               kata_kunci, "&corrections=true&popular=true&per_page=240"))
      records <- vector("list", length = mydata$count)
      print(mydata$count)
      for(i in 1:mydata$count){
        nama_produk <- str_c(mydata$products$name[i],"/",mydata$products$display_unit)
        harga <- mydata$products$price[i]
        mydata$products$name<-gsub(" ","%20",mydata$products$name)
        link<-str_c("https://api.happyfresh.com/api/stock_locations/401/products/search?q=",
                    mydata$products$name[i],"&corrections=true")
        records[[i]] <- data_frame(nama = nama_produk,harga = harga,link=link)
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
    id_situs<-as.integer(input$Website) + 1
    if(id_situs == 2){
      nama_situs = 'KlikIndomaret'
    } else if (id_situs == 3){
      nama_situs = 'Hypermart'
    } else if(id_situs==4){
      nama_situs = 'Happyfresh'
    }
    alias<-paste(nama_situs,alias,sep = "_")
    for(i in 1:length(alias)){
    query <- sprintf(
      "INSERT IGNORE INTO %s (%s) VALUES ('%s')",
      table_komoditas, 
      paste("nama","alias","id_situs","link",sep = ", "),
      paste(data_scraper$data$nama[i],alias[i],id_situs,data_scraper$data$link[i], sep = "', '")
    )
    print(query)
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
  
})
