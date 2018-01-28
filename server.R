
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

#retrieve data dari database
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))
databaseName <- "hargapangan"
table <- "dataharga"
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
  #data
}
data<-loadData()

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
  
  #untuk menaruh navbar jadi align kanan
  shinyjs::addClass(id = "action", class = "navbar-right")
  shinyjs::addClass(id = "menus", class = "navbar-right")
  
  #otomatis tutup aplikasi jika session berakhir
  session$onSessionEnded(stopApp)
  
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
  
  observeEvent(input$komoditas, {
    komoditas_terpilih<-paste(input$komoditas,"")
    #prophet function
    testprophet<-cbind(data[,1],as.numeric((data[,input$komoditas])))
    colnames(testprophet) <- c("ds","y")
    testprophet <- data.frame(testprophet)
    testprophet$y <- as.numeric(as.character(testprophet$y))
    testprophet$ds<-(as.Date(testprophet$ds, format = "%Y-%m-%d"))
    
    m <- prophet(testprophet, yearly.seasonality = TRUE, daily.seasonality = TRUE)
    future <- make_future_dataframe(m, periods = 10)
    forecast <- predict(m, future)
  })
  
  output$ts_plot <- renderPlotly({
    plot_ly(x = forecast$ds, y =forecast$yhat)
  })
  
  output$ts_plot2 <- renderPlot({
    prophet_plot_components(m, forecast)
  })
  
  output$tabelprophet <- DT::renderDataTable({
    DT::datatable(forecast, 
                  options = list(scrollX = TRUE, pageLength = 10, order = list(1, 'desc'))) %>% formatRound(columns=2:17,digits=4)
  })
  
  output$tabel1 <- DT::renderDataTable({
    DT::datatable(data[, c(1,3:39), drop = FALSE], 
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
  
  
})
