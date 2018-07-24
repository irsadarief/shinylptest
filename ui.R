library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyLP)
library(shinyjs)
library(shinysky)
library(plotly)
library(shinycssloaders)


#fungsi sehingga bisa menaruh inputs pada navbar
navbarPageWithInputs <- function(..., inputs = NULL, inputs2 = NULL) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs, inputs2)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(form,
    navbar[[3]][[1]]$children[[1]])
  navbar
}

shinyUI(
  
  # masukkan fluid page agar bisa menaruh icon gambar logo pada navbar
  fluidPage(
    #include shinyjs
    useShinyjs(), 
    
    #include css 
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    
    #untuk memilih tema shinytheme
    #shinythemes::themeSelector(),
    
    list(tags$head(HTML('<link rel="icon", href="logo.png",
                        type="image/png" />'))),
    div(style="padding: 0px 0px; width: '10%'",
        titlePanel(
          title="", windowTitle="Forecasting Harga Pangan Jakarta"
        )
    ),
    
    navbarPageWithInputs(title=div(icon("line-chart"),tags$b("Forecasting"),"Harga Pangan Jakarta"),
               id = "menus",
               
               inverse = F, # for diff color view
               #pilih tema shinytheme
               inputs = actionButton("login_trigger","Login", class = "btn-info",style = "margin-left:20px;"),
               inputs2 = actionButton("logout_trigger","Logout", class = "btn-warning",style = "margin-left:20px;"),
               theme = shinytheme("simplex"),
               tabPanel("Beranda", icon = icon("home"), value = "Beranda",
                        column(12,align="center",
                               
                               div( id="selects", 
                               h1("WELCOME"),
                               h4("Indonesia First Price Scraping Dashboard"),
                               tags$style(type='text/css', "#selects .shiny-input-container:not(.shiny-input-container-inline){width:100%} 
                                          #selects .selectize-input {width:100%; max-width:500px; height:40px; font-size:125%;} 
                                          #selects .selectize-dropdown {text-align :left; font-size:125%;}"),
                               uiOutput("dropdown_variabel")
                               , style = "padding-top:10%;"
                               )
                        )
                        ),
               tabPanel("Dashboard", icon = icon("area-chart"), value = "Dashboard",
                        fluidRow(
                                 sidebarPanel(width = 3,
                                 h4(tags$b("Pengaturan Parameter")),
                                 ### parameter: yearly.seasonality
                                 checkboxInput("yearly","Yearly Seasonality", value = TRUE),
                                 
                                 ### parameter: weekly.seasonality 
                                 checkboxInput("monthly","Weekly Seasonality", value = TRUE),
                                 
                                 uiOutput("tanggal_dashboard"),
                                 sliderInput("periode",label = "Prediksi hari", min=1,max=100,value = 10),
                                 a(id = "prophet_advanced", "Tampilkan / Sembunyikan Info Rinci"),
                                 hidden(
                                 div(id = "advanced",
                                 ### parameter: seasonality.prior.scale
                                 numericInput("seasonality_scale","Seasonality Prior Scale", value = 10),
                                 
                                 ### parameter: changepoint.prior.scale
                                 numericInput("changepoint_scale","Changepoint Prior Scale", value = 0.05, step = 0.01),
                                 
                                 ### parameter: mcmc.samples
                                 numericInput("mcmc.samples", "MCMC Samples", value = 0),
                                 
                                 ### parameter: interval.width
                                 numericInput("interval.width", "Interval Width", value= 0.8, step = 0.1),
                                 ### parameter: uncertainty.samples
                                 numericInput("uncertainty.samples","Uncertainty Samples", value = 1000)
                                 )),
                                 h6(""),
                                 actionButton("update_model", "Update Prophet Model",class = "btn-primary")
                          ),
                          
                          column(9,
                                 fluidRow(column(4,selectizeInput("dashboard_website", "Pilih Website",c("Klikindomaret" = 2, "Hypermart" = 3, "Happyfresh" = 4, "PIHPS Jakarta" = 1) , selected = "", options = list(placeholder = "Pilih Website")))
                                          ,column(8,uiOutput("dropdown_variabel2")))
                                 ,tabsetPanel(
                                   tabPanel("Plot", h4(""),withSpinner(plotlyOutput("ts_plot")),
                                            h6(""),
                                            a(id = "tabel_advanced", "Tampilkan / Sembunyikan Info Rinci"),
                                            h6(""),
                                            hidden(
                                              div(id = "plot_advanced",
                                                  withSpinner(DT::dataTableOutput("tabel_hasil"))
                                            )
                                            )
                                            ),
                                   tabPanel("Plot Komponen", h1(""), withSpinner(plotOutput("ts_plot2"))),
                                   tabPanel("Tabel", h1(""), withSpinner(DT::dataTableOutput("tabelprophet"))),
                                   tabPanel("Statistik Deskriptif", h1(""),
                                            column(3, verbatimTextOutput("teks_stat_deskriptive")),
                                            column(9, withSpinner(plotlyOutput("stat_deskriptif")))
                                            )
                                 )
                                 )
                          )
                        ),
               tabPanel("Scraper", icon = icon("search"), value = "Scraper",
                        fluidRow(
                          sidebarPanel(width = 12,
                                       h4(tags$b("Pengaturan")),
                                       selectizeInput("Website", "Pilih Website",c("Klikindomaret" = 1, "Hypermart" = 2, "Happyfresh" = 3) , selected = "", options = list(placeholder = "Pilih Website")),
                                       textInput("kata_kunci","Kata Kunci", placeholder = "Masukkan Kata Kunci Disini"),
                                       actionButton("start_scraper", "Mulai Scraping",class = "btn-primary")
                          ),
                          column(12,
                                 hidden(
                                 div(id = "hasil_scraper",
                                     h3("Hasil Scraping", align = "center"),
                                     h6(""),
                                     withSpinner(DT::dataTableOutput("tabel_scraper")),actionButton("add_database", "Masukkan Ke Database",class = "btn-primary"))
                                 ),
                                 bsModal(id = 'sukses_simpan', title = 'Sukses Simpan', trigger = '',
                                         size = 'large', p("Data Telah Sukses Disimpan ke Database"))
                          )
                        )
                        ),
               tabPanel("Data", icon = icon("database"),
                        fluidRow(
                           sidebarPanel(width = 3,
                                        uiOutput("tanggal_data"),
                                        uiOutput("dropdown_variabel_data"),
                                        br(),
                                        h4(),
                                        actionButton("update_pilih_data", "Perbarui Pilihan",class = "btn-primary"),
                                        actionButton(inputId = "clearAllTop", 
                                                     label = "Clear selection",
                                                     class = "btn-info",
                                                     icon = icon("square-o")),
                                        #actionButton(inputId = "selectAllTop",
                                        #             label = "Select all",
                                        #             class = "btn-primary",
                                        #             icon = icon("check-square-o")),
                                        h4(),
                                        br(),
                                        a(id = "pilih_tambahan", "Atau Pilih Berdasarkan Kelompok Komoditas"),
                                        hidden(
                                          div(id = "pilihan_tambahan",
                                            checkboxGroupInput("kelompok_komoditas","Kelompok Komoditas",
                                                        choices = c("Semua Komoditas" = "all",
                                                                    "Beras" = "beras",
                                                                    "Cabe" = "cabe",
                                                                    "Bumbu Dapur" = "bumbu",
                                                                    "Bawang" = "bawang",
                                                                    "Daging dan Telur" = "daging",
                                                                    "Ikan" = "ikan",
                                                                    "Buah" = "buah",
                                                                    "Susu" = "susu",
                                                                    "Margarin" = "margarin"
                                                                    )
                                                        ),
                                            #hidden(
                                            #  div(id = "group_komoditas",                  
                                            #      checkboxInput("beras","Seluruh Komoditas Beras", value = TRUE),
                                            #      checkboxInput("cabe","Seluruh Komoditas Cabe", value = TRUE),
                                            #      checkboxInput("bawang","Seluruh Komoditas Bawang", value = TRUE),
                                            #      checkboxInput("buah","Seluruh Komoditas Buah", value = TRUE),
                                            #      checkboxInput("daging","Seluruh Komoditas Daging", value = TRUE),
                                            #      checkboxInput("susu","Seluruh Komoditas Susu", value = TRUE),
                                            #      checkboxInput("tepung","Seluruh Komoditas Tepung", value = TRUE)
                                            #  )
                                            #),
                                            #a(id = "data_all_komoditas", "Tampilkan Lebih Banyak .."),
                                            #hidden(
                                            #  div(id = "all_komoditas",
                                            #    uiOutput("themesControl") # the id
                                            #  )
                                            #),
                                            h4(),
                                            #actionButton(inputId = "clearAllBottom",
                                            #             label = "Clear selection",
                                            #             class = "btn-info",
                                            #             icon = icon("square-o")),
                                            #actionButton(inputId = "selectAllBottom",
                                            #             label = "Select all",
                                            #             class = "btn-primary",
                                            #             icon = icon("check-square-o"))
                                            actionButton("pilih_data_cek", "Pilih Kelompok Komoditas",class = "btn-info")
                                          )
                                        )
                                        ),
                           column(9,
                               tabsetPanel(
                                tabPanel(p(icon("line-chart"), "Grafik"), h4(""),withSpinner(plotlyOutput("data_plot"))
                                         ), 
                                tabPanel(p(icon("table"), "Tabel"), 
                                        withSpinner(DT::dataTableOutput("tabel_data")),
                                        h4(),
                                        downloadButton("downloadData", "Download Data")
                                        )
                               )
                          )
                        )
               ),
               tabPanel("Tentang", icon = icon("user"),
                        fluidRow(
                          h2("Tentang Situs Ini !", align = "center"),
                          hr(),
                          wells(content = "sistem yang mampu melakukan web crawling data harga pada PIPHS Jakarta dan beberapa situs retail besar di Indonesia. 
                          Selain melakukan web scraping, sistem ini  dapat melakukan cleaning pada data yang telah didapatkan. 
                          Pembangunan modul ini dilakukan berdasarkan masalah tidak adanya sistem yang mampu melakukan web scraping secara otomatis dan dinamis pada data harga sehingga kita dapat mengetahui pergerakan harga secara real time. Selain itu aplikasi web scraping pada umumnya sulit untuk dioperasikan dan tidak dapat melakukan proses lebih jauh. Saat ini juga belum terdapat sistem yang melakukan peramalan harga untuk masa yang akan datang data harga online secara real time.
                          Sistem ini dibangun dengan menggunakan R yang ditulis dengan aplikasi Rstudio. Sistem ini memanfaatkan library rvest dan jsonlite untuk melakukan web scraping. Selain itu, sistem ini secara rutin melakukan pembaruan data harga setiap hari secara otomatis. Sistem ini berbasis web dan mampu mempublikasikan data harga hasil web scraping yang telah dikumpulkan sehingga dapat dimanfaatkan oleh berbagai pihak. Sistem ini akan dibangun menggunakan package Shiny sehingga dapat dengan mudah dioperasikan oleh pengguna. Selain itu, untuk melakukan peramalan harga untuk masa yang akan datang sistem ini akan menggunakan library prophet.
                                ",
                                size = "large")
                          ),
                          fluidRow(
                            column(6, panel_div("info", "Version", "0.0.1 - 29.01.2018
                                                Code for this app has been published @Github")),
                            column(6, panel_div("success", "Kontak Kami",
                                                "Email : <a href='mailto:irsad.arief@gmail.com?Subject=DashboardHargaPangan' target='_top'>Muhammad Irsad Arief</a>
                                                "))
                          ),
                          fluidRow(
                            column(12, panel_div("primary", "Kredit",
                                                "<a href='https://shiny.rstudio.com/'>Shiny</a> Oleh <a href='https://www.rstudio.com/'>RStudio</a> 
                                                <br/><a href='https://rstudio.github.io'>R Interface to the jQuery Plug-in DataTables </a> Oleh <a href='https://www.rstudio.com/'>Rstudio</a>
                                                <br/><a href='https://facebook.github.io/prophet/'>Prophet, a forecasting tool</a> Oleh <a href='https://research.fb.com/category/data-science/'>Facebook Data Science Team</a>
                                                <br/><a href='httpshttp://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/'>rvest: easy web scraping with R</a> Oleh Hadley Wickham 
                                                <br/><a href='https://github.com/andrewsali/shinycssloaders/'>Shiny CSS Loaders</a> Oleh <a href='https://github.com/andrewsali'>Andrew Sali</a>
                                                <br/><a href='https://ebailey78.github.io/shinyBS/'>ShinyBS</a> Oleh <a href='https://github.com/ebailey78'>Ebailey78</a>
                                                <br/><a href='https://github.com/jasdumas/shinyLP'>ShinyLP</a> Oleh <a href='https://github.com/jasdumas'>Jasmine Dumas</a>
                                                <br/><a href='https://plot.ly/r'>Plotly for R</a> Oleh <a href='https://plot.ly/'>Plotly</a>
                                                <br/><a href='https://cran.r-project.org/web/packages/pastecs/index.html'>pastecs: Package for Analysis of Space-Time Ecological Series</a> Oleh Philippe Grosjean 
                                                <br/><a href='https://deanattali.com/shinyjs/'>Shinyjs</a> Oleh <a href='https://deanattali.com/'>Dean Attali</a>
                                                <br/>Situs ini terinspirasi dari situs <a href='http://www.dataseries.org/'>Switzerland's data series in one place</a> oleh <a href='http://www.christophsax.com/'>Cristoph Sax</a>
                                                "))
                          )
               ),
               tabPanel("Temukan Kami di Twitter", icon = icon("twitter"),
                        HTML("<a href='https://twitter.com/irsad_arief?ref_src=twsrc%5Etfw' class='twitter-follow-button' 
                             data-show-count='false'>Follow @irsad_arief</a>
                             <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>"),
                        fluidRow(
                          column(2),
                          column(8,
                                 HTML("<a class='twitter-timeline' data-lang='id' data-height='1200' 
                                      href='https://twitter.com/scraper_harga?ref_src=twsrc%5Etfw'>Tweets by Scraper Harga Komoditas</a>
                                      <script async src='https://platform.twitter.com/widgets.js' charset='utf-8'></script>")
                                 ),
                          column(2)
                        )
                        
               ),
               tabPanel("Kelola Pengguna", icon = icon("users"), value = "Admin",
                        h2("Kelola Pengguna", align = "center"),
                        hr(),
                        fluidRow(
                          column(1),
                          column(10,withSpinner(DT::dataTableOutput("tabel_admin"))),
                          column(1)
                        ),
                        bsModal(id = 'sukses_ubah_status', title = 'Sukses Ubah Status Pengguna', trigger = '',
                                size = 'large', p("Data Status Pengguna Telah Berhasil Diperbarui"))
               )
      ),bsModal("login_modal", "Login", "login_trigger", size = "small" ,
                wellPanel(id = "login",
                          textInput("username", "Username:"),
                          passwordInput("password", "Password:"),
                          actionButton("login", "Log in",class = "btn-primary"),
                          h5("Belum Punya Akun ?", align = "center"),
                          hr(),actionButton("signup_trigger","Sign Up",class = "btn-info"),
                          textOutput("message"))
                
      ),
      bsModal("signup_modal", "Sign Up", "signup_trigger", size = "small" ,
            wellPanel(id = "signup",
                      textInput("first_name", "Nama Depan:"),
                      textInput("last_name", "Nama Belakang:"),
                      textInput("username_daftar", "Username:"),
                      textInput("email", "Email:"),
                      passwordInput("password_daftar", "Password:"),
                      actionButton("signup", "Sign Up",class = "btn-primary"))
            
      ),
      bsModal(id = 'sukses_daftar', title = 'Sukses Sign Up', trigger = '',
            size = 'large', p("Anda Telah Berhasil Mendaftar Pada Sistem Aplikasi Ini")),
      bsModal(id = 'gagal_signup', title = 'Gagal Sign Up', trigger = '',
            size = 'large', p("Informasi Yang Anda Masukkan Tidak Tepat")),
      bsModal(id = 'gagal_login', title = 'Gagal Login', trigger = '',
            size = 'large', p("Username Dan Atau Password Yang Anda Masukkan Tidak Tepat")),
    bsModal(id = 'gagal_login_kosong', title = 'Gagal Login', trigger = '',
            size = 'large', p("Username Dan Atau Password Yang Anda Masukkan Tidak Boleh Kosong")),
      bsModal("tutorial_modal", "Tutorial Penggunaan Aplikasi", "tutorial_trigger", size = "large" ,
             h4("1. Pilih komoditas yang anda ")
             
    )
    
  )
)
