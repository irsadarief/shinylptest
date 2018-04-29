
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyLP)
library(shinyjs)
library(shinysky)
library(plotly)
library(shinycssloaders)
#library(shinymaterial)
#library(htmltools)



#fungsi sehingga bisa menaruh inputs pada navbar
navbarPageWithInputs <- function(..., inputs = NULL) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(form,
    navbar[[3]][[1]]$children[[1]])
  navbar
}

shinyUI(
  
  # masukkan fluid page agar bisa menaruh icon gambar logo pada navbar
  # sumber: http://stackoverflow.com/a/24764483
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
               theme = shinytheme("simplex"),
               #inputs = actionButton("action", "Action", icon("paper-plane"), styleclass = "link"),
               
               tabPanel("Beranda", icon = icon("home"), value = "Beranda",
                        column(12,align="center",
                               
                               div( id="selects", 
                               h1("LOREM IPSUM"),
                               h4("Lorem ipsum dolor sit amet, consectetur adipiscing elit"),
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
                                 checkboxInput("yearly","yearly.seasonality", value = TRUE),
                                 
                                 ### parameter: weekly.seasonality 
                                 checkboxInput("monthly","weekly.seasonality", value = TRUE),
                                 
                                 sliderInput("tanggal",label = "Waktu",
                                             min = as.Date(data$ds[1],"%Y-%m-%d"),
                                             max = as.Date(data$ds[length(data$ds)],"%Y-%m-%d"),
                                             value=c(as.Date(data$ds[1]),as.Date(data$ds[length(data$ds)])),
                                             timeFormat="%Y-%m-%d"),
                                 sliderInput("periode",label = "Prediksi hari", min=1,max=100,value = 10),
                                 a(id = "prophet_advanced", "Tampilkan / Sembunyikan Info Rinci"),
                                 hidden(
                                 div(id = "advanced",
                                 ### parameter: seasonality.prior.scale
                                 numericInput("seasonality_scale","seasonality.prior.scale", value = 10),
                                 
                                 ### parameter: changepoint.prior.scale
                                 numericInput("changepoint_scale","changepoint.prior.scale", value = 0.05, step = 0.01),
                                 
                                 ### parameter: mcmc.samples
                                 numericInput("mcmc.samples", "mcmc.samples", value = 0),
                                 
                                 ### parameter: interval.width
                                 numericInput("interval.width", "interval.width", value= 0.8, step = 0.1),
                                 ### parameter: uncertainty.samples
                                 numericInput("uncertainty.samples","uncertainty.samples", value = 1000)
                                 )),
                                 h6(""),
                                 actionButton("update_model", "Update Prophet Model",class = "btn-primary")
                          ),
                          column(9,
                                 uiOutput("dropdown_variabel2")
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
                                       selectizeInput("Website", "Pilih Website",c("Klikindomaret" = 1, "Hypermart" = 2) , selected = "", options = list(placeholder = "Pilih Website")),
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
                                 bsModal(id = 'sukses_simpan', title = 'Dum Dum', trigger = '',
                                         size = 'large', p("here is my mumbo jumbo"))
                          )
                        )
                        ),
               tabPanel("Data", icon = icon("database"),
                        h2("Hello Visitors!", align = "center"),
                        hr(),
                        tabPanel("Beras", DT::dataTableOutput("tabel1")),
                        h4(),
                        fluidRow(
                          p(class = 'text-center', downloadButton("downloadData", "Download Data"))
                        ),
                        h4(),
                        wells(content = "Imporant Info can go up here before a
                              user starts exploring the application and its features",
                              size = "default")
               ),
               tabPanel("Tentang", icon = icon("user"),
                        fluidRow(
                          h2("Tentang!", align = "center"),
                          hr(),
                          wells(content = "Imporant Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum at lorem ut turpis mattis maximus at ac tellus. Phasellus in sem at augue rhoncus consectetur. Sed eu risus ante. Nulla sagittis luctus mi, sit amet semper velit dictum faucibus. Nullam eu pretium tortor, nec molestie mauris. Nunc nibh purus, malesuada id sapien id, pellentesque sodales ante. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nam feugiat sit amet odio nec sagittis.
                                          Curabitur sit amet nulla id nibh vestibulum tincidunt. Pellentesque at metus ex. Cras aliquam id dui non vulputate. Donec sed ipsum id nisl viverra tempor. In a leo lectus. Pellentesque sit amet maximus lorem. Morbi eget massa ac libero dignissim laoreet a vitae justo. Nulla lobortis quis diam non euismod. Praesent dictum ornare leo id lacinia. Proin tincidunt, mauris eget pulvinar vestibulum, risus risus volutpat justo, sit amet maximus turpis urna quis ante. Quisque fringilla lacus sed ipsum ultrices, in pellentesque nulla fermentum. Vestibulum interdum nulla justo. ",
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
                            column(6, panel_div(class_type = "primary", panel_title = "Petunjuk Penggunaan",
                                                content = "How to use the app")),
                            column(6, panel_div("danger", "Credits",
                                                "<a href='https://shiny.rstudio.com/'>Shiny</a> By <a href='https://www.rstudio.com/'>RStudio</a> 
                                                <br/><a href='https://github.com/andrewsali/shinycssloaders/'>Shiny CSS Loaders</a> By <a href='https://github.com/andrewsali'>Andrew Sali</a>
                                                <br/><a href='https://ebailey78.github.io/shinyBS/'>ShinyBS</a> By <a href='https://github.com/ebailey78'>Ebailey78</a>
                                                <br/><a href='https://github.com/jasdumas/shinyLP'>ShinyLP</a> By <a href='https://github.com/jasdumas'>Jasmine Dumas</a>
                                                <br/><a href='https://rstudio.github.io'>R Interface to the jQuery Plug-in DataTables </a> By <a href='https://www.rstudio.com/'>Rstudio</a>
                                                <br/><a href='https://facebook.github.io/prophet/'>Prophet, a forecasting tool</a> By <a href='https://research.fb.com/category/data-science/'>Facebook Data Science Team</a>
                                                <br/><a href='https://plot.ly/r'>Plotly for R</a> By <a href='https://plot.ly/'>Plotly</a>
                                                <br/><a href='https://cran.r-project.org/web/packages/pastecs/index.html'>pastecs: Package for Analysis of Space-Time Ecological Series</a> By Philippe Grosjean 
                                                <br/><a href='https://deanattali.com/shinyjs/'>Shinyjs</a> By <a href='https://deanattali.com/'>Dean Attali</a>
                                                <br/>This website is inspired by <a href='http://www.dataseries.org/'>Switzerland's data series in one place</a> By <a href='http://www.christophsax.com/'>Cristoph Sax</a>
                                                "))
                          )
               ),
               tabPanel("Temukan Kami di Twitter", icon = icon("twitter")),
               tabPanel("Home Page 1", icon = icon("home"),
                        
                        jumbotron("Hi ShinyLP!", "Call attention to important application features or provide guidance",
                                  buttonLabel = "Click Me"),
                        fluidRow(
                          column(6, panel_div(class_type = "primary", panel_title = "Directions",
                                              content = "How to use the app")),
                          column(6, panel_div("success", "Application Maintainers",
                                              "Email Me: <a href='mailto:jasmine.dumas@gmail.com?Subject=Shiny%20Help' target='_top'>Jasmine Dumas</a>"))
                        ),  # end of fluidRow
                        fluidRow(
                          column(6, panel_div("info", "App Status", "Include text with status, version and updates")),
                          column(6, panel_div("danger", "Security and License", "Copyright 2016")),
                          
                          #### FAVICON TAGS SECTION ####
                          tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                          
                          #### JAVASCRIPT TAGS SECTION #### - ENABLE WHEN READY
                          #tags$head(tags$script(src='pl.js')),
                          
                          bsModal("modalExample", "Instructional Video", "tabBut", size = "large" ,
                                  p("Additional text and widgets can be added in these modal boxes. Video plays in chrome browser"),
                                  iframe(width = "560", height = "315", url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
                          )
                        ))
      )
    
  )
) # end of shiny