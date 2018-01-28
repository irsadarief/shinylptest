
library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyLP)
library(shinyjs)
library(shinysky)
library(plotly)
#library(htmltools)

#list variabel
listvar<-c("Beras IR. I (IR 64)"="BerasIR1",
           "Beras IR. II (IR 64) Ramos"="BerasIR2Ramos",
           "Beras IR. III (IR 64)"="BerasIR3",
           "Beras Muncul .I"="BerasMuncul",
           "Beras IR 42/Pera"="BerasIR42Pera",
           "Beras Setra I/Premium"="BerasSetraI/Premium",
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
                               h4("Dolor sit Amet "),
                               tags$style(type='text/css', "#selects .shiny-input-container:not(.shiny-input-container-inline){width:100%} 
                                          #selects .selectize-input {width:100%; max-width:500px; height:40px; font-size:125%;} 
                                          #selects .selectize-dropdown {text-align :left; font-size:125%;}"),
                               selectizeInput("country", "",listvar , selected = "", options = list(placeholder = "Pilih Komoditas")
                               ), style = "padding-top:10%;"
                               )
                        )
                        ),
               tabPanel("Dashboard", icon = icon("area-chart"), value = "Dashboard",
                        fluidRow(
                          column(9,
                                 selectInput("komoditas", "Pilih Komoditas", listvar)
                                 , tabsetPanel(
                                   tabPanel("Plot", h1(""),plotlyOutput("ts_plot")),
                                   tabPanel("Plot Komponen", h1(""), plotOutput("ts_plot2")),
                                   tabPanel("Tabel", h1(""), DT::dataTableOutput("tabelprophet"))
                                 )
                                 ),
                          column(3,
                                 h1("")
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
                                size = "large"),
                          fluidRow(
                            column(6, panel_div(class_type = "primary", panel_title = "Directions",
                                                content = "How to use the app")),
                            column(6, panel_div("success", "Application Maintainers",
                                                "Email Me: <a href='mailto:jasmine.dumas@gmail.com?Subject=Shiny%20Help' target='_top'>Muhammad Irsad Arief</a>"))
                          )
                        )
               ),
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