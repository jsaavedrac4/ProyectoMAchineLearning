library(shiny)
library(DT)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("flatly"),"MODELO PREDICTIVO ",
                 
                 tabPanel("Dataset",DT::dataTableOutput("Dataset")),
                 
                 ####
                 tabPanel("Exploracion",
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput(inputId = "consulta",
                                          label = "Seleccionar consulta",
                                          choices = c("Exploracion 1"="e1",
                                                      "Exploracion 2"="e2",
                                                      "Exploracion 3"="e3",
                                                      "Exploracion 4"="e4",
                                                      "Exploracion 5"="e5",
                                                      "Exploracion 6"="e6",
                                                      "Exploracion 7"="e7",
                                                      "Exploracion 8"="e8",
                                                      "Exploracion 9"="e9",
                                                      "Exploracion 10"="e10",
                                                      "Exploracion 11"="e11",
                                                      "Exploracion 12"="e12",
                                                      "Exploracion 13"="e13",
                                                      "Exploracion 14"="e14",
                                                      "Exploracion 15"="e15",
                                                      "Exploracion 16"="e16",
                                                      "Exploracion 17"="e17",
                                                      "Exploracion 18"="e18",
                                                      "Exploracion 19"="e19",
                                                      "Exploracion 20"="e20",
                                                      "Exploracion 21"="e21",
                                                      "Exploracion 22"="e22",
                                                      "Exploracion 23"="e23",
                                                      "Exploracion 24"="e24",
                                                      "Exploracion 25"="e25",
                                                      "Exploracion 27"="e26",
                                                      "Exploracion 27"="e27",
                                                      "Exploracion 28"="e28",
                                                      "Exploracion 29"="e29",
                                                      "Exploracion 30"="e30"
                                                      
                                                      
                                          ))
                            ),
                            mainPanel(
                              
                              textOutput("text"),
                              
                              hr(),
                              plotlyOutput("plot"),
                              
                              hr(),
                              dataTableOutput("tb1")
                            )
                            
                            
                            
                            
                          )),
                 
                 
                 
                 navbarMenu("Modelos",
                            
                            tabPanel("Modelo 1",
                                     sidebarLayout(
                                       
                                       h4("Algortimo Arbol de Decisiones"),
                                       
                                       mainPanel(
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("grafico",plotOutput("grafico1")),
                                                     tabPanel("Matriz de confusion", verbatimTextOutput("matriz3")),
                                                     tabPanel("Presicion", verbatimTextOutput("matriz4"))
                                         )
                                       )
                                     )
                            ),
                            
                            tabPanel("Modelo 2",
                                     sidebarLayout(
                                       
                                       h4("Algortimo Random Forest"),
                                       
                                       mainPanel(
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Matriz de confusion", verbatimTextOutput("matriz6")),
                                                     tabPanel("Presicion", verbatimTextOutput("matriz7"))
                                         )
                                       )
                                     )
                                     
                            ),
                            tabPanel("Modelo 3",
                                     
                                     sidebarLayout(
                                       
                                       h4("Algortimo SVM"),
                                       
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Matriz de confusion", verbatimTextOutput("matriz1")),
                                           tabPanel("Presicion", verbatimTextOutput("matriz2"))
                                         )
                                       )
                                     )
                                     
                                     
                                     
                            ),
                            
                            
                            
                            
                            
                            tabPanel("Modelo 4",
                                     sidebarLayout(
                                       
                                       h4("Naive Bayes"),
                                       
                                       mainPanel(
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Matriz de confusion", verbatimTextOutput("matriz8")),
                                                     tabPanel("Presicion", verbatimTextOutput("matriz9"))
                                         )
                                       )
                                     )
                                     
                                     
                                     
                            ),
                            tabPanel("Modelo 5",sidebarLayout(
                              
                              h4("Redes Neuronales"),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Matriz de confusion", verbatimTextOutput("matriz20")),
                                            tabPanel("Presicion", verbatimTextOutput("matriz21"))
                                )
                              )
                            ))
                            
                 ),
                 
                 
                 tabPanel("CrossValidation",
                          sidebarLayout(
                            
                            h4("Cross Validation"),
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Presicion", verbatimTextOutput("matriz11")),
                                          tabPanel("Matriz de confusion", verbatimTextOutput("matriz10"))
                              )
                            )
                          )
                          
                          
                          
                          
                          
                 )
)





#######

server <- function(input, output) {
  
  #######Impresion dataset
  output$Dataset <- DT::renderDataTable({
    DT::datatable(maysi)
  })  
  
  ####TExto Exploracion
  textWInput <- reactive({
    switch(input$consulta,
           e1 = "Total de produccion de docenas semanales",
           e2 = "Total de metros de cuero que se utiliza para la fabricacion por docena en cada semana",
           e3 = "Cantidad Total de docenas producidas en cada mes de cada año segun las tallas",
           e4 = "Cantidad Total del material que se utilizo en cada mes de cada año segun las tallas",
           e5 = "Cantidad de docenas producidas cada año en cada campaña",
           e6 = "Cantidad total de docenas desde el año 2010 al 2017 por cada campaña",
           e7 = "Cantidad total de material en metros utilizados en cada talla desde el 2010 hasta el 2017",
           e8 = "Cantidad total de docenas producidas por cada año de cada campaña segun el color",
           e9 = "Cantidad docenas de cada campaña por color del todo el año 2010 segun cada mes",
           e10 = "total de docenas segun la talla y cada color",
           e11 = "Total de material utilizado segun la cantidad total de docenas producidad porel color en su respectiva campaña",
           e12= "Cantidad total de material utilizado segun las docenas producidas de cada campaña del mes de Enero",
           e13= "Cantidad de docenas producidas por docenas segun cada modelo de las campania Botas",
           e14 = "Cantidad de modelos de la campañia botas que no se producjeron cada año",
           e15 = "Cantidad de material utilizado segun la cantidad de docenas producidad en el mes de Mayo de la campaña de Botas segun su color",
           e16 = "Cantidad de docenas producidas en en el año 2017 segun la campaña Escolar con su respectivo modelo",
           e17 = "Cantidad de inversion por el material utilizado en metros segun cada año y por cada campañia",
           e19 = "Cantidad de inversion en la mano de obra utilizado en cada año segun la campaña",
           e20 = "Cantidad de inversion en el año 2015 segun cada mes",
           e21 = "Total de material en metros utilizados con la cantidad del costo invertido segun la cantidad de cada anio en cada campañia",
           e22 = "Cantidad inversion de planta segun la cantidad de docenas producidad en cada año",
           e23="Ganancia total de cada año segun la camapania",
           e24="Ganancia total segun cada mes del año 2012,2013 y 2016",
           e25="Ganancia segun la cantidad docenas producidas de cada campaña en cada año",
           e26="Ganancia segun la cantidad de docenas producidas por talla en cada año segun cada campaña",
           e27="Ganancia total en cada año",
           e28="Inversion de otros costos segun la cantidad de docenas producidas en cada campaña por año",
           e29="Ganancia total en la campaña solo de charol en cada año",
           e30="inversion total de cada año"
           
    )
  })
  
  output$text<- renderText({
    textWInput()
  })
  ######Grafico Exploracion
  GraficoEInput <- reactive({
    switch(input$consulta,
           e1 = p1,
           e2 = p2,
           e3 = p3,
           e4 = p4,
           e5 = p5,
           e6 = p6,
           e7 = p7,
           e8 = p8,
           e9 = p9,
           e10 = p10,
           e11 = p11,
           e12= p12,
           e13= p13,
           e14 = p14,
           e15 = p15,
           e16 = p16,
           e17 = p18,
           e19 = p19,
           e20 = p20,
           e21 = p21,
           e22 = p22,
           e23=p23,
           e24=p24,
           e25=p25,
           e26=p26,
           e27=p27,
           e28=p28,
           e29=p29,
           e30=p30
           
    )
  })
  
  output$plot<- renderPlotly({
    GraficoEInput()
  })
  ######Datasets
  dataEInput <- reactive({
    switch(input$consulta,
           e1 = c1,
           e2 = c2,
           e3 = c3,
           e4 = c4,
           e5 = c5,
           e6 = c6,
           e7 = c7,
           e8 = c8,
           e9 = c9,
           e10 = c10,
           e11 = c11,
           e12= c12,
           e13= c13,
           e14 = c14,
           e15 = c15,
           e16 = c16,
           e17 = c18,
           e19 = c19,
           e20 = c20,
           e21 = c21,
           e22 = c22,
           e23=c23,
           e24=c24,
           e25=c25,
           e26=c26,
           e27=c27,
           e28=c28,
           e29=c29,
           e30=c30
           
    )
  })
  
  output$tb1<- DT::renderDataTable({
    dataEInput()
  })
  
  ######Modelos
  
  
  output$matriz1 <- renderPrint({
    
    SVMtabla
    
  })
  
  output$matriz2 <- renderPrint({
    
    sum(diag(SVMtabla))/sum(SVMtabla)
    
  })
  ######Arbol Decisiones Modelo
  output$grafico1 <- renderPlot({
    plot(arbol)
  })
  
  output$matriz3 <- renderPrint({
    
    arboltabla
    
  })
  
  output$matriz4 <- renderPrint({
    
    sum(diag(arboltabla))/sum(arboltabla)
    
  })
  
  ######Random forest
  
  
  output$matriz6 <- renderPrint({
    
    RFtabla
    
  })
  
  output$matriz7 <- renderPrint({
    
    sum(diag(RFtabla))/sum(RFtabla)
    
  })
  ######Naive Bayes
  
  
  output$matriz8 <- renderPrint({
    
    cm1
    
  })
  
  output$matriz9 <- renderPrint({
    
    sum(diag(cm1))/sum(cm1)
    
  })
  
  ######Neural Network
  output$matriz20 <- renderPrint({
    
    nnettabla
    
  })
  output$matriz21 <- renderPrint({
    
    prec
    
  })
  
  
  
  
  #####Cross Validation
  
  output$matriz11 <- renderPrint({
    
    r1
    
  })
  output$matriz10 <- renderPrint({
    
    mc
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)