  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #
  
  library(shiny)
  library(shinydashboard)
  library(markovchain)
  library(expm)
  library(queueing)
  library(rhandsontable)
  library(readxl)
  library(matrixcalc) 
  # Creating dataset
  k=0
  f=0
  s=0
  rpta=0
  p=0
  n=0
  l=0
  m=c()
  m2=c()
  
  a<-matrix(c("",1,2,3,"Bajar",0.5,0.1,0,"Mantener",0.5,0.3,0.2,"Subir",0,0.6,0.8),nrow = 4,ncol = 4)
  df1=a
  b<-matrix(c("",1,2,3,"Bajar",0.8,0.25,0,"Mantener",0.2,0.75,1,"Subir",0,0,0),nrow = 4,ncol = 4)
  df2=b
  c<-matrix(c("",1,2,3,"Bajar",0,0,0,"Mantener",1,0.35,0.3,"Subir",0,0.65,0.7),nrow = 4,ncol = 4)
  df3=c
  
  estadossl = c( " 0 ,  0 ,  0 " ,  " 1 ,  0 ,  0 " ,  " 2 ,  0 ,  0 " ,  " 3 ,  0 ,  0 " ," 0 ,  1 ,  0 " ,  " 1 ,  1 ,  0 " ,  " 2 ,  1 ,  0 " ,  " 3 ,  1 ,  0 " ," 0 ,  2 ,  0 " ,  " 1 ,  2 ,  0 " ,  " 2 ,  2 ,  0 " ,  " 3 ,  2 ,  0 " ," 0 ,  0 ,  1 " ,  " 1 ,  0 ,  1 " ,  " 2 ,  0 ,  1 " ,  " 3 ,  0 ,  1 " ," 0 ,  1 ,  1 " ,  " 1 ,  1 ,  1 " ,  " 2 ,  1 ,  1 " ,  " 3 ,  1 ,  1 " ," 0 ,  2 ,  1 " ,  " 1 ,  2 ,  1 " ,  " 2 ,  2 ,  1 " ,  " 3 ,  2 ,  1 " ," 0 ,  0 ,  2 " ,  " 1 ,  0 ,  2 " ,  " 2 ,  0 ,  2 " ,  " 3 ,  0 ,  2 " ," 0 ,  1 ,  2 " ,  " 1 ,  1 ,  2 " ,  " 2 ,  1 ,  2 " ,  " 3 ,  1 ,  2 " ," 0 ,  2 ,  2 " ,  " 1 ,  2 ,  2 " ,  " 2 ,  2 ,  2 " ,  " 3 ,  2 ,  2 ")
  
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    # Application title
    titlePanel("Soluciones TISC"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
      selectInput(inputId = "fase",
                  label = "Escoja la fase que desea visualizar",
                  choices = c("Seleccionar consulta","Politicas de Inventario", "Proceso de Produccion", "Politica de Atención") ),
      conditionalPanel(condition = "input.fase == 'Politicas de Inventario'", 
        titlePanel("Politicas de Inventario"),
        numericInput(inputId ="CostoInventarioUnidad", label="Ingrese el costo de una unidad de un papel determinado en inventario por semana", value=1),
        numericInput(inputId ="CostoPedido", label="Ingrese el costo de hacer un pedido de un papel determinado", value=1),
        selectInput(inputId ="TipoPapel", label="Ingrese el tipo de papel a analizar",choices=c("Papel Bond","Papel Propalcote","Papel Químico","Papel Periódico","Maule"), selected ="Papel Bond", multiple = FALSE),
        titlePanel("Cantidad de unidades que se piden para llenar el inventario"),
        numericInput(inputId ="CantidadLlenar", label="Unidades a incrementar el inventario", value=12),
        titlePanel("") ),
      conditionalPanel(condition = "input.fase == 'Proceso de Produccion'",
                       
      titlePanel("Proceso de Produccion"),
      selectInput(inputId = "requer",
                  label = "Escoja el requerimiento que desea visualizar",
                  choices = c("Parametros del tiempo promedio en produccion", "Parametro de numero de veces de Buffers llenos", "Parametro de tiempo promedio antes de Buffers llenos") ),
      
      conditionalPanel(condition = "input.requer == 'Parametros del tiempo promedio en produccion'",
                       numericInput(inputId ="TasaA", label="Ingrese el valor de la tasa de entrada a la maquina de presion.", value=1/20),
                       numericInput(inputId ="TasaB", label="Ingrese el valor de la tasa de procesamiento de la maquina de presion.", value=1/10),
                       numericInput(inputId ="TasaC", label="Ingrese el valor de la tasa de procesamiento a la maquina de recubrimiento", value=1/8),
                       numericInput(inputId ="TasaD", label="Ingrese el valor de la tasa de procesamiento a la maquina de alisado.", value=11/60)),
      
      conditionalPanel(condition = "input.requer == 'Parametro de numero de veces de Buffers llenos'",
                       selectInput(inputId = "estado1",
                                   label = "Escoja el estado inicial para determinar el numero de veces que los Buffers estaran llenos en el dia:",
                                   choices = estadossl),
                       h5("Tenga en cuenta que el primer valor hace referencia al número de rollos en Prensas, el segundo en la zona de Recubrimiento y el último a Alisado.")),
      conditionalPanel(condition = "input.requer == 'Parametro de tiempo promedio antes de Buffers llenos'",
                       
                       selectInput(inputId = "estado2",
                                   label = "Escoja el estado inicial para determinar el tiempo promedio antes de que los Buffers estén llenos:",
                                   choices = estadossl),
                       h5("Tenga en cuenta que el primer valor hace referencia al número de rollos en Prensas, el segundo en la zona de Recubrimiento y el último a Alisado.")
      ))
      
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        conditionalPanel(condition = "input.fase == 'Politicas de Inventario'",
        fluidRow(column(width=12),
        h2("Estrategia 1"),
        h3("Probabilidad tener x cantidad de unidades en inventario al finalizar la semana n"),
        plotOutput("ProbEstadoEstable")),
        fluidRow(column(width=12),h3("Valores Esperados")),
        fluidRow(column(width=4,
        h4("Costos de inventario"),
        textOutput("cInv"),
        h4("Costos por pedidos"),
        textOutput("cPed")),
        column(width=4,
        h4("Probabilidad de faltantes"),
        textOutput("cFaltantes")),
        column(width=4,
        h4("Inventario promedio semanal"),
        textOutput("invProm"),
        h4("Costos totales anuales"),
        textOutput("cTot"))),
        fluidRow(column(width=12),
        h2("Estrategia 2"),
        h3("Probabilidad tener x cantidad de unidades en inventario al finalizar la semana n"),
        plotOutput("ProbEstadoEstable2")),
        fluidRow(column(width=12),h3("Valores Esperados")),
        fluidRow(column(width=4,
        h4("Costos de inventario"),
        textOutput("cInv2"),
        h4("Costos por pedidos"),
        textOutput("cPed2")),
        column(width=4,
        h4("Probabilidad de faltantes"),
        textOutput("cFaltantes2")),
        column(width=4,
        h4("Inventario promedio semanal"),
        textOutput("invProm2"),
        h4("Costos totales anuales"),
        textOutput("cTot2")))),
        
        
        
                         conditionalPanel(condition = "input.fase == 'Proceso de Produccion'",
                                          
                                          conditionalPanel(condition = "input.requer == 'Parametros del tiempo promedio en produccion'",
                                                           fluidRow(column(width=12) ,
                                                                    h2("Tiempo promedio de un rollo en el area de produccion"),
                                                                    textOutput("tiempoprom"))
                                          ),                
                                          conditionalPanel(condition = "input.requer == 'Parametro de numero de veces de Buffers llenos'",
                                                           fluidRow(column(width=12),
                                                                    h2("Numero de veces de Buffers llenos en un dia laboral"),
                                                                    textOutput("nveces"))),                 
                                          conditionalPanel(condition = "input.requer == 'Parametro de tiempo promedio antes de Buffers llenos'",
                                                           fluidRow(column(width=12) ,
                                                                    h2("Tiempo promedio antes de que los buffers esten llenos"),
                                                                    textOutput("tiempollenos"))))   
        
        
      ,
      conditionalPanel(condition = "input.fase == 'Politica de Atención'",
      fluidRow(
        titlePanel(title = "Soluciones TISC"),
        h4("Politica de Atencion al Cliente"),
        hr(),
        column(4,
               helpText("Probabilidades de cambio de categoria dado que se encuentra en Oro"),
               rHandsontableOutput("table1"),
               br(),
               actionButton("save","Guardar Datos")),
        column(4,
               helpText("Probabilidades de cambio de categoria dado que se encuentra en Plata"),
               rHandsontableOutput("table")),
        column(4,
               helpText("Probabilidades de cambio de categoria dado que se encuentra en Bronce"),
               rHandsontableOutput("table2"),
               br(),
               textOutput("resultado"))
      )
      
    ))
  
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    library(readxl)
    library(knitr)
    library(ggplot2)
    library(magrittr)
    library(dplyr)
    library(plyr)
    library(stats)
    library(markovchain)
    library(knitr)
    
    f22<-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22")
    f20<-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")
    f24<-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24")
    f25<-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25")
    
    k
    
    
    #Estrategia 1
    
    
    
    l=k*191400
    costosTotales=l+g
    costosTotales=costosTotales*4
    costosAnuales=costosTotales*12
    costosAnuales
    
    output$ProbEstadoEstable <- renderPlot({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      estados = seq(from=0,to=n,by=1)
      model1 = matrix(0, nrow=n+1,ncol=n+1)
      h=input$CantidadLlenar
      for(i in estados){
        for(j in estados){
          if(j>=1 & i<=n-h & j<=i+h){
            model1[i+1,j+1]=dpois(h-j+i,p)
          }
          if(i<=n-h & j==0){
            model1[i+1,j+1]=1-ppois(h-1+i,p)
          }
          if(i>=1 & n-(h-1)<=i & i<=n & j<=i){
            model1[i+1,j+1]=dpois(i-j,p)
          }
          if(n-(h-1)<=i & i<=n & j==0){
            model1[i+1,j+1]=1-ppois(i-1,p)
          }
        }
      }
      if(n == 22){
        cad1 = new(Class="markovchain", states=f22, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 20){
        cad1 = new(Class="markovchain", states=f20, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 24){
        cad1 = new(Class="markovchain", states=f24, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 25){
        cad1 = new(Class="markovchain", states=f25, byrow = TRUE, transitionMatrix = model1)
      }
      is.irreducible(cad1)
      steadyStates(cad1)
      m=steadyStates(cad1)
    
      barplot(m, names.arg = estados, ylab="Probabilidad Asociada", xlab="Cantidad de rollos de papel en inventario al final de una semana", border = "red")
    })
    
    output$cInv<-renderText({
      s=0
      for(i in 1:n){
        s = s+(i-1)*m[i]
      }
      f=input$CostoInventarioUnidad
      g=f*s
      toString(g)
    })
    
    output$cPed<-renderText({
        k=0
        r=n-12
        for(i in 1:(r+1)){
          k=k+m[i]*input$CostoPedido
        }
        toString(k)
      })
    
    output$cFaltantes<-renderText({
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      rpta=0
      i=1
      if(n>23){
        for(i in 1:n){
          if(i==0){
            rpta=rpta+m[i]*(1-ppois(24,p))
          }else if(i<12){
            rpta=rpta+m[i]*(1-ppois(i+12,p))
          }else{
            rpta=rpta+m[i]*(1-ppois(i,p))
          }
        }  
      }else{
        for(i in 1:n){
          if(i==0){
            rpta=rpta+m[i]*(1-ppois(12,p))
          }else{
            rpta=rpta+m[i]*(1-ppois(i,p))
          }
        }  
      }
      toString(rpta)
    })
    
    output$invProm <- renderText({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      estados = seq(from=0,to=n,by=1)
      model1 = matrix(0, nrow=n+1,ncol=n+1)
      h=input$CantidadLlenar
      for(i in estados){
        for(j in estados){
          if(j>=1 & i<=n-h & j<=i+h){
            model1[i+1,j+1]=dpois(h-j+i,p)
          }
          if(i<=n-h & j==0){
            model1[i+1,j+1]=1-ppois(h-1+i,p)
          }
          if(i>=1 & n-(h-1)<=i & i<=n & j<=i){
            model1[i+1,j+1]=dpois(i-j,p)
          }
          if(n-(h-1)<=i & i<=n & j==0){
            model1[i+1,j+1]=1-ppois(i-1,p)
          }
        }
      }
      if(n == 22){
        cad1 = new(Class="markovchain", states=f22, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 20){
        cad1 = new(Class="markovchain", states=f20, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 24){
        cad1 = new(Class="markovchain", states=f24, byrow = TRUE, transitionMatrix = model1)
      }else if(n == 25){
        cad1 = new(Class="markovchain", states=f25, byrow = TRUE, transitionMatrix = model1)
      }
      is.irreducible(cad1)
      steadyStates(cad1)
      m=steadyStates(cad1)
      esp = 0
      for(i in 1:n){
        esp = esp + m[i]*i
      }
      toString(esp)
    })
    
    output$invProm2 <- renderText({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      estados = seq(from=0,to=n,by=1)
      model2 = matrix(0, nrow=n+1,ncol=n+1)
      
      for(i in estados){
        for(j in estados){
          if(j>0){
            model2[i+1,j+1]=dpois(n-j,p)
          }else{
            model2[i+1,j+1]=1-ppois(n-1,p)
          }
        }
      }
      if(n == 22){
        cad2 = new(Class="markovchain", states=f22, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 20){
        cad2 = new(Class="markovchain", states=f20, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 24){
        cad2 = new(Class="markovchain", states=f24, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 25){
        cad2 = new(Class="markovchain", states=f25, byrow = TRUE, transitionMatrix = model2)
      }
      is.irreducible(cad2)
      steadyStates(cad2)
      m2=steadyStates(cad2)
      esp2 = 0
      for(i in 1:n){
        esp2 = esp2 + m2[i]*i
      }
      toString(esp2)
    })
    
    output$ProbEstadoEstable2 <- renderPlot({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      estados = seq(from=0,to=n,by=1)
      model2 = matrix(0, nrow=n+1,ncol=n+1)
      
      for(i in estados){
        for(j in estados){
          if(j>0){
            model2[i+1,j+1]=dpois(n-j,p)
          }else{
            model2[i+1,j+1]=1-ppois(n-1,p)
          }
        }
      }
      
      if(n == 22){
        cad2 = new(Class="markovchain", states=f22, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 20){
        cad2 = new(Class="markovchain", states=f20, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 24){
        cad2 = new(Class="markovchain", states=f24, byrow = TRUE, transitionMatrix = model2)
      }else if(n == 25){
        cad2 = new(Class="markovchain", states=f25, byrow = TRUE, transitionMatrix = model2)
      }
      is.irreducible(cad2)
      steadyStates(cad2)
      m2=steadyStates(cad2)
      
      barplot(m2, names.arg = estados, ylab="Probabilidad Asociada", xlab="Cantidad de rollos de papel en inventario al final de una semana", border = "red")
    })
    
    output$cTot<-renderText({
      h=input$CantidadLlenar
      s=0
      for(i in 1:n){
        s = s+(i-1)*m[i]
      }
      f=input$CostoInventarioUnidad
      g=f*s
      k=0
      r=n-h
      for(i in 1:(r+1)){
        k=k+m[i]*input$CostoPedido
      }
      l=k*191400
      costosTotales=l+g
      costosTotales=costosTotales*4
      costosAnuales=costosTotales*12
      toString(costosAnuales)
    })
    
    output$cTot2<-renderText({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      s=0
      for(i in 1:n){
        s = s+(i-1)*m2[i]
      }
      f=input$CostoInventarioUnidad
      g=f*s
      k=1-m2[n]*input$CostoPedido
      l=k*191400
      costosTotales=l+g
      costosTotales=costosTotales*4
      costosAnuales=costosTotales*12
      toString(costosAnuales)
    })
    
    output$cInv2<-renderText({
      s=0
      for(i in 1:n){
        s = s+(i-1)*m2[i]
      }
      f=input$CostoInventarioUnidad
      g=f*s
      toString(g)
    })
    
    output$cPed2<-renderText({
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      k=1-m2[n]*input$CostoPedido
      toString(k)
    })
    
    output$cFaltantes2<-renderText({
      if(input$TipoPapel=="Papel Bond"){
        p=8.93
      }else if(input$TipoPapel=="Papel Propalcote"){
        p=7.88
      }else if(input$TipoPapel=="Papel Químico"){
        p=10.69
      }else if(input$TipoPapel=="Papel Periódico"){
        p=9.93
      }else{
        p=11.05
      }
      if(input$TipoPapel=="Papel Bond"){
        n=22
      }else if(input$TipoPapel=="Papel Propalcote"){
        n=20
      }else if(input$TipoPapel=="Papel Químico"){
        n=25
      }else if(input$TipoPapel=="Papel Periódico"){
        n=22
      }else{
        n=24
      }
      rpta=0
      i=1
      if(n>23){
        for(i in 1:n){
          if(i==0){
            rpta=rpta+m2[i]*(1-ppois(24,p))
          }else if(i<12){
            rpta=rpta+m2[i]*(1-ppois(i+12,p))
          }else{
            rpta=rpta+m2[i]*(1-ppois(i,p))
          }
        }  
      }else{
        for(i in 1:n){
          if(i==0){
            rpta=rpta+m2[i]*(1-ppois(12,p))
          }else{
            rpta=rpta+m2[i]*(1-ppois(i,p))
          }
        }  
      }
      toString(rpta)
    })
    
    output$tiempoprom <-renderText({
      
      a=input$TasaA
      b=input$TasaB
      c=input$TasaC
      d=input$TasaD
      
      estados = as.character(1:36)
      matrizcad = matrix(0, nrow=36,ncol=36, dimnames = list(estados,estados))
      
      for(i in 1:36){
        for(j in 1:36){
          if(i %% 4 != 0 & j == i+1){
            matrizcad[i,j]=a
          }
          if(i>=2 & i <=8 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=14 & i <=20 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=26 & i <=32 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>= 5 & i <= 8 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 17 & i <= 20 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 9 & i <= 12 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>= 21 & i <= 24 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>=13 & j == i-12){
            matrizcad[i,j]=d
          }
          if(i==1 & j==i){
            matrizcad[i,j]=-a
          }
          if(i>=2 & i<=3 & j==i){
            matrizcad[i,j]=-(a+b)
          }
          if(i==4 & j==i){
            matrizcad[i,j]=-b
          }
          if(i==5 & j==i){
            matrizcad[i,j]=-(a+c)
          }
          if(i>=6 & i <=7 & j==i){
            matrizcad[i,j]=-(a+b+c)
          }
          if(i==8 & j==i){
            matrizcad[i,j]=-(b+c)
          }
          if(i>= 9 & i<= 11 & j==i){
            matrizcad[i,j]=-(a+2*c)
          }
          if(i==12 & j==i){
            matrizcad[i,j]=-2*c
          }
          if(i==17 & j==i ){
            matrizcad[i,j]=-(a+c+d)
          }
          if(i==18 & j==i | i==19 & j==i){
            matrizcad[i,j]=-(a+b+c+d)
          }
          if(i==16 & j==i | i==20 & j==i ){
            matrizcad[i,j]=-(b+c+d)
          }
          if(i>= 21 & i<=23 & j==i){
            matrizcad[i,j]=-(a+2*c+d)
          }
          if(i==24 & j==i){
            matrizcad[i,j]=-(2*c+d)
          }
          if(i==13 & j==i |i==25 & j==i |i==29 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i>=33 & i<=35 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i==14 & j==i |i==15 & j==i |i==26 & j==i |i==27 & j==i |i==30 & j==i |i==31 & j==i ){
            matrizcad[i,j]=-(a+b+d)
          }
          if(i==16 & j==i|i==28 & j==i |i==32 & j==i ){
            matrizcad[i,j]=-(b+d)
          }
          if(i==36 & j==i ){
            matrizcad[i,j]=-d
          }
        }
      }
      
      #=====markov=====
      
      cad = new("ctmc", states=estados, byrow = TRUE, generator = matrizcad, name = "Cadena de markov")
      is.CTMCirreducible(cad)
      steadyStates(cad)
      pi= c(steadyStates(cad))
      
      tefectivo= 0.05*(1-sum(pi[c(4,8,12,16,20,24,32,36)]))
      tefectivo
      rollosPromedio = 1*(pi[2]+pi[5]+pi[13])+2*(pi[3]+pi[6]+pi[9]+pi[14]+pi[17]+pi[25])+3*(pi[4]+pi[7]+pi[10]+pi[15]+pi[18]+pi[21]+pi[26]+pi[29])+4*(pi[8]+pi[11]+pi[16]+pi[19]+pi[22]+pi[27]+pi[30]+pi[33])+5*(pi[12]+pi[20]+pi[23]+pi[28]+pi[31]+pi[34])+6*(pi[24]+pi[32]+pi[35])+7*pi[36]
      rollosPromedio
      tiemposistema = rollosPromedio/tefectivo
      tiemposistema
      toString(paste("Un rollo permanece en promedio ",tiemposistema," segundos en el area de produccion."))
      
    })
    
    output$nveces <-renderText({
      
      alpha = c()
      
      a=input$TasaA
      b=input$TasaB
      c=input$TasaC
      d=input$TasaD
      
      estados = as.character(1:36)
      matrizcad = matrix(0, nrow=36,ncol=36, dimnames = list(estados,estados))
      
      for(i in 1:36){
        for(j in 1:36){
          if(i %% 4 != 0 & j == i+1){
            matrizcad[i,j]=a
          }
          if(i>=2 & i <=8 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=14 & i <=20 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=26 & i <=32 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>= 5 & i <= 8 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 17 & i <= 20 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 9 & i <= 12 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>= 21 & i <= 24 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>=13 & j == i-12){
            matrizcad[i,j]=d
          }
          if(i==1 & j==i){
            matrizcad[i,j]=-a
          }
          if(i>=2 & i<=3 & j==i){
            matrizcad[i,j]=-(a+b)
          }
          if(i==4 & j==i){
            matrizcad[i,j]=-b
          }
          if(i==5 & j==i){
            matrizcad[i,j]=-(a+c)
          }
          if(i>=6 & i <=7 & j==i){
            matrizcad[i,j]=-(a+b+c)
          }
          if(i==8 & j==i){
            matrizcad[i,j]=-(b+c)
          }
          if(i>= 9 & i<= 11 & j==i){
            matrizcad[i,j]=-(a+2*c)
          }
          if(i==12 & j==i){
            matrizcad[i,j]=-2*c
          }
          if(i==17 & j==i ){
            matrizcad[i,j]=-(a+c+d)
          }
          if(i==18 & j==i | i==19 & j==i){
            matrizcad[i,j]=-(a+b+c+d)
          }
          if(i==16 & j==i | i==20 & j==i ){
            matrizcad[i,j]=-(b+c+d)
          }
          if(i>= 21 & i<=23 & j==i){
            matrizcad[i,j]=-(a+2*c+d)
          }
          if(i==24 & j==i){
            matrizcad[i,j]=-(2*c+d)
          }
          if(i==13 & j==i |i==25 & j==i |i==29 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i>=33 & i<=35 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i==14 & j==i |i==15 & j==i |i==26 & j==i |i==27 & j==i |i==30 & j==i |i==31 & j==i ){
            matrizcad[i,j]=-(a+b+d)
          }
          if(i==16 & j==i|i==28 & j==i |i==32 & j==i ){
            matrizcad[i,j]=-(b+d)
          }
          if(i==36 & j==i ){
            matrizcad[i,j]=-d
          }
        }
      }
      
      estadoactual = 0
      
      #=====Embebida======
      
      embebida = generatorToTransitionMatrix(matrizcad)
      
      for(i in 1:36){
        if(input$estado1 == estadossl[i]){
          estadoactual = i
        }
      }
      
      for(i in 1:36){
        if(estadoactual == i){
          alpha = c(alpha, 1)
        }
        else{
          alpha = c(alpha, 0)
        }
      }
      
      L=matrix(0,36,1)
      for(i in 1:36){
        for(j in 1:36){
          if(i==j){
            L[i,1]=-1/matrizcad[i,j]
          }
        } 
      }
      L
      
      M=matrix(0,36,36)
      TK=0
      for(i in 0:4662){
        M=M+(embebida^i)
      }
      TK=alpha%*%M%*%L
      
      TK
      as.numeric(TK)
      
      n=5000
      TK[1]=0
      STOP=FALSE
      
      M=matrix(0,36,36)
      for (i in seq(0,n)) 
      {
        M=M+(embebida%^%i)
        pasos=i
        TK=alpha%*%M%*%L
        
        if (TK > 25200) {
          break
        }
      }
      M=matrix(0,36,36)
      for(i in seq(0,pasos-1)){
        M=M+(embebida%^%i)
      }
      TK=alpha%*%M%*%L
      
      P=M%^%0
      
      k = pasos - 1
      M=matrix(0,36,36)
      for(i in 0:k){
        M=M+(embebida%^%i)
      }
      
      toString( paste("Los buffers estarán llenos ", round(M[estadoactual,36],4)," veces desde el estado ", input$estado1, "."))
    })
    
    output$tiempollenos <-renderText({
      a=input$TasaA
      b=input$TasaB
      c=input$TasaC
      d=input$TasaD
      
      estados = as.character(1:36)
      matrizcad = matrix(0, nrow=36,ncol=36, dimnames = list(estados,estados))
      
      for(i in 1:36){
        for(j in 1:36){
          if(i %% 4 != 0 & j == i+1){
            matrizcad[i,j]=a
          }
          if(i>=2 & i <=8 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=14 & i <=20 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>=26 & i <=32 & i != 17 & i != 5 & i != 29 & j==i+3){
            matrizcad[i,j]=b
          }
          if(i>= 5 & i <= 8 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 17 & i <= 20 & j==i+8){
            matrizcad[i,j]=c
          }
          if(i>= 9 & i <= 12 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>= 21 & i <= 24 & j==i+8){
            matrizcad[i,j]=2*c
          }
          if(i>=13 & j == i-12){
            matrizcad[i,j]=d
          }
          if(i==1 & j==i){
            matrizcad[i,j]=-a
          }
          if(i>=2 & i<=3 & j==i){
            matrizcad[i,j]=-(a+b)
          }
          if(i==4 & j==i){
            matrizcad[i,j]=-b
          }
          if(i==5 & j==i){
            matrizcad[i,j]=-(a+c)
          }
          if(i>=6 & i <=7 & j==i){
            matrizcad[i,j]=-(a+b+c)
          }
          if(i==8 & j==i){
            matrizcad[i,j]=-(b+c)
          }
          if(i>= 9 & i<= 11 & j==i){
            matrizcad[i,j]=-(a+2*c)
          }
          if(i==12 & j==i){
            matrizcad[i,j]=-2*c
          }
          if(i==17 & j==i ){
            matrizcad[i,j]=-(a+c+d)
          }
          if(i==18 & j==i | i==19 & j==i){
            matrizcad[i,j]=-(a+b+c+d)
          }
          if(i==16 & j==i | i==20 & j==i ){
            matrizcad[i,j]=-(b+c+d)
          }
          if(i>= 21 & i<=23 & j==i){
            matrizcad[i,j]=-(a+2*c+d)
          }
          if(i==24 & j==i){
            matrizcad[i,j]=-(2*c+d)
          }
          if(i==13 & j==i |i==25 & j==i |i==29 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i>=33 & i<=35 & j==i ){
            matrizcad[i,j]=-(a+d)
          }
          if(i==14 & j==i |i==15 & j==i |i==26 & j==i |i==27 & j==i |i==30 & j==i |i==31 & j==i ){
            matrizcad[i,j]=-(a+b+d)
          }
          if(i==16 & j==i|i==28 & j==i |i==32 & j==i ){
            matrizcad[i,j]=-(b+d)
          }
          if(i==36 & j==i ){
            matrizcad[i,j]=-d
          }
        }
      }
      
      #=====markov=====
      
      cad = new("ctmc", states=estados, byrow = TRUE, generator = matrizcad, name = "Cadena de markov")
      
      for(i in 1:36){
        if(input$estado2 == estadossl[i]){
          estadoactual2 = i
        }
      }
      toString(paste("El tiempo promedio antes de que los buffers se llenen completamente, iniciando en el estado ", input$estado2, ",", " es de ", round(ExpectedTime(cad,estadoactual2,36,TRUE),4), " segundos."))
    })
    
    #--------Fase 3
    
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
  
