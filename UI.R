
shinyUI(fluidPage(
########### title 입력#############
   titlePanel(h1("마킹 MAP")),
 
########### side 입력창#############
  sidebarLayout(
    sidebarPanel(
      h2("Data Input"),
      textInput("LOT", "", "입력하세요"),
     p("※Plz input Extention or Coating Lot", style = "color:blue"),
     p("   ex) 20170221CC03009"),
     
     fluidRow(
       column(7,
     checkboxGroupInput("마킹", 
                        label = h5(strong("Marking Choice")), 
                        choices = list("마킹有" = "Y","마킹無" = "N"),selected = "Y"
                        )),
       column(5,
     checkboxGroupInput("강약", 
                        label = h5(strong("강약불량")), 
                        choices = list("강불량" = "강","약불량" = "강"),selected = "강"
     ))
         ),
    
    numericInput("소자폭", h5("소자폭"), 1300 ),
    numericInput("원단길이", h5("원단길이"), 1240 ),
    numericInput("모델TD", h5("모델 TD mm"), 200 ),
    numericInput("모델MD", "모델 MD mm", 300 ),
    numericInput("재단열", "재단열", 6 ),
    numericInput("nulling", "nulling mm", 50 )
    
    
    
    )
    
    ,
  
########### main 입력창#############
    
  mainPanel(
    h3("자동검사 Map",align = "center"),
    plotOutput("MAP"),  
    h3("재단마킹수율",align = "center"),
    tableOutput("yield"),
    h5("by.choichoe",align="right")
    
    )
)
)
)


