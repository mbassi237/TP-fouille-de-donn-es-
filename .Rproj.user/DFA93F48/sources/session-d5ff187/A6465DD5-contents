#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(arules)
library(arulesViz)
library(shinythemes)
# Define UI for application that draws a histogram

  ui <- fluidPage(
    shinythemes::themeSelector(),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        wellPanel(
          fileInput('file' , h3('rechercher le fichier')),
          #textInput("file" , h3("entre le fichier") , value = ""),
          actionButton("afficher" , "Valider"),
          br(),
          br(),
          br(),
          br(),
          sliderInput("slider2" , label = "taille des bandes :" , min = 0 , max = 50 , value = 25)
        )
        #textInput("file" , h3("fichier en entree") , value = "entrez le fichier")
      ),
      mainPanel(
        #width = 8,
        fixed = "TRUE",
        tabsetPanel(
          tabPanel(
            'data',
            DTOutput('data_sortie'),
            downloadButton('save_data' , 'save to CSV')
          ),
          
          tabPanel(
            'histogramme',
            plotOutput('histogramme_sortie')
          ),
          
          tabPanel(
            'transactions',
            verbatimTextOutput('transaction_sortie')
          ),
          
          tabPanel(
            'rules',
            DTOutput('regle_sortie')
          ),
          
          tabPanel(
            'rulesVisual',
            plotOutput('graphique')
          ),
          
          tabPanel(
            'statistiques',
            verbatimTextOutput('summary')
          )
        ),
        theme = shinytheme("cosmo")
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df<-reactive({
    ha<-read.csv("groceries.csv", header=T , dec = '.' , sep = ',')
  })
  
  observeEvent(input$afficher , {
    output$data_sortie<-renderDT(df())
  })
  
  output$save_data<-downloadHandler(
    filename <- function(){
      #pour gerer les parametres de la sauvegarde
      paste("data_" , Sys.Date() , ".csv" , sep = ',')
    },
    content <- function(file){
      write.csv(df() , file)
    }
  )
  
  output$summary<-renderPrint({
    summary(groceries)
  })
  
  trans<-as(groceries , "transactions")
  transactions<-function() {
    inspect(
      trans
    )
  }
  
  output$transaction_sortie<-renderPrint({
    transactions()
  })
  
  modele <- apriori(data = groceries, parameter = list(support = 0.05, confidence = 0.8 , target= "rules"))
  renderTable<-function() {
    inspect(
      modele
    )
  }
  
    output$histogramme_sortie<-renderPlot({
      itemFrequencyPlot(trans , support=0.1 , cex.names=0.8 , main="itemsets frequents" , col="skyblue")
    })
  
  graphe<-function() {
    plot(modele)
  }
  
  output$graphique<-renderPlot({
    graphe()
  })
  
  output$regle_sortie<-renderDT({
    renderTable()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
