
# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(devtools)
library(ggplot2)
library(lubridate)



# Preparation des données -------------------------------------------------


library(readr)
bilan_electrique_transpose <- read_delim("C:/Users/Camille/Documents/Projet_shiny/bilan-electrique-transpose.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)

# renommer les variables
colnames(bilan_electrique_transpose) <- c("jour","categorie_client","puissance")
bilan <- bilan_electrique_transpose[order(bilan_electrique_transpose$jour),]
bilan$categorie_client=as.factor(bilan$categorie_client)

# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
    'Impact du covid sur les consommations électriques',
    
    tabPanel('Mon département',
             id = 'departements',
             
             
             
             sidebarLayout(
                 sidebarPanel(
                     
                     selectInput("segment",
                                 "Choisissez votre segment:",
                                 choices = levels(bilan$categorie_client),
                                 selected = 'Doubs'),
                     sliderInput("obs",
                                 "periode de temps:",
                                 min = as.Date("2020-03-17","%Y-%m-%d"),
                                 max = as.Date("2020-11-20","%Y-%m-%d"),
                                 value=as.Date("2020-03-21"),timeFormat="%Y-%m-%d"),
                     valueBoxOutput("progressBox")
                 ),
                 
                 
                 mainPanel(
                     ##affichage du nom du departement
                     h3(textOutput('nom_segment')),
                     
                     
                     ####TODO: remplacer par la table par un datatable 
                     plotOutput('courbe_realise'),
                     tableOutput('ma_table'),
                     downloadLink('downloadData', 'telecharger')
                     
                     
                     
                 )
             )
             
    )
)



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$nom_segment <- renderText({
        input$segment
    })
    
    
    filtre <- reactive({
        bilan %>% 
            filter(categorie_client == input$segment) %>%
            filter(jour < input$obs)
    })
    
    get_somme <- reactive({
        
        recupere_donnée <- bilan %>% 
            filter(categorie_client == input$segment) %>%
            filter(jour < input$obs)
        somme <- sum(recupere_donnée$puissance)
        
        somme
        
        
    })
    
    output$courbe_realise <- renderPlot({
        
        df <- filtre() %>%
            select(jour, puissance)%>%
            tidyr::pivot_longer(-c("jour"))%>%
            filter(jour > as.Date ("2020-03-15"))
        
        
        fig= ggplot(df) +
            aes(y  = value, x = jour, color = name)+
            geom_line()
        
        fig
        
    })
    output$ma_table <- renderTable({
        out <-  filtre()%>%
            filter(jour > as.Date ("2020-03-15"))
        print(out)
        out
    } )
    
    
    
    output$progressBox <- renderValueBox({
        valueBox("somme consommation reelle", get_somme()
        )
    })
    
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep=";")
        },
        content = function(file) {
            write.csv(filtre(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
