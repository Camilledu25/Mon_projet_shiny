# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(devtools)
library(ggplot2)
library(lubridate)
library(caret)
library(plotly)


# Preparation des données -------------------------------------------------


library(readr)
bilan_electrique_transpose <- readRDS('data/conso_data.rds')
temp <- readRDS('data2/temperatures2.rds')

# renommer les variables conso

colnames(bilan_electrique_transpose) <- c("jour","categorie_client","puissance")
bilan <- bilan_electrique_transpose[order(bilan_electrique_transpose$jour),]
bilan$categorie_client=as.factor(bilan$categorie_client)

# formater les variables de puissance

bilan<-bilan%>%filter(jour>=as.Date("2015-12-18"))

bilanR<-bilan%>%filter(categorie_client=="Résidentiels")%>%select(-categorie_client)
colnames(bilanR) <- c("jour","puiss_residentiels")
bilanP<-bilan%>%filter(categorie_client=="Professionnels")%>%select(-categorie_client)
colnames(bilanP) <- c("jour","puiss_professionnels")
bilanE<-bilan%>%filter(categorie_client=="Entreprises")%>%select(-categorie_client)
colnames(bilanE) <- c("jour","puiss_entreprises")
bilanPME<-bilan%>%filter(categorie_client=="PME / PMI")%>%select(-categorie_client)
colnames(bilanPME) <- c("jour","puiss_pme.pmi")

join<- left_join(bilanR, bilanP)
join<- left_join(join, bilanE)
join<- left_join(join, bilanPME)

bilanFinal<-join

##### variable explicatives 
temp$Horodate<-as.Date(temp$Horodate, format = "%d/%m/%Y")
temp$heure<-as.character(temp$heure)
temp<-temp%>%filter(heure==as.character("12:00:00"))
temperature_ord <- temp[order(temp$Horodate),]


temperature_ord<-temperature_ord%>%filter(Horodate>=as.Date("2015-12-18"))
temperature_ord<-temperature_ord%>%filter(Horodate<=as.Date("2020-11-20"))
temperature_ord<-temperature_ord%>%select(Horodate,temperature)
colnames(temperature_ord) <- c("jour","temperature")

# Joidre table bilan et temperature

bilan_final1<- left_join(bilanFinal, temperature_ord)

# joindre les 3 autres variables explicatives

temperature_jfer <- mutate(bilan_final1,
                           jour_ferier = case_when(
                               day(jour) == 25 & month(jour)==12 ~ 1,
                               day(jour) == 31 & month(jour)==12 ~ 1,
                               day(jour) == 11 & month(jour)==11 ~ 1,
                               day(jour) == 14 & month(jour)==07 ~ 1,
                               day(jour) == 01 & month(jour)==01 ~ 1,
                               day(jour) == 15 & month(jour)==08 ~ 1,
                               day(jour) == 08 & month(jour)==05 ~ 1,
                               day(jour) == 01 & month(jour)==05 ~ 1,
                               day(jour) == 01 & month(jour)==11 ~ 1,
                               jour==as.Date("2016-05-16")~1,
                               jour==as.Date("2016-05-15")~1,
                               jour==as.Date("2016-05-05")~1,
                               jour==as.Date("2016-03-28")~1,
                               jour==as.Date("2016-03-27")~1,
                               jour==as.Date("2015-05-25")~1,
                               jour==as.Date("2015-05-24")~1,
                               jour==as.Date("2015-05-14")~1,
                               jour==as.Date("2015-04-06")~1,
                               jour==as.Date("2015-04-05")~1,
                               jour==as.Date("2017-06-05")~1,
                               jour==as.Date("2017-06-04")~1,
                               jour==as.Date("2017-05-25")~1,
                               jour==as.Date("2017-04-17")~1,
                               jour==as.Date("2017-04-16")~1,
                               jour==as.Date("2018-05-21")~1,
                               jour==as.Date("2018-05-20")~1,
                               jour==as.Date("2018-05-10")~1,
                               jour==as.Date("2018-04-02")~1,
                               jour==as.Date("2018-04-01")~1,
                               jour==as.Date("2019-06-10")~1,
                               jour==as.Date("2019-06-09")~1,
                               jour==as.Date("2019-05-30")~1,
                               jour==as.Date("2019-04-22")~1,
                               jour==as.Date("2019-04-21")~1,
                               jour==as.Date("2020-06-01")~1,
                               jour==as.Date("2020-05-31")~1,
                               jour==as.Date("2020-05-21")~1,
                               jour==as.Date("2020-04-13")~1,
                               jour==as.Date("2020-04-12")~1,
                               TRUE~0))

temp_jfer_pos <- mutate(temperature_jfer,
                        position_semaine = case_when(
                            wday(jour) >= 2 & wday(jour)<=6 ~ "de lundi a vendredi",
                            wday(jour) == 7 | wday(jour)==1 ~ "week-end"))

temp_jfer_pos_vac <- mutate(temp_jfer_pos,
                            vacance = case_when(
                                jour>=as.Date("2015-12-19") & jour <=as.Date("2016-01-04") ~1,
                                jour>=as.Date("2016-02-06") & jour <=as.Date("2016-02-13") ~1/3,
                                jour>as.Date("2016-02-13") & jour <=as.Date("2016-02-22") ~2/3,
                                jour>as.Date("2016-02-22") & jour <=as.Date("2016-02-29") ~2/3,
                                jour>as.Date("2016-02-29") & jour <=as.Date("2016-03-07") ~1/3,
                                jour>=as.Date("2016-04-02") & jour <=as.Date("2016-04-09") ~1/3,
                                jour>as.Date("2016-04-09") & jour <=as.Date("2016-04-16") ~2/3,
                                jour>as.Date("2016-04-16") & jour <=as.Date("2016-04-25") ~2/3,
                                jour>as.Date("2016-04-25") & jour <=as.Date("2016-05-02") ~1/3,
                                jour>as.Date("2016-07-05") & jour <=as.Date("2016-09-01") ~1,
                                jour>as.Date("2016-10-19") & jour <=as.Date("2016-11-03") ~1,
                                jour>as.Date("2016-12-17") & jour <=as.Date("2017-01-03") ~1,
                                jour>as.Date("2017-02-04") & jour <=as.Date("2017-02-11") ~1/3,
                                jour>as.Date("2017-02-11") & jour <=as.Date("2017-02-18") ~2/3,
                                jour>as.Date("2017-02-18") & jour <=as.Date("2017-02-27") ~2/3,
                                jour>as.Date("2017-02-27") & jour <=as.Date("2017-03-06") ~1/3,
                                jour>=as.Date("2017-04-01") & jour <=as.Date("2017-04-08") ~1/3,
                                jour>as.Date("2017-04-08") & jour <=as.Date("2017-04-24") ~2/3,
                                jour>as.Date("2017-04-24") & jour <=as.Date("2017-05-02") ~1/3,
                                jour>as.Date("2017-07-08") & jour <=as.Date("2017-09-04") ~1,
                                jour>as.Date("2017-10-21") & jour <=as.Date("2017-11-06") ~1,
                                jour>as.Date("2017-12-23") & jour <=as.Date("2018-01-08") ~1,
                                jour>as.Date("2018-02-10") & jour <=as.Date("2018-02-17") ~1/3,
                                jour>as.Date("2018-02-17") & jour <=as.Date("2018-03-05") ~2/3,
                                jour>as.Date("2018-03-05") & jour <=as.Date("2018-03-12") ~1/3,
                                jour>as.Date("2018-04-07") & jour <=as.Date("2018-04-14") ~1/3,
                                jour>as.Date("2018-04-14") & jour <=as.Date("2018-04-30") ~2/3,
                                jour>as.Date("2018-04-30") & jour <=as.Date("2018-05-07") ~1/3,
                                jour>as.Date("2018-07-07") & jour <=as.Date("2018-09-03") ~1,
                                jour>as.Date("2018-10-20") & jour <=as.Date("2018-11-05") ~1,
                                jour>as.Date("2018-12-22") & jour <=as.Date("2019-01-07") ~1,
                                jour>as.Date("2019-02-09") & jour <=as.Date("2019-02-16") ~1/3,
                                jour>as.Date("2019-02-16") & jour <=as.Date("2019-03-04") ~2/3,
                                jour>as.Date("2019-03-04") & jour <=as.Date("2019-03-11") ~1/3,
                                jour>as.Date("2019-04-06") & jour <=as.Date("2019-04-13") ~1/3,
                                jour>as.Date("2019-04-13") & jour <=as.Date("2019-04-29") ~2/3,
                                jour>as.Date("2019-04-29") & jour <=as.Date("2019-05-06") ~1/3,
                                jour>as.Date("2019-07-06") & jour <=as.Date("2019-09-02") ~1,
                                jour>as.Date("2019-10-19") & jour <=as.Date("2019-11-04") ~1,
                                jour>as.Date("2019-12-21") & jour <=as.Date("2020-01-06") ~1,
                                jour>as.Date("2020-02-08") & jour <=as.Date("2020-02-15") ~1/3,
                                jour>as.Date("2020-02-15") & jour <=as.Date("2020-03-02") ~2/3,
                                jour>as.Date("2020-03-02") & jour <=as.Date("2020-03-09") ~1/3,
                                jour>as.Date("2020-04-04") & jour <=as.Date("2020-04-11") ~1/3,
                                jour>as.Date("2020-04-11") & jour <=as.Date("2020-04-27") ~2/3,
                                jour>as.Date("2020-04-27") & jour <=as.Date("2020-05-04") ~1/3,
                                jour>as.Date("2020-07-04") & jour <=as.Date("2020-09-01") ~1,
                                jour>as.Date("2020-10-02") & jour <=as.Date("2020-11-02") ~1,
                                TRUE~0))

temp_jfer_pos_vac$vacance<-as.factor(temp_jfer_pos_vac$vacance)
temp_jfer_pos_vac$jour_ferier<-as.factor(temp_jfer_pos_vac$jour_ferier)
temp_jfer_pos_vac$position_semaine<-as.factor(temp_jfer_pos_vac$position_semaine)

bilan_realise<-temp_jfer_pos_vac

# modele knn residentielle 

set.seed(1234)
ctrl<-trainControl(method="cv",number=5)
param=expand.grid(k=seq(2,100,by=1))
model_knn<-train(puiss_residentiels~.-puiss_professionnels-puiss_entreprises-puiss_pme.pmi-jour,data=temp_jfer_pos_vac,method='knn',trControl=ctrl,
                 preProcess=c("center","scale"),tuneGrid=param)
model_knn
a=predict(model_knn)
v<-data.frame(a)
colnames(v) <- c("pred_puiss_residentiels")
join2<-cbind(temp_jfer_pos_vac, v)
plot(puiss_residentiels ~jour,data=join2,type="l",col="blue")
par(new=TRUE)
plot(pred_puiss_residentiels~jour,data=join2,type="l",col="red")

# modele knn entreprise

set.seed(1234)
ctrl<-trainControl(method="cv",number=5)
param=expand.grid(k=seq(2,100,by=1))
model_knn<-train(puiss_entreprises~.-puiss_professionnels-puiss_residentiels-puiss_pme.pmi-jour-pred_puiss_residentiels,data=join2,method='knn',trControl=ctrl,
                 preProcess=c("center","scale"),tuneGrid=param)
model_knn
b=predict(model_knn)
w<-data.frame(b)
colnames(w) <- c("pred_puiss_entreprises")
join2<-cbind(join2, w)
plot(puiss_entreprises ~jour,data=join2[1:100,],type="l",col="blue")
par(new=TRUE)
plot(pred_puiss_entreprises~jour,data=join2[1:100,],type="l",col="red")

# modele knn professionnelle

set.seed(1234)
ctrl<-trainControl(method="cv",number=5)
param=expand.grid(k=seq(2,100,by=1))
model_knn<-train(puiss_professionnels~.-puiss_entreprises-puiss_residentiels-puiss_pme.pmi-jour-pred_puiss_residentiels-pred_puiss_entreprises,data=join2,method='knn',trControl=ctrl,
                 preProcess=c("center","scale"),tuneGrid=param)
model_knn
c=predict(model_knn)
h<-data.frame(c)
colnames(h) <- c("pred_puiss_professionnels")
join2<-cbind(join2, h)
plot(puiss_professionnels ~jour,data=join2[1:100,],type="l",col="blue")
par(new=TRUE)
plot(pred_puiss_professionnels~jour,data=join2[1:100,],type="l",col="red")

# modele knn pmepmi

set.seed(1234)
ctrl<-trainControl(method="cv",number=5)
param=expand.grid(k=seq(2,100,by=1))
model_knn<-train(puiss_pme.pmi~.-puiss_entreprises-puiss_residentiels-puiss_professionnels-jour-pred_puiss_residentiels-pred_puiss_entreprises-pred_puiss_professionnels,data=join2,method='knn',trControl=ctrl,
                 preProcess=c("center","scale"),tuneGrid=param)
model_knn
e=predict(model_knn)
g<-data.frame(e)
colnames(g) <- c("pred_puiss_pme.pmi")
join2<-cbind(join2, g)

join2<-mutate(join2,ecart_pme.pmi=abs(pred_puiss_pme.pmi-puiss_pme.pmi))
join2<-mutate(join2,ecart_professionnels=abs(pred_puiss_professionnels-puiss_professionnels))
join2<-mutate(join2,ecart_entreprises=abs(pred_puiss_entreprises-puiss_entreprises))
join2<-mutate(join2,ecart_residentiels=abs(pred_puiss_residentiels-puiss_residentiels))

finale<-join2
finale<-finale%>%filter(jour>=as.Date("2020-03-17"))

# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
    'Impact du covid sur les consommations électriques',
    
    tabPanel('Etude',
             id = 'departements',
             
             
             
             sidebarLayout(
                 sidebarPanel(
                     
                     selectInput("segment",
                                 "Choisissez votre segment:",
                                 choices = c("entreprises","professionnels","residentiels","pme.pmi"),
                                 multiple = TRUE,
                                 selected = 'entreprises'),
                     selectInput("obs",
                                 "date de début:",
                                 choices = sort(unique(finale$jour)),
                                 selected = as.Date("2020-03-17",timeFormat="%Y-%m-%d")),
                     selectInput("obs1",
                                 "date finale:",
                                 choices = sort(unique(finale$jour)),
                                 selected = as.Date("2020-08-17",timeFormat="%Y-%m-%d"))
                     # sliderInput("obs1",
                     #             "date finale:",
                     #             min = as.Date("2020-03-17","%Y-%m-%d"),
                     #             max = as.Date("2020-11-20","%Y-%m-%d"),
                     #             value=as.Date("2020-04-21"),timeFormat="%Y-%m-%d"),
                     
                 ),
                 
                 
                 mainPanel(
                     ##affichage du nom du departement
                     h3(textOutput('nom_segment')),
                     
                     
                     ####TODO: remplacer par la table par un datatable 
                     plotlyOutput('courbe_realise_mod'),
                     valueBoxOutput("sommered"),
                     valueBoxOutput("sommepred"),
                     valueBoxOutput("diff"),
                     valueBoxOutput("diffp"),
                     tableOutput('ma_table2'),
                     
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
        finale %>% 
            select(jour,paste0('puiss_',
                               input$segment),paste0('pred_puiss_',
                                                     input$segment),paste0('ecart_',
                                                                           input$segment))%>%
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)
    })
    
    get_somme <- reactive({
        
        recupere_donnee<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('puiss_',
                          input$segment))
        
        somme <- sum(recupere_donnee)
        
        somme
        
        
    })
    
    get_somme_pr <- reactive({
        
        recupere_donnee1<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('pred_puiss_',
                          input$segment))
        
        somme1 <- sum(recupere_donnee1)
        
        somme1
        
        
    })
    get_somme_di <- reactive({
        
        recupere_donnee2<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('ecart_',
                          input$segment))
        
        somme2 <- sum(recupere_donnee2)
        
        somme2
        
        
    })
    
    get_somme_di_p <- reactive({
        
        recupere_donnee<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('puiss_',
                          input$segment))
        
        somme <- sum(recupere_donnee)
        
        
        recupere_donnee2<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('ecart_',
                          input$segment))
        
        somme2 <- sum(recupere_donnee2)
        
        somme3<-somme2*100/somme
        somme3
        
        
    })
    
    
    output$courbe_realise_mod <- renderPlotly({
        
        df1 <- filtre() %>%
            select(jour, contains('pred_puiss_'),contains('puiss_'))%>%
            tidyr::pivot_longer(-c("jour"))
        
        
        
        
        fig= ggplot(df1) +
            aes(y  = value, x = jour, color = name)+
            geom_line()+
            theme_bw()+
            theme(legend.position = 'bottom')
        
        
        ggplotly(fig)
        
    })
    
    output$ma_table2 <- renderTable({
        out <-  filtre()
        out$jour<-as.character(out$jour)
        
        print(out)
        out
    } )
    
    
    output$sommered <- renderValueBox({
        valueBox("Somme de la consommation reelle", get_somme()
        )
    })
    output$sommepred <- renderValueBox({
        
        valueBox("Somme de la consommation prédite", get_somme_pr()
        )
    })
    
    output$diff <- renderValueBox({
        valueBox("Somme de l'ecart entre prédiction et realisation", get_somme_di()
        )
    })
    
    output$diffp <- renderValueBox({
        valueBox("Pourcentage de l'ecart", get_somme_di_p()
        )
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(filtre(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
