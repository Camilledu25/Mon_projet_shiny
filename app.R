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
library(reactable)

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

finale2<-finale%>%mutate(conss_pme.pmi=puiss_pme.pmi*24/1000000000)%>%
    mutate(conss_professionnels=puiss_professionnels*24/1000000000)%>%
    mutate(conss_entreprises=puiss_entreprises*24/1000000000)%>%
    mutate(conss_residentiels=puiss_residentiels*24/1000000000)%>%
    mutate(pred_conss_pme.pmi=pred_puiss_pme.pmi*24/1000000000)%>%
    mutate(pred_conss_professionnels=pred_puiss_professionnels*24/1000000000)%>%
    mutate(pred_conss_residentiels=pred_puiss_residentiels*24/1000000000)%>%
    mutate(pred_conss_entreprises=pred_puiss_entreprises*24/1000000000)

finale3<-finale2%>%select(-puiss_pme.pmi,-puiss_professionnels,-puiss_entreprises,-puiss_residentiels,-pred_puiss_pme.pmi,-pred_puiss_professionnels,-pred_puiss_residentiels,-pred_puiss_entreprises)
finale4<-finale3%>%select(-ecart_pme.pmi,-ecart_professionnels,-ecart_entreprises,-ecart_residentiels)

join3<-mutate(finale4,ecart_pme.pmi=conss_pme.pmi-pred_conss_pme.pmi)
join3<-mutate(join3,ecart_professionnels=conss_professionnels-pred_conss_professionnels)
join3<-mutate(join3,ecart_entreprises=conss_entreprises-pred_conss_entreprises)
join3<-mutate(join3,ecart_residentiels=conss_residentiels-pred_conss_residentiels)

finale<-join3

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(
        title="Impact du covid"
    ),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Etude des données Gwh", tabName = "departements")
        ),

             
                 selectInput("segment",
                                 "Choisissez votre segment:",
                                 choices = c("entreprises","professionnels","residentiels","pme.pmi"),
                                 multiple = TRUE,
                                 selected = 'professionnels'),
                     selectInput("obs",
                                 "date de début:",
                                 choices = sort(unique(finale$jour)),
                                 selected = as.Date("2020-03-17",timeFormat="%Y-%m-%d")),

                     uiOutput("secondSelection")
                     
                     
                ) ,
                 
    dashboardBody(
        
        tabItem('departements',
            plotlyOutput('courbe_realise_mod'),
            valueBoxOutput("sommered"),
            valueBoxOutput("sommepred"),
            valueBoxOutput("diff"),
            valueBoxOutput("diffp"),
            # box( title = "table de donnee",
            #             tableOutput("ma_table2"),status = "primary",style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
            #      ),
            # 
            #tableOutput('ma_table2'),
            dataTableOutput('ma_table2'),
            downloadLink('downloadData', 'telecharger')
            
            
        )
    )
                     
                     
                     
                 
             
             
    )

# Server ------------------------------------------------------------------


server <- function(input, output) {
    
    # output$nomtable <- renderText({
    #     a="Table de données en Gwh"
    #     a
    # })
    
    
    filtre <- reactive({
        finale %>% 
            select(jour,paste0('conss_',
                               input$segment),paste0('pred_conss_',
                                                     input$segment),paste0('ecart_',
                                                                           input$segment))%>%
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)
    })
    
    get_somme <- reactive({
        
        recupere_donnee<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('conss_',
                          input$segment))
        
        somme <- sum(recupere_donnee)
        
        somme
        
        
    })
    
    get_somme_pr <- reactive({
        
        recupere_donnee1<- finale %>% 
            filter(jour >= input$obs)%>%
            filter(jour <= input$obs1)%>%
            select(paste0('pred_conss_',
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
            select(paste0('conss_',
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
            select(jour, contains('pred_conss_'),contains('conss_'))%>%
            tidyr::pivot_longer(-c("jour"))
        
        
        
        
        fig= ggplot(df1) +
            aes(y  = value, x = jour, color = name)+
            geom_line()+
            theme_bw()+
            theme(legend.position = 'bottom')+
            ggtitle("Graphique des consommations réalisée et prédites par segment") +
            xlab("Date") + ylab("Consommation (Gwh)")
        
        
        ggplotly(fig)
        
    })
    
    output$ma_table2 <- renderDataTable({
        out <-  filtre()
        out$jour<-as.character(out$jour)
         datatable(out, options = list(scrollX = TRUE))
        # out<-reactable(out)
        # print(out)
        # out
    
    } )
    
    
    output$sommered <- renderValueBox({
        valueBox(paste0(as.character(round(get_somme(),1))," Gwh"),"Somme de la consommation reelle",color="blue")
        
    })
    output$sommepred <- renderValueBox({
        
        valueBox(paste0(as.character(round(get_somme_pr(),1))," Gwh"),"Somme de la consommation prédite",color="blue"
        )
    })
    
    output$diff <- renderValueBox({

        
        valueBox(paste0(as.character(round(get_somme_di(),1))," Gwh"), paste0("Ecart : impact de ",as.character(round(get_somme_di_p(),3)),"% sur la consommation"),color = "blue"
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
    
    
    output$secondSelection <- renderUI({ 
        fin<-finale%>%filter(jour>input$obs)
        selectInput("obs1",
                    "date finale:", 
                    choices = sort(unique(fin$jour)),
                    selected = as.Date("2020-08-17",timeFormat="%Y-%m-%d"))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
