---
  title: "Final Project"
author: "Lauren Mauser"
date: "7/29/2022"
output:
---
install.packages("RSQLite")
install.packages("RMySQL")
install.packages("DBI")
install.packages("odbc")
install.packages("config")
install.packages('rsconnect')
  Load the RSQLite Library
```{r}
library(RSQLite)
library(RMySQL)
library(DBI)
library(odbc)
library(config)
```
rsconnect::setAccountInfo(name='heartdiseaseriskfactorsurvey2022',
                          token='65641DB44728FA62CFE0CC4C141E0327',
                          secret='u36LZpfBklPJyievgfpcCtmH2OJh3txhozh05oNC')
library(rsconnect)

rsconnect::deployApp('C:/Users/laure')

#Connect to MySQL workbench database 
conn<- dbConnect(RMySQL::MySQL(), 
                  .connection_string="Driver = {MySQL ODBC 8.0 Unicode Driver};",
                  Server= "127.0.0.1", dbname = "heartdisease",
                  UID="root", password="root", Port=3306)

dbListTables(conn)

#Import tables
Demographics <- dbReadTable(conn, "demographics")
Gen_Health_Cond <- dbReadTable(conn, "gen_health_cond")
Heart_2020_Cleaned <- dbReadTable(conn, "heart_2020_cleaned")
Life_Style <- dbReadTable(conn, "life_style")
Mod_Health_Cond <- dbReadTable(conn, "mod_health_cond")
Phys_Health_Cond <- dbReadTable(conn, "phys_health_cond")
Serious_Health_Cond <- dbReadTable(conn, "serious_health_cond")

#Create RSQLite database
HD_conn <- dbConnect(RSQLite::SQLite(), "heartdisease.db")
dbListTables(HD_conn)

#write tables
dbWriteTable(HD_conn, "demographics", Demographics)
dbWriteTable(HD_conn, "gen_health_cond", Gen_Health_Cond)
dbWriteTable(HD_conn, "heart_2020_cleaned", Heart_2020_Cleaned)
dbWriteTable(HD_conn, "life_style", Life_Style)
dbWriteTable(HD_conn, "mod_health_cond", Mod_Health_Cond)
dbWriteTable(HD_conn, "phys_health_cond", Phys_Health_Cond)
dbWriteTable(HD_conn, "serious_health_cond", Serious_Health_Cond)

install.packages("shiny")
install.packages("shinythemes")
install.packages("dplyr")
install.packages("readr")
install.packages("shinysurveys")
install.packages("shinyWidgets")

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(shinysurveys)
library(shinyWidgets)
question<-c("Do you smoke?","Do you smoke?","Do you drink alcohol?","Do you drink alcohol?","What is your BMI?",
            "What is your gender?","What is your gender?","How old are you?","What race do you identify as?",
            "What race do you identify as?","What race do you identify as?","What race do you identify as?",
            "What race do you identify as?","What race do you identify as?","Have you had a stroke?", "Have you had a stroke?",
            "Do you have kidney disease?", "Do you have kidney disease?", "Do you have skin cancer?",
            "Do you have skin cancer?","Do you have asthma?","Do you have asthma?","Do you have diabetes?",
            "Do you have diabetes?","Do you have diabetes?","Do you have diabetes?",
            "In the past 30 days, how many days was your physical health not good?",
            "In the past 30 days, how many days was your mental health not good?","Do you have difficulty walking?", 
            "Do you have difficulty walking?","How many hours do you sleep a day?",
            "Have you taken this survey before and wish to update your answers? Please enter your patient id below or type '0' if this is your first time taking the survey.")
option <- c( "Yes","No","Yes","No","Your Answer","Female","Male","Your Answer","American Indian/Alaskan Native",
"Asian","Black","Hispanic","Other","White","Yes","No","Yes","No","Yes","No","Yes","No","Yes","No, borderline diabetes","No",
"Yes (during pregnancy)","Your Answer","Your Answer","Yes","No","Your Answer","Enter Patient ID or 0")
input_type <- c("y/n","y/n","y/n","y/n","numeric","mc","mc","numeric","mc","mc","mc","mc","mc","mc","y/n","y/n","y/n","y/n","y/n",
                "y/n","y/n","y/n","mc","mc","mc","mc","numeric","numeric","y/n","y/n","numeric","numeric")
input_id <- c("smoking","smoking","alcohol","alcohol","BMI","gender","gender","age","race","race","race",
              "race","race","race","stroke","stroke","kidneydisease","kidneydisease","skincancer","skincancer",
              "asthma","asthma","diabetes","diabetes","diabetes","diabetes","phys_health","ment_health","diff_walk","diff_walk",
              "sleep","update")
dependence <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
dependence_value <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
required <- c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T)

df<-data.frame(question, option, input_type, input_id, dependence, dependence_value, required)


dbGetQuery(HD_conn, "SELECT *
           FROM heart_2020_cleaned
           ORDER BY patient_id DESC LIMIT 10")

dbExecute(HD_conn, "DELETE FROM Heart_2020_Cleaned WHERE patient_id = ***")


#UI
if (interactive()){
  shinyApp(
    ui <- fluidPage(theme=shinytheme("cerulean"),
                    titlePanel("What is your risk for Heart Disease?"),
                    
                    sidebarLayout(
                      sidebarPanel(
                        h1("Already taken the Heart Disease survey?"),h2("Enter your patient id to review your answers."),
                        numericInput("patientid","Enter Patient ID", 1),
            
                        h1("Want to delete your results? Enter Patient ID and click button below."),
                        numericInput("patientid2","Enter Patient ID",1), 
                        actionButton("button","Delete my data")),
                      mainPanel( 
                        tabsetPanel(tabPanel("Survey",surveyOutput(df=df,
                                                                   survey_title='Please answer the following questions to determine your risk for Heart Disease.',
                                                                   survey_description='These health questions can determine preventive care to lower your risk of Heart Disease.'),
                                             textOutput('result')), tabPanel("Results",tableOutput('patientinfo'))
                        )
                                       
                      )
                    )
    ),
  
    
               
    server <- function(input, output){
      
      output$patientinfo <- renderTable({dbGetQuery(HD_conn,
                                                  "SELECT L.smoking, L.alc_drinking,H.BMI, 
                                                  D.sex, D.age, D.race, S.stroke, S.kidney_disease, 
                                                  S.skin_cancer, M.asthma, M.diabetic, P.phys_health,
                                                  P.ment_health, P.diff_walking, G.sleep_time
                                                    from heart_2020_cleaned as H 
                                                    Inner JOIN life_style as L
                                                    ON H.life_style_id = L.life_style_id
                                                    Inner JOIN demographics as D
                                                    ON H.demographics_id = D.demographics_id
                                                    Inner JOIN serious_health_cond as S
                                                    ON H.serious_health_cond_id = s.serious_health_cond_id
                                                    Inner Join mod_health_cond as M
                                                    ON H.mod_health_cond_id = M.mod_health_cond_id
                                                    Inner Join phys_health_cond as P
                                                    ON H.phys_health_cond_id = P.phys_health_cond_id
                                                    Inner Join gen_health_cond as G
                                                    ON H.gen_health_cond_id= G.gen_health_cond_id
                                                    WHERE H.patient_id = ?",
                                                  params=input$patientid)})
      observeEvent(input$button, {dbExecute(HD_conn, "DELETE FROM Heart_2020_Cleaned WHERE patient_id = ?", params=input$patientid2)})
      
      renderSurvey()
      
      observeEvent(input$submit, {
        
        HeartDisease <- 'N/A'
        HeartDisease <-data.frame(HeartDisease)
        BMI <- input$BMI
        BMI <-data.frame(BMI)
        if(input$update==0){
        patient_id2 <- dbGetQuery(HD_conn, "SELECT MAX(patient_id)+1 AS patient_id
                                 FROM heart_2020_cleaned")}
        else{patient_id<- input$update
            patient_id2<-data.frame(patient_id)
            dbExecute(HD_conn, "DELETE FROM Heart_2020_Cleaned WHERE patient_id =?",params=input$update)
            
        }
        life_style_id<-dbGetQuery(HD_conn,"SELECT life_style_id
                          FROM life_style 
                          Where smoking = ?
                          AND alc_drinking = ?", params=c(input$smoking,input$alcohol))
        demographics_id<-dbGetQuery(HD_conn,"SELECT demographics_id
                          FROM demographics 
                          Where sex = ?
                          AND race = ? AND age ='50-54'", params=c(input$gender,input$race))
        serious_health_cond_id<-dbGetQuery(HD_conn,"SELECT serious_health_cond_id
                          FROM serious_health_cond 
                          Where stroke = ?
                          AND kidney_disease = ? AND skin_cancer =?", params=c(input$stroke,input$kidneydisease,input$skincancer))
        mod_health_cond_id<-dbGetQuery(HD_conn, "SELECT mod_health_cond_id
                                       FROM mod_health_cond
                                       WHERE asthma=? AND diabetic =?",params=c(input$asthma,input$diabetes))
        gen_health_cond_id<-dbGetQuery(HD_conn, "SELECT gen_health_cond_id
                                       from gen_health_cond
                                       WHERE sleep_time=? 
                                       LIMIT 1", params=input$sleep)
        phys_health_cond_id<-dbGetQuery(HD_conn, "SELECT phys_health_cond_id
                                        from phys_health_cond
                                        WHERE phys_health=? AND ment_health=?
                                        AND diff_walking = ? AND phys_act='No'", params=c(input$phys_health,input$ment_health,input$diff_walk))
        df_hd <-data.frame(HeartDisease, BMI, patient_id2,life_style_id,demographics_id,serious_health_cond_id,mod_health_cond_id,gen_health_cond_id,phys_health_cond_id)
        dbWriteTable(HD_conn,"heart_2020_cleaned",df_hd,append=TRUE)
        
        
        showModal(modalDialog(title='Survey Completed',
          if(input$smoking=='Yes'){'You have a higher risk for Heart Disease due to life style choices. Please consider not smoking.'},
            if(input$BMI>30){ 'You have a higher risk for Heart Disease due to BMI. Please consider lowering BMI.'},
            if(input$sleep<7){'You have a higher risk for Heart Disease due to sleep habits. Please consider sleeping at least 7 hours a night.'},
          if(input$stroke=='Yes'){'You have a higher risk for Heart Disease due to other health conditions. Please consider treatment to prevent future strokes.'},
          if(input$kidneydisease=='Yes'){'You have a higher risk for Heart Disease due to other health conditions. Please consider treatment for kidney disease.'},
          if(input$diabetes=='Yes'){'You have a higher risk for Heart Disease due to a pre-existing health condition. Please consider treatment for diabetes.'},
          if(input$diff_walk =='Yes'){'You have a higher risk for Heart Disease due to a physical health condition. Please consider exercising to make walking less difficult.'},
          if(input$smoking=='No' && input$BMI <=30 && input$sleep>=7 && input$stroke=='No' && input$kidneydisease=='No' && input$diabetes=='No' && input$diff_walk=='No'){'You have a low risk for Heart Disease.'}         
        ))
      })
      
    }
    
  )
}

shinyApp(ui=ui, server=server)

dbDisconnect(HD_conn)


