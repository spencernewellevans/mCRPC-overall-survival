#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(MASS)
library(survival)

event_fit <<- readRDS("data/event_model.rds")
ttd_fit <<- readRDS("data/ttd_model.rds")
train_data <<- readRDS("data/train_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Overall Survival Prediction Model for mCRPC Patients"),
  
  # PATIENT INFORMATION
  inputPanel(verticalLayout(
    h4("Patient Information:"),
    numericInput("weightbl", "Patient's Weight (kg):", value = 78.6, min = 0, max = NA, step = 0.1),
    numericInput("age", "Patient's age:", value = 71, min = 18, max = NA, step = 1),
    selectInput("race", "Patient's Race:", c("Asian", "Black", "White", "Other"))
  )),
  
  #LAB RESULTS
  inputPanel(verticalLayout(
    h4("Lab Results:"),
    numericInput("alp", "Alkaline phophatase lab value (U/L):", value = 141, min = 0, max = NA, step = 0.1),
    numericInput("ast", "Aspartate aminotransferase lab value (U/L):", value = 14, min = 0, max = NA, step = 0.1),
    numericInput("ca", "Calcium lab value (MMOL/L):", value = 1.25, min = 0, max = NA, step = 0.1),
    numericInput("tbili", "Total bilirubin lab result (UMOL/L):", value = 9.405, min = 0, max = NA, step = 0.1)
  )),
  
  
  #TREATMENT HISTORY
  inputPanel(verticalLayout(
    h4("Treatment History:"),
    radioButtons("corticosteroid", "Does the patient have a history of taking corticosteroid medication?",
                 c("yes", "no"), selected = "yes", inline = TRUE),
    radioButtons("analgesics", "Does the patient have a history of taking analgesic medication?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("gonadotropin", "Does the patient have a history of taking gonadotropin medication?",
                 c("yes", "no"), selected = "no", inline = TRUE)
  )),
  
  #MEDICAL HISTORY INPUTS
  inputPanel(verticalLayout(
    h4("Medical History:"),
    radioButtons("mhsocial", "Does the patient have any social issues that could impact condition reports?",
                 c("yes", "no"), selected = "yes", inline = TRUE),
    radioButtons("mhsurg", "Does the patient have a history of surgical procedures?",
                 c("yes", "no"), selected = "yes", inline = TRUE),
    radioButtons("mhneopla", "Does the patient have a history of neoplasm?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("mhnerv", "Does the patient have a history of nervous system disorder?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("gibleed", "Does the patient have a history of gastrointestinal bleeding?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("lymphadenectomy", "Does the patient have a history of bilateral lymphadenectomy?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("mi", "Does the patient have a history of myocardial infarction?",
                 c("yes", "no"), selected = "no", inline = TRUE)
  )),
  
  #CANCER METASTASIS
  inputPanel(verticalLayout(
    h4("Cancer Metastasis:"),
    radioButtons("liver", "Are tumor metastases present in the liver?",
                 c("yes", "no"), selected = "no", inline = TRUE),
    radioButtons("pleura", "Are tumor metastases present in the pleura?",
                 c("yes", "no"), selected = "yes", inline = TRUE),
    radioButtons("kidneys", "Are tumor metastases present in the kidneys?",
                 c("yes", "no"), selected = "no", inline = TRUE)
  )),
  
  submitButton("Make Prediction"),
  
  inputPanel(
    htmlOutput("surv_pred")
  )
)

# Define server logic
server <- function(input, output) {
  
  output$surv_pred <- renderUI({
    
    test_var <<- input$race
    
    # Translate age input
    age1 <- 0
    age2 <- 0
    age3 <- 0
    age <- input$age
    if (age >= 18 && age < 65) {
      age1 <- 1
    } else if (age > 64 && age < 75){
      age2 <- 1
    } else {
      age3 <- 1
    }
    
    # Translate race input
    race <- input$race
    race1 <- 0
    race2 <- 0
    race3 <- 0
    race4 <- 0
    if (race == "Asian") {
      race1 <- 1
    } else if (race == "Black") {
      race2 <- 1
    } else if (race == "White") {
      race3 <- 1
    } else {
      race4 <- 1
    }
    
    cort <- input$corticosteroid
    gesics <- input$analgesics
    gona <- input$gonadotropin
    gi <- input$gibleed
    social <- input$mhsocial
    surg <- input$mhsurg
    neopla <- input$mhneopla
    nerv <- input$mhnerv
    lymph <- input$lymphadenectomy
    myocard <- input$mi
    liv <- input$liver
    lung <- input$pleura
    kid <- input$kidneys
    
    # Convert radio inputs from y/n to 1/0
    if (cort == "yes") {
      corticosteroid <- 1
    } else {
      corticosteroid <- 0
    }
    if (gesics == "yes") {
      analgesics <- 1
    } else {
      analgesics <- 0
    }
    if (gona == "yes") {
      gonadotropin <- 1
    } else {
      gonadotropin <- 0
    }
    if (gi == "yes") {
      gibleed <- 1
    } else {
      gibleed <- 0
    }
    if (social == "yes") {
      mhsocial <- 1
    } else {
      mhsocial <- 0
    }
    if (surg == "yes") {
      mhsurg <- 1
    } else {
      mhsurg <- 0
    }
    if (neopla == "yes") {
      mhneopla <- 1
    } else {
      mhneopla <- 0
    }
    if (nerv == "yes") {
      mhnerv <- 1
    } else {
      mhnerv <- 0
    }
    if (lymph == "yes") {
      lymphadenectomy <- 1
    } else {
      lymphadenectomy <- 0
    }
    if (myocard == "yes") {
      mi <- 1
    } else {
      mi <- 0
    }
    if (liv == "yes") {
      liver = 1
    } else {
      liver = 0
    }
    if (lung == "yes") {
      pleura = 1
    } else {
      pleura = 0
    }
    if (kid == "yes") {
      kidneys = 1
    } else {
      kidneys = 0
    }
    
    # Construct data frame from input data
    new_event_data <<- data.frame(
      "WEIGHTBL" = input$weightbl,
      "Age1" = age1,
      "Age2" = age2,
      "Age3" = age3,
      "Asian" = race1,
      "Black" = race2,
      "White" = race3,
      "OtherRace" = race4,
      "ALP" = input$alp,
      "AST" = input$ast,
      "CA" = input$ca,
      "TBILI" = input$tbili,
      "CORTICOSTEROID" = corticosteroid,
      "ANALGESICS" = analgesics,
      "GONADOTROPIN" = gonadotropin,
      "GIBLEED" = gibleed,
      "MHSOCIAL" = mhsocial,
      "MHSURG" = mhsurg,
      "MHNEOPLA" = mhneopla,
      "MHNERV" = mhnerv,
      "LYMPHADENECTOMY" = lymphadenectomy,
      "MI" = mi,
      "LIVER" = liver,
      "PLEURA" = pleura,
      "KIDNEYS" = kidneys
    )
    
    
    new_data_test <<- new_event_data
    
    
    # Make death prediction based on user data
    death_pred <- predict(event_fit, new_event_data)
    
    # Ouput to UI based on prediciton
    if (death_pred$class == 1) {
      death_msg <- "Death: YES"
    } else {
      death_msg <- "Death: NO"
    }
    
    # If death is predicted 'YES' then continue to ttd prediciton
    ttd_pred <- NA
    if (death_pred$class == 1) {
      sfit <- survfit(ttd_fit, new_event_data)
      ttd_pred <- sfit$time[which.min(abs(sfit$surv - 0.75))]
      ttd_msg <- paste("Estimated time to death: ", paste(ttd_pred, "days", sep = " "), sep = "<br/>")
      HTML(paste(death_msg, ttd_msg, sep="<br/>"))
    } else {
      HTML(death_msg)
    }
    
    # HTML(death_out)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

