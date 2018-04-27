#install.packages("DT")
#install.packages("shiny")
#install.packages("shinyjs")
#install.packages("digest ")
#install.packages("shiny")

inputPred1<-0
fieldsMandatory <- c("name", "salary")
appCSS <- ".mandatory_star { color: red; }"


#fieldsAll <- c("name", "age ", "pregnant", "gender", "weight","height","bmi","headCircum","armCircum","waistCircum","sad","carbs","fiber","calcium","kcal","fat")
fieldsAll <- c("AgeYears", "SAD","DR1BWATZ", "DR1TALCO","DR1TCARB")
responsesDir <- file.path("responses")

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}
shiny::shinyApp(
  ui = fluidPage(theme = "bootstrap.css",
    titlePanel(h1("Diabetes Prediction")),
    div(
      id = "form",
      h2(textInput("AgeYears", "Age")),
      h2(textInput("SAD", "Sagital Abdominal Diameter")),
      h2(textInput("DR1BWATZ", "Water")),
      h2(textInput("DR1TALCO", "Alcohol")),
      h2(textInput("DR1TCARB", "Carb")),
     
      actionButton("submit", "Process", class = "btn-primary")
    )
    ,
   
    mainPanel(
      h3("Results: "),
      h3(textOutput("result"))
      
    )
  ),
  
  
  server = function(input, output, session) {
    
    
   
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })  
    
    
    
    
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      print("Inside form data formdata")
      print(data)
      data <- t(data)
      #print("Daa in formdata after transposing",data)
      #data
    })
    
    
    saveData <- function(data) {
      fileName <- "input1.csv"
      #print(data)
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
    }
    processdata<-function(){
      print("Insde process Data")
      input <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/DPA/Code/responses/input1.csv',sep=",", header=TRUE)
     
      ##
      
      finalLasso<- read.csv("C:/Users/chintana/OneDrive/Masters/Projects/Dataset/Cleant/finalexamDemoDiet.csv", stringsAsFactors = F)
      finalLasso<-finalLasso[,-c(1,2)] # removng X and SEQN
      
      finalGLM<-finalLasso[,c("AgeYears","SAD","DR1BWATZ","DR1TALCO","DR1TCARB","is_diabetic")]
      modelfinalGLM <- glm(is_diabetic~.,family = binomial(link = 'logit'), data = finalGLM)
      finalGLMPredictProb <- predict(modelfinalGLM,finalGLM,type = "response")
      finalGLMPredict <- ifelse(finalGLMPredictProb > 0.5,1,0)
      print("midwya")
      print("Taking input from Shiny form")
      input <- read.table('C:/Users/chintana/OneDrive/Masters/Projects/DPA/Code/responses/input1.csv',sep=",", header=TRUE)
      head(input)
      
      print("Processing from Input File")
      inputPredProb <- predict(modelfinalGLM,input,type = "response")
      inputPred1 <- ifelse(inputPredProb > 0.5,"Oops! Better cut down on the Sugar !! Looks like you are Diabetic :-(","Woohoo !!Treat yourself to an IceCream , You are NOT Diabetic :-)")
      print(inputPred1)
      inputPred1
          }
    
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      print("here")
      print(formData())
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      #shinyjs::show("thankyou_msg")
      #print("Before preocess data")
      inputPred1<-processdata()
      print(inputPred1)
      output$result <- renderPrint({inputPred1})
      #print("after displayin")
    })
    
      
    
    
  }
)

