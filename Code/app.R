#install.packages("DT")
#install.packages("shiny")
#install.packages("shinyjs")
#install.packages("digest ")
#install.packages("shiny")


fieldsMandatory <- c("name", "salary")
appCSS <- ".mandatory_star { color: red; }"


fieldsAll <- c("name", "age ", "pregnant", "gender", "weight","height","bmi","headCircum","armCircum","waistCircum","sad","carbs","fiber","calcium","kcal","fat")

responsesDir <- file.path("responses")

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

shiny::shinyApp(
  ui = fluidPage(
    titlePanel("Diabetes Prediction"),
    div(
      id = "form",
      
      textInput("name",labelMandatory("Name"), ""),
      textInput("age ", "Age"),
      checkboxInput("pregnant", "Are you Pregnant", FALSE),
      selectInput("gender", "Gender",
                  c("",  "Male", "Female")),
      textInput("weight",labelMandatory("Weight"), ""),
      textInput("height",labelMandatory("Height"), ""),
      textInput("bmi",labelMandatory("BMI"), ""),
      textInput("headCircum",labelMandatory("Head Circumfernce"), ""),
      textInput("armCircum",labelMandatory("Arm Circumference"), ""),
      textInput("waistCircum",labelMandatory("Waist Circumference"), ""),
      textInput("sad",labelMandatory("Sagital Abdominal Diameter"), ""),
      textInput("carbs",labelMandatory("Food- Carbs"), ""),
      textInput("fiber",labelMandatory("Food- Fiber"), ""),
      textInput("calcium",labelMandatory("Food- Calcium"), ""),
      textInput("kcal",labelMandatory("Food- kcal"), ""),
      textInput("fat",labelMandatory("Food -Fat"), ""),
      
      actionButton("submit", "Submit", class = "btn-primary")
    )
    ,
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
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
      data <- t(data)
      data
    })
    
    
    saveData <- function(data) {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))
      
      write.csv(x = data, file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
    }
    
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      print("here")
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
  }
)

