# Still change variable names; put reference to in/out + the tab for which it is used

library(shiny) ; library(data.table)

data <- as.data.frame(fread("train.csv")) # never worked with data.table, change code later

# extract numeric vars for selection in UI
numeric_vars <- names(data[, sapply(data, is.numeric)])
numeric_vars <- numeric_vars[!numeric_vars %in% c("ID", "target")]

shinyUI(navbarPage(
  "Stage 1: Exploratory Analysis",
  
  tabPanel("1.0 Readme",
           
           "This app is for exploratory analysis of the data."),
  
  tabPanel("1.1 Numeric Variables One Way",
           
           fluidPage(
             fluidRow(column(
               3,
               wellPanel(
                 selectInput(
                   inputId = "var",
                   label = "Select your numeric variable:",
                   choices = numeric_vars
                 ),
                 
                 sliderInput(
                   inputId = "n_breaks",
                   label = "Number of bins in histogram:",
                   min = 50, max = 1000, value = 100, step = 50
                 ),
                 
                 sliderInput(
                   inputId = "range",
                   label = "The range of the X interval:",
                   min = 0, max = 1, value = c(0, 1), step = 0.01
                 ),
                 
                 selectInput(
                   inputId = "in_1_1_transform",
                   label = "Select your tranformation:",
                   choices = c("none", "sqrt", "log")
                 ),
                 
                 checkboxInput(inputId = "average", label = "Add indicators"),
                 
                 checkboxInput(inputId = "in_1_1_round", label = "Round (2dec)")
                 
               )
               
             ),
             
             column(9,
                    wellPanel(
                      plotOutput(outputId = "main_plot", height = "420px")
                    ))),
             
             fluidRow(
               column(3, "Complete here with an explanation of the panel."),
               
               column(
                 2, align = "center",
                 wellPanel(tableOutput(outputId = "out_1_1_table1"))
                 
               ),
               column(2, align = "center",
                      wellPanel(tableOutput(outputId = "out_1_1_table2"))),
               
               column(2, align = "center",
                      wellPanel(tableOutput(outputId = "out_1_1_table3"))),
               
               column(3, align = "center",
                      wellPanel(tableOutput(outputId = "out_1_1_table4")))
             )
             
           )),
  tabPanel(
    "1.2 Categorical variables one way",
    
    "This sheet is the next to construct, put some
    interesting plots customized for categorical features"
    
    
  )
  
))
