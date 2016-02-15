shinyUI(
  navbarPage(
    "Stage 1: Exploratory Analysis",

    tabPanel("1.0 Readme",
             "This app is for exploratory analysis of the data."),
    tabPanel("1.1 Dataset Overview",
             dataTableOutput("infoTable")),

    tabPanel("1.1Bis Levelwise exploration for categorical vars",
             fluidPage(
               fluidRow(column(3,
                        wellPanel(
                        selectInput(
                        inputId = "catVar",
                        label = "Select a categorical variable:",
                        choices = categorical_vars
                        ))),
                        column(9,
                        wellPanel(
                        highchartOutput("hcPlot")
                      )))
             )
    ),

    tabPanel("1.2 Numerics",

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

                   checkboxInput(inputId = "in_1_1_round", label = "Round (2dec)"),

                   "This panel allows for the investigation of the nu;erical variables."

                 )

               ),

               column(9,
                      wellPanel(
                        plotOutput(outputId = "main_plot", height = "420px")
                      ))),

               fluidRow(
                 column(
                   2, offset = 3, align = "center",
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
      "1.3 Categorical",

      fluidPage(
        fluidRow(column(
          3,
          wellPanel(
            selectInput(
              inputId = "in_1_2_var",
              label = "Select your categorical variable:",
              choices = categorical_vars
            ),

            radioButtons(
              inputId = "in_1_2_plot_direction",
              label = "Select plot orientation:",
              choices = c("horizontal", "vertical")
            ),

            selectInput(
              inputId = "in_1_2_plot_subset",
              label = "Select subset:",
              choices = c("top5", "top10", "bottom5", "bottom10", "all")
            ),

            "This panel allows you to investigate the distribution of the diffeent categorical variables.
            Still enable functionality of most and least occurring."


          )

        ),

        column(9,
               wellPanel(
                 plotOutput(outputId = "out_1_2_main_plot", height = "420px")
               ))),

        fluidRow(
          column(
            3, offset = 3, align = "center",
            wellPanel(tableOutput(outputId = "out_1_2_table1"))
          ),

          column(3, align = "center",
                 wellPanel(tableOutput(outputId = "out_1_2_table2"))),

          column(3, align = "center",
                 wellPanel(tableOutput(outputId = "out_1_2_table3")))
        )

      )
      ),

    tabPanel("1.4 Correlations",

             fluidPage(fluidRow(
               column(
                 3,
                 wellPanel(
                   sliderInput(
                     inputId = "in_1_3_limit",
                     label = "Correlation absolute value treshold",
                     min = 0, max = 0.99, value = 0.85, step = 0.01
                   ),

                   selectInput(
                     inputId = "in_1_3_plottype",
                     label = "Select plottype:",
                     choices = c("circle", "square", "ellipse", "number", "shade",
                                 "color", "pie")
                   ),

                   selectInput(
                     inputId = "in_1_3_order",
                     label = "Select ordering:",
                     choices = c("hclust", "FPC", "alphabet", "AOE", "original")
                   ),

                   selectInput(
                     inputId = "in_1_3_cortype",
                     label = "Select correlation type:",
                     choices = c("pearson", "spearman") # kendall is too slow, spearman feasible on subset
                   ),

                   "This panel allows you for investigation of the correlation patterns.
                   The treshold with spearmans row doesn't seem to function properly.
                   Still add a tool - when you select one variable, you can see the variables with which
                   correlation is highest."

                 )
               ),

               column(9,
                      wellPanel(
                        plotOutput(outputId = "out_1_3_main_plot", height = "725px")
                      ))
               ))),

    tabPanel("1.5 Missings",

             fluidPage(fluidRow(
               wellPanel(plotOutput(outputId = "out_1_4_main_plot", height = "800px"))
             ))),

    tabPanel("1.6 PCA",

             "package factoMineR, will need to work on a subset probably, do clustering also"),

    tabPanel("1.7 One-way on target",

             fluidPage(
               fluidRow(column(
                 3,
                 wellPanel(
                   selectInput(
                     inputId = "in_1_6_var",
                     label = "Select your numeric variable:",
                     choices = numeric_vars
                   ),

                   "For the moment only numeric variables are chosen.
                   For numeric vars we should be able to select transformations,
                   for categorical vars we should be able to create a group others"


                   )),
                 column(
                   9, wellPanel(
                     wellPanel(plotOutput(outputId = "out_1_6_main_plot", height = "600"))
                   )
                 )))),

    tabPanel("1.8 Two way on target",

             fluidPage(
                   fluidRow(column(
                         3,
                         wellPanel(
                               selectInput(
                                     inputId = "in_1_8_var1",
                                     label = "Select your first numeric variable:",
                                     choices = numeric_vars
                               ),

                               selectInput(
                                     inputId = "in_1_8_var2",
                                     label = "Select your second numeric variable:",
                                     choices = numeric_vars
                               ),

                               "DOES NOT WORK YET, trouble with implementing s(v1, v2, k = 40) in the formula for gam.
                                    For the moment only numeric variables are chosen.
                                    Still misleading visualization, because scale can be very small
                              and still big colour difference.
                               For numeric vars we should be able to select transformations,
                               for categorical vars we should be able to create a group others and we will
                               use facet_grid"


                         )),
                         column(
                               9, wellPanel(
                                     wellPanel(plotOutput(outputId = "out_1_8_main_plot", height = "600"))
                               )
                         )))

           )

             )
    )
