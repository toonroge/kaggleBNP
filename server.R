library(ggplot2) ; library(moments) ; library(dplyr) ; library(readr) ; library(data.table)

data0 <- as.data.frame(fread("train.csv"))

shinyServer(function(input, output) {
  ##################### Update data with responses from Input #####################
  
  data <- reactive({
    data <- data0
    if (input$in_1_1_transform == "log") {
      data[[input$var]] <- log(data[[input$var]])
    }
    else if (input$in_1_1_transform == "sqrt") {
      data[[input$var]] <- sqrt(data[[input$var]])
    }
    if (input$in_1_1_round == T) {
      data[[input$var]] <- round(data[[input$var]], 2)
    }
    data
  })
  
  ##################### Calculate indicators summarizing distribution #####################
  
  average <- reactive({
    round(mean(data()[[input$var]], na.rm = T), 2)
  })
  
  se <- reactive({
    round(sd(data()[[input$var]], na.rm = T), 2)
  })
  
  median <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.5, na.rm = T
    )), 2)
  })
  
  m3 <- reactive({
    round(as.numeric(skewness(data()[[input$var]], na.rm = T)), 2)
  })
  
  m4 <- reactive({
    round(as.numeric(kurtosis(data()[[input$var]], na.rm = T)), 2)
  })
  
  q000 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.00, na.rm = T
    )), 2)
  })
  
  q001 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.01, na.rm = T
    )), 2)
  })
  
  q005 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.05, na.rm = T
    )), 2)
  })
  
  q010 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.10, na.rm = T
    )), 2)
  })
  
  q025 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.25, na.rm = T
    )), 2)
  })
  
  q075 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.75, na.rm = T
    )), 2)
  })
  
  q090 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.90, na.rm = T
    )), 2)
  })
  
  q095 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.95, na.rm = T
    )), 2)
  })
  
  q099 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 0.99, na.rm = T
    )), 2)
  })
  
  q100 <- reactive({
    round(as.numeric(quantile(
      data()[[input$var]], probs = 1.00, na.rm = T
    )), 2)
  })
  
  most_occuring <- reactive({
    head(summary(as.factor(data()[[input$var]])), 4)
  })
  
  pctg_missing <- reactive({
    round(sum(is.na(data()[[input$var]])) / nrow(data()), 2)
  })
  
  ##################### Output elements #####################
  
  output$main_plot <- renderPlot({
    # Part not depending on whether vertical lines or not
    p <- ggplot(data(), aes(x = data()[[input$var]])) +
      geom_histogram(bins = input$n_breaks) +
      xlim(as.numeric(quantile(data()[[input$var]], input$range, na.rm = T)))
    
    # if vertical lines
    if (input$average == T) {
      # get dataframe with info for vertical lines
      names <- c("Average", "Median")
      values <- c(average(), median())
      vertical_lines <- data.frame(names = names, values = values)
      
      p + geom_vline(
        data = vertical_lines, aes(xintercept = values, colour = names), show.legend = T
      )
      
    }
    
    # if no vertical lines
    else{
      p
    }
    
  })
  
  output$out_1_1_table1 <- renderTable({
    names <- c("Average", "Sd", "Median", "skewness", "kurtosis")
    values <- c(average(), se(), median(), m3(), m4())
    data.frame(names = names, values = values)
    
  })
  
  output$out_1_1_table2 <- renderTable({
    names <- c("Min", "q01", "q05", "q10", "q25")
    values <- c(q000(), q001(), q005(), q010(), q025())
    data.frame(names = names, values = values)
    
  })
  
  output$out_1_1_table3 <- renderTable({
    names <- c("q75", "q90", "q95", "q99", "Max")
    values <- c(q075(), q090(), q095(), q099(), q100())
    data.frame(names = names, values = values)
    
  })
  
  output$out_1_1_table4 <- renderTable({
    names <-
      c("1stMost", "2ndMost", "3thMost", "4thMost", "PctgMissing")
    values <- c(as.numeric(names(most_occuring())), pctg_missing())
    count <-
      c(as.numeric(most_occuring()), round(pctg_missing() * nrow(data()), 0))
    data.frame(names = names, values = values, count = count)
    
  })
  
})