shinyServer(function(input, output) {
  ##################### Update data with responses from Input ##########

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


  statTbl <- reactive({
    return(stats(DT = dataAxaml, varnames = input$catVar,
                 elements = "binaryTarget"))
  })

  output$infoTable <- renderDataTable({ci})

  output$hcPlot <- renderHighchart({
    st <- statTbl()
    nm = names(st)[1]
    axaml::plotter(st, y1 = "exposure", y2 = "binaryTarget")[[nm]]
  })

  ##################### 1_3 Update Correlation matrix #####################

  corm <- reactive({
    if (input$in_1_3_cortype == "spearman") {
      cap <- 1200000 / (nrow(data0) * length(numeric_vars))
      data0 <- data0[runif(nrow(data0)) < cap,]

    }

    cor(x = data0[, numeric_vars, with = F],
        use = "pairwise.complete.obs",
        method = input$in_1_3_cortype)

  })

  corm_melt <- reactive({
    melt(corm()) %>% arrange(desc(abs(value))) %>% filter(value < 1)
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


  ##################### Indicators for 1_2 #####################

  out_1_2_most_occuring <- reactive({
    data() %>% group_by_(input$in_1_2_var) %>% summarize(n = n()) %>% arrange(desc(n)) %>% head(3)
  })

  out_1_2_least_occuring <- reactive({
    data() %>% group_by_(input$in_1_2_var) %>% summarize(n = n()) %>% arrange(n) %>% head(3)
  })

  out_1_2_pctg_missing <- reactive({
    round(sum(is.na(data()[[input$in_1_2_var]])) / nrow(data()), 2)
  })

  out_1_2_less_100obs <- reactive({
    data() %>% group_by_(input$in_1_2_var) %>% summarize(n = n()) %>% filter(n < 100) %>% nrow()
  })

  out_1_2_levels <- reactive({
    length(levels(as.factor(data()[[input$in_1_2_var]])))
  })

  ##################### Output elements #####################
  #####################
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

  ################## Output elements panel 1_2 ########################

  output$out_1_2_main_plot <- renderPlot({
    # Part before determination horizontal/vertical
    # v22 gives a bug - fix this still
    p <-
      ggplot(data(), aes(x = as.factor(data()[[input$in_1_2_var]]))) +
      geom_bar()

    # if horizontal
    if (input$in_1_2_plot_direction == "horizontal") {
      # get dataframe with info for vertical lines
      p + coord_flip()

    }

    # if vertical
    else{
      p
    }

  })

  output$out_1_2_table1 <- renderTable({
    data <- data.frame(
      names = c("1stMost", "2ndMost", "3thMost"),
      values = out_1_2_most_occuring()[, 1],
      count = out_1_2_most_occuring()[, 2],
      pctg = round(out_1_2_most_occuring()[, 2] / nrow(data()), 2)
    )
    names(data) <- c("names", "values", "count", "pctg")
    data
  })

  output$out_1_2_table2 <- renderTable({
    data <- data.frame(
      names = c("1stLeast", "2ndLeast", "3thLeast"),
      values = out_1_2_least_occuring()[, 1],
      count = out_1_2_least_occuring()[, 2],
      pctg = round(out_1_2_least_occuring()[, 2] / nrow(data()), 2)
    )
    names(data) <- c("names", "values", "count", "pctg")
    data
  })

  output$out_1_2_table3 <- renderTable({
    data <- data.frame(
      names = c("levels", "<100 obs", "pctg missing"),
      values = c(
        out_1_2_levels(), out_1_2_less_100obs(), out_1_2_pctg_missing()
      )
    )
  })

  ################## Output elements panel 1_3 ########################


  output$out_1_3_main_plot <- renderPlot({

    cor_vars <-
      corm_melt() %>% filter(abs(value) > input$in_1_3_limit) %>% select(Var1)
    cor_vars <- unique(as.character(cor_vars[,1]))

    filter_corm <- corm()[cor_vars, cor_vars]


    corrplot(
      filter_corm, order = input$in_1_3_order, method = input$in_1_3_plottype
    )
  })

  ################## Output elements panel 1_4 ########################

  output$out_1_4_main_plot <- renderPlot({
      image(data_missing)
  })

################## Output elements panel 1_6 ########################

output$out_1_6_main_plot <- renderPlot({

  p1 <- ggplot(data0, aes(x = data0[[input$in_1_6_var]], y = target))
  p1 <- p1 + geom_smooth()

  p2 <- ggplot(data0, aes(x = data0[[input$in_1_6_var]], y = ..density..))
  p2 <- p2 + geom_density()

  grid.arrange(p1, p2, heights = c(3,1))

  })

})

################## Output elements panel 1_8 ########################



