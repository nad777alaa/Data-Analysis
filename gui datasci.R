library(shiny)
library(reader)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Select CSV file"),
      actionButton("run", "Run Analysis"),   
      selectInput("num_clusters", "Number of Clusters", selected  = 3, choices = c(2,3,4)),
      sliderInput("support", "Enter minimum support:", value = 0.1,min=0.001,max=1),
      sliderInput("confidence", "Enter minimum confidence:", value = 0.1,min=0.001,max=1),
      
    ),
    mainPanel(
      fluidRow(
        column(6, textOutput("data_summary")),
        column(6, textOutput("duplicated_rows"))
      ),
      fluidRow(
        column(6, textOutput("data_without_duplicates")),
        column(6, textOutput("integer_columns"))
      ),
      fluidRow(
        column(6, textOutput("na_summary")),
        column(6, textOutput("outlier_summary"))
      ),
      fluidRow(
        column(12, plotOutput("boxplot"))
      ),
      fluidRow(
        # column(6, plotOutput("boxplot2")),
        column(6, plotOutput("pie_chart")),
        column(4, plotOutput("scatter_plot"))
      ),
      fluidRow(
        column(6, plotOutput("bar_plot")),
        column(6, plotOutput("histogram"))
      ),
      fluidRow(
        column(6, plotlyOutput("scatter_plot1")),
        column(6, tableOutput("clusters_table"))
      ),
      
      fluidRow(
        # column( 12 ,tableOutput("association_results") , title = "Association")
        column(12,tableOutput("association_results") , title = "Association")
        
      )
    )
  )
)

server <- function(input, output) {
  # Reactive expressions
  data <- reactive({
    req(input$csv_file)
    read.csv(input$csv_file$datapath)
  })
  
  data_without_duplicates <- reactive({
    req(data())
    distinct(data())
  })
  
  grouped_data <- reactive({
    data_without_duplicates() %>%
      group_by(customer) %>%
      summarise(
        total_transactions = n(),
        total_amount = sum(total),
        avg_age = mean(age),
        count = sum(count),
        city = unique(city)
      )
  })
  
  kmeans_results <- reactive({
    k <- input$num_clusters 
    columns_to_cluster <- grouped_data() %>% select(avg_age, total_amount)
    kmeans_result <- kmeans(columns_to_cluster, centers = k)
    return(kmeans_result)
  })
  
  # Output renderers
  output$data_summary <- renderText({
    req(data())
    paste("Data Summary:", nrow(data()), "rows", ncol(data()), "columns")
  })
  
  output$duplicated_rows <- renderText({
    req(data())
    duplicated_rows <- sum(duplicated(data()))
    paste("Duplicated rows:", duplicated_rows)
  })
  
  output$data_without_duplicates <- renderText({
    req(data_without_duplicates())
    paste("Data without duplicates:", nrow(data_without_duplicates()), "rows", ncol(data_without_duplicates()), "columns")
  })
  
  output$integer_columns <- renderText({
    req(data_without_duplicates())
    integer_cols <- c(
      "count" = is.integer(data_without_duplicates()$count),
      "total" = is.integer(data_without_duplicates()$total),
      "rnd" = is.integer(data_without_duplicates()$rnd),
      "age" = is.integer(data_without_duplicates()$age)
    )
    paste("Integer columns:", paste(names(integer_cols)[integer_cols], collapse = ", "))
  })
  
  output$na_summary <- renderText({
    req(data_without_duplicates())
    na_sum <- sum(is.na(data_without_duplicates()))
    paste("NA values:", na_sum)
  })
  
  output$outlier_summary <- renderText({
    req(data_without_duplicates())
    boxplot_data <- boxplot(data_without_duplicates()$total)
    outliers <- boxplot_data$out
    paste("Outliers:", paste(outliers, collapse = ", "))
  })
  
  output$boxplot <- renderPlot({
    req(data_without_duplicates())
    boxplot(data_without_duplicates()$total)
  })
  
  output$pie_chart <- renderPlot({
    df <- data_without_duplicates()
    x <- table(df$paymentType)
    percent <- paste0(round(100*x/sum(x)),"%")
    pie(x, labels = percent, main = "payment types", col = c("grey","black"))
    legend("bottomright", legend = c("cash","credit"), fill =c("grey","black") )
  })
  
  output$scatter_plot <- renderPlot({
    df <- data_without_duplicates()
    plot(x=df$age, y=df$total, xlab = "age", ylab = "total", col="blue", main = "age and sum of total spending")
  })
  
  output$bar_plot <- renderPlot({
    df <- data_without_duplicates()
    sorted_data <- sort(df$total, decreasing = TRUE)
    totalspending <- aggregate(df$total, by=list(df$city), FUN=sum)
    totalspending <- totalspending[order(-totalspending$x),]
    barplot(totalspending$x, names.arg=totalspending$Group.1, las=2, col = "green", main = "each city total spending")
  })
  
  output$histogram <- renderPlot({
    df <- data_without_duplicates()
    hist(df$total, col = "red", xlab = "total", ylab = "frequency", main = "frequency of the total spending")
  })
  
  output$outlier_text <- renderText({
    df <- data_without_duplicates()
    outlier <- boxplot(df$total)$out
    if (length(outlier) > 0) {
      paste("Outlier(s) found:", paste(outlier, collapse = ", "))
    } else {
      "No outlier found."
    }
  })
  
  output$scatter_plot1 <- renderPlotly({
    
    columns_to_cluster <- grouped_data() %>% select(avg_age, total_amount)
    plot_ly(x = columns_to_cluster$avg_age, y = columns_to_cluster$total_amount, color = factor(kmeans_results()$cluster),
            type = "scatter", mode = "markers", marker = list(size = 10)) %>%
      layout(title = "K-means Clustering", xaxis = list(title = "Age"), yaxis = list(title = "Total Spending"))
    
  })
  
  
  output$clusters_table <- renderTable({
    columns_to_cluster <- grouped_data() %>% select(avg_age, total_amount)
    kmeans_result_df <- data.frame(cluster = kmeans_results()$cluster, avg_age = columns_to_cluster$avg_age, total_amount = columns_to_cluster$total_amount)
    kmeans_result_df
  })
  
  
  output$association_results <- renderTable({
    req(data_without_duplicates())
    req(data())
    items <- data_without_duplicates()$items
    items_list <- strsplit(items, ",")
    transactions <- as(items_list, "transactions")
    inspect(transactions)
    # f1 <-input$support
    # f2 <- input$confidence
    # 
    rules=apriori(transactions,parameter = list(support=as.numeric(input$support) ,confidence=as.numeric(input$confidence) ))
    rules_data=as(rules , "data.frame")
    return(rules_data)
    #     if(f1<=1&&f1>=0.001&&f2<=1&&f2>=0.001){
    # 
    # }else{
    #   print("incorrect minimum support or confidence ")
    #   return()
    # } 
    # return()
  })
  
 
  }



shinyApp(ui = ui, server = server)