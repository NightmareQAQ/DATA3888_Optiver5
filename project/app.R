library(shiny)
library(shinyWidgets)
library(DT)
library(htmltools)
library(dplyr)
library(ggplot2)
library(rugarch)
library(ggplot2)
library(plotly)
library(tidyverse)
input_stock <- read.delim("./trades.csv", header=TRUE)


process_stock <- function(stock_data) {
  stock_data <- stock_data %>% mutate(
    WAP = (bid_price1 * ask_size1 + ask_price1 * bid_size1) / (bid_size1 + ask_size1),
    BidAskSpread = ask_price1 / bid_price1 - 1)
  stock_data <- stock_data %>% mutate(BidAskSpread = ask_price1 / bid_price1 - 1)
  
  
  # compute log return
  log_r1 <- list()
  time_IDs <- unique(stock_data[, 1])[1:5] # num of time_id
  for (i in 1 : length(time_IDs)) {
    sec <- stock_data %>% filter(time_id == time_IDs[i]) %>% pull(seconds_in_bucket)
    price <- stock_data %>% filter(time_id == time_IDs[i]) %>% pull(WAP)
    log_r <- log(price[-1] / price[1:(length(price) - 1)])
    log_r1[[i]] <- data.frame(time = sec[-1], log_return = log_r)
    time.no.change <- (1:600)[!(1:600 %in% log_r1[[i]]$time)]
    if (length(time.no.change) > 0) {
      new.df <- data.frame(time = time.no.change, log_return = 0)
      log_r1[[i]] <- rbind(log_r1[[i]], new.df)
      log_r1[[i]] <- log_r1[[i]][order(log_r1[[i]]$time), ]
    }
  }
  
  
  
  
  vol <- list()
  comp_vol <- function(x) {
    return(sqrt(sum(x ^ 2)))
  }
  for (i in 1 : length(log_r1)) {
    log_r1[[i]] <- log_r1[[i]] %>% mutate(time_bucket = ceiling(time / 30))
    vol[[i]] <- aggregate(log_return ~ time_bucket, data = log_r1[[i]], FUN = comp_vol)
    colnames(vol[[i]]) <- c('time_bucket', 'volatility')
  }
  
  
  list.reg <- list() # list for regression
  stock_data <- stock_data %>% mutate(time_bucket = ceiling(seconds_in_bucket / 30),
                                      num_order = bid_size1 + ask_size1 + bid_size2 + ask_size2)
  
  
  vol.train <- list()
  vol.val <- list()
  
  for (i in 1 : length(log_r1)) {
    vol.train[[i]] <- vol[[i]][1:16, ]
    vol.val[[i]] <- vol[[i]][-(1:16), ]
  }
  
  
  
  
  list.reg <- list() # list for regression
  stock_data <- stock_data %>% mutate(time_bucket = ceiling(seconds_in_bucket / 30),
                                      num_order = bid_size1 + ask_size1 + bid_size2 + ask_size2)
  len.train <- length(vol.train[[1]]$volatility)
  
  list.reg.val <- list()
  len.val <- length(vol.val[[1]]$volatility)
  
  
  
  
  list.HAV <- list() # list for regression
  quar <- list()
  list.HAV.val = list()
  
  
  
  comp_quar <- function(x) {
    return(length(x) / 3 * sum(x ^ 4))
  }
  
  
  
  for (i in 1 : length(log_r1)) {
    quar[[i]] <- aggregate(log_return ~ time_bucket, data = log_r1[[i]], FUN = comp_quar)
    colnames(quar[[i]]) <- c('time_bucket', 'quarticity')
  }
  
  
  
  for (i in 1 : length(vol)) {
    mean.vol <- rep(0, len.train - 5)
    for (j in 1 : 5) {
      mean.vol <- mean.vol + vol.train[[i]]$volatility[j : (j + len.train - 6)] / 5
    }
    list.HAV[[i]] <- data.frame(vol = vol.train[[i]]$volatility[-(1:5)], 
                                vol_1 = vol.train[[i]]$volatility[5:(len.train - 1)],
                                mean_vol_5 = mean.vol)
  }
  
  
  
  # new added
  for (i in 1 : length(vol)) {
    mean.vol <- rep(0, len.val)
    for (j in 12 : 16) {
      mean.vol <- mean.vol + vol[[i]]$volatility[j : (j + 3)] / 5
    }
    list.HAV.val[[i]] <- data.frame(vol = vol.val[[i]]$volatility, 
                                    vol_1 = vol[[i]]$volatility[16:19],
                                    mean_vol_5 = mean.vol)
  }
  
  
  
  
  
  for (i in 1 : length(vol)) {
    stats.bucket <- stock_data %>% 
      filter(time_id == time_IDs[i] & time_bucket != 0) %>% 
      select(c(BidAskSpread, WAP, num_order, time_bucket))
    mean.price <- aggregate(WAP ~ time_bucket, data = stats.bucket, FUN = mean)
    mean.order <- aggregate(num_order ~ time_bucket, data = stats.bucket, FUN = mean)
    mean.BAS <- aggregate(BidAskSpread ~ time_bucket, data = stats.bucket, FUN = mean)
    list.reg.val[[i]] <- 
      data.frame(volatility = vol.val[[i]]$volatility, 
                 price = mean.price$WAP[len.train:(len.train + len.val - 1)],
                 order = mean.order$num_order[len.train:(len.train + len.val - 1)],
                 BidAskSpread = mean.BAS$BidAskSpread[len.train:(len.train + len.val - 1)])
  }
  
  
  for (i in 1 : length(vol)) {
    stats.bucket <- stock_data %>% 
      filter(time_id == time_IDs[i] & time_bucket != 0) %>% 
      select(c(BidAskSpread, WAP, num_order, time_bucket)) 
    
    # for each 30-sec time bucket, we compute the following statistics
    mean.price <- aggregate(WAP ~ time_bucket, data = stats.bucket, FUN = mean)
    mean.order <- aggregate(num_order ~ time_bucket, data = stats.bucket, FUN = mean)
    mean.BAS <- aggregate(BidAskSpread ~ time_bucket, data = stats.bucket, FUN = mean)
    list.reg[[i]] <- data.frame(volatility = vol.train[[i]]$volatility[-1], 
                                price = mean.price$WAP[1:(len.train - 1)],
                                order = mean.order$num_order[1:(len.train - 1)],
                                BidAskSpread = mean.BAS$BidAskSpread[1:(len.train - 1)])
  }
  
  
  
  
  clustering_df <- data.frame(stock_id = numeric(),
                              time_id = numeric(),
                              mean_price = numeric(),
                              mean_BAS = numeric(),
                              sum_order = numeric(),
                              mean_volatility = numeric(),
                              max_diff_price = numeric())
  
  id =  unique(stock_data$stock_id)
  time_id  = unique(stock_data$time_id)
  
  for (i in 1: length(list.reg)) {
    new_summary = c(id,
                    time_id[i],
                    mean(list.reg[[i]]$price), 
                    mean(list.reg[[i]]$BidAskSpread), 
                    sum(list.reg[[i]]$order), 
                    mean(list.reg[[i]]$volatility), 
                    max(list.reg[[i]]$price)-min(list.reg[[i]]$price))
    clustering_df[nrow(clustering_df) + 1,] <- new_summary
    
  }
  
  
  
  
  
  
  
  
  # output <- c(stock_data, stock_data_info)
  #return(list(list.reg, stock_data_info,list.reg.val, list.HAV, quar, list.HAV.val))
  #return(list(list.reg, stock_data_info, list.reg.val, list.HAV, quar, list.HAV.val, arma_garch_models))
  return(list(list.reg, clustering_df, list.reg.val, list.HAV, quar, list.HAV.val, log_r1))
}

dir_short <- "./individual_book_train"

all_files_short_initial <- list.files(dir_short) # Select only the first 20 stock files
numbers <- sapply(all_files_short_initial, function(x) as.numeric(gsub("[^0-9]+", "", x)))
dir_short <- all_files_short_initial[order(numbers)]



all_files_short <- dir_short[1:5] # Select only the first 20 stock files


lst_regresssion_all = list()
lst.reg.val_all = list()
list.HAV_all = list()
quar_all = list()
list.HAV.val_all = list()
log_r1_all = list()
cluster_df = data.frame()


for (i in all_files_short) {
  # file_path <- paste0(dir_short, "/", i)
  file_path <- paste0("./individual_book_train/", i)
  stock_data <- read.csv(file_path)
  
  output <- process_stock(stock_data)
  stock_data_processed = output[[2]]
  cluster_df = rbind(cluster_df, stock_data_processed)
  list.reg = output[[1]]
  lst.reg.val = output[[3]]
  list.HAV = output[[4]]
  quar = output[[5]]
  list.HAV.val = output[[6]]
  log_r1 = output[[7]]
  stock_id <- sub(".*_(.*)\\..*", "\\1", i)
  lst_regresssion_all[[stock_id]] <- list.reg
  lst.reg.val_all[[stock_id]] = lst.reg.val
  list.HAV_all[[stock_id]] = list.HAV
  quar_all[[stock_id]] = quar
  list.HAV.val_all[[stock_id]] = list.HAV.val
  log_r1_all[[stock_id]] = log_r1
  
}

cluster_df = cluster_df[-c(171:175),]
set.seed(3888)
kmeans_result <- kmeans(cluster_df[, c("mean_price","mean_BAS", "mean_volatility", "max_diff_price")], centers = 3)
# Add the cluster results to the data set
cluster_df$cluster <- as.factor(kmeans_result$cluster)

all_stock_cluster = unique(cluster_df$stock_id)
all_timeid_cluster = unique(cluster_df$time_id)

all_stockid_monitor = unique(input_stock$stock_id)




ui <- navbarPage("Extended Shiny App", id = "tabs",
                 
                 tabPanel("Home",
                          tags$head(
                            tags$style(HTML("
  .home-background {
    background: url('home_2.png') no-repeat center center;
    background-size: cover;
    position: absolute;
    top: 5%;
    bottom: 0;
    left: 0;
    right: 0;
    z-index: -1;
  }
  .home-background::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    
    z-index: -1;
  }
  .navbar-right {
    float: right!important;
    margin-right: -15px;
  }
  .full-height {
    height: 100%; 
  }
  .full-width-height {
    width: 100%;
    height: 100%;
  }
  .detail-box {
    position: absolute;
    bottom: 5%;
    left: 10%;
    color: #ffffff;
    transform: translateY(10%);
  }
  .detail-box h1 {
    font-size: 3rem;
    font-weight: bold;
    text-transform: uppercase;
    color: #ffffff;
    margin-bottom: 15px;
  }
  .detail-box p {
    color: #ffffff;
  }
  .custom-detail-box {
    position: absolute;
    bottom: 0;
    left: 0;
  }
  .btn-container {
    position: absolute;
    top: 0;
    right: 0;
    border: 2px solid white;
    padding: 20px;
    display: inline-block;
    background-color: rgba(0, 0, 0, 0.3);
  }
  .transparent-button {
    background-color: transparent;
    color: white;
    border: none;
  }
  .carousel_btn-box {
    display: none;
  }
"))
                          ),
                          div(class = "home-background",
                              div(class = "full-width-height",
                                  fluidRow(class = "full-height",
                                           column(12, align = "left", # 更改为 align = "left"
                                                  
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$br(),
                                                  tags$div(class = "carousel_btn-box",
                                                           tags$a(class = "carousel-control-prev", href = "#", role = "button", "data-slide" = "prev",
                                                                  tags$i(class = "fa fa-angle-left", "aria-hidden" = "true"),
                                                                  tags$span(class = "sr-only", "Previous")
                                                           ),
                                                           tags$a(class = "carousel-control-next", href = "#", role = "button", "data-slide" = "next",
                                                                  tags$i(class = "fa fa-angle-right", "aria-hidden" = "true"),
                                                                  tags$span(class = "sr-only", "Next")
                                                           )
                                                  ),
                                                  #tags$h1("DATA3888", align = "center", style = "font-size: 3rem; color: white;"),
                                                  #tags$h3("Optver5", align = "center", style = "font-size: 2rem; color: white;"),
                                                  tags$br(),
                                                  div(class = "btn-container navbar-right",
                                                      actionButton("goToTab1", "Monitor", class = "btn-primary transparent-button", style = "width: 150px; height: 40px; font-size: 25px; margin: 10px;"),
                                                      actionButton("goToTab2", "Model", class = "btn-primary transparent-button", style = "width: 150px; height: 40px; font-size: 25px; margin: 10px;")
                                                  )
                                           )
                                  )
                              )
                          )
                 )
                 
                 
                 ,
                 tabPanel("Monitor",
                          fluidPage(
                            titlePanel("Stock Volatility Monitor"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "stock_id",
                                            label = "Stock_ID",
                                            choices = all_stockid_monitor,
                                            selected = 8382),
                                numericInput("time_id_start", "Time ID Start:", 1),
                                numericInput("time_id_end", "Time ID End:", 3),
                                numericInput("percentile", "Percentile:", 0.75, min = 0, max = 1, step = 0.01),
                                numericInput("volume", "Volume:", 10),
                                
                                actionButton("run_button", "Run")
                              ),
                              
                              mainPanel(
                                tags$h2("Monitor Results", align = "center"),
                                tags$br(),
                                DTOutput("view"),
                                tags$br(),
                                tags$br(),
                                plotlyOutput("linePlot")
                              )
                            )
                          )
                 ),
                 tabPanel("Cluster_Model",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "stock_cluster",
                                          label = "Stock_ID",
                                          choices = all_stock_cluster,
                                          selected = "0"),
                              selectInput(inputId = "time_id",
                                          label = "Time_ID",
                                          choices = all_timeid_cluster,
                                          selected = "0"),
                              actionButton("run_cluster", "Run Cluster Analysis"),
                              actionButton("go_to_about", "Model Introduction")
                            ),
                            mainPanel(
                              tags$h2("Cluster & Model", align = "center"),
                              tags$br(),
                              tags$br(),
                              verbatimTextOutput("cluster"),
                              tags$br(),
                              tags$br(),
                              tags$h3("Cluster DataFrame", align = "center"),
                              DT::dataTableOutput("cluster_dt"),
                              tags$br(),
                              tags$br(),
                              tags$h3("Results of MSE & MAPE for different model", align = "center"),
                              div(
                                style = "display: flex; justify-content: space-between; position: relative;",
                                plotlyOutput("ModelPlot", width = "50%"),
                                tags$div(
                                  style = "width: 2px; background-color: green; position: absolute; top: 0; bottom: 0; left: 50%; transform: translateX(-50%);"
                                ),
                                plotlyOutput("Model2Plot", width = "50%")
                              ),
                              tags$br(),
                              tags$br(),
                              tags$h3("Comparison of true and predicted volatility for different model", align = "center"),
                              plotlyOutput("LineREPlot"),
                              tags$br(),
                              plotlyOutput("LineHAVPlot")
                            )
                          )
                 )
                 ,
                 tabPanel("Model_Intro",
                          tags$iframe(src = "service.html", height = "800px", width = "100%", frameborder = "0")
                 )
                 ,
                 tabPanel("About",
                          tags$iframe(src = "about.html", height = "800px", width = "100%", frameborder = "0")
                 )
                 
                 
                 
)

server <- function(input, output, session) {
  output$distPlot1 <- renderPlot({
    if (input$dist1 == "norm") {
      dist <- rnorm(input$obs1)
    } else if (input$dist1 == "unif") {
      dist <- runif(input$obs1)
    }
    hist(dist)
  })
  
  output$distType1 <- renderText({
    paste("In Monitor, you have selected", input$dist1, "distribution.")
  })
  
  output$distPlot2 <- renderPlot({
    if (input$dist2 == "norm") {
      dist <- rnorm(input$obs2)
    } else if (input$dist2 == "unif") {
      dist <- runif(input$obs2)
    }
    hist(dist)
  })
  
  output$distType2 <- renderText({
    paste("In Cluster_Model, you have selected", input$dist2, "distribution.")
  })
  
  
  output$homeTable <- renderDataTable({
    # Your data here. Here is a simple example with iris dataset
    #input_stock <- read.delim("./trades.csv", header=TRUE)
    #input_stock
    iris
  })
  
  # The new server code for Monitor tab
  observeEvent(input$run_button, {
    # Read and filter data
    first_five_time_id <- unique(input_stock$time_id)[input$time_id_start:input$time_id_end]
    filtered_data <- input_stock %>% filter(time_id %in% first_five_time_id & stock_id == input$stock_id ) 
    # Run rolling function
    result <- rolling(input$volume, filtered_data, input$percentile)
    # Show result
    output$view <- renderDT({result[[1]]})
    # ggplot
    output$linePlot <- renderPlotly({
      p <- ggplotly(result[[2]])
      p
    })
  })
  observeEvent(input$run_cluster, {
    index_vec = as.vector(replicate(5, 1:5))  
    cluster_df = cluster_df %>% mutate(index = index_vec)
    
    filtered_cluster = cluster_df %>% filter(stock_id == input$stock_cluster & time_id == input$time_id) 
    output_cluster = filtered_cluster$cluster
    index =  filtered_cluster$index
    
    str_stockid = as.character(1)
    train_reg = lst_regresssion_all[[str_stockid]][[index]]
    test_reg = lst.reg.val_all[[str_stockid]][[index]]
    train_hav = list.HAV_all[[str_stockid]][[index]]
    test_hav = list.HAV.val_all[[str_stockid]][[index]]
    quar = quar_all[[str_stockid]][[index]]
    train_arma = log_r1_all[[str_stockid]][[index]]
    
    
    # Check which cluster
    
    output$cluster = {
      
      renderText(
        paste("The time span you choose is belong to cluster ",output_cluster)
      )
    }
    
    # cluster_dt
    output$cluster_dt = DT::renderDataTable({
      DT::datatable(filtered_cluster)
    })
    # Regression model fitting
    # 检查 final_regression$volatility 中是否有零值
    zero_indices1 <- train_reg$volatility == 0
    
    
    # 如果存在零值，将它们替换为其他 volatility 值的最小值
    if (any(zero_indices1)) {
      min_quarticity1 <- min(train_reg$volatility[!zero_indices1])
      train_reg$volatility[zero_indices1] <- min_quarticity1
    }
    
    zero_indices_pre_1<- is.na(train_reg$price)
    if (any(zero_indices_pre_1)) {
      min_quarticity1 <- min(train_reg$volatility[!zero_indices_pre_1])
      train_reg$volatility[zero_indices_pre_1] <- min_quarticity1
      min_price1 <- min(train_reg$price[!zero_indices_pre_1])
      train_reg$price[zero_indices_pre_1] <- min_price1
      min_order1 <- min(train_reg$order[!zero_indices_pre_1])
      train_reg$order[zero_indices_pre_1] <- min_order1
      min_BidAskSpread1 <- min(train_reg$BidAskSpread[!zero_indices_pre_1])
      train_reg$BidAskSpread[zero_indices_pre_1] <- min_BidAskSpread1
    }
    
    #cluster1 fit model
    lm.models<- lm(volatility ~ price + order + BidAskSpread, train_reg, weight = 0.8 ^ (((length(train_reg$volatility)+1 - 2):0) / 2))
    #cluster1 predit
    pred.lm <- predict(lm.models, newdata = test_reg)
    
    
    
    # Regression Accuracy
    
    #Accuracy for cluster1
    MSE.lm  = mean((test_reg$volatility - pred.lm) ^ 2)
    MAPE.lm <- median(abs((test_reg$volatility - pred.lm)/test_reg$volatility)) * 100
    
    
    
    
    #train_hav = list.HAV_all[[str_stockid]][index]
    #test_hav = list.HAV.val_all[[str_stockid]][index]
    #quar = quar_all[[str_stockid]][index]
    #HAV model fitting
    
    # 检查 final_hav_quar$quarticity 中是否有零值
    zero_indices1 <- quar$quarticity == 0
    
    # 如果存在零值，将它们替换为其他 quarticity 值的最小值
    if (any(zero_indices1)) {
      min_quarticity1 <- min(quar$quarticity[!zero_indices1])
      quar$quarticity[zero_indices1] <- min_quarticity1
    }
    
    
    
    HAV.wls.models <- lm(vol ~ vol_1 + mean_vol_5, train_hav,
                         weights = train_hav$vol_1 / 
                           sqrt(quar$quarticity[5:(length(train_hav$vol)+1 - 1)]))
    
    
    pred.HAV<- predict(HAV.wls.models, newdata = test_hav)
    
    
    
    
    
    #Accuracy for cluster1
    MSE.HAV  = mean((train_hav$vol - pred.HAV) ^ 2)
    # Calculate MAPE
    MAPE.HAV <- median(abs((train_hav$vol - pred.HAV)/train_hav$vol)) * 100
    
    
    #arma garch
    filter_log_r1 <- function(log_r1) {
      log_r1 %>% filter(time <= 480) %>% pull(log_return)
    }
    
    
    final_log_r1_c1 = filter_log_r1(train_arma)
    
    spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 1)), 
                       distribution.model = "norm")
    #final_log_r1_c1
    garch.models_c1 <- ugarchfit(spec = spec, data = final_log_r1_c1,solver = 'hybrid')
    fspec <- getspec(garch.models_c1)
    setfixed(fspec) <- as.list(coef(garch.models_c1))
    future.path <- fitted(ugarchpath(fspec, n.sim = 30, m.sim = 1000))
    # Due to numerical issues, sometimes NA value can be produced 
    # We simply replace NA value with 0; you may come up with a better idea in your own project
    future.path[is.na(future.path)] <- 0 
    RV.pred <- mean(sqrt(colSums(future.path ^ 2)))
    
    
    #accuracy for cluster1
    MSE.ARMA_GARCH  = mean((test_hav$vol - RV.pred) ^ 2)
    
    MAPE.ARMA_GARCH <- median(abs((test_hav$vol - RV.pred)/test_hav$vol)) * 100
    
    
    
    
    
    
    
    
    
    
    #normalize 
    
    
    mse_data_adj <- data.frame(
      Model = factor(rep(c("Linear Regression", "HAV", "Arma-Garch"), )),
      MSE = c(MSE.lm, MSE.HAV, MSE.ARMA_GARCH),
      MAPE = c(MAPE.lm, MAPE.HAV, MAPE.ARMA_GARCH)
    )
    # Reshape the data
    mse_data_adj_long <- mse_data_adj %>% 
      pivot_longer(cols = c(MSE, MAPE), names_to = "Metric", values_to = "Value")
    
    # Separate the data into two data frames based on Metric
    mse_data_adj_long_mse <- mse_data_adj_long %>% filter(Metric == "MSE")
    mse_data_adj_long_mape <- mse_data_adj_long %>% filter(Metric == "MAPE")
    
    output$ModelPlot <- renderPlotly({
      # Plot for MSE
      ggplotly(ggplot(data = mse_data_adj_long_mse, aes(x = Model, y = Value)) + 
                 geom_bar(stat = "identity", position = "dodge") +
                 theme_minimal() +
                 labs(x = "Model", y = "MSE") +
                 ggtitle("MSE Comparison"))
      
    })
    output$Model2Plot <- renderPlotly({
      # Plot for MAPE
      ggplotly(ggplot(data = mse_data_adj_long_mape, aes(x = Model, y = Value)) + 
                 geom_bar(stat = "identity", position = "dodge") +
                 theme_minimal() +
                 labs(x = "Model", y = "MAPE") +
                 ggtitle("MAPE Comparison"))
      
      
    })
    output$LineREPlot <- renderPlotly({
      # 创建数据框
      df <- data.frame(Value1 = test_reg$volatility, Value2 = pred.lm)
      
      # 绘制图表
      p <- ggplot(df, aes(x = 1:length(test_reg$volatility))) +
        geom_line(aes(y = Value1, colour = "Actual volatility")) +
        geom_line(aes(y = Value2, colour = "Predicted volatility")) +
        labs(title = "Comparison of actual and predicted volatility", x = "Time bucket", y = "Volatility", colour = "actual vs. predicted") +
        scale_colour_manual(values = c("blue", "red")) +
        ggtitle("Linear Regression Model")+
        theme_minimal()
      
      p <- ggplotly(p)
      p
    })
    output$LineHAVPlot <- renderPlotly({
      # 创建数据框
      df <- data.frame(Value1 = test_hav$vol, Value2 = pred.HAV)
      
      # 绘制图表
      p2 <- ggplot(df, aes(x = 1:length(test_hav$vol))) +
        geom_line(aes(y = Value1, colour = "Actual volatility")) +
        geom_line(aes(y = Value2, colour = "Predicted volatility")) +
        labs(title = "Comparison of actual and predicted volatility", x = "Time bucket", y = "Volatility", colour = "actual vs. predicted") +
        scale_colour_manual(values = c("blue", "red")) +
        ggtitle("HAV Model")+
        theme_minimal()
      P2 <- ggplotly(p2)
      p2
    })
    
  })
  output$imageBox <- renderImage({
    # You can use any PNG or JPG image here
    list(src = "./picture.png", contentType = "image/png", width = 400)
  }, deleteFile = FALSE)
  observeEvent(input$goToTab1, {
    updateNavbarPage(session, "tabs", selected = "Monitor")
  })
  
  observeEvent(input$goToTab2, {
    updateNavbarPage(session, "tabs", selected = "Cluster_Model")
  }
  )
  observeEvent(input$go_to_about, {
    updateTabsetPanel(session, "tabs", selected = "Model_Intro")
  })
  
  
  # Define functions here
  process_data <- function(historical_stock_file) {
    unique_time_ids <- unique(historical_stock_file$time_id)
    results <- tibble()
    
    offset <- 0
    for (i in seq_along(unique_time_ids)) {
      current_time_id <- unique_time_ids[i]
      
      current_group <- historical_stock_file %>% filter(time_id == current_time_id)
      
      if (i > 1) {
        offset <- max(results$seconds_in_bucket) + 1 - min(current_group$seconds_in_bucket)
      }
      
      current_group <- current_group %>% mutate(seconds_in_bucket = seconds_in_bucket + offset)
      results <- bind_rows(results, current_group)
    }
    
    input_stock <- results
    # process log_r
    input_stock = as.data.frame(input_stock)
    
    log_r1 <- data.frame()
    time_IDs <- unique(input_stock[, 1])
    time_IDs = time_IDs[1:3]
    
    sec <- input_stock  %>% pull(seconds_in_bucket)
    price <- input_stock %>% pull(price)
    log_r <- log(price[-1] / price[1:(length(price) - 1)])
    log_r1 <- data.frame(time = sec[-1], log_return = log_r)
    time.no.change <- (1:max(sec))[!(1:max(sec) %in% log_r1$time)]
    if (length(time.no.change) > 0) {
      new.df <- data.frame(time = time.no.change, log_return = 0)
      log_r1 <- rbind(log_r1, new.df)
      log_r1 <- log_r1[order(log_r1$time), ]
    }
    
    # volatility
    vol <- data.frame()
    comp_vol <- function(x) {
      return(sqrt(sum(x ^ 2)))
    }
    
    log_r1 <- log_r1 %>% mutate(time_bucket = ceiling(time / 30))
    vol <- aggregate(log_return ~ time_bucket, data = log_r1, FUN = comp_vol)
    colnames(vol) <- c('time_bucket', 'volatility')
    
    # quarticity
    HAV_dataframe <- data.frame()
    quar <- data.frame()
    
    comp_quar <- function(x) {
      return(length(x) / 3 * sum(x ^ 4))
    }
    
    quar <- aggregate(log_return ~ time_bucket, data = log_r1, FUN = comp_quar)
    colnames(quar) <- c('time_bucket', 'quarticity')
    
    mean.vol <- rep(0, length(vol$volatility) - 5)
    for (j in 1 : 5) {
      mean.vol <- mean.vol + vol$volatility[j : (j + length(vol$volatility) - 6)] / 5
    }
    HAV_dataframe <- data.frame(time_bucket = vol$time_bucket[-(1:5)], 
                                vol = vol$volatility[-(1:5)], 
                                vol_1 = vol$volatility[5:(length(vol$volatility) - 1)],
                                mean_vol_5 = mean.vol)
    
    return(list(log_r1, vol, quar, HAV_dataframe))
  }
  
  
  cal_threshold_value <- function(history_stock, percentile) {
    threshold = quantile(history_stock$vol, probs = percentile)
    return(threshold)
  }
  
  
  monitor_func = function(history_data, quar_data, monitor_data, percentile_threshold){
    
    quar_train = quar_data
    HAV_train = history_data
    HAV_test = monitor_data
    
    HAV.wls.models <- lm(vol ~ vol_1 + mean_vol_5, HAV_train,
                         weights = HAV_train$vol_1 / 
                           sqrt(quar_train$quarticity))
    pred.HAV <- predict(HAV.wls.models, newdata = HAV_test)
    
    threshold = cal_threshold_value(history_data, percentile_threshold)
    
    if (pred.HAV > threshold){
      return(c(TRUE, pred.HAV))
    }
    
    if (pred.HAV <= threshold){
      return(c(FALSE, pred.HAV))
    }
  }
  
  
  rolling <- function(train_volume, combined_data, threshold_per) {
    all_data = process_data(combined_data)
    all_quar = all_data[[3]]
    all_HAV = all_data[[4]]
    
    true_vol =  all_HAV$vol[train_volume+1:(length(all_HAV$vol)-train_volume)]
    risky_bucket = c()
    risky_predicted_vol = c()
    all_predict = c()
    all_threshold = c()
    
    
    i = 1
    #train_volume = 30
    train_HAV = all_HAV[i:train_volume,]
    test_HAV = all_HAV[train_volume + 1,]
    quar_HAV = all_quar[(i+4):(train_volume+5-1),]
    
    while (train_volume <= length(all_HAV$vol)-1){
      #print(i)
      #print(train_volume)
      #print(length(train_HAV$vol_1))
      #print(length(quar_HAV$quarticity))
      monitor_result = monitor_func(train_HAV, quar_HAV, test_HAV, threshold_per)
      if(monitor_result[1] == TRUE){
        risky_predicted_vol = c(risky_predicted_vol,monitor_result[2])
        risky_bucket = c(risky_bucket,test_HAV$time_bucket)
      }
      
      all_predict = c(all_predict, monitor_result[2])
      all_threshold = c(all_threshold, cal_threshold_value(train_HAV,threshold_per))
      
      i = i+1
      train_volume = train_volume+1
      train_HAV = all_HAV[i:train_volume,]
      test_HAV = all_HAV[train_volume + 1,]
      quar_HAV = all_quar[(i+4):(train_volume+5-1),]
    }
    
    risky_df <- data.frame(
      risky_time_start = (risky_bucket-1)*30+1,
      risky_time_end = risky_bucket*30,
      predicted_volatility = risky_predicted_vol)
    
    
    ploting_df = data.frame(
      true_volatility = true_vol,
      threshold = all_threshold,
      predicted_volatility = all_predict
    )
    
    plot = ggplot(ploting_df, aes(x = 1:length(ploting_df$true_volatility))) +
      geom_line(aes(y = ploting_df$true_volatility, colour = "True volatility")) +
      geom_line(aes(y = ploting_df$threshold, colour = "threshold")) +
      geom_line(aes(y = ploting_df$predicted_volatility, colour = "Predicted volatility")) +
      labs(title = "True volatility VS. Predicted volatility Vs. Threshold", x = "Bucket", y = "volatility", colour = "Vector") +
      scale_colour_manual(values = c("blue","red", "green")) +
      theme_minimal()
    
    return(list(risky_df, plot))
  }
  
  # Add Cluster Tab function here:
  # Process_stock-function
}

shinyApp(ui = ui, server = server)
