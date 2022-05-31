#######################################################
# SHINY - CUSTOMER SEGMENTATION - AUTO - MARKET    ####
#######################################################


library(reshape2)
library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(factoextra)
library(tidyverse)
library(cluster)

#######################################################
# DATA SOURCES                                        #
#######################################################

### READ DATA  ####

Data <- read.csv("Train.csv")

######################################################
#  FUNCTIONS                                         #
######################################################

### clean data ###

cleandata <- function(data){
  
  #   #### Adjust variables to use - Columns 2 to 9###
  
  MarketData <- data[,2:9]
  
  #   summary(MarketData)
  
  ### convert all blank spaces in NA and eliminate all NAs with "complete.cases"###
  ### Check categories different from "No"-"yes"
  
  unique(MarketData$Ever_Married)
  
  MarketData[MarketData == ""] <- NA
  MarketData <- MarketData[complete.cases(MarketData),]
  data <- MarketData
  return(data)
  
  
}

dataprep <- function(datap){
  
  numdatap <- datap %>% select_if(is.numeric)
  chardatap <- datap %>% select_if(is.factor)
  chardatap$Gender <- as.character(chardatap$Gender)
  chardatap$Ever_Married <- as.character(chardatap$Ever_Married)
  chardatap$Graduated <- as.character(chardatap$Graduated)
  chardatap$Profession <- as.character(chardatap$Profession)
  chardatap$Spending_Score <- as.character(chardatap$Spending_Score)
  
  ### char binary conversion and remove of dummy variable
  chardatap_bin <- dummy_cols(chardatap, remove_most_frequent_dummy = TRUE)
  
  ### final working data set ####
  
  Final_MarketData <- cbind(numdatap,chardatap_bin[,6:18])
  Final_MarketData[,1:16] <- scale(Final_MarketData[,1:16])
  
  return(Final_MarketData)
  
}


desc_plot <- function (var){
  
  data_desc <- cleandata(Data)
  
  if (var == "Gender") {
    D_Plot <- ggplot(data_desc,aes(Gender)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "red") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + ggtitle("Gender")
  }
  
  if (var == "Married") {
    D_Plot <- ggplot(data_desc,aes(Ever_Married)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "blue") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") +xlab("Ever married") + ggtitle("Ever Married")
  }
  
  if (var == "Graduated") {
    D_Plot <- ggplot(data_desc,aes(Graduated))+ geom_bar(aes(y = (..count..)/sum(..count..)), fill = "purple") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + ggtitle("Graduated")
  }
  
  if (var == "Profession") {
    D_Plot <- ggplot(data_desc,aes(Profession)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "pink") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + ggtitle("Profession")
  }
  
  if (var == "Spending_Score") {
    D_Plot <- ggplot(data_desc,aes(Spending_Score)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "violet") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + xlab("Spending score") + ggtitle("Spending Score")
  }
  
  if (var == "Age") {
    D_Plot <- ggplot(data_desc,aes(Age)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "orange") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + ggtitle("Age")
  }
  
  if (var == "Work_Experience") {
    D_Plot <- ggplot(data_desc,aes(Work_Experience)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "green") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + xlab("Work experience (years)")+ ggtitle("Work Experience")
  }
  
  if (var == "Family_Size") {
    D_Plot <- ggplot(data_desc,aes(Family_Size)) + geom_bar(aes(y = (..count..)/sum(..count..)), fill = "brown") + scale_y_continuous(labels=scales::percent) + ylab("relative frequencies") + xlab("Family size")+ ggtitle("Family Size")
  }
  
  return(D_Plot)
}


market_plot <- function(var2,k){
  
  # Final_MarketData_c <-cbind(Final_MarketData,cluster$cluster) Normalized data
  # seg_var <- input$segment_var
  datap <- cleandata(Data)
  Final_MarketData <- dataprep(datap)
  # k <- input$kvalue
  set.seed(1584)# same results to each call
  cluster <- kmeans(Final_MarketData, centers = k, iter.max = 10)
  MarketData_c <- cbind(datap,cluster$cluster) #Original cleaned customer data
  names(MarketData_c)[9] <- "New_Segments" #Change name of cluster$cluster column
  ###########################################################################
  # MarketData_c is the detailed data to export to work with other tools ####
  ###########################################################################
  
  if (var2 == "Gender") {
    C_Plot <- ggplot(MarketData_c, aes(Gender, fill = New_Segments)) + geom_bar( position = "dodge")  + facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Gender by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Married") {
    C_Plot <- ggplot(MarketData_c, aes(Ever_Married, fill = New_Segments)) + geom_bar( position = "dodge")  + facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Married by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Graduated") {
    C_Plot <- ggplot(MarketData_c, aes(Graduated, fill = New_Segments)) + geom_bar( position = "dodge") + facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Graduated by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Profession") {
    C_Plot <- ggplot(MarketData_c, aes(Profession, fill = New_Segments)) + geom_bar( position = "dodge") + facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Profession by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Spending_Score") {
    C_Plot <- ggplot(MarketData_c, aes(Spending_Score, fill = New_Segments)) + geom_bar( position = "dodge")+ facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Spending Score by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Age") {
    C_Plot <- ggplot(MarketData_c, aes(Age, fill = New_Segments)) + geom_bar( position = "dodge") + facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Age by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Work_Experience") {
    C_Plot <- ggplot(MarketData_c, aes(Work_Experience, fill = New_Segments)) + geom_bar( position = "dodge")+ facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Work Experience by Cluster") + ylab("Number of customers")
  }
  
  if (var2 == "Family_Size") {
    C_Plot <- ggplot(MarketData_c, aes(Family_Size, fill = New_Segments)) + geom_bar( position = "dodge")+ facet_grid(MarketData_c$New_Segments ~ .) + theme_bw() + ggtitle("Family Size by Cluster") + ylab("Number of customers")
  }
  return(C_Plot)
}

final_segments_plot <- function (k,min,max){
  
  ## run kmeans cluster algorithm ###
  
  datap <- cleandata(Data)
  Final_MarketData <- dataprep(datap)
  set.seed(1584)
  cluster <- kmeans(Final_MarketData, centers = k, iter.max = 10)
  centers <- as.data.frame(cluster$centers)
  centers$clust <- c(1:k)
  
  #### organice the centers table for better plot analysis ####
  
  Seg <- melt(centers, id.vars = "clust", measure.vars = 
                c("Age",
                  "Work_Experience",
                  "Family_Size",
                  "Gender_Female",
                  "Ever_Married_No",
                  "Graduated_No",
                  "Profession_Doctor",
                  "Profession_Engineer",
                  "Profession_Entertainment",
                  "Profession_Executive",
                  "Profession_Healthcare",
                  "Profession_Homemaker",
                  "Profession_Lawyer",
                  "Profession_Marketing",
                  "Spending_Score_Average",
                  "Spending_Score_High"))
  
  ###  Final segments data plot ####
  
  Seg_plot <- ggplot(Seg, aes(clust,value,fill = clust)) + geom_col(position = "dodge") + geom_hline(yintercept = max, linetype = 2, color = "red", size = 1) + geom_hline(yintercept = min, linetype = 2, color = "red", size = 1) + facet_grid(cols = vars(variable)) + theme_bw() + ggtitle("Segments and variables") + xlab("New segments (clust)")
  
  return(Seg_plot)
  
}


#######################################################
# USER INTERFACE                                      #
#######################################################
ui <- dashboardPage(
  dashboardHeader(
    title = h3("Market Segmentation"),
    titleWidth = 300,
    disable = F),
  dashboardSidebar(### your logo here ###
    img(src = "logo-vertical-2022_388x300.jpg",height = 388, width = 300, align = "center"),
    hr(),
    h5(HTML("DATA UPLOAD AND CLEANING"), style="text-align:center"),
    hr(),
    disable = F,
    width = 300,
    collapsed = F,
    actionButton("case_story"," 1.Read: case story"), #  id input$case_story
    actionButton("Data"," 2.Read: data source"),
    actionButton(inputId = "upload",
                 label = " 3.Data upload ",
                 icon =  icon("refresh")),
    actionButton("cleandata"," 4.Read: data cleaning"),  
    actionButton(inputId = "clean",
                 label = " 5.Data cleaning ",
                 icon =  icon("refresh")),
    hr(),
    h5(HTML("DESCRIPTIVE ANALYSIS"), style="text-align:center"),
    hr(),
    actionButton("descriptive"," 6. Read: decriptive analysis"),
    selectInput("desc_var","Descriptive variables",
                choices = list(
                  "Gender" = "Gender",
                  "Married" = "Married",
                  "Graduated" = "Graduated",
                  "Profession" = "Profession",
                  "Spending score" = "Spending_Score",
                  "Age"= "Age",
                  "Work experience" = "Work_Experience",
                  "Family size" = "Family_Size"),
                selected = "Gender"),
    actionButton("Plot","Plot", icon("option-vertical",lib ="glyphicon")),
    hr(),
    h5(HTML("PREPARE THE DATA SET"), style="text-align:center"),
    hr(),
    actionButton("prepare_data"," 7.Read: preparing the data set"),
    actionButton(inputId = "dataprep",
                 label = "8.Data preparation ",
                 icon =  icon("refresh")),
    hr(),
    h5(HTML("NUMBER OF CLUSTERS (K)"), style="text-align:center"),
    hr(),
    actionButton("parameter"," 8.Read: Select k-means parameter"),
    actionButton(inputId = "elbow",
                 label = "9.Elbow method",
                 icon =  icon("refresh")),
    sliderInput(
      inputId = "kvalue",
      label= "K value for silhouette method",
      min = 1,
      max = 10,
      value = 4),
    actionButton(inputId = "silhouette",
                 label = "10.Silhouette method",
                 icon =  icon("refresh")),
    hr(),
    h5(HTML("SEGMENTS DEFINITION"), style="text-align:center"),
    hr(),
    actionButton("newsegments"," 9.Read: New segments"),
    selectInput("segment_var","New segments vs variables",
                choices = list(
                  "Gender" = "Gender",
                  "Married" = "Married",
                  "Graduated" = "Graduated",
                  "Profession" = "Profession",
                  "Spending score" = "Spending_Score",
                  "Age"= "Age",
                  "Work experience" = "Work_Experience",
                  "Family size" = "Family_Size"),
                selected = "Gender"),
    actionButton("new_s","Plot new segments", icon("option-vertical",lib ="glyphicon")),
    actionButton("finalsegments"," 10.Read: Final segments"),
    sliderInput(
      inputId = "range",
      label= "Select the range",
      min = -0.4,
      max = 0.4,
      value = c(-0.3,0.3)),
    actionButton(inputId = "finaldata_c",
                 label = "11. Create the segments table",
                 icon =  icon("refresh")),
    actionButton("finalsegments_c","12. Final segments plot"),
    hr(),
    tags$div(class="header", checked=NA,
             tags$p("¿DO YOU WANT TO TELL US ABOUT YOUR BUSINESS CASE?", align = "center"),
             tags$a(href="##YOUR URL SITE HERE###" , h5("CONTACT US HERE", align = "center", target = "_blank"))
    ),
    hr(),
    tags$div(class="header", checked=NA,
             tags$a(href="### YOUR MAIN MANU URL HERE #### ", h5("GO TO MAIN PAGE", align = "center", target = "_blank"))
    ),
    hr(),
    actionButton("refresh","Refresh", icon("refresh")),
    hr()
  ),
  
  
  
  dashboardBody(
    
    
    box(
      title = "Upload data",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("Data")
    ),
    
    box(
      title = "Cleaned data",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("MarketData")
    ),
    
    box(
      title = "Descriptive analysis",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId ="D_Plot")
    ),
    
    box(
      title = "Prepared data set",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("finaldata")
    ),
    
    box(
      title = "K paramter - elbow method",
      status = "info",
      width = 12,
      height = 750,
      solidHeader = T,
      uiOutput("fviz_nbclust")
    ),
    
    box(
      title = "K parameter - silhouette method",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "fviz_silhouette")
    ),
    
    box(
      title = "New cluster analysis",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "C_Plot")
    ),
    
    box(
      title = "Final data cluster",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      DT::dataTableOutput("centers")
    ),
    
    box(
      title = "New segments",
      status = "info",
      width = 12,
      height = 600,
      solidHeader = T,
      plotOutput(outputId = "Seg_plot")
    )
    
  )
  
)




#######################################################
# server                                              #
#######################################################

server <- function( input, output, session){
  
  ###### DATA UPLOAD AND CLEANING ######################
  
  observeEvent(input$case_story,{
    
    showModal(modalDialog(
      
      title = "CUSTOMER SEGMENTATION STORY CASE - AUTO INDUSTRY ",
      HTML("
         An automobile company has plans to enter new markets with 
         their existing products (P1, P2, P3, P4 and P5). After 
         intensive market research, they’ve deduced that the behavior
         of new market is similar to their existing market.<br><br>
         
         Content<br><br>

         In their existing market, the sales team has classified all
         customers into 4 segments (A, B, C, D ). Then, they performed
         segmented outreach and communication for different segment of
         customers. This strategy has work exceptionally well for them.
         They plan to use the same strategy on new markets and have 
         identified 8,068 new potential customers.<br><br>
         You are required to help the manager to predict the right group of the
         new customers"),
    
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", # window size
      easyClose = T,
      fade = T #Effect
      
    ))
    
  })
  
  observeEvent(input$Data,{
    
    showModal(modalDialog(
      
      title = "DATA SOURCE ",
      HTML("Original data source is from Kaggle, from the <br>
            AV - Janatahack customer segmentation data base<br>      
            from: https://www.kaggle.com/vetrirah/customer"),
     
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T 
      
    ))
    
  })
  
  observeEvent(input$upload,{
    updateActionButton(                                  
      session = session,
      inputId = "upload",
      label = "3. Loaded data",
      icon = icon("ok",lib ="glyphicon")
    )
    output$Data <- 
      output$results <- DT::renderDataTable(
        Data,
        options = list(scrollX = TRUE)
      )
  })
  
  observeEvent(input$cleandata,{
    
    showModal(modalDialog(
      
      title = "DATA CLEANING ",
      HTML("The 'ID' and 'Var_1' columns do not add value, will be deleated; <br>
      same with 'Segmentation' columns, it refers to the old segments. Rows with <br>
      NA values will be deleted, other inconsistent values will be deleted."),
      
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T 
      
    ))
    
  })
  
  
  
  
  
  observeEvent(input$descriptive,{
    
    showModal(modalDialog(
      
      title = "DESCRIPTIVE ANALYSIS",
      HTML("Select the variable to plot. A bar plot will give some insights about the data patterns. <br>
           The traditional segmentation is based on descriptive information, but it does not have <br>
           the multi-variable deep analysis obtained by the cluster algorithm that will be applied <br>
           in the next section..." 
      ),
    
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T
      
    ))
    
  })
  
  
  
  observeEvent(input$clean,{
    updateActionButton(                                   
      session = session,
      inputId = "clean",
      label = " 5.Cleaned data",
      icon = icon("ok",lib ="glyphicon")
    )
    MarketData <- cleandata(Data)
    output$MarketData <- 
      output$results <- DT::renderDataTable(
        MarketData,
        options = list(scrollX = TRUE)
      )
  })
  
  
  ######### DESCRIPTIVE ANALYSIS ##################################      
  
  
  observeEvent(input$Plot,{
    
    
    updateSelectInput(
      session = session,
      inputId = "desc_var",
      label = "Descriptive plot",
      choices = list(
        "Gender" = "Gender",
        "Married" = "Married",
        "Graduated" = "Graduated",
        "Profession" = "Profession",
        "Spending score" = "Spending_Score",
        "Age"= "Age",
        "Work experience" = "Work_Experience",
        "Family size" = "Family_Size"),
      selected ="Gender")
    
    updateActionButton(                                   
      session = session,
      inputId = "Plot",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
    
    descv <- input$desc_var
    
    ### Descriptive bar plots  ####
    
    if (descv == "Gender") {output$D_Plot <- renderPlot({desc_plot(var = "Gender")})}
    if (descv == "Married") {output$D_Plot <- renderPlot({desc_plot(var = "Married")})}
    if (descv == "Graduated") {output$D_Plot <- renderPlot({desc_plot(var = "Graduated")})}
    if (descv == "Profession") {output$D_Plot <- renderPlot({desc_plot(var = "Profession")})}
    if (descv == "Spending_Score") {output$D_Plot <- renderPlot({desc_plot(var = "Spending_Score")})}
    if (descv == "Age") {output$D_Plot <- renderPlot({desc_plot(var = "Age")})}
    if (descv == "Work_Experience") {output$D_Plot <- renderPlot({desc_plot(var = "Work_Experience")})}
    if (descv == "Family_Size") {output$D_Plot <- renderPlot({desc_plot(var = "Family_Size")})}
  })
  
  
  
  
  #### PREPARE THE DATA SET ################################
  
  observeEvent(input$prepare_data,{
    
    showModal(modalDialog(
      
      title = "PREPARING THE DATA SET",
      HTML("The clustering algorithm that will be used for the purpose of this project <br>
           is the K-Means. It requires the data to follow some requirments such as: a) Select all<br>
           the character variables, and convert them into binary data; i.e sex (values 0,1) for men<br>
           and women.b) The most frequent dummy variable will be removed as a data science <br>
           best practice c)Numeric and categorical binary data will be in a new data set d) The working <br>
           data set will be the new data set, but normalized , which means that it will have <br>
           a common scale in all variables as input to apply the K-Means algorithm."),
     
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T 
      
    ))
    
  })
  
  observeEvent(input$dataprep,{
    updateActionButton(                                  
      session = session,
      inputId = "dataprep",
      label = "8. prepared data",
      icon = icon("ok",lib ="glyphicon")
    )
    
    ### char binary conversion and normalization, and numeric data cbind
    
    datap <- cleandata(Data)
    Final_MarketData <- dataprep(datap)
    
    ### Working data set output ####
    
    output$finaldata <- 
      output$results <- DT::renderDataTable(
        Final_MarketData,
        options = list(scrollX = TRUE)
      )
  })
  
  
  
  
  #### k-Mean parameter using the elbow rule, and silhouette methods ####
  
  observeEvent(input$parameter,{
    
    showModal(modalDialog(
      
      title = "RUNNING THE K MEANS MODEL AND DEFINING THE K SEGMENTS",
      HTML("The K-means unsupervised clustering algorithm finds the mean, and the minimum <br>
            squared euclidean distances to define a first cluster prototype; then , the centroid,<br> 
            is used to classify new data into each cluster. If the centroid value is the same than<br> 
            the calculated mean of the previous cluster definition, the iteration process stops.<br>
            The initial number of clusters is defined by the K parameter; and for the scope of <br>
            this project, in the next step, two methodologies will be used to calculate the initial<br> 
            value of K: The elbow and the silhouette methods. In the next visualization, the elbow <br>
            method suggests the value of  k = 6. More information about the k means<br> 
            algorithm in:<br>
            <br>
            https://towardsdatascience.com/k-means-a-complete-introduction-1702af9cd8c<bR>
            <br>
            NOTE: THIS REPORT WILL TAKE ABOUT 15 SECONDS TO RUN<br>"
      ),
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T 
      
    ))
    
  })
  
  
  
  observeEvent(input$elbow,{
    updateActionButton(
      session = session,
      inputId = "elbow",
      label = "9.Elbow method",
      icon = icon("ok",lib ="glyphicon")
    )
    
    datap <- cleandata(Data)
    Final_MarketData <- dataprep(datap)
    ### to run in shiny.io (THE IMAGE OF THE RESULTS - NOT RUNNING THE ALGORITHM )###
    {output$fviz_nbclust <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="Elbow.png")})}
    ## TO RUN THE ALGORITHM IN YOUR MACHINE -- TAKES MORE TIME!
    ###  output$fviz_nbclust <- renderPlot({
    ## fviz_nbclust(Final_MarketData,kmeans, method = "wss") + labs(subtitle = "Elbow Method - wss") + geom_vline(xintercept = 6, linetype = 2)
  })
  
  observeEvent(input$silhouette,{
    updateActionButton(
      session = session,
      inputId = "silhouette",
      label = "10.Silhouette method",
      icon = icon("ok",lib ="glyphicon")
    )
    
    datap <- cleandata(Data)
    Final_MarketData <- dataprep(datap)
    k <- input$kvalue
    set.seed(1584)
    output$fviz_silhouette <- renderPlot({
      fviz_nbclust(Final_MarketData,kmeans, method = "silhouette") + labs(subtitle = "Silhouette method") 
      cluster <- kmeans(Final_MarketData, centers = k, iter.max = 10)
      sil <- silhouette(cluster$cluster, dist(Final_MarketData))
      fviz_silhouette(sil)
    })
    
  })
  
  observeEvent(input$newsegments,{
    
    showModal(modalDialog(
      
      title = "NEW SEGMENTS DEFINITION",
      HTML("The new segments will be defined according to the decided k value with the K-Means alhorithm,<br>
            and the initial cleaned customer data base will be classified with a new variable, so each <br>
            client will belong to a cluster or segment. The next section allows to plot the predicted <br>
            segment vs the selected customer variable in terms of the number of customers in each segment.<br>"
      ),
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m",
      easyClose = T,
      fade = T
      
    ))
    
  })
  
  ### new segments ###
  
  observeEvent(input$new_s,{
    
    updateSelectInput(
      session = session,
      inputId = "segment_var",
      label = "New segments plot",
      choices = list(
        "Gender" = "Gender",
        "Married" = "Married",
        "Graduated" = "Graduated",
        "Profession" = "Profession",
        "Spending score" = "Spending_Score",
        "Age"= "Age",
        "Work experience" = "Work_Experience",
        "Family size" = "Family_Size"),
      selected ="Gender")
    
    updateActionButton(                                   
      session = session,
      inputId = "new_s",
      label = " Select and Plot",
      icon = icon("option-vertical",lib ="glyphicon"))
    
    
    ###  Predictive new segment plots  ####
    
    # Final_MarketData_c <-cbind(Final_MarketData,cluster$cluster) Normalized data if needed
    seg_var <- input$segment_var
    k <- input$kvalue
    
    if (seg_var == "Gender") {output$C_Plot <- renderPlot({market_plot(var2 = "Gender",k)})}
    if (seg_var == "Married") {output$C_Plot <- renderPlot({market_plot(var2 = "Married",k)})}
    if (seg_var == "Graduated") {output$C_Plot <- renderPlot({market_plot(var2 = "Graduated",k)})}
    if (seg_var == "Profession") {output$C_Plot <- renderPlot({market_plot(var2 = "Profession",k)})}
    if (seg_var == "Spending_Score") {output$C_Plot <- renderPlot({market_plot(var2 = "Spending_Score",k)})}
    if (seg_var == "Age") {output$C_Plot <- renderPlot({market_plot(var2 = "Age",k)})}
    if (seg_var == "Work_Experience") {output$C_Plot <- renderPlot({market_plot(var2 = "Work_Experience",k)})}
    if (seg_var == "Family_Size") {output$C_Plot <- renderPlot({market_plot(var2 = "Family_Size",k)})}
    
  })
  
  
  
  observeEvent(input$finalsegments,{
    
    showModal(modalDialog(
      
      title = "FINAL SEGMENTS DEFINITION",
      HTML("The previous visualizations give input about the behaviour of each variable within<br>
           different cluster models; but, still it is difficult to decide which are the right<br>
           segments; the next visualizations will give a comprehensive input that includes all<br> 
           variables, this will allow to check its importance in each cluster or segment, in<br> 
           a global view. That input, the market experience, and the customer knowledge will drive<br> 
           to the best decision. The following step is to create the normalized data table with<br> 
           the selected number of segments; then, for all segments, the higher and lower variable<br> 
           values will be identified, out of a selected range. Those variables will help to define<br> 
           each final segment.<br>"
      ),
      footer = tagList(modalButton("CLOSE WINDOW")),
      size = "m", 
      easyClose = T,
      fade = T 
      
    ))
    
  })
  
  
  observeEvent(input$finaldata_c,{
    updateActionButton(                                  
      session = session,
      inputId = "finaldata_c",
      label = "12. Create final data segments",
      icon = icon("ok",lib ="glyphicon")
    )
    
    ### Final centroids data table
    
    datap <- cleandata(Data)
    Final_MarketData <- dataprep(datap)
    k <- input$kvalue
    set.seed(1584)
    cluster <- kmeans(Final_MarketData, centers = k, iter.max = 10)
    centers <- as.data.frame(cluster$centers)
    
    ### Working data set output ####
    
    output$centers <- 
      output$results <- DT::renderDataTable(
        centers,
        options = list(scrollX = TRUE)
      )
  })
  
  
  observeEvent(input$finalsegments_c,{
    updateActionButton(                                  
      session = session,
      inputId = "finalsegments_c",
      label = "13. Visualize final segments",
      icon = icon("ok",lib ="glyphicon")
    )
    
    ###  Final segments data plot ####
    
    k <- input$kvalue
    min <-input$range[1]
    max <-input$range[2]
    
    output$Seg_plot <- renderPlot({final_segments_plot(k,min,max)})
    
  })
  
  observeEvent(input$refresh,{
    updateActionButton(                                  
      session = session,
      inputId = "refresh",
      label = "refresh",
      icon = icon("ok",lib ="glyphicon")
    )
    
    session$reload()
    
  })
}


#######################################################
# LAUNCH                                             #
#######################################################

shinyApp( ui = ui , server = server)  



