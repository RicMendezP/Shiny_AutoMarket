# Shiny_AutoMarket
Auto market segments identification
The following R code uses the K-MEANS algorithm to find the best market segments for an auto industry business case using shiny

Context
An automobile company has plans to enter new markets with their existing products (P1, P2, P3, P4 and P5). After intensive market research, they’ve deduced that the behavior of new market is similar to their existing market.

Content
In their existing market, the sales team has classified all customers into 4 segments (A, B, C, D ). Then, they performed segmented outreach and communication for different segment of customers. This strategy has work exceptionally well for them. They plan to use the same strategy on new markets and have identified 2627 new potential customers.

You are required to help the manager to predict the right group of the new customers. Check the original data here: 

https://www.kaggle.com/vetrirah/customer

CODE NOTES:

1.The code uses the elbow and the silhouette methodS to find the value of K (K-MEANS); but, the elbow method algorithm is time consuming, so the code has the option to run directly in your machine or show only the results:

 ###to run in shiny.io (THE IMAGE OF THE RESULTS - NOT RUNNING THE ALGORITHM )###
    {output$fviz_nbclust <- renderUI({tags$img(style = "height:700px;width:100%; scrolling=nyes",src="Elbow.png")})}
    
    ## TO RUN THE ALGORITHM IN YOUR MACHINE -- TAKES MORE TIME!
  
  output$fviz_nbclust <- renderPlot({
    fviz_nbclust(Final_MarketData,kmeans, method = "wss") + labs(subtitle = "Elbow Method - wss") + geom_vline(xintercept = 6, linetype = 2)
    
 
    
    

