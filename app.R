############ START: INITIAL PACKAGE LOADING ############
library(rvest)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(stringr)
library(bitops)
library(RCurl)
library(highcharter)
library(XML)
############ END: INITIAL PACKAGE LOADING ############

############ START: INITIAL FUNCTIONS LOADING ############
fetchBusinesses <- function(search_description, search_location, numOfBusinesses) { 
  search_description <- sub(" ", "+", search_description)
  search_location <- sub(" ", "+", search_location)
  curCount <- 1
  hasNextPage <- character(1)
  restaurants <- character(0)
  restaurants_search_info <- read_html(paste0("https://www.yelp.com/search?", 
                                              "find_desc=", search_description,
                                              "&find_loc=", search_location))
  
  while (!identical(hasNextPage, character(0)) && curCount <= numOfBusinesses) {
    restaurant_links <- restaurants_search_info %>% 
      html_nodes("div.search-results-content li.regular-search-result h3.search-result-title a.biz-name") %>% html_attr("href")
    
    restaurant_links <- sub("/biz/", "", restaurant_links)
    restaurant_links <- sub("\\?(.*)", "", restaurant_links) #we are interested in replacing first ? and onwards with blank
    
    for(i in 1:length(restaurant_links)) {
      if (curCount <= numOfBusinesses) {
        restaurants[curCount] <- restaurant_links[i]
        curCount = curCount + 1
      }
    }
    
    hasNextPage <- restaurants_search_info %>% 
      html_nodes("div.pagination-links a.next") %>% html_text()
    
    nextPageLink <- restaurants_search_info %>% 
      html_nodes("div.pagination-links a.next") %>% html_attr("href")
    
    if (!identical(hasNextPage, character(0)) && curCount <= numOfBusinesses) {
      restaurants_search_info <- read_html(paste0("https://www.yelp.com", nextPageLink))
    }
  }
  return (restaurants)
}
getAvgStarsOfUser <- function(user_id) {
  reviewer_html_info <- read_html(paste0("https://www.yelp.com", user_id))
  
  rating_table <- reviewer_html_info %>% 
    html_nodes("table.histogram") %>% html_table(fill = TRUE, trim = TRUE)
  
  if (length(rating_table) != 0) {
    stars <- data.frame()
    stars <- rbind(stars, rating_table[[1]][1,4])
    stars <- rbind(stars, rating_table[[1]][3,4])
    stars <- rbind(stars, rating_table[[1]][5,4])
    stars <- rbind(stars, rating_table[[1]][7,4])
    stars <- rbind(stars, rating_table[[1]][9,4])
    colnames(stars)[1] <- "stars"
    to.multiply <- c(5,4,3,2,1)
    average_stars <- stars %>% mutate(stars.scores = stars * to.multiply ) %>% summarise(mean= sum(stars.scores)/sum(stars))
    return (average_stars)
  } else {
    return(c("NA", "NA", "NA", "NA", "NA"))
  }
}
fetchReviews <- function(business_name, numOfReviews) { 
  curCount <- 0
  hasNextPage <- character(1)
  cleanReviewsDF <- data.frame()
  business_html_info <- read_html(paste0("https://www.yelp.com/biz/", business_name))
  
  while (!identical(hasNextPage, character(0)) && curCount < numOfReviews) {
    
    reviews_person <- business_html_info %>% 
      html_nodes(".review-sidebar a.user-display-name") %>% html_text()
    
    reviews_count <- business_html_info %>% 
      html_nodes(".review-sidebar li.review-count b") %>% html_text
    
    profile_links <- business_html_info %>% 
      html_nodes(".review-sidebar a.user-display-name") %>% html_attr("href")
    
    profiles_averagestars <- character(0)
    for(i in 1:length(profile_links)) {
      if (curCount + i <= numOfReviews) {
        profiles_averagestars[i] <- as.double(getAvgStarsOfUser(profile_links[i]))
      }
    }
    
    reviews_ratings <- business_html_info %>% 
      html_nodes(".review-content .star-img img") %>% html_attr("alt")
    
    reviews_date <- business_html_info %>% 
      html_nodes(".review-content span.rating-qualifier meta") %>% html_attr("content") 
    
    reviews_content <- business_html_info %>% 
      html_nodes(".review-content p")
    reviews_content <- sub('<p itemprop="description" lang="en">', '', reviews_content)
    reviews_content <- sub('<p lang="en">', ' ', reviews_content)
    reviews_content <- sub('</p>', ' ', reviews_content)
    reviews_content <- str_replace_all(reviews_content, '<br/>', ' ')
    
    for(i in 1:length(reviews_person)) {
      if (curCount < numOfReviews) {
        # get sentiment
        sentiment_obj <- postForm("https://gateway-a.watsonplatform.net/calls/text/TextGetTextSentiment?apikey=301b6a65b92b3c475baba00be032ffa5faffc910", .params = list(text=reviews_content[i]), style = 'post')
        sentiment_xml <- xmlParse(sentiment_obj)
        review_sentiment <- as.double(xpathSApply(sentiment_xml, "//*/score", xmlValue))
        
        cleanReviewsDF <- rbind(cleanReviewsDF, data.frame(business_name,
                                                           reviews_date[i], 
                                                           reviews_person[i], 
                                                           profiles_averagestars[i],
                                                           reviews_count[i],
                                                           reviews_ratings[i], 
                                                           reviews_content[i],
                                                           review_sentiment))
        curCount = curCount + 1
      }
    }
    
    hasNextPage <- business_html_info %>% 
      html_nodes("div.review-pager a.next") %>% html_text()
    
    nextPageLink <- business_html_info %>% 
      html_nodes("div.review-pager a.next") %>% html_attr("href")
    
    if (!identical(hasNextPage, character(0)) && curCount < numOfReviews) {
      business_html_info <- read_html(nextPageLink)
    }
  }
  return (cleanReviewsDF)
}
############ END: INITIAL FUNCTIONS LOADING ############

############ START: UI ###########
ui <- dashboardPage(
  dashboardHeader(title = "Big Mouth Scraper"),
  dashboardSidebar(
    tags$head(tags$style(HTML('button.btn.btn-primary {
                                margin-left: 15px !important;
                              }'))),
    textInput('searchDescription', label = "Search Description: ", value = "Coffee"),
    textInput('searchLocation', label = "Search Location: ", value = "Toronto, ON"),
    sliderInput('numOfBusinesses', "Number of Businesses: ", value = 10,  min = 1, max = 100),
    sliderInput('numOfReviews', "Number of Reviews: ", value = 5,  min = 1, max = 100),
    submitButton()
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Restaurant Sentiments",
        width = 6,
        DT::dataTableOutput('reviews')
      ),
      box(
        title = "Sentiment Graph",
        width = 6,
        highchartOutput("sentiment_graph", height = "500px")
      )
    )
  )
)
############ END: UI ###########

############ START: SERVER ###########
server <- function(input, output) {
   getBusinesses <- reactive({
     businesses <- fetchBusinesses(input$searchDescription, input$searchLocation, input$numOfBusinesses)
     return (businesses)
   })
  
   getReviews <- reactive({
     businesses <- getBusinesses()
     for (i in c(1:length(businesses))) {
       reviews <- fetchReviews(businesses[i], input$numOfReviews)
       if(i == 1) {
        totalreviews <- reviews
       } else {
         totalreviews <- rbind(totalreviews, reviews)
       }
     }
     return(totalreviews)
   })
   
   getAverageSentimentData <- reactive({
     allreviews <- getReviews()
     avg_sentiment <- as.data.table(allreviews[c(1,3,8)])
     avg_sentiment[, pos := ifelse(review_sentiment >= 0, review_sentiment, 0)][, neg := ifelse(review_sentiment < 0, review_sentiment, 0)]
     avg_sentiment <- avg_sentiment[, .(pos = mean(pos, rm.na=TRUE), neg = mean(neg, rm.na=TRUE)), by = business_name]
     avg_sentiment[, pos:=round(pos, digits=5)]
     avg_sentiment[, neg:=round(neg, digits=5)]
     colnames(avg_sentiment) <- c("Restaurant", "Average Positive Sentiment", "Average Negative Sentiment")
     return(avg_sentiment)
   })
   
   getSentinmentGraphData <- reactive({
     allreviews <- getReviews()
     avg_sentiment <- as.data.table(allreviews[c(1,3,8)])
     avg_sentiment[, pos := ifelse(review_sentiment >= 0, review_sentiment, 0)][, neg := ifelse(review_sentiment < 0, -review_sentiment, 0)]
     avg_sentiment <- avg_sentiment[, .(pos = mean(pos, rm.na=TRUE), neg = mean(neg, rm.na=TRUE)), by = business_name]
     avg_sentiment[, pos:=round(pos, digits=5)]
     avg_sentiment[, neg:=round(neg, digits=5)]
     colnames(avg_sentiment) <- c("Restaurant", "Average Positive Sentiment", "Average Negative Sentiment")
     return(avg_sentiment)
   })
   
   output$sentiment_graph <- renderHighchart({
     sentiment_list <- getSentinmentGraphData() %>% do(
       data = list(
         data = apply(., 1, function(x){
           attributes(x) <- NULL
           list(
             name = x[1],
             y = as.double(x[2]),
             x = as.double(x[3])
           )
         })
       )
     )
     sentiment_list <- sentiment_list$data
     
     highchart() %>% 
       hc_chart(type = "scatter") %>% 
       hc_add_series(data = sentiment_list[[1]]$data) %>%
       hc_add_series(type = 'line', data = 
                       list(
                         list(x = 0, y = 0), 
                         list(x = 1, y = 1)), 
                     zAxis = 0, 
                     name = "Average Line") %>%
       hc_plotOptions(
         scatter = list(
           dataLabels = list(
             enabled = FALSE,
             format = "{point.name}",
             allowOverlap = TRUE
           ),
           tooltip = list(
             useHTML = TRUE,
             headerFormat = "",
             pointFormat = "<strong>Restaurant:</strong> {point.name}<br/>
             <strong>Avg. Pos. Sentiment:</strong> {point.y}<br/>
             <strong>Avg. Neg. Sentiment:</strong> -{point.x}"
           )
          )
        ) %>%
     hc_xAxis(
       title = list(
         text = "Negative Sentiment"
       )
     ) %>%
       hc_yAxis(
         title = list(
          text = "Positive Sentiment"
         )
       )
   })
   
   output$reviews <- DT::renderDataTable(
     getAverageSentimentData(),
     class = 'cell-border stripe',
     options = list(
       dom = 'tpr',
       pageLength = 10
     ),
     rownames= FALSE
   )
}
############ END: SERVER ###########

# Run the application 
shinyApp(ui = ui, server = server)

