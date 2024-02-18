# Load necessary libraries
library(shiny)
library(shinydashboard)
library(plotly)

library(readxl)
library(shinythemes)

library(leaflet)
library(dplyr)

# Load the Walmart dataset from Excel (replace 'your_dataset.xlsx' with the actual file path)
walmart_data <- read_excel("walmart_data.xlsx")
store_locations <- read_excel("Walmart_Location.xlsx")


# UI definition
ui <- fluidPage(
  tags$head(tags$link(rel = "shortcut icon", href = "https://cdn.icon-icons.com/icons2/1369/PNG/512/-shopping-cart_90604.png")),
  theme = shinytheme("cyborg"),
  titlePanel("Walmart Sales Prediction and Data Visualization"),
  
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Home",
             
             fluidRow(
               column(width = 6,
                      img(src = "https://cloudfront-us-east-2.images.arcpublishing.com/reuters/HFE7GTJC2FOLVHH3JDYHESVMAI.jpg", width = "100%", style = "margin-top: 40px; margin-left: 40px")  # Adjust the image path and width as needed
               ),
               column(width = 5,
                      h4("Walmart's Sales Dynamics", style = "margin-top: 40px; margin-left: 40px"),
                      wellPanel("Walmart's sales are influenced by external factors such as temperature, unemployment, fuel prices, and festivals.",
                                br(),
                                
                                br(),
                                "Walmart strategically tailors promotions around festivals to capitalize on increased consumer spending during these periods. The interplay of these factors, coupled with Walmart's adaptability and strategic responses, determines the overall impact on the retail giant's sales.",
                                style= "margin-left: 40px"),
                      
                      h6("How Different Factors Impact Sale", style = "margin-top: 40px; margin-left: 40px"),
                      wellPanel("Temperature:", br(),
                                
                                "1. Seasonal Demand: Sales of seasonal items like clothing, outdoor furniture, and seasonal foods may be affected by temperature fluctuations.",br(),
                                "2. Climate-Dependent Products: Extreme temperatures can influence the demand for weather-dependent products, such as winter gear or summer-related items.", br(),br(),
                                
                                "Unemployment:",br(),
                                
                                "1. Consumer Spending: High unemployment rates can lead to reduced consumer spending, affecting the sales of non-essential items.",br(),
                                "2. Value Shopping: During economic downturns, consumers may prioritize value-oriented products, benefiting Walmart's focus on affordability.",br(),br(),
                                
                                "Fuel Prices:",br(),
                                
                                "1. Transportation Costs: Fluctuations in fuel prices impact transportation costs, affecting the overall cost of goods and potentially influencing pricing strategies.",br(),
                                "2. Consumer Behavior: Higher fuel prices may lead to increased demand for local shopping, benefiting Walmart's widespread presence.",br(),br(),
                                
                                "Festivals:",br(),
                                
                                "1. Seasonal Promotions: Walmart often tailors promotions and discounts around major festivals, capitalizing on increased consumer spending during these periods.",br(),
                                "2. Holiday Sales: Festive seasons can drive higher sales in various departments, such as electronics, toys, and decorations.",
                                
                                
                                style = " margin-left: 40px")
               )
             ),
             fluidRow(
               h4("About Visualizations & Predictions in this Application: ", style = "margin-top: 50px; margin-left: 40px"),
               column(width=3, style = "margin-top: 20px; margin-left: 40px; margin-bottom: 100px;",
                      h6("Trends Visualization:"),br(),
                      " - Utilizing dynamic charts and graphs in R Shiny to showcase sales patterns over time.",br(),
                      "- Users can explore trends for different product categories or specific store locations.
              "),
               column(width=3, style = "margin-top: 20px; margin-left: 40px;margin-bottom: 100px;",
                      h6("Map Visualization:"),br(),
                      " - Integrating geographical data to create an interactive map interface.",br(),
                      "- Visual representation of stores, temperature and unemployment data across various regions.",br(),
                      "- Enables users to identify hotspots and areas for improvement geographically.
              "),
               column(width=3, style = "margin-top: 20px; margin-left: 40px;margin-bottom: 100px;",
                      h6("Sales Prediction:"),br(),
                      "- Incorporating predictive analytics within the R Shiny application.",br(),
                      "- Advanced algorithms are employed to generate accurate sales forecasts.",br(),
                      "- Empowering stakeholders with insights for making informed decisions and optimizing strategies to enhance Walmart store performance."),
               
             ),
             wellPanel( a("Dataset Link", href = "https://www.kaggle.com/datasets/yasserh/walmart-dataset/data"))
             
    ),
    
    
    
    tabPanel("Trend Visualizations",
             
             sidebarPanel(
               width = 3,
               style = "margin-top: 40px;",
               # Input options
               selectInput("option", "Select a Parameter to see how it affects Sales:",
                           choices = c("Sales", "Temperature", "Unemployment", "Fuel Prices", "CPI", "Date", "Interdependence of attributes"))
             ),
             
             # Main panel
             mainPanel(
               style = "margin-top: 40px;",
               # Output
               uiOutput("dynamic_ui")
             ),
    ),
    
    
    
    
    tabPanel("Map Visualizations",
             style = "margin-top: 20px;",
             tabsetPanel(
               id = "subTabs1",
               
               # Sub tabs inside the first main tab
               tabPanel("Store Map",
                        style = "margin-top: 40px;",
                        wellPanel("Locations of 400 Walmart Stores in the United States",
                                  br(),
                                  a("Dataset Link", href = "https://www.kaggle.com/datasets/thedevastator/walmart-store-locations-across-the-united-states")
                        ),
                        
                        leafletOutput("store_map"),
               ),
               tabPanel("Temperature Map",
                        style = "margin-top: 40px;",
                        wellPanel("Avg Temperature near store locations",
                                  br(),
                                  "Note: Arbitary values have been taken for demonstration purpose"
                        ),
                        leafletOutput("temp_map"),
               ),
               tabPanel("Fuel Prices Map",
                        style = "margin-top: 40px;",
                        wellPanel("Avg Fuel Prices near store locations",
                                  br(),
                                  "Note: Arbitary values have been taken for demonstration purpose"
                        ),
                        leafletOutput("fuel_map"),
               ),
               tabPanel("Unemployment Map",
                        style = "margin-top: 40px;",
                        wellPanel("Avg Unemployment rate near store locations",
                                  br(),
                                  "Note: Arbitary values have been taken for demonstration purpose"
                        ),
                        leafletOutput("unemployed_map"),
               ),
             ),
             
             
             
             
    ),
    
    
    
    tabPanel(
      "Sales Prediction",
      wellPanel(h6("A brief about the parameters that affect Walmart Sales:"),
                ("Temperature: It is observed that lower temperatures are linked to higher footfall of customers in the United States"),
                br(),
                br(),
                ("Store: Locations of various stores across the United States have an affect on their customer size, and hence their sales."),
                br(),
                br(),
                ("Unemployment: Higher employment rates increase purachsing power of customers and hence the sales"),
                br(),
                br(),
                ("Festival : Festive season generally brings in more customers. Affect of Christmas and Thanksgiving is prominent on Walmart Sales in the USA")
      ),
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("temperature", "Temperature", min = min(walmart_data$Temperature, na.rm = TRUE), max = max(walmart_data$Temperature, na.rm = TRUE), value = median(walmart_data$Temperature, na.rm = TRUE)),
          sliderInput("store", "Store", min = min(walmart_data$Store, na.rm = TRUE), max = max(walmart_data$Store, na.rm = TRUE), value = median(walmart_data$Store, na.rm = TRUE)),
          sliderInput("unemployment", "Unemployment", min = min(walmart_data$Unemployment, na.rm = TRUE), max = max(walmart_data$Unemployment, na.rm = TRUE), value = median(walmart_data$Unemployment, na.rm = TRUE)),
          #sliderInput("cpi", "CPI", min = min(walmart_data$CPI, na.rm = TRUE), max = max(walmart_data$CPI, na.rm = TRUE), value = median(walmart_data$CPI, na.rm = TRUE)),
          selectInput("festival", "Festival", choices = c("Normal Day", "Thanksgiving", "A week before Christmas"),selected = "Normal Day"),
          actionButton("predict_button", "Predict Sales")
        ),
        mainPanel(
          #plotOutput("scatterplot"),
          #plotOutput("boxplot"),
          wellPanel(h4("Sales Prediction")),
          wellPanel( fluidRow(verbatimTextOutput("prediction_output")))
        )
      )
    )
  )
  
  
  
  
)


# Server definition
# Server definition
# Server definition
server <- function(input, output, session) {
  
  # Data Exploration Plots
  output$scatterplot <- renderPlot({
    plot_ly(data = walmart_data, x = ~Temperature, y = ~Weekly_Sales, mode = "markers", type = "scatter", marker = list(size = 10))
  })
  
  output$boxplot <- renderPlot({
    plot_ly(data = walmart_data, x = ~factor(1), y = ~Weekly_Sales, type = "box", boxpoints = "all", jitter = 0.3, pointpos = -1.8)
  })
  
  # Regression Model for Prediction
  model <- reactive({
    glm(Weekly_Sales ~ Store + Temperature + festival + Unemployment,
        data = walmart_data)
  })
  
  # Prediction
  output$prediction_output <- renderPrint({
    req(input$predict_button)
    new_data <- data.frame(
      Store = input$store,
      Temperature = input$temperature,
      festival=input$festival,
      Unemployment = input$unemployment
      
    )
    prediction <- predict(model(), new_data)
    paste("Predicted Sales: $", round(prediction, 2))
  })
  
  
  
  output$store_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = store_locations,
                 ~lon, ~lat,
                 popup = ~name,
                 
      )
    
    
  })
  output$temp_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = store_locations,
                       ~lon, ~lat,
                       popup = ~paste("Temperature:", temp),
                       radius = 5,  # Adjust the radius of the circle marker as needed
                       color = "orange",  # Change the color of the marker
                       stroke = FALSE,
                       fillOpacity = 1
      )
  })
  output$unemployed_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = store_locations,
                       ~lon, ~lat,
                       popup = ~paste("Unemployed %:", unemployed),
                       radius = 5,  # Adjust the radius of the circle marker as needed
                       color = "gray",  # Change the color of the marker
                       stroke = FALSE,
                       fillOpacity = 1
      )
  })
  output$fuel_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = store_locations,
                       ~lon, ~lat,
                       popup = ~paste("Fuel Prices: ", fuel_price),
                       radius = 5,  # Adjust the radius of the circle marker as needed
                       color = "brown",  # Change the color of the marker
                       stroke = FALSE,
                       fillOpacity = 1
      )
  })
  
  
  output$dynamic_ui <- renderUI({
    option <- input$option
    
    if (option == "Sales") {
      wellPanel(h3("SALES TRENDS"), align = "center")
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    plotOutput("sales_trend"),
                    plotOutput("stores_sales")),
        align = "center"
      )
    }
    else if (option == "Temperature"){
      wellPanel(h5("TEMPERATURE",align="center"))
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("temperature_plot"),
                           plotOutput("temperature_sales"),align="center")
      )
    }
    else if (option == "Fuel Prices"){
      wellPanel(h5("FUEL PRICES",align="center"))
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("fuelpriceplot"),
                           plotOutput("fuel_sales"),align="center")
      )
    }
    else if (option == "CPI"){
      wellPanel(h5("CPI",align="center"))
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("cpiplot"),
                           plotOutput("cpi_sales"),align="center")
      )
    }
    else if (option == "Unemployment"){
      wellPanel(h5("UNEMPLOYMENT",align="center"))
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("unemploymentplot"),
                           plotOutput("unemployment_sales"),align="center")
      )
    }
    else if (option == "Date"){
      wellPanel(h5("Date",align="center"))
      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                           plotOutput("holidays"),
                           plotOutput("holidayflag"),align="center")
      )
    }
    else if (option == "Interdependence of attributes"){
      wellPanel(h5("Interdependence of attributes",align="center"))
      mainPanel(
        fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("tempvsfestival"),
                             plotOutput("cpivsunemp"),
                             
                             align="center")
        ),
        fluidRow(plotOutput("storevsunemp"),
                 align="center")
        ,width="900px")
      
    }
  })
  
  
  
  output$sales_trend <- renderPlot({
    store_sales <- walmart_data %>%
      group_by(Store) %>%
      summarise(Total_Sales = sum(Weekly_Sales))
    
    ggplot(store_sales) +
      geom_col(mapping = aes(x= Store, y = Total_Sales, fill = Total_Sales)) +
      labs(title = "Total Sales by Store",
           x = "Store",
           y = "Total Sales") +
      scale_fill_gradient(low = "#2d388a", high = "#00aeef")
  })
  output$stores_sales <- renderPlot({
    walmart_data%>%
      ggplot(aes(Weekly_Sales, reorder(Store,
                                       FUN = median, Weekly_Sales)))+
      geom_boxplot()+
      labs(title = 'Store: There are difference in weekly sales',
           x='Weekly sales',
           y='Store')+
      theme_bw()
  })
  output$temperature_plot <- renderPlot({
    walmart_data%>%
      ggplot(aes(Temperature))+
      geom_histogram(bins=100)+
      labs(title = 'Temperature: On average, most of the records are high temperatures',
           
           x='Temperature')+
      theme_bw()
  })
  output$temperature_sales <- renderPlot({
    walmart_data%>%
      ggplot(aes(Temperature, Weekly_Sales))+
      geom_point(alpha =1/10)+
      labs(title = 'Temperature: There are more people to shop in low temperatures',
           y='Weekly sales',
           x='Temperature')+
      theme_bw()
  })
  output$fuelpriceplot <- renderPlot({
    walmart_data%>%
      ggplot(aes(Fuel_Price))+
      geom_histogram(bins=50)+
      labs(title = 'Fuel Price: There are two peaks, 2.5-3.0 and 3.5-4.0',
           y='Count',
           x='Fuel Price')+
      theme_bw()
  })
  output$fuel_sales <- renderPlot({
    walmart_data%>%
      ggplot(aes(Fuel_Price, Weekly_Sales))+
      geom_point(alpha =1/10)+
      labs(title = 'Fuel Price: There are more people to shop in specific fuel price',
           y='Weekly sales',
           x='Fuel Price')+
      theme_bw()
  })
  output$cpiplot <- renderPlot({
    walmart_data%>%
      ggplot(aes(CPI))+
      geom_histogram(bins=100)+
      labs(title = 'CPI: There are data ranging from 125-150, 175-200, 200-225',
           y='Count',
           x='CPI')+
      theme_bw()
  })
  output$cpi_sales <- renderPlot({
    walmart_data%>%
      ggplot(aes(CPI, Weekly_Sales))+
      geom_point(alpha =1/10)+
      labs(title = 'CPI: Seems that different range of CPI have same sales distributions',
           y='Weekly sales',
           x='CPI')+
      theme_bw()
  })
  output$unemploymentplot <- renderPlot({
    walmart_data%>%
      ggplot(aes(Unemployment))+
      geom_histogram(bins=50)+
      labs(title = 'Unemployment: Most are ranging in 6-9',
           y='Count',
           x='Unemployment')+
      theme_bw()
  })
  output$unemployment_sales <- renderPlot({
    walmart_data%>%
      ggplot(aes(Unemployment, Weekly_Sales))+
      geom_point(alpha =1/10)+
      labs(title = 'Unemployment: Lower unemployment rate has higher weekly sales',
           y='Weekly sales',
           x='Unemployment')+
      theme_bw()
  })
  output$holidays <- renderPlot({
    walmart_data%>%
      group_by(Date)%>%
      summarise(Weekly_Sales = mean(Weekly_Sales, na.rm=T))%>%
      ggplot(aes(Date, Weekly_Sales))+
      geom_point(aes(color=Weekly_Sales>1200000), show.legend = F)+
      geom_line(group=1,color='grey')+
      labs(title = 'Date: Some dates have extreme high weekly sales',
           y='Weekly sales',
           x='Date')
    
    
  })
  output$holidayflag <- renderPlot({
    walmart_data%>%
      group_by(Date)%>%
      mutate(Weekly_Sales = mean(Weekly_Sales, na.rm=T))%>%
      ggplot(aes(Date, Weekly_Sales))+
      geom_point(aes(color = festival,
                     shape = festival),size = 2)+
      geom_line(group=1, color='grey')+
      labs(title = 'Festival: Thanksgiving have extreme high weekly sales',
           subtitle = 'It will reach a sales peak a week before Christmas',
           y='Weekly sales',
           x='Date')+
      theme_bw()
  })
  output$tempvsfestival <- renderPlot({
    walmart_data%>%
      ggplot(aes(Temperature, color=festival))+
      geom_density(aes(fill=festival), alpha = 1/10)+
      labs(title = 'Temp vs. Festival: Normal days have highest temperature',
           subtitle = 'High sales might due to festival, not due to temperature',
           y='Density',
           x='Temperature')+
      theme_bw()
  })
  output$cpivsunemp <- renderPlot({
    walmart_data%>%
      ggplot(aes(cut_number(CPI,5), Unemployment))+
      geom_boxplot()+
      labs(title = 'CPI vs. Unemployment: More CPI and Lower Unemployment rate',
           y='Unemployment',
           x='CPI')+
      
      theme_bw()
  })
  output$storevsunemp <- renderPlot({
    walmart_data%>%
      ggplot(aes(Unemployment,
                 reorder(Store, fun=median, Unemployment)))+
      geom_boxplot()+
      labs(title = 'Store vs. Unemployment: Different regions have different unemployment rate',
           y='Unemployment',
           x='Store')
  })
  
  
  
  
  
  
  
  
}

# Run the Shiny app
shinyApp(ui,server)