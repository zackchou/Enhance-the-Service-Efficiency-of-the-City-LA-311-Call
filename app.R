library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)
library(viridis)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Visualization of 311 Call Analysis"),
 
     # Tabset
    tabsetPanel(
      tabPanel(title = "Data Overview", 
               navbarPage(title = "Year",
                 tabPanel( title = "2016 Overview",
                           dateRangeInput("dtr", "Date Range:", start = "2016-01-01", end = "2016-11-27"),
                           plotOutput("space_overview"),
                           br(),
                           plotlyOutput("time_overview"),
                           helpText("As we can see in the plot")),
                 tabPanel(title = "2011-2015 Overview", 
                          dateRangeInput("dtr2", "Date Range:", start = "2011-01-01", end = "2015-05-31"),
                          plotOutput("space_overview2"),
                          br(),
                          plotlyOutput("time_overview2"),
                          helpText("As we can see in the plot"))
                 )
               ),
      
      tabPanel(title = "Call Resolution", 
               
               sidebarLayout(      
                 # Define the sidebar with one input
                 sidebarPanel(
                   selectInput("call_type", "Request Type:", 
                               choices=c("Processed Info", "Transferred Info", "Referred Info", "Invalid Info", "Given Info")),
                   hr(),
                   helpText("Invalid Info concludes Caller Hanger up, Got Voicemail, Info NotAvailable, Line Busy and N/A")
                 ),
                 
                 # Create a spot for the barplot
                 mainPanel(
                   plotOutput("map"),
                   plotlyOutput("heatmap")
                 )
               )
      ),
      tabPanel(title = "Process Efficiency",
               plotlyOutput("eff"),
               helpText("As we can see in the plot"))
    )
  
    
   
)




#################################################################################################
server <- function(input, output) {
  
  load(file = "/Users/zackchou/Desktop/calldata.rda")
  load(file = "/Users/zackchou/Desktop/calldata_2.rda")
  load(file = "/Users/zackchou/Desktop/la_zip.rda")
  la <- qmap("2000 Wellington Rd Los Angeles, CA 90016", zoom = 11, color = "bw")
  
  output$time_overview <- renderPlotly({
    
    newdata <- filter(newdata, input$dtr[1] <= date & input$dtr[2] >= date)
    
    a <- newdata %>% 
      filter(!is.na(yr),
             yr == 2016,
             input$dtr[1] <= date & input$dtr[2] >= date) %>% 
      group_by(yr, wd, hr) %>% 
      summarise(count = n()) %>% 
      ggplot(aes(x = wd, y = factor(hr), fill = count))+
      geom_tile()+
      scale_fill_viridis()+
      labs(title = "Yearly Call Time Distribution 2016", y = "Hour", x = NULL, fill = "Frequency")+
      theme_classic()+
      theme(panel.background = element_rect(color = "black"), 
            axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    ggplotly(a)
    
  })
  
  output$time_overview2 <- renderPlotly({
    
    a2 <- call1 %>% 
      filter(input$dtr2[1] <= Date & input$dtr2[2] >= Date) %>% 
      group_by(wd, hr) %>% 
      summarise(count = n()) %>% 
      ggplot(aes(x = wd, y = factor(hr), fill = count))+
      geom_tile()+
      scale_fill_viridis()+
      labs(title = "Yearly Call Time Distribution 2011-2015", y = "Hour", x = NULL, fill = "Frequency")+
      theme_classic()+
      theme(panel.background = element_rect(color = "black"), 
            axis.text.x = element_text(angle = 45, vjust = 0.5))
    
    ggplotly(a2)
    
  })
  
  output$space_overview <- renderPlot({
    
    newdata <- filter(newdata, input$dtr[1] <= date & input$dtr[2] >= date)
    
    data2 <- newdata %>% 
      group_by(yr, mth, ZipCode, RequestType) %>% 
      summarise(count = n()) %>% 
      arrange(-count)
    
    joindata2 <- left_join(lazip, data2, by = c("id" = "ZipCode"))
    
    
    
    la+
      geom_polygon(data = filter(joindata2, yr == 2016), 
                   aes(x = long, y = lat, group = group, fill = count, alpha = count),
                   color = "darkgrey")+
      theme_void()+
      scale_fill_viridis()+
      labs(title = "Call Frequency by Zip Area 2016", fill =  "Frequency", alpha = "Transparency")
    
  })
  
  output$space_overview2 <- renderPlot({
    
    call2011 <- call1 %>%
      filter(input$dtr2[1] <= Date & input$dtr2[2] >= Date)
    
    data2011 <- call2011 %>% 
      group_by(Zip.Code) %>% 
      summarise(count = n()) %>% 
      arrange(-count)
    
    joindata2011 <- left_join(lazip, data2011, by = c("id" = "Zip.Code"))
    
    
    
    la+
      geom_polygon(data = joindata2011, 
                   aes(x = long, y = lat, group = group, fill = count, alpha = count),
                   color = "darkgrey")+
      theme_void()+
      scale_fill_viridis()+
      labs(title = "Call Frequency by Zip Area 2011-2015", fill =  "Frequency", alpha = "Transparency")
    
  })
  
  
  output$map <- renderPlot({
    
    sort = filter(call1,
                  call1$Call.Type == input$call_type)
    
    sort = sort %>%
      group_by(Zip.Code) %>%
      summarise(count = n()) %>%
      arrange(-count)
    
    
    data3 <- left_join(lazip, sort, by = c("id" = "Zip.Code"))
    
    la+
      geom_polygon(data = data3, 
                   aes(x = long, y = lat, fill = count, group = group, alpha = count), 
                   color = "darkgrey")+ 
      theme_void()+ 
      labs(title = "Distribution of 311 Given Info in LA")+
      scale_fill_viridis()
    
  })
  
  output$heatmap <- renderPlotly({
    
    cate = filter(call1,
                  call1$Call.Type == input$call_type)
    
    cate %>%
      group_by(wd, hr) %>%
      summarise(count = n()) %>% 
      ggplot(aes(x = wd, y = factor(hr), fill = count))+ 
      geom_tile()+ 
      xlab("Weekday")+ 
      ylab("Hour")+
      theme_classic()+
      theme(panel.background = element_rect(color = "black"), 
            axis.text.x = element_text(angle = 45, vjust = 0.5))+
      scale_fill_viridis()
  })
    
    
  output$eff <- renderPlotly({
    
    b <- newdata %>% 
      group_by(RequestType) %>% 
      summarise(count = n(),
                sumtime = sum(proc_time),
                eff = count/sumtime) %>% 
      filter(!RequestType %in% c("Report Water Waste", "Other")) %>% 
      ggplot(aes(x = reorder(RequestType, -eff), y = eff, fill = eff))+
      geom_bar(stat = "identity", show.legend = F)+
      coord_flip()+
      scale_fill_viridis(direction = 1)+
      labs(title = "Efficiency Rating", x = "Request Type", y = "Efficiency Ratio")+
      theme_classic()
    
    ggplotly(b)
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

