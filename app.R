# Load the required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(plotly)
library(RColorBrewer)
library(scales)
library(rsconnect)

# Read the df
df <- read.csv("Combined_Covid_19_and_State_Temperature.csv")
# Get the unique states in the df
states <- unique(df$State.name)

color_scale <- colorRampPalette(c("green", "yellow", "red"))

# Create the interactive US map
us_map <- plot_geo(df, locationmode = "USA-states")
us_map <- add_trace(us_map,
                    z = ~relative_temp,
                    colorscale = color_scale,
                    text = ~paste("State: ", state, "<br>Month: ", month, "<br>Relative Temperature: ", relative_temp),
                    hoverinfo = "text",
                    marker = list(line = list(color = "white", width = 1)),
                    showlegend = FALSE)
us_map <- layout(us_map,
                 title = "Interactive US Map",
                 geo = list(scope = "usa"))
# UI stuff
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .text-box {
        background-color: #f0f0f0;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
    "))
  ),
  #Bill
  titlePanel("COVID-19 and Temperature Analysis"),
  tabsetPanel(
    id = "nav",
    tabPanel("Introduction",
             fluidRow(
               column(
                 width = 12,
                 tags$img(src = "https://img.freepik.com/free-vector/flatten-curve_23-2148554363.jpg?w=2000&t=st=1685572633~exp=1685573233~hmac=077129f55ff187f3ed338f7d7b4abdcb793d7c9b4762491f559f6f8143e63787", 
                          width = "100%")
               )
             ),
             fluidRow(
               column(
                 width = 4,
                 div(class = "text-box",
                     h3("About"),
                     p("This page allows you to explore the relationship between COVID-19 cases and temperature in different states."),
                     p("Select two states to compare their average temperature and mortality rate over time."),
                     hr(),
                     h4("Instructions"),
                     p("1. Choose a page from the tabs at the top to compare data."),
                     p("2. The 'Comparison by state' tab will display the average temperature and mortality rate plots between states."),
                     p("3. Use the tabs at the top to navigate between different sections of the app.")
                 ),
                 div(class = "text-box",
                     h3("Data Source"),
                     p("We collected a data set from the", tags$a("Centers for Disease Control", href = "https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36"), ", which has data about COVID-19 cases and deaths in different states from 2021 to 2023, and temperature data set from", tags$a("NOAA (National Oceanic and Atmospheric Administration)", href = "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/statewide/time-series/1/tavg/all/1/2021-2022?base_prd=true&begbaseyear=1901&endbaseyear=2000"), " that has the average temperature data for each month for all 50 states for 2021.")
                 )
               ),
               column(
                 width = 8,
                 div(class = "text-box",
                     h3("Overview"),
                     p("The COVID-19 pandemic has been a serious issue since its emergence in 2019. The pandemic has affected millions of people over the years. A number of researchers are continuing their studies in order to solve the issues that come up with the pandemic. For our study, our main topic of the project will be about determining if there is a correlation between the mortality rate of COVID-19 and the weather temperature."),
                     p("The strong, central, unifying theme is a yearning to understand COVID-19 as a result of the loss of life, income, and stability that it has caused. There have been several research papers that have been done on this topic, but there are no solid conclusions on it yet. The topic of our final project deals with the fundamental issues in our world, as millions of lives have been affected by the COVID-19 pandemic. There is an opportunity for us to offer intelligent insight because we would talk about how temperature could have an impact on people infected with the virus and this is not a question that can be simply answered with common sense. There are still scientific debates on whether temperature does have an impact on the mortality rate, so this project will provide new insights that aren't obvious.")
                 ),
                 div(class = "text-box",
                     h3("Goals"),
                     p("The goal of our data analysis is to uncover the relationship between COVID-19's mortality rate and temperature. We aim to examine and compare the data across states, within regions, and between regions to identify patterns in the impact of COVID-19."),
                     p("Specifically, our goals include:"),
                     tags$ul(
                       tags$li("Analyzing the correlation between COVID-19 mortality rate and temperature in different states."),
                       tags$li("Exploring the disparities in COVID-19 effects between different regions based on temperature variations."),
                       tags$li("Providing valuable information for policymakers, healthcare professionals, and researchers to develop targeted strategies and interventions."),
                       tags$li("Contributing to the ongoing scientific understanding of COVID-19 and its relationship with temperature.")
                     )
                 )
               )
             )
    ),
    #Bill
    tabPanel("U.S Map visualization",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput("month", "Select a Month", choices = month.name, selected = month.name[1]),
                   h3("Graph Description"),
                   p("The interactive U.S. map displays the average temperatures in different states across different months,using continuous color scales that can be incredibly helpful for visualizing and understanding 
                   temperature patterns across different states. By utilizing a continuous color scale, the map provides a clear and intuitive representation of temperature variations, allowing users to easily identify areas of higher or lower temperatures.
                     This interactive map allows users to explore temperature differences across various months,, enabling them to observe seasonal changes and identify regions that experience significant temperature variations."
                   ),
                   style = "margin-top: 30px;"
                 ),
                 
                 mainPanel(
                   plotlyOutput("map"),
                   style = "margin-top: 30px;"
                 )
               )
             )
    ),
    #Sai
    tabPanel("Comparison Between Regions",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "region_name", label = "Choose a Region First Selection", choices = c("North East", "South East", "South West", "Mid West", "West")),
                 br(),
                 selectInput(inputId = "region_name2", label = "Choose a Region Second Selection", choices = c("North East", "South East", "South West", "Mid West", "West")),
                 h3("Graph Description"),
                 p("This visualization allows the user to input two different regions of the United States and see the average temperature by month and the average mortality rate by month for each of them. Each region of the United States has a different bar color associated with it and the graph titles change to show the names of the regions. The X axis is the month while the Y axis is either the average mortality rate (%) or the average temperature in Fahrenheit. The graphs are grouped by region vertically and by the subject, (mortality or temperature), horizontally." )
               ),
               mainPanel(
                 fluidRow(
                   column(width = 6,
                          h4(textOutput("scatter_heading")),
                          plotlyOutput(outputId = "scatter"),
                   ),
                   column(width = 6,
                          h4(textOutput("scatter2_heading")),
                          plotlyOutput(outputId = "scatter2"),
                   )
                 ),
                 
                 fluidRow(
                   column(width = 6,
                          h4(textOutput("scatter1_heading")),
                          plotlyOutput(outputId = "scatter1"),
                   ),
                   column(width = 6,
                          h4(textOutput("scatter3_heading")),
                          plotlyOutput(outputId = "scatter3"),
                   )
                 )
                 
               )
             )
    ),
    tabPanel("Comparison within Regions ",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   h1("Select the Region of your Interest"),
                   selectInput(
                     inputId = "region_steve",
                     label = "Select a Region",
                     choices = unique(df$Region)
                   ),
                   h3("Graph Description"),
                   p("The scatterplot of the temperature in the first tab panel shows the average 
          temperature of the three states within the same region over the year. 
          This will help us compare the average temperature for the three states within 
          the same region. The scatterplot of the mortality rate of COVID-19 in the 
          second tab panel shows the mortality rate of the three states within the same 
          region over the year. This will help us to see the mortality rate for the three 
          states within the same region. This will help us figure out if there are possibilities 
          of a correlation between average temperature and the mortality rate of COVID-19."
                   ),
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     tabPanel("Average Temperature", plotOutput(outputId = "scatter1_steve")),
                     tabPanel("Mortality Rates", plotOutput(outputId = "scatter2_steve"))
                   )
                 )
               )
             )
    ),
  tabPanel("Comparison by state",
           fluidRow(
             column(
               width = 12,
               sidebarLayout(
                 sidebarPanel(
                   fluidRow(
                     column(
                       width = 12,
                       h3("Select the State of Your Interest"),
                       selectInput("state1", "State 1", choices = states),
                       selectInput("state2", "State 2", choices = states)
                     )
                   ),
                   fluidRow(
                     column(
                       width = 12,
                       h3("Graph Description"),
                       p("This line graph represents the relationship between COVID-19 mortality rates and average temperature for each month in different states. You can choose two states from the select input boxes to compare their average temperature and mortality rate."),
                       p("The graph illustrates the general trend of temperature and mortality rate changes for each state in the year 2021, allowing users to see which state has a higher average temperature and how that relates to COVID-19 mortality rate.")
                     )
                   )
                 ),
                 mainPanel(
                   uiOutput("selectedStates"),
                   plotOutput("tempPlot"),
                   plotOutput("mortalityPlot")
                 )
               )
             )
           )
  ),
  tabPanel("Conclusion",
           fluidRow(
             column(
               width = 12,
               tags$img(src = "https://www.apha.org/-/media/Images/Topics/COVID/Covid_not_gone_graphic.ashx", 
                        width = "100%")
             )
           ),
           fluidRow(
             column(
               width = 12,
               div(class = "text-box",
                   h3("Takeaways"),
                   p("One takeaway from this data analysis project is that there are no obvious correlations between the average temperature and the mortality rate of COVID-19 in regional-level analysis, however, there are small correlations at the state level on a case-by-case basis for these variables. Cases where the mortality rate is high for states during lower temperature seasons and cases the other way around do exist. This might be an indication that there are other variables that affect the mortality rate of COVID-19 in a more significant manner than the weather temperature. This will help researchers in data and medical fields look for lurking variables that might have an effect on the mortality rate of COVID-19. Even though there seems to be a correlation between the average temperature and mortality rates, this cannot definitively be determined in this project due to the variance."),
                   p("Another takeaway is that data analysis and visualization are critical tools to help prove or disprove research questions and hypotheses. At the beginning of this project, our group thought that there would be a strong correlation between our variables. This was due to our background research mentioning that viruses are more active during warmer months. Through the tools we learned in this class, we were able to clear up our own misconceptions and disprove our hypothesis.")
               )
             )
           ),
           fluidRow(
             column(
               width = 12,
               div(class = "text-box",
                   h3("Conclusion"),
                   p("We conducted this project using the zoom-in data story. We started at the regional level comparison and zoomed into the state-level comparison for data analysis. The advantage of using a zoom-in data story is that starting from a wider lens and zooming into smaller aspects enables us to have a broad perspective on our question at the start, then move into a more nuanced understanding and view."),
                   p("In conclusion, we know that there are small correlations between average temperature and COVID-19 mortality rate at the state level. This could be a finding that future researchers refer to in order to plan further studies. There were no correlations when looking at the data by region, but after zooming in we found some sporadic correlations. Therefore, this could be a topic for researchers in the medical field to dig deeper into so that they may learn more about the variables that affect the mortality rates for COVID-19. In a more realistic sense, because of the lack of correlation between our two variables, future studies could also ignore weather temperature and focus on other variables. These variables could include population density, average BMI, vaccine distribution rates, etc.")
               )
             )
           ),
           fluidRow(
             column(
               width = 12,
               div(class = "text-box",
                   h3("Group Reflection"),
                   p("Throughout this data analysis project, our team was able to clean, aggregate, and summarize data in search of a conclusion. Though we did not find any conclusive data correlations, our project has broadened the perspective of how COVID-19 research should be conducted and has, for the most part, ruled out weather temperature as an important factor in Covid mortality. These analysis and visualization skills will be very useful in our respective careers. Through this project, we believe that we have a strong core understanding of the learning objectives of Informatics 201.")
               )
             )
           )
  )
           
            )
  )

# server stuff
#Bill Server stuff
server <- function(input, output) {
  # Title for the page
  output$selectedStates <- renderUI({
    tags$h1(paste("Selected States: ", input$state1, "and", input$state2))
  })
  # Average Temperature plot
  output$tempPlot <- renderPlot({
    state1_data <- subset(df, df$State.name == input$state1)
    state2_data <- subset(df, df$State.name == input$state2)
    avg_temp_state1 <- aggregate(Average.temperature.. ~ month, state1_data, mean)
    avg_temp_state2 <- aggregate(Average.temperature.. ~ month, state2_data, mean)
    ggplot() +
      geom_line(data = avg_temp_state1, aes(x = month, y = Average.temperature.., color = input$state1)) +
      geom_line(data = avg_temp_state2, aes(x = month, y = Average.temperature.., color = input$state2)) +
      labs(x = "Month", y = "Average Temperature", color = "State") +
      scale_color_manual(
        values = c("blue", "red"),
        breaks = c(input$state1, input$state2),
        labels = c(input$state1, input$state2)
      ) +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      scale_y_continuous(limits = c(0, 100), labels = scales::number_format()) +
      theme_minimal() +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 18, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 13)) +
      ggtitle(paste("Average Temperature of", input$state1, "and", input$state2)) +
      xlab("Month") + ylab("Average Temperature (°F)")
  })
  
  
  # Mortality rate plot
  output$mortalityPlot <- renderPlot({
    state1_data <- subset(df, df$State.name == input$state1)
    state2_data <- subset(df, df$State.name == input$state2)
    state1_data$M_Rate <- as.numeric(gsub("%", "", state1_data$M_Rate))/100
    state2_data$M_Rate <- as.numeric(gsub("%", "", state2_data$M_Rate))/100
    mortality_state1 <- aggregate(M_Rate ~ month, state1_data, mean)
    mortality_state2 <- aggregate(M_Rate ~ month, state2_data, mean)
    ggplot() +
      geom_line(data = mortality_state1, aes(x = month, y = M_Rate, color = input$state1)) +
      geom_line(data = mortality_state2, aes(x = month, y = M_Rate, color = input$state2)) +
      labs(x = "Month", y = "Mortality Rate", color = "State") +
      scale_color_manual(
        values = c("blue", "red"),
        breaks = c(input$state1, input$state2),
        labels = c(input$state1, input$state2)
      ) +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      scale_y_continuous(limits = c(0.00, 0.03), labels = scales::percent_format()) +
      theme_minimal() +
      theme(legend.text = element_text(size = 15),
            legend.title = element_text(size = 18, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 13)) +
      ggtitle(paste("Average Mortality Rate of", input$state1, "and", input$state2)) +
      xlab("Month") + ylab("Mortality Rate")
  })
  
  
#sai server stuff
  df <- read.csv("Combined_Covid_19_and_State_Temperature.csv")
  
  df <- mutate(df, death_rate = (df$tot_death/df$tot_cases) * 100)
  
  ne_df <- filter(df, Region == "North East")
  se_df <- filter(df, Region == "South East")
  sw_df <- filter(df, Region == "South West")
  midw_df <- filter(df, Region == "Mid West")
  w_df <- filter(df, Region == "West")
  
  ne_grp <- group_by(ne_df, month)
  ne_group <- summarize(ne_grp, mean_death = mean(death_rate), mean_temp = mean(Average.temperature..))
  
  se_grp <- group_by(se_df, month)
  se_group <- summarize(se_grp, mean_death = mean(death_rate), mean_temp = mean(Average.temperature..))
  
  sw_grp <- group_by(sw_df, month)
  sw_group <- summarize(sw_grp, mean_death = mean(death_rate), mean_temp = mean(Average.temperature..))
  
  
  midw_grp <- group_by(midw_df, month)
  midw_group <- summarize(midw_grp, mean_death = mean(death_rate), mean_temp = mean(Average.temperature..))
  
  w_grp <- group_by(w_df, month)
  w_group <- summarize(w_grp, mean_death = mean(death_rate), mean_temp = mean(Average.temperature..))
  
  ne_temp_lineplot <- ggplot(ne_group, aes(x = month, y = mean_temp, fill= "North East")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Temperature (Farenheit)") +
    scale_fill_manual(values = c("North East" = "red"), name = "") +
    theme_minimal()
  ne_temp_lineplot
  
  ne_death_lineplot <- ggplot(ne_group, aes(x = month, y = mean_death, fill= "North East")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Death rate (%)") +
    scale_fill_manual(values = c("North East" = "red"), name = "") +
    theme_minimal()
  ne_death_lineplot
  
  se_death_lineplot <- ggplot(se_group, aes(x = month, y = mean_death, fill= "South East")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Death rate (%)") +
    scale_fill_manual(values = c("South East" = "blue"), name = "") +
    theme_minimal()
  se_death_lineplot
  
  se_temp_lineplot <- ggplot(se_group, aes(x = month, y = mean_temp, fill= "South East")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Temperature (Farenheit)") +
    scale_fill_manual(values = c("South East" = "blue"), name = "") +
    theme_minimal()
  se_temp_lineplot
  
  sw_death_lineplot <- ggplot(sw_group, aes(x = month, y = mean_death, fill = "South West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Death rate (%)") +
    scale_fill_manual(values = c("South West" = "cyan"), name = "") +
    theme_minimal()
  sw_death_lineplot
  
  sw_temp_lineplot <- ggplot(sw_group, aes(x = month, y = mean_temp, fill = "South West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Temperature (Farenheit)") +
    scale_fill_manual(values = c("South West" = "cyan"), name = "") +
    theme_minimal()
  sw_temp_lineplot
  
  midw_death_lineplot <- ggplot(midw_group, aes(x = month, y = mean_death, fill = "Mid West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Death rate (%)") +
    scale_fill_manual(values = c("Mid West" = "green"), name = "") +
    theme_minimal()
  midw_death_lineplot
  
  midw_temp_lineplot <- ggplot(midw_group, aes(x = month, y = mean_temp, fill = "Mid West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Temperature (Farenheit)") +
    scale_fill_manual(values = c("Mid West" = "green"), name = "") +
    theme_minimal()
  midw_temp_lineplot
  
  w_death_lineplot <- ggplot(w_group, aes(x = month, y = mean_death, fill = "West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Death rate (%)") +
    scale_fill_manual(values = c("West" = "purple"), name = "") +
    theme_minimal()
  w_death_lineplot
  
  w_temp_lineplot <- ggplot(w_group, aes(x = month, y = mean_temp, fill = "West")) +
    geom_bar(stat="identity") +
    xlab("Month") +
    ylab("Temperature (Farenheit)") +
    scale_fill_manual(values = c("West" = "purple"), name = "") +
    theme_minimal()
  w_temp_lineplot
  
  output$song_name <- renderUI({
    
  })
  
  output$scatter <- renderPlotly({
    
    region_name <- input$region_name
    if (region_name == "North East") {
      return(ne_temp_lineplot)
    } else if (region_name == "South East") {
      return(se_temp_lineplot)
    } else if (region_name == "South West") {
      return(sw_temp_lineplot)
    } else if (region_name == "Mid West") {
      return(midw_temp_lineplot)
    } else {
      return(w_temp_lineplot)
    }
    
  })
  
  output$scatter2 <- renderPlotly({
    
    region_name2 <- input$region_name2
    if (region_name2 == "North East") {
      return(ne_temp_lineplot)
    } else if (region_name2 == "South East") {
      return(se_temp_lineplot)
    } else if (region_name2 == "South West") {
      return(sw_temp_lineplot)
    } else if (region_name2 == "Mid West") {
      return(midw_temp_lineplot)
    } else {
      return(w_temp_lineplot)
    }
    
  })
  output$scatter1 <- renderPlotly({
    
    region_name <- input$region_name
    if (region_name == "North East") {
      return(ne_death_lineplot)
    } else if (region_name == "South East") {
      return(se_death_lineplot)
    } else if (region_name == "South West") {
      return(sw_death_lineplot)
    } else if (region_name == "Mid West") {
      return(midw_death_lineplot)
    } else {
      return(w_death_lineplot)
    }
  })
  
  output$scatter3 <- renderPlotly({
    
    region_name2 <- input$region_name2
    if (region_name2 == "North East") {
      return(ne_death_lineplot)
    } else if (region_name2 == "South East") {
      return(se_death_lineplot)
    } else if (region_name2 == "South West") {
      return(sw_death_lineplot)
    } else if (region_name2 == "Mid West") {
      return(midw_death_lineplot)
    } else {
      return(w_death_lineplot)
    }
  })
  output$scatter_heading <- renderText({
    region_name <- input$region_name
    paste("Avg Temperature Data for", region_name)
  })
  
  output$scatter2_heading <- renderText({
    region_name2 <- input$region_name2
    paste("Avg Temperature Data for", region_name2)
  })
  
  output$scatter1_heading <- renderText({
    region_name <- input$region_name
    paste("Avg Mortality Rate Data for", region_name)
  })
  
  output$scatter3_heading <- renderText({
    region_name2 <- input$region_name2
    paste("Avg Mortality Rate Data for", region_name2)
  })
  
  #Steve server stuff
  filtered_data <- reactive({
    subset(df, Region == input$region_name)
  })
  
  output$scatter1_steve <- renderPlot({
    region_info <- filter(df, Region == input$region_steve)
    a <- ggplot(region_info, aes(x=month, y=Average.temperature.., color = Region))+
      geom_point()+
      scale_color_manual(values = c("South West"="red","North East"="brown","South East"="darkgreen","Mid West"="blue","West"="purple"))+
      ggtitle("Average Temperature Of States In A Region Over A Year")+
      labs(y = "Average Temperature", x = "Month")+
      scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"))+
      theme(plot.title = element_text(size= 17))+
      theme(plot.title= element_text(face= "bold"))
    return(a)
  })
  
  output$scatter2_steve <- renderPlot({
    region_info <- filter(df, Region == input$region_steve)
    b <- ggplot(region_info, aes(x=month, y=M_Rate, color = Region))+
      geom_point()+ 
      scale_color_manual(values = c("South West"="red","North East"="brown","South East"="darkgreen","Mid West"="blue","West"="purple"))+
      ggtitle("Mortality Rates of COVID-19 Of States In A Region Over A Year")+
      labs(y = "Mortality Rate", x = "Month")+
      scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12"))+
      theme(plot.title = element_text(size= 17))+
      theme(plot.title= element_text(face= "bold"))
    return(b)
  })
  
  #U.S map stuff
  output$map <- renderPlotly({
    # Convert the selected month name to a numeric value
    selected_month <- match(input$month, month.name)
    
    # Filter the data based on the selected month
    filtered_data <- df[df$month == selected_month, ]
    
    # Create the choropleth plot
    p <- plot_ly(
      data = filtered_data,
      type = 'choropleth',
      locations = filtered_data$state,
      locationmode = 'USA-states',
      z = filtered_data$Average.temperature..,
      colorscale = 'Reds',
      text = paste("State: ", filtered_data$State.name, "<br>Temperature: ", filtered_data$Average.temperature..),
      colorbar = list(title = "Average Temperature")
    )
    
    # Set the layout with zooming on the US
    p <- layout(
      p,
      title = paste0("Temperature Representation of States in ", input$month),
      geo = list(scope = 'usa', projection = list(type = 'albers usa'))
    )
    
    # Return the plotly object
    p
  })
}

  
# Run the Shiny app
shinyApp(ui, server)