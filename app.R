library(shiny)
library(plotly)
library(readr)
library(tidyverse)
library(readxl)
#library(rnaturalearth)
library(ggplot2)
library(leaflet)
library(sf)
library(maps)

source("done_code.R", local = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML('
      body { 
        font-family: "Arial", sans-serif; 
        margin-top: 20px; /* Reduce top margin */
        margin-right: 220px; /* Maintain right margin */
        margin-bottom: 80px; /* Adjust bottom margin if needed */
        margin-left: 220px; /* Maintain left margin */ /* Increase body margin */
        background-color: #f8f9fa;}
.header { 
  text-align: center; 
  padding: 20px; 
}
.title { 
  font-size: 30px; 
  font-weight: bold; 
}
.content { 
  margin: 40px; /* Increase content margin */
    line-height: 1.6; 
  background-color: #ffffff; /* Ensure content background is white */
    padding: 20px; /* Padding inside the content area */
    border: 1px solid #dddddd; /* Optional: add border to the content area */
}
.plot { 
  margin: 60px; /* Increase plot margin */
    padding: 20px; /* Padding inside the plot area */
    background-color: #ffffff; /* White background for plots */
    border: 10px solid #dddddd; /* Optional: add border to the plot area */
}
'))
  ),
  div(class = "header",
    div(class = "title", "Tokyo 2020: A Benchmark for Gender Equality in Sports")
  ),
  div(class = "content",
      HTML("<p><strong>The Olympic Games have made several attempts to make it gender equal. However there are still some problem areas left, even though the Tokyo Olympic Games were nearly gender equal.</strong></p>"),
      
      p("The Tokyo 2020 Olympics emerged as a pivotal event in the history of gender equality in sports, nearly achieving equal participation between male and female athletes—a striking contrast to the Games of previous decades. With women representing 48% of the athletes, the event illustrated significant progress towards a more inclusive sporting world."),
      p("Within the narrative of the Tokyo 2020 Olympics, users are encouraged to engage with the visual data through interactive plots. The Participation Plot allows for a dynamic view of the gender distribution among athletes. By hovering over the plot, viewers can discover specific percentages and numbers, offering an intuitive understanding of the balance achieved."),
      p("Adjacent to this, the pie chart provides an immediate visual representation of the athlete composition by gender. Clicking on the segments of the pie chart will highlight the respective sections, detailing the proportion of female and male athletes."),
      fluidRow(
        column(6, 
               h3("Timeline Tokyo with Gender Distribution", style="text-align: center;"),
               plotlyOutput("plot_gender_participation")
        ),
        column(6, style = "display: flex; flex-direction: column; justify-content: center;", 
               h3("Gender in Tokyo 2020", style="text-align: center;"),
               plotlyOutput("pie_gender")
        )
      ),
      p("Beyond the numbers, the Games confronted the complexities of the pandemic, which not only tested the resilience of athletes but also sparked debates on inclusivity, particularly affecting female athletes who are mothers. The Games responded to these challenges, adapting policies to better support athlete parents."),
      p("This iteration of the Olympics showcased a significant trend among various nations, including the USA, Australia, the UK, Russia, Canada, and China, in advancing past traditional gender barriers by dispatching a proportionally higher number of female athletes. This shift mirrors a global progression towards gender equity in sports. Intriguingly, a statistical analysis revealed a weak to moderate negative correlation between the ratio of female to male athletes and the Gender Inequality Index (GII)—a measure reflecting women's disadvantage in reproductive health, empowerment, and labor market participation. The lower the GII-score the better women fare in the country. This suggests nations with greater female representation in the Olympics tend to fare better in gender equality overall.", 
        br(), 
        "As we delve deeper into the complexities faced during the pandemic, the discussion extends to the Interactive Map of Gender Equality. This map invites users to zoom in and out for a global perspective or a more detailed country-by-country analysis, revealing the correlation between a nation's gender equality index and its Olympic representation."),
      fluidRow(plotlyOutput("interactive_map_equality")
      ),
      p("A closer examination of the distribution of athletes by discipline showed a varied landscape: while some sports like football and water polo were still male-dominated, others such as handball and gymnastics exhibited a more equitable or female-favoring participation. These variances underscore the ongoing need for a multi-faceted approach to gender equality that encompasses not only equal representation but also equitable access to resources, media attention, and cultural support."),
      p("In essence, Tokyo 2020 was a mirror reflecting both the strides taken towards gender parity and it signifies that it is still a journey that remains. It underscored the transformative power of the Olympic platform to propel societal change and reminded us that the quest for equality in sports, as in all areas of life, requires persistent effort and unwavering commitment."),
      p("Caution: The plot about the disciplines is scrollable, you need to scroll to get the full information."),
      h3("Disciplines and their Gender Distribution", style="text-align: center;"),
            # Adding scrollable style to the plot
      div(style = "overflow-y: scroll; height: 400px;",  # Adjust height as needed
          plotlyOutput("interactive_discipline_event_ALL")
      ),
      p(" 
        "),
      p("This is a link to the Dashboard with further plots and information", 
        a("Link to the Dashboard", href = "https://milicapajkic.shinyapps.io/Narration_and_Visualization/", target = "_blank"))
  )
)

server <- function(input, output) {
  output$plot_gender_participation <- renderPlotly({ 
    
    # Assuming plot_gender_participation is already a plotly object created elsewhere in your code.
    
    highlighted_period <- list(
      type = "rect",
      # Add the rectangle coordinates
      x0 = 1980, x1 = 2020,
      y0 = -8000, y1 = 8000,
      fillcolor = "#FFF100", # Using yellow as the frame color
      opacity = 0.2,
      line = list(
        color = "#000"
      )
    )
    
    plot_gender_participation <- plot_gender_participation %>%
      layout(shapes = list(highlighted_period))
    
    plot_gender_participation <- plot_gender_participation %>% 
      layout(yaxis = list(tickformat = ',.0f')) %>% 
      config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"))
    plot_gender_participation 
  })
  
  
  # Render the Interactive GII Map
  output$interactive_map_GII <- renderPlotly({
    interactive_map_GII %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoomIn2d", "zoomOut2d", "pan2d",
      "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
      "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
      "toggleHover", "resetViews", "sendDataToCloud",
      "toggleSpikelines", "resetViewMapbox"
    ))
  })
  
  output$interactive_map_equality <- renderPlotly({
    interactive_map_equality %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoomIn2d", "zoomOut2d", "pan2d",
      "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
      "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
      "toggleHover", "resetViews", "sendDataToCloud",
      "toggleSpikelines", "resetViewMapbox"
    ))
  })
  
  # Render the Interactive Map of Gender Equality
  output$interactive_map_equality_chosen <- renderPlotly({
    # Depending on the input, render the appropriate map
    req(input$map_equality_type) # Ensure that input$map_equality_type is not NULL
    if(input$map_equality_type == "actual") {
      interactive_map_equality %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      )) # Assuming this is the actual map plot object
    } else {
      interactive_map_equality_ratio %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      )) # Assuming this is the ratio map plot object
    }
  })
  
  # Render the Pie Chart of Gender Distribution
  output$pie_gender <- renderPlotly({ 
    pie_gender %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
      "zoomIn2d", "zoomOut2d", "pan2d",
      "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
      "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
      "toggleHover", "resetViews", "sendDataToCloud",
      "toggleSpikelines", "resetViewMapbox"
    )) })
  
  output$interactive_discipline_event_ALL <- renderPlotly({ 
    interactive_discipline_event %>%
      layout(autosize = TRUE, height = 600) %>%  # Set the height to your desired value
      config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      ))
  })
  
  
  # Render the Interactive Discipline Event Plot
  output$interactive_discipline_event <- renderPlotly({
    if(input$discipline_type == "all") {
      interactive_discipline_event  %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      )) # Full data
    } else if(input$discipline_type == "female_dominated") {
      # Assuming code to filter female dominated disciplines
      interactive_discipline_event_women %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      ))
    } else {
      # Assuming code to filter male dominated disciplines
      interactive_discipline_event_men %>% config(displayModeBar = TRUE, modeBarButtonsToRemove = list(
        "zoomIn2d", "zoomOut2d", "pan2d",
        "select2d", "lasso2d", "zoomInGeo", "zoomOutGeo", "resetGeo", 
        "hoverClosestGeo", "hoverClosestGl2d", "hoverClosestPie",
        "toggleHover", "resetViews", "sendDataToCloud",
        "toggleSpikelines", "resetViewMapbox"
      ))
    }
  })
}

shinyApp(ui, server)