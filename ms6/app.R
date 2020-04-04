#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(DT)

count_reports <- read_csv("raw-data/Count Reports for AC - Sheet1.csv")[-2,]
contest_table <- read_csv("raw-data/contest_table.txt")

count_reports <- count_reports %>% clean_names() %>% na.omit("Description") %>%
  mutate(description = as.numeric(description)) %>%
  mutate(precinct_name = description %% 100000)

contest_table <- contest_table %>% clean_names() %>%
  mutate(precinct_name = as.numeric(precinct_name)) %>%
  mutate(total_votes = absentee_votes + early_votes + election_votes) %>%
  mutate(vote_proportion = total_votes/ballots)

full_results <- left_join(contest_table, count_reports, by = "precinct_name") %>%
  filter(contest_title == "UNITED STATES REPRESENTATIVE 48th District") %>%
  filter(ad != "NA")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  selectInput("lat_ad", label = "Assembly District", choices = full_results$ad),
  selectInput("viet_c", label = "City", choices = full_results$city),
  
   # Application title
   titlePanel("2018 Midterm Elections in California's 48 Congressional District"),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("districtPlot"),
         plotOutput("lat_districtPlot")
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
   output$districtPlot <- renderPlot({
     full_results %>% filter(city == input$viet_c,
                             candidate_name == "HARLEY ROUDA") %>%
     ggplot(aes(x = vote_proportion, y = alyssa_vietnamese_an, color = ad)) + geom_point() +
       xlab("Vote Proportion") +
       ylab("Vietnamese Voters") +
       ggtitle("Proportion of Vote for Harley Rouda (D) \nin California's 48th Congressional District by Precinct",
               subtitle = "Sorted by City, Measured Against Number of Vietnamese Voters")
     
   })
   
   output$lat_districtPlot <- renderPlot({
     full_results %>% filter(ad == input$lat_ad,
                             candidate_name == "HARLEY ROUDA") %>%
       ggplot(aes(x = vote_proportion, y = alyssa_latino_an, color = city)) + geom_point() +
       xlab("Vote Proportion") +
       ylab("Vietnamese Voters") +
       ggtitle("Proportion of Latino Vote for Harley Rouda (D) \nin California's 48th Congressional District by Precinct",
               subtitle = "Sorted by Assembly District, Measured Against Number of Latino Voters")
     
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

