library(readr)
library(tidyverse)

#Loading Data
nfldata <- read_csv("~/Desktop/Data Science/Stat231JackDove/Homeworks/spreadspoke_scores.csv")

#WRANGLING

#Wrangling by Year
nfltotalsdata <- nfldata %>%
  group_by(schedule_season) %>%
  summarize(avg_score_total = mean(score_home + score_away), 
            avg_score_difference = mean((score_home-score_away)), 
            avg_temperature= (mean(weather_temperature)), 
            avg_spread_magnitude = mean(abs(spread_favorite)), 
            avg_overunder = mean(over_under_line),
            avg_spread_accuracy = mean(abs(abs(score_home-score_away)-abs(spread_favorite))),
            avg_overunder_accuracy = mean(abs((score_home+score_away)-over_under_line)))

#Wrangling by Points bins (and year)
nfltotalsdata_pointsbins <- nfldata %>%
  mutate(scoretotal = score_home + score_away) %>%
  mutate(scoringbin = ifelse(scoretotal > mean(scoretotal), "highscoring", "lowscoring")) %>%
  group_by(schedule_season, scoringbin) %>%
  summarize(avg_score_total = mean(score_home + score_away), 
            avg_score_difference = mean((score_home-score_away)), 
            avg_temperature= (mean(weather_temperature)), 
            avg_spread_magnitude = mean(abs(spread_favorite)), 
            avg_overunder = mean(over_under_line),
            avg_spread_accuracy = mean(abs(abs(score_home-score_away)-abs(spread_favorite))),
            avg_overunder_accuracy = mean(abs((score_home+score_away)-over_under_line)))

#In 1988, Over Under is Consistently Tracked: Year Data
post1978nfltotalsdata <- nfltotalsdata %>%
  filter(schedule_season > 1987)

#In 1988, Over Under is Consistently Tracked: Bin Data
post1978nfltotalsdata_pointsbins <- nfltotalsdata_pointsbins %>%
  filter(schedule_season > 1987)





#CHARTS

#Plot 1

y_choice_values1 = names(post1978nfltotalsdata)[5:6]
y_choice_names1 <- c("Average Spread Magnitude"
                    , "Average Over/Under")

#Point Spread Magnitude Column Chart
point_spread_magnitude <- ggplot(data= post1978nfltotalsdata, aes(x=schedule_season, y = avg_spread_magnitude), color = "Green") + geom_line() + geom_point()

#Over Under Column Chart
over_under <- ggplot(data= post1978nfltotalsdata, aes(x=schedule_season, y = avg_overunder), color = "Green") + geom_line() + geom_point()

#Plot 2

y_choice_values2 = names(post1978nfltotalsdata)[5:6]
y_choice_names2 <- c("Average Spread Magnitude"
                     , "Average Over/Under")

y_choice_values3 = names(post1978nfltotalsdata)[2:3]
y_choice_names3 <- c("Average Score Total"
                     , "Average Score Difference")

#Total Score vs. Over Under Line Chart
totalscore_vs_overunder_chart <- ggplot(data=post1978nfltotalsdata, aes(x=schedule_season)) +
                                         geom_line(aes(y=avg_score_total), color = "Red") +
                                         geom_line(aes(y=avg_overunder), color = "Blue")

#Score Difference vs. Spread Magnitude Line Chart
scorediff_vs_spreadmag_chart <- ggplot(data=post1978nfltotalsdata, aes(x=schedule_season)) +
  geom_line(aes(y=avg_score_difference), color = "Red") +
  geom_line(aes(y=avg_spread_magnitude), color = "Blue")

#Plot 3

y_choice_values4 = names(post1978nfltotalsdata_pointsbins)[8:9]
y_choice_names4 <- c("Average Spread Accuracy"
                     , "Average Over/Under Accuracy")

#Spread Accuracy by Points Bin Line Chart
spread_accuracy_by_points_bin <- ggplot(
  data= post1978nfltotalsdata_pointsbins, 
  aes(x= schedule_season, y= avg_spread_accuracy, group=scoringbin, color=scoringbin)) + geom_line()
  
#Over Under Accuracy by Points Bin Line Chart
Over_Under_Accuracy_by_points_bin <- ggplot(
  data= post1978nfltotalsdata_pointsbins, 
  aes(x= schedule_season, y= avg_overunder_accuracy, group=scoringbin, color=scoringbin)) + geom_line()


library(shiny)

ui <- fluidPage(
  
  h1("Historical Overview of NFL Betting Lines"),
  
  sidebarLayout(
    sidebarPanel(
  #Line Chart of Outcome Variable vs. Time
      h3("Chart Inputs"),
      h4("Figure 1: "),
  radioButtons(inputId = "plot1"
               , label = "Choose a betting line type: "
               , choiceValues = y_choice_values1
               , choiceNames = y_choice_names1
               , selected = "avg_spread_magnitude"),
  #Line Chart of Betting Line Type and Actual Value vs. Time
  h4("Figure 2: "),
  radioButtons(inputId = "plot2pt1"
               , label = "Choose a betting line type: "
               , choiceValues = y_choice_values2
               , choiceNames = y_choice_names2
               , selected = "avg_spread_magnitude"),
  radioButtons(inputId = "plot2pt2"
               , label = "Choose an outcome measure: "
               , choiceValues = y_choice_values3
               , choiceNames = y_choice_names3
               , selected = "avg_score_difference"),
  #Line Chart of Betting Line Type Accuracy by Points Bin vs. Time
  h4("Figure 3: "),
  radioButtons(inputId = "plot3"
               , label = "Choose a betting line accuracy type: "
               , choiceValues = y_choice_values4
               , choiceNames = y_choice_names4
               , selected = "avg_spread_accuracy"),
  h3("About"),
  p("Point spreads and over/under lines from 1978-2020 were collected from 
  repole.com/sun4cast/data.html and sportsline.com. These plots display historical trends in lines,
  their proximity to game outcomes, and their accuracy by game scoring level."),
  h4("Definitions"),
  tags$ul(
    tags$li("Spread Magnitude = |Favorite's Point Spread|"),
    tags$li("Over/Under = Posted Over/Under Line"),
    tags$li("Score Total = Home Score + Away Score"),
    tags$li("Score Difference = |Home Score - Away Score|"),
    tags$li("Spread Accuracy = |Spread Magnitude - Total Score| (Zero = Perfect Line)"),
    tags$li("Over/Under Accuracy = |Over/Under - Total Score| (Zero = Perfect Line)"),
  ),
  h3("Upcoming Game Lines (DraftKings)"),
  tableOutput(outputId = "scrape"),
  
), 
mainPanel(
  plotOutput(outputId = "plotone"),
  plotOutput(outputId = "plottwo"),
  plotOutput(outputId = "plotthree")
)
)
)

server <- function(input, output) {
  
  
  output$plotone <- renderPlot({
    
    ggplot(data= post1978nfltotalsdata, 
           aes_string(x= "schedule_season", y = input$plot1), 
           color = "Green") + 
           geom_line() + 
           geom_point() +
           labs(x = "Season", y=y_choice_names1[y_choice_values1==input$plot1], title = "Figure 1: Betting Line Trends") 
    
  })
  
  output$plottwo <- renderPlot({
    
    ggplot(data=post1978nfltotalsdata, 
           aes(x=schedule_season)) +
             geom_line(aes_string(y=input$plot2pt1), color = "Red") +
             geom_line(aes_string(y=input$plot2pt2), color = "Blue")  +
           labs(x = "Season", y="Points", subtitle = "Red: Betting Line, Blue: Outcome", title = "Figure 2: Betting Lines vs. Outcomes")
    
  })
  
  output$plotthree <- renderPlot({
  
    ggplot(data= post1978nfltotalsdata_pointsbins) + 
    geom_line(aes_string(x="schedule_season", y = input$plot3, color = "scoringbin")) + 
      labs(x="Season", y=y_choice_names4[y_choice_values4==input$plot3], title = "Figure 3: Betting Line Accuracy by Scoring Level")
  
  })
  
  output$scrape <- renderTable ({
    library(tidyverse)
    library(robotstxt)
    library(rvest)
    library(knitr)
    library(janitor)
    library(shiny)
    scrapeurl <- "https://sportsbook.draftkings.com/leagues/football/3?category=game-lines&subcategory=game"
    awayspread <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[2]/div[2]/div[1]/div/div/div/div[1]/span') %>%
      html_text()
    homespread <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[2]/div[2]/div[2]/div/div/div/div[1]/span') %>%
      html_text()
    overunder <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[2]/div[3]/div[1]/div/div/div/div[1]/span[3]') %>%
      html_text()
    awayteam <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[2]/div[1]/div[1]/a/div/div[2]/span') %>%
      html_text()
    hometeam <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[2]/div[1]/div[2]/a/div/div[2]/span') %>%
      html_text()
    date <- scrapeurl %>%
      read_html() %>%
      html_node(xpath = '//*[@id="root"]/section/section[2]/section/div[3]/div/div[3]/div/div/div[2]/div/div[2]/div[2]/section/div[1]/div[1]/div/span') %>%
      html_text()
    
    colnames <- c("Date", "Home Team", "Home Spread", "Away Team", "Away Spread", "Over/Under")
    values <- c(date, hometeam, homespread, awayteam, awayspread, overunder)
    
    data <- data.frame(colnames, values)
    data1 <- data %>%
      rename (Game = colnames) %>%
      rename(Information = values)
    data1
    
    
    
  
    
    
  })
  
}

shinyApp(ui = ui, server = server)





