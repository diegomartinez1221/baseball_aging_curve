library(shiny)
library(shinythemes)
library(tidyverse)

#data required

complete_dataset<-read_csv("complete_dataset.csv")
salary_dataset<-read_csv("salary_dataset.csv")

# Define UI for application and theme

ui <- fluidPage(theme = shinytheme("superhero"),tabsetPanel(
  
  # first tab 
  
  tabPanel("Introduction",
           titlePanel("Inefficiencies in Baseball Markets Due to Aging Curves"),
           htmlOutput("warExplanation"),
           mainPanel(
             plotOutput("histPlot"),
             
             br(),
             br(),
             
             htmlOutput("plotExplanation"), 
             
             plotOutput("tierPlot")
             
           ))
  ,
  
  #second tab

  tabPanel("Comparing Players",
           
           # Application title
           
           titlePanel("Individual Aging Curves"),
           
           # Select player from dropdown menu for their aging curve graph. 
           
           sidebarLayout(
             sidebarPanel(
               selectInput("name",label = strong("Players"),
                           choices = unique(complete_dataset$name_common),
                           selected = "Mike Trout",
                           multiple = TRUE
               )),
             
             # Show aging curve plot
             
             mainPanel(
               plotOutput("linePlot")
             )
           )
  ),
  
  # tab 3
  
  tabPanel("Comparing Positions",
           titlePanel("How Players At Different Positions Age"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("tier", 
                            "Tier:", 
                            unique(complete_dataset$tier),
                            selected = 1),
               checkboxGroupInput("position",
                                  "Position:",
                                  choices = c("C","1B", "2B","3B","SS","OF"), 
                                  selected = "OF")),
             mainPanel(
               plotOutput("posPlot")
             )
             
             
           )
  )
)
)
# Define server logic required to show lineplot

server <- function(input, output) {
  
  #necessary libraries 
  
  library(ggplot2)
  library(tidyverse)
  library(ggthemes)
  library(directlabels)
  
  # text explanation of project
  
  output$warExplanation<-renderUI({

ex_1<-p("As I have watched the last few Baseball Offseasons unfold, I have become extremely interested in the 
new trends Major League Baseball Teams have have approached signing players.No longer are the best free agent 
bats being signed to the largest per year value contracts. Bryce Harper's $330 Million Dollar Contract has been 
lauded over for being the largest free agent contract in American Sports History; however, it is nothing compared 
to what he was projected to receive a few years earlier in the neighborhood of the $400 over 10 years. Yes, it may 
be the largest sum of salary contract, but when you look at the salary per year, which is roughly 25 million per 
year over 13 years, he is not even in the top 10 of per year value. It was rumored the Giants were in on Bryce Harper
offering around $40 million per year; however for a short period of time. In the current market we are seeing an interesting 
trade off between contract longetivity and per year value. I believe this is due to player's aging curves. Player performance
and age are not a linear relationship; however, they tend to be curved, improving gradually until a certain age then a 
decline. MLB teams are willing to pay for a players prime, but in doing so they will also have to deal with overpaying for 
the players eventual decline. Aging Curves and being able to predict a players future are becoming an increasing part of 
the game today. Through this project, I seek to better understand aging curves myself as well as help you.
")

ex_2<- p("I will use Wins Above Replace (WAR) as a metric that takes into account all aspects
of the game to analyze performance curves. The goals for this project are to compare the improvement 
and eventual decline of all batters careers dating back to 1950. I wil compare players across 
generations aging curves to show how the career paths of today's stars compare to legends of 
the past. I also compare players' position to see if different types of of players age differently.
Finally,I built models based off of retired players to predict how we can expect current stars careers
to play out as well as to see on average change of in WAR from yearto year of a player's career.")
  


ex_3<- p(strong("WAR"))
ex_4<- p("'Wins Above Replacement (WAR) is an attempt by the sabermetric baseball community to summarize 
         a player’s total contributions to their team in one statistic. You should always use more than 
         one metric at a time when evaluating players, but WAR is all-inclusive and provides a useful 
         reference point for comparing players (Fangraphs).'A negative WAR= Worse than Minor League Replacement, 0-1=Scrub,
         1-2= Role Player, 2-3=Solid Player, 3-4=High Quality Starter, 4-5= All Star, 5-6 = Superstar Talents,
        6+= MVP or Hall of Fame Caliber. To help you become more familiar with the statistics,
         I have created this histogram so you can understand the range and distribution of WAR, which 
        as you can see is skewed to the right.")


  HTML(paste(ex_1, br(),ex_2, br(), ex_3, ex_4))
  
  
  
  
})  
  
  output$histPlot<- renderPlot({
    complete_dataset%>%
      ggplot(aes(x=WAR))+geom_histogram(bins = 100)+scale_x_continuous(breaks = seq(-3,12,1))+
      scale_y_continuous(breaks = seq(0,3000,250), limits = c(0,3000))+ labs(x="WAR", y= "Count", title= "Distribution of WAR") +
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ))
    
  })
  
  output$tierPlot<-renderPlot({
    complete_dataset%>%
      mutate(tier = as.factor(tier))%>%
      group_by(tier,age)%>%
      mutate( player_count = n(), ave_war_age= mean(WAR))%>%
      filter(player_count > 3)%>%
      ggplot(aes(x= age, y= ave_war_age))+geom_smooth(level = 0.50, aes(group = tier, color = tier)) +
      scale_x_continuous(breaks = seq(18,45,1), limits = c(18,45))+
      scale_y_continuous(breaks = seq(-1,6,1), limits = c(-1,6))+
      labs(x= "Age",
           y= "Average War", 
           title = "Aging Curves for MLB Hitters", 
           subtitle ="Average War Per Age for Each Tier of Hitters",
           caption = "Lahman Database and Baseball Reference",
           color = "Tier")+
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ))
  })
  
  output$plotExplanation<-renderUI({
    
    
    ex_1<-p("There is a stark difference between the very best players in the MLB
            and everyday starters let alone the average player. Thus, I chose to tier 
            my dataset for analysis purposes. Otherwise, I believe the generalized aging 
            curves would not be representative. By taking the z-score of the average WAR
            for each player across their career, I broke the dataset into 5 tiers, tier 1
            being the very best to ever play the game. Even they show the same levels 
            of decline as they age.")  
  
    HTML(paste(ex_1))
  })
  
  # tab 2 ------------------------------------ 
  #creating variables for line graph. Needed to be reactive so that it changes
  #as each player in the drop down menu is selected.
  
  subset<-reactive({complete_dataset%>% filter(name_common %in%input$name)})
  
  
  output$linePlot <- renderPlot({
    
    # draws line plot for player chosen
    
    ggplot(subset(), aes(x=subset()$age, y=subset()$WAR, color = subset()$name_common))+
      geom_smooth(formula = y~x+x^2, se = FALSE)+
      geom_line(alpha = 0.30)+
      geom_point(alpha = 0.65)+
      scale_y_continuous(breaks = seq(-4,13,1), limits = c(-4,13)) + 
      scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
      labs(x="Age", y = "War", color = "Players")+
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ))
  }
  )
  
  # tab 3 ____________________________________
  
  positions<-reactive({complete_dataset%>% 
      filter(tier == input$tier)%>%
      filter(POS %in% input$position)
  })
  
  output$posPlot <- renderPlot({
    
    positions()%>%
      group_by(age, POS)%>%
      mutate(people = n(),ave_war = mean(WAR))%>%
      filter(people>3)%>%
      
      # draws line plot for player chosen
      
      ggplot(aes(x=age, y=ave_war))+
      geom_smooth(aes(color= POS, fill = POS))+
      geom_dl(aes(label = POS), method = list(dl.combine( "last.points"))) +
      scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
      scale_y_continuous(breaks = seq(-1,7,1), limits = c(-1,7)) +
      labs(x="Age", y = "War", color = "Players")+
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ),
            legend.position = "none")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
