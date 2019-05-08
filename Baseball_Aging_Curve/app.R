library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)

#data required. creted the .csvs in Data Manipulation.R script

complete_dataset<-read_csv("complete_dataset.csv")
salary_dataset<-read_csv("salary_dataset.csv")

# Define UI for application and theme

ui <- fluidPage(theme = shinytheme("superhero"),tabsetPanel(
  
  # first tab 
  
  tabPanel("Introduction",
           
  #descriptive title to engage viewers       
           titlePanel("Aging Curves Affect On Major League Baseball"),
  
  #explaining project, created in the server. 
  
           htmlOutput("warExplanation"),
  
           mainPanel(
             
  #histogram of War plot 
             plotOutput("histPlot"),
             
             br(),
             br(),
             
   #exlanation of first analysis of aging curves. created in the server.  
   
             htmlOutput("plotExplanation"), 
             
   # general all players aging curve plot.
   
             plotOutput("tierPlot")
             
           ))
  ,
  
  #second tab

  tabPanel("Comparing Players",
           
           # Application title
           
           titlePanel("Individual Aging Curves"),
           
           # Select player from dropdown menu for their aging curve graph.
           #multiple can be selected for comparison
           
           sidebarLayout(
             sidebarPanel(
               selectInput("name",label = strong("Players"),
                           choices = unique(complete_dataset$name_common),
                           selected = "Albert Pujols",
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
               
               #buttons to pick a tier, cannot pick multiple because graph would
               #get convoluted
               
                 radioButtons("tier", 
                            "Tier:", 
                            unique(complete_dataset$tier),
                            selected = 1),
                
                #ability to check off positions within a tier to compare aging
                #curves of positions
                 
               checkboxGroupInput("position",
                                  "Position:",
                                  choices = c("C","1B", "2B","3B","SS","OF"), 
                                  selected = "OF")),
             mainPanel(
               plotOutput("posPlot")
             )
             
             
           )
  ),
  
  tabPanel("Problems with Salaries",
           
           #title 
           
           titlePanel("Salary Curves Do Not Immitate Performance Curves"),
           sidebarLayout(fluid = TRUE,
             sidebarPanel(
               
               #button to pick tier for second plot and gt table. need to make
               #sure the name salary_tier is different then name of radio button
               #from previous tab or else both buttons will influence both
               #graphs on each panel
          
              
               
               radioButtons("salary_tier", 
                            "Tier:", 
                            unique(salary_dataset$tier),
                            selected = "1"),
               
               #same concept as checkbox for position aging curves, also need a
               #different input name
               
               checkboxGroupInput("salary_position",
                                  "Position:",
                                  choices = c("C","1B", "2B","3B","SS","OF"), 
                                  selected = "OF")),
          mainPanel(
            h5("For my analysis of salaries, you will notice I used Standard Salaries
               instead of just Salary. This is because the datset goes all the way back 
               to 1950. The effects of inflation and the difference in the amount of money 
              in baseball today distorts the graphs. Thus I standardized salaries for each year 
              meaning the y axis is salaries compared to the rest of the league in that year. Thus, 
               values close to 0 are near the average salary in a particular year, while +/- 2 are
               either extremely large or small salaries respectively"),
            
            plotOutput("salaryPlot"),
            br(),
            br(),
            plotOutput("salaryposPlot"),
            br(),
            br(),
            gt_output("peakTable"),
            br(),
            br(),
            h3("Conclusions"),
            br(),
            h5("Salaries peak way later than when performance peaks.
               We can see that in the graphs as well as in the table. 
               The problem is MLB teams are paying for what the player was,
               not necessarily who the player will become. All the ages of 
               the peak salary are easily within the declining years of careers.
               Many contracts are back heavy, meaning salaries increase gradually
               each year of the contract, which is extremely inefficient seeing how 
               players decline in performance this late in their careers.
               The data I use for these plots goes back to 1950. It would be interesting to
               see if subsetting the data to the Moneyball era and recent years has an effect 
               on age of peak salary. Certainly with how the market has been over the past few offseasons,
               I would assume that there would be a difference as teams now do not want to sign these 
               long contracts and waste money on the decline of once great players.")
            
          )
           )
  ),
  
  tabPanel("About",
           
           #giving credit to the sources of my data. Also including my repo. 
           
           mainPanel(
             h5("This project was created for Gov 1005: Data, a course taught by David Kane at Harvard University."),
             br(),
             h5("I would like to give thanks to Sean Lahman for compiling", a("the Lahman Baseball Database", href="http://www.seanlahman.com/baseball-archive/statistics/"), "and
             Baseball Reference for", a("WAR Data", href = "https://www.baseball-reference.com/data/war_daily_bat.txt"), "which is updated daily."),
             br(),
             h5("A link to my Github repository can be found", a("here.", href = "https://github.com/diegomartinez1221/baseball_aging_curve"))
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
  
#tab 1 --------------------------------  
  
  
  # text explanation of project
  
  output$warExplanation<-renderUI({

ex_1<-p("As I have watched the last few Baseball Offseasons unfold, I have become extremely interested in the 
new trends Major League Baseball Teams have approached signing players. No longer are the best free agent 
bats being signed to the largest per year value contracts. Bryce Harper's $330 Million Dollar Contract has been 
lauded over for being the largest free agent contract in American Sports History; however, it is nothing compared 
to what he was projected to receive a few years earlier in the neighborhood of the $400 over 10 years. Yes, it may 
be the largest sum of salary contract, but when you look at the salary per year, which is roughly 25 million per 
year over 13 years, he is not even in the top 10 of per year value. It was rumored the Giants were in on Bryce Harper
offering around $40 million per year; however for a short period of time. In the current market we are seeing an interesting 
trade off between contract longevity and per year value. I believe this is due to player's aging curves. Player performance
and age are not a linear relationship; however, they tend to be curved, improving gradually until a certain age then a 
decline. MLB teams are willing to pay for a players prime, but in doing so they will also have to deal with overpaying for 
the player's eventual decline. Aging Curves and being able to predict a players future are becoming an increasing part of 
the game today. Through this project, I seek to better understand aging curves myself as well as help you.
")

ex_2<- p("I will use Wins Above Replace (WAR) as a metric that takes into account all aspects
of the game including offense, defense, and running to analyze performance curves. The goals for this project are to 
compare the improvement and eventual decline of all batters' careers dating back to 1950. I wil compare players across 
generations aging curves to show how the career paths of today's stars compare to legends of 
the past. I also compare players' position to see if different types of of players age differently.
Finally, I dig into salaries to see how salaries change as players age compared to their performance.")
  


ex_3<- p(strong("WAR"))
ex_4<- p("'Wins Above Replacement (WAR) is an attempt by the sabermetric baseball community to summarize 
         a player’s total contributions to their team in one statistic. You should always use more than 
         one metric at a time when evaluating players, but WAR is all-inclusive and provides a useful 
         reference point for comparing players (Fangraphs).'A negative WAR= Worse than Minor League Replacement, 0-1=Scrub,
         1-2= Role Player, 2-3=Solid Player, 3-4=High Quality Starter, 4-5= All Star, 5-6 = Superstar Talents,
        6+= MVP or Hall of Fame Caliber. To help you become more familiar with the statistics,
         I have created this histogram so you can understand the range and distribution of WAR, which 
        as you can see is skewed to the right.")

#collects all the text from above, orders them and displays them with breaks so
#they are not clumped together

  HTML(paste(ex_1, br(),ex_2, br(), ex_3, ex_4))
  
  
  
  
})  
  
  #plot displaying histogram of WAR
  
  output$histPlot<- renderPlot({
    
    #using complete dataset, no need for salary data
    
    complete_dataset%>%
      
      ggplot(aes(x=WAR)) +
      
      #WAR is on a small scale so I wanted to include many bins. 
      
      geom_histogram(bins = 100) +
      
      #range of WAR
      
      scale_x_continuous(breaks = seq(-3,12,1)) +
      
      #redid breaks of y axis to give better idea of the counts 
      
      scale_y_continuous(breaks = seq(0,3000,250), limits = c(0,3000)) +
      
      #nicer labels
      
      labs(x="WAR", y= "Count", title= "Distribution of WAR") +
      
      #I chose to do all my graphs in 538 format. 
      
      theme_fivethirtyeight() +
      
      #originally 538 graphs do not include axis labels, this adds axis labels.
      
      theme(axis.title = element_text(colour = "black" ))
    
  })
  
  output$tierPlot<-renderPlot({
    complete_dataset%>%
      
      #tier was a number causing the color in the graph to be a scale, this
      #converts to a factor giving each curve a different color.
      
      mutate(tier = as.factor(tier))%>%
      
      #necessary combination to get average curves for each age
      
      group_by(tier,age)%>%
      
      #creating average WAR for each age for each tier to see differences and
      #the curved shape of performance
      
      mutate( player_count = n(), ave_war_age= mean(WAR))%>%
      
      #do not want one person at 1 age to influence the curve 
      
      filter(player_count > 3)%>%
      
      #creating ggplot
      
      ggplot(aes(x= age, y= ave_war_age)) +
      
      #creates the curved line for each tier 
      
      geom_smooth(level = 0.50, aes(group = tier, color = tier)) +
      
      # range of ages in dataset
      
      scale_x_continuous(breaks = seq(18,45,1), limits = c(18,45)) +
      
      #not the complete range of war.
      
      scale_y_continuous(breaks = seq(-1,6,1), limits = c(-1,6)) +
      labs(x= "Age",
           y= "Average War", 
           title = "Aging Curves for MLB Hitters", 
           subtitle ="Average War Per Age for Each Tier of Hitters",
           color = "Tier") +
      
      #fivethirtyeight theme for aesthetic purposes and adding axis labels 
      
      theme_fivethirtyeight() +
      theme(axis.title = element_text(colour = "black" ))
  })
  
  
  #explanation for the tier plot. 
  
  output$plotExplanation<-renderUI({
    
    
    ex_1<-p("There is a stark difference between the very best players in the MLB
            and everyday starters let alone the average player. Thus, I chose to tier 
            my dataset for analysis purposes. Otherwise, I believe the generalized aging 
            curves would not be representative. By taking the z-score of the average WAR
            for each player across their career, I broke the dataset into 5 tiers, tier 1
            being the very best to ever play the game. Even they show the same levels 
            of decline as they age. This tiered breakdown of the data is the same groupings 
            I use for the entire project.")  
  
    HTML(paste(ex_1))
  })
  
  # tab 2 ------------------------------------ 
  #creating variables for line graph. Needed to be reactive so that it changes
  #as each player in the drop down menu is selected.
  
  # needs to be reactive to be able to change with different names that are
  # selected from drop down box
  
  subset<-reactive({complete_dataset%>% filter(name_common %in%input$name)})
  
  
  output$linePlot <- renderPlot({
    
    # draws line plot for player chosen
    
    ggplot(subset(), aes(x=subset()$age, y=subset()$WAR, color = subset()$name_common))+
      
    #smoothed trend line of performance  
      
      geom_smooth(formula = y~x+x^2, se = FALSE, size = 1.5)+
      
      # actual datapoints and WARs from players career. 
      
      geom_line(alpha = 0.20)+
      geom_point(alpha = 0.5)+
      
    # ranges of WAR and Age in the dataset
      
      scale_y_continuous(breaks = seq(-4,13,1), limits = c(-4,13)) + 
      scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
      
      
      labs(x="Age", y = "War", color = "Players",
           caption = "The smoothed line represents the general trend 
           of players career while the transparent line and points
           represent the players actaul performance each year")+
      
      # for aesthetic purpose and including axis labels 
      
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ))
  }
  )
  
  # tab 3 ____________________________________
  

  
  positions<-reactive({complete_dataset%>% 
      
      # reacts to the radiobutton selection
      
      filter(tier == input$tier)%>%
      
      #rects to checkbox selections 
      
      filter(POS %in% input$position)

  })
  
  output$posPlot <- renderPlot({
    
    positions()%>%
      
      #necessary group by to analysis data collectively for  each position 
      
      group_by(age, POS)%>%
      
      #mean war per positon per age.
      
      mutate(people = n(),ave_war = mean(WAR))%>%
      
      # do not want 1 player at very old or young ages to affect curves.
      
      filter(people>3)%>%
      
      # draws curve for each position within each tier selected 
      
      ggplot(aes(x=age, y=ave_war))+
      
      #gives same color to line and confidence interval
      
      geom_smooth(aes(color= POS, fill = POS))+
      
      #eliminates need for legend, gives position at very end of line. make sure
      #to download the library
      
      geom_dl(aes(label = POS), method = list(dl.combine( "last.points"))) +
      
      #ranges of age and WAR rescaled
      
      scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
      scale_y_continuous(breaks = seq(-1,7,1), limits = c(-1,7)) +
      labs(x="Age", y = "War", color = "Players")+
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ),
            legend.position = "none")
  })
  

  
#tab 4 --------------------------
 

   
output$salaryPlot <-renderPlot({
  
  #make sure to now use salary dataset
  
  salary_dataset%>%
    
  #tiers were numbers, need them to be factors to color in the graph
    
    mutate(tier = as.factor(tier))%>%
    group_by(tier,age)%>%
    
    #mean standard salary per age created 
    
    mutate( player_count = n(), ave_st_salary_age= mean(standard_salary))%>%
    
    #do not want few random points to influence graph. 
    
    filter(player_count > 5)%>%
    
    #graph for each tiers salary as they age
    
    ggplot(aes(x= age, y= ave_st_salary_age, group = tier, color = tier))+
    geom_smooth(level = 0.50) +
    scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
    labs(x= "Age", y= "Standardized Salaries", 
         title = "Salary Curves for MLB Hitters", 
         caption = "Lahman Database and Baseball Reference")
  
})

  # same code for the position graph from the previous panel, except using
  # salary dataset and salaries for analysis

  new_salaries<-reactive({salary_dataset%>% 
      filter(tier == input$salary_tier)%>%
      filter(POS %in% input$salary_position)
    
  })

output$salaryposPlot <- renderPlot({
  
  new_salaries()%>%
    group_by(age, POS)%>%
    mutate(people = n(),ave_salary = mean(standard_salary))%>%
    filter(people>1)%>%
    ggplot(aes(x=age, y=ave_salary))+
    geom_smooth(se= FALSE, aes(color= POS))+
    scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
    scale_y_continuous(breaks = seq(-1,2,0.5), limits = c(-1,2)) +
    labs(x="Age", y = "Standard Salary", color = "Position",
         title = "Salary Curves by Position")+
    theme_fivethirtyeight()+
    theme(axis.title = element_text(colour = "black" ))
})

#creating variables for the table at the bottom of the page. I created at what
#age do players earn their highest salary as well as at what age do players
#perform the best. I create 6 year minimum because you need to have played 6
#years to be able to enter free agency

  
max_salary<-salary_dataset%>%
  group_by(bbrefID)%>%
  filter(years >=6)%>%
  filter(salary == max(salary))%>%
  filter(WAR == max(WAR))%>%
  mutate(age_highest_salary = age, 
         war_highest_salary = WAR,
         salary_highest_salary = salary)%>%
  select(bbrefID, year_ID,age_highest_salary, war_highest_salary, salary_highest_salary)


max_WAR<-salary_dataset%>%group_by(bbrefID)%>%
  filter(years >=6)%>%
  filter(WAR == max(WAR))%>%
  filter(salary == max(salary))%>%
  mutate(age_highest_war = age, 
         war_highest_war = WAR,
         salary_highest_war = salary)%>%
  select(bbrefID, year_ID,age_highest_war, war_highest_war, salary_highest_war)
     
#join the two created variables

 max_comparison<-max_WAR%>%inner_join(max_salary, by= "bbrefID")
 
 #join it to the pull salary dataset and make it reactive so it changes with the
 #tier that is chosen
  
 peak_comparison<-reactive({
   
   salary_dataset%>%inner_join(max_comparison, by= "bbrefID")%>%
     filter(tier == input$salary_tier)
 })
 
 output$peakTable<-render_gt({
 
 peak_comparison()%>%
  
   # ages were for some reason character variables
          
   mutate(age_highest_war = as.numeric(age_highest_war), age_highest_salary= as.numeric(age_highest_salary))%>%
   group_by(POS)%>%
     
   #find the averages to be able to compare.  
     
   summarise(avg_age_war = mean(age_highest_war), avg_age_salary= mean(age_highest_salary))%>%
   gt()%>%
   tab_header(title = "Comparing Age of Peak Performance to Age of Highest Pay")%>%
   cols_label(POS= "Position",
              avg_age_war = "Age of Peak War",
              avg_age_salary= "Age of Peak Salary")%>%
     
    # do not need any decimals for age.
      
   fmt_number(columns= vars(avg_age_war, avg_age_salary), decimals= 0)
 
 
    
    
  })
 
 


  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
