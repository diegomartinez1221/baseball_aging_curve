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
           titlePanel("Aging Curves of Major League Baseball Hitters"),
  
  #explaining project, created in the server. 
  
           htmlOutput("warExplanation"),
  
           mainPanel(
             
  #histogram of War plot 
             
             plotOutput("histPlot"),
             
             br(),
             br(),
             
   #explanation of first analysis of aging curves. created in the server.  
   
             htmlOutput("plotExplanation"), 
             
   # general all players aging curve plot.
   
             plotOutput("tierPlot")
             
           ))
  ,
  
  #second tab

  tabPanel("Comparing Players",
           
           # Application title
           
           titlePanel("Individual Aging Curves"),
           
           #Select player from dropdown menu for their aging curve graph.
           #multiple can be selected for comparison. Albert Pujols is a good
           #current player with a typical aging curve to reference
           
           sidebarLayout(
             sidebarPanel(
               selectInput("name",label = strong("Players"),
                           choices = unique(complete_dataset$name_common),
                           selected = "Albert Pujols",
                           multiple = TRUE
               )),
             
             # Show aging curve plot
             
             mainPanel(
               plotOutput("linePlot"),
               br(),
               br(),
               h4("As you will notice by searching and comparing different players, not everyone
                  has the same career trajectories. The nice curves from the previous page
                  are only the averages of all the various shaped curves. Not all players peak exactly 
                  around 28 and not everyone will begin to decline shortly after, many players 
                  plateau for awhile. However, this is a fun tool to compare many of the greats
                  in history!")
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
               plotOutput("posPlot"),
               h5( "Footnote: OF includes all 3 outfield positions, the dataset did not split players
                   into their specific outfield positons. The gradual decline may seem odd as 
                   Center Field is a very difficult position to play; however, this may be due 
                   to all Outfielders being lumped together. Also, Center Fielders tend to
                   transition to the easier Corner Outfield positions as they get older."),
               br(),
               br(),
               h3("Insights"),
               br(),
               h4("Players at harder defensive positions have a steeper decline.
                  This is especially evident looking at catchers. The wear and tear on the
                  knees from squatting seems to really hamper careers. Shortstops also show
                  really steep drop offs as it is also one of the hardest positions on the
                  diamond to play. Typical hitting positions like First Base and Outfield exhibit
                  a more gradual decline, which extends their careers.")
             )
             
             
           )
  ),
  
  tabPanel("Problems with Salaries",
           
           #title 
           
           titlePanel("Salary Curves Do Not Imitate Performance Curves"),
           sidebarLayout(fluid = TRUE,
             sidebarPanel(
               
               #button to pick tier for second plot and gt table. need to make
               #sure the name salary_tier is different then name of radio button
               #from previous tab or else both buttons will influence both
               #graphs on each panel
          
          
               
               radioButtons("salary_tier", 
                            "Tier:", 
                            unique(salary_dataset$tier),
                            selected = "1")
    
              ),
          mainPanel(
            h4("For my analysis of salaries, you will notice I used Standard Salaries
               instead of just Salary. This is because the datset goes all the way back 
               to 1950. The effects of inflation and the difference in the amount of money 
              in baseball today distorts the graphs. Thus I standardized salaries for each year 
              meaning the y axis is salaries compared to the rest of the league in that year. Thus, 
               values close to 0 are near the average salary in a particular year, while +/- 2 are
               either extremely large or small salaries respectively."),
            
            plotOutput("salaryPlot"),
            br(),
            br(),
            gt_output("peakTable1"),
            br(),
            br(),
            gt_output("peakTable2"),
            h5("Peak Salary Age has gone down indicating changes in how MLB
               Teams approach paying players. Peak WAR seems to have decreased
               in some positions; however, this is due to having players that are
               still very young in the last year of the dataset, 2018, and have not 
               reached the age of when players usually peak around 27-28. The importance
              of this table is the gap between Peak WAR (27-28) and Salary Age has decreased
              from the previous table."),
            br(),
            br(),
            h3("Conclusions"),
            br(),
            h4("Salaries peak way later than when performance peaks.
               We can see this in the graphs as well as in the tables. 
               The problem is MLB teams are having to pay for what the player was,
               not necessarily who the player will become. All the ages of 
              peak salary are easily within the declining years of careers.
               Many contracts have been back heavy, meaning salaries increase gradually
               each year of the contract. This is extremely inefficient seeing how 
               players decline in performance this late in their careers and as they get worse 
              you are paying them more and more.

              Consecutively, 27-29 is when most players enter free agency, the time when players are
              allowed into the open market and when players sign big money contracts. As we 
              have seen, around 27-29 is when players hit their peak thus the free agency system
              is set up in a way where free agent signings are coming off some of the best years of their
              career. The term 'Free Agency Year' refers to the year before a 
              player hits the open market where a player hopes to play at his best to garner a larger contract
              the next year. Thus, it must be extremely difficult to not want to offer a large contract
              to a player performing extremely well the past few years while trying also forecast future 
              performance because a decline is sure to come.  
              However, we see a tightening of the gap between Peak Performance and Peak Salary
              in recent years. Front Offices are being smarter and must be taking aging 
              factors into account in their models of player evaluation. If they are going to sign
               someone for a long period, they will lower the per year value like what occured with
               Bryce Harper. Going forward, perhaps we will see teams begin to structure salaries 
              like aging curves with a peak that correlates with the ages of peak performance and 
               then declines as the player gets older.")
            
          )
           )
  ),
  
  tabPanel("About",
           
           #giving credit to the sources of my data. Also including my repo. 
           
           mainPanel(
             h5("This project was created for Gov 1005: Data, a course taught by David Kane at Harvard University."),
             br(),
             h5("I would like to give thanks to Sean Lahman for compiling the", a("Lahman Baseball Database", 
                href="http://www.seanlahman.com/baseball-archive/statistics/"), "and Baseball Reference for", 
                a("WAR Data", href = "https://www.baseball-reference.com/data/war_daily_bat.txt"), "which is
                updated daily."),
             br(),
             h5("A link to my Github repository can be found",
                a("here.", href = "https://github.com/diegomartinez1221/baseball_aging_curve"))
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
new ways Major League Baseball Teams have approached signing players. No longer are the best free agent 
bats being signed to the largest per year value contracts. Bryce Harper's $330 Million Dollar contract 
is the largest free agent contract in American Sports History; however, it is not what he was projected to 
receive a few years earlier in the neighborhood of the $400 over 10 years. Yes, it may 
be the largest sum of salary contract, but when you look at the salary per year, which is roughly 25 million per 
year over 13 years, he is not in the top 10 of per year value. It was rumored the Giants were in on Bryce Harper
offering around $40 million per year; however for a short span of time. In the current market we are seeing an interesting 
trade off between contract longevity and per year value. I believe this is due to player's aging curves. Player performance
and age are not a linear relationship; however, they tend to be curved, improving gradually until a certain age then a 
decline. MLB teams are willing to pay for a players prime, but in doing so they will also have to deal with overpaying for 
the player's eventual decline. Aging Curves and being able to predict a players future are becoming an increasing part of 
the game today. Through this project, I seek to better understand aging curves myself as well as help you.
")

ex_2<- p("I will use Wins Above Replace (WAR) as a metric that takes into account all aspects
of the game including offense, defense, and running to analyze performance curves. The goals for this project are to 
compare the improvement and eventual decline of all batters' careers dating back to 1950. I will compare players across 
generations aging curves to show how the career paths of today's stars compare to legends of 
the past. I also compare players' position to see if different types of of players age differently.
Finally, I dig into salaries to see how salaries change as players age compared to their performance.")
  


ex_3<- p(strong("WAR"))
ex_4<- p("Wins Above Replacement (WAR) is an attempt by the sabermetric baseball community to summarize 
         a player’s total contributions to their team in one statistic. You should always use more than 
         one metric at a time when evaluating players, but WAR is all-inclusive and provides a useful 
         reference point for comparing players (Fangraphs). A negative WAR= Worse than Minor League Replacement, 0-2=Role/Bench Player, 2-3=Starter, 3-4=High Quality Starter, 4-5=All Star, 5-6 =Superstar Talents,
        6+=MVP or Hall of Fame Caliber. To help you become more familiar with the statistic,
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
            I use for the entire project. Finally, a note on survivorship bias. Approaching
            the tail end of the distrution of age, these numbers may not be as representative
            due to a lack of data. Not many players will make it into their late 30s or early 40s,
            only the very best 38-42 year olds will 'survive' that long in the MLB.")  
  
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
      
      #formatting and adding meaningful titles and caption explanation.
      
      labs(x="Age",
           y = "War",
           color = "Players")+ 
      theme_fivethirtyeight()+
      theme(axis.title = element_text(colour = "black" ),
            legend.position = "none",
            plot.caption = element_text(hjust = 0)
            )
  })
  

  
#tab 4 --------------------------
 

   
output$salaryPlot <-renderPlot({
  
  #make sure to now use salary dataset
  
  salary_dataset%>%
    
  #tiers were numbers, need them to be factors to color in the graph
    
    mutate(tier = as.factor(tier))%>%
    group_by(tier,age)%>%
    
    #mean standard salary per age created 
    
    mutate( player_count = n(), 
            ave_st_salary_age= mean(standard_salary))%>%
    
    #do not want few random points to influence graph. 
    
    filter(player_count > 5)%>%
    
    #graph for each tiers salary as they age
    
    ggplot(aes(x= age, y= ave_st_salary_age, 
               group = tier,
               color = tier))+
    geom_smooth(level = 0.50) +
    scale_x_continuous(breaks = seq(17,48,1), limits = c(17,48)) +
    labs(x= "Age", y= "Standardized Salaries", 
         title = "Salary Curves for MLB Hitters", 
         caption = "Lahman Database and Baseball Reference")
  
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
 
 output$peakTable1<-render_gt({
 
 peak_comparison()%>%
  
   # ages were for some reason character variables
          
   mutate(age_highest_war = as.numeric(age_highest_war),
          age_highest_salary= as.numeric(age_highest_salary))%>%
   group_by(POS)%>%
     
   #find the averages to be able to compare.  
     
   summarise(avg_age_war = mean(age_highest_war), 
             avg_age_salary= mean(age_highest_salary))%>%
   gt()%>%
   tab_header(title = "Comparing Ages of Peak Performance and Highest Pay, 1950-2018")%>%
   cols_label(POS= "Position",
              avg_age_war = "Age of Peak War",
              avg_age_salary= "Age of Peak Salary")%>%
     
    # do not need any decimals for age.
      
   fmt_number(columns= vars(avg_age_war, avg_age_salary), decimals= 0)
 
 
    
    
  })
 
 
 output$peakTable2<-render_gt({
   
   peak_comparison() %>%
     
     #only keeping players from recent years
     
     filter(year_ID>2013)%>%
     
     # ages were for some reason character variables
     
     mutate(age_highest_war = as.numeric(age_highest_war), 
            age_highest_salary= as.numeric(age_highest_salary))%>%
     group_by(POS)%>%
     
     #find the averages to be able to compare.  
     
     summarise(avg_age_war = mean(age_highest_war), avg_age_salary= mean(age_highest_salary))%>%
     gt()%>%
     tab_header(title = "How Peaks Have Changed in Recent Years, 2013-2018")%>%
     cols_label(POS= "Position",
                avg_age_war = "Age of Peak War",
                avg_age_salary= "Age of Peak Salary")%>%
     
     # do not need any decimals for age.
     
     fmt_number(columns= vars(avg_age_war, avg_age_salary), decimals= 0)
   
   
   
   
 })
 

  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
