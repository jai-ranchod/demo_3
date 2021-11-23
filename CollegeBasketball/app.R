#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- fluidPage(
  #tags$h2("Add a shiny app background image"),
  setBackgroundImage(src = "https://2hbs3q1gce7v3ovpvh34am44-wpengine.netdna-ssl.com/wp-content/uploads/2016/03/MDollarBracketChallenge790x415-Background-450x270.jpg"),
    #Row 1
    column(12,wellPanel(
      titlePanel("NCAA Basketball Metrics by Conference: 2020-2021 Season"),
      h5("The purpose of this app is to compare the relationships between key performance indicators and outcomes across conferences."),
    h6("All metrics on a per game basis.  Data includes regular season, conference tournament, and national tournament games in the 2020-2021 season.  
                           Axes are scaled to match across conferences for outcome and metrics. Information pulled from https://www.sports-reference.com"),style = "padding: 5px;")),
                       
    #Row 2
    column(3,{wellPanel(
            pickerInput("conf1", "Conference #1 (Left)",
                         c("ACC" = "ACC",
                           "Big Ten" = "BigTen",
                           "Pac 12" = "Pac12",
                           "Big 12" = "Big12",
                           "SEC" = "SEC",
                           "Atlantic 10" = "A10",
                           "West Coast Conference" = "WCC",
                           "Big East" = "BigEast",
                           "Missouri Valley Conference" = "MVC"),
                         selected = "ACC"),style = "padding: 5px;")}),
    column(3,{wellPanel(
         pickerInput("conf2", label = "Conference #2 (Right)",
                        choices = c("ACC" = "ACC",
                          "Big Ten" = "BigTen",
                          "Pac 12" = "Pac12",
                          "Big 12" = "Big12",
                          "SEC" = "SEC",
                          "Atlantic 10" = "A10",
                          "West Coast Conference" = "WCC",
                          "Big East" = "BigEast",
                          "Missouri Valley Conference" = "MVC"),
                        selected = "BigTen", multiple = FALSE,),style = "padding: 5px;")}),
    column(3,{wellPanel(pickerInput(inputId = "metric",
                        label = "Metric",
                     choices = c(
                         "--Offensive--",
                         "Assists" = "AST",
                         "Effective Field Goal %" = "eFGPct",
                         "Field Goals" = "FG",
                         "Field Goal Attempts" = "FGA",
                         "Field Goal %" = "FGPct",
                         "Free Throws" = "FT",
                         "Free Throw Attempts" = "FTA",
                         "Free Throw %" = "FTPct",
                         "Offensive Rating" = "OR",
                         "Offensive Rebounds" = "ORB",
                         "Points For" = "PTS",
                         "Three Point Shots Made" = "ThreePT",
                         "Three Point Shot Attempts" = "ThreePTA",
                         "Three Point Shot %" = "ThreePTPct",
                         "Turnovers" = "TOV",
                         "--Defensive--",
                         "Blocks" = "BLK",
                         "Defensive Rating (negative metric)" = "DRT",
                         "Defensive Rebounds" = "DRB",
                         "Points Against" = "PTSA",
                         "Steals" = "STL",
                         "--General--",
                         "Net Rating" = "NetRating",
                         "Personal Fouls" = "PF",
                         "Total Rebounds" = "TRB",
                         "--Advanced--",
                         "Strength of Schedule" = "SOS",
                         "Marginal Possession Points" = "PointsPlus",
                         "Pace" = "Pace"),
                     selected = "AST",
                     multiple = FALSE,
                     choicesOpt = list(disabled = c("--Offensive--","AST", "eFGPct", "FG", "FGA", "FGPct", "FT", "FTA", "FTPct", "OR", "ORB", "PTS", "ThreePT", "ThreePTA", "ThreePct", "TOV",
                                                    "--Defensive--", "BLK", "DRT", "DRB", "PTSA", "STL",
                                                    "--General--", "NetRating", "PF", "TRB",
                                                    "--Advanced--", "SOS", "PointsPlus", "Pace") %in% c("--Offensive--", "--Defensive--", "--General--","--Advanced--")) #List must be in the same order as the dropdown options
                     ),style = "padding: 5px;")}), #radio button 2
    column(3,{wellPanel(selectInput("outcome", "Outcome",
                        c("Win %" = "WinPct",
                          "Average Point Differential" = "AvgPointDiff"),
                        selected = "WinPct"),style = "padding: 5px;")}),
    #Row 3
    column(width = 6,align = "center",{

                    plotOutput("distPlot1")

          }),#SecondColumn
    column(6,align = "center",{
      plotOutput("distPlot2")}),
    #Row 4
    column(width = 6,wellPanel(align = "center",strong(textOutput("text1")))),
    column(6,wellPanel(align = "center",strong(textOutput("text2"))))
  )#fluidpage

# Define server logic required to draw a histogram
server <- function(input, output) 
{
    withProgress(message = "Checking Required Packages", min = 0, max = 100,{
    { if(!require("rvest")) install.packages("rvest")
    if(!require("reldist")) install.packages("reldist")
    if(!require("data.table")) install.packages("data.table")
    if(!require("dplyr")) install.packages("dplyr")
    if(!require("tidyr")) install.packages("tidyr")
    if(!require("ggplot2")) install.packages("ggplot2")
    if(!require("ggrepel")) install.packages("ggrepel")
    
    library(rvest)
    library(reldist)
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(ggrepel)
   }#Checking for required packages
    setProgress(value = 5, message = "Loading ACC")
    {ACC_Names <- c("Syracuse", "Duke", "Virginia", "Florida State", "Virginia Tech", "Georgia Tech", "Clemson", "UNC", "Louisville", "NC State", "Notre Dame", "Pitt", "Miami (FL)", "Wake Forest", "Boston College")
    ACC_MadeTourney <- c("Yes","No","Yes","Yes","Yes","Yes","Yes","Yes","No","No","No","No","No","No","No")
    
    ACCTeams <- as.data.frame(cbind(ACC_Names, ACC_MadeTourney))
    
    colnames(ACCTeams)[1] <- "School"
    colnames(ACCTeams)[2] <- "MadeTourney"
    }#ACC
    setProgress(value = 10, message = "Loading Big Ten")
    {BigTen_Names <- c("Michigan", "Illinois", "Iowa", "Purdue", "Ohio State", "Wisconsin", "Rutgers", "Maryland", "Michigan State", "Indiana", "Penn State", "Northwestern", "Minnesota", "Nebraska")
    BigTen_MadeTourney <- c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No","No")
    
    BigTenTeams <- as.data.frame(cbind(BigTen_Names,BigTen_MadeTourney))
    colnames(BigTenTeams)[1] <- "School"
    colnames(BigTenTeams)[2] <- "MadeTourney"
    }#Big Ten
    setProgress(value = 15, message = "Loading Pac 12")
    {Pac12_Names <- c("Arizona", "Arizona State", "California", "Colorado", "Oregon", "Oregon State", "Stanford", "UCLA", "USC", "Utah", "Washington", "Washington State")
    Pac12_MadeTourney <- c("No", "No", "No", "Yes","Yes", "Yes", "No", "Yes", "Yes", "No", "No","No")
    
    Pac12Teams <- as.data.frame(cbind(Pac12_Names,Pac12_MadeTourney))
    colnames(Pac12Teams)[1] <- "School"
    colnames(Pac12Teams)[2] <- "MadeTourney"
    }#Pac 12
    setProgress(value = 20, message = "Loading Big 12")
    {Big12_Names <- c("Baylor", "Kansas", "Texas", "West Virginia", "Oklahoma State","Texas Tech", "Oklahoma", "TCU", "Kansas State", "Iowa State")
    Big12_MadeTourney <- c("Yes", "Yes","Yes","Yes","Yes","Yes","Yes","No", "No", "No")
    
    Big12Teams <- as.data.frame(cbind(Big12_Names, Big12_MadeTourney))
    colnames(Big12Teams)[1] <- "School"
    colnames(Big12Teams)[2] <- "MadeTourney"
    }#Big 12
    setProgress(value = 25, message = "Loading SEC")
    {SEC_Names <- c("Alabama", "Arkansas", "LSU", "Tennessee", "Florida", "Ole Miss", "Missouri", "Kentucky", "Mississippi State", "Georgia", "South Carolina", "Vanderbilt", "Texas A&M", "Auburn")
    SEC_MadeTourney <- c("Yes","Yes","Yes","Yes","Yes","No","Yes","No","No","No","No","No","No","No")
    
    SECTeams <- as.data.frame(cbind(SEC_Names, SEC_MadeTourney))
    colnames(SECTeams)[1] <- "School"
    colnames(SECTeams)[2] <- "MadeTourney"
    }#SEC
    setProgress(value = 30, message = "Loading Atlantic 10")
    {A10_Names <- c("St. Bonaventure", "VCU", "Dayton", "George Mason", "Davidson", "Duquesne", "Rhode Island", "La Salle", "UMass", "Richmond","Saint Louis", "George Washington", "St. Joseph's", "Fordham")
    A10_MadeTourney <- c("Yes", "Yes", "No","No","No","No","No","No","No","No","No","No","No","No")
    
    A10Teams <- as.data.frame(cbind(A10_Names,A10_MadeTourney))
    colnames(A10Teams)[1] <- "School"
    colnames(A10Teams)[2] <- "MadeTourney"}#A10
    setProgress(value = 35, message = "Loading WCC")
    {WCC_Names <- c("Gonzaga", "BYU", "Loyola Marymount", "Pepperdine", "Pacific", "Santa Clara", "Saint Mary's", "San Francisco", "San Diego", "Portland")
      WCC_MadeTourney <- c("Yes", "Yes","No", "No", "No", "No", "No","No","No", "No")
      WCCTeams <- as.data.frame(cbind(WCC_Names, WCC_MadeTourney))
      colnames(WCCTeams)[1] <- "School"
      colnames(WCCTeams)[2] <- "MadeTourney"
    }#WCC
    setProgress(value = 40, message = "Loading Big East")
    {
      BigEast_Names <- c("Creighton", "UConn", "Villanova", "Seton Hall", "St. John's (NY)", "Providence", "Butler", "Marquette", "Georgetown", "Xavier", "DePaul")
      BigEast_MadeTourney <- c("Yes", "Yes", "Yes", "No", "No", "No", "No", "No", "Yes", "No", "No")
      
      BigEastTeams <- as.data.frame(cbind(BigEast_Names, BigEast_MadeTourney))
      colnames(BigEastTeams)[1] <- "School"
      colnames(BigEastTeams)[2] <- "MadeTourney"
      
    }#Big East
    setProgress(value = 45, message = "Loading Missouri Valley")
    {
      MVC_Names <- c("Loyola (IL)", "Drake", "Missouri State", "Indiana State", "Evansville", "Northern Iowa", "Valparaiso", "Bradley", "Southern Illinois", "Illnois State")
      MVC_MadeTourney <- c("Yes", "Yes", "No","No", "No", "No", "No", "No", "No", "No")
      MVCTeams <- as.data.frame(cbind(MVC_Names, MVC_MadeTourney))
      
      colnames(MVCTeams)[1] <- "School"
      colnames(MVCTeams)[2] <- "MadeTourney"
    }#MVC
    {
      df2 <- rbind(ACCTeams, BigTenTeams, Pac12Teams, Big12Teams,SECTeams,A10Teams, WCCTeams, BigEastTeams, MVCTeams)}#Combining Conference Tables
    setProgress(value = 60, message = "Loading Web Data")
    {confvec <- c("big-ten", "big-12","acc", "pac-12","sec","atlantic-10","wcc", "big-east", "mvc")
    confnamevec <- c("Big Ten", "Big 12", "ACC", "Pac 12", "SEC", "Atlantic 10", "West Coast Conference", "Big East", "Missouri Valley Conference")
    df1 <- data.frame()
    for(i in 1:length(confvec))
    {
      path <- "https://www.sports-reference.com/cbb/conferences/"
      
      extension <-"/2021.html"
      url <- paste(paste(path), paste(confvec[i]), paste(extension), sep = "")
      h <- read_html(url)
      Nodes <- h %>% html_nodes("table")
      table <- html_table(Nodes[[3]])
      table <- table[,-1] #Removing Rank Column
      table <- table[,-c(3:4)]
      table <- table[,-c(4:10)]
      
      table <- table[,-7]
      table <- table[,-26]
      
      table <- table[-1,] 
      Conference <- rep(confnamevec[i], nrow(table))
      table <- cbind(Conference, table)
      
      df1 <- rbind(table, df1)
    }
    
    colnames(df1)[2] <- "School"
    colnames(df1)[3] <- "Games"
    colnames(df1)[4] <- "WinPct"
    colnames(df1)[5] <- "OffensiveRating"
    colnames(df1)[6] <- "DefensiveRating"
    colnames(df1)[7] <- "NetRating"
    colnames(df1)[8] <- "FieldGoals"
    colnames(df1)[9] <- "FieldGoalAttempts"
    colnames(df1)[10] <- "FieldGoalPct"
    colnames(df1)[11] <- "ThreePointers"
    colnames(df1)[12] <- "ThreePointAttempts"
    colnames(df1)[13] <- "ThreePointPct"
    colnames(df1)[14] <- "EffectiveFGPct"
    colnames(df1)[15] <- "FreeThrows"
    colnames(df1)[16] <- "FreeThrowAttempts"
    colnames(df1)[17] <- "FreeThrowPct"
    colnames(df1)[18] <- "OffensiveRebounds"
    colnames(df1)[19] <- "TotalRebounds"
    colnames(df1)[20] <- "Assists"
    colnames(df1)[21] <- "Steals"
    colnames(df1)[22] <- "Blocks"
    colnames(df1)[23] <- "Turnovers"
    colnames(df1)[24] <- "PersonalFouls"
    colnames(df1)[25] <- "PointsFor"
    colnames(df1)[26] <-"PointsAgainst"
    colnames(df1)[27] <- "SimpleRatingSystem"
    colnames(df1)[28] <- "StrengthOfSchedule"
    colnames(df1)[29] <- "Pace"
    }#Pulling web data
    setProgress(value = 85, message = "Calculating Statistics")
    {for(i in 3:ncol(df1))
    {
      df1[,i] <- as.numeric(df1[,i])
      
    }
    
    df1 <- df1 %>% mutate(DefensiveRebounds = TotalRebounds-OffensiveRebounds, AveragePointDiff = (PointsFor - PointsAgainst))
    
    df <- inner_join(df1, df2, by = "School")
    
    df <- df %>% mutate(TwoPointAttempts = FieldGoalAttempts - ThreePointAttempts, TwoPointers = FieldGoals - ThreePointers, TwoPointPct = TwoPointers/TwoPointAttempts)
    df <-df %>% mutate(PointsPlus = (((TwoPointAttempts*TwoPointPct*2)+(ThreePointAttempts*ThreePointPct*3)+(FreeThrowAttempts*FreeThrowPct))/(FreeThrowAttempts+TwoPointAttempts+ThreePointAttempts))*(Steals+Blocks+OffensiveRebounds-Turnovers))
    
    df <- df %>% gather(metric, data, c(Games,OffensiveRating,DefensiveRating,PointsPlus,NetRating,FieldGoals,FieldGoalAttempts,FieldGoalPct,TwoPointers, TwoPointAttempts, TwoPointPct,ThreePointers, ThreePointAttempts, ThreePointPct,EffectiveFGPct, FreeThrows, FreeThrowAttempts, FreeThrowPct, OffensiveRebounds, DefensiveRebounds,TotalRebounds,
                                        Assists, Steals, Blocks, Turnovers, PersonalFouls, PointsFor, PointsAgainst, SimpleRatingSystem, StrengthOfSchedule, Pace))
    df <- df %>% gather(Outcome, NumericOutcome, c(WinPct, AveragePointDiff))
    
    df$MadeTourney <- as.factor(df$MadeTourney)
    df$data <- as.numeric(df$data)
    df$NumericOutcome <- as.numeric(df$NumericOutcome)
    
    {
    Assist_low <- df %>% filter(metric == "Assists") %>% summarise(min(data)) %>% pull()
    Assist_high <- df %>% filter(metric == "Assists") %>% summarise(max(data)) %>% pull()
    eFGPct_low <- df %>% filter(metric == "EffectiveFGPct") %>% summarise(min(data)) %>% pull()
    eFGPct_high <- df %>% filter(metric == "EffectiveFGPct") %>% summarise(max(data)) %>% pull()
    FG_low <- df %>% filter(metric == "FieldGoals") %>% summarise(min(data)) %>% pull()
    FG_high <- df %>% filter(metric == "FieldGoals") %>% summarise(max(data)) %>% pull()
    FGA_low <- df %>% filter(metric  == "FieldGoalAttempts") %>% summarise(min(data)) %>% pull()
    FGA_high <- df %>% filter(metric == "FieldGoalAttempts") %>% summarise(max(data)) %>% pull()
    FGPct_low <- df %>% filter(metric == "FieldGoalPct") %>% summarise(min(data)) %>% pull()
    FGPct_high <- df %>% filter(metric == "FieldGoalPct") %>% summarise(max(data)) %>% pull()
    FT_low <- df %>% filter(metric == "FreeThrows") %>% summarise(min(data)) %>% pull()
    FT_high <- df %>% filter(metric == "FreeThrows") %>% summarise(max(data)) %>% pull()
    FTA_low <- df %>% filter(metric == "FreeThrowAttempts") %>% summarise(min(data)) %>% pull()
    FTA_high <- df %>% filter(metric == "FreeThrowAttempts") %>% summarise(max(data)) %>% pull()
    FTPct_low <- df %>% filter(metric == "FreeThrowPct") %>% summarise(min(data)) %>% pull()
    FTPct_high <- df %>% filter(metric == "FreeThrowPct") %>% summarise(max(data)) %>% pull()
    OR_low <- df %>% filter(metric == "OffensiveRating") %>% summarise(min(data)) %>% pull()
    OR_high <- df %>% filter(metric == "OffensiveRating") %>% summarise(max(data)) %>% pull()
    ORB_low <- df %>% filter(metric == "OffensiveRebounds") %>% summarise(min(data)) %>% pull()
    ORB_high <- df %>% filter(metric == "OffensiveRebounds") %>% summarise(max(data)) %>% pull()
    PTS_low <- df %>% filter(metric == "PointsFor") %>% summarise(min(data)) %>% pull()
    PTS_high <- df %>% filter(metric == "PointsFor") %>% summarise(max(data)) %>% pull()
    ThreePT_low <- df %>% filter(metric == "ThreePointers") %>% summarise(min(data)) %>% pull()
    ThreePT_high <- df %>% filter(metric == "ThreePointers") %>% summarise(max(data)) %>% pull()
    ThreePTA_low <- df %>% filter(metric == "ThreePointAttempts") %>% summarise(min(data)) %>% pull()
    ThreePTA_high <- df %>% filter(metric == "ThreePointAttempts") %>% summarise(max(data)) %>% pull()
    ThreePointPct_low <- df %>% filter(metric == "ThreePointPct") %>% summarise(min(data)) %>% pull()
    ThreePointPct_high <- df %>% filter(metric == "ThreePointPct") %>% summarise(max(data)) %>% pull()
    Turnovers_low <- df %>% filter(metric == "Turnovers") %>% summarise(min(data)) %>% pull()
    Turnovers_high <- df %>% filter(metric == "Turnovers") %>% summarise(max(data)) %>% pull()
    }#Offensive Graph Limits
    {
      Blocks_low <- df %>% filter(metric == "Blocks") %>% summarise(min(data)) %>% pull()
      Blocks_high <- df %>% filter(metric == "Blocks") %>% summarise(max(data)) %>% pull()
      DefensiveRating_low <- df %>% filter(metric == "DefensiveRating") %>% summarise(min(data)) %>% pull()
      DefensiveRating_high <- df %>% filter(metric == "DefensiveRating") %>% summarise(max(data)) %>% pull()
      DefensiveRebounds_low <- df %>% filter(metric == "DefensiveRebounds") %>% summarise(min(data)) %>% pull()
      DefensiveRebounds_high <- df %>% filter(metric == "DefensiveRebounds") %>% summarise(max(data)) %>% pull()
      PointsAgainst_low <- df %>% filter(metric == "PointsAgainst") %>% summarise(min(data)) %>% pull()
      PointsAgainst_high <- df %>% filter(metric == "PointsAgainst") %>% summarise(max(data)) %>% pull()
      Steals_low <- df %>% filter(metric == "Steals") %>% summarise(min(data)) %>% pull()
      Steals_high <- df %>% filter(metric == "Steals") %>% summarise(max(data)) %>% pull()
      
    }#Defensive Graph Limits
    {
      NetRating_low <- df %>% filter(metric == "NetRating") %>% summarise(min(data)) %>% pull()
      NetRating_high <- df %>% filter(metric == "NetRating") %>% summarise(max(data)) %>% pull()
      PersonalFouls_low <- df %>% filter(metric == "PersonalFouls") %>% summarise(min(data)) %>% pull()
      PersonalFouls_high <- df %>% filter(metric == "PersonalFouls") %>% summarise(max(data)) %>% pull()
      TotalRebounds_low <- df %>% filter(metric == "TotalRebounds") %>% summarise(min(data)) %>% pull()
      TotalRebounds_high <- df %>% filter(metric == "TotalRebounds") %>% summarise(max(data)) %>% pull()
      
      
    }#General Metrics
    {
      StrengthOfSchedule_low <- df %>% filter(metric == "StrengthOfSchedule") %>% summarise(min(data)) %>% pull()
      StrengthOfSchedule_high <- df %>% filter(metric == "StrengthOfSchedule") %>% summarise(max(data)) %>% pull()
      PointsPlus_low <- df %>% filter(metric == "PointsPlus") %>% summarise(min(data)) %>% pull()
      PointsPlus_high <- df %>% filter(metric == "PointsPlus") %>% summarise(max(data)) %>% pull()
      Pace_low <- df %>% filter(metric == "Pace") %>% summarise(min(data)) %>% pull()
      Pace_high <- df %>% filter(metric == "Pace") %>% summarise(max(data)) %>% pull()
      
    }#Advanced Metrics
    
    }
    setProgress(value = 95, message = "Preparing Graphics")
    output$distPlot1 <- renderPlot({
      {
        if(input$outcome == "WinPct")
        {
          OutcomeOne <- df %>% filter(Outcome == "WinPct")
          outcomeTitle <- "Win %"
          y1 <- 0
          y2 <- 1
        }
        if(input$outcome == "AvgPointDiff")
        {
          OutcomeOne <- df %>% filter(Outcome == "AveragePointDiff")
          outcomeTitle <- "Average Point Differential"
          y1 <- -15
          y2 <- 20
        }
        }#Choosing outcome  
      {if(input$conf1 == "ACC")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "ACC")
            confTitle <- "ACC"
        }
        if(input$conf1 == "BigTen")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "Big Ten")
            confTitle <- "Big Ten"
            
        }
        if(input$conf1 == "Pac12")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "Pac 12")
            confTitle <- "Pac 12"
        }  
        if(input$conf1 == "Big12")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "Big 12")
            confTitle <- "Big 12"
        }
        if(input$conf1 == "SEC")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "SEC")
            confTitle <- "SEC"
        }
        if(input$conf1 == "A10")
        {
            ConfOne <- OutcomeOne %>% filter(Conference == "Atlantic 10")
            confTitle <- "Atlantic 10"
        }
        if(input$conf1 == "WCC")
        {
          ConfOne <- OutcomeOne %>% filter(Conference == "West Coast Conference")
          confTitle <- "West Coast Conference"
        }
        if(input$conf1 == "BigEast")
        {
          ConfOne <- OutcomeOne %>% filter(Conference == "Big East")
          confTitle <- "Big East"
        }
        if(input$conf1 == "MVC")
        {
          ConfOne <- OutcomeOne %>% filter(Conference == "Missouri Valley Conference")
          confTitle <- "Missouri Valley Conference"
        }
       } #selecting conference
      { 
           ##Offensive Metrics##
        {if(input$metric == "AST")
        {
            Final1 <- ConfOne %>% filter(metric == "Assists")
            metricTitle <- "Assists"
            x1 <- Assist_low
            x2 <- Assist_high
        }
        if(input$metric == "eFGPct")
        {
          Final1 <- ConfOne %>% filter(metric == "EffectiveFGPct")
            metricTitle <- "Effective Field Goal %"
            x1 <- eFGPct_low
            x2 <- eFGPct_high
        }  
           if(input$metric == "FG")
           {
             Final1 <- ConfOne %>% filter(metric == "FieldGoals")
               metricTitle <- "Field Goals"
               x1 <- FG_low
               x2 <- FG_high
           } 
           if(input$metric == "FGA")
           {
             Final1 <- ConfOne %>% filter(metric == "FieldGoalAttempts")
               metricTitle <- "Field Goal Attempts"
               x1 <- FGA_low
               x2 <- FGA_high
           } 
           if(input$metric == "FGPct")
           {
             Final1 <- ConfOne %>% filter(metric == "FieldGoalPct")
               metricTitle <- "Field Goal %"
               x1 <- FGPct_low
               x2 <- FGPct_high
           } 
        if(input$metric == "FT")
        {
          Final1 <- ConfOne %>% filter(metric == "FreeThrows")
            metricTitle <- "Free Throws"
            x1 <- FT_low
            x2 <- FT_high
        }
        if(input$metric == "FTA")
        {
          Final1 <- ConfOne %>% filter(metric == "FreeThrowAttempts")
            metricTitle <- "Free Throws Attempted"
            x1 <- FTA_low
            x2 <- FTA_high
        }
        if(input$metric == "FTPct")
        {
          Final1 <- ConfOne %>% filter(metric == "FreeThrowPct")
            metricTitle <- "Free Throw Percentage"
            x1 <- FTPct_low
            x2 <- FTPct_high
        }
        if(input$metric == "OR")
        {
          Final1 <- ConfOne %>% filter(metric == "OffensiveRating")
            metricTitle <- "Offensive Rating"
            x1 <- OR_low
            x2 <- OR_high
        }
        if(input$metric == "ORB")
        {
          Final1 <- ConfOne %>% filter(metric == "OffensiveRebounds")
            metricTitle <- "Offensive Rebounds"
            x1 <- ORB_low
            x2 <- ORB_high
        }
        if(input$metric == "PTS")
        {
          Final1 <- ConfOne %>% filter(metric == "PointsFor")
            metricTitle <- "Points For"
            x1 <- PTS_low
            x2 <- PTS_high
        }
        if(input$metric == "ThreePT")
        {
          Final1 <- ConfOne %>% filter(metric == "ThreePointers")
            metricTitle <- "Three Point Shots Made"
            x1 <- ThreePT_low
            x2 <- ThreePT_high
        }   
        if(input$metric == "ThreePTA")
        {
          Final1 <- ConfOne %>% filter(metric == "ThreePointAttempts")
            metricTitle <- "Three Point Shot Attempts"
            x1 <- ThreePTA_low
            x2 <- ThreePTA_high
        }
        if(input$metric == "ThreePTPct")
        {
          Final1 <- ConfOne %>% filter(metric == "ThreePointPct")
            metricTitle <- "Three Point Shot %"
            x1 <- ThreePointPct_low
            x2 <- ThreePointPct_high
        }
        if(input$metric == "TOV")
        {
          Final1 <- ConfOne %>% filter(metric == "Turnovers")
            metricTitle <- "Turnovers"
            
            x1 <- Turnovers_low
            x2 <- Turnovers_high
        }}#Offensive Metrics
           ##Defensive Metrics##
        {if(input$metric == "BLK")
        {
          Final1 <- ConfOne %>% filter(metric == "Blocks")
            metricTitle <- "Blocks"
            x1 <- Blocks_low
            x2 <- Blocks_high
        }
        if(input$metric == "DRT")
        {
          Final1 <- ConfOne %>% filter(metric == "DefensiveRating")
            metricTitle <- "Defensive Rating (negative metric)"
            x1 <- DefensiveRating_low
            x2 <- DefensiveRating_high
        }
        if(input$metric == "DRB")
        {
          Final1 <- ConfOne %>% filter(metric == "DefensiveRebounds")
            metricTitle <- "Defensive Rebounds"
            x1 <- DefensiveRebounds_low
            x2 <- DefensiveRebounds_high
        }
        if(input$metric == "PTSA")
        {
          Final1 <- ConfOne %>% filter(metric == "PointsAgainst")
            metricTitle <- "Points Against"
            x1 <- PointsAgainst_low
            x2 <- PointsAgainst_high
        }
        if(input$metric == "STL")
        {
          Final1 <- ConfOne %>% filter(metric == "Steals")
            metricTitle <- "Steals"
            x1 <- Steals_low
            x2 <- Steals_high
            
        }}#Defensive Metrics
        ##General Metrics##
        {if(input$metric == "NetRating")
           {
             Final1 <- ConfOne %>% filter(metric == "NetRating")
               metricTitle <- "Net Rating (Estimated Point Differential per 100 Possessions)"
               x1 <- NetRating_low
               x2 <- NetRating_high
           }
           if(input$metric == "PF")
           {
             Final1 <- ConfOne %>% filter(metric == "PersonalFouls")
               metricTitle <- "Personal Fouls"
               x1 <- PersonalFouls_low
               x2 <- PersonalFouls_high
           }
           if(input$metric == "TRB")
           {
             Final1 <- ConfOne %>% filter(metric == "TotalRebounds")
               metricTitle <- "Total Rebounds"
               x1 <- TotalRebounds_low
               x2 <- TotalRebounds_high
               
           }}#General Metrics
        ##Advanced Metrics##
        {if(input$metric == "SOS")
           {
             Final1 <- ConfOne %>% filter(metric == "StrengthOfSchedule")
               metricTitle <- "Strength of Schedule"
               x1 <- StrengthOfSchedule_low
               x2 <- StrengthOfSchedule_high
           }
           if(input$metric == "PointsPlus")
           {
             Final1 <- ConfOne %>% filter(metric == "PointsPlus")
               metricTitle <- "Marginal Points per Game"
               x1 <- PointsPlus_low
               x2 <- PointsPlus_high
           }
           if(input$metric == "Pace")
           {
             Final1 <- ConfOne %>% filter(metric == "Pace")
             metricTitle <- "Pace (Estimated  Possessions per 40 Minutes)"
             x1 <- Pace_low
             x2 <- Pace_high
             
           }}#advanced metric
    } #selecting metric
      {setorder(Final1, -data)
        r1 <- cor(Final1$NumericOutcome, Final1$data)}#Setting Order and calculating correlation
      output$text1 <- renderText({paste("Correlation Coefficient:\n",paste(round(r1,2)),sep = " ")})
      {g <- Final1 %>% ggplot(aes(x = data, y = NumericOutcome))+
            labs(colour = "Made\nNational\nTournement")+
            stat_smooth(method = "loess", se = FALSE)+
            geom_smooth(method='lm',color = "black", alpha = 0.8, size = 0.5, se = FALSE)+
            geom_label_repel(aes(label = School), size = 3.2)+
            geom_point(aes(color = MadeTourney))+
            scale_color_manual(values = c("red", "blue"))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
            ylab(paste(outcomeTitle))+
            xlab(paste(metricTitle))+
            ggtitle(paste(confTitle))+
            theme_bw()+
            xlim(x1,x2)+
            ylim(y1,y2)+
            theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14), legend.title = element_text(size = 14))
        }#Making Graph
      g
    })#,height = 600)
    
    output$distPlot2 <- renderPlot({      
      {
      if(input$outcome == "WinPct")
      {
        OutcomeTwo <- df %>% filter(Outcome == "WinPct")
        outcomeTitle <- "Win %"
        y1 <- 0
        y2 <- 1
      }
      if(input$outcome == "AvgPointDiff")
      {
        OutcomeTwo <- df %>% filter(Outcome == "AveragePointDiff")
        outcomeTitle <- "Average Point Differential"
        y1 <- -15
        y2 <- 20
      }
    }#Choosing outcome  
      {if(input$conf2 == "ACC")
      {
        ConfTwo <- OutcomeTwo %>% filter(Conference == "ACC")
        confTitle <- "ACC"
      }
        if(input$conf2 == "BigTen")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Big Ten")
          confTitle <- "Big Ten"
          
        }
        if(input$conf2 == "Pac12")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Pac 12")
          confTitle <- "Pac 12"
        }  
        if(input$conf2 == "Big12")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Big 12")
          confTitle <- "Big 12"
        }
        if(input$conf2 == "SEC")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "SEC")
          confTitle <- "SEC"
        }
        if(input$conf2 == "A10")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Atlantic 10")
          confTitle <- "Atlantic 10"
        }
        if(input$conf2 == "WCC")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "West Coast Conference")
          confTitle <- "West Coast Conference"
        }
        if(input$conf2 == "BigEast")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Big East")
          confTitle <- "Big East"
        }
        if(input$conf2 == "MVC")
        {
          ConfTwo <- OutcomeTwo %>% filter(Conference == "Missouri Valley Conference")
          confTitle <- "Missouri Valley Conference"
        }
      } #selecting conference
      { 
        ##Offensive Metrics##
        {if(input$metric == "AST")
        {
          Final2 <- ConfTwo %>% filter(metric == "Assists")
          metricTitle <- "Assists"
          x1 <- Assist_low
          x2 <- Assist_high
        }
          if(input$metric == "eFGPct")
          {
            Final2 <- ConfTwo %>% filter(metric == "EffectiveFGPct")
            metricTitle <- "Effective Field Goal %"
            x1 <- eFGPct_low
            x2 <- eFGPct_high
          }  
          if(input$metric == "FG")
          {
            Final2 <- ConfTwo %>% filter(metric == "FieldGoals")
            metricTitle <- "Field Goals"
            
            x1 <- FG_low
            x2 <- FG_high
          } 
          if(input$metric == "FGA")
          {
            Final2 <- ConfTwo %>% filter(metric == "FieldGoalAttempts")
            metricTitle <- "Field Goal Attempts"
            x1 <- FGA_low
            x2 <- FGA_high
          } 
          if(input$metric == "FGPct")
          {
            Final2 <- ConfTwo %>% filter(metric == "FieldGoalPct")
            metricTitle <- "Field Goal %"
            x1 <- FGPct_low
            x2 <- FGPct_high
          } 
          if(input$metric == "FT")
          {
            Final2 <- ConfTwo %>% filter(metric == "FreeThrows")
            metricTitle <- "Free Throws"
            x1 <- FT_low
            x2 <- FT_high
          }
          if(input$metric == "FTA")
          {
            Final2 <- ConfTwo %>% filter(metric == "FreeThrowAttempts")
            metricTitle <- "Free Throws Attempted"
            x1 <- FTA_low
            x2 <- FTA_high
          }
          if(input$metric == "FTPct")
          {
            Final2 <- ConfTwo %>% filter(metric == "FreeThrowPct")
            metricTitle <- "Free Throw Percentage"
            x1 <- FTPct_low
            x2 <- FTPct_high
          }
          if(input$metric == "OR")
          {
            Final2 <- ConfTwo %>% filter(metric == "OffensiveRating")
            metricTitle <- "Offensive Rating"
            x1 <- OR_low
            x2 <- OR_high
          }
          if(input$metric == "ORB")
          {
            Final2 <- ConfTwo %>% filter(metric == "OffensiveRebounds")
            metricTitle <- "Offensive Rebounds"
            x1 <- ORB_low
            x2 <- ORB_high
          }
          if(input$metric == "PTS")
          {
            Final2 <- ConfTwo %>% filter(metric == "PointsFor")
            metricTitle <- "Points For"
            x1 <- PTS_low
            x2 <- PTS_high
          }
          if(input$metric == "ThreePT")
          {
            Final2 <- ConfTwo %>% filter(metric == "ThreePointers")
            metricTitle <- "Three Point Shots Made"
            x1 <- ThreePT_low
            x2 <- ThreePT_high
          }   
          if(input$metric == "ThreePTA")
          {
            Final2 <- ConfTwo %>% filter(metric == "ThreePointAttempts")
            metricTitle <- "Three Point Shot Attempts"
            x1 <- ThreePTA_low
            x2 <- ThreePTA_high
          }
          if(input$metric == "ThreePTPct")
          {
            Final2 <- ConfTwo %>% filter(metric == "ThreePointPct")
            metricTitle <- "Three Point Shot %"
            x1 <- ThreePointPTPct_low
            x2 <- ThreePointPTPct_high
          }
          if(input$metric == "TOV")
          {
            Final2 <- ConfTwo %>% filter(metric == "Turnovers")
            metricTitle <- "Turnovers"
            x1 <- Turnovers_low
            x2 <- Turnovers_high
          }}#Offensive Metrics
        ##Defensive Metrics##
        {if(input$metric == "BLK")
        {
          Final2 <- ConfTwo %>% filter(metric == "Blocks")
          metricTitle <- "Blocks"
          x1 <- Blocks_low
          x2 <- Blocks_high
        }
          if(input$metric == "DRT")
          {
            Final2 <- ConfTwo %>% filter(metric == "DefensiveRating")
            metricTitle <- "Defensive Rating (negative metric)"
            x1 <- DefensiveRating_low
            x2 <- DefensiveRating_high
            
          }
          if(input$metric == "DRB")
          {
            Final2 <- ConfTwo %>% filter(metric == "DefensiveRebounds")
            metricTitle <- "Defensive Rebounds"
            x1 <- DefensiveRebounds_low
            x2 <- DefensiveRebounds_high
          }
          if(input$metric == "PTSA")
          {
            Final2 <- ConfTwo %>% filter(metric == "PointsAgainst")
            metricTitle <- "Points Against"
            x1 <- PointsAgainst_low
            x2 <- PointsAgainst_high
            
          }
          if(input$metric == "STL")
          {
            Final2 <- ConfTwo %>% filter(metric == "Steals")
            metricTitle <- "Steals"
            x1 <- Steals_low
            x2 <- Steals_high
            
          }}#Defensive Metrics
        ##General Metrics##
        {if(input$metric == "NetRating")
        {
          Final2 <- ConfTwo %>% filter(metric == "NetRating")
          metricTitle <- "Net Rating (Estimated Point Differential per 100 Possessions)"
          x1 <- NetRating_low
          x2 <- NetRating_high
        }
          if(input$metric == "PF")
          {
            Final2 <- ConfTwo %>% filter(metric == "PersonalFouls")
            metricTitle <- "Personal Fouls"
            x1 <- PersonalFouls_low
            x2 <- PersonalFouls_high
          }
          if(input$metric == "TRB")
          {
            Final2 <- ConfTwo %>% filter(metric == "TotalRebounds")
            metricTitle <- "Total Rebounds"
            x1 <- TotalRebounds_low
            x2 <- TotalRebounds_high
            
          }}#General Metrics
        ##Advanced Metrics##
        {if(input$metric == "SOS")
        {
          Final2 <- ConfTwo %>% filter(metric == "StrengthOfSchedule")
          metricTitle <- "Strength of Schedule"
          x1 <- StrengthOfSchedule_low
          x2 <- StrengthOfSchedule_high
          
        }
          if(input$metric == "PointsPlus")
          {
            Final2 <- ConfTwo %>% filter(metric == "PointsPlus")
            metricTitle <- "Marginal Points per Game"
            x1 <- PointsPlus_low
            x2 <- PointsPlus_high
          }
          if(input$metric == "Pace")
          {
            Final2 <- ConfTwo %>% filter(metric == "Pace")
            metricTitle <- "Pace (Estimated  Possessions per 40 Minutes)"
            x1 <- Pace_low
            x2 <- Pace_high
            
          }}#advanced metric
      } #selecting metric
      {setorder(Final2, -data)
        r2 <- cor(Final2$NumericOutcome, Final2$data)}#Setting Order and calculating correlation
      output$text2 <- renderText({paste("Correlation Coefficient:\n",paste(round(r2,2)),sep = " ")})
      #setProgress(value = 80, message = "Rendering Graphics")
      {g2 <- Final2 %>% ggplot(aes(x = data, y = NumericOutcome))+
          labs(colour = "Made\nNational\nTournement")+
          stat_smooth(method = "loess", se = FALSE)+
          geom_smooth(method='lm',color = "black", alpha = 0.8, size = 0.5, se = FALSE)+
          geom_label_repel(aes(label = School), size = 3.2)+
          geom_point(aes(color = MadeTourney))+
          scale_color_manual(values = c("red", "blue"))+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))+
          ylab(paste(outcomeTitle))+
          xlab(paste(metricTitle))+
          ggtitle(paste(confTitle))+
          theme_bw()+
          xlim(x1,x2)+
          ylim(y1,y2)+
          theme(plot.title = element_text(face = "bold"), axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), legend.text = element_text(size = 14), legend.title = element_text(size = 14))
      }#Making Graph
      g2})
    setProgress(value = 100)
    })#withProgressBar
}#server layer
# Run the application 
shinyApp(ui = ui, server = server)
