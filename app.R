# Kyle Garrison Beard
# NC State | ST 558 | Final Project 
# This code is free and open source

library(shiny)
library(shinydashboard)
library(tidyverse)
library(knitr)
library(plotly)
library(kableExtra)
library(ggiraph)
library(ggiraphExtra)
library(rgl)
library(caret)
library(tree)
library(randomForest)
library(gbm)

# ============================================================
# Deliverables
# ============================================================

# Data source is given here:
# https://www.kaggle.com/ronaldjgrafjr/nfl-draft-outcomes

# --- General ---
# [X] Multiple tabs
# [X] At least 2 different dynamic UI elements
# [X] Button to save whichever plot is being views
# [X] Button to save data current being viewed by plot or table
# [X] Ability to click on a plot or select a region
# [X] Include some type of math type (mathJax)
# [X] Include link and other formatted text forms

# --- Modeling (not clusting) ---
# [X] Two supervised models
# [X] User functionality for choosing model settings (variables, # of trees, etc) for editing output
# [X] Give the user a way to use model for prediction (select predictor values)

# ============================================================
# GLOBAL VARIABLES
# ============================================================
nflData <- read.csv("nfl_draft.csv") %>% group_by(Team) 
nflData$Player_Id <- NULL
nflData <- nflData %>% filter(Round < 8)

# ============================================================
# UI SIDE
# ============================================================
ui <- dashboardPage(
    
    # Set the header for the dashboard
    dashboardHeader(title = "NFL Draft Picks Exploration"),
    
    # Establish names, pointers, and icons for the tab items
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Scroll & Filter", tabName = "view", icon = icon("filter")),
            menuItem("Explore Deeper", tabName = "exploration", icon = icon("chart-bar")),
            menuItem("Modeling", tabName = "model", icon = icon("brain")),
            menuItem("Clustering", tabName = "cluster", icon = icon("braille"))
        )
    ),
    
    # Body content
    dashboardBody(
        
        tabItems(
            
            # (1) Home Page | Describe the data and abilities of app
            tabItem(tabName = "home",
                fluidRow(
                    box(
                        h2("Welcome!"),
                        h4("This dataset displays the NFL Draft results from 1985 to 2015 for players picked, their position, team, and average stats in the pros. Please select one of the following options in the menu to explore the data further!"),
                        uiOutput("tab"),
                        h3("1. Scroll & Filter"),
                        h5("In this section, you can filter by Team or Position to see all of the NFL Draft data!"),
                        h3("2. Explore Deeper"),
                        h5("In this section, you can dive deeper into the analytics of how the NFL Draft rounds have an influence on performance through the league, games played, and positional powerhouses."),
                        h3("3. Modeling"),
                        h5("In this section, you can perform two different models to predict how the data will correspond to different performance and the probability of being a first rounder."),
                        h3("4. Clustering"),
                        h5("In this section, we'll let you explore different variables and any clustering which could happen between them! You can pick the variables, the clustering parameters, and the number of iterations.")
                    )
                ),
                fluidRow(
                    box(
                        h6("This RShiny application was made with love by Kyle Beard intended for the educations use of ST 558 at NC State for a Summer 2019 final project. All application code within this app is free and open source.")
                    )
                )
            ),
            
            # (2) Data Viewing | Allows the user to scroll through the data (or subset data of interest)
            tabItem(tabName = "view",
                fluidRow(
                    box(h3("Filter by team and position"),
                        selectizeInput("team1", "Choose team:", selected = "PIT", choices = levels(as.factor(nflData$Team))),
                        selectizeInput("position1", "Choose position:", selected = "QB", choices = levels(as.factor(nflData$Position_Standard))),
                        downloadButton("downloadTable", "Download CSV")
                    ),
                    br(),
                    DT::dataTableOutput("table1", width = 1200)
                )
            ),
            
            # (3) Data Exploration | Common numerical and graphical summaries can be created by user
            tabItem(tabName = "exploration",
                fluidRow(
                    box(h3("Playing Favorites"),
                        h6("First Round talent is usually the frontrunner for playing the most games right? Let's take a look at each pro team and their breakdown of games played per round draft pick!"),
                        selectizeInput("team2", "Choose team:", selected = "PIT", choices = levels(as.factor(nflData$Team))),
                        plotOutput("plot1")
                    ),
                    
                    box(h3("Powerhouse Position Colleges"),
                        h6("The Accumulated Value (AV) score is a scalar metric that ranks the overall value a player brought their team each season. Let's filter by college to see which positions were notoriously better in the pros!"),
                        uiOutput("equation"),
                        checkboxInput("compareSchools", "Compare two schools?"),
                        selectizeInput("school1", "Choose a college:", selected = "Clemson", choices = levels(as.factor(nflData$College))),
                        conditionalPanel("input.compareSchools", 
                                         selectizeInput("school2", "Choose a college:", selected = "Alabama", choices = levels(as.factor(nflData$College)))),
                        plotOutput("plot2"),
                        downloadButton("downloadPlot")
                    ),
                    
                    box(h3("QB Touchdown Battle!"),
                        h6("Quarterbacks are biggest scorers on the field! Let's take a look at how their Passing Yards vs TDs look with this interactive graph! Hover over the points to see the career stats and try to guess which QB was responsible for the stat! Then, go over to Scroll & Filter to double-check your guess!"),
                        plotlyOutput("plot3")
                    ),
                    
                    box(h3("Popularity Contest"),
                        h6("Let's take a look at what the most popular First Round positions are by team chosen in the upper left box:"),
                        uiOutput("popularityTitle"),
                        tableOutput("contingency")
                    )
                )
            ),
            
            # (4) Clustering or PCA | Include dendogram for cluster or biplot for PCA - user should specify algorithm aspects
            tabItem(tabName = "cluster",
                fluidRow(
                    box(h3("Clustering"),
                        h6("Let's try to find some clusters! Choose your x-axis and y-axis clustering values! Please enter the cluster and iteration amounts:"),
                        selectInput('xcol', 'X Variable', names(nflData), selected = "Pick"),
                        selectInput('ycol', 'Y Variable', names(nflData), selected = "First4AV"),
                        numericInput('clusters', 'Clusters:', value = 5, min = 1, max = 10),
                        numericInput('iterations', 'Algorithm Iterations:', value = 2, min = 1, max = 10),
                        plotOutput("cluster")
                    ),
                    box(h3("Dendogram"),
                        h6("Please be patient while it loads..."),
                        plotOutput("dendogram")
                    )
                )
            ),
            
            # (5) Modeling | See deliverables above
            tabItem(tabName = "model",
                fluidRow(
                    box(h3("Simple Linear Regression (SLR)"),
                        h6("Can we predict which the response (Accumulated Value (AV), Draft Round, or Age) a player would be drafted in? Let's pick a position and see which plot will be the best! Note: The variables used in the predictions are yardage per pass/rush/reception, attempts, and TDs."),
                        selectizeInput("lmPosition", "Choose position:", selected = "QB", choices = as.factor(c("QB", "WR", "RB"))),
                        selectizeInput("lmOutput", "Choose response:", selected = "CarAV", choices = as.factor(c("CarAV", "Round", "Age"))),
                        plotOutput("slr")
                    ),
                    box(h3("Generalized Linear Model (GLM)"),
                        h6("Let's use an glm to predict the probability of first rounders! Top talents has to have a trend, right? Select one of the parameters for the binomial model."),
                        selectizeInput("glmPredictor", "Choose predictor:", selected = "CarAV", choices = as.factor(c("CarAV", "First4AV", "Games Played"))),
                        plotOutput("glm")
                    )
                )
            )
        )
    )
)

# ============================================================
# SERVER SIDE
# ============================================================
server <- function(input, output) {
    
    # --- (1) Home ---
    
    # Display URl to view more information
    url <- a("here.", href="https://www.nfl.com/draft/home")
    output$tab <- renderUI({
        tagList("To learn more about the NFL Draft ", url)
    })
    
    # --- (2) Scroll & Filter ---
    
    # Data table to display team vs position
    output$table1 <- DT::renderDataTable(
        
        nflData %>% filter(Team == input$team1, Position_Standard == input$position1),
        options = list(scrollX = TRUE)
    )
    
    # Action button to download table1 to csv file
    output$downloadTable <- downloadHandler(
        filename = function() {
            paste('table', ".csv", sep = "")
        },
        content = function(file) {
            write.csv(saveDataTable(), file, row.names = FALSE)
        }
    )
    
    # --- (3) Explore Deeper ---
    
    # Generate a plot from user selected team for Round drafted vs. Games played
    output$plot1 <- renderPlot({
        
        data <- nflData %>% filter(Team == input$team2) %>% group_by(Position_Standard) %>% group_by(Round)
        
        g <- ggplot(data, aes(x = Round, y = G))
        g + geom_bar(stat = "identity") + scale_x_discrete(limits = c(1, 7)) +
            xlab("Round Drafted") + ylab("Total Games Played") + title("Games Played vs. Draft Round")
    })
    
    # Reactive variable for dynamic UI plot for choosing college vs. Approx Value
    collegePositionAV <- reactive({
        
        multi <- input$compareSchools
        
        if (multi == TRUE) {
            schools <- c(input$school1, input$school2)
        } else {
            schools <- c(input$school1)
        }
        
        data <- nflData %>% filter(College == schools)
        
        # create plot
        g <- ggplot(data, aes(x = Position_Standard, y = CarAV))
        g + geom_bar(stat = "identity", aes(fill = College), position = "dodge") + 
            xlab("Position") + ylab("Accumulated Value") + title("Career Approximate Value Metrics per Position")
    })
    
    # Plot reactive college above
    output$plot2 <- renderPlot({
        
        collegePositionAV()
    })
    
    # Action button to save data table
    saveDataTable <- reactive({
        nflData %>% filter(Team == input$team1, Position_Standard == input$position1)
    })
    
    # Contignency table for viewing popularity of positions drafted in each round
    output$contingency <- renderTable({
        
        data <- nflData %>% group_by(Position) %>% filter(Round < 8, Team == input$team2)
        tab <- sort(table(data$Position_Standard, dnn = "Position"), decreasing = TRUE)
    })
    
    # Dynamic UI element for changing the title for contingency table based upon selection
    output$popularityTitle <- renderUI({
        
        paste0("Frequency of ", input$team2, " First Round Picks")
    })
    
    # Interactive plot for QB passing yards vs TDs
    output$plot3 <- renderPlotly({
        
        data <- nflData %>% filter(Position == "QB")
        g <- ggplot(data, aes(x = Pass_Yds, y = Pass_TD)) + geom_point(aes(colour = Team)) + 
            xlab("Passing Yards") + ylab("Touchdowns")
    })
    
    # Action button to download reactive plot
    output$downloadPlot <- downloadHandler(
        filename = function() { paste('collegeValuePerPosition', '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = collegePositionAV(), device = "png")
        }
    )
    
    # Display equation for AV was calculated for dynamic college plot
    output$equation <- renderUI({
        withMathJax(
            helpText('$$\\sum_{y=rookie}^{retirement} AV_y$$')
        )
    })
    
    # --- (4) Modeling ---
    
    # SLR to predict Career AV by different position parameters
    output$slr <- renderPlot({
        
        set.seed(1)
        train <- sample(1:nrow(nflData), size = nrow(nflData)*0.8)
        test <- dplyr::setdiff(1:nrow(nflData), train)
        nflTrain <- nflData[train, ]
        nflTest <- nflData[test, ]
        
        if (input$lmPosition == "QB") {
            
            if (input$lmOutput == "CarAV") {
                slrFit <- lm(CarAV ~ Pass_Yds * Pass_TD * Pass_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            if (input$lmOutput == "Round") {
                slrFit <- lm(Round ~ Pass_Yds * Pass_TD * Pass_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            if (input$lmOutput == "Age") {
                slrFit <- lm(Age ~ Pass_Yds * Pass_TD * Pass_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
        }
        
        if (input$lmPosition == "WR") {
            
            if (input$lmOutput == "CarAV") {
                slrFit <- lm(CarAV ~ Rec_Yds * Rec_Tds * Rec, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            if (input$lmOutput == "Round") {
                slrFit <- lm(Round ~ Rec_Yds * Rec_Tds * Rec, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            if (input$lmOutput == "Age") {
                slrFit <- lm(Age ~ Rec_Yds * Rec_Tds * Rec, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
        }
        
        if (input$lmPosition == "RB") {
            
            if (input$lmOutput == "CarAV") {
                slrFit <- lm(CarAV ~ Rush_Yds * Rush_TDs * Rush_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            
            if (input$lmOutput == "Round") {
                slrFit <- lm(Round ~ Rush_Yds * Rush_TDs * Rush_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
            
            if (input$lmOutput == "Age") {
                slrFit <- lm(Age ~ Rush_Yds * Rush_TDs * Rush_Att, data = nflTrain)
                slrPred <- predict(slrFit, newdata = nflTest, se.fit = TRUE)
                plot(slrFit)
            }
        }
    })
    
    # GLM for predicting probability that the player was a 1st Round draft pick based on AV on first 4 seasons
    output$glm <- renderPlot({
        
        set.seed(1)
        train <- sample(1:nrow(nflData), size = nrow(nflData) * 0.8)
        test <- dplyr::setdiff(1:nrow(nflData), train)
        nflTrain <- nflData[train, ]
        nflTest <- nflData[test, ]
        
        if (input$glmPredictor == "CarAV") {
            glmFit <- glm(FirstRounder ~ CarAV, data = nflTrain, family = "binomial")
            ggPredict(glmFit, se = TRUE, interactive = FALSE)
        }

        else if (input$glmPredictor == "First4AV") {
            glmFit <- glm(FirstRounder ~ First4AV, data = nflTrain, family = "binomial")
            ggPredict(glmFit, se = TRUE, interactive = FALSE)
        }
        
        else {
            glmFit <- glm(FirstRounder ~ G, data = nflTrain, family = "binomial")
            ggPredict(glmFit, se = TRUE, interactive = FALSE)
        }
    })
    
    # --- (5) Clustering ---
    
    # Create reactive cluster data from user input
    clusterData <- reactive({
        
        nflData[, c(input$xcol, input$ycol)]
    })
    
    # Create kmeans cluster reactive
    clusters <- reactive({
        set.seed(15)
        kmeans(clusterData(), input$clusters, iter.max = input$iterations, algorithm = "MacQueen")
    })
    
    # Plot cluster for visual purposes from user input selection
    output$cluster <- renderPlot({
        
        # Palette taken from Clustering notes
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(clusterData(),
             col = clusters()$cluster,
             pch = 20, 
             cex = 3)
        
        points(clusters()$centers, 
               pch = 4, 
               cex = 4, 
               lwd = 4)
    })
    
    # Display additional dendogram from cluster selection
    output$dendogram <- renderPlot({
        
        dd <- dist(data.frame(nflData[, c(input$xcol, input$ycol)]), method = "euclidean")
        hc <- hclust(dd, method = "ward.D2")
        plot(hc, xlab = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
