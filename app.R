## Main.R ##
library(shinydashboard)
source("Functions.R")

### UI ###
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Football Analysis"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Player", tabName = "player", icon = icon("user")),
      menuItem("Comparison", tabName = "comparison", icon = icon("people-arrows")),
      menuItem("Team", tabName = "team", icon = icon("users")),
      menuItem("Stats", icon = icon("hdd"), 
               href = "https://fbref.com")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Home"),
              fluidRow(
                box(width = 12,
                    h3("Welcome to your Football Analysis."),
                    h4("Here are some sample graphs:")),
                box(plotOutput("sample_graph_1", height = "500px", width = "500px")),
                box(plotOutput("sample_graph_2", height = "500px", width = "500px")),
                box(plotOutput("sample_graph_3", height = "500px", width = "500px"))
              )
      ),
      # First tab content
      tabItem(tabName = "player",
              h2("Player Stats"),
              fluidRow(
                box(title="Search", solidHeader = TRUE, color = "purple",
                       selectInput("search","Search", choices = player_names),
                       actionButton("go", "Go!")
                )
              ),
              fluidRow(
                box(title="Player Stats", solidHeader = TRUE, collapsible = TRUE, width = "8", color = "teal",
                     plotOutput("plot1", height = "750px", width = "750px")
                    ),
              )
      ),
      
      # Second tab content
      tabItem(tabName = "comparison",
              h2("Comparison"),
              fluidRow(
                box(title="Search", solidHeader = TRUE, color = "purple",
                    selectInput("search2_1","Search", choices = player_names),
                    selectInput("search2_2", "Search", choices = player_names),
                    actionButton("go2", "Go!")
                )
              ),
              fluidRow(
                box(title="Player Comparison", solidHeader = TRUE, collapsible = TRUE, width = "8", color = "teal",
                    plotOutput("plot2", height = "750px", width = "750px")
                )
              )
              ),
      
      # Third tab content
      tabItem(tabName = "team",
              h2("Team Stats"),
              fluidRow(
                box(title="Search", solidHeader = TRUE, color = "purple",
                    selectInput("search3","Search", choices = team_names),
                    selectInput("select", "Select an option",
                            c(" "=" ", "Gls+Ast"="Gls, Ast", "Poss"="Poss", "G vs. xG"="Gls, xG"),
                            selected=" "),
                    actionButton("go3", "Go!")
                )
              ),
              fluidRow(
                box(title="Team Stats", solidHeader = TRUE, collapsible = TRUE, width = "8", color = "teal",
                    plotOutput("plot3", height = "750px", width = "750px")
                )
              )
      )
    )
  )
)

### SERVER ###
server <- function(input, output) {
  observeEvent(input$go, {
    search_term <- input$search
    # Use the search_term to create a graph
    # The graph will be displayed in the "plot" output
    output$plot1 <- renderPlot({
      create_graph_player(prem_players_stats, search_term)
    })
  })
  observeEvent(input$go2, {
    search_term_1 <- input$search2_1
    search_term_2 <- input$search2_2
    output$plot2 <- renderPlot({
      create_graph_players(prem_players_stats, search_term_1, search_term_2)
    })
  })
  observeEvent(input$go3, {
    search_team <- input$search3
    search_term <- input$select
    output$plot3 <- renderPlot({
      create_graph_team(prem_team_stats, search_team, search_term)
    })
  })
  output$sample_graph_1 <- renderPlot({
    create_graph_player(prem_players_stats, random_name_1)
  })
  output$sample_graph_2 <- renderPlot({
    create_graph_players(prem_players_stats, random_name_2, random_name_3)
  })
  output$sample_graph_3 <- renderPlot({
    create_graph_team(prem_team_stats,random_team, "Gls, xG")
  })
}

### RUN ###
shinyApp(ui, server)
