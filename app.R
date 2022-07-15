library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggrepel) 
# library(bslib)


pokemon <- read.csv("Pokemon.csv")
pokemon <- pokemon[, -1]
pokemon[pokemon == ""] <- NaN


# Define UI for application
  ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(
      # titleWidth='16%',
      title = span(tagList(img(src="pokeball.png", height = "35", width = "35"), "  MyPokemon")),
      tags$li(class = "dropdown", tags$a(href = "https://pokemondb.net/", icon("info-circle"), "PokemonDB", target="_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://github.com/", icon("github"), "Github", target="_blank"))
      ),
    dashboardSidebar(
      # width = "16%",
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("gamepad")),
        menuItem("Pokemon-Stats Visual",
                 icon = icon("bar-chart"),
                 menuSubItem('Ability',
                             tabName = 'visual1',
                             icon = icon('line-chart')),
                 menuSubItem('Generation & Type',
                             tabName = 'visual2',
                             icon = icon('line-chart'))),
        menuItem("Pokemon Matching", tabName = "match", icon = icon("search")),
        menuItem("Pokemon Comparison", tabName = "compare", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("table"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "purple_gradient"),
      
      tabItems(
        # Home tab content
        tabItem(tabName = "home",
                img(src = "home5.jpg", width = "100%", height = 180),
                br(p(" "),
                   p(" ")),
                fluidRow(
                  box(
                    title = strong("Introduction", style = 'font-size:22px;'), solidHeader = TRUE, 
                    status = "primary", collapsible = TRUE, width = 12, 
                    br(p(" ")),
                    div(
                      tags$iframe(src="https://www.youtube.com/embed/UTCyA5n3LGI",
                                  width="80%",
                                  height="400",
                                  frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
                      style="text-align:center",
                      p(a("Video Link", href = "https://www.youtube.com/watch?v=UTCyA5n3LGI"))),
                      column(1,),
                      column(10, br(p(h4("Pokemon, electronic game series from Nintendo that debuted in Japan in February 1996 as Pokemon Green and Pokemon Red. The franchise later became wildly popular in the United States and around the world.", style = 'font-size:20px;color:white;')),
                                    br(),
                                    p(h4("The series,originally produced for the company's Game Boy line of handheld consoles, was introduced in 1998 to the United States with two titles, known to fans as Red and Blue. In the games, players assume the role of Pokemon trainers, obtaining cartoon monsters and developing them to battle other Pokemon. Pokemon became one of the most successful video game franchises in the world.", style = 'font-size:20px;color:white;'))
                        )),
                      column(1,)
                      
                    )
                      ),
                fluidRow(
                  box(
                    title = strong("Team", style = 'font-size:22px;'), solidHeader = TRUE, 
                    status = "primary", width = 12, collapsible = TRUE,
                    column(12, 
                           tags$div(
                             fluidRow(
                               column(2, tags$img(src="jyleng.jpg", width = 110, height = 145, align ="center")),
                               column(10, tags$div(strong("Jingyi Leng:", style = 'font-size:20px;color:white;')),
                               br(),
                               tags$li("MS in Information Systems, 2021", style = 'font-size:20px;color:white;'),
                               tags$li("Carey Business School, Johns Hopkins University", style = 'font-size:20px;color:white;'),
                               tags$li("Aspire to be a great Pokemon trainer :D", style = 'font-size:20px;color:white;')
                               )
                             )
                           ),
                    )
                  ),
                )
        ),

        # Match tab content
        tabItem(tabName = "match",
                img(src = "home5.jpg", width = "100%", height = 180),
                fluidRow(
                  column(3,
                         br(),
                         box(
                           title = "Select Type:", 
                           solidHeader = TRUE,
                           status = "info",
                         width = 12,
                         selectInput("select1", 
                                     label = h4("Type 1:"), 
                                      choices = c("",unique(as.character(pokemon$Type.1))), 
                                      selected = NULL),
                         selectInput("select2", 
                                     label = h4("Type 2:"), 
                                     choices = c("",unique(as.character(pokemon$Type.2))), 
                                     selected = NULL),
                         ),
                         box(
                           title = "Select Generation:", 
                             solidHeader = TRUE,
                             status = "info",
                             width = 12,
                             div(
                             selectInput("select3", 
                                         label = h4("Generation:"), 
                                         choices = c("",unique(as.character(pokemon$Generation))),
                                         selected = NULL),
                         ),
                         ),
                         box(
                           title = "Is Legendary?", 
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           checkboxGroupInput("check1",
                                              label = NULL,
                                              choices = unique(as.character(pokemon$Legendary))),
                         ),
                      ),
                  column(6,
                         br(),
                         box(
                           width = 24,
                           fluidRow(
                             column(4, align = "center",
                                    textOutput("pname1"),
                                    imageOutput("pimg1"),
                                    ),
                             column(4, align = "center",
                                    textOutput("pname2"),
                                    imageOutput("pimg2"),
                                    ),
                             column(4, align = "center",
                                    textOutput("pname3"),
                                    imageOutput("pimg3"),
                                    ),
                           )
                           
                         ),
                         ),
                  column(3,
                         br(),
                         box(
                           title = "Select Range:", 
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           sliderInput("slider1", label = h4("Total:"), min = 100, 
                                       max = 800, value = c(400, 500)),
                           br(),
                           hr(),
                           sliderInput("slider2", label = h4("HP:"), min = 0, 
                                       max = 300, value = c(130, 170)),
                           br(),
                           hr(),
                           sliderInput("slider3", label = h4("Speed:"), min = 0, 
                                       max = 200, value = c(80, 120)),
                           br(),
                           br(),
                         ),
                      ),
                  ),
                # checkboxInput("holiday", label = "Show holidays", value = FALSE),
                # plotlyOutput("plot1", height = 500)
        ),
        
        # Compare tab content
        tabItem(tabName = "compare",
                img(src = "home5.jpg", width = "100%", height = 180),
                strong(h2("Cannot decide take which Pokemon to battle?")),
                br(),
                fluidRow(
                  column(1,),
                  column(5, align="center",
                         selectizeInput(
                           inputId = "search1", 
                           label = "Enter Pokemon 1:",
                           multiple = FALSE,
                           choices = c("Search Bar" = "", unique(as.character(pokemon$Name))),
                           options = list(
                             create = FALSE,
                             placeholder = "Search Me",
                             maxItems = '1',
                             onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                             onType = I("function (str) {if (str === \"\") {this.close();}}")
                           )
                         ),
                         box(
                         width = 12,
                         height = 280,
                         span(h3(strong(textOutput("search1name"))), style="text-align:center;"),
                         fluidRow(
                                column(1,),
                                column(4, align="right",
                                  span(textOutput("s1type1"), style="font-size:15px;"),
                                  span(textOutput("s1type2"), style="font-size:15px;"),
                                  span(textOutput("s1against"),style="font-size:15px;"),
                                  span(textOutput("s1weak"),style="font-size:15px;"),
                                  # span("Type 1:",style="font-size:15px;"),
                                  # br(),
                                  # span("Type 2:",style="font-size:15px;"),
                                  # br(),
                                  # span("Against_to:",style="font-size:15px;"),
                                  # br(),
                                  # span("Weak_to:",style="font-size:15px;"),
                                ),
                                column(7, align="left",
                                    span(textOutput("search1type1"), style="font-size:15px;font-weight:bold;"),
                                    span(textOutput("search1type2"), style="font-size:15px;font-weight:bold;"),
                                    span(textOutput("search1against"),style="font-size:15px;font-weight:bold;"),
                                    span(textOutput("search1weak"),style="font-size:15px;font-weight:bold;"),
                                ),
                                ),
                         span(imageOutput("search1img"),style="display: block; text-align:center;"),
                         )),
                  # column(1,),
                  column(5, align="center",
                         selectizeInput(
                           inputId = "search2", 
                           label = "Enter Pokemon 2:",
                           multiple = FALSE,
                           choices = c("Search Bar" = "", unique(as.character(pokemon$Name))),
                           options = list(
                             create = FALSE,
                             placeholder = "Search Me",
                             maxItems = '1',
                             onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                             onType = I("function (str) {if (str === \"\") {this.close();}}")
                           )
                         ),
                         box(
                         width = 12,
                         height = 280,
                         span(h3(strong(textOutput("search2name"))),style="text-align:center;"),
                         fluidRow(
                                column(1,),
                                column(4, align="right",
                                    span(textOutput("s2type1"), style="font-size:15px;"),
                                    span(textOutput("s2type2"), style="font-size:15px;"),
                                    span(textOutput("s2against"),style="font-size:15px;"),
                                    span(textOutput("s2weak"),style="font-size:15px;"),
                                    # span("Type 1:",style="font-size:15px;"),
                                    # br(),
                                    # span("Type 2:",style="font-size:15px;"),
                                    # br(),
                                    # span("Against_to:",style="font-size:15px;"),
                                    # br(),
                                    # span("Weak_to:",style="font-size:15px;"),
                                     ),
                                 column(7, align="left",
                                          span(textOutput("search2type1"), style="font-size:15px;font-weight:bold;"),
                                          span(textOutput("search2type2"), style="font-size:15px;font-weight:bold;"),
                                          span(textOutput("search2against"),style="font-size:15px;font-weight:bold;"),
                                          span(textOutput("search2weak"),style="font-size:15px;font-weight:bold;"),
                                        ),),
                         span(imageOutput("search2img"),style=" text-align:center;"),
                         ),
                         ),
                  column(1,),
                ),
                fluidRow(
                  column(2,),
                  column(8,
                         br(),
                         tabBox(width = 12,
                                id="tab3",
                                tabPanel("6-Dimension Comparison", plotlyOutput("plot7")),
                                tabPanel("Total Comparison", plotOutput("plot8")),
                                ),
                         
                  ),
                  column(2,),
                  
                ),
        ),
        
        # Data tab content
        tabItem(tabName = "data",
                img(src = "home5.jpg", width = "100%", height = 180),
                br(),
                br(),
                br(),
                fluidRow(
                  column(2,
                sidebarPanel(
                      width = 12,
                      tags$style(".well {background-color:black;opacity:0.5}"),
                      checkboxGroupInput("show_vars", h4(strong("Select columns:")),
                                         names(pokemon), selected = names(pokemon)[c(1:5,10:12)])
                    )),
                  column(10,
                mainPanel(
                  fluidRow(
                    column(3,
                           selectInput("type1",
                                       "Type 1:",
                                       c("All",
                                         unique(as.character(pokemon$Type.1))))
                    ),
                    column(3,
                           selectInput("type2",
                                       "Type 2:",
                                       c("All",
                                         unique(as.character(pokemon$Type.2))))
                    ),
                    column(2,
                           selectInput("gen",
                                       "Generation:",
                                       c("All",
                                         unique(as.character(pokemon$Generation))))
                    ),
                    column(3,
                           selectInput("leg",
                                       "Legendary:",
                                       c("All",
                                         unique(as.character(pokemon$Legendary))))
                    
                  ),
                     ),
                  dataTableOutput("pokemondata")
                )),
                    )
        ),
        
        # Visual1 tab content
        tabItem(tabName = "visual1",
                fluidRow(
                  column(1,),
                  column(10,
                  br(),
                  tabBox(width = 12,
                              id="tab1",
                              tabPanel("Plot1", plotlyOutput("plot1")),
                              tabPanel("Plot2", plotlyOutput("plot2")),
                              tabPanel("Plot3", plotlyOutput("plot3"))),
                          
                        ),
                  column(1,),
                  )
        ),
        
        # Visual2 tab content
        tabItem(tabName = "visual2",
                fluidRow(
                  column(1,),
                  column(10,
                  br(),
                  tabBox(width = 12,
                        id="tab2",
                         tabPanel("Plot4", plotlyOutput("plot4")),
                         tabPanel("Plot5", plotlyOutput("plot5")),
                         tabPanel("Plot6", plotlyOutput("plot6"))),
                  ),
                  column(1,),
                )
        )
      )
    )
  )
  
  
# Define Server for application
  server <- function(input, output, session) {
    
    # match
    output$pname1 <- renderText({
      if(!input$select1 %in% pokemon$Type.1 || !input$select2 %in% pokemon$Type.2 || !input$select3 %in% pokemon$Generation){" "}
      else{
         pokemon <- pokemon %>%
           filter(Type.1 == input$select1) %>% 
           filter(Type.2 == input$select2) %>% 
           filter(Generation == input$select3)
         pokemon$Name[2]}
    })
    
    output$pimg1 <- renderImage({
      pokemon <- pokemon %>%
        filter(Type.1 == input$select1) %>% 
        filter(Type.2 == input$select2) %>% 
        filter(Generation == input$select3)
      filename <- normalizePath(file.path('www/images', paste(pokemon$Name[2], '.png', sep='')))
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)
    
    # compare
    output$s1type1 <- renderText({
      ""
      if(input$search1 %in% pokemon$Name){
        "Type 1:"
      }
    })
    
    output$s1type2 <- renderText({
      ""
      if(input$search1 %in% pokemon$Name){
        "Type 2:"
      }
    })
    
    output$s1against <- renderText({
      ""
      if(input$search1 %in% pokemon$Name){
        "Against_to:"
      }
    })
    
    output$s1weak <- renderText({
      ""
      if(input$search1 %in% pokemon$Name){
        "Weak_to:"
      }
    })
    
    output$search1name <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search1,]
      toupper(pokemon$Name)
    })
    
    output$search1img <- renderImage({
      filename <- normalizePath(file.path('www/images', paste(input$search1, '.png', sep='')))
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)
    # img(src=dat$image, width="600", height="315"), style="text-align:center")
    
    output$search1type1 <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search1,]
      pokemon$Type.1
      # paste("Type 1:", pokemon$Type.1)
    })
    
    output$search1type2 <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search1,]
      pokemon$Type.2
      # paste("Type 2:", pokemon$Type.2)
    })
    
    output$search1against <- renderText({
      
      pokemon <- pokemon %>% mutate(Against_to = case_when(pokemon$Type.1 == "Normal"~ "None",
                                                           pokemon$Type.1 == "Grass"~ "Ground, Rock, Water",
                                                           pokemon$Type.1 == "Fire"~"Bug, Steel, Grass, Ice",
                                                           pokemon$Type.1 == "Water"~"Ground, Rock, Fire",
                                                           pokemon$Type.1 == "Bug"~"Grass, Phychic, Dark",
                                                           pokemon$Type.1 == "Poison"~"Grass, Fairy",
                                                           pokemon$Type.1 == "Dragon"~"Dragon",
                                                           pokemon$Type.1 == "Flying"~"Fighting, Bug, Grass",
                                                           pokemon$Type.1 == "Ground"~"Poison, Rock, Steel, Fire, Electric",
                                                           pokemon$Type.1 == "Electric"~"Flying, Water",
                                                           pokemon$Type.1 == "Fairy"~"Fighting, Dragon, Dark",
                                                           pokemon$Type.1 == "Fighting"~"Normal, Rock, Steel, Ice, Dark",
                                                           pokemon$Type.1 == "Psychic"~"Fighting, Poison",
                                                           pokemon$Type.1 == "Rock"~"Flying, Bug, Fire, Ice",
                                                           pokemon$Type.1 == "Steel"~"Rock, Ice, Fairy",
                                                           pokemon$Type.1 == "Ice"~"Flying, Ground, Grass, Dragon",
                                                           pokemon$Type.1 == "Dark"~"Ghost, Phychic",
                                                           pokemon$Type.1 == "Ghost"~"Ghost, Phychic"))
      pokemon <- pokemon[pokemon$Name==input$search1,]
      pokemon$Against_to
      # paste("Against to:", pokemon$Against_to)
    })
    
    output$search1weak <- renderText({
      
      pokemon <- pokemon %>% mutate(Weak_to = case_when(pokemon$Type.1 == "Normal"~ "Fighting",
                                                        pokemon$Type.1 == "Grass"~ "Flying, Poison, Bug, Fire, Ice",
                                                        pokemon$Type.1 == "Fire"~"Ground, Rock, Water",
                                                        pokemon$Type.1 == "Water"~"Grass, Electric",
                                                        pokemon$Type.1 == "Bug"~"Flying, Rock, Fire",
                                                        pokemon$Type.1 == "Poison"~"Ground, Phychic",
                                                        pokemon$Type.1 == "Dragon"~"Ice, Dragon, Fairy",
                                                        pokemon$Type.1 == "Flying"~"Rock, Electric, Ice",
                                                        pokemon$Type.1 == "Ground"~"Water, Grass, Ice",
                                                        pokemon$Type.1 == "Electric"~"Ground",
                                                        pokemon$Type.1 == "Fairy"~"Poison, Steel",
                                                        pokemon$Type.1 == "Fighting"~"Flying, Phychic, Fairy",
                                                        pokemon$Type.1 == "Psychic"~"Bug, Ghost, Dark",
                                                        pokemon$Type.1 == "Rock"~"Fighting, Ground, Steel, Water, Grass",
                                                        pokemon$Type.1 == "Steel"~"Fighting, Ground, Fire",
                                                        pokemon$Type.1 == "Ice"~"Fighting, Rock, Steel, Fire",
                                                        pokemon$Type.1 == "Dark"~"Fighting, Bug, Fairy",
                                                        pokemon$Type.1 == "Ghost"~"Dark, Ghost"))
      pokemon <- pokemon[pokemon$Name==input$search1,]
      pokemon$Weak_to
      # paste("Weak to:", pokemon$Weak_to)
    })
    
    output$s2type1 <- renderText({
      ""
      if(input$search2 %in% pokemon$Name){
        "Type 1:"
      }
    })
    
    output$s2type2 <- renderText({
      ""
      if(input$search2 %in% pokemon$Name){
        "Type 2:"
      }
    })
    
    output$s2against <- renderText({
      ""
      if(input$search2 %in% pokemon$Name){
        "Against_to:"
      }
    })
    
    output$s2weak <- renderText({
      ""
      if(input$search2 %in% pokemon$Name){
        "Weak_to:"
      }
    })
    
    output$search2name <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search2,]
      toupper(pokemon$Name)
    })
    
    output$search2img <- renderImage({
      filename <- normalizePath(file.path('www/images', paste(input$search2, '.png', sep='')))
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)
    
    output$search2type1 <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search2,]
      pokemon$Type.1
      # paste("Type 1:", pokemon$Type.1)
    })
    
    output$search2type2 <- renderText({
      pokemon <- pokemon[pokemon$Name==input$search2,]
      pokemon$Type.2
      # paste("Type 2:", pokemon$Type.2)
    })
    
    output$search2against <- renderText({
      
      pokemon <- pokemon %>% mutate(Against_to = case_when(pokemon$Type.1 == "Normal"~ "None",
                                                           pokemon$Type.1 == "Grass"~ "Ground, Rock, Water",
                                                           pokemon$Type.1 == "Fire"~"Bug, Steel, Grass, Ice",
                                                           pokemon$Type.1 == "Water"~"Ground, Rock, Fire",
                                                           pokemon$Type.1 == "Bug"~"Grass, Phychic, Dark",
                                                           pokemon$Type.1 == "Poison"~"Grass, Fairy",
                                                           pokemon$Type.1 == "Dragon"~"Dragon",
                                                           pokemon$Type.1 == "Flying"~"Fighting, Bug, Grass",
                                                           pokemon$Type.1 == "Ground"~"Poison, Rock, Steel, Fire, Electric",
                                                           pokemon$Type.1 == "Electric"~"Flying, Water",
                                                           pokemon$Type.1 == "Fairy"~"Fighting, Dragon, Dark",
                                                           pokemon$Type.1 == "Fighting"~"Normal, Rock, Steel, Ice, Dark",
                                                           pokemon$Type.1 == "Psychic"~"Fighting, Poison",
                                                           pokemon$Type.1 == "Rock"~"Flying, Bug, Fire, Ice",
                                                           pokemon$Type.1 == "Steel"~"Rock, Ice, Fairy",
                                                           pokemon$Type.1 == "Ice"~"Flying, Ground, Grass, Dragon",
                                                           pokemon$Type.1 == "Dark"~"Ghost, Phychic",
                                                           pokemon$Type.1 == "Ghost"~"Ghost, Phychic"))
      pokemon <- pokemon[pokemon$Name==input$search2,]
      pokemon$Against_to
      # paste("Against to:", pokemon$Against_to)
    })
    
    output$search2weak <- renderText({
      
      pokemon <- pokemon %>% mutate(Weak_to = case_when(pokemon$Type.1 == "Normal"~ "Fighting",
                                                        pokemon$Type.1 == "Grass"~ "Flying, Poison, Bug, Fire, Ice",
                                                        pokemon$Type.1 == "Fire"~"Ground, Rock, Water",
                                                        pokemon$Type.1 == "Water"~"Grass, Electric",
                                                        pokemon$Type.1 == "Bug"~"Flying, Rock, Fire",
                                                        pokemon$Type.1 == "Poison"~"Ground, Phychic",
                                                        pokemon$Type.1 == "Dragon"~"Ice, Dragon, Fairy",
                                                        pokemon$Type.1 == "Flying"~"Rock, Electric, Ice",
                                                        pokemon$Type.1 == "Ground"~"Water, Grass, Ice",
                                                        pokemon$Type.1 == "Electric"~"Ground",
                                                        pokemon$Type.1 == "Fairy"~"Poison, Steel",
                                                        pokemon$Type.1 == "Fighting"~"Flying, Phychic, Fairy",
                                                        pokemon$Type.1 == "Psychic"~"Bug, Ghost, Dark",
                                                        pokemon$Type.1 == "Rock"~"Fighting, Ground, Steel, Water, Grass",
                                                        pokemon$Type.1 == "Steel"~"Fighting, Ground, Fire",
                                                        pokemon$Type.1 == "Ice"~"Fighting, Rock, Steel, Fire",
                                                        pokemon$Type.1 == "Dark"~"Fighting, Bug, Fairy",
                                                        pokemon$Type.1 == "Ghost"~"Dark, Ghost"))
      pokemon <- pokemon[pokemon$Name==input$search2,]
      pokemon$Weak_to
      # paste("Weak to:", pokemon$Weak_to)
    })
    
    output$plot7 <- renderPlotly({
    
    avg_data <- pokemon %>% 
      select(Name, HP, Defense, Attack, Sp..Atk, Sp..Def, Speed, HP) %>% 
      summarise(HP = mean(HP),
                Attack = mean(Attack),
                Defense = mean(Defense),
                special_attack = mean(Sp..Atk),
                special_defense = mean(Sp..Def),
                Speed = mean(Speed)
      )
    
    p1 <- pokemon %>% 
      filter(Name == input$search1) %>% 
      select(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>% 
      rename(special_attack = Sp..Atk) %>%
      rename(special_defense = Sp..Def)
    
    p2 <- pokemon %>% 
      filter(Name == input$search2) %>% 
      select(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>% 
      rename(special_attack = Sp..Atk) %>%
      rename(special_defense = Sp..Def)
    
    f <- plot_ly(
      type = 'scatterpolar',
      # fill = 'none',
      mode = "lines+markers"
    ) 
    f <- f %>%
      add_trace(
        r = c(as.numeric(avg_data), as.numeric(avg_data)[1]),
        theta = c(names(avg_data), "HP"),
        name = 'Average of All Pokemon',
        fill = "toself",
        fillcolor = '#80808050', 
        marker = list(color = '#808080'), 
        line = list(color = '#808080')
      )
    if(input$search1 %in% pokemon$Name){
    f <- f %>%
      add_trace(
        r = c(as.numeric(p1), as.numeric(p1)[1]),
        theta = c(names(p1), "HP"),
        name = input$search1,
        fill = 'none',
        marker = list(
          color = '#ffa30f',   # '#FF0000',
          size = 12),
        line = list(color = '#ffa30f',
                    width = 4)
      )
    }
    if(input$search2 %in% pokemon$Name){
    f <- f %>%
      add_trace(
        r = c(as.numeric(p2), as.numeric(p2)[1]),
        theta = c(names(p2), "HP"),
        name = input$search2,
        fill = 'none',
        marker = list(
          color = '#3463a8',  # '#2e5185',  # '#0000FF',
          size = 12),
        line = list(color = '#3463a8',
                    width = 4)
      )
    }
    f <- f %>%
      layout(
        title = "6-Dimension Comparison",
        titlefont=list(size=26),
        margin = list(t = 90, l = 12),
        legend = list(title = list(text = "Pokemon"),
                      font = list(size = 13),
                      y = 0.5,
                      x = 0.8,
                      bgcolor = '#ECECEC'),
        paper_bgcolor = "rgb(255, 255, 255)",
        polar = list(
          bgcolor = "rgb(255, 255, 255)",
          radialaxis = list(
            visible = T,
            layer = 'below traces'
            # range = c(0,180)
          ),
          angularaxis = list(
            tickwidth = 3,
            linewidth = 3,
            tickfont = list(size = 17),
            layer = 'below traces'
          )
        )
      )
    
    f
    
    })
    
    output$plot8 <- renderPlot({
      avg_total <- pokemon %>%
        select(Total) %>%
        summarise(Avg = mean(Total))

      p1_total <- pokemon %>%
        filter(Name == input$search1) %>%
        select(Name, Total)

      p2_total <- pokemon %>%
        filter(Name == input$search2) %>%
        select(Name, Total)
      
      avg_ttl <- data.frame(Pokemon = colnames(avg_total),
                            Total = avg_total[1,])
      
      ttl <- data.frame(Pokemon = c(p1_total[1,1], p2_total[1,1], colnames(avg_total)),
                        Total = c(p1_total[1,2], p2_total[1,2],avg_total[1,]))

      ttl$Pokemon <- factor(ttl$Pokemon, levels = ttl$Pokemon)
      
      if(input$search1 %in% pokemon$Name && input$search2 %in% pokemon$Name){
        f <- ggplot(ttl, mapping = aes(x=Pokemon, y=Total, fill=Pokemon)) +
          geom_bar(stat = "identity", width = 0.7) +
          scale_fill_manual(values=c("#ffa30f",
                                     "#3463a8",
                                     "#808080")) +
          coord_cartesian(ylim = c(0,800)) +
          ggtitle("Total Comparison") +
          geom_label(aes(label=Total), size = 7, color = "white", show_guide = F) +
          theme_minimal() +
          theme(panel.grid.major.x = element_blank(),
                plot.title = element_text(size = 28, hjust = 0.5),
                plot.margin = margin(t=30, l=30, b=30, r=30),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 15),
                axis.title = element_text(size = 18),
                axis.text.y = element_text(size = 15),
                axis.text.x = element_text(size = 20),
                legend.background = element_rect(color = NA, fill = '#ECECEC'))

        f
      } else{
    g <- ggplot(avg_ttl, mapping = aes(x=Pokemon, y=Total, fill=Pokemon)) +
        geom_bar(stat = "identity", width = 0.3) +
        scale_fill_manual(values="#808080") +
        coord_cartesian(ylim = c(0,800)) +
        ggtitle("Total Comparison") +
        geom_label(aes(label=Total), size = 7, color = "white", show_guide = F) +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              plot.title = element_text(size = 28, hjust = 0.5),
              plot.margin = margin(t=30, l=30, b=30, r=30),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 15),
              axis.title = element_text(size = 18),
              axis.text.y = element_text(size = 15),
              axis.text.x = element_text(size = 20),
              legend.background = element_rect(color = NA, fill = '#ECECEC'))
      
        g
      }

    })
    
    # data
    output$pokemondata = renderDataTable({
      datatable({
      if (input$type1 != "All") {
        pokemon <- pokemon[pokemon$Type.1 == input$type1,]
      }
      if (input$type2 != "All") {
        pokemon <- pokemon[pokemon$Type.2 == input$type2,]
      }
      if (input$gen != "All") {
        pokemon <- pokemon[pokemon$Generation == input$gen,]
      }
      if (input$leg != "All") {
        pokemon <- pokemon[pokemon$Legendary == input$leg,]
      }
      })
        datatable(pokemon[, input$show_vars, drop = FALSE])
    })
    
    # visual
    output$plot1 <- renderPlotly({
    
    })
    output$plot2 <- renderPlotly({
      
    })
    output$plot3 <- renderPlotly({
      
    })
    output$plot4 <- renderPlotly({
      
    })
    output$plot5 <- renderPlotly({
      
    })
    output$plot6 <- renderPlotly({
      
    })
    # output$plot1 = renderPlot({
    #   
    # })
   
    
  }
  
  shinyApp(ui = ui, server = server)
  
