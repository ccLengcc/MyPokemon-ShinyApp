library(shiny)
library(shinydashboard)
library(DT)
library(dashboardthemes)
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggrepel) 
library(shinyBS)
library(treemapify)
library(hexbin)

game <- read.csv("game release.csv")
pokemon <- read.csv("Pokemon.csv")
pokemon <- pokemon[, -1]
pokemon[pokemon == ""] <- NaN
# For display datatable
pokemon1 <-  pokemon[,c(-1, -2)]


# Define UI for application
  ui <- dashboardPage(
    skin = "yellow",

    
    dashboardHeader(
      # titleWidth='16%',
      title = span(tagList(img(src="pokeball.png", height = "35", width = "35"), "  MyPokemon")),
      tags$li(class = "dropdown", tags$a(href = "https://pokemondb.net/", icon("info-circle"), "PokemonDB", target="_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://github.com/ccLengcc/MyPokemon-ShinyApp", icon("github"), "Github", target="_blank"))
      ),
    dashboardSidebar(
      # width = "16%",
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("gamepad")),
        menuItem("Pokemon-Stats Visual",
                 icon = icon("bar-chart"),
                 menuSubItem('Generation & Type',
                             tabName = 'visual1',
                             icon = icon('line-chart')),
                 menuSubItem('Ability',
                             tabName = 'visual2',
                             icon = icon('line-chart'))),
        menuItem("Pokemon Matching", tabName = "match", icon = icon("search")),
        menuItem("Pokemon Comparison", tabName = "compare", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("table"))
      )
    ),
    dashboardBody(
      shinyDashboardThemes(theme = "purple_gradient"),
      
      tags$script(type="text/javascript",'function show(x){
              Shiny.setInputValue("in1",x);
                }'),
      
      # tags$head(tags$link(rel = "stylesheet",
      #                     type = "text/css", href = "<file name>")),
      
      
      tabItems(
        # Home tab content
        tabItem(tabName = "home",
                img(src = "home5.jpg", width = "100%", height = 180),
                br(p(" "),
                   p(" ")),
                fluidRow(
                  box(
                    title = strong("About Pokemon", style = 'font-size:22px;'), solidHeader = TRUE, 
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
                      column(10, br(p(h4("Pokemon, electronic game series from Nintendo that debuted in Japan in February 1996 as Pokemon Green and Pokemon Red.", style = 'font-size:20px;color:white;')),
                                    br(),
                                    p(h4("The series,originally produced for the company's Game Boy line of handheld consoles, was introduced in 1998 to the United States with two titles, known to fans as Red and Blue. In the games, players assume the role of Pokemon trainers, obtaining cartoon monsters and developing them to battle other Pokemon. The franchise later became wildly popular in the United States and around the world.", style = 'font-size:20px;color:white;')),
                                    br(),
                                    p(h4("Here is a list of Pokemon core game series release:", style = 'font-size:20px;color:white;')),
                                    br(),
                                    dataTableOutput("game")
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
                strong(h2("Searching for a Pokemon with required features?")),
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
                                      choices = c("All",unique(as.character(pokemon$Type.1))), 
                                      selected = "Electric"),
                         selectInput("select2", 
                                     label = h4("Type 2:"), 
                                     choices = c("All",unique(as.character(pokemon$Type.2))), 
                                     selected = "All"),
                         ),
                         box(
                           title = "Select Generation:", 
                             solidHeader = TRUE,
                             status = "info",
                             width = 12,
                             div(
                             selectInput("select3", 
                                         label = h4("Generation:"), 
                                         choices = c("All",unique(as.character(pokemon$Generation))),
                                         selected = "All"),
                         ),
                         ),
                         box(
                           title = "Is Legendary?", 
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           radioButtons("radio1",
                                              label = NULL,
                                              choices = unique(as.character(pokemon$Legendary))),
                         ),
                      ),
                  column(6,
                         br(),
                         box(
                           h4("Click for more information:"),
                           width = 24,
                           style = "overflow-y:scroll; max-height: 600px;overflow-x:hidden;",
                           uiOutput("ui1", width = "600px")
                           )
                           
                         ),
                  column(3,
                         br(),
                         box(
                           title = "Select Range:", 
                           solidHeader = TRUE,
                           status = "info",
                           width = 12,
                           sliderInput("slider1", label = h4("Total:"), min = 100, 
                                       max = 800, value = c(100, 800)),
                           br(),
                           hr(),
                           sliderInput("slider2", label = h4("HP:"), min = 0, 
                                       max = 300, value = c(0, 300)),
                           br(),
                           hr(),
                           sliderInput("slider3", label = h4("Speed:"), min = 0, 
                                       max = 200, value = c(0, 200)),
                           br(),
                           br(),
                         ),
                      ),
                  ),

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
                  column(1,),
                  column(10,
                         br(),
                         tabBox(width = 12,
                                id="tab3",
                                tabPanel("6-Dimension Comparison", plotlyOutput("plot7")),
                                tabPanel("Total Comparison", plotOutput("plot8")),
                                ),
                         
                  ),
                  column(1,),
                  
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
                                         names(pokemon1), selected = names(pokemon1)[c(1:10,12)])
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
                    column(3,
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
                              tabPanel("Generation Overview", plotlyOutput("plot6")),
                              
                         
                              tabPanel("Type Overview",
                                       fluidRow(
                                         column(3,
                                                selectInput("p3select1", label = h4(strong("Pokemon to show:")), 
                                                            choices = c("All", "Legendary"), 
                                                            selected = "All"),
                                         ),
                                         column(9,),
                                       ),
                                       plotOutput("plot3"))),
                          
                        
                  
                  
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
                        tabPanel("Pokemon Ability Profile", 
                                 fluidRow(
                                   column(4,),
                                   column(4, align="center",
                                          selectInput("p5select1", label = h4(strong("Select Pokemon:")), 
                                                      choices = unique(as.character(pokemon$Name)), 
                                                      selected = "pikachu"),
                                          imageOutput("p5img", height = "40px"),
                                          br(),
                                          br()
                                   ),
                                   column(4,),
                                 ),
                                 plotlyOutput("plot5")
                                 
                        ),
                        
                        tabPanel("Ability Overview", 
                                 fluidRow(
                                   column(3,
                                          selectInput("p2select1", label = h4(strong("Ability:")), 
                                                      choices = colnames(pokemon[6:12]), 
                                                      selected = "HP")
                                   ),
                                   column(3,),
                                   column(3,),
                                   column(3,),
                                 ),
                                 plotlyOutput("plot21"),
                                 br(),
                                 plotlyOutput("plot22"),
                                 br(),
                                 plotlyOutput("plot23"),
                                 br(),
                                 
                        ),
                         tabPanel("Ability Comparison-All", 
                                  fluidRow(
                                    column(3,
                                           selectInput("p4select1", label = h4(strong("X-axis:")), 
                                                       choices = colnames(pokemon[7:12]), 
                                                       selected = "HP"),
                                    ),
                                    column(3,
                                           selectInput("p4select2", label = h4(strong("Y-axis:")), 
                                                       choices = colnames(pokemon[7:12]), 
                                                       selected = "Attack"),
                                    ),
                                    column(6,),
                                  ),
                                  plotlyOutput("plot4", width = "970px", height = "670px"),
                                  ),
                        
                        
                        tabPanel("Ability Comparison-Legendary", 
                                 fluidRow(
                                   column(3,
                                          selectInput("p1select1", label = h4(strong("X-axis:")), 
                                                      choices = colnames(pokemon[7:12]), 
                                                      selected = "HP")
                                   ),
                                   column(3,
                                          selectInput("p1select2", label = h4(strong("Y-axis:")), 
                                                      choices = colnames(pokemon[7:12]), 
                                                      selected = "Attack"),
                                   ),
                                   column(3,
                                          selectInput("p1select3", label = h4(strong("Number to show:")), 
                                                      choices = c(10,30,"All"), 
                                                      selected = 30),
                                   ),
                                   column(3,),
                                 ),
                                 checkboxInput("p1check1", label = "Show Legendary names:", value = TRUE),
                                 br(),
                                 plotOutput("plot1"),
                                 br(),
                        ),
                         
                        
                        ),
                  ),
                  column(1,),
                )
        )
      )
    )
  )
  
  
# Define Server for application
  server <- function(input, output, session) {
    
    output$game = renderDT(game, options = list(pageLength = 5))
    
    # match

    
    filtered_data <- reactive({
      if(input$select1!="All" && input$select2!="All" && input$select3!="All"){
      pokemon %>% 
        filter(Type.1 == input$select1) %>% 
        filter(Type.2 == input$select2) %>% 
        filter(Generation == input$select3) %>% 
        filter(Legendary == input$radio1) %>% 
        filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
        filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
        filter(between(Speed, input$slider3[1], input$slider3[2]))
      }
      else if(input$select1!="All" && input$select2=="All" && input$select3!="All"){
          pokemon %>% 
            filter(Type.1 == input$select1) %>% 
            filter(Generation == input$select3) %>% 
            filter(Legendary == input$radio1) %>% 
            filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
            filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
            filter(between(Speed, input$slider3[1], input$slider3[2]))
        } 
        else if(input$select1!="All" && input$select2!="All" && input$select3=="All"){
            pokemon %>% 
              filter(Type.1 == input$select1) %>% 
              filter(Type.2 == input$select2) %>%
              filter(Legendary == input$radio1) %>% 
              filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
              filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
              filter(between(Speed, input$slider3[1], input$slider3[2]))
        }
      else if(input$select1=="All" && input$select2!="All" && input$select3!="All"){
        pokemon %>% 
          filter(Type.2 == input$select2) %>%
          filter(Generation == input$select3) %>%
          filter(Legendary == input$radio1) %>% 
          filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
          filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
          filter(between(Speed, input$slider3[1], input$slider3[2]))
      }
      else if(input$select1!="All" && input$select2=="All" && input$select3=="All"){
        pokemon %>% 
          filter(Type.1 == input$select1) %>%
          filter(Legendary == input$radio1) %>% 
          filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
          filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
          filter(between(Speed, input$slider3[1], input$slider3[2]))
      } 
      else if(input$select1=="All" && input$select2!="All" && input$select3=="All"){
        pokemon %>% 
          filter(Type.2 == input$select2) %>%
          filter(Legendary == input$radio1) %>% 
          filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
          filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
          filter(between(Speed, input$slider3[1], input$slider3[2]))
      } 
      else if(input$select1=="All" && input$select2=="All" && input$select3!="All"){
        pokemon %>% 
          filter(Generation == input$select3) %>%
          filter(Legendary == input$radio1) %>% 
          filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
          filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
          filter(between(Speed, input$slider3[1], input$slider3[2]))
      } 
          else{
              pokemon %>% 
                filter(Legendary == input$radio1) %>% 
                filter(between(Total, input$slider1[1], input$slider1[2])) %>% 
                filter(between(HP, input$slider2[1], input$slider2[2])) %>% 
                filter(between(Speed, input$slider3[1], input$slider3[2]))
            }
   
        
         })
    
    output$ui1 <- renderUI({
      fluidRow(column(12, id = "columns",
                      apply(filtered_data(), 1, function(item) {
                        wellPanel(style = 'display:inline-block;height:270px;width:270px;background-color:rgb(255,255,255);margin-right:10px;',
                                  HTML(
                                    paste0(
                                      "<div style='cursor:pointer;' onclick=show('",
                                      item['id'],
                                      "') >",
                                      "<h3>",
                                      toupper(item['Name']),
                                      "</h3>",
                                      # "<p> Generation: ",
                                      # item['Generation'],
                                      # "</p>",
                                      "<img src='",
                                      paste('https://img.pokemondb.net/artwork/large/',item['Name'],'.jpg', sep=''),
                                      
                                      # "<img src='images/",
                                      # paste(item['Name'], '.png', sep=''),
                                      
                                      "' alt='Cannot find image' style='height:160px;width:162px;' /></div>"
                                    )
                                  ))
                      })))
    })
    
    observeEvent(input$in1, {
      showModal(modalDialog(title = h4(strong("Detailed Info")),
                            size = "l",
                            tags$iframe(src = paste0("https://sg.portal-pokemon.com/play/pokedex", input$in1) # https://pokedex.org/#/pokemon/1
                                        , style="width:100%;",  frameborder="0"
                                        ,id="iframe"
                                        , height = "500px"))
                            # HTML('<button id="click" onclick="window.open(\'https://pokemondb.net/pokedex/charizard\',\'popUpWindow\',\'height=500,width=400,left=100,top=100,resizable=yes,scrollbars=yes,toolbar=yes,menubar=no,location=no,directories=no, status=yes\');"></button>')
                            
      )
      
    })
    
    
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
        fillcolor = '#b2b2b250',    # '#808050'
        marker = list(color = '#b2b2b2'),  # '#808080'
        line = list(color = '#b2b2b2')     # '#808080'
      )
    if(input$search1 %in% pokemon$Name){
    f <- f %>%
      add_trace(
        r = c(as.numeric(p1), as.numeric(p1)[1]),
        theta = c(names(p1), "HP"),
        name = input$search1,
        fill = 'none',
        marker = list(
          color = '#FFF000',   # '#FF0000', '#ffa30f'
          size = 12),
        line = list(color = '#FFF000',
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
          color = '#77CCFF',  # '#2e5185', '#0000FF', #3463a8', '#6390F0'
          size = 12),
        line = list(color = '#77CCFF',
                    width = 4)
      )
    }
    f <- f %>%
      layout(
        title = "6-Dimension Comparison",
        titlefont=list(size=26, color = "white"),
        margin = list(t = 90, l = 12),
        legend = list(title = list(text = "Pokemon"),
                      font = list(size = 13),
                      y = 0.5,
                      x = 0.8,
                      bgcolor = '#ECECEC'),  # '#ECECEC'
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)',
        # paper_bgcolor = "rgb(255, 255, 255)",
        polar = list(
          # bgcolor = "rgb(255, 255, 255)",
          bgcolor = 'rgba(0,0,0,0)',
          radialaxis = list(
            visible = T,
            layer = 'below traces',
            color = "rgba(255,255,255,0.3)"
            # range = c(0,180)
          ),
          angularaxis = list(
            tickwidth = 3,
            linewidth = 3,
            color = "rgba(255,255,255,0.3)",
            tickfont = list(size = 17, color = "white"),
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
          scale_fill_manual(values=c("#FFF000",      # "#ffa30f"
                                     "#77CCFF",      # "#3463a8", "#6390F0"
                                     "#b2b2b2")) +   # "#808080"
          coord_cartesian(ylim = c(0,800)) +
          ggtitle("Total Comparison") +
          geom_label(aes(label=round(Total,0)), size = 7, color = "#4e5895", show_guide = F) +
          theme_minimal() +
          theme(panel.grid = element_blank(),
                axis.line = element_line(size = 1, color = "white"),
                axis.line.x.top = element_blank(),
                axis.line.y.right = element_blank(),
                plot.title = element_text(size = 28, hjust = 0.5, color = "white"),
                plot.margin = margin(t=30, l=30, b=30, r=30),
                # panel.border = element_rect(color = "white", fill = 'transparent'),
                legend.title = element_text(size = 16, color = "#4e5895"),
                legend.text = element_text(size = 15, color = "#4e5895"),
                axis.title = element_text(size = 18, color = "white"),
                axis.text.y = element_text(size = 15, color = "white"),
                axis.text.x = element_text(size = 20, color = "white"),
                legend.background = element_rect(color = NA, fill = '#ECECEC'))
                # panel.background = element_rect(fill='transparent'),
                # plot.background = element_rect(fill='transparent', color=NA))

        f
      } else{
    g <- ggplot(avg_ttl, mapping = aes(x=Pokemon, y=Total, fill=Pokemon)) +
        geom_bar(stat = "identity", width = 0.3) +
        scale_fill_manual(values="#b2b2b2") +  # "#808080"
        coord_cartesian(ylim = c(0,800)) +
        ggtitle("Total Comparison") +
        geom_label(aes(label=round(Total,0)), size = 7, color = "#4e5895", show_guide = F) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.line = element_line(size = 1, color = "white"),
              axis.line.x.top = element_blank(),
              axis.line.y.right = element_blank(),
              plot.title = element_text(size = 28, hjust = 0.5, color = "white"),
              plot.margin = margin(t=30, l=30, b=30, r=30),
              # panel.border = element_rect(color = "white", fill = 'transparent'),
              legend.title = element_text(size = 16, color = "#4e5895"),
              legend.text = element_text(size = 15, color = "#4e5895"),
              axis.title = element_text(size = 18, color = "white"),
              axis.text.y = element_text(size = 15, color = "white"),
              axis.text.x = element_text(size = 20, color = "white"),
              legend.background = element_rect(color = NA, fill = '#ECECEC'))
      
        g
      }

    }, bg = 'transparent')
    
    # data
    output$pokemondata = renderDataTable({
      
      datatable({
      if (input$type1 != "All") {
        pokemon1 <- pokemon1[pokemon1$Type.1 == input$type1,]
      }
      if (input$type2 != "All") {
        pokemon1 <- pokemon1[pokemon1$Type.2 == input$type2,]
      }
      if (input$gen != "All") {
        pokemon1 <- pokemon1[pokemon$Generation == input$gen,]
      }
      if (input$leg != "All") {
        pokemon1 <- pokemon1[pokemon1$Legendary == input$leg,]
      }
      })
        datatable(pokemon1[, input$show_vars, drop = FALSE])
    })
    
    # visual
    
    ###########2D Ability Comparison (Legendary) Plot#############   
    output$plot1 <- renderPlot({
      colorsEarth <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1")
      color <-  c("#E74C3C", "#98FF66", "#FFF000", "#00FFFF", "#ff94d1", "#e3ebff")
      #3498DB
      #DF678C
      #82C341
      #FF69B4
      #cda3ff
      my_theme <- theme(
        text = element_text(color = "white"),
        # plot.title = element_text(size = 28, face = "bold"),
        plot.title = element_text(size = 34),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15, color = "white"),
        axis.line = element_line(size = 1.2, color = "white"),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', color = NA),
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=15),
        plot.subtitle = element_text(size = 16),
        panel.grid = element_blank(),
        
        panel.background = element_rect(fill='transparent',color = NA),
        plot.background = element_rect(fill='transparent', color=NA)
      )

      
      x_axis <- input$p1select1
      y_axis <- input$p1select2
      
      f <- if(input$p1select3!="All"){
      pokemon %>% 
        filter(Legendary == "TRUE") %>% 
        arrange(desc(Total)) %>% 
        head(input$p1select3) %>%
        mutate(Generation = as.factor(Generation)) %>%
        
        ggplot(aes(x = get(x_axis), y = get(y_axis))) +
        geom_point(aes(color = Generation), size = 8) +
        theme_bw() +
        # labs(x = input$p1select1, y = input$p1select2, title = paste("<",input$p1select1,">", '/', "<",input$p1select2,">"), color = "Generation",
             # subtitle = paste("Top", input$p1select3, "Legendary order by Total Abiility\n")) +
          labs(x = input$p1select1, y = input$p1select2, title = paste(input$p1select1, 'vs', input$p1select2), color = "Generation",
               subtitle = paste("Top", input$p1select3, "Legendary order by Total Abiility\n")) +
        my_theme +
        theme(panel.border = element_rect(color = "white")) +
        scale_color_manual(values = color)
      } else{
        pokemon %>% 
          filter(Legendary == "TRUE") %>% 
          arrange(desc(Total)) %>%
          mutate(Generation = as.factor(Generation)) %>%
          
          ggplot(aes(x = get(x_axis), y = get(y_axis))) +
          geom_point(aes(color = Generation), size = 8) +
          theme_bw() +
          labs(x = "HP", y = "Special Attack", title = paste(input$p1select1, '/', input$p1select2), color = "Generation",
               subtitle = "All Legendary order by Total Ability\n") +
          my_theme +
          theme(panel.border = element_rect(color = "white")) +
          scale_color_manual(values = color)
      }
      
      
      if(input$p1check1==TRUE){
        f <- f +
          geom_label_repel(aes(label = Name, color = Generation), size = 5, hjust = 0.5, fill = "black", show_guide = F)
        f
      } else{
        f
      }
    
    }, bg="transparent")

    ###########Ability Overview Plot#############   
    output$plot21 <- renderPlotly({
      color <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1", "#985277")
      ability <- c("Total", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")
      # make the list
      colorlist <- setNames(as.list(color), ability)
      # Access value using the key
      # colorlist["HP"]
      
      my_theme <- theme(
        text = element_text(color = "white"),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13, color = "white"),
        axis.line = element_line(size = 1, color = "white"),
        panel.grid.major = element_blank()
      )
      
      selected <- input$p2select1
      
      f <- pokemon %>% 
        select(input$p2select1) %>% 
        ggplot(aes(x = get(selected)))+
        geom_histogram(aes(y = ..density.., text = paste("Density: ", round(..density..,5), "\n", selected, ": ", round(x,2))), fill = "#FFBD00", alpha = 0.6) + 
        geom_density(aes(text = paste("Density: ", round(..density..,5), "\n", selected, ": ", round(x,2))), color = "#FFBD00", size = 1) +
        theme_bw() +
        geom_rug(aes(text = paste(selected, ": ", get(selected))),color = "#FFBD00") +
        labs( 
          x = selected,
          title = paste('Distribution of <', selected, ">")) +
        my_theme +
        theme(panel.border = element_rect(color = "white"),
              panel.background = element_rect(fill=alpha('white', 0.4)),
              plot.background = element_rect(fill='transparent', color=NA))
        
      
      f <- ggplotly(f, tooltip = "text")
      f
      
      
    })
      
    output$plot22 <- renderPlotly({
      
      colors <- c('#A6B91A', '#705746', '#6F35FC', '#F7D02C', '#D685AD', '#C22E28', '#EE8130', '#A98FF3', 
                   '#735797', '#7AC74C', '#E2BF65', '#96D9D6', '#A8A77A', '#A33EA1', '#F95587', '#B6A136', '#B7B7CE', '#6390F0') 
      
      my_theme <- theme(
        text = element_text(color = "white"),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13, color = "white"),
        axis.text.x = element_text(size = 8, color = "white"),
        axis.line = element_line(size = 1, color = "white"),
        panel.border = element_rect(color = "white"),
        axis.ticks = element_line(color = "white")
      )
      
      selected <- input$p2select1

      f <- pokemon %>%
        ggplot(aes(x = Type.1, y = get(selected))) +
        geom_boxplot(aes(fill = Type.1), outlier.fill = "white", outlier.color = "white", outlier.shape = 18, outlier.alpha = 0.5, outlier.size = 2.5) +
        theme_test() +
        my_theme +
        theme(legend.position="none",
              panel.background = element_rect(fill=alpha('white', 0.4)),
              plot.background = element_rect(fill='transparent', color=NA)) +
        labs(x = "Type", y = selected, title = paste("<", selected, "> across Types"),
             fill = "Pokemon Type") +
        scale_fill_manual(values = colors)
     
       f
      
    })
    
    output$plot23 <- renderPlotly({
      
      colors <-  c("#E74C3C", "#98FF66", "#FFF000", "#00FFFF", "#ff94d1", "#e3ebff")
      
      my_theme <- theme(
        text = element_text(color = "white"),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13, color = "white"),
        axis.text.x = element_text(size = 8, color = "white"),
        axis.line = element_line(size = 1, color = "white"),
        panel.border = element_rect(color = "white"),
        axis.ticks = element_line(color = "white")
      )
      
      selected <- input$p2select1
      
      f <- pokemon %>%
        ggplot(aes(x = as.factor(Generation), y = get(selected))) +
        geom_boxplot(aes(fill = as.factor(Generation)), outlier.fill = "white", outlier.color = "white", outlier.shape = 18, outlier.alpha = 0.5, outlier.size = 2.5) +
        theme_test() +
        my_theme +
        theme(legend.position="none",
              panel.background = element_rect(fill=alpha('white', 0.4)),
              plot.background = element_rect(fill='transparent', color=NA)) +
        labs(x = "Generation", y = selected, title = paste("<", selected, "> across Generations"),
             fill = "Pokemon Generation") +
        scale_fill_manual(values = colors)
      
      f
      
    })
    

    ###########Type Overview Plot#############   
    output$plot3 <- renderPlot({

      colors1 <- c('#A6B91A', '#705746', '#6F35FC', '#F7D02C', '#D685AD', '#C22E28', '#EE8130', '#A98FF3', 
                   '#735797', '#7AC74C', '#E2BF65', '#96D9D6', '#A8A77A', '#A33EA1', '#F95587', '#B6A136', '#B7B7CE', '#6390F0')
      colors2 <- c('#705746', '#6F35FC', '#F7D02C', '#D685AD', '#EE8130', '#A98FF3', 
                   '#735797', '#7AC74C', '#E2BF65', '#96D9D6', '#A8A77A', '#F95587', '#B6A136', '#B7B7CE', '#6390F0')

      if(input$p3select1 == "All"){
      Count <- pokemon %>%
        group_by(Type.1) %>%
        summarise(count = n())
      }else{
        Count <- pokemon %>%
          filter(Legendary == TRUE) %>% 
          group_by(Type.1) %>%
          summarise(count = n())
      }

      if(input$p3select1 == "All"){
      Count %>%
        ggplot(aes(area = count, fill = Type.1,
                   label = paste(Type.1, count, sep = "\n"))) +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "none",
              plot.background = element_rect(fill='transparent', color=NA),
              text = element_text(color = "white"),
              plot.title = element_text(size = 24)) +
        labs(title = "Distribution of Types\n") +
        scale_fill_manual(values = colors1)
        }else{
          Count %>%
            ggplot(aes(area = count, fill = Type.1,
                       label = paste(Type.1, count, sep = "\n"))) +
            geom_treemap() +
            geom_treemap_text(colour = "white",
                              place = "centre",
                              size = 15) +
            theme(legend.position = "none",
                  plot.background = element_rect(fill='transparent', color=NA),
                  text = element_text(color = "white"),
                  plot.title = element_text(size = 24)) +
          labs(title = "Distribution of Types (Legendary)\n") +
            scale_fill_manual(values = colors2)
        }
      
    }, bg = "transparent")
    
    ###########2D Ability Comparison Plot#############   
    output$plot4 <- renderPlotly({
      
      my_theme <- theme(
        text = element_text(color = "white"),
        panel.grid.major = element_line(color = "rgba(255,255,255,0.3)"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15, color = "white"),
        axis.line = element_line(size = 1.2, color = "white"),
        legend.box.background = element_rect(fill = "transparent",size = 1),
        legend.background = element_rect(fill = "transparent"),
        legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.title = element_text(size = 15),
        legend.text = element_text(size=10),
        
        panel.background = element_rect(fill='transparent',color = NA),
        plot.background = element_rect(fill='transparent', color=NA))
      
      colorsEarth <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1")
      
      x_axis <- input$p4select1
      y_axis <- input$p4select2
      
      pokemon %>% 
        ggplot(aes(x = get(x_axis), y = get(y_axis))) +
        
        geom_hex(bins = 30, color = "grey18") +
        theme_light() +
        labs(x = x_axis, y = y_axis, title = paste(x_axis, "vs", y_axis)) +
        my_theme +
        scale_fill_gradient2(low = colorsEarth[5], mid = colorsEarth[2], high = colorsEarth[1],
                             midpoint = 8)
      
    })
    
    ###########Pokemon Ability Profile Plot#############
    output$plot5 <- renderPlotly({
      
      all_avg <- pokemon %>%
        summarise(Total_avg=mean(Total),
                  HP_avg=mean(HP),
                  Attack_avg=mean(Attack),
                  Defense_avg=mean(Defense),
                  SpAtk_avg=mean(Sp..Atk),
                  SpDef_avg=mean(Sp..Def),
                  Speed_avg=mean(Speed))
      
      type_avg <- pokemon %>%
        group_by(Type.1) %>% 
        summarise(Total_avg=mean(Total),
                  HP_avg=mean(HP),
                  Attack_avg=mean(Attack),
                  Defense_avg=mean(Defense),
                  SpAtk_avg=mean(Sp..Atk),
                  SpDef_avg=mean(Sp..Def),
                  Speed_avg=mean(Speed)) %>% 
        column_to_rownames(var="Type.1")
      
      
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Total[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$Total_avg, font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0, 0.05)),
          title =list(text = "Total", font = list(size = 20)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 800), tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "Total_avg"]),
            steps = list(
              list(range = c(0, all_avg$Total_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$HP[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$HP_avg,font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0.15, 0.20)),
          title =list(text = "HP", font = list(size = 20)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 280),tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "HP_avg"]),
            steps = list(
              list(range = c(0, all_avg$HP_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Attack[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$Attack_avg, font = list(size = 17)),
          domain = list(x = list(0.2, 0.8), y = c(0.30, 0.35)),
          title =list(text = "Attack", font = list(size = 20)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 200),tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "Attack_avg"]),
            steps = list(
              list(range = c(0, all_avg$Attack_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Defense[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$Defense_avg, font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0.45, 0.50)),
          title =list(text = "Defense", font = list(size = 20)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 250),tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "Defense_avg"]),
            steps = list(
              list(range = c(0, all_avg$Defense_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Sp..Atk[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$SpAtk_avg, font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0.60, 0.65)),
          title =list(text = "Special\n Attack", font = list(size = 16)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 200),tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "SpAtk_avg"]),
            steps = list(
              list(range = c(0, all_avg$SpAtk_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Sp..Def[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$SpDef_avg, font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0.75, 0.80)),
          title =list(text = "Special\n Defense",
                      font = list(size = 16, color = "white")),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 250),tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "SpDef_avg"]),
            steps = list(
              list(range = c(0, all_avg$SpDef_avg), color = "lightgray")),
            bar = list(color = "blue")))
      fig <- fig %>%
        add_trace(
          type = "indicator",
          mode = "number+gauge+delta",
          value = pokemon$Speed[pokemon$Name==input$p5select1],
          number = list(font = list(size = 20)),
          delta = list(reference = all_avg$Speed_avg, font = list(size = 17)),
          domain = list(x = c(0.2, 0.8), y = c(0.90, 0.95)),
          title =list(text = "Speed", font = list(size = 20)),
          gauge = list(
            bgcolor = "white",
            borderwidth = 2,
            bordercolor = "black",
            shape = "bullet",
            axis = list(range = list(NULL, 200), tickwidth = 1, tickcolor = "white"),
            threshold = list(
              line= list(color = "black", width = 3),
              thickness = 0.75,
              value = type_avg[pokemon$Type.1[pokemon$Name==input$p5select1], "Speed_avg"]),
            steps = list(
              list(range = c(0, all_avg$Speed_avg), color = "lightgray")),
            bar = list(color = "blue")))
      
      fig <- fig %>%
        layout(
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          font = list(color = "white", family = "Arial"),
          width = 1000, height = 400,
          annotations = 
            list(x = 0.9, y = -0.16, text = paste("Grey Bar: Average of all Pokemons","\n", "Black line: Average of Pokemons of that type"),
                 font=list(size=10, color="color"))
            )
      
      fig
      
    })
    
    output$p5img <- renderImage({
      filename <- normalizePath(file.path('www/images', paste(input$p5select1, '.png', sep='')))
      # Return a list containing the filename
      list(src = filename)
    }, deleteFile = FALSE)

    ###########Generation Overview Plot#############
    output$plot6 <- renderPlotly({
      
      my_theme <- theme(
        text = element_text(color = "white"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text.y = element_text(size = 13, color = "white"),
        axis.text.x = element_text(size = 8, color = "white"),
        axis.line = element_line(size = 1, color = "white"),
        panel.border = element_rect(color = "white"),
        axis.ticks = element_line(color = "white")
      )
      
      pokemon_total <- pokemon %>% 
        group_by(Generation, Type.1) %>% 
        summarise(n=n()) %>% 
        arrange(Generation, desc(n)) %>% 
        slice(4:12) %>% 
        group_by(Generation) %>%
        summarise(n = sum(n)) %>% 
        mutate(Type.1 = rep("Others",6))
      
      pokemon_top3bytype <- pokemon %>% 
        select(Generation, Type.1) %>% 
        group_by(Generation, Type.1) %>% 
        summarise(n=n()) %>% 
        arrange(Generation, desc(n)) %>% 
        slice(1:3)
      
      pokemon_totalbytype <- pokemon_total %>% 
        rbind(pokemon_top3bytype) %>% 
        arrange(Generation)
      
      f <- pokemon_totalbytype %>%
        ggplot(aes(x = as.factor(Generation), y = n, fill = reorder(Type.1, -desc(n)), text = paste("Type:", Type.1, "\n", "Count: ", n,"\n", "Generation: ", Generation))) +
        geom_bar(position = "stack", stat = "identity") +
        labs(y = "Number of Pokemons", x = "Generation", title = "Distribution of Generations") +
        scale_fill_manual(values = c("#D685AD", "#B6A136", "#735797", "#7AC74C", "#A6B91A", "#A8A77A", "#6390F0", "#808080")) +
        my_theme +
        theme(panel.border = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill='transparent'),
              plot.background = element_rect(fill='transparent', color=NA),
              legend.position="none")
      
      
      f <- ggplotly(f, tooltip = "text")
      f
      
    })
   
    
  }
  
  shinyApp(ui = ui, server = server)
  
