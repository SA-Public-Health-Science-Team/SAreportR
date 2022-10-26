
library(shiny)
library(shinyjs)
library(shinythemes)

ui <- fluidPage(
    theme = shinytheme("lumen"),
    #  shinythemes::themeSelector(),
    includeCSS(here::here("www/styles.css")),
    titlePanel("SAreportR"),
    helpText("Situational Awareness Report Renderer"),
    sidebarPanel(
        h2("Instructions"),
        br(),
        p("Navigate the tabs to select the report(s) you would like to download. Use the options to set the parameters of the report. Hover over the options for more information. Once you have set the parameters, press download."),
        br(),
        actionButton("new_game", "Refresh Data", icon = icon("refresh")),
        width = 3
    ),
    mainPanel(
        tabsetPanel(
            id = "tabs",
            tabPanel(
                title = "Demo Reports",
                column(
                    12,
                    tableOutput("move_history")
                ),
                icon = icon("eye")
            ),
            tabPanel(
                title = "Director's Executive Summary",
                strong("Introduction"),
                p("You are the CEO of a company that researches and sells consumer technology goods.
          A new product has been invented. You, along with one competitor, are the first to market."),
          img(src = 'howtoplay.png', id = "img_howtoplay"),
          br(),
          br(),
          strong("Consumers"),
          p("You earn money by creating products that consumers like. Consumers are the
          blocks at the top row. The number on each block tells you how many consumers are in it. A consumer block's
          position indicates its dream product. All consumers like products with a high technology level.
          But there are also differences in taste, shown by their different positions on the preference fit axis."),
          strong("Products"),
          p("Filled circles represent products. Your first product (filled green circle) is already on the market. You can count the distance from a product to a consumer block. The closer a product, the more a consumer block
          likes it. If a new product is closer to a consumer block than any previous product, the consumer block buys the product
          and the product maker receives money equal to the number of consumers in the block."),
          strong("Moves"),
          p("New products can be developed by improving the technology of the product
          that you already have, or by changing its marketing. You can also imitate your competitor's latest product.
          Possible moves are indicated by empty circles. The number atop indicates the cost."),
          strong("End of the game"),
          p("The game ends when one producer makes a product that has the highest technology level (10). It
          also ends if both producers decide they don't want to do anything one after another."),
          icon = icon("file-lines")
            ),
          tabPanel(
              title = "Director's Daily Brief Bullets",
              radioButtons("order", strong("Move order"),
                           choices = c("Go first", "Go second"), inline = TRUE
              ),
              sliderInput("y_cost", strong("Cost of improving technology"),
                          min = 0, max = 100, value = 8
              ),
              sliderInput("x_cost", strong("Cost of adjusting marketing"),
                          min = 0, max = 100, value = 8
              ),
              sliderInput("imi_cost", strong("Cost of imitation"),
                          min = 0, max = 100, value = 8
              ),
              helpText("Settings take effect when a new game is started."),
              icon = icon("list")
          ),
          tabPanel(
              title = "COVID-19 Response Update",
              p("This business game was part of a study at Aalborg University on human and AI business decision making.
          Read more about the study at ", tags$a(href = "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html",
                                                 "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html")),
          br(),
          p("Source code is available at ", tags$a(href = "https://github.com/psimm/businessgame", "https://github.com/psimm/businessgame")),
          br(),
          p("Contact: Paul Simmering (paul.simmering@gmail.com)"),
          icon = icon("book")
          ),
          tabPanel(
              title = "White House Press Briefs",
              p("This business game was part of a study at Aalborg University on human and AI business decision making.
          Read more about the study at ", tags$a(href = "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html",
                                                 "https://projekter.aau.dk/projekter/da/studentthesis/human-and-ai-decision-making-in-a-game-of-innovation-and-imitation(9121a1ed-d5d7-4cf0-b725-41f822533544).html")),
          br(),
          p("Source code is available at ", tags$a(href = "https://github.com/psimm/businessgame", "https://github.com/psimm/businessgame")),
          br(),
          p("Contact: Paul Simmering (paul.simmering@gmail.com)"),
          icon = icon("newspaper")
          )
        )
    )
)

server<-function(input,output){}


shinyApp(ui,server)
