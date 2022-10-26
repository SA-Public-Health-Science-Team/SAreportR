
library(shiny)
library(shinyjs)
library(shinythemes)
library(magrittr)
#library(here)

options(shiny.autoload.r=FALSE)
#golem::add_rstudioconnect_file()

jurisdictions <- list("AK","AL","AR","AS","AZ","CA","CNMI","CO","CT","DC",
                      "DE","FL","FSM","GA","GU","HI","IA","ID","IL","IN","KS",
                      "KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
                      "ND","NE","NH","NJ","NM","NV","NY","NYC","OH","OK","OR",
                      "PA","PR","PW","RI","RMI","SC","SD","TN","TX","USVI","UT",
                      "VT","VA","WA","WI","WV","WY","N/A")

# UI ----

ui <- fluidPage(
    theme = shinytheme("lumen"),
    #  shinythemes::themeSelector(),
    includeCSS("www/styles.css"),
    h1("SAreportR"),
    h3("Situational Awareness Report Renderer"),
    br(),

    # UI - Sidebar Panel ----
    sidebarPanel(
        h3("Instructions"),
        br(),
        p("Navigate the tabs to select the report(s) you would like to download. Use the options to set the parameters of the report. Hover over the options for more information. Once you have set the parameters, press download."),
        br(),
        p("This app was developed by the Situational Awareness Branch Public Health Science Team. Source code is available on ", tags$a(href = "https://github.com/soriedumbuya/SAreportR", "GitHub.")),
        br(),
        p("Contact: SA Public Health Science Team (eocsaanalyst@cdc.gov)"),
        br(),
        br(),
        actionButton("new_game", "Refresh Data", icon = icon("arrows-rotate")),
        width = 3
    ),
    mainPanel(
        tabsetPanel(
            id = "tabs",

            # UI - Demo Report Tab ----
            tabPanel(
                title = "Demo Reports",
                shinyjs::useShinyjs(),
                fluidPage(
                    fluidRow(
                        id = "formDemo",
                        br(),
                        selectInput("levelDemo", "Administrative Level:",
                                    c("National" = "National" ,
                                      "Regional" = "Regional",
                                      "Jurisdictional" = "Jurisdictional")),
                        shinyjs::hidden(tags$div(
                            id = "regionSelectDemo",
                            selectInput("regionDemo", "Region:",
                                        c(1:10)
                            )
                        )),
                        shinyjs::hidden(tags$div(
                            id = "jurisSelectDemo",
                            selectInput("jurisDemo", "Jurisdiction:",
                                        jurisdictions)
                        )),

                        selectInput("timeframeDemo", "Timeframe:",
                                    c("Daily",
                                      "Weekly",
                                      "Monthly"))
                    ),

                    shinyjs::hidden(tags$div(
                        id = "nationButtonDemo",
                        fluidRow(downloadButton("downloadNationalDemo",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "regionButtonDemo",
                        fluidRow(downloadButton("downloadRegionalDemo",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "jurisButtonDemo",
                        fluidRow(downloadButton("downloadJurisdictionalDemo",
                                                "Download Report"))
                    )),
                ),
                icon = icon("eye")
            ),

            # UI - ExSum Tab ----

            tabPanel(
                title = "Director's Executive Summary",
                shinyjs::useShinyjs(),
                fluidPage(
                    fluidRow(
                        id = "formExSum",
                        br(),
                        selectInput("levelExSum", "Administrative Level:",
                                    c("National" = "National" ,
                                      "Regional" = "Regional",
                                      "Jurisdictional" = "Jurisdictional")),
                        shinyjs::hidden(tags$div(
                            id = "regionSelectExSum",
                            selectInput("regionExSum", "Region:",
                                        c(1:10)
                            )
                        )),
                        shinyjs::hidden(tags$div(
                            id = "jurisSelectExSum",
                            selectInput("jurisExSum", "Jurisdiction:",
                                        jurisdictions)
                        )),

                        selectInput("timeframeExSum", "Timeframe:",
                                    c("Daily",
                                      "Weekly",
                                      "Monthly"))
                    ),

                    shinyjs::hidden(tags$div(
                        id = "nationButtonExSum",
                        fluidRow(downloadButton("downloadNationalExSum",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "regionButtonExSum",
                        fluidRow(downloadButton("downloadRegionalExSum",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "jurisButtonExSum",
                        fluidRow(downloadButton("downloadJurisdictionalExSum",
                                                "Download Report"))
                    )),
                ),
            ),

            # UI - DBB Tab ----

            tabPanel(
                title = "Director's Daily Brief Bullets",
                shinyjs::useShinyjs(),
                fluidPage(
                    fluidRow(
                        id = "form3",
                        br(),
                        selectInput("level3", "Administrative Level:",
                                    c("National" = "National" ,
                                      "Regional" = "Regional",
                                      "Jurisdictional" = "Jurisdictional")),
                        shinyjs::hidden(tags$div(
                            id = "regionSelect3",
                            selectInput("region3", "Region:",
                                        c(1:10)
                            )
                        )),
                        shinyjs::hidden(tags$div(
                            id = "jurisSelect3",
                            selectInput("juris3", "Jurisdiction:",
                                        jurisdictions)
                        )),

                        selectInput("timeframe3", "Timeframe:",
                                    c("Daily",
                                      "Weekly",
                                      "Monthly"))
                    ),

                    shinyjs::hidden(tags$div(
                        id = "nationButtonDBB",
                        fluidRow(downloadButton("downloadNationalDBB",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "regionButtonDBB",
                        fluidRow(downloadButton("downloadRegionalDBB",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "jurisButtonDBB",
                        fluidRow(downloadButton("downloadJurisdictionalDBB",
                                                "Download Report"))
                    )),
                ),
                icon = icon("list")
            ),

            # UI - RUR Tab ----

            tabPanel(
                title = "COVID-19 Response Update",
                shinyjs::useShinyjs(),
                fluidPage(
                    fluidRow(
                        id = "form4",
                        br(),
                        selectInput("level4", "Administrative Level:",
                                    c("National" = "National" ,
                                      "Regional" = "Regional",
                                      "Jurisdictional" = "Jurisdictional")),
                        shinyjs::hidden(tags$div(
                            id = "regionSelect4",
                            selectInput("region4", "Region:",
                                        c(1:10)
                            )
                        )),
                        shinyjs::hidden(tags$div(
                            id = "jurisSelect4",
                            selectInput("juris4", "Jurisdiction:",
                                        jurisdictions)
                        )),

                        selectInput("timeframe4", "Timeframe:",
                                    c("Daily",
                                      "Weekly",
                                      "Monthly"))
                    ),

                    shinyjs::hidden(tags$div(
                        id = "nationButtonRUR",
                        fluidRow(downloadButton("downloadNationalRUR",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "regionButtonRUR",
                        fluidRow(downloadButton("downloadRegionalRUR",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "jurisButtonRUR",
                        fluidRow(downloadButton("downloadJurisdictionalRUR",
                                                "Download Report"))
                    )),
                ),
                icon = icon("book")
            ),

            # UI - WH Slides Tab ----

            tabPanel(
                title = "White House Press Briefs",
                shinyjs::useShinyjs(),
                fluidPage(
                    fluidRow(
                        id = "form5",
                        br(),
                        selectInput("level5", "Administrative Level:",
                                    c("National" = "National" ,
                                      "Regional" = "Regional",
                                      "Jurisdictional" = "Jurisdictional")),
                        shinyjs::hidden(tags$div(
                            id = "regionSelect5",
                            selectInput("region5", "Region:",
                                        c(1:10)
                            )
                        )),
                        shinyjs::hidden(tags$div(
                            id = "jurisSelect5",
                            selectInput("juris5", "Jurisdiction:",
                                        jurisdictions)
                        )),

                        selectInput("timeframe5", "Timeframe:",
                                    c("Daily",
                                      "Weekly",
                                      "Monthly"))
                    ),

                    shinyjs::hidden(tags$div(
                        id = "nationButtonWH",
                        fluidRow(downloadButton("downloadNationalWH",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "regionButtonWH",
                        fluidRow(downloadButton("downloadRegionalWH",
                                                "Download Report"))
                    )),

                    shinyjs::hidden(tags$div(
                        id = "jurisButton",
                        fluidRow(downloadButton("downloadJurisdictionalWH",
                                                "Download Report"))
                    )),
                ),
                icon = icon("newspaper")
            )
        )
    )
)

#B. Server ----

server <-function(input,output){


    # B.1 Demo ----

    # B.1.A Dropdowns ----

    observeEvent(input$levelDemo, {
        if (input$levelDemo == "National") {
            shinyjs::show("nationButtonDemo")
        } else {
            shinyjs:: hide("nationButtonDemo")
        }
    })

    observeEvent(input$levelDemo, {
        if (input$levelDemo == "Regional") {
            shinyjs::show("regionSelectDemo")
        } else {
            shinyjs:: hide("regionSelectDemo")
        }
    })


    observeEvent(input$levelDemo, {
        if (input$levelDemo == "Regional") {
            shinyjs::show("regionButtonDemo")
        } else {
            shinyjs:: hide("regionButtonDemo")
        }
    })

    observeEvent(input$levelDemo, {
        if (input$levelDemo == "Jurisdictional") {
            shinyjs::show("jurisSelectDemo")
        } else {
            shinyjs:: hide("jurisSelectDemo")
        }
    })

    observeEvent(input$levelDemo, {
        if (input$levelDemo == "Jurisdictional") {
            shinyjs::show("jurisButtonDemo")
        } else {
            shinyjs:: hide("jurisButtonDemo")
        }
    })


    # B.1.B Buttons -----
    output$downloadNationalDemo <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/demo/nat_demo_report.qmd',
                output_format = "html",
                output_file =  paste0('templates/demo/',
                                      "US ",
                                      input$timeframeDemo,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(timeframe =  input$timeframeDemo)
            )
            file.copy(output, file)


        })

    output$downloadRegionalDemo <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/demo/reg_demo_report.qmd',
                output_format = "html",
                output_file =  paste0('templates/demo/',
                                      "Region ",
                                      input$regionDemo,
                                      " - ",
                                      input$timeframeDemo,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(region = input$regionDemo,
                                      timeframe =  input$timeframeDemo)
            )
            file.copy(output, file)


        })

    output$downloadJurisdictionalDemo <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/demo/juris_demo_report.qmd',
                output_format = "html",
                output_file =  paste0('templates/demo/',
                                      input$jurisDemo,
                                      " - ",
                                      input$timeframeDemo,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(juris = input$jurisDemo,
                                      timeframe =  input$timeframeDemo)
            )
            file.copy(output, file)


        })

    # B.2 ExSum ----

    # B.2.A Dropdown ----

    observeEvent(input$levelExSum, {
        if (input$levelExSum == "National") {
            shinyjs::show("nationButtonExSum")
        } else {
            shinyjs:: hide("nationButtonExSum")
        }
    })

    observeEvent(input$levelExSum, {
        if (input$levelExSum == "Regional") {
            shinyjs::show("regionSelectExSum")
        } else {
            shinyjs:: hide("regionSelectExSum")
        }
    })


    observeEvent(input$levelExSum, {
        if (input$levelExSum == "Regional") {
            shinyjs::show("regionButtonExSum")
        } else {
            shinyjs:: hide("regionButtonExSum")
        }
    })

    observeEvent(input$levelExSum, {
        if (input$levelExSum == "Jurisdictional") {
            shinyjs::show("jurisSelectExSum")
        } else {
            shinyjs:: hide("jurisSelectExSum")
        }
    })

    observeEvent(input$levelExSum, {
        if (input$levelExSum == "Jurisdictional") {
            shinyjs::show("jurisButtonExSum")
        } else {
            shinyjs:: hide("jurisButtonExSum")
        }
    })


    # B.2.B Buttons ----

    output$downloadNationalExSum <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/exsum/nat_exsum.qmd',
                output_format = "html",
                output_file =  paste0('templates/exsum/',
                                      "US ",
                                      input$timeframeExSum,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(timeframe =  input$timeframeExSum)
            )
            file.copy(output, file)


        })

    output$downloadRegionalExSum <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/exsum/reg_exsum.qmd',
                output_format = "html",
                output_file =  paste0('templates/exsum/',
                                      "Region ",
                                      input$regionExSum,
                                      " - ",
                                      input$timeframeExSum,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(region = input$regionExSum,
                                      timeframe =  input$timeframeExSum)
            )
            file.copy(output, file)


        })

    output$downloadJurisdictionalExSum <-  downloadHandler(
        filename = "NA",

        content = function(file){
            output <-  quarto::quarto_render(
                input = 'templates/exsum/juris_exsum.qmd',
                output_format = "html",
                output_file =  paste0('templates/exsum/',
                                      input$jurisExSum,
                                      " - ",
                                      input$timeframeExSum,
                                      " Report ",
                                      (Sys.Date()) %>%
                                          format("%Y%m%d"),
                                      ".html"),
                execute_params = list(juris = input$jurisExSum,
                                      timeframe =  input$timeframeExSum)
            )
            file.copy(output, file)


        })
}


shinyApp(ui,server)
