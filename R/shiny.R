
library(shiny)
library(shinydashboard)

jurisdictions <- list("AK","AL","AR","AS","AZ","CA","CNMI","CO","CT","DC",
                      "DE","FL","FSM","GA","GU","HI","IA","ID","IL","IN","KS",
                      "KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC",
                      "ND","NE","NH","NJ","NM","NV","NY","NYC","OH","OK","OR",
                      "PA","PR","PW","RI","RMI","SC","SD","TN","TX","USVI","UT",
                      "VT","VA","WA","WI","WV","WY","N/A")

ui<-dashboardPage(skin="red",

                  dashboardHeader(title="SA Products Generator",
                                  titleWidth = "30%"),
                  dashboardSidebar(
                      tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
                      width = "30%",
                      sidebarMenu(
                          menuItem("Director's Brief",
                                   tabName="dbb",
                                   icon=icon("file"),
                                   selected=TRUE),
                          menuItem("Director's Executive Summary",
                                   tabName="exsum",
                                   icon=icon("file-word")),
                          menuItem("Response Update Report",
                                   tabName="rur",
                                   icon=icon("file-pdf")),
                          menuItem("White House Slides",
                                   tabName="wh",
                                   icon=icon("file-powerpoint")),
                          menuItem("Incident Management",
                                   tabName="im",
                                   icon=icon("file-powerpoint"))
                      )),

                  dashboardBody(
                      tags$style(
                          "p, div {font-size: 16px;}"
                      ),
                      tabItems(
                          tabItem("dbb",
                                  fluidPage(
                                      mainPanel(
                                          tabsetPanel(
                                              tabPanel("Case and Death Reporting",
                                                       checkboxGroupInput("zero_reporting",
                                                                          "Zero New Cases & Deaths Reported:",
                                                                          choiceNames = jurisdictions,
                                                                          choiceValues = jurisdictions,
                                                                          inline = TRUE),
                                                       checkboxGroupInput("reporting_cadence",
                                                                          "Reporting Cadence Exculsions:",
                                                                          choiceNames = jurisdictions,
                                                                          choiceValues = jurisdictions,
                                                                          inline = TRUE),
                                                       checkboxGroupInput("historical_reconciliation",
                                                                          "Historical Reconciliation Esclusions:",
                                                                          choiceNames = jurisdictions,
                                                                          choiceValues = jurisdictions,
                                                                          inline = TRUE),
                                                       checkboxGroupInput("backlog",
                                                                          "Backlog: Exclusions",
                                                                          choiceNames = jurisdictions,
                                                                          choiceValues = jurisdictions,
                                                                          inline = TRUE)),
                                              tabPanel("Hospitalizations",
                                                       fluidRow(
                                                           id = "form",

                                                           textInput("name", "Submitted by", ""),
                                                           textAreaInput("favourite_pkg", "Add Text"),
                                                           fileInput("file1", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file2", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file3", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file4", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv"))
                                                       )),
                                              tabPanel("Vaccination",
                                                       fluidRow(
                                                           id = "form",

                                                           textInput("name", "Submitted by", ""),
                                                           textAreaInput("favourite_pkg", "Add Text"),
                                                           fileInput("file1", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file2", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file3", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file4", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv"))
                                                       )),
                                              tabPanel("Testing",
                                                       fluidRow(
                                                           id = "form",

                                                           textInput("name", "Submitted by", ""),
                                                           textAreaInput("favourite_pkg", "Add Text"),
                                                           fileInput("file1", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file2", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file3", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv")),
                                                           fileInput("file4", "Upload Image",
                                                                     multiple = FALSE,
                                                                     accept = c("text/csv",
                                                                                "text/comma-separated-values,text/plain",
                                                                                ".csv"))
                                                       )),
                                              tabPanel("Download",
                                                       downloadButton("downloadReport", "Download DBB")))
                                      ),

                                      textOutput("txt"),
                                      width=12
                                  )),
                          tabItem("exsum",
                                  fluidPage(
                                      fluidRow(
                                          id = "form",

                                          selectInput("level", "Administrative Level:",
                                                      c("National" = "cyl",
                                                        "Regional" = "am",
                                                        "Jurisdictional" = "gear")),

                                          selectInput("timeframe", "Timeframe:",
                                                      c("Daily" = "cyl",
                                                        "Weekly" = "am",
                                                        "Monthly" = "gear"))
                                      )
                                  ),
                                  downloadButton("downloadExSum", "Download")),
                          tabItem("rur",
                                  fluidPage( fluidRow(
                                      id = "form",

                                      textInput("name", "Submitted by", ""),
                                      textAreaInput("favourite_pkg", "Add Text"),
                                      fileInput("file1", "Upload Image",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      fileInput("file2", "Upload Image",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      fileInput("file3", "Upload Image",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      fileInput("file4", "Upload Image",
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"))
                                  ),
                                  downloadButton("downloadRUR", "Download Response Update Report")
                                  )),
                          tabItem("wh",
                                  fluidPage(
                                      downloadButton("downloadWHPPT", "Download White House Slides")
                                  )),
                          tabItem("im",
                                  fluidPage(
                                      fluidRow(
                                          id = "form",

                                          textInput("name", "Submitted by", ""),
                                          textAreaInput("favourite_pkg", "Add Text"),
                                          fileInput("file1", "Upload Image",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          fileInput("file2", "Upload Image",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          fileInput("file3", "Upload Image",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv")),
                                          fileInput("file4", "Upload Image",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv"))
                                      ),
                                      downloadButton("downloadIMPPT", "Download Incident Management Slides")
                                  ))
                      )
                  ))

server<-function(input,output){


    output$downloadReport <- downloadHandler(
        filename = paste0("DBB_",
                          (Sys.Date() +1) %>%
                              format("%m_%d_%Y"),
                          ".docx"),
        content = function(file){
            output <- rmarkdown::render(
                input = "DBB_v2.Rmd",
                output_format = "word_document",

                params = list(zero_report_states = input$zero_report,
                              reporting_cadence = input$reporting_cadence,
                              historical_rec = input$historical_reconciliation,
                              backlog = input$backlog)
            )

            file.copy(output, file)



        })



    output$downloadExSum <- downloadHandler(
        filename = paste0("(FOUO) CDC COVID-19 RESPONSE - CDC DIRECTORS EXSUM ",
                          (Sys.Date() +1) %>%
                              format("%Y%m%d"),
                          ".docx"),
        content = function(file){
            output <- rmarkdown::render(
                input = "ExeSum.Rmd",
                output_format = "word_document",

                params = list(zero_report_states = input$zero_report,
                              reporting_cadence = input$reporting_cadence,
                              historical_rec = input$historical_reconciliation,
                              backlog = input$backlog)
            )

            file.copy(output, file)


        })

    output$downloadWHPPT <- downloadHandler(
        filename = paste0("(FOUO) CDC COVID-19 DIRECTORS WH PRESS BRIEFING ",
                          (Sys.Date() +1) %>%
                              format("%Y%m%d"),
                          ".ppt"),
        content = function(file){
            output <- rmarkdown::render(
                input = "WH_Slides.Rmd",
                output_format = "powerpoint_presentation"
            )

            file.copy(output, file)


        })

    output$vaccinevalue <- renderText({
        icons <- paste(input$vaccinetest, collapse = ", ")
        paste("You input: ", icons)
    })

    output$vacvalue <- renderText({input$vaccinetest
    })
    output$testmapvalue2 <- renderText({input$testmap
    })
    output$testmapvalue <- renderText({
        icons <- paste(input$testmap, collapse = ", ")
        paste("You input: ", icons)
    })

}


shinyApp(ui,server)
