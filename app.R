# Load Packages
library(htmltools)
library(htmlwidgets)
library(reactable)
library(scales)
library(dplyr)
library(ggplot2)
library(gt)

## Set up Space
remove(list = ls())


## Load Data
#setwd(datafolder)
load("FMuse.Rdata")

## UI
ui <- fluidPage( 

## Visitor tracking, main report DLs, product report DLs  
tags$head(includeHTML(("google-analytics.html"))),
  
  
navbarPage("Maine Farmers Market Price Report",
             # Tab Panel 1 - Week-by-Week Prices
                tabPanel("Monthly Report",
                    sidebarLayout( 
                        sidebarPanel(position = "left",
                                     fluidRow(class = "btn-sm",
                                              tags$style(HTML(".checkbox {margin: -10px;}")),
                                              radioButtons(inputId = "marketselect", label = "Region", 
                                                           choices = c("State Average" = "State Average",
                                                                       "Northern" = "Northern",
                                                                       "Central" = "Central",
                                                                       "Downeast" = "Downeast",
                                                                       "Southern" = "Southern"
                                                                      ),
                                                           selected = "State Average")
                                                             ),
                                     fluidRow(class = "btn-sm",
                                              selectInput(inputId = "year", label = "Market Year:", 
                                                          choices = monthlist$year,
                                                          selected = 2025)
                                     ), 
                                     
                                     fluidRow(class = "btn-sm",
                                              selectInput(inputId = "month", label = "Month:", 
                                                           choices = NULL)
                                              ),
                                                                         
                                     
                                     fluidRow(class = "btn-sm",
                                             tags$style(HTML(".checkbox {margin: -10px;}")),
                                             radioButtons(inputId = "organic", label = "Prices", 
                                                          choices = c("All Prices" = "All",
                                                                      "Not Organic" = "Not Organic",
                                                                      "Organic Only" = "Organic"),
                                                          selected = "All")
                                     ),
                                     
                                     downloadButton("marketreport", "Generate Market Report")
                                     
                        ),
                        mainPanel(
                        
                        conditionalPanel(
                              condition ="input.marketselect == 'State Average' ",
                              HTML('<center><img src="map_labels.png" width= "75%" ></center>')
                                                  ),

                        conditionalPanel(
                          condition ="input.marketselect == 'Northern' ",
                          HTML('<center><img src="map_northern.png" width= "75%" ></center>')
                        ),
                          
                        conditionalPanel(
                          condition ="input.marketselect == 'Central' ",
                          HTML('<center><img src="map_central.png" width= "75%" ></center>')
                        ),
                        
                        conditionalPanel(
                          condition ="input.marketselect == 'Downeast' ",
                          HTML('<center><img src="map_downeast.png" width= "75%" ></center>')
                        ),
                        
                        conditionalPanel(
                          condition ="input.marketselect == 'Southern' ",
                          HTML('<center><img src="map_southern.png" width= "75%" ></center>')
                        ),
                        
                        gt_output("reporttable")
                        
                        )
                        )),
             
             
    # Tab Panel 2 - Product Prices Over Time
             
     tabPanel("Product Details",
              sidebarLayout(
                  sidebarPanel(position = "left",
                               fluidRow(class = "btn-sm",
                                        tags$style(HTML(".checkbox {margin: -10px;}")),
                                        radioButtons(inputId = "marketselect2", label = "Region", 
                                                     choices = c("State Average" = "State Average",
                                                                 "Northern" = "Northern",
                                                                 "Central" = "Central",
                                                                 "Downeast" = "Downeast",
                                                                 "Southern" = "Southern"
                                                     ),
                                                     selected = "State Average")
                                        ),
     
                                        fluidRow(class = "btn-sm",
                                                 tags$style(HTML(".checkbox {margin: -10px;}")),
                                                 checkboxGroupInput(inputId = "year2", label = "Market Year:",
                                                             choices = c(2022,2023,2024,2025) ,
                                                             selected = c(2022,2023,2024,2025) )
                                        ),
     
     
                                        fluidRow(class = "btn-sm",
                                                 tags$style(HTML(".checkbox {margin: -10px;}")),
                                                 radioButtons(inputId = "organic2", label = "Prices",
                                                              choices = c("All Prices" = "All",
                                                                          "Not Organic" = "Not Organic",
                                                                          "Organic Only" = "Organic"),
                                                              selected = "All")
                                        ),
     
     
     
                                        fluidRow(class = "btn-sm",
                                                 tags$style(HTML(".checkbox {margin: 10px;}")),
                                                 radioButtons(inputId = "products", label = "Product",
                                                                    choices = products$label,
                                                                    selected = "Basil (lb)")
                                        ),
                                        downloadButton("productreport", "Generate Products Report")
     
                           ),
                           mainPanel(
     
                            
                             h3(textOutput("page2title")),
                             
                             br(),
                             
                             plotOutput("productplot"),
                             
                             br(),
                             
                             gt_output("producttable")
                             
                           )
                             )
                       )
    
    ),
    
    br(),
    
    h3(strong("Acknowledgements")),
    
    h4("This report is made possible with funding through the Maine Department of Agriculture, Conservation, and Forestry and the support of the Maine Federation of Farmers Markets, Maine Organic Farmers and Gardeners Association, and the Maine Agricultural and Forest Experiment Station at University of Maine.
       
       This work is also supported by the Hatch Act, project award no. 5501357, from the U.S. Department of Agriculture's National Institute of Food and Agriculture. Any opinions, findings, conclusions, or recommendations expressed in this publication are those of the author(s) and should not be construed to represent any official USDA or U.S. Government determination or policy."),
    
    HTML('<center><img src="banner.png" width= "100%" ></center>')
    
)   



## SF
server <- function(input, output) {
 
  ## First Page Table
  
  ## First Page - Dynamic Month list for filter
  monthsinyear <- reactive({
      filter(monthlist, year == input$year)
    })
  
  observeEvent(monthsinyear(), {
    choices <- setNames(monthsinyear()$month,monthsinyear()$month.labels)  
    updateSelectInput(inputId = "month", choices = choices, selected = 6) 
  })
  
  # Set up data
  reporttabledata <- reactive({
    rtdata <- data.full %>% filter(marketregion == input$marketselect &
                          year == input$year &
                          organic == input$organic &
                          month == input$month) %>% 
              rename(`Month` = thismonth,
                      Region = marketregion,
                     `Price Group` = organic,
                     `Avg. Price`= price,
                     `Min` = min,
                     `Max` = max,
                     `Number Reporting` = num.reporting,
                     Product= product) %>% 
              select(-product.num, -month,-year)
    return(rtdata)
  })
  
  # Make Table
  output$reporttable <- render_gt({
      reporttabledata() %>% 
        gt() %>% 
        opt_row_striping(row_striping = TRUE) %>% 
        tab_source_note(source_note = "(S) indicates that data supressed due to fewer than three reported prices." ) %>%
        tab_source_note(source_note = "(.) indicates no data available.") 
      })
    
  
  ## Second Page Time Series Plots
  
 
  # Reactive panels

   # Set up Data

producttimedata <- reactive({
    ptdata <- data.full %>% 
      filter(marketregion == input$marketselect2) %>%
      filter(organic == input$organic2) %>%
      filter(product == input$products) %>%
      filter(year %in% input$year2) %>% 
      mutate(price.num = as.numeric(price))
    return(ptdata)
  })

helperdata <- reactive({
  helpdata <- data.full %>% 
    filter(product == input$products) %>%
    mutate(price.num = as.numeric(price))
  return(helpdata)
})

  # Make Plot 

  output$productplot <- renderPlot({
    producttimedata() %>%
       ggplot(aes(x=month,y=price.num,color = factor(year),linetype=factor(year))) + geom_line(size=1.5) + geom_point() +
       ylim(0,max(helperdata()$price.num, na.rm = TRUE)) + 
       scale_x_continuous(breaks = c(6,7,8,9,10), labels = c("Jun","Jul","Aug","Sept","Oct")) +
       scale_color_manual(values=c("2022" = cbPalette[1], "2023" = cbPalette[2], "2024" = cbPalette[3], "2025" = cbPalette[4]),name="Year") + 
       scale_linetype_manual(values=c("2022" = "dotdash", "2023" = "dotted", "2024" = "dashed", "2025" = "solid"), name="Year") +
       labs(x = "Month", y = "Avg. Price ($)") + 
       theme(legend.position="bottom")
  })


  ## Second Page Table
  # Make Table 
  output$producttable <- render_gt({
    producttimedata() %>%
      arrange(-year,month) %>% 
      select(`Month`=thismonth, `Region` = marketregion,`Series` = organic, `Product` = product, `Avg. Price` = price, `Min` = min, `Max` = max, `# Rep.` = num.reporting) %>% 
      gt() %>%
      opt_row_striping(row_striping = TRUE) %>% 
      tab_source_note(source_note = "(S) indicates that data supressed due to fewer than three reported prices." ) %>%
      tab_source_note(source_note = "(.) indicates no data available.") 
  })
  
      


  ## Second Page title
  
  producttitle <- reactive({
      title.text <- paste("Product Details for",input$products,"(",input$marketselect2,",",input$organic2,")", sep= " ")
    })
  output$page2title <- renderText(producttitle())
  
  
  
  
  ## Main Page Report
  
  output$marketreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "marketpricereport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "marketpricereport.Rmd")
      tempPic <- file.path(tempdir(), "banner.png")
      tempPic2 <- file.path(tempdir(), "map_labels.png")
      file.copy("marketpricereport.Rmd", tempReport, overwrite = TRUE)
      file.copy("banner.png",tempPic, overwrite = TRUE)
      file.copy("map_labels.png",tempPic2, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(marketselect = input$marketselect,
                    month  = input$month,
                    organic = input$organic,
                    year = input$year,
                    datafull = data.full,
                    monthlist = monthlist)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  ## Second Page Report

  output$productreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "productspricereport.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "productspricereport.Rmd")
      tempPic <- file.path(tempdir(), "banner.png")
      file.copy("productspricereport.Rmd", tempReport, overwrite = TRUE)
      file.copy("banner.png",tempPic, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(marketselect = input$marketselect2,
                     prices = input$organic2,
                     productlist = input$products,
                     datafull = data.full,
                     yearchoice = input$year2)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  
    
}


## Run app 
shinyApp(ui = ui, server = server)
