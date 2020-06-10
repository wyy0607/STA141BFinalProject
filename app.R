library(shiny)
library(jsonlite)
library(tidyverse)
library(DT)

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Vehicle information"),

  # Sidebar with select inputs
  sidebarLayout(
    sidebarPanel(

      # let user choose vehicle type
      selectInput("Type", "Please pick a vehicle type:",
        choices = c("Car", "Trailer", "Moto", "Bus", "Vehicle", "Truck")
      ),

      # let user choose corresponding brand
      selectInput("Brand", "Please pick a brand:", choices = "-"),

      # let user choose a start year for the histogram
      selectInput("start_year", "Please pick a year to start",
        choices = c("-", c(1971:format(Sys.Date(), "%Y")))
      ),

      # let user choose an end year for the histogram
      selectInput("end_year", "Please pick a year to end",
        choices = c("-", c(1971:format(Sys.Date(), "%Y")))
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",

        # tab for a barplot of number of brands making the vehicle types
        tabPanel(
          "barplot for brands of type",
          plotOutput("Barplt")
        ),

        # tab for list of models for selected type and brand
        tabPanel(
          "models for selected type and brand",
          dataTableOutput("Model")
        ),

        tabPanel(
          "manufacturer info for selected brand",
          dataTableOutput("Manufacturer")
        ),
        tabPanel(
          "barplot for models over years",
          plotOutput("Barplt2")
        )
      )
    )
  )
)




# Define server for aplication
server <- function(input, output, session) {

  # update the input brand making the selected type based on input type
  observeEvent(input$Type, {
    Brand <- fromJSON(
      str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/GetMakesForVehicleType/{Type}?format=json",
        Type = input$Type
      ),
      flatten = TRUE
    )$Results$MakeName
    updateSelectInput(session, "Brand", choices = Brand)
  })

  # find the models for selected vehicle type and brand
  output$Model <- renderDataTable({
    datatable(data.frame(fromJSON(
      str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/getmodelsformakeyear/make/{Brand}/vehicleType/{Type}?format=json",
        Brand = gsub("[[:blank:]]", "_", input$Brand), Type = input$Type
      ),
      flatten = TRUE
    )$Results)
    %>% select(Make_Name, Model_Name))
  })

  # find manufacturer info for selected brand
  output$Manufacturer <- renderDataTable({

    # display "no manufacturer" when the brand does not have one
    if (length(data.frame(
      fromJSON(
        str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/getmanufacturerdetails/{Brand}?format=json",
          Brand = gsub("[[:blank:]]", "_", input$Brand)
        ),
        flatten = TRUE
      )$Results
    )) == 0) {
      datatable(
        data.frame(Mfr_Name = "No manufacturer", Address = NA, City = NA, StateProvince = NA, PostalCode = NA, Country = NA, ContactEmail = NA, ContactPhone = NA)
      )
    }

    # display the corresponding info when the brand have manufacturers
    else {
      datatable(
        data.frame(fromJSON(
          str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/getmanufacturerdetails/{Brand}?format=json",
            Brand = gsub("[[:blank:]]", "_", input$Brand)
          ),
          flatten = TRUE
        )$Results, na.strings = "NA")
        %>% select(Mfr_Name, Address, City, StateProvince, PostalCode, Country, ContactEmail, ContactPhone)
      )
    }
  })

  output$Barplt <- renderPlot({

    # creating the dataframe for plot
    x <- c("Car", "Trailer", "Moto", "Bus", "Vehicle", "Truck")
    y <- list()
    for (i in 1:length(x)) {
      y[i] <- fromJSON(
        str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/GetMakesForVehicleType/{Type}?format=json",
          Type = x[i]
        ),
        flatten = TRUE
      )$Count
    }
    df <- data.frame(x, y)

    # ploting the barplot of brands for different vehicle types
    ggplot(data = df, aes(x, y)) +
      geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
      xlab("Vehicle Type") +
      ylab("number of Brands") +
      ggtitle("number of brands for different vehicle type") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$Barplt2 <- renderPlot({

    # if user does not choose start_year or end_year,
    # display it from the earliest year to latest year
    if (input$start_year == "-") {
      start_year <- 1971
    }
    else {
      start_year <- input$start_year
    }
    if (input$end_year == "-") {
      end_year <- format(Sys.Date(), "%Y")
    }
    else {
      end_year <- input$end_year
    }

    # create dataframe for plot
    Year <- start_year:end_year
    y <- list()
    for (i in 1:length(Year)) {
      y[i] <- fromJSON(
        str_glue("https://vpic.nhtsa.dot.gov/api/vehicles/GetCanadianVehicleSpecifications/?Year={Year}&Make={Brand}&Model=&units=&format=json",
          Year = Year[i], Brand = gsub("[[:blank:]]", "_", input$Brand)
        )
      )$Count
    }
    y <- unlist(y)
    df <- data.frame(Year, y)

    # plot the barplot
    ggplot(df, aes(Year, y)) +
      geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
      xlab("year") +
      ylab("number of models") +
      ggtitle("barplot of model numbers for years") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}





# Run the application
shinyApp(ui = ui, server = server)
