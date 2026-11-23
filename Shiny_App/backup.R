### Natalie Goeler-Slough
### OCN 682 Final Project
### Benthic Explorer Shiny App
### November 20, 2026


# Building off of week 12 homework with shiny app of coralnet benthic data (from my herbivory project):


### Loading libraries:
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(here)
library(bslib)
library(leaflet)


### Loading data:
coralnet <- read_csv("Data/annotations.csv")
labelset <- read_csv("Data/CoralNet_Labelset.csv")


#codes from labelset to exclude (not included in benthic cover %)
exclude_codes <- c("OOQ", "PVC", "UNK", "MOBL")

#defining codes that count as "grazeable substrate"
grazeable_codes <- c("TURF_H", "TURF_R", "TURF_S", "MAL", "MA_EN", "Stubby", "CCA_H", "CCA_R", "CCAG")

#setting up benthic palette:
benthic_pal <- c(
  "Coral" = "#C0504D",
  "CCA" = "#F4C2C2",
  "Macroalgae" = "#0F3800",
  "Turf" = "#16A34A",
  "Sand" = "#E8D8B0",
  "Urchin" = "#5E50A1",
  "Other Invertebrates" = "#0096C7",
  "Cyanobacteria" = "#5C1A48"
)

#pre-filtering/cleaning data
benthic_clean <- coralnet |>
  mutate(Date = as.Date(Date)) |> #making sure date column is as a date
  rename(Label.code = "Label code") |>
  rename(GoPro.ID = "GoPro ID") |>
  mutate(Site = str_squish(str_replace_all(Site, "[._]", " "))) |> #changing names to match user dropdown below
  left_join(labelset, by = "Label.code") |> #adding labelset to coralnet data
  filter(!Label.code %in% exclude_codes) #excluding codes not included in benthic cover


#setting up site and date choices for user, based on options available in dataset
site_choices <- sort(unique(benthic_clean$Site))
date_choices <- sort(unique(benthic_clean$Date))
date_choices <- setNames(
  as.character(date_choices),
  format(date_choices, "%b %d, %Y")   #changing date formatting so it's nicer for the label the user sees
)

#setting table of site coordinates
sites <- tribble(
  ~Site,             ~lat,      ~lon,
  "Hanauma Bay",     21.268538,	-157.695141,
  "Electric Beach",  21.351979,	-158.131955,
  "Ko Olina North",  21.3391,	  -158.13505,
  "Magic Island",    21.28175,	-157.85071,
  "Maunalua Bay",    21.26997,	-157.7441,
  "Three Tables",    21.64603,	-158.06706
)

### Setting up UI:
ui <- navbarPage(
  title = "Benthic Herbivory Project", #title of entire app
  theme = bs_theme(bootswatch = "lux"), #using my go-to theme lux again
  # PAGE 1 = map of study sites
  tabPanel(
    "Study Sites",
    leafletOutput("map", height = "calc(100vh - 100px)")
  ),
  # PAGE 2 = benthic composition explorer
  tabPanel(
    "Benthic Composition Explorer",
    sidebarLayout( 
      sidebarPanel( #making user choice options on a sidebar panel
        width = 3,
        radioGroupButtons(
          inputId = "view", label = "View:",
          choices = c("Site/date summary" = "summary", #user chooses if they want to see summary data
                      "Individual quadrats" = "quadrat"), #or individual quadrat data
          selected = "summary", direction = "vertical", justified = TRUE
        ),
        pickerInput(
          inputId = "site", label = "Select Site(s):", #can select which sites (default = all)
          choices = site_choices, selected = site_choices, multiple = TRUE,
          options = pickerOptions(actionsBox = TRUE, size = 10,
                                  selectedTextFormat = "count > 2")
        ),
        pickerInput(
          inputId = "date", label = "Select Date(s):", #select which dates to view (default = all)
          choices = date_choices, selected = NULL, multiple = TRUE,
          options = pickerOptions(actionsBox = TRUE, size = 10,
                                  selectedTextFormat = "count > 2",
                                  noneSelectedText = "All dates")
        )
      ),
      mainPanel( 
        width = 9,
        tabsetPanel( #separate tabs for plot or table output
          type = "pills", #making the tables "pills" style (more like a button)
          tabPanel("Plot", #one tab for the plot
                   h3(textOutput("plot_title")),
                   uiOutput("bar_ui")),
          tabPanel("Table", #one tab for the table
                   h4(textOutput("table_title")),
                   tableOutput("table"))
        )
      )
    )
  )
)


### Setting up server:
server <- function(input,output){
  #setting up reactive data that filters by user inputted sites / dates 
  filtered_data <- reactive({
    req(input$site)
    df <- benthic_clean |> filter(Site %in% input$site) #filtering by inputted site(s)
    if (length(input$date) > 0) {
      df <- df |> filter(Date %in% as.Date(input$date)) #filtering by inputted date(s)
    }
    #if there isn't data for that site/date combination, print saying there's no survey data
    validate(need(nrow(df) > 0, "No survey data found for this site/date combination."))
    df
  })
  #calculating benthic cover, by quadrat
  quadrat_cover <- reactive({
    filtered_data() |>
      filter(Subcategory %in% names(benthic_pal)) |> #making sure only subcategories from benthic palette are included
      group_by(Name) |> 
      mutate(Total.points = n()) |> #calculating total points per quadrat
      group_by(Name, Date, Site, GoPro.ID, Subcategory, Total.points) |>
      summarise(Point.count = n(), .groups = "drop") |>
      mutate(Percent.cover = (Point.count / Total.points) * 100) |> #calculating % cover per quadrat
      complete(
        nesting(Name, Date, Site, GoPro.ID),
        Subcategory = names(benthic_pal),
        fill = list(Point.count = 0, Percent.cover = 0)
      ) |>
      mutate(Subcategory = factor(Subcategory, levels = names(benthic_pal)))
  })
  #calculations for summarized cover (if selected summary version vs individual quadrats)
  summary_cover <- reactive({
    quadrat_cover() |>
      filter(Subcategory %in% names(benthic_pal)) |> #making sure only subcategories from benthic palette are included
      group_by(Site, Date, Subcategory) |>
      summarise(Mean.cover = mean(Percent.cover), #calculating summarized % cover (averaged across quadrats)
                n.quadrats = n_distinct(Name),
                .groups = "drop")
  })
  #adding plot title above sub-plots (separated so it doesn't overlap with individual plot titles)
  output$plot_title <- renderText({
    if (input$view == "quadrat") "Benthic Cover by Quadrat"
    else "Mean Benthic Cover per Site/Date"
  })
  #determining how many panels/plots will be drawn (based on how many sites/dates selected)
  n_panels <- reactive({
    d <- quadrat_cover()
    if (input$view == "summary") n_distinct(d$Site) #distinct # of quadrat cover values, if by summary
    else nrow(distinct(d, Site, Date)) #distinct # of quadrat cover values, if by individual quadrat
  })
  #setting plot height based on number of panels/plots
  plot_height <- reactive(250 + 350 * ceiling(n_panels() / 2))
  #each plot/panel gets the same height
  output$bar_ui <- renderUI({
    plotlyOutput("bar", height = paste0(plot_height(), "px"))
  })
  #now making stacked barplot 
  output$bar <- renderPlotly({
    plot <- if (input$view == "quadrat") { #if selected per quadrat
      ggplot(quadrat_cover(),
             aes(x = GoPro.ID, y = Percent.cover, fill = Subcategory,
                 text = paste0(Subcategory, ": ", round(Percent.cover, 1), "%"))) +
        geom_col() +
        scale_fill_manual(values = benthic_pal, drop = FALSE) +
        facet_wrap(~ Site + Date, ncol = 2, scales = "free_x") +
        labs(x = "Quadrat", y = "Percent Cover (%)",
             fill = "Benthic Category")
    } else {  #if selected summary option
      ggplot(summary_cover(),
             aes(x = factor(Date), y = Mean.cover, fill = Subcategory,
                 text = paste0(Subcategory, ": ", round(Mean.cover, 1), "%"))) +
        geom_col() +
        scale_fill_manual(values = benthic_pal, drop = FALSE) +
        facet_wrap(~ Site, ncol = 2, scales = "free_x") +
        labs(x = "Survey Date", y = "Mean Percent Cover (%)",
             fill = "Benthic Category")
    } 
    plot <- plot +
      theme_minimal(base_size = 18) +
      theme( #customizing theme / text for plots (mostly making text larger + more legible)
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 22, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        panel.spacing = unit(2, "lines") #adding space between panel rows so labels don't overlap
      )
    
    ggplotly(plot, tooltip = "text", height = plot_height()) |> #rendering plotly, plot height by # of plots as set above
      layout(margin = list(b = 140, t = 60)) #adding margin for padding so that text isn't cut off
  })
  output$table <- renderTable({
    if (input$view == "quadrat") {
      quadrat_cover() |> #taking quadrat cover values from calculations above
        transmute(Site, Date = as.character(Date), Quadrat = GoPro.ID, #fixing labels/column names for table
                  Subcategory, Percent.cover = round(Percent.cover, 1)) |>
        pivot_wider(names_from = Subcategory, values_from = Percent.cover) |>
        arrange(Site, Date, Quadrat)
    } else {
      summary_cover() |> #taking summary values from cover calculations above
        transmute(Site, Date = as.character(Date),
                  Subcategory, Mean.cover = round(Mean.cover, 1)) |>
        pivot_wider(names_from = Subcategory, values_from = Mean.cover) |>
        arrange(Site, Date)
    }
  }, digits = 1)
  #adding map output
  output$map <- renderLeaflet({
    leaflet(sites) |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        label = ~Site,
        radius = 8, color = "white", weight = 2,
        fillColor = "#0096C7", fillOpacity = 0.9
      )
  })
  
}

### Go app go !!
shinyApp(ui = ui, server=server)

