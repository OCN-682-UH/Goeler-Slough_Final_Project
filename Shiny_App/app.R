### Natalie Goeler-Slough
### OCN 682 Final Project
### Benthic Herbivory Shiny App
### November 20, 2026


### LOADING LIBRARIES:
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(here)
library(bslib)
library(leaflet)



### LOADING DATA:
coralnet <- read_csv("data/coralnet_annotations.csv") #CoralNet benthic analysis, point annotations
labelset <- read_csv("data/CoralNet_Labelset.csv") #CoralNet labelset with subcategory + broad categories assigned
herbivory <- read_csv("data/Herbivory_Data_FinalProj.csv") #Data from herbivory videos, containing bite rates with paired fish ID, joined with field data



### PRELIMINARY DATA SETUP:

#Codes from CoralNet labelset to exclude (not included in benthic cover %)
exclude_codes <- c("OOQ", "PVC", "UNK", "MOBL")

#Setting up benthic palette:
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

#Pre-filtering/cleaning benthic data
benthic_clean <- coralnet |>
  mutate(Date = as.Date(Date)) |> #making sure date column is as a date
  rename(Label.code = "Label code") |>
  rename(GoPro.ID = "GoPro ID") |>
  mutate(Site = str_squish(str_replace_all(Site, "[._]", " "))) |> #changing names to match user dropdown below
  left_join(labelset, by = "Label.code") |> #adding labelset to coralnet data
  filter(!Label.code %in% exclude_codes)#excluding codes not included in benthic cover

#Setting up site and date choices for user, based on options available in dataset
site_choices <- sort(unique(benthic_clean$Site))
date_choices <- sort(unique(benthic_clean$Date))
date_choices <- setNames(
  as.character(date_choices),
  format(date_choices, "%b %d, %Y")   #changing date formatting so it's nicer for the label the user sees
)

#Setting table of site coordinates
sites <- tribble(
  ~Site,             ~lat,      ~lon,
  "Hanauma Bay",     21.268538,	-157.695141,
  "Electric Beach",  21.351979,	-158.131955,
  "Ko Olina North",  21.3391,	  -158.13505,
  "Magic Island",    21.28175,	-157.85071,
  "Maunalua Bay",    21.26997,	-157.7441,
  "Three Tables",    21.64603,	-158.06706
)

#Analyzing broad benthic summaries per site (to display on map)
#calculating % grazeable substrate (turf, CCA, macroalgae) and % coral cover 
site_stats <- benthic_clean |>
  group_by(Site, Date, Name) |>
  summarise(
    total_points = n(),
    grazeable_points = sum(Subcategory %in% c("Turf", "CCA", "Macroalgae"), na.rm = TRUE), #number of coral points per quadrat
    coral_points = sum(Subcategory == "Coral", na.rm = TRUE), #number of coral points per quadrat
    .groups = "drop"
  ) |>
  mutate(
    Grazeable = grazeable_points / total_points * 100, #calculating grazeable % per quadrat
    Coral     = coral_points / total_points * 100 #calculating % coral cover per quadrat
  ) |>
  group_by(Site) |>   #calculating average per site / across quadrats
  summarise(
    Grazeable = mean(Grazeable),
    Coral = mean(Coral),
    n.quadrats = n()
  )

#Calculating grazeable substrate proportion/percentage per quadrat (to standardize bite rate):
quad_grazeable <- benthic_clean |>
  group_by(Site, Date, Name, GoPro.ID) |>
  summarise(
    total_points = n(),
    grazeable_points = sum(Subcategory %in% c("Turf", "CCA", "Macroalgae"), na.rm = TRUE), #number of coral points per quadrat
    .groups = "drop"
  ) |>
  mutate(
    grazeable = grazeable_points / total_points * 100, #calculating grazeable % per quadrat
    GoPro = str_to_upper(str_sub(GoPro.ID, 1, 1)), #takes only first letter of GoPro.ID column for new GoPro column that matches with herbivory videos
    Site = str_squish(str_replace_all(Site, " ", ".")) #adding period in site name to match video data
  ) 

#Calculating bite rates per site, to size on map:
#making a dataframe with each video and its analyzeable duration
herb_videos <- herbivory |>
  group_by(Site, Video_date, GoPro) |>
  summarise(duration = first(as.numeric(Analyzeable_duration)) / 3600,
            .groups = "drop")
#calculating total bites per video:
bites <- herbivory |>
  count(Site, Video_date, GoPro, name = "bites")
#calculating bite rates (bites/hr) per video, combining total bites + analyzeable duration:
bite_rates <- herb_videos |>
  left_join(bites, by = c("Site", "Video_date", "GoPro")) |>
  left_join(quad_grazeable, by = c("Site", "GoPro")) |>
  mutate(bites_per_hr = (bites / duration)) |> #dividing total bite number by duration for bites/hr
  mutate(bite_rate_std = ((bites / duration) * (grazeable/100))) |>  #standardizing by grazeable substrate
  mutate(Site = str_squish(str_replace_all(Site, "[._]", " "))) 

#Adding site benthic summary stats and bite rates to sites tribble to map
sites_map <- sites |> 
  left_join(site_stats, by = "Site") |>
  left_join(bite_rates, by = "Site")

#Making table of the "top biters" -- fish with the highest bite numbers
top_biters <- herbivory |>
  filter(!is.na(Fish_ID)) |>
  count(Fish_family, Fish_ID, name = "bites", sort = TRUE) |>
  slice_head(n = 6) |>  #showing top 6 species
  mutate(across(where(is.character), ~ str_replace(.x, "Scaridae", "Labridae"))) |> #fixing old family name Scaridae to be aligned with new Labridae designation
  mutate(photo = paste0(str_replace_all(Fish_ID, "[^A-Za-z]+", "_"), ".jpg"), #adding column for photo name for these top fish
         Fish_ID = factor(Fish_ID, levels = rev(Fish_ID)))   #setting factor to keep the top biter at the top of the plot


### SETTING UP UI:

ui <- navbarPage(
  title = "Benthic Herbivory Project", #title of entire app
  theme = bs_theme(bootswatch = "lux"), #using my go-to theme lux again
  # PAGE 1 = map of study sites
  tabPanel(
    "Bite Rates on O‘ahu",
    fluidRow(
      column(3,
             h3("Project overview"), #adding a column for my project overview
             p("On coral reefs, herbivorous fish are the first line of defense preventing macroalgae from smothering coral. This project uses underwater video and benthic surveys to measure how much fish grazing is happening at sites on Oʻahu, and what the reef surface looks like beneath it. Explore fish grazing rates, benthic composition, and the fish species taking the most bites."),
             p("Methods: GoPro videos were used to count bite rates by fish within 1m², where focal benthic quadrats were analyzed and annotated in CoralNet."),
             p("Map (right): Circle size shows the bite rate (standardized by proportion of grazeable substrate) at each site. Click a site to see its
         bite rate, grazeable substrate, and coral cover.")
      ),
      column(9, leafletOutput("map", height = "80vh"))
    )
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
        p("Explore benthic composition across O‘ahu: pick sites and dates on the left, switch between
     individual quadrats and site/date summary, and view the results below as a plot or a table."),
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
  ),
  tabPanel(
    "Top Biters",
    h3("Fish species with the most bites"),
    fluidRow(
      column(6, plotOutput("top_plot", height = "450px")),
      #adding photos of top 6 biting species for users that don't know scientific names
      column(6,
             fluidRow(lapply(seq_len(nrow(top_biters)), function(i) {
               column(4,
                      img(src = file.path("fish", top_biters$photo[i]), width = "100%"),
                      p(em(as.character(top_biters$Fish_ID[i])), br(), top_biters$bites[i], "bites")
               )
             })),
             p("Photo credits: Rickard Zerpe, A. nigrofuscus & A. olivaceus; Steve Mohundro, T. duperrey; Diego Delso, S. psittacus; Don Loarie, S. balteata; Chris Sulots, A. nigroris")
      )
    )
  )
)



### SETTING UP SERVER:

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
    leaflet(sites_map) |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~scales::rescale(bite_rate_std, to = c(10, 40)), #resizing circles such that bigger = more grazing
        popup = ~paste0("Site: ", Site, "<br>", #pasting the site name
                        "Bite rate: ", round(bite_rate_std, 1), " bites/hr", "<br>", #printing bite rate
                        "Grazeable substrate: ", round(Grazeable, 1),"%", "<br>", #and grazeable substrate (%)
                        "Coral cover: ", round(Coral, 1),"%"), #and coral cover (%)
        color = "white", weight = 2,
        fillColor = "#0096C7", fillOpacity = 0.9
      )
  })
  #adding simple plot of top biters (fish species with most bites)
  output$top_plot <- renderPlot({
    ggplot(top_biters, aes(x = bites, y = Fish_ID, fill = Fish_family)) +
      geom_col() +
      scale_fill_brewer(palette = "Set2") + 
      labs(x = "Total bites", y = NULL, fill = "Family") +
      theme_minimal(base_size = 16)
  })
  
}


### DEPLOYING SHINY APP
shinyApp(ui = ui, server=server)

