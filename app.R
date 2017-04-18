library(shiny)
library(ggvis)

pop <- read.csv("pop.csv")

fert <- read.csv("fert.csv")

life <- read.csv("life.csv")

reg <- read.csv("Continent.csv")

ui <- fluidPage(
  headerPanel('Interactive with ggvis'),
  sidebarPanel(
    sliderInput("year", "Year", 1960, 2014, 2000, 1, round = T)
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)

server <- function(input, output) {
  # year<- reactive({
  #   paste("X", as.character(input$year), sep = "")
  #   })
  #reactive({
  
  #year <- "X2000" ## Need to figure out how to ge this reactively
  # oneyear <- data.frame(pop["Country.Name"], pop[year], fert[year], life[year])
  # oneyear <- merge(x = oneyear, y = reg, by = "Country.Name", all.x = TRUE)
  # colnames(oneyear) <- c('Country', 'Pop', 'Fert', 'Life', 'Region')
  # oneyear <- na.omit(oneyear)
  
  oneyear <- reactive({
    year <- paste("X", as.character(input$year), sep = "")
    oneyear <- data.frame(pop["Country.Name"], pop[year], fert[year], life[year])
    oneyear <- merge(x = oneyear, y = reg, by = "Country.Name", all.x = TRUE)
    colnames(oneyear) <- c('Country', 'Pop', 'Fert', 'Life', 'Region')
    na.omit(oneyear)
  })
  
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- oneyear[oneyear$Country == x$Country, ]
    #paste0(names(row), ": ", format(row), collapse = "<br />")
    row$Country
  }
  
  
  
  oneyear %>% 
    ggvis(~Life, ~Fert, key := ~Country, size = ~Pop,fill = ~Region) %>%
    add_axis("x", title = "Life Expectancy") %>%
    add_axis("y", title = "Fertility") %>%
    add_tooltip(all_values, "hover") %>%
    layer_points() %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}

shinyApp(ui = ui, server = server)