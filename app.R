library(shiny)
library(ggvis)

pop <- read.csv("pop.csv")


fert <- read.csv("fert.csv")

life <- read.csv("life.csv")

reg <- read.csv("Continent.csv")

#year <- paste("X", as.character(2012), sep = "") ##
# oneyear <- data.frame(pop["Country.Name"], pop[year], fert[year], life[year])
# oneyear <- merge(x = oneyear, y = reg, by = "Country.Name", all.x = TRUE)
# colnames(oneyear) <- c('Country', 'Pop', 'Fert', 'Life', 'Region')
# oneyear <- na.omit(oneyear)


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
# can do tooltip with plotly, ggplot
# try map with pop/fertility/life exp instead of mtcars

server <- function(input, output) {
  # year<- reactive({
  #   paste("X", as.character(input$year), sep = "")
  #   })
  
  year <- "X2000" ##
  oneyear <- data.frame(pop["Country.Name"], pop[year], fert[year], life[year])
  oneyear <- merge(x = oneyear, y = reg, by = "Country.Name", all.x = TRUE)
  colnames(oneyear) <- c('Country', 'Pop', 'Fert', 'Life', 'Region')
  oneyear <- na.omit(oneyear)
  
  
  input_size <- 20
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- mtc[mtc$Country == x$Country, ]
    #paste0(names(row), ": ", format(row), collapse = "<br />")
    row$Country
  }
  
  mtc <- oneyear
  
  mtc %>% 
    ggvis(~Life, ~Fert, key := ~Country, size = ~Pop,fill = ~Region) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points() %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}

shinyApp(ui = ui, server = server)