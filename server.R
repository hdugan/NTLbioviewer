# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(EDIutils)

loadLTERinverts <- function() {
  # North Temperate Lakes LTER: Benthic Macroinvertebrates 1981 - current
  
  revision = list_data_package_revisions(scope = 'knb-lter-ntl', identifier = "11", filter = "newest")
  packageid = paste0('knb-lter-ntl.11.', revision)
  res = read_data_entity_names(packageid)
  raw = read_data_entity(packageId = packageid, entityId = res$entityId[1])
  LTERinverts = read_csv(file = raw)
}

loadLTERcrayfish<- function() {
  # North Temperate Lakes LTER: Crayfish Abundance 1981 - current
  
  revision = list_data_package_revisions(scope = 'knb-lter-ntl', identifier = "3", filter = "newest")
  packageid = paste0('knb-lter-ntl.3.', revision)
  res = read_data_entity_names(packageid)
  raw = read_data_entity(packageId = packageid, entityId = res$entityId[1])
  LTERcrayfish = read_csv(file = raw)
}

loadLTERfishrichness<- function() {

  revision = list_data_package_revisions(scope = 'knb-lter-ntl', identifier = "245", filter = "newest")
  packageid = paste0('knb-lter-ntl.245.', revision)
  res = read_data_entity_names(packageid)
  raw = read_data_entity(packageId = packageid, entityId = res$entityId[1])
  TERfishrichness = read_csv(file = raw)
}

loadLTERfishabundance<- function() {
  # North Temperate Lakes LTER: Fish Abundance 1981 - current
  
  revision = list_data_package_revisions(scope = 'knb-lter-ntl', identifier = "7", filter = "newest")
  packageid = paste0('knb-lter-ntl.7.', revision)
  res = read_data_entity_names(packageid)
  raw = read_data_entity(packageId = packageid, entityId = res$entityId[1])
  TERfishabundance = read_csv(file = raw)
}



# LTERinverts = loadLTERinverts() |>
#   mutate(lakeid = if_else(lakeid == 'sp', 'SP', lakeid)) |> 
#   filter(!is.na(number_indiv), !is.na(description)) |> 
#   rename(spname = description) |> 
#   group_by(lakeid, year4, spname) |> 
#   summarise(n = sum(number_indiv)) |> 
#   mutate(value = 1) |> 
#   mutate(item = 'inverts')

LTERcrayfish = loadLTERcrayfish() |> 
  filter(!is.na(total_caught), !is.na(spname)) |> 
  mutate(CPUE = total_caught/effort) |> 
  mutate(item = 'rusty') |> 
  filter(spname != 'CRAYFISH') |> 
  filter(gearid == 'CRAYTR') |> 
  mutate(gearid = case_when(gearid == 'BSEINE' ~ 'Beach Seine (ended 2019)',
                            gearid == 'CRAYTR' ~ 'Crayfish Trap',
                            gearid == 'ELFISH' ~ 'Electrofishing',
                            gearid == 'FYKNET' ~ 'Fyke Net',
                            gearid == 'TRAMML' ~ 'Trammel Net',
                            gearid == 'VGN' ~ 'Vertical Gill Net'))
  

# LTERfishrichness = loadLTERfishrichness()

LTERfishabundance = loadLTERfishabundance() |> 
  filter(!is.na(total_caught), !is.na(spname)) |> 
  mutate(CPUE = total_caught/effort) |> 
  mutate(item = 'fish') |> 
  filter(spname != 'UNIDENTIFIED') |> 
  filter(!gearid %in% c('ESHOCK','MINNOW')) |> 
  mutate(gearid = if_else(grepl('VGN', gearid), 'VGN', gearid)) |> 
  mutate(gearid = case_when(gearid == 'BSEINE' ~ 'Beach Seine (ended 2019)',
                            gearid == 'CRAYTR' ~ 'Crayfish Trap',
                            gearid == 'ELFISH' ~ 'Electrofishing',
                            gearid == 'FYKNET' ~ 'Fyke Net',
                            gearid == 'TRAMML' ~ 'Trammel Net',
                            gearid == 'VGN' ~ 'Vertical Gill Net')) |> 
  group_by(spname) |> 
  filter(n() > 25) 



matchtable = data.frame(vars =  c('rusty',
                                  'fish'),
                        names = c('Crayfish',
                                  'Fish'),
                        url = c(#rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=11',1),
                          rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=3',1),
                          rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=7',1)))

lakelocations = data.frame(Lake = c("Allequash Lake", "Big Muskellunge Lake", 
                                    "Crystal Bog", "Crystal Lake", "Sparkling Lake", "Trout Bog", 
                                    "Trout Lake", "Lake Mendota", "Lake Monona", "Lake Wingra", "Fish Lake"),
                           Lat = c(46.038317, 46.021067, 46.007583, 46.00275, 46.007733, 
                                   46.04125, 46.029267, 43.09885, 43.06337, 43.05258, 43.28733), 
                           Long = c(89.620617, -89.611783, -89.606183, -89.612233, -89.701183, 
                                    -89.686283, -89.665017, -89.40545, -89.36086, -89.42499, 
                                    -89.65173))

allLTER = LTERcrayfish |> 
  bind_rows(LTERfishabundance) |>
  mutate(lakename = case_when(lakeid == 'AL' ~ 'Allequash',
                              lakeid == 'BM' ~ 'Big Musky',
                              lakeid == 'CR' ~ 'Crystal',
                              lakeid == 'CB' ~ 'Crystal Bog',
                              lakeid == 'SP' ~ 'Sparkling',
                              lakeid == 'TR' ~ 'Trout',
                              lakeid == 'TB' ~ 'Trout Bog',
                              lakeid == 'ME' ~ 'Mendota',
                              lakeid == 'MO' ~ 'Monona',
                              lakeid == 'FI' ~ 'Fish',
                              lakeid == 'WI' ~ 'Wingra')) |> 
  filter(year4 != 1981)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Get full variable name from matchtable
  varname <- reactive({
    a <- matchtable %>% dplyr::filter(names == input$input.vars) %>% dplyr::pull(vars)
    return(a)
  })
  
  # Get Lake names
  getLakes = reactive({
    useitem = matchtable |> filter(names == input$input.vars) |> pull(vars)
    b = allLTER |> filter(item == varname())
    b = c('All Lakes', unique(b$lakename))
    return(b)
  })
  
  # ## used to find lakes dependent on variable
  # output$dataLakes <- renderUI({
  #   selectInput("lakes", "select lakes", choices = getLakes(), selected = NA)
  # })
  
  ## used to find lakes dependent on variable
  output$dataLakes <- renderUI({
    checkboxGroupInput("lakes", "Select lakes", choices = getLakes(), selected = 'All Lakes')
  })
  
  allLTER_filtered_1 <- reactive({ 
    d = allLTER %>% 
      filter(item == varname())
    if (input$lakes[1] != 'All Lakes') {
      d = d |> filter(lakename %in% input$lakes) #unlist(strsplit(input$lakes, split=" "))
    }
    return(d)
  })
  
  # Get Species names
  getSpecies = reactive({
    d = allLTER_filtered_1()
    d = sort(unique(d$spname))
    return(d)
  })

  ## used to find species dependent on variable
  output$dataSpecies <- renderUI({
    selectInput("species", "Select species", choices = getSpecies(), selected = 'WALLEYE')
  })
  
  allLTER_filtered_2 <- reactive({ 
    d = allLTER %>% 
      filter(item == varname()) |> 
      filter(spname == input$species)
    if (input$lakes[1] != 'All Lakes') {
      d = d |> filter(lakename %in% input$lakes)
    }
    return(d)
  })
  
  # Get gear names
  getGear = reactive({
    d = allLTER_filtered_2()
    d = sort(unique(d$gearid))
    return(d)
  })
  
  ## used to find gear dependent on variable
  output$dataGear <- renderUI({
    selectInput("gear", "Select gear", choices = getGear(), selected = 'Electrofishing')
  })
  
  # Get url of dataset name from matchtable
  output$urlname <- renderUI({
    a <- matchtable %>% dplyr::filter(names == input$input.vars) %>% dplyr::pull(url)
    url <- a("EDI Dataset Page", href = a, target="_blank")
    tagList("The full dataset and citation for this dataset can be found at: ", url)
  })
  
  renderUI({
    paste("URL link:", url)
  })
  
  
  # OUTPUT TESTING
  # output$testvar = renderText(print(allLTER_filtered_1()))
  # output$testvar = renderText(print(names(allLTER_filtered_1())))
  # output$testvar = renderText(print(nrow(allLTER_filtered_1())))
  
  
  allLTER_filtered_3 <- reactive({ 
    d = allLTER %>% 
      filter(item == varname()) |> 
      filter(spname == input$species) |> 
      filter(gearid == input$gear) 
    if (input$lakes[1] != 'All Lakes') {
      d = d |> filter(lakename %in% input$lakes)
    }
    return(d)
  })
  
  # Plots 
  plotInput <- reactive({ 
    
    p = ggplot(allLTER_filtered_3())
    
    if(input$lakes[1] == 'All Lakes' | length(input$lakes) > 1) {
      p = p + facet_wrap(~lakename)
    }

    if('Free y-axis' %in% input$scales) {
        p = p + facet_wrap(~lakename, scales = "free_y")
    }
    
    p = p +
      geom_col(aes(x = year4, y = CPUE), size = 0.5, fill = '#f0ce3a') + #
      xlim(1981, as.numeric(format(Sys.Date(), "%Y"))) +
      ylab('CPUE')

    if ('Log y-axis' %in% input$scales) {
      p = p + scale_y_log10()
    }
    p = p +
      # ylab(input$input.vars) +
      theme_minimal(base_size = 14, base_family = 'Helvetica') +
      theme(axis.title.x = element_blank(),
            legend.position = 'bottom',
            panel.grid = element_line(colour = "grey80"))
  })
  
  # Display plot
  output$distPlot <- renderPlot({
    print(plotInput())
  })
  
  # Display plot
  # output$verb <- renderText({
  #   
  #   if(input$input.lake == 'All northern lakes') {
  #     allLTER_fil = allLTER_filtered() %>% filter(lakename %in% c('Allequash','Big Musky','Crystal','Crystal Bog','Sparkling','Trout','Trout Bog'))
  #   } else if (input$input.lake == 'All southern lakes') {
  #     allLTER_fil = allLTER_filtered() %>% filter(lakename %in% c('Mendota','Monona','Fish','Wingra'))
  #   } else {
  #     allLTER_fil = allLTER_filtered() %>% filter(lakename == input$input.lake)
  #   }
  #   species = sort(unique(allLTER_fil$spname))
  #   
  #   paste0('Species: ', paste(species, collapse = ', '))
  # })
  
  # Download plot
  output$downloadImage = downloadHandler(
    filename = function() {paste0(input$input.lake,'_',varname(),'_',input$input.depth,'m.png')},
    content = function(file) {
      ggsave(file, plot = plotInput() + theme_bw(base_size = 8), width = 6, height = 4, units = 'in', dpi = 500,
             device = "png")
    })
  
  # # Make map 
  map = leaflet(data = lakelocations) %>%
    addTiles() %>%
    addMarkers(~Long, ~Lat, popup = ~as.character(Lake), label = ~as.character(Lake)) %>%
    setView(-89.6, 44.5, zoom = 6)
  output$myMap = renderLeaflet(map)
  
  
})
