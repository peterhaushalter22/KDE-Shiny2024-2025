library(shiny)
library(readxl)
library (ks)
library (rgl)
library (misc3d)
library (dplyr)
library (htmlwidgets)
library(ggplot2)
library(pandoc)
library(rjson)  
library(shinyFiles)
library(zip)
library(shinyWidgets)

#Remeber, x = long, y = lat

COLORS = c("red","firebrick2", "orange", 
           "brown","burlywood2", "tan3",
           "yellow","gold","darkgoldenrod3",
           "pink","deeppink", "bisque", 
           "blue", "cyan","navy",
           "green","darkgreen","lightgreen",
           "purple","violet", "maroon",
           "gray", "darkgray", "black")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title, centered
  div(
    style = 
      "height: 80px; background-color: cyan; width: 100%;",
    tags$h1(
      "Sezarc: KDE", 
      style = 
        "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
    )
  ),
  #Spacing
  div(style = "height: 40px; background-color: white;"),

  #Side bar for all the input elements, including drop downs, text inputs, and sliders  
  
    sidebarLayout(
        #Beginning of side bar panel
        sidebarPanel(
          position = "left",
          width = 6,
          #File input selection
          fileInput(inputId = "file", label = "Choose Excel file", multiple = TRUE),
          #shinyDirButton('folder', 'Click here to select a folder to hold the output files', 'Please select a folder', FALSE),
          div(
            style = 
              "height: 10px;"),
          div(
            tags$h4(
              "Column Selections",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs, correspond to a dropdown that selects from the spreadsheet columns
          uiOutput("sheet_select"),
          uiOutput("name_select"),
          uiOutput("x_select"),
          uiOutput("y_select"),
          uiOutput("z_select"),
          
          #Adds a solid line between UI elements using html
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Bandwidth Stages",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to slider elements 
          uiOutput("scaling_slider"),
          uiOutput("stages_slider"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Bandwidth Selections",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to checkboxes for the different bandwidth pilots
          uiOutput("samse"),
          uiOutput("unconstr"),
          uiOutput("dscalar"),
          uiOutput("dunconstr"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Optional Factors",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to checkboxes for 2D and Noise
          uiOutput("twoD"),
          uiOutput("noise"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Required: Trial Types",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to checkboxes for what type of 3D KDE output you want, single entity or two entities
          uiOutput("single_trial"),
          uiOutput("double_trial"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Depth Selections",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to text inputs for depth and enclosure depth
          #Also an html element that tells you the min and max you can enter (unsure if actually implemented)
          htmlOutput("min_max"),
          uiOutput("depth_sections"),
          uiOutput("enclosure_depth"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          div(
            tags$h4(
              "Contours",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          
          #UI outputs that correspond to a text input for contours, one string, comma separated numbers, no spaces
          uiOutput("contours"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          
          div(
            tags$h4(
              "Contour Colors Animal 1:",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          #colors
          #UI outputs that correspond to drop down selections for contour colors for animal 1
          uiOutput("animal1_color1"),
          uiOutput("animal1_color2"),
          # uiOutput("animal1_color3"),
          # uiOutput("animal1_color4"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          
          div(
            tags$h4(
              "Contour Colors Animal 2:",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          #colors
          #UI outputs that correspond to drop down selections for contour colors for animal 2
          uiOutput("animal2_color1"),
          uiOutput("animal2_color2"),
          # uiOutput("animal2_color3"),
          # uiOutput("animal2_color4"),
          
          div(tags$h4(
            style =
              "border-top: 1px solid #000000; height: 10px;"
          )
          ),
          
          
          #Legend for the colors, displaying the color each color string will actually look like on the plot
          div(
            tags$h4(
              "Color Legend",
              style =
                "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            )
          ),
          #tempvar <- COLORS[3],
          div(
            tags$span(
              "red",
              style = "font-size: 20px; color: red; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "firebrick2",
              style = "font-size: 20px;color: rgb(238,44,44); position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
              tags$span(
                "orange",
                style = "font-size: 20px;color: orange; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
              ),
          style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
          
          div(
            tags$span(
              "brown",
              style = "font-size: 20px;color: brown; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "burlywood2",
              style = "font-size: 20px;color: rgb(238,197,145); position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "tan3",
              style = "font-size: 20px;color: rgb(205,133,63); position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          div(
            tags$span(
              "yellow",
              style = "font-size: 20px;color: yellow; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "gold",
              style = "font-size: 20px;color: gold; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "goldenrod3",
              style = "font-size: 20px;color: rgb(205,155,29); position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          div(
            tags$span(
              "pink",
              style = "font-size: 20px;color: pink; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "deeppink",
              style = "font-size: 20px;color: deeppink; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "bisque",
              style = "font-size: 20px;color: bisque; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          div(
            tags$span(
              "blue",
              style = "font-size: 20px;color: blue; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "cyan",
              style = "font-size: 20px;color: cyan; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "navy",
              style = "font-size: 20px;color: navy; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          div(
            tags$span(
              "green",
              style = "font-size: 20px;color: green; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "darkgreen",
              style = "font-size: 20px;color: darkgreen; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "lightgreen",
              style = "font-size: 20px;color: lightgreen; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          
          div(
            tags$span(
              "purple",
              style = "font-size: 20px;color: purple; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "violet",
              style = "font-size: 20px;color: violet; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "maroon",
              style = "font-size: 20px;color: maroon; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
          div(
            tags$span(
              "gray",
              style = "font-size: 20px;color: gray; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "darkgray",
              style = "font-size: 20px;color: darkgray; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            tags$span(
              "black",
              style = "font-size: 20px;color: black; position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
            ),
            style = "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
          ),
          
        ),
        
        # Main panel, where the KDE run and download buttons are, as well as the progress bar
        mainPanel(
          position = "right",
          width = 6,
          #Progress bar
          progressBar(id = "pb", value = 0, title = "KDE Progress:", display_pct = T, total = 100),
          #Run button
          actionButton("action1", "Run KDE"),
          #Switched from downloadLink to download Button
          downloadButton("downloadData", "Download KDE RESULTS"),
        )
    )
)

# Server functions, where the calculations are actually handled as well as the backend for the UI buttons
server <- function(input, output, session) {
  #List of colors used
  COLORS = c("red","firebrick2", "orange", 
             "brown","burlywood2", "tan3",
             "yellow","gold","darkgoldenrod3",
             "pink","deeppink", "bisque", 
             "blue", "cyan","navy",
             "green","darkgreen","lightgreen",
             "purple","violet", "maroon",
             "gray", "darkgray", "black")
  
  #Server level functions used for calculating the KDE
  #Functions all originally used in kde, stuffed into server
  calcKernelVol <- function(fhat, perc) { # Calculates perc% kernel volume
    ct <- contourLevels(fhat, cont=perc, approx=TRUE)
    vol.voxel <- prod(sapply(fhat$eval.points, diff)[1,]) # Calculate volume of single voxel
    no.voxel <- sum(fhat$estimate>ct) # Calculate number of voxels
    vol <- no.voxel*vol.voxel # Calculate total volume as product
    return(vol) }
  
  calcIntersect <- function(fhat1, fhat2, perc) { # Calculates volume of intersection of perc% volumes
    ct1 <- contourLevels(fhat1, cont=perc, approx=TRUE) 
    ct2 <- contourLevels(fhat2, cont=perc, approx=TRUE) 
    vol.voxel <- prod(sapply(fhat1$eval.points, diff)[1,]) 
    no.voxel <- sum(fhat1$estimate>ct1 & fhat2$estimate>ct2) 
    intersect <- no.voxel*vol.voxel
    return(intersect) }
  
  genLabel <- function(m, n, pilot) { # Generate label for KDE settings
    return(paste("M",m,",N",n,",",pilot,sep="")) }
  
  genBounds <- function(data1, data2, if2D) { # Generate bounds for two volumes
    data <- rbind(data1, data2)
    mins <- c()
    maxs <- c()
    if(if2D) {
      mins <- c(min(data$X), min(data$Y))
      maxs <- c(max(data$X), max(data$Y)) }
    else {
      mins <- c(min(data$X), min(data$Y), min(data$Z))
      maxs <- c(max(data$X), max(data$Y), max(data$Z)) }
    bounds <- c(mins, maxs)
    return(bounds) }
  
  KDETrialSingle <- function(data, if2D, percs, m, n, pilot, imgDir, colorSingle, opacitySingle, display2D) { # Tries KDE with given settings for single volumes
    band <- Hpi(data, nstage=n, pilot=pilot)*m # Generate bandwidth matrix
    fhat <- kde(data, H=band) # Generate KDE
    if(typeof(fhat$x) == "list") { fhat$x  <- data.matrix(fhat$x) } # Convert data type to avoid sample size limit
    
    colorIndexOffset <- 0
    
    if(if2D) { # Create and save 2D plot
      colorIndexOffset <- 1
      imgName <- paste(imgDir,"/",genLabel(m,n,pilot),".png",sep="")
      png(imgName)
      print(paste("imgName", imgName, sep=" "))
      plot(fhat, display=display2D, cont=percs, asp=1, col=colorSingle)
      dev.off()
    }
    else { # Create and save 3D widget
      imgName <- paste(imgDir,"/",genLabel(m,n,pilot),".html",sep="")
      plot(fhat, display="rgl", cont=percs, asp=1, col=colorSingle, alpha=opacitySingle)
      scene <- scene3d()
      saveWidget(rglwidget(scene), file=imgName)
      clear3d(type = "all")
      rgl.close() }
    vols <- vector()
    
    # Write color key to a file
    color_key_file <- file(paste(imgDir, "/", "key.txt", sep=""))
    reversed_percs <- rev(percs)
    key_entries = c()
    for(i in 1:length(percs)){
      key_entries <- c(key_entries, paste(reversed_percs[i], "% Contour: ", colorSingle[i+colorIndexOffset]))
    }
    
    writeLines(key_entries, color_key_file)
    close(color_key_file)
    
    for(perc in percs) { vols <- append(vols, calcKernelVol(fhat,perc)) } # Store calculated volumes
    return(vols) }
  
  KDETrialDouble <- function(data1, data2, if2D, percs, m, n, pilot, imgDir, colorDouble1, colorDouble2, opacityDouble1, opacityDouble2, display2D, name1, name2) { # Tries KDE with given settings for two volumes
    band1 <- Hpi(data1, nstage=n, pilot=pilot)*m
    band2 <- Hpi(data2, nstage=n, pilot=pilot)*m
    bounds <- genBounds(data1, data2, if2D) # Generate outer bounds for KDE
    dims <- 3
    if(if2D) { dims <- 2 }
    fhat1 <- kde(data1, H=band1, xmin=bounds[1:dims], xmax=bounds[(dims+1):(dims*2)])
    if(typeof(fhat1$x) == "list") { fhat1$x  <- data.matrix(fhat1$x) }
    fhat2 <- kde(data2, H=band2, xmin=bounds[1:dims], xmax=bounds[(dims+1):(dims*2)])
    if(typeof(fhat2$x) == "list") { fhat2$x  <- data.matrix(fhat2$x) }
    colorIndexOffset <- 0
    # handle 3D
    if(!if2D) {
      imgName <- paste(imgDir,"/",genLabel(m,n,pilot),".html",sep="")
      plot(fhat1, display="rgl", cont=percs, asp=1, col=colorDouble1, alpha=opacityDouble1)
      plot(fhat2, display="rgl", cont=percs, asp=1, add=TRUE, col=colorDouble2, alpha=opacityDouble2)
      scene <- scene3d()
      saveWidget(rglwidget(scene), file=imgName)
      
      clear3d(type = "all")
      
      rgl.close() }
    # handle 2D
    else{
      colorIndexOffset <- 1
      imgName <- paste(imgDir, "/", genLabel(m,n,pilot), ".png", sep="")
      png(imgName)
      plot(fhat1, display=display2D, cont=percs, asp=1, col=colorDouble1, alpha=0.5)
      plot(fhat2, display=display2D, cont=percs, asp=1, add=TRUE, col=colorDouble2, alpha=0.5)
      dev.off()
    }
    
    color_key_file <- file(paste(imgDir, "/", "key.txt", sep=""))
    reversed_percs <- rev(percs)
    key_entries = c()
    
    for(i in 1:length(percs)){
      key_entries <- c(key_entries, paste(name1, " ", reversed_percs[i], " % Contour: ", colorDouble1[i + colorIndexOffset]))
      key_entries <- c(key_entries, paste(name2, " ", reversed_percs[i], " % Contour: ", colorDouble2[i + colorIndexOffset]))
    }
    
    writeLines(key_entries, color_key_file)
    close(color_key_file)
    
    vols <- vector()
    for(perc in percs) {
      vols <- append(vols, calcKernelVol(fhat1, perc))
      vols <- append(vols, calcKernelVol(fhat2, perc))
      vols <- append(vols, calcIntersect(fhat1, fhat2, perc)) }
    return(vols) }
  
  KDESingle <- function(data, if2D, percs, ms, ns, pilots, imgDir, colorSingle, opacitySingle, display2D) { # Performs KDE for single volume with all combinations of settings
    volumes <- data.frame(matrix(ncol=1+length(percs), nrow=0))
    colnames(volumes) <- c("Label", paste(percs))
    for(m in ms) { # Iterate through options for bandwidth optimization/selection
      for(n in ns) {
        for(pilot in pilots) {
          vols <- KDETrialSingle(data, if2D, percs, m, n, pilot, imgDir, colorSingle, opacitySingle, display2D)
          row <- data.frame(c(genLabel(m,n,pilot), as.list(vols))) # Construct row for output matrix
          colnames(row) <- c("Label", paste(percs)) # Rename columns for merging
          volumes <- rbind(volumes, row) }}}
    return(volumes) }
  
  KDEDouble <- function(data1, data2, if2D, percs, ms, ns, pilots, imgDir, colorDouble1, colorDouble2, opacityDouble1, opacityDouble2, display2D, name1, name2) { # Performs KDE for two volumes with all combinations of settings
    volumes <- data.frame(matrix(ncol=1+3*length(percs), nrow=0))
    prefixes <- c("V1", "V2", "V&")
    colnames <- c(outer(prefixes, paste(percs), paste))
    volnames <- c(outer(prefixes, paste(percs), paste))
    colnames(volumes) <- c("Label", volnames)
    for(m in ms) {
      for(n in ns) {
        for(pilot in pilots) {
          vols <- KDETrialDouble(data1, data2, if2D, percs, m, n, pilot, imgDir, colorDouble1, colorDouble2, opacityDouble1, opacityDouble2, display2D, name1, name2)
          row <- data.frame(c(genLabel(m,n,pilot), as.list(vols)))
          colnames(row) <- c("Label", volnames)
          #colnames(row) <- NA
          volumes <- rbind(volumes, row) }}}
    return(volumes) }
  
  prepData <- function(raw, name, nameCol, xCol, yCol, zCol, zIncr, ifNoise, if2D) { # Transforms the data into a usable form
    data <- raw[raw[nameCol] == name,] # Select only rows corresponding to desired animal
    if(if2D) {
      data <- select(data, xCol, yCol)
      colnames(data) <- c("X", "Y") }
    else {
      data <- select(data, xCol, yCol, zCol) # Select coordinate columns as X,Y,Z
      colnames(data) <- c("X", "Y", "Z") } # Rename columns to X,Y,Z
    data <- na.omit(data) # Remove rows with missing data
    # Adding noise here. runif(n, min, max) produces a uniform sample of size n between the values of min and max
    if(ifNoise & !if2D) { data[,3] <- data[,3] + runif(nrow(data), -zIncr, 0) } # Add noise to Z
    return(data) }
  
  run <- function(path, sheet, nameCol, xCol, yCol, zCol, dir, out_file, excluded, zIncr, ifNoise, ifSingle, ifDouble, if2D, percs, ms, ns, pilots, colorSingle, colorDouble1, colorDouble2, opacitySingle, opacityDouble1, opacityDouble2, display2D) { # Runs program
    raw <- read_excel(path, sheet=sheet)
    names <- unique(raw[nameCol]) # Get unique names for iterating
    colnames(excluded) <- nameCol # Rename columns for processing
    names <- anti_join(names, excluded, by=nameCol) # Remove excluded from names
    
    totalRunsSingle <- nrow(names)
    totalRunsDouble <- choose(nrow(names), 2)
    oneRun <- 1
    
    if(ifSingle && ifDouble) {
      totalRuns <- totalRunsSingle + totalRunsDouble
    } else if (ifSingle) {
      totalRuns <- totalRunsSingle
    } else {
      totalRuns <- totalRunsDouble
    }
    
    if(! dir.exists(dir)) { dir.create(dir) }
    # Add background color for 2D plots
    if(if2D) {
      colorSingle <- c("white", colorSingle)
      colorDouble1 <- c("white", colorDouble1)
      colorDouble2 <- c("white", colorDouble2)
    }
    if(ifSingle) {
      total_out_file_single <- (paste(dir, "/Cumulative-Tables/output_total_single.csv", sep=""))
      print(paste("total runs", totalRuns, sep = ": "))
      print(oneRun)
      for(i in 1:nrow(names)) {
        name <- as.character(names[i,])
        data <- prepData(raw, name, nameCol, xCol, yCol, zCol, zIncr, ifNoise, if2D) # Preprocess data
        imgDir <- paste(dir,"/Single-Trial-Results/",name,sep="")
        if(! dir.exists(imgDir)) { dir.create(imgDir) }
        volumes <- KDESingle(data, if2D, percs, ms, ns, pilots, imgDir, colorSingle, opacitySingle, display2D) # Perform calculations
        print(paste(name,":",sep="")) # Output results
        print(volumes)
        out_file_name = paste(dir, (paste("Single-Trial-Results/", name, "-", "output.csv", sep="")), sep="/")
        write.table(name, total_out_file_single, row.names=FALSE, sep=", ", append = TRUE , col.names=FALSE, quote=TRUE, na="NA")
        write.table(volumes, out_file_name, row.names=TRUE, sep=", ", col.names=TRUE, quote=TRUE, na="NA")
        write.table(volumes, total_out_file_single, row.names=FALSE, sep=", ", append = TRUE , col.names=TRUE, quote=TRUE, na="NA")
        if (oneRun == totalRuns) {
          updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = 99)
        } else {
          updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = (oneRun/totalRuns) * 100)
        }
        oneRun <- oneRun + 1
      }}
    if(nrow(names) > 1 & ifDouble) {
      total_out_file_double <- (paste(dir, "/Cumulative-Tables/output_total_double.csv", sep=""))
      print(paste("total Runs", totalRuns, sep = ": "))
      for(i in 1:(nrow(names)-1)) {
        name1 <- as.character(names[i,])
        data1 <- prepData(raw, name1, nameCol, xCol, yCol, zCol, zIncr, ifNoise, if2D)
        for(j in (i+1):nrow(names)) {
          name2 <- as.character(names[j,])
          data2 <- prepData(raw, name2, nameCol, xCol, yCol, zCol, zIncr, ifNoise, if2D)
          tag <- paste(name1,"&",name2)
          imgDir <- paste(dir,"/Double-Trial-Results/",tag,sep="")
          if(! dir.exists(imgDir)) { dir.create(imgDir) }
          volumes <- KDEDouble(data1, data2, if2D, percs, ms, ns, pilots, imgDir, colorDouble1, colorDouble2, opacityDouble1, opacityDouble2, display2D, name1, name2)
          print(paste(tag,":",sep=""))
          print(volumes)
          nameLabel <- paste(tag,":",sep="")
          write.table("\n", total_out_file_double, row.names=FALSE, sep=", ", append = TRUE , col.names=FALSE, quote=FALSE, na="NA")
          write.table(nameLabel, total_out_file_double, row.names=FALSE, sep=", ", append = TRUE , col.names=FALSE, quote=TRUE, na="NA")
          write.table(volumes, total_out_file_double, row.names=FALSE, sep=", ", append = TRUE , col.names=TRUE, quote=TRUE, na="NA")
          out_file_name = paste(dir, "Double-Trial-Results", (paste(name1, name2, "output.csv", sep="-")), sep="/")
          write.table(volumes, out_file_name, row.names=TRUE, sep=", ", col.names=TRUE, quote=TRUE, na="NA")
          if (oneRun == totalRuns) {
            updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = 99)
          } else { 
            updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = (oneRun/totalRuns) * 100)
          }
          oneRun <- oneRun + 1
        }}}}
  
  
  
  
  #Get Column Names from Excel sheet
  
  #serverVolumes = getVolumes()() # this makes the directory at the base of your computer.
  #observe({
    #shinyDirChoose(input, 'folder', roots=serverVolumes, filetypes=c('', 'txt'))
  #})
  
  #Download handler on the server side 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("KDE-Data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      file.copy("TestZip.zip", file)
      #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
      #session$onSessionEnded(function() { file.remove(file.path(dir(pattern = ".csv"))) })
    }
  )
  
  output$sheet_select <- renderUI({
    req(input$file)
    selectInput("sheet_col", "Sheet Column", choices = excel_sheets(input$file$datapath), label = "Sheet Name") 
  })
  
  # "choices = " Reads the excel for the colnames
  #Select dropdown for the name column
  output$name_select <- renderUI({
    req(input$file)
    selectInput("name_col", "Name Column", choices = colnames(read_excel(input$file$datapath)), label = "Name Column") 
  })
  #Select dropdown for the X column
  output$x_select <- renderUI({
    req(input$file)
    selectInput("x_col", "X Column", choices = colnames(read_excel(input$file$datapath)), label = "X Column")
  })
  #Select dropdown for the Y column
  output$y_select <- renderUI({
    req(input$file)
    selectInput("y_col", "Y Column", choices = colnames(read_excel(input$file$datapath)), label = "Y Column")
  })
  #Select dropdown for the Z column
  output$z_select <- renderUI({
    req(input$file)
    selectInput("z_col", "Z Column", choices = colnames(read_excel(input$file$datapath)), label = "Z Column")
  })
  
  #Sliders for Scaling Factor and Stages in Bandwidth Stages
  
  #Slider for the bandwidth scaling
  output$scaling_slider <- renderUI({
    req(input$file)
    sliderInput("scaling_slider_server", "Scaling Factor (m)", 1, 10, 1, 1)
  })
  #Slider for the bandwidth stages scaling
  output$stages_slider <- renderUI({
    req(input$file)
    sliderInput("stages_slider_server", "Number of Stages in Bandwidth Optimization", 1, 10, 1, 1)
  })
  
  #Check boxes for bandwidth types
  output$samse <- renderUI({
    req(input$file)
    checkboxInput("samse_checkbox", "samse", value = FALSE)
  })
  output$unconstr <- renderUI({
    req(input$file)
    checkboxInput("unconstr_checkbox", "unconstr", value = FALSE)
  })
  output$dscalar <- renderUI({
    req(input$file)
    checkboxInput("dscalar_checkbox", "dscalar", value = FALSE)
  })
  output$dunconstr <- renderUI({
    req(input$file)
    checkboxInput("dunconstr_checkbox", "dunconstr", value = FALSE)
  })
  output$twoD <- renderUI({
    req(input$file)
    checkboxInput("twoD_checkbox", "Is Data 2D?", value = FALSE)
  })
  output$noise <- renderUI({
    req(input$file)
    checkboxInput("noise_checkbox", "Add Noise?", value = FALSE)
  })
  output$single_trial <- renderUI({
    req(input$file)
    checkboxInput("single_trial_checkbox", "Incorporate Single Trial Results?", value = FALSE)
  })
  output$double_trial <- renderUI({
    req(input$file)
    checkboxInput("double_trial_checkbox", "Incorporate Double Trial Results?", value = FALSE)
  })
  
  #Number inputs
  output$min_max <- renderText({
    req(input$file)
    HTML('Mininmum Value: 1, Maximum value: 1000')
  })
  
  output$depth_sections <- renderUI({
    req(input$file)
    numericInput("depth_sections_input", "Number of Depth Sections", 1, 1, 1000, 1)
  })
  output$enclosure_depth <- renderUI({
    req(input$file)
    numericInput("enclosure_depth_input", "Enclosure Depth in Meters", 1, 1, 1000, 1)
  })
  
  #Text input for contours
  output$contours <- renderUI({
    req(input$file)
    textInput("contours_input", "Input Contours", "50")
  })
  
  #Dropdown for animal 1 colors
  output$animal1_color1 <- renderUI({
    req(input$file)
    selectInput("a1c1", label = "Color 1", choices = COLORS, selected = "red") 
  })
  output$animal1_color2 <- renderUI({
    req(input$file)
    selectInput("a1c2", label = "Color 2", choices = COLORS, selected = "orange") 
  })
  # output$animal1_color3 <- renderUI({
  #   req(input$file)
  #   selectInput("a1c3", label = "Color 3", choices = COLORS, selected = "yellow") 
  # })
  # output$animal1_color4 <- renderUI({
  #   req(input$file)
  #   selectInput("a1c4", label = "Color 4", choices = COLORS, selected = "pink") 
  # })
  #Dropdown for animal 2 colors
  output$animal2_color1 <- renderUI({
    req(input$file)
    selectInput("a2c1", label = "Color 1", choices = COLORS, selected = "green")
  })
  output$animal2_color2 <- renderUI({
    req(input$file)
    selectInput("a2c2", label = "Color 2", choices = COLORS, selected = "blue")
  })
  
  # output$animal2_color3 <- renderUI({
  #   req(input$file)
  #   selectInput("a2c3", label = "Color 3", choices = COLORS, selected = "cyan")
  # })
  # output$animal2_color4 <- renderUI({
  #   req(input$file)
  #   selectInput("a2c4", label = "Color 4", choices = COLORS, selected = "purple")
  # })
  
  
  
  #Button to Run KDE
  observeEvent(input$action1, {
    
    #updateTextInput(session, "KDEtext", value = "KDE is Running")
    
    req(input$file)
    print(input$file$datapath)
    options(stringsAsFactors = FALSE)
    sheet <- input$sheet_col
    
    #Dir path is not needed in shiny, because it will all be downloaded in the web from working directory
    #dir <- file.path(tempdir())
    #dir.create(dir)
    
    #works with putting into dir <- getwd()
    #dir <- getwd()
    #dir <- tempdir()
    
    #This bit of code erases the temp files that are stored in a temporary directory at the beginning of each run
    tempFile <- input$file
    #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
    unlink(paste0(normalizePath(tempdir()), "/", "Cumulative-Tables"), recursive = TRUE)
    unlink(paste0(normalizePath(tempdir()), "/", "Double-Trial-Results"), recursive = TRUE)
    unlink(paste0(normalizePath(tempdir()), "/", "Single-Trial-Results"), recursive = TRUE)
    print("below")
    #print(paste0(normalizePath(tempdir()), "/", dir(tempdir())))
    #input$file <- tempFile
    #print("above")
    
    #Make the directory a  temp directory, set it do the current/work directory
    tmp_dir <- tempdir()
    setwd(tmp_dir)
    dir <- getwd()
    print(paste("wd is: ", getwd()))
    print(paste("tempdir is :", dir))
    
    #Reset progress bar to 0 for multiple runs
    updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = 0)
    
    out_file <- paste(dir, "/output.csv", sep = "")
    print(out_file)
    # Output directory
    # out_file <- paste(dir, "/output.csv")
    # dir <- file.choose()
    excluded <- data.frame(c("Calibration"))                              # Names to be excluded
    #zIncr <- 5.18134715 - 3.454231434                                     # Increment in Z for adding noise
    
    #Stores the noise input into a new variable
    ifNoise <- input$noise_checkbox   
    #Change later
    # Controls if there is noise added
    
    #Stores the double and single inputs into a new variable
    ifSingle <- input$single_trial_checkbox                                                  # Controls if the single-entity KDEs are done
    ifDouble <- input$double_trial_checkbox                                                   # Controls if the double-entity KDEs are done
    
    #colorParams
    ## Display Parameters                                                 # Lengths should match length of percs
    
    #Color ordering is done first based on group, then the largest contour (biggest that covers the other one) takes the first color, the smaller
    #contour on the inside takes the second color.
    #colorSingle <- c("red", "orange", "yellow", "pink", "purple")         # Colors for single-entity KDEs
    #colorDouble1 <- c("red", "orange", "yellow", "pink")                  # Colors for first entity of 3D double-entity KDEs
    #colorDouble2 <- c("green", "blue","cyan", "purple")                   # Colors for second entity of 3D double-entity KDEs
    
    
    #colorSingle <- c("yellow", "red", "orange", "purple")         # Colors for single-entity KDEs
    #colorDouble1 <- c("yellow", "red", "orange", "purple")                       # Colors for first entity of 3D double-entity KDEs
    #colorDouble2 <- c("blue","green", "cyan", "purple")
    
    
    #Apparently you will never need more than 2 colors
    #Switching the colors
    
    #Nancy said for the majority of cases  you'll only need 2 colors, these are just the back up incase it's ever more (the yellow and pink)
    colorSingle <- c(input$a1c1, input$a1c2, 'yellow', "pink")                  # Colors for single-entity KDEs
    colorDouble1 <- c(input$a1c1, input$a1c2, "yellow", "pink")                 # Colors for first entity of 3D double-entity KDEs
    colorDouble2 <- c(input$a2c1, input$a2c2, "cyan", "purple") 
    
    #original = .25, .50, .95
    #Opacity values for how colors stack up on each other
    opacitySingle <- c(0.35, 1)                                           # Opacities for 3D single-entity KDEs
    opacityDouble1 <- c(0.25, 0.50, 0.95)                                 # Opacities for first entity of 3D double-entity KDEs
    opacityDouble2 <- c(0.25, 0.50, 0.95)                                 # Opacities for second entity of 3D double-entity KDEs
    display2D <- "filled.contour"                                         # Plot type for 2D (filled.contour, slice, persp, image)
    
    # Set static args
    
    
    #Storing each respective input into new variables before calling the function
    path <- input$file$datapath
    if2D <- input$twoD_checkbox
    nameCol <- input$name_col
    xCol <- input$x_col
    yCol <- input$y_col
    zCol <- input$z_col
    ifNoise <- input$noise_checkbox
    ms <- input$scaling_slider_server
    ns <- input$stages_slider_server
    samse <- input$samse_checkbox
    unconstr <- input$unconstr_checkbox
    dscalar <- input$dscalar_checkbox
    dunconstr <- input$dunconstr_checkbox
    enclosure_depth <- input$enclosure_depth_input
    depth_sections <-  input$depth_sections_input
    
    # Set contours (percs)
    
    #Parses the contours string and stores it in percs variable as a list
    percsString <- input$contours_input
    percsList <- as.list(strsplit(percsString, ",")[[1]])
    percs <- lapply(percsList, function(x) as.integer(x))
    print(percs)
    # Determining depth section height
    # Currently, heights are marked as the top of a section, so the range between the max and min
    # depth values goes from the top of the highest section to the top of the lowest section.
    # The range of the bottom section is not included, so we subtract 1 from section count
    # when finding heights of individual sections below
    
    #ZIncr is calculated with depth/depth sections
    zIncr <- enclosure_depth / depth_sections
    print(zIncr)
    
    #Stores each pilot continuously depending on how many you select
    pilots <- c()
    if(samse){
      pilots <- c(pilots, "samse")
    }
    if(unconstr){
      pilots <- c(pilots, "unconstr")
    }
    if(dscalar){
      pilots <- c(pilots, "dscalar")
    }
    if(dunconstr){
      pilots <- c(pilots, "dunconstr")
    }
    
    #dirInfo <- (paste(dir, "/output_total_double.csv", sep=""))
    #print(dirInfo)
    
    #file.create(paste(dir, "/output_total_single.csv", sep=""))
    
    #Creating the folders the file will be stored in
    dir.create(file.path(getwd(), "Single-Trial-Results"))
    dir.create(file.path(getwd(), "Double-Trial-Results"))
    dir.create(file.path(getwd(), "Cumulative-Tables"))
    
    #dir.create(file.path(dir, "Single-Trial-Results"))
    #dir.create(file.path(dir, "Double-Trial-Results"))
    #dir.create(file.path(dir, "Cumulative-Tables"))
    
    #Creating both total output csvs, which aggregate the data together instead of having it in separate files
    file.create("Cumulative-Tables/output_total_single.csv")
    #file.create(file.path(dir, "output_total_double.csv"))
    file.create("Cumulative-Tables/output_total_double.csv")
    
    #Storing the volumes before run, so it can access it without it being in a for loop, since the total tables only need to write it once (at the top of the file)
    volumes <- data.frame(matrix(ncol=1+3*length(percs), nrow=0))
    prefixes <- c("V1", "V2", "V&")
    volnames <- c(outer(prefixes, paste(percs), paste))
    colnames(volumes) <- c("Label", volnames)
    
    #write.table(volumes, "C:/3DKDE/Output_from_Zoo/output_total_double.csv", row.names=TRUE, sep=", ", append = TRUE , col.names=TRUE, quote=TRUE, na="NA")

    #Calling the run function which is the one actually doing the KDE calculations
    run(path, sheet, nameCol, xCol, yCol, zCol, dir, out_file, excluded, zIncr, ifNoise, ifSingle, ifDouble, if2D, percs, ms, ns, pilots, colorSingle, colorDouble1, colorDouble2, opacitySingle, opacityDouble1, opacityDouble2, display2D)
    #Lists the files and stores each one ending in one of the specified patterns as a list in Zip_Files
    Zip_Files <- list.files(path = getwd(), pattern = ".txt$|.csv$|.js$|.css$|samse.html$|unconstr.html$|dscalar.html$|dunconstr.html$", recursive = TRUE)
    #Zip_Files <- list.files(path = dir, pattern = ".csv$|.js$|.css$|.html$", recursive = TRUE)
    
    #Zips all the files listed in Zip_Files and stores it in TestZip.zip
    zip::zip(zipfile = "TestZip.zip", files = Zip_Files)
    
    #For deleting old files still kept in the temp directory
    #files <- list.files(tmp_dir, full.names = T, pattern = "^file")
    #file.remove(tempFiles)
    
    #Don't delete here, deleting before files can be downloaded, duh
    #unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
    
    print("Done Running KDE")
    
    #Update the progress bar to 100, KDE is done running 
    updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = 100)
    #updateTextInput(session, "KDEtext", value = "KDE is Finished")
    
    })
}


#KDE Script/Functions




# Run the application with stack trace on, to see more detailed error messages
options(shiny.fullstacktrace=TRUE)
shinyApp(ui = ui, server = server)
