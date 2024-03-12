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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  div(
    style = 
      "height: 80px; background-color: cyan; width: 100%;",
    tags$h1(
      "Sezarc: KDE", 
      style = 
        "position: relative; text-align:center; top: 50%; -ms-transform: translateY(-50%); transform: translateY(-50%); padding-right: 10px; padding-left: 10px;"
    )
  ),
  div(style = "height: 40px; background-color: white;"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          position = "left",
          width = 6,
          fileInput(inputId = "file", label = "Choose Excel file", multiple = TRUE),
          #shinyDirButton('folder', 'Click here to select a folder to hold the output files', 'Please select a folder', FALSE),
          uiOutput("name_select"),
          uiOutput("x_select"),
          uiOutput("y_select"),
          uiOutput("z_select"),
          uiOutput("scaling_slider"),
          uiOutput("stages_slider"),
          uiOutput("samse"),
          uiOutput("unconstr"),
          uiOutput("dscalar"),
          uiOutput("dunconstr"),
          uiOutput("twoD"),
          uiOutput("noise"),
          uiOutput("single_trial"),
          uiOutput("double_trial"),
          htmlOutput("min_max"),
          uiOutput("depth_sections"),
          uiOutput("enclosure_depth"),
          uiOutput("contours")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          position = "right",
          width = 6,
          progressBar(id = "pb", value = 0, title = "KDE Progress:", display_pct = T, total = 100),
          actionButton("action1", "Run KDE"),
          #Switched from downloadLink to download Button
          downloadButton("downloadData", "Download KDE RESULTS"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
        write.table(volumes, out_file_name, row.names=TRUE, sep=", ", col.names=TRUE, quote=TRUE, na="NA")
        write.table(volumes, total_out_file_single, row.names=TRUE, sep=", ", append = TRUE , col.names=TRUE, quote=TRUE, na="NA")
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
  
  #updateTextInput(session, "KDEtext", value = "KDE has not been started")
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("KDE-Data-", Sys.Date(), ".zip", sep="")
    },
    content = function(file) {
      file.copy("TestZip.zip", file)
    }
  )
  
  output$name_select <- renderUI({
    req(input$file)
    selectInput("name_col", "Name Column", choices = colnames(read_excel(input$file$datapath)), label = "Name Column") 
  })
  output$x_select <- renderUI({
    req(input$file)
    selectInput("x_col", "X Column", choices = colnames(read_excel(input$file$datapath)), label = "X Column")
  })
  output$y_select <- renderUI({
    req(input$file)
    selectInput("y_col", "Y Column", choices = colnames(read_excel(input$file$datapath)), label = "Y Column")
  })
  output$z_select <- renderUI({
    req(input$file)
    selectInput("z_col", "Z Column", choices = colnames(read_excel(input$file$datapath)), label = "Z Column")
  })
  
  #Sliders for Scaling Factor and Stages in Bandwidth Stages
  
  output$scaling_slider <- renderUI({
    req(input$file)
    sliderInput("scaling_slider_server", "Scaling Factor (m)", 1, 10, 1, 1)
  })
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
    textInput("contours_input", "Input Contours")
  })
  
  #Button to Run KDE
  observeEvent(input$action1, {
    
    #updateTextInput(session, "KDEtext", value = "KDE is Running")
    
    req(input$file)
    print(input$file$datapath)
    options(stringsAsFactors = FALSE)
    sheet <- 1  
    
    #Dir path is not needed in shiny, because it will all be downloaded in the web from working directory
    #dir <- file.path(tempdir())
    #dir.create(dir)
    
    #works with putting into dir <- getwd()
    #dir <- getwd()
    #dir <- tempdir()
    setwd(tempdir())
    dir <- getwd()
    print(paste("wd is: ", getwd()))
    print(paste("tempdir is :", dir))
    
    out_file <- paste(dir, "/output.csv", sep = "")
    print(out_file)
    # Output directory
    # out_file <- paste(dir, "/output.csv")
    # dir <- file.choose()
    excluded <- data.frame(c("Calibration"))                              # Names to be excluded
    #zIncr <- 5.18134715 - 3.454231434                                     # Increment in Z for adding noise
    ifNoise <- input$noise_checkbox   
    #Change later
    # Controls if there is noise added
    ifSingle <- input$single_trial_checkbox                                                  # Controls if the single-entity KDEs are done
    ifDouble <- input$double_trial_checkbox                                                   # Controls if the double-entity KDEs are done
    
    ## Display Parameters                                                 # Lengths should match length of percs
    colorSingle <- c("red", "orange", "yellow", "pink", "purple")         # Colors for single-entity KDEs
    colorDouble1 <- c("red", "orange", "yellow", "pink")                  # Colors for first entity of 3D double-entity KDEs
    colorDouble2 <- c("green", "blue","cyan", "purple")                   # Colors for second entity of 3D double-entity KDEs
    opacitySingle <- c(0.35, 1)                                           # Opacities for 3D single-entity KDEs
    opacityDouble1 <- c(0.25, 0.50, 0.95)                                 # Opacities for first entity of 3D double-entity KDEs
    opacityDouble2 <- c(0.25, 0.50, 0.95)                                 # Opacities for second entity of 3D double-entity KDEs
    display2D <- "filled.contour"                                         # Plot type for 2D (filled.contour, slice, persp, image)
    
    #moved args assignment to assignment or DIR for the python menu implementation
    #args = commandArgs(trailingOnly=TRUE)
    
    # Set static args
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
    percsString <- input$contours_input
    percsList <- as.list(strsplit(percsString, ",")[[1]])
    percs <- lapply(percsList, function(x) as.integer(x))
    print(percs)
    # Determining depth section height
    # Currently, heights are marked as the top of a section, so the range between the max and min
    # depth values goes from the top of the highest section to the top of the lowest section.
    # The range of the bottom section is not included, so we subtract 1 from section count
    # when finding heights of individual sections below
    
    zIncr <- enclosure_depth / depth_sections
    print(zIncr)
    
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
    
    
    dir.create(file.path(getwd(), "Single-Trial-Results"))
    dir.create(file.path(getwd(), "Double-Trial-Results"))
    dir.create(file.path(getwd(), "Cumulative-Tables"))
    
    #dir.create(file.path(dir, "Single-Trial-Results"))
    #dir.create(file.path(dir, "Double-Trial-Results"))
    #dir.create(file.path(dir, "Cumulative-Tables"))
    
    file.create("Cumulative-Tables/output_total_single.csv")
    #file.create(file.path(dir, "output_total_double.csv"))
    file.create("Cumulative-Tables/output_total_double.csv")
    volumes <- data.frame(matrix(ncol=1+3*length(percs), nrow=0))
    prefixes <- c("V1", "V2", "V&")
    volnames <- c(outer(prefixes, paste(percs), paste))
    colnames(volumes) <- c("Label", volnames)
    
    #write.table(volumes, "C:/3DKDE/Output_from_Zoo/output_total_double.csv", row.names=TRUE, sep=", ", append = TRUE , col.names=TRUE, quote=TRUE, na="NA")
    
    #Update progress bar currently is only accurate IF only double or single is checked, NOT BOTH
    
    run(path, sheet, nameCol, xCol, yCol, zCol, dir, out_file, excluded, zIncr, ifNoise, ifSingle, ifDouble, if2D, percs, ms, ns, pilots, colorSingle, colorDouble1, colorDouble2, opacitySingle, opacityDouble1, opacityDouble2, display2D)
    Zip_Files <- list.files(path = getwd(), pattern = ".csv$|.js$|.css$|samse.html$|unconstr.html$|dscalar.html$|dunconstr.html$", recursive = TRUE)
    #Zip_Files <- list.files(path = dir, pattern = ".csv$|.js$|.css$|.html$", recursive = TRUE)
    zip::zip(zipfile = "TestZip.zip", files = Zip_Files)
    print("Done Running KDE")
    updateProgressBar(session = session, id = "pb", title = "KDE Progress:", value = 100)
    #updateTextInput(session, "KDEtext", value = "KDE is Finished")
    
    })
}


#KDE Script/Functions




# Run the application 
options(shiny.fullstacktrace=TRUE)
shinyApp(ui = ui, server = server)
