library(shiny)
library(magicdesign)

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("magicdesignee"),
  
  # Use the sidebarLayout with sidebar panel for inputs and main panel for results.
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(title="Main",
                 
                 br(),
                 
                 # Input: n
                 numericInput(inputId="ip.n", label="Number of founders", value=8, min=3, max=128, step=1, width="200px"),
                 
                 # Input: n.chr
                 numericInput(inputId="ip.n.chr", label="Number of chromosomes", value=1, min=1, max=100, step=1, width="200px"),
                 
                 # Input: chr.names
                 uiOutput(outputId="ip.chr.names"),
                 
                 # Input: chr.len
                 uiOutput(outputId="ip.chr.len"),
                 
                 # Input: inbred.
                 selectInput(inputId="ip.inbred", label="Inbred founders", choices=c("Yes", "No"), width="200px"),
                 
                 # Input: n.sim
                 numericInput(inputId="ip.n.sim", label="Number of simulations", value=1, min=1, max=1000, step=1, width="200px"),
                 
                 # Input: marker.dist
                 numericInput(inputId="ip.marker.dist", label="Marker distance (Morgan)", value=0.01, min=0.001, max=0.1, step=0.001, width="200px"),
                 
                 # Input: hap.int
                 numericInput(inputId="ip.hap.int", label="Haplotype interval", value=0.05, min=0.001, max=0.1, step=0.001, width="200px"),
                 
                 # Input: n.hap
                 selectInput(inputId="ip.n.hap", label="Number of haploid marker data to use in evaluation", choices=c(1,2), width="200px"),
                 
                 # Input: n.try
                 numericInput(inputId="ip.n.try", label="Number of tries (ignored in unbalanced design)", value=1000, min=10, max=10000, step=1, width="200px"),
                 
                 # Reset all
                 br(),
                 actionButton(inputId="reset", label="Reset all to defaults")
                 
        ),
        tabPanel(title="Designs",
                 tabsetPanel(
                   tabPanel(title="1",
                            
                            br(),
                            
                            # Input: balanced.1
                            uiOutput(outputId="ip.balanced.1"),
                            
                            # Input: m.1
                            uiOutput(outputId="ip.m.1"),
                            
                            br(),
                            
                            # Input: reps.1
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.1"),
                            br(),
                            
                            # Input: self.1
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.1"),
                            br(),
                            
                            # Input: addx.1
                            uiOutput(outputId="ip.addx.1"),
                            
                            # Input: repx.1
                            numericInput(inputId="ip.repx.1", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.1
                            numericInput(inputId="ip.selfx.1", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.1
                            uiOutput(outputId="ip.minimize.1", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.1
                                     fileInput(inputId="ip.ped.1", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.1", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 1.
                            actionButton(inputId="run.1", label="Run design 1"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 1.
                            actionButton(inputId="reset.1", label="Reset design 1 to defaults")
                            
                   ),
                   tabPanel(title="2",
                            
                            br(),
                            
                            # Input: balanced.2
                            uiOutput(outputId="ip.balanced.2"),
                            
                            # Input: m.2
                            uiOutput(outputId="ip.m.2"),
                            
                            br(),
                            
                            # Input: reps.2
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.2"),
                            br(),
                            
                            # Input: self.2
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.2"),
                            br(),
                            
                            # Input: addx.2
                            uiOutput(outputId="ip.addx.2"),
                            
                            # Input: repx.2
                            numericInput(inputId="ip.repx.2", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.2
                            numericInput(inputId="ip.selfx.2", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.2
                            uiOutput(outputId="ip.minimize.2", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.2
                                     fileInput(inputId="ip.ped.2", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.2", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 2.
                            actionButton(inputId="run.2", label="Run design 2"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 2.
                            actionButton(inputId="reset.2", label="Reset design 2 to defaults")
                            
                   ),
                   tabPanel(title="3",
                            
                            br(),
                            
                            # Input: balanced.3
                            uiOutput(outputId="ip.balanced.3"),
                            
                            # Input: m.3
                            uiOutput(outputId="ip.m.3"),
                            
                            br(),
                            
                            # Input: reps.3
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.3"),
                            br(),
                            
                            # Input: self.3
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.3"),
                            br(),
                            
                            # Input: addx.3
                            uiOutput(outputId="ip.addx.3"),
                            
                            # Input: repx.3
                            numericInput(inputId="ip.repx.3", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.3
                            numericInput(inputId="ip.selfx.3", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.3
                            uiOutput(outputId="ip.minimize.3", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.3
                                     fileInput(inputId="ip.ped.3", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.3", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 3.
                            actionButton(inputId="run.3", label="Run design 3"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 3.
                            actionButton(inputId="reset.3", label="Reset design 3 to defaults")
                            
                   ),
                   tabPanel(title="4",
                            
                            br(),
                            
                            # Input: balanced.4
                            uiOutput(outputId="ip.balanced.4"),
                            
                            # Input: m.4
                            uiOutput(outputId="ip.m.4"),
                            
                            br(),
                            
                            # Input: reps.4
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.4"),
                            br(),
                            
                            # Input: self.4
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.4"),
                            br(),
                            
                            # Input: addx.4
                            uiOutput(outputId="ip.addx.4"),
                            
                            # Input: repx.4
                            numericInput(inputId="ip.repx.4", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.4
                            numericInput(inputId="ip.selfx.4", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.4
                            uiOutput(outputId="ip.minimize.4", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.4
                                     fileInput(inputId="ip.ped.4", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.4", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 4.
                            actionButton(inputId="run.4", label="Run design 4"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 4.
                            actionButton(inputId="reset.4", label="Reset design 4 to defaults")
                            
                   ),
                   tabPanel(title="5",
                            
                            br(),
                            
                            # Input: balanced.5
                            uiOutput(outputId="ip.balanced.5"),
                            
                            # Input: m.5
                            uiOutput(outputId="ip.m.5"),
                            
                            br(),
                            
                            # Input: reps.5
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.5"),
                            br(),
                            
                            # Input: self.5
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.5"),
                            br(),
                            
                            # Input: addx.5
                            uiOutput(outputId="ip.addx.5"),
                            
                            # Input: repx.5
                            numericInput(inputId="ip.repx.5", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.5
                            numericInput(inputId="ip.selfx.5", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.5
                            uiOutput(outputId="ip.minimize.5", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.5
                                     fileInput(inputId="ip.ped.5", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.5", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 5.
                            actionButton(inputId="run.5", label="Run design 5"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 5.
                            actionButton(inputId="reset.5", label="Reset design 5 to defaults")
                            
                   ),
                   tabPanel(title="6",
                            
                            br(),
                            
                            # Input: balanced.6
                            uiOutput(outputId="ip.balanced.6"),
                            
                            # Input: m.6
                            uiOutput(outputId="ip.m.6"),
                            
                            br(),
                            
                            # Input: reps.6
                            h5(strong("Number of replicates for X-way individuals")),
                            uiOutput(outputId="ip.reps.6"),
                            br(),
                            
                            # Input: self.6
                            h5(strong("Number of generations to self for X-way individuals")),
                            uiOutput(outputId="ip.self.6"),
                            br(),
                            
                            # Input: addx.6
                            uiOutput(outputId="ip.addx.6"),
                            
                            # Input: repx.6
                            numericInput(inputId="ip.repx.6", label="Number of replicates for the extra cross individuals", value=1, min=1, max=100, step=1),
                            
                            # Input: selfx.6
                            numericInput(inputId="ip.selfx.6", label="Number of generations to self after the extra cross", value=3, min=0, max=10, step=1),
                            
                            # Input: minimize.6
                            uiOutput(outputId="ip.minimize.6", width="200px"),
                            
                            fluidRow(
                              column(width=9,
                                     # Input: pedigree.6
                                     fileInput(inputId="ip.ped.6", label="Pedigree in CSV format", accept=".csv")
                              ),
                              
                              column(width=3,
                                     
                                     checkboxInput(inputId="header.6", label="Header", value=TRUE)
                              )
                            ),
                            
                            # Run design 6.
                            actionButton(inputId="run.6", label="Run design 6"),
                            
                            br(),
                            br(),
                            br(),
                            
                            # Reset design 6.
                            actionButton(inputId="reset.6", label="Reset design 6 to defaults")
                            
                   )
                 )
        )
      )
    ),
    
    mainPanel(
      # from https://stackoverflow.com/questions/40538365/r-shiny-how-to-get-square-plot-to-use-full-width-of-panel
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
      tabsetPanel(
        
        # Instructions for users.
        tabPanel(title="Instructions",
                 br(),
                 p(tags$code("magicdesignee"),
                   " is a Shiny implementation of the ",
                   tags$code("magicdesign"),
                   "R package. ",
                   "There are two tabs for entering input arguments: ",
                   strong("Main"), " and ", strong("Design"), ". ",
                   "The ", strong("Main"), " tab includes all common input arguments that are shared among all designs. ",
                   "The ", strong("Designs"), " tabs are meant for input arguments unique to each design. ",
                   "There are up to six allowed designs. ",
                   "The designs will not be evaluated until the ", strong("Run design X"), " button has been clicked. ",
                   "In addition, users can reset the input arguments back to default and clear the results ",
                   "by clicking on the ", strong("Reset"), " buttons. ",
                   "The reset button in the main tab will reset everything while ",
                   "the reset buttons in the Designs tab will only reset that specific design input arguments."),
                 br(),
                 p("There are four tabs for results displays: ",
                   tags$ul(
                     tags$li(strong("Summary information"), ": information about each design, ",
                             "including number of founders, funnels, replications, and etc."),
                     tags$li(strong("Recombinant haplotype distribution"), ": proportions of total and individual ",
                             "recombinant haplotypes within the specified interval."),
                     tags$li(strong("Founder genome distribution"), ": proportions of founder genomes and ",
                             "non-recombining segment lengths."),
                     tags$li(strong("Pedigree"), ": interactive pedigree plot.")
                   ),
                   "The plots can be saved as image file by clicking the Download button and ",
                   "the pedigree can be saved as either an interactive HTML file or CSV file. ",
                   "Further details on the sub plots are described in their respective tabs."),
                 br(),
                 p("The important Main input arguments are: ",
                   "Number of founders, Number of chromosomes, Chromosome lengths and Number of simulations. ",
                   "The rest can be left as default."),
                 br(),
                 p("The important Designs input arguments are: ",
                   "Balanced design, Number of funnels/funnel sets, ",
                   "Number of replicates and Number of generations to self. "),
                 br(),
                 p("Depending on the type of design, users may wish to add an extra generation of crossing, ",
                   "which also comes with the number of replicates and generations to self. ",
                   "Alternatively, users can also upload a pedigree containing all founders and ",
                   "crosses going from the founders to the final RILs. ",
                   "The pedigree file must contain four columns in the order of individual ID, ",
                   "parent 1 ID, parent 2 ID and generation number. ",
                   "The individual IDs must all be unique, the parent IDs must be found in the individual IDs, ",
                   "the founder parent IDs can be left empty or as 0, and the generation number must be continuous ",
                   "and begins with 0 in the founders."),
                 br(),
                 p("More details can be found in the",
                   tags$a(href="https://cjyang-sruc.github.io/files/magicdesign_vignette", "vignette"),
                   "for the R package",
                   tags$code("magicdesign"), "."),
                 br(),
                 p("Please",
                   tags$a(href="mailto:cyang@sruc.ac.uk", "contact me"),
                   "if you have any question or suggestion.")
        ),
        
        # Summary information for all designs.
        tabPanel(title="Summary information",
                 tableOutput(outputId="summary")
        ),
        
        # Plot for the recombinations within hap.int.
        tabPanel(title="Recombination analysis",
                 br(),
                 downloadButton(outputId="DL.plot.interval", label="Download this plot"),
                 br(),
                 uiOutput(outputId="ip.fpair"),
                 br(),
                 p("Subplot", strong("A"), ": proportion of total recombinant haplotypes."),
                 p("Subplot", strong("B"), ": number of unique recombinant haplotypes."),
                 p("Subplot", strong("C"), ": count of individual recombinant haplotypes",
                   "(points are means, lines are spread from min to max)."),
                 plotOutput(outputId="plot.interval")
        ),
        
        # Plot for the founder genome distribution.
        tabPanel(title="Founder genome distribution",
                 br(),
                 downloadButton(outputId="DL.plot.whole", label="Download this plot"),
                 br(),
                 checkboxInput(inputId="annotate", label="Annotate", value=TRUE),
                 br(),
                 p("Subplot", strong("A"), ": proportion of each founder genome."),
                 p("Subplot", strong("B"), ": proportion of 1 to n unique founders in each chromosome."),
                 p("Subplot", strong("C"), ": count of non-recombining segment lengths"),
                 plotOutput(outputId="plot.whole")
        ),
        
        # Plot for the pedigree.
        tabPanel(title="Pedigree",
                 br(),
                 p("Note: replicates in the last crossing generation are not displayed to reduce clutter."),
                 br(),
                 downloadButton(outputId="DL.plot.ped", label="Download this pedigree as HTML"),
                 downloadButton(outputId="DL.csv.ped", label="Download this pedigree as CSV"),
                 br(),
                 uiOutput(outputId="ip.pedplot"),
                 selectInput(inputId="ip.w2h", label="width-to-height ratio", choices=seq(0.1, 10, 0.1), selected=2, width="200px"),
                 plotly::plotlyOutput(outputId="ped", width="800px", height="600px")
        )
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  ### inputs for the MAIN tab.
  
  # convert n to numeric.
  n <- reactive(as.numeric(input$ip.n))
  
  # calculate the number of crossing generation (nx) based on n.
  nx <- reactive(log(2^ceiling(log(n(),2)),2))
  
  # convert n.chr
  n.chr <- reactive(as.numeric(input$ip.n.chr))
  
  # create n.chr boxes for chromosome names.
  output$ip.chr.names <- renderUI({
    
    textInput(inputId="ip.chr.names",
              label="Chromosome names",
              value=paste(1:n.chr(), collapse=","))
    
  })
  chr.names <- reactive({
    if(n.chr() > 1) unlist(strsplit(x=input$ip.chr.names, split=",")) else input$ip.chr.names
  })
  
  # create n.chr boxes for chromosome lengths.
  output$ip.chr.len <- renderUI({
    chr.ls <- lapply(1:n.chr(), FUN=function(x) numericInput(inputId=paste("chr", x, sep=""),
                                                             label=paste("Chr ", chr.names()[x], sep=""),
                                                             value=1,
                                                             min=0.1,
                                                             max=10,
                                                             step=0.001))
    do.call(flowLayout, c(chr.ls, list(cellArgs=list(style="width: 100px;"))))
  })
  chr.len <- reactive({
    unlist(lapply(1:n.chr(), FUN=function(x) input[[paste("chr", x, sep="")]]))
  })
  
  # convert inbred.
  inbred <- reactive({
    req(input$ip.inbred)
    if(input$ip.inbred=="Yes") TRUE else FALSE
  })
  
  # convert n.sim
  n.sim <- reactive(as.numeric(input$ip.n.sim))
  
  # convert marker.dist
  marker.dist <- reactive(as.numeric(input$ip.marker.dist))
  
  # convert hap.int
  hap.int <- reactive(as.numeric(input$ip.hap.int))
  
  # convert n.hap
  n.hap <- reactive(as.numeric(input$ip.n.hap))
  
  # convert n.try
  n.try <- reactive(as.numeric(input$ip.n.try))
  
  
  ### inputs for the DESIGN 1 tab.
  
  # restrict balanced.1 based on user choice of n.
  output$ip.balanced.1 <- renderUI({
    selectInput(inputId="ip.balanced.1",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.1 <- reactive({
    req(input$ip.balanced.1)
    if(input$ip.balanced.1=="Yes") TRUE else FALSE
  })
  
  # restrict m.1 based on user choice of n and balanced.1.
  output$ip.m.1 <- renderUI({
    
    mm.1 <- if(balanced.1()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.1 <- if(balanced.1()){
      paste("Number of funnel sets (min = ", mm.1[1], ", max = ", mm.1[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.1[1], ", max = ", mm.1[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.1",
                 label=label.1,
                 value=mm.1[1],
                 min=mm.1[1],
                 max=mm.1[2],
                 step=1)
  })
  m.1 <- reactive(as.numeric(input$ip.m.1))
  
  # create nx boxes for reps.1.
  output$ip.reps.1 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.1.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.1 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.1.", x, sep="")]])))
  })
  
  # create nx boxes for self.1.
  output$ip.self.1 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.1.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.1 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.1.", x, sep="")]])))
  })
  
  # restrict addx.1 based on user choice of m.1
  output$ip.addx.1 <- renderUI({
    req(m.1())
    selectInput(inputId="ip.addx.1",
                label="Type of extra cross to add",
                choices=if(m.1() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.1 <- reactive({
    req(input$ip.addx.1)
    if(input$ip.addx.1 == 0) NULL else as.numeric(input$ip.addx.1)
  })
  
  # convert repx
  repx.1 <- reactive(as.numeric(input$ip.repx.1))
  
  # convert selfx
  selfx.1 <- reactive(as.numeric(input$ip.selfx.1))
  
  # restrict minimize.1 based on user choice of m.1.
  output$ip.minimize.1 <- renderUI({
    req(m.1()) 
    selectInput(inputId="ip.minimize.1",
                label="Minimize individuals & crosses",
                choices=if(m.1() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.1 <- reactive({
    req(input$ip.minimize.1)
    if(input$ip.minimize.1=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.1 <- reactive({
    if(is.null(input$ip.ped.1)){
      NULL
    } else {
      read.csv(file=input$ip.ped.1$datapath, header=input$header.1)
    }
  })
  
  # evaluate design 1.
  mpop.1 <- eventReactive(input$run.1,
                          {
                            shinyjs::disable("run.1")
                            magic.eval(ped=ped.1(),
                                       n=n(),
                                       m=m.1(),
                                       reps=reps.1(),
                                       self=self.1(),
                                       inbred=inbred(),
                                       balanced=balanced.1(),
                                       minimize=minimize.1(),
                                       n.try=n.try(),
                                       addx=addx.1(),
                                       repx=repx.1(),
                                       selfx=selfx.1(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### inputs for the DESIGN 2 tab.
  
  # restrict balanced.2 based on user choice of n.
  output$ip.balanced.2 <- renderUI({
    selectInput(inputId="ip.balanced.2",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.2 <- reactive({
    req(input$ip.balanced.2)
    if(input$ip.balanced.2=="Yes") TRUE else FALSE
  })
  
  # restrict m.2 based on user choice of n and balanced.2.
  output$ip.m.2 <- renderUI({
    
    mm.2 <- if(balanced.2()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.2 <- if(balanced.2()){
      paste("Number of funnel sets (min = ", mm.2[1], ", max = ", mm.2[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.2[1], ", max = ", mm.2[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.2",
                 label=label.2,
                 value=mm.2[1],
                 min=mm.2[1],
                 max=mm.2[2],
                 step=1)
  })
  m.2 <- reactive(as.numeric(input$ip.m.2))
  
  # create nx boxes for reps.2.
  output$ip.reps.2 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.2.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.2 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.2.", x, sep="")]])))
  })
  
  # create nx boxes for self.2.
  output$ip.self.2 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.2.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.2 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.2.", x, sep="")]])))
  })
  
  # restrict addx.2 based on user choice of m.2
  output$ip.addx.2 <- renderUI({
    req(m.2())
    selectInput(inputId="ip.addx.2",
                label="Type of extra cross to add",
                choices=if(m.2() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.2 <- reactive({
    req(input$ip.addx.2)
    if(input$ip.addx.2 == 0) NULL else as.numeric(input$ip.addx.2)
  })
  
  # convert repx
  repx.2 <- reactive(as.numeric(input$ip.repx.2))
  
  # convert selfx
  selfx.2 <- reactive(as.numeric(input$ip.selfx.2))
  
  # restrict minimize.2 based on user choice of m.2.
  output$ip.minimize.2 <- renderUI({
    req(m.2()) 
    selectInput(inputId="ip.minimize.2",
                label="Minimize individuals & crosses",
                choices=if(m.2() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.2 <- reactive({
    req(input$ip.minimize.2)
    if(input$ip.minimize.2=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.2 <- reactive({
    if(is.null(input$ip.ped.2)){
      NULL
    } else {
      read.csv(file=input$ip.ped.2$datapath, header=input$header.2)
    }
  })
  
  # evaluate design 2.
  mpop.2 <- eventReactive(input$run.2,
                          {
                            shinyjs::disable("run.2")
                            magic.eval(ped=ped.2(),
                                       n=n(),
                                       m=m.2(),
                                       reps=reps.2(),
                                       self=self.2(),
                                       inbred=inbred(),
                                       balanced=balanced.2(),
                                       minimize=minimize.2(),
                                       n.try=n.try(),
                                       addx=addx.2(),
                                       repx=repx.2(),
                                       selfx=selfx.2(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### inputs for the DESIGN 3 tab.
  
  # restrict balanced.3 based on user choice of n.
  output$ip.balanced.3 <- renderUI({
    selectInput(inputId="ip.balanced.3",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.3 <- reactive({
    req(input$ip.balanced.3)
    if(input$ip.balanced.3=="Yes") TRUE else FALSE
  })
  
  # restrict m.3 based on user choice of n and balanced.3.
  output$ip.m.3 <- renderUI({
    
    mm.3 <- if(balanced.3()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.3 <- if(balanced.3()){
      paste("Number of funnel sets (min = ", mm.3[1], ", max = ", mm.3[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.3[1], ", max = ", mm.3[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.3",
                 label=label.3,
                 value=mm.3[1],
                 min=mm.3[1],
                 max=mm.3[2],
                 step=1)
  })
  m.3 <- reactive(as.numeric(input$ip.m.3))
  
  # create nx boxes for reps.3.
  output$ip.reps.3 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.3.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.3 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.3.", x, sep="")]])))
  })
  
  # create nx boxes for self.3.
  output$ip.self.3 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.3.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.3 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.3.", x, sep="")]])))
  })
  
  # restrict addx.3 based on user choice of m.3
  output$ip.addx.3 <- renderUI({
    req(m.3())
    selectInput(inputId="ip.addx.3",
                label="Type of extra cross to add",
                choices=if(m.3() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.3 <- reactive({
    req(input$ip.addx.3)
    if(input$ip.addx.3 == 0) NULL else as.numeric(input$ip.addx.3)
  })
  
  # convert repx
  repx.3 <- reactive(as.numeric(input$ip.repx.3))
  
  # convert selfx
  selfx.3 <- reactive(as.numeric(input$ip.selfx.3))
  
  # restrict minimize.3 based on user choice of m.3.
  output$ip.minimize.3 <- renderUI({
    req(m.3()) 
    selectInput(inputId="ip.minimize.3",
                label="Minimize individuals & crosses",
                choices=if(m.3() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.3 <- reactive({
    req(input$ip.minimize.3)
    if(input$ip.minimize.3=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.3 <- reactive({
    if(is.null(input$ip.ped.3)){
      NULL
    } else {
      read.csv(file=input$ip.ped.3$datapath, header=input$header.3)
    }
  })
  
  # evaluate design 3.
  mpop.3 <- eventReactive(input$run.3,
                          {
                            shinyjs::disable("run.3")
                            magic.eval(ped=ped.3(),
                                       n=n(),
                                       m=m.3(),
                                       reps=reps.3(),
                                       self=self.3(),
                                       inbred=inbred(),
                                       balanced=balanced.3(),
                                       minimize=minimize.3(),
                                       n.try=n.try(),
                                       addx=addx.3(),
                                       repx=repx.3(),
                                       selfx=selfx.3(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### inputs for the DESIGN 4 tab.
  
  # restrict balanced.4 based on user choice of n.
  output$ip.balanced.4 <- renderUI({
    selectInput(inputId="ip.balanced.4",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.4 <- reactive({
    req(input$ip.balanced.4)
    if(input$ip.balanced.4=="Yes") TRUE else FALSE
  })
  
  # restrict m.4 based on user choice of n and balanced.4.
  output$ip.m.4 <- renderUI({
    
    mm.4 <- if(balanced.4()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.4 <- if(balanced.4()){
      paste("Number of funnel sets (min = ", mm.4[1], ", max = ", mm.4[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.4[1], ", max = ", mm.4[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.4",
                 label=label.4,
                 value=mm.4[1],
                 min=mm.4[1],
                 max=mm.4[2],
                 step=1)
  })
  m.4 <- reactive(as.numeric(input$ip.m.4))
  
  # create nx boxes for reps.4.
  output$ip.reps.4 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.4.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.4 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.4.", x, sep="")]])))
  })
  
  # create nx boxes for self.4.
  output$ip.self.4 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.4.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.4 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.4.", x, sep="")]])))
  })
  
  # restrict addx.4 based on user choice of m.4
  output$ip.addx.4 <- renderUI({
    req(m.4())
    selectInput(inputId="ip.addx.4",
                label="Type of extra cross to add",
                choices=if(m.4() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.4 <- reactive({
    req(input$ip.addx.4)
    if(input$ip.addx.4 == 0) NULL else as.numeric(input$ip.addx.4)
  })
  
  # convert repx
  repx.4 <- reactive(as.numeric(input$ip.repx.4))
  
  # convert selfx
  selfx.4 <- reactive(as.numeric(input$ip.selfx.4))
  
  # restrict minimize.4 based on user choice of m.4.
  output$ip.minimize.4 <- renderUI({
    req(m.4()) 
    selectInput(inputId="ip.minimize.4",
                label="Minimize individuals & crosses",
                choices=if(m.4() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.4 <- reactive({
    req(input$ip.minimize.4)
    if(input$ip.minimize.4=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.4 <- reactive({
    if(is.null(input$ip.ped.4)){
      NULL
    } else {
      read.csv(file=input$ip.ped.4$datapath, header=input$header.4)
    }
  })
  
  # evaluate design 4.
  mpop.4 <- eventReactive(input$run.4,
                          {
                            shinyjs::disable("run.4")
                            magic.eval(ped=ped.4(),
                                       n=n(),
                                       m=m.4(),
                                       reps=reps.4(),
                                       self=self.4(),
                                       inbred=inbred(),
                                       balanced=balanced.4(),
                                       minimize=minimize.4(),
                                       n.try=n.try(),
                                       addx=addx.4(),
                                       repx=repx.4(),
                                       selfx=selfx.4(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### inputs for the DESIGN 5 tab.
  
  # restrict balanced.5 based on user choice of n.
  output$ip.balanced.5 <- renderUI({
    selectInput(inputId="ip.balanced.5",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.5 <- reactive({
    req(input$ip.balanced.5)
    if(input$ip.balanced.5=="Yes") TRUE else FALSE
  })
  
  # restrict m.5 based on user choice of n and balanced.5.
  output$ip.m.5 <- renderUI({
    
    mm.5 <- if(balanced.5()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.5 <- if(balanced.5()){
      paste("Number of funnel sets (min = ", mm.5[1], ", max = ", mm.5[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.5[1], ", max = ", mm.5[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.5",
                 label=label.5,
                 value=mm.5[1],
                 min=mm.5[1],
                 max=mm.5[2],
                 step=1)
  })
  m.5 <- reactive(as.numeric(input$ip.m.5))
  
  # create nx boxes for reps.5.
  output$ip.reps.5 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.5.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.5 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.5.", x, sep="")]])))
  })
  
  # create nx boxes for self.5.
  output$ip.self.5 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.5.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.5 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.5.", x, sep="")]])))
  })
  
  # restrict addx.5 based on user choice of m.5
  output$ip.addx.5 <- renderUI({
    req(m.5())
    selectInput(inputId="ip.addx.5",
                label="Type of extra cross to add",
                choices=if(m.5() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.5 <- reactive({
    req(input$ip.addx.5)
    if(input$ip.addx.5 == 0) NULL else as.numeric(input$ip.addx.5)
  })
  
  # convert repx
  repx.5 <- reactive(as.numeric(input$ip.repx.5))
  
  # convert selfx
  selfx.5 <- reactive(as.numeric(input$ip.selfx.5))
  
  # restrict minimize.5 based on user choice of m.5.
  output$ip.minimize.5 <- renderUI({
    req(m.5()) 
    selectInput(inputId="ip.minimize.5",
                label="Minimize individuals & crosses",
                choices=if(m.5() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.5 <- reactive({
    req(input$ip.minimize.5)
    if(input$ip.minimize.5=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.5 <- reactive({
    if(is.null(input$ip.ped.5)){
      NULL
    } else {
      read.csv(file=input$ip.ped.5$datapath, header=input$header.5)
    }
  })
  
  # evaluate design 5.
  mpop.5 <- eventReactive(input$run.5,
                          {
                            shinyjs::disable("run.5")
                            magic.eval(ped=ped.5(),
                                       n=n(),
                                       m=m.5(),
                                       reps=reps.5(),
                                       self=self.5(),
                                       inbred=inbred(),
                                       balanced=balanced.5(),
                                       minimize=minimize.5(),
                                       n.try=n.try(),
                                       addx=addx.5(),
                                       repx=repx.5(),
                                       selfx=selfx.5(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### inputs for the DESIGN 6 tab.
  
  # restrict balanced.6 based on user choice of n.
  output$ip.balanced.6 <- renderUI({
    selectInput(inputId="ip.balanced.6",
                label="Balanced design",
                choices=if(n() > 16) c("No") else c("Yes", "No"))
  })
  balanced.6 <- reactive({
    req(input$ip.balanced.6)
    if(input$ip.balanced.6=="Yes") TRUE else FALSE
  })
  
  # restrict m.6 based on user choice of n and balanced.6.
  output$ip.m.6 <- renderUI({
    
    mm.6 <- if(balanced.6()){
      
      if(n()==3 | n()==4){
        c(1,1)
      } else if(n()==5){
        c(1,48)
      } else if(n()==6){
        c(1,285)
      } else if(n()==7){
        c(1,135)
      } else if(n()==8){
        c(1,45)
      } else {
        c(1,100)
      }
      
    } else {
      
      if(n()==3){
        c(1:3)
      } else if(n()==4){
        c(0,3)
      } else if(n()==5){
        c(1,240)
      } else if(n()==6){
        c(1,855)
      } else if(n()==7){
        c(1,945)
      } else if(n()==8){
        c(0,315)
      } else if(n()==16 | n()==32 | n()==64 | n()==128){
        c(0,1000)
      } else {
        c(1,1000)
      }
      
    }
    
    label.6 <- if(balanced.6()){
      paste("Number of funnel sets (min = ", mm.6[1], ", max = ", mm.6[2], ")", sep="")
    } else {
      paste("Number of funnels (min = ", mm.6[1], ", max = ", mm.6[2], ")", sep="")
    }
    
    numericInput(inputId="ip.m.6",
                 label=label.6,
                 value=mm.6[1],
                 min=mm.6[1],
                 max=mm.6[2],
                 step=1)
  })
  m.6 <- reactive(as.numeric(input$ip.m.6))
  
  # create nx boxes for reps.6.
  output$ip.reps.6 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (reps)", sep="")
    reps.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("reps.6.", x, sep=""),
                                                          label=temp[x],
                                                          choices=1:100,
                                                          selected=if(nx()==x) 10 else 1,
                                                          width="200px"))
  })
  reps.6 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("reps.6.", x, sep="")]])))
  })
  
  # create nx boxes for self.6.
  output$ip.self.6 <- renderUI({
    temp <- paste(c(2,4,8,16,32,64,128), "-ways (self)", sep="")
    self.ls <- lapply(1:nx(), FUN=function(x) selectInput(inputId=paste("self.6.", x, sep=""),
                                                          label=temp[x],
                                                          choices=0:10,
                                                          width="200px"))
  })
  self.6 <- reactive({
    as.numeric(unlist(lapply(1:nx(), FUN=function(x) input[[paste("self.6.", x, sep="")]])))
  })
  
  # restrict addx.6 based on user choice of m.6
  output$ip.addx.6 <- renderUI({
    req(m.6())
    selectInput(inputId="ip.addx.6",
                label="Type of extra cross to add",
                choices=if(m.6() == 0) c(0, 1, 2) else c(0, 2),
                selected=0)
  })
  addx.6 <- reactive({
    req(input$ip.addx.6)
    if(input$ip.addx.6 == 0) NULL else as.numeric(input$ip.addx.6)
  })
  
  # convert repx
  repx.6 <- reactive(as.numeric(input$ip.repx.6))
  
  # convert selfx
  selfx.6 <- reactive(as.numeric(input$ip.selfx.6))
  
  # restrict minimize.6 based on user choice of m.6.
  output$ip.minimize.6 <- renderUI({
    req(m.6()) 
    selectInput(inputId="ip.minimize.6",
                label="Minimize individuals & crosses",
                choices=if(m.6() == 0) "No" else c("Yes", "No"),
                selected="No")
  })
  minimize.6 <- reactive({
    req(input$ip.minimize.6)
    if(input$ip.minimize.6=="Yes") TRUE else FALSE
  })
  
  # read the pedigree.
  ped.6 <- reactive({
    if(is.null(input$ip.ped.6)){
      NULL
    } else {
      read.csv(file=input$ip.ped.6$datapath, header=input$header.6)
    }
  })
  
  # evaluate design 6.
  mpop.6 <- eventReactive(input$run.6,
                          {
                            shinyjs::disable("run.6")
                            magic.eval(ped=ped.6(),
                                       n=n(),
                                       m=m.6(),
                                       reps=reps.6(),
                                       self=self.6(),
                                       inbred=inbred(),
                                       balanced=balanced.6(),
                                       minimize=minimize.6(),
                                       n.try=n.try(),
                                       addx=addx.6(),
                                       repx=repx.6(),
                                       selfx=selfx.6(),
                                       marker.dist=marker.dist(),
                                       chr.len=chr.len(),
                                       n.sim=n.sim(),
                                       hap.int=hap.int(),
                                       n.hap=n.hap())
                          }
  )
  
  
  ### actions for all RESETS.
  
  # reset button.
  observeEvent(input$reset,
               {
                 shinyjs::reset("ip.n")
                 shinyjs::reset("ip.marker.dist")
                 shinyjs::reset("ip.n.chr")
                 shinyjs::reset("ip.chr.names")
                 shinyjs::reset("ip.chr.len")
                 shinyjs::reset("ip.n.sim")
                 shinyjs::reset("ip.hap.int")
                 shinyjs::reset("ip.n.hap")
                 shinyjs::reset("ip.keep")
                 shinyjs::reset("ip.inbred")
                 shinyjs::reset("ip.n.try")
                 shinyjs::reset("ip.fpair")
                 shinyjs::reset("annotate")
                 
                 shinyjs::enable("run.1")
                 shinyjs::reset("ip.ped.1")
                 shinyjs::reset("header.1")
                 shinyjs::reset("ip.balanced.1")
                 shinyjs::reset("ip.m.1")
                 shinyjs::reset("ip.reps.1")
                 shinyjs::reset("ip.self.1")
                 shinyjs::reset("ip.minimize.1")
                 shinyjs::reset("ip.addx.1")
                 shinyjs::reset("ip.repx.1")
                 shinyjs::reset("ip.selfx.1")
                 
                 shinyjs::enable("run.2")
                 shinyjs::reset("ip.ped.2")
                 shinyjs::reset("header.2")
                 shinyjs::reset("ip.balanced.2")
                 shinyjs::reset("ip.m.2")
                 shinyjs::reset("ip.reps.2")
                 shinyjs::reset("ip.self.2")
                 shinyjs::reset("ip.minimize.2")
                 shinyjs::reset("ip.addx.2")
                 shinyjs::reset("ip.repx.2")
                 shinyjs::reset("ip.selfx.2")
                 
                 shinyjs::enable("run.3")
                 shinyjs::reset("ip.ped.3")
                 shinyjs::reset("header.3")
                 shinyjs::reset("ip.balanced.3")
                 shinyjs::reset("ip.m.3")
                 shinyjs::reset("ip.reps.3")
                 shinyjs::reset("ip.self.3")
                 shinyjs::reset("ip.minimize.3")
                 shinyjs::reset("ip.addx.3")
                 shinyjs::reset("ip.repx.3")
                 shinyjs::reset("ip.selfx.3")
                 
                 shinyjs::enable("run.4")
                 shinyjs::reset("ip.ped.4")
                 shinyjs::reset("header.4")
                 shinyjs::reset("ip.balanced.4")
                 shinyjs::reset("ip.m.4")
                 shinyjs::reset("ip.reps.4")
                 shinyjs::reset("ip.self.4")
                 shinyjs::reset("ip.minimize.4")
                 shinyjs::reset("ip.addx.4")
                 shinyjs::reset("ip.repx.4")
                 shinyjs::reset("ip.selfx.4")
                 
                 shinyjs::enable("run.5")
                 shinyjs::reset("ip.ped.5")
                 shinyjs::reset("header.5")
                 shinyjs::reset("ip.balanced.5")
                 shinyjs::reset("ip.m.5")
                 shinyjs::reset("ip.reps.5")
                 shinyjs::reset("ip.self.5")
                 shinyjs::reset("ip.minimize.5")
                 shinyjs::reset("ip.addx.5")
                 shinyjs::reset("ip.repx.5")
                 shinyjs::reset("ip.selfx.5")
                 
                 shinyjs::enable("run.6")
                 shinyjs::reset("ip.ped.6")
                 shinyjs::reset("header.6")
                 shinyjs::reset("ip.balanced.6")
                 shinyjs::reset("ip.m.6")
                 shinyjs::reset("ip.reps.6")
                 shinyjs::reset("ip.self.6")
                 shinyjs::reset("ip.minimize.6")
                 shinyjs::reset("ip.addx.6")
                 shinyjs::reset("ip.repx.6")
                 shinyjs::reset("ip.selfx.6")
                 
               }
  )
  
  # reset.1 button.
  observeEvent(input$reset.1,
               {
                 shinyjs::enable("run.1")
                 shinyjs::reset("ip.ped.1")
                 shinyjs::reset("header.1")
                 shinyjs::reset("ip.balanced.1")
                 shinyjs::reset("ip.m.1")
                 shinyjs::reset("ip.reps.1")
                 shinyjs::reset("ip.self.1")
                 shinyjs::reset("ip.minimize.1")
                 shinyjs::reset("ip.addx.1")
                 shinyjs::reset("ip.repx.1")
                 shinyjs::reset("ip.selfx.1")
               }
  )
  
  # reset.2 button.
  observeEvent(input$reset.2,
               {
                 shinyjs::enable("run.2")
                 shinyjs::reset("ip.ped.2")
                 shinyjs::reset("header.2")
                 shinyjs::reset("ip.balanced.2")
                 shinyjs::reset("ip.m.2")
                 shinyjs::reset("ip.reps.2")
                 shinyjs::reset("ip.self.2")
                 shinyjs::reset("ip.minimize.2")
                 shinyjs::reset("ip.addx.2")
                 shinyjs::reset("ip.repx.2")
                 shinyjs::reset("ip.selfx.2")
               }
  )
  
  # reset.3 button.
  observeEvent(input$reset.3,
               {
                 shinyjs::enable("run.3")
                 shinyjs::reset("ip.ped.3")
                 shinyjs::reset("header.3")
                 shinyjs::reset("ip.balanced.3")
                 shinyjs::reset("ip.m.3")
                 shinyjs::reset("ip.reps.3")
                 shinyjs::reset("ip.self.3")
                 shinyjs::reset("ip.minimize.3")
                 shinyjs::reset("ip.addx.3")
                 shinyjs::reset("ip.repx.3")
                 shinyjs::reset("ip.selfx.3")
               }
  )
  
  # reset.4 button.
  observeEvent(input$reset.4,
               {
                 shinyjs::enable("run.4")
                 shinyjs::reset("ip.ped.4")
                 shinyjs::reset("header.4")
                 shinyjs::reset("ip.balanced.4")
                 shinyjs::reset("ip.m.4")
                 shinyjs::reset("ip.reps.4")
                 shinyjs::reset("ip.self.4")
                 shinyjs::reset("ip.minimize.4")
                 shinyjs::reset("ip.addx.4")
                 shinyjs::reset("ip.repx.4")
                 shinyjs::reset("ip.selfx.4")
               }
  )
  
  # reset.5 button.
  observeEvent(input$reset.5,
               {
                 shinyjs::enable("run.5")
                 shinyjs::reset("ip.ped.5")
                 shinyjs::reset("header.5")
                 shinyjs::reset("ip.balanced.5")
                 shinyjs::reset("ip.m.5")
                 shinyjs::reset("ip.reps.5")
                 shinyjs::reset("ip.self.5")
                 shinyjs::reset("ip.minimize.5")
                 shinyjs::reset("ip.addx.5")
                 shinyjs::reset("ip.repx.5")
                 shinyjs::reset("ip.selfx.5")
               }
  )
  
  # reset.6 button.
  observeEvent(input$reset.6,
               {
                 shinyjs::enable("run.6")
                 shinyjs::reset("ip.ped.6")
                 shinyjs::reset("header.6")
                 shinyjs::reset("ip.balanced.6")
                 shinyjs::reset("ip.m.6")
                 shinyjs::reset("ip.reps.6")
                 shinyjs::reset("ip.self.6")
                 shinyjs::reset("ip.minimize.6")
                 shinyjs::reset("ip.addx.6")
                 shinyjs::reset("ip.repx.6")
                 shinyjs::reset("ip.selfx.6")
               }
  )
  
  
  ### create a new reactive value called "out".
  out <- reactiveValues()
  
  
  ### convert the runs/resets to 0/1 so the results can be reset as well.
  
  # initial values.
  {
    out$run0.1 <- 0
    out$run0.2 <- 0
    out$run0.3 <- 0
    out$run0.4 <- 0
    out$run0.5 <- 0
    out$run0.6 <- 0
    out$run.1 <- 0
    out$run.2 <- 0
    out$run.3 <- 0
    out$run.4 <- 0
    out$run.5 <- 0
    out$run.6 <- 0
    
    out$reset0 <- 0
    out$reset <- 0
    
    out$reset0.1 <- 0
    out$reset0.2 <- 0
    out$reset0.3 <- 0
    out$reset0.4 <- 0
    out$reset0.5 <- 0
    out$reset0.6 <- 0
    out$reset.1 <- 0
    out$reset.2 <- 0
    out$reset.3 <- 0
    out$reset.4 <- 0
    out$reset.5 <- 0
    out$reset.6 <- 0
  }
  
  # convert run.1.
  observe({
    if(length(input$run.1)>0){
      if((input$run.1 - out$run0.1) > 0){
        out$run0.1 <- input$run.1
        out$run.1 <- 1
        out$reset <- 0
        out$reset.1 <- 0
      }
    }
  })
  
  # convert run.2.
  observe({
    if(length(input$run.2)>0){
      if((input$run.2 - out$run0.2) > 0){
        out$run0.2 <- input$run.2
        out$run.2 <- 1
        out$reset <- 0
        out$reset.2 <- 0
      }
    }
  })
  
  # convert run.3.
  observe({
    if(length(input$run.3)>0){
      if((input$run.3 - out$run0.3) > 0){
        out$run0.3 <- input$run.3
        out$run.3 <- 1
        out$reset <- 0
        out$reset.3 <- 0
      }
    }
  })
  
  # convert run.4.
  observe({
    if(length(input$run.4)>0){
      if((input$run.4 - out$run0.4) > 0){
        out$run0.4 <- input$run.4
        out$run.4 <- 1
        out$reset <- 0
        out$reset.4 <- 0
      }
    }
  })
  
  # convert run.5.
  observe({
    if(length(input$run.5)>0){
      if((input$run.5 - out$run0.5) > 0){
        out$run0.5 <- input$run.5
        out$run.5 <- 1
        out$reset <- 0
        out$reset.5 <- 0
      }
    }
  })
  
  # convert run.6.
  observe({
    if(length(input$run.6)>0){
      if((input$run.6 - out$run0.6) > 0){
        out$run0.6 <- input$run.6
        out$run.6 <- 1
        out$reset <- 0
        out$reset.6 <- 0
      }
    }
  })
  
  # convert reset.
  observe({
    if(length(input$reset)>0){
      if((input$reset - out$reset0) > 0){
        out$reset0 <- input$reset
        out$reset <- 1
        out$run.1 <- 0
        out$run.2 <- 0
        out$run.3 <- 0
        out$run.4 <- 0
        out$run.5 <- 0
        out$run.6 <- 0
      }
    }
  })
  
  # convert reset.1.
  observe({
    if(length(input$reset.1)>0){
      if((input$reset.1 - out$reset0.1) > 0){
        out$reset0.1 <- input$reset.1
        out$reset.1 <- 1
      }
    }
  })
  
  # convert reset.2.
  observe({
    if(length(input$reset.2)>0){
      if((input$reset.2 - out$reset0.2) > 0){
        out$reset0.2 <- input$reset.2
        out$reset.2 <- 1
      }
    }
  })
  
  # convert reset.3.
  observe({
    if(length(input$reset.3)>0){
      if((input$reset.3 - out$reset0.3) > 0){
        out$reset0.3 <- input$reset.3
        out$reset.3 <- 1
      }
    }
  })
  
  # convert reset.4.
  observe({
    if(length(input$reset.4)>0){
      if((input$reset.4 - out$reset0.4) > 0){
        out$reset0.4 <- input$reset.4
        out$reset.4 <- 1
      }
    }
  })
  
  # convert reset.5.
  observe({
    if(length(input$reset.5)>0){
      if((input$reset.5 - out$reset0.5) > 0){
        out$reset0.5 <- input$reset.5
        out$reset.5 <- 1
      }
    }
  })
  
  # convert reset.6.
  observe({
    if(length(input$reset.6)>0){
      if((input$reset.6 - out$reset0.6) > 0){
        out$reset0.6 <- input$reset.6
        out$reset.6 <- 1
      }
    }
  })
  
  
  ### RESULTS - combine available designs into a list.
  
  # combine the magic.eval outputs.
  mpop <- reactive({
    c(if(out$run.1 > 0 & out$reset==0 & out$reset.1==0) list(mpop.1()),
      if(out$run.2 > 0 & out$reset==0 & out$reset.2==0) list(mpop.2()),
      if(out$run.3 > 0 & out$reset==0 & out$reset.3==0) list(mpop.3()),
      if(out$run.4 > 0 & out$reset==0 & out$reset.4==0) list(mpop.4()),
      if(out$run.5 > 0 & out$reset==0 & out$reset.5==0) list(mpop.5()),
      if(out$run.6 > 0 & out$reset==0 & out$reset.6==0) list(mpop.6()))
  })
  
  # combine the design names.
  design <- reactive({
    c(if(out$run.1 > 0 & out$reset==0 & out$reset.1==0) 1,
      if(out$run.2 > 0 & out$reset==0 & out$reset.2==0) 2,
      if(out$run.3 > 0 & out$reset==0 & out$reset.3==0) 3,
      if(out$run.4 > 0 & out$reset==0 & out$reset.4==0) 4,
      if(out$run.5 > 0 & out$reset==0 & out$reset.5==0) 5,
      if(out$run.6 > 0 & out$reset==0 & out$reset.6==0) 6)
  })
  
  
  ### RESULTS - Summary information.
  
  # display the summary information for all designs.
  output$summary <- renderTable({
    if(!is.null(mpop())) magic.summary(input=mpop(), design.names=design())
  }, rownames=TRUE)
  
  
  ### RESULTS - (interval) recombinant haplotypes.
  
  # input: founder pairs to display in plot-interval.
  output$ip.fpair <- renderUI({
    
    fp <- cbind(sort(rep(1:n(), n())), rep(1:n(), n()))
    fp <- fp[!(fp[,1]==fp[,2]), ]
    fp <- paste(fp[,1], fp[,2], sep="-")
    
    selectInput(inputId="ip.fpair",
                label="Founder pairs of recombinant haplotypes",
                choices=fp,
                selected=fp[1],
                multiple=TRUE)
  })
  fpair <- reactive({
    req(input$ip.fpair)
    matrix(as.numeric(unlist(strsplit(input$ip.fpair, "-"))), ncol=2, byrow=TRUE)
  })
  
  # display the plot-interval for all designs.
  output$plot.interval <- renderPlot({
    if(!is.null(mpop())) out$interval <- magic.plot(input=mpop(), display="interval", fpair=fpair(), design.names=design())
  }, height=reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/4 ,0)))
  
  # download plot - interval.
  output$DL.plot.interval <- downloadHandler(
    filename=function(){
      "magicdesign_plot_interval.png"
    },
    content=function(file){
      ggplot2::ggsave(filename=file, plot=out$interval, height=7, width=7, units="in", dpi=600)
    }
  )
  
  
  ### RESULTS - (whole) founder genome distribution.
  
  # display the plot-whole for all designs.
  output$plot.whole <- renderPlot({
    if(!is.null(mpop())) out$whole <- magic.plot(input=mpop(), display="whole", chr.names=chr.names(), annotate=input$annotate, design.names=design())
  }, height=reactive(ifelse(!is.null(input$innerWidth), input$innerWidth*3/4*1.5*(1+ceiling((n.chr()+1)/3)+ceiling(n.chr()/3))/7 ,0)))
  
  # download plot - whole.
  output$DL.plot.whole <- downloadHandler(
    filename=function(){
      "magicdesign_plot_whole.png"
    },
    content=function(file){
      ggplot2::ggsave(filename=file, plot=out$whole, height=1.5*(1+ceiling((n.chr()+1)/3)+ceiling(n.chr()/3)), width=7, units="in", dpi=600)
    }
  )
  
  
  ### RESULTS - Pedigree.
  
  # input: offer pedigree choices to display based on available designs.
  output$ip.pedplot <- renderUI({
    
    pp <- which(c(out$run.1, out$run.2, out$run.3, out$run.4, out$run.5, out$run.6) > 0 &
                  out$reset == 0 &
                  c(out$reset.1, out$reset.2, out$reset.3, out$reset.4, out$reset.5, out$reset.6) == 0)
    
    selectInput(inputId="ip.pedplot",
                label="Design",
                choices=pp)
  })
  pedplot <- reactive({
    req(input$ip.pedplot)
    as.numeric(input$ip.pedplot)
  })
  
  # obtain the pedigree chosen by the user.
  ped.option <- reactive({
    if(pedplot() == 1){
      mpop.1()$ped
    } else if(pedplot() == 2){
      mpop.2()$ped
    } else if(pedplot() == 3){
      mpop.3()$ped
    } else if(pedplot() == 4){
      mpop.4()$ped
    } else if(pedplot() == 5){
      mpop.5()$ped
    } else if(pedplot() == 6){
      mpop.6()$ped
    }
  })
  
  # convert w2h to numeric.
  w2h <- reactive(as.numeric(input$ip.w2h))
  
  # display the pedigree.
  output$ped <- plotly::renderPlotly({
    out$ped <- magic.ped2plot(ped=ped.option(), w2h.ratio=w2h())
  })
  
  # download plot - pedigree.
  output$DL.plot.ped <- downloadHandler(
    filename=function(){
      "magicdesign_pedigree.html"
    },
    content=function(file){
      htmlwidgets::saveWidget(widget=out$ped, file=file)
    }
  )
  
  # download csv - pedigree.
  output$DL.csv.ped <- downloadHandler(
    filename=function(){
      "magicdesign_pedigree.csv"
    },
    content=function(file){
      write.csv(x=ped.option(), file=file, row.names=F, quote=F)
    }
  )
  
  #troubleshooting
  #output$check <- renderPrint(str(out$reset))
  #verbatimTextOutput(outputId="check")
  
}

# Run the application 
shinyApp(ui = ui, server = server)
