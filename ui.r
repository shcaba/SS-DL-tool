

shinyUI(fluidPage(
  titlePanel("Welcome to the Stock Synthesis data-limited tool"),
      h5(p(strong("This tool uses the Stock Synthesis framework to implement the following types of models:"))),
       tags$ul(tags$li(h5(p(em("Length-based estimation of relative biomass, SPR and F."))))),
       tags$ul(tags$li(h5(p(em("Length + catch-based estimation of biomass, relative biomass, SPR and F and catch limits."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Simple Stock Synthesis (SSS) estimator of sustainable catch."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Extended Simple Stock Synthesis (XSSS) estimator of sustainable catch."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Abundance and catch-based estimation of biomass, relative biomass, SPR and F and catch limits."))))),
sidebarLayout(
   sidebarPanel(
    fileInput('file1', 'Choose length composition input file',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv'
              )
            ),
    fileInput('file2', 'Choose catch time series input file',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv'
              )
            ),
    fileInput('file3', 'Choose abundance series input file',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv'
              )
            ),

    fluidRow(column(width=4,textInput("Scenario_name", "Scenario label", value="Scenario 1"))),
    h3("Model Dimension inputs"),
    h5(p(em(""))),
    wellPanel(
      fluidRow(column(width=4,numericInput("styr", "Starting year", value=1980,min=1, max=10000, step=1)),
              column(width=4,numericInput("endyr","Ending year", value=2019,min=1, max=10000, step=1)),    
              column(width=4,numericInput("Nages","Max. age", value=30,min=1, max=1000, step=1))),    
    ),
    wellPanel(
    h3("Life history inputs"),
    wellPanel(
      h5(em("Female")),
      fluidRow(column(width=6,numericInput("M_f", "Natural mortality", value=NA,min=0, max=10000, step=0.01)),
             column(width=6,numericInput("k_f","Growth coefficient k", value=NA,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("Linf_f", "Asymptotic size (Linf)", value=NA,min=0, max=10000, step=0.01)),
              column(width=6,numericInput("t0_f","Age at length 0 (t0)", value=NA,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("L50_f", "Length at 50% maturity", value=NA,min=0, max=10000, step=-0.01)),
              column(width=6,numericInput("L95_f","Length at 95% maturity", value=NA,min=0, max=10000, step=0.01))),    
    ),
    fluidRow(column(width=10,checkboxInput("male_parms","Males specific values?",FALSE))),
    #fluidRow(column(width=7, h3("Males specific values?")),column(width=2,checkboxInput("male_parms","Males specific values?",FALSE,width="150%"))),
    wellPanel(
                   uiOutput("Male_parms_inputs_label"),
                   uiOutput("Male_parms_inputs1"),
                   uiOutput("Male_parms_inputs2")
    ),
    ),
    h3("Productivity"),
    fluidRow(column(width=6,numericInput("h","Steepness", value=0.9,min=0.2, max=1, step=0.01)),
      column(width=6,numericInput("lnR0", "Initial recruitment (lnR0)", value=9,min=0, max=20, step=0.01))),
    h3("Selectivity"),
    fluidRow(selectInput("Sel_choice","Length selectivity type",c("Logistic","Dome-shaped"))),
    fluidRow(column(width=6,numericInput("Sel50", "Length at 50% Selectivity", value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("Sel50_phase","Estimation phase", value=1,min=-1000, max=10, step=1))),   
    fluidRow(column(width=6,numericInput("Selpeak", "Length at Peak Selectvity", value=NA,min=0, max=10000, step=0.01)),
            column(width=6,numericInput("Selpeak_phase","Estimation phase", value=1,min=-1000, max=10, step=1))),   
   
   #Recruitment estimation
    fluidRow(column(width=10,checkboxInput("rec_choice","Estimate recruitment?",FALSE))),
    wellPanel(
    #          fluidRow(column(width=8,offset=-10, h3("Estimate recruitment?")),column(width=6,checkboxInput("rec_choice","",FALSE))),
    #          fluidRow(column(width=8, h3("Estimate recruitment?")),column(width=4,radioButtons("rec_choice","",FALSE))),
   tags$style("
       .checkbox { /* checkbox is a div class*/
       #  line-height: 10px;
         margin-bottom: 0px; 
        margin-left: 0px;
        font-size: 20px;
       }
      input[type='checkbox']{ /* style for checkboxes */
          width: 20px; /*Desired width*/
          height: 20px; /*Desired height*/
          line-height: 100px; 
        
        # span { 
        #   margin-left: 30px;  /*set the margin, so boxes don't overlap labels*/
        #   line-height: 30px; 
      }

       }"),
                   uiOutput("Rec_options1"),
                   uiOutput("Rec_options2")
                ),
     fluidRow(column(width=10,checkboxInput("jitter_choice","Jitter starting values?",FALSE))),
    wellPanel(
            uiOutput("Jitter_value")
              ),
    actionButton("run_SS",strong("Run Model"),
      width="100%",
      icon("play-circle"),
      style="font-size:120%;border:2px solid;background:#ccffcc"),
    
  ),
      mainPanel(        
        tabsetPanel(
            tabPanel("Data and Parameters",
            h4("Length composition data"),
            plotOutput("Ltplot"),
            h4("Catch data"),
            plotOutput("Ctplot"),
            h4("Life history"),
            column(6,plotOutput("Mplot")),
            column(6,plotOutput("VBGFplot"))
                  )       
          ) 
   )
   )
)
)

