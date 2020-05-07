require(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Welcome to the Stock Synthesis data-limited tool"),
      h5(p(strong("This tool uses the Stock Synthesis framework to implement the following types of models:"))),
       tags$ul(tags$li(h5(p(em("Length-based estimation of relative biomass, SPR and F."))))),
       tags$ul(tags$li(h5(p(em("Length + catch-based estimation of biomass, relative biomass, SPR and F and catch limits."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Simple Stock Synthesis (SSS) estimator of sustainable catch."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Extended Simple Stock Synthesis (XSSS) estimator of sustainable catch."))))),
       tags$ul(tags$li(h5(p(em("COMING SOON: Abundance and catch-based estimation of biomass, relative biomass, SPR and F and catch limits."))))),
sidebarLayout(
   sidebarPanel(
wellPanel(
  h4(strong("Choose data file")),
  fluidRow(column(width=12,fileInput('file1', 'Length composition',
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv'
                                    )
  ))),
  fileInput('file3', 'Age composition',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv'
            )
          ),

 fluidRow(column(width=12,fileInput('file2', 'Catch time series',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv'
                           )
  ))),
 
  #Mute for now, pull back in when index methods are ready
  # fileInput('file3', 'Abundance index',
  #           accept = c(
  #             'text/csv',
  #             'text/comma-separated-values',
  #             'text/tab-separated-values',
  #             'text/plain',
  #             '.csv'
  #           )
  #         ),
  h4(strong("Clear data files")),
     fluidRow(column(width=4,actionButton("reset_lt", "Length")),
              column(width=4,actionButton("reset_age", "Ages")),
              column(width=4,actionButton("reset_ct", "Catches"))),
),
    
#    fluidRow(column(width=10,checkboxInput("mod_sims","Specify model years?",FALSE))),
    wellPanel(
    h3("Model years"),
    h5(p(em("Starting values based on data"))),
      tags$ul(tags$li(h5(p(em("If using only length or age data, starting model year is based on earliest year minus age at 95% Linf"))))),
    # h5(p(em("Start year recommendations are:"))),
    #   tags$ul(tags$li(h5(p(em("If length data only, count the year back from the first year of length data based on maximum age likely contained in length data"))))),
    #   tags$ul(tags$li(h5(p(em("If using catch data, use the first year of catches"))))),
    # h5(p(em(""))),
 #   wellPanel(
        uiOutput("Model_dims1"),
      ),
 #   ),
    wellPanel(
    h3("Life history inputs"),
    wellPanel(
      h5(em("Female")),
      fluidRow(column(width=6,numericInput("Nages","Max. age", value=NA,min=1, max=1000, step=1)),
             column(width=6,numericInput("M_f", "Natural mortality", value=NA,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("Linf_f", "Asymptotic size (Linf)", value=NA,min=0, max=10000, step=0.01)),
              column(width=6,numericInput("k_f","Growth coefficient k", value=NA,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("t0_f","Age at length 0 (t0)", value=NA,min=0, max=10000, step=0.01)),
              column(width=6,numericInput("CV_lt_f","CV at length", value=0.1,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("L50_f", "Length at 50% maturity", value=NA,min=0, max=10000, step=0.01)),
              column(width=6,numericInput("L95_f","Length at 95% maturity", value=NA,min=0, max=10000, step=0.01))),    
      fluidRow(column(width=6,numericInput("WLa_f", "Weight-Length alpha", value=0.00001,min=0, max=10000, step=0.000000001)),
              column(width=6,numericInput("WLb_f","Weight-length beta", value=3,min=0, max=10000, step=0.01))),    
      # fluidRow(column(width=6,numericInput("Fec_a", "Length at 50% maturity", value=NA,min=0, max=10000, step=-0.01)),
      #         column(width=6,numericInput("Fec_b","Length at 95% maturity", value=NA,min=0, max=10000, step=0.01))),    
    ),
    fluidRow(column(width=10,checkboxInput("male_parms","Males specific values?",FALSE))),
    #fluidRow(column(width=7, h3("Males specific values?")),column(width=2,checkboxInput("male_parms","Males specific values?",FALSE,width="150%"))),
    wellPanel(
                   uiOutput("Male_parms_inputs_label"),
                   uiOutput("Male_parms_inputs1"),
                   uiOutput("Male_parms_inputs2"),
                   uiOutput("Male_parms_inputs3"),
                   uiOutput("Male_parms_inputs4")
    ),
    ),
#    shinysj::hide()
    wellPanel(
    h3("Productivity"),
    fluidRow(column(width=6,numericInput("h","Steepness", value=0.7,min=0.2, max=1, step=0.01)),
      column(width=6,numericInput("lnR0", "Initial recruitment (lnR0)", value=9,min=0, max=20, step=0.01))),
    ),

    wellPanel(
    h3("Selectivity"),
    h5("The phase input indicates estimated parameters. To fix the parameter, set the phase value to a negative number"),
    fluidRow(selectInput("Sel_choice","Length selectivity type",c("Logistic","Dome-shaped"))),
    # fluidRow(column(width=6,numericInput("Sel50", "Length at 50% Selectivity", value=NA,min=0, max=10000, step=0.01)),
    #         column(width=6,numericInput("Sel50_phase","Estimation phase", value=1,min=-1000, max=10, step=1))),   
    # fluidRow(column(width=6,numericInput("Selpeak", "Length at Peak Selectvity", value=NA,min=0, max=10000, step=0.01)),
    #         column(width=6,numericInput("Selpeak_phase","Estimation phase", value=1,min=-1000, max=10, step=1))),   
              #if(input$Sel_choice=="Logistic")
              #  {uiOutput("Sel_logistic")},
              #if(input$Sel_choice=="Dome-shaped")
              #  {uiOutput("Sel_domed")}
       uiOutput("Sel_parms1"),
       uiOutput("Sel_parms2"),
       uiOutput("Sel_parms3"),
       uiOutput("Sel_parms4"),
       uiOutput("Sel_parms5")
       
   ),
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
            uiOutput("Jitter_value"),
            h5("Jittering refers to changing the input starting values."),
            h5("Jittering provides a quick way to adjust starting values for two main purposes:"),
              tags$ul(tags$li(h5(p("Start the model at different values to assist model convergence.")))),
              tags$ul(tags$li(h5(p("Validate global versus local model convergence. This requires running many models at different jittered starting values to make sure a lower minimized likelihood value is not found. If a lower likelihood value is found, that would be considered the best fit model.")))),
            h5("Run just 1 jitter value to find a converged model. Then run multiple jittered models to confrim that model is the best fit model."),
              ),

     fluidRow(column(width=10,checkboxInput("RP_choices","Define reference points?",FALSE))),
    wellPanel(
          uiOutput("RP_selection1"),
          uiOutput("RP_selection2")
        ),
     
    fluidRow(column(width=10,checkboxInput("Forecast_choice","Define forecasts?",FALSE))),
    wellPanel(
          uiOutput("Forecasts")
        ),
 
    wellPanel(
    fluidRow(column(width=8,textInput("Scenario_name", strong("Choose the name of your scenario"), value="Scenario 1"))),
    h5(p(em("Each scenario folder is saved. Changing the scenario name therefore creates a new folder of results."))),
    h5(p(em("Using different scenario names when changing data or parameter values allows easy sensitivity exploration."))),
    h5(p(em(""))),
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
            h4("Age composition data"),
            plotOutput("Ageplot"),
            h4("Catch data"),
            plotOutput("Ctplot"),
            h4("Life history"),
            column(6,plotOutput("Mplot")),
            column(6,plotOutput("VBGFplot"))
                  ),       
          tabPanel("Model output",
            h4("Checking model convergence. Check also fit to length composition data"),
            tableOutput("converge.grad"),
            tableOutput("converge.covar"),
            tableOutput("converge.dec"),
            tags$head(tags$style("#converge.grad{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                         )
              ),
            tags$head(tags$style("#converge.dec{color: green;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                         )
              ),
            br(),
            h4("Relative spawning output"),
            tableOutput("SSout_relSB_table"),
            br(),
            h4("Fishing intensity"),
            tableOutput("SSout_F_table"),
            br(),
            h4("Estimated parameters"),
            tableOutput("Parameters_table"),
            br(),
            h4("Time series"),
            tableOutput("SSout_table")
            ),
          tabPanel("Jitter exploration",
            plotOutput("Jitterplot"),
            h4("Blue values indicate minimum likelihood values; red indicate values higher than the minimum."),  
            h4("Any iteration with a blue value can be used as the new best-fit (reference) model.")  
            ),
          tabPanel("Likelihood profile",
            
            ),
          tabPanel("Sensitivity comparisons",
            
            )
          ) 
   )
   )
)
)

