library(shiny)

# ui.R
# GUI for the whole program

shinyUI(navbarPage("",
                   
                   tabPanel("Basic Simulation",
                            
                            titlePanel("Basic Simulation"),
                            
                            h4("Please visit the Instructions tab to learn about how to use this program!"),
                            
                            tableOutput("test"),
                            # textOutput("test"),
                            
                            sidebarLayout(
                                    sidebarPanel(
                                            numericInput("q", label = "q", value = 0.8),
                                            sliderInput("E.init", label = "initial E", value = 0,
                                                        min = -1, max = 1, step = 0.05),
                                            sliderInput("S.plus", label = "S max", value = 0.5,
                                                        min = 0, max = 1, step = 0.05),
                                            sliderInput("d", label = "d", value = 0.2,
                                                        min = 0, max = 1, step = 0.05),
                                            sliderInput("C.init", label = "initial C", value = 0,
                                                        min = 0, max = 1, step = 0.05),
                                            sliderInput("A.init", label = "initial addictive acts", value = 0.4,
                                                        min = 0, max = 0.8, step = 0.05),
                                            numericInput("lamda.init", label = "inital lamda", value = 0.5),
                                            numericInput("weeks", label = "No. of weeks", value = 25,
                                                         step = 1, min = 1, max = 1000),
                                            numericInput("no.simulations", label = "No. of simulations",
                                                         value = 100, step = 1, min = 0, max = 3000)
                                            
                                    ),
                                    
                                    mainPanel(
                                            wellPanel(
                                                    h4("Output Options"),
                                                    checkboxInput("display.success", label = "Display success output"),
                                                    helpText("Out of all simulation runs, how often is patient addicted at the end."),
                                                    br(),
                                                    selectInput("graph.type", label = "Graph Type",
                                                                choices = list("A over time" = 1, "C over time" = 2,
                                                                               "S over time" = 3, "V over time" = 4,
                                                                               "A & C over time" = 5, "S & V over time" = 6)),
                                                    checkboxInput("graph.success", label = "Display successful run",
                                                                  value = TRUE),
                                                    helpText("Display a simulation where a participant is NOT addicted at the end.")
                                                    
                                            )#,
                                            
                                            # plotOutput("time.plot"),
                                            
                                            # textOutput("success.rate")
                                    )
                            )
                   ),
                   
                   tabPanel("Parameters Explained",
                            h4("A"),
                            p("A represents the number of addictive acts of a patient."),
                            br(),
                            h4("C"),
                            p("Craving of a person for the addictive substance."),
                            br(),
                            h4("d"),
                            p("A paramter indicating how quickly craving decays. If craving never gets smaller,
                               d would be 0. If craving is instantly gone, d would be 1. A realistic value for
                               alcohol addiction is 0.2 ."),
                            br(),
                            h4("initial A"),
                            p("How often the person uses the addictive substance at the beginning of the simulation."),
                            br(),
                            h4("initial C"),
                            p("The craving for the addictive substance a patient has at the beginning of the simulation."),
                            br(),
                            h4("initial E"),
                            p("A start parameter for how many events are expected that trigger the addicive action.
                              For example, how often is there a group setting in which people might be tempted to drink.
                              E ranges between -1 and 1."),
                            br(),
                            h4("initial lamda"),
                            p("Represents the intensity of external influences, i.e. how much random external influences
                              influence the patient."),
                            br(),
                            h4("No. of weeks and simulations"),
                            p("For how many weeks should the simulation be run? How often should the simulation be repeated?
                               Note that very large numbers can cause lomg waiting times and possibly program crashes."),
                            h4("S"),
                            p("Self-control of a person."),
                            br(),
                            h4("S max"),
                            p("The maximum self-control of a person with which the simulation starts."),
                            br(),
                            h4("q"),
                            p("q is the maximum consumption of the addictive substance per week. 
                                  For example, for alcohol we choose 0.8 representing 80 drinks per week."),
                            br(),
                            h4("V"),
                            p("An auxiliary variable calculated from C, S, and A. It can be conceptualized as addiction
                              vulnerability. If it is 1 the patient is addicted, if it is 0 the patient is not addicted. V
                              is used to calculate the success rate of the simulations."),
                            br()
                   ),
                   
                   tabPanel("Instructions",
                            h2("Welcome to SADE!"),
                            h4("What is SADE?"),
                            h4("How do I use SADE?"),
                            h4("What options can I select?"),
                            h4("Where can I get the app?")
                   ),
                   
                   navbarMenu("More",
                              tabPanel("Bifurcation Diagram",
                                       "Make Bifurcs here."
                              ),
                              
                              tabPanel("Therapy Success",
                                       "Make stuff here"
                              ),
                              
                              tabPanel("Data Download",
                                       "Make stuff here."
                              )
                              
                   )
                   
)
)