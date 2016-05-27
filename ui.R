library(shiny)

# ui.R
# GUI for the whole program

shinyUI(navbarPage("", 
                   inverse = TRUE,
                   windowTitle = "SimulADE",
                   
                   tabPanel("Basic Simulation",
                            
                            titlePanel("Basic Simulation"),
                            
                            h4("Please visit the Instructions tab to learn how you can use this program!"),
                           
                            sidebarLayout(
                                    # Parameter Input
                                    sidebarPanel(
                                            checkboxInput("custom", label = "Do you want to customize the parameters?",
                                                          value = FALSE),
                                            conditionalPanel("input.custom == 1", 
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
                                                             numericInput("lamda.init", label = "inital lamda", value = 0,
                                                                          min = 0),
                                                             numericInput("weeks", label = "No. of weeks", value = 25,
                                                                          step = 1, min = 1, max = 1000),
                                                             numericInput("no.simulations", label = "No. of simulations",
                                                                          value = 100, step = 1, min = 0, max = 3000),
                                                             actionButton("reset", label = "Reset parameters")
                                            )
                                    ),
                                    
                                    mainPanel(
                                            wellPanel(
                                                    # Output parameter input
                                                    selectInput("graph.type", label = "Graph Type",
                                                                choices = list("A over time" = 1, "C over time" = 2,
                                                                               "S over time" = 3, "V over time" = 4,
                                                                               "A & C over time" = 5, "S & V over time" = 6)),
                                                    checkboxInput("ds", label = "Display success output: Percent of simulation runs in which the patient is ``clean'' at the end."),
                                                    checkboxInput("graph.success", label = "Display successful run",
                                                                  value = 1),
                                                    br(),
                                                    # Success Output
                                                    conditionalPanel("input.ds != 0",
                                                                     wellPanel(
                                                                             h4(textOutput("success_rate"))
                                                                     )
                                                    )
                                            ),
                                            
                                            # Plot Output
                                            plotOutput("time.plot")
                                    )
                            )
                   ),
                   
                   # Page to explain parameters
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
                            br(),
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
                   
                   # Instructions and About
                   tabPanel("Instructions",
                            h2("Welcome to SimulADE!"),
                            br(),
                            br(),
                            h4("What is SimulADE?"),
                            p("SimulADE stand for ", strong("Simul"), "ation of ", strong("A"), "ddiction with ",
                              strong("D"), "ifference ", strong("E"), "quations. In this program you can insert a number of ",
                              "parameters and see how this might affect the progress of addiction. You can also investigate ",
                              "into stable states in addictions with bifurcation diagrams or you can check how successfull ",
                              "a certain kind of addiction theraphy could be. ", em("This feature is not implemented quite yet! Sorry!"),
                              " Please note, however, that this program was only tested for alcohol addiction. This means it ", strong("might"),
                              "work for other addictions as well but I cannot guarantee it. If this app is developed further, it might include",
                              "options for other addictions."),
                            p("Even though SimulADE now seems like a great thing, it really is only the outer shell of a model by Grasman, Grasman, and van ",
                              "der Maas (2016). Their paper describes the model in full detail and gives explanations",
                              "to the interested reader. The paper's citation is:"),
                            br(),
                            p("Grasman, J., Grasman, R. P. P. P., & van der Maas, H. L. J. (2016). ", em("The dynamics of addiction: craving versus self-control"), ". Manuscript submitted for revision."),
                            br(),
                            br(),
                            h4("How do I use SimulADE?"),
                            p("You can use SimulADE in multiple ways. You could try to understand the model by Grasman, Grasman, and van der",
                              "Maas (2016) better. Or you could be interested in how addictions progress. Or you could just be looking",
                              "around for something to do. In any case: You should try to have fun with this app. Try out multiple options",
                              " checkout what this and that does. This is how you will probably get the most out of it. If you are looking",
                              "for further directions - read on."),
                            p("First and foremost, you should start on the ", em("Basic Simulation"), " page. Here you can already see a graph",
                              " and select output options. You can choose to display different addiction parameters over time. Moreover, you ",
                              "can let SimulADE know whether you want to see a ``successfull´´ output or not - so one in which the patient is NOT addicted",
                              " (read: in which he is ``clean´´) at the end of the time period. Sometimes, there are no ``successfull´´ or ``unsuccessfull´´ ",
                              "outputs. You can check this by ticking the ", em("Display Success Output"), " box. There you can see how often the ",
                              "patient is NOT addicted at the end of the time period."),
                            p("As a next step you might want to change some parameters yourself. Check the box on the left, ",
                              em("Customize parameters"), ", and get going. The graphs will update immediately, but note that unrealistic",
                              "inputs might also cause weird outputs - data doesn't think for itself (what my old stats teacher used to say).",
                              "While changing parameters back and forth have a look at the ", em("Parameters Explained"), " tab to learn what",
                              "you are changing and where in the model this affects something. If you want to go back to default, click the ",
                              em("Reset"), " button."),
                            p("Note that the graph only displays the data from one simulation, the first to be exact. Hence, it might be that ",
                              "it is one not representative of the other simulations. At the moment, the program does not provide an option to ",
                              "select another simulation, but you can download all simulation files in the ", em("Downloads"), " tab. If you liked",
                              "the graph that was produced, you can download it there too."),
                            p("If you want to go a step further, check out the ", em("Bifurcation Diagram"), " tab to investigate where the ",
                              "stable states are in this model. For further info on that, delve into the paper by Grasman, Grasman, and van der Maas",
                              " (2016)."),
                            p("If you want to know more and cannot find info in the paper or on this site, feel free to contact me ",
                              a("via e-mail.", href = "mailto: marie.ritter@student.uva.nl"), "."),
                            br(),
                            br(),
                            h4("Where can I get the app?"),
                            p("If you want to explore this app and how it works further, please go to my", a("GitHub Repo", href = "https://github.com/MarieRitterUva/ProgrammingNextStep.git"),
                              ". You can download all files and run them in your local R. If you do that, checkout the showcase mode",
                              "of Shiny R to get the code displayed next to the app."),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br()
                   ),
                   
                   navbarMenu("More",
                              tabPanel("Bifurcation Diagram",
                                       h3("Create Bifurcation diagram"),
                                       # Bifurcation Parameter Input
                                       wellPanel(
                                               "To create a bifurction diagram please choose from these two options and hit submit.",
                                               fluidRow(
                                                       column(5,
                                                              selectInput("bifurc", label = "Choose a bifurcation parameter:",
                                                                          choices = list("E" = 1,
                                                                                         "S plus" = 2,
                                                                                         "d" = 3,
                                                                                         "initial C" = 4,
                                                                                         "lamda" = 5,
                                                                                         "inital A" = 6),
                                                                          selected = 1
                                                              )
                                                       ),
                                                       column(5,
                                                              selectInput("Y", label = "Choose your Y parameter:",
                                                                          choices = list("S" = "S", "C" = "C"),
                                                                          selected = "C"
                                                              )
                                                       )
                                               ),
                                               
                                               actionButton("go.bifurc", label = "Make bifurcation diagram!")
                                       ),
                                       
                                       plotOutput("bifurcation")
                              ),
                              
                              # Therapy Simulation
                              tabPanel("Therapy Success",
                                       h4("Sorry!"),
                                       p("Sorry, but this feature is still in development.",
                                         " Why not try another feature first and come back in a couple of weeks for this?")
                              ),
                              
                              # Download page for data and graph
                              tabPanel("Downloads",
                                       wellPanel(
                                               h4("Data Download"),
                                               p("You can dowload the data you just simulated as a *.zip file. This",
                                                 "file will contain multiple *.csv files - one for each of the simulation runs. ",
                                                 "So if you chose", em("no.simulations"), "to be 100, you will have 1 *.zip file with 100 *.csv files,",
                                                 "which you can use to create your own plot, run analyses, or just store for later usage.",
                                                 "Have fun with them!"),
                                               downloadButton("downloadData", "Give me those files! - Download")
                                       ),
                                       wellPanel(
                                               h4("Plot Download"),
                                               p("If you liked the plot you produced, you can download it here. Note that",
                                                 "you can only download the plot from ", em("Basic Simulation"), "at this",
                                                 "point and that you have to set all parameters on the ", em("Basic Simulation"),
                                                 "page ", strong("before"), "you download it here."),
                                               downloadButton("downloadPlot", "I want the plot! - Download")
                                       )
                              )
                              
                   )
                   
)
)