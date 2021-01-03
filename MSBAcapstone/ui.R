# ui for the app

#build application header
header <- dashboardHeader(title = 'Team Worm Burner Capstone Application')

#build application sidebar
sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem('Welcome', tabName = 'welcome', icon = icon('golf-ball', "font-awesome")),
            menuItem('Data Exploration', tabName = 'dataExplore', icon = icon('chart-line', "font-awesome"), startExpanded = FALSE,
                     menuSubItem("Summary Statistics", tabName = 'summary'),
                     menuSubItem("Frequency Charts", tabName = 'distro'),
                     menuSubItem('Defect Examination', tabName = 'outlier'),
                     menuSubItem("Correlation Charts", tabName = 'corr'),
                     menuSubItem("Variable Relationships", tabName = 'relation'),
                     menuSubItem('Group to Variable comparison', tabName = 'tester'),
                     menuSubItem('Clustering', tabName = 'cluster')),
            menuItem('Model', tabName = 'model', icon = icon('laptop', lib = 'font-awesome'), startExpanded = FALSE,
                     menuSubItem('Swing Profile Tool', tabName = 'tool')),
            menuItem('Meet the Team', tabName = 'team', icon = icon('user-friends', "font-awesome"))
        ))

#build application body
body <- dashboardBody(
        tabItems(
                
            tabItem(tabName = 'welcome',
                    h2("Welcome to my Capstone Application"),
                    img(src='golfBall.jpg', height=300, width=400, align = "center"),
                    h3("Executive Summary"),
                    p("This application uses randomly generated data due to NDA restrictions. It is designed to show the capabilities of what I built and should not be used to accurately pick a golf club"),
                    ),
            
            tabItem(tabName = 'summary',
                        h2("Summary Statistics", align = 'center'),
                        fluidRow(
                            verbatimTextOutput('sum')
                        )
                    ),
        
            tabItem(tabName = 'distro',
                        h2("Frequency Charts", align = 'center'),
                        fluidRow(
                            box(plotOutput("tester", height = 350)),
                            box(plotOutput("group", height = 350)),
                            box(plotOutput("fullClub", height = 350)),
                            box(plotOutput("testDate", height = 350))),
                        fluidRow(
                            column(12,plotOutput("testTime"))
                        )
                    ),
            
            tabItem(tabName = 'outlier',
                    h2('Defect Examination', align = 'center'),
                    fluidRow(
                        column(12, selectInput('var5', 'Variable', choices = colnames(justNum), selected = 'Total_Yards'))),
                    fluidRow(
                        box(plotOutput('out', height = 450)),
                        box(plotOutput('out2', height = 450)),
                        box(plotOutput('out3', height = 250)),
                        box(plotOutput('out4', height = 250))
                    )
            ),
            
            tabItem(tabName = 'corr',
                    h2('Correlation Charts', align = 'center'),
                    fluidRow(
                        plotOutput('corr1', height = 700),
                        )
                    ),
            
            tabItem(tabName = 'relation',
                    h2('Variable Relationships', align = 'center'),
                    fluidRow(
                        column(4, selectInput('var1', 'First Variable', choices = colnames(justNum), selected = 'Club_Speed')),
                        column(4, selectInput('var2', 'Second Variable', choices = colnames(justNum), selected = 'Total_Yards')),
                        column(4, selectInput('var12', 'View selection', choices = c('All', 'Without Defects', 'Only Defects')))
                    ),
                    fluidRow(
                        column(12, plotOutput('relation', height = 550))
                    ),
            ),
            
            tabItem(tabName = 'tester',
                    h2('Group to Variable Comparison', align = 'center'),
                    fluidRow(
                        column(4, selectInput('var3', 'Group to examine', choices = colnames(catOnly))),
                        column(4, selectInput('var4', 'Variable to examine', choices = colnames(justNum), selected = 'Total_Yards'))
                        ),
                    fluidRow(
                        column(12, plotOutput('compare', height = 550))
                        )
                    ),
            
            tabItem(tabName = 'cluster',
                    h2('Simple Cluster Analysis of Variables', align = 'center'),
                    fluidRow(
                        column(4, selectInput('var6', 'X axis', choices = colnames(justNum), selected = 'Total_Yards')),
                        column(4, selectInput('var7', 'Y axis', choices = colnames(justNum), selected = 'Club_Speed')),
                        column(4, selectInput('var8', 'Group', choices = colnames(catOnly), selected = 'Tester_ID')),
                        column(12, plotOutput('clus', height = 700))
                        )
                    ),
            
            tabItem(tabName = 'tool',
                    h2('Swing Profile Tool', align = 'center'),
                    h3('Please select your profile', align = 'left'),
                    helpText('Adjust the sliders to match the closest fit to your swing profile'),
                    fluidRow(
                            column(3, selectInput('club', 'Club', choices = sort(unique(as.character(catOnly$Full_Club_Name))), 
                                                  selected = 'Mizuno ST200')),
                            column(3, sliderInput('Club_Speed','Club Speed', min = 60,
                                                 max = 140, 100, step = .1)),
                            column(3, sliderInput('Angle_Of_Attack','Angle of Attack', min = -30,
                                                  max = 30, 5.8, step = .1)),
                            column(3, sliderInput('Club_Path','Club Path', min = -20,
                                                  max = 20, 9.4, step = .1)),
                            column(3, sliderInput('Facet_To_Path','Face to Path', min = -40,
                                                  max = 40, 0.9, step = .1)),
                            column(3, sliderInput('Lie','Lie', min = -10,
                                                  max = 20, 5.4, step = .1)),
                            column(3, sliderInput('Loft','Loft', min = -10,
                                                  max = 50, 24.8, step = .1)),
                            column(3, sliderInput('Closure_Rate','Closure Rate', min = 0,
                                                  max = 6000, 2356.8, step = .1)),
                            column(6, sliderInput('Face_Impact_Lateral','Face Impact Lateral', min = -70,
                                                  max = 60, 31, step = .1)),
                            column(6, sliderInput('Face_Impact_Vertical','Face Impact Vertical', min = -100,
                                                  max = 140, -4.1, step = .1)),
                            br(),
                            h3('Your projected results', align = 'left'),
                            column(4, verbatimTextOutput('Pred')),
                            column(4, verbatimTextOutput("Pred2")),
                            column(4,p('   ')),
                            br(),
                            fluidRow(
                                column(5, h3('Top 5 Clubs to maximize Total Yards', align = 'left')),
                                column(5, h3('Top 5 Clubs to minimize Offline Yards', align = 'left')),
                                br(),
                                column(4, verbatimTextOutput("recommend")),
                                column(4, verbatimTextOutput('recommend2'))
                            )
                                   
                    )),
            
            tabItem(tabName = 'team',
                    h2("Meet the team", align = 'center'),
                    h3('Steve Hojnicki', align = 'left'),
                    img(src='hojnicki.png', height=300, width=250, align = "center"),
                    p("Steve hails from Burke, Va. He received his bachelor’s degree in Systems Management and commission as a Field Artillery officer from the United States Military Academy in 2009. Since graduation he has served extensively throughout Europe, Asia and the Middle East in a variety of operational assignments. He is currently pursuing a Masters of Science in Business Analytics from William & Mary while serving at the US Army's Futures and Concepts Center as an Operations Research and Systems Analyst. Connect with me on", tags$a(href="https://www.linkedin.com/in/stevenhojnicki/","LinkedIn"),),
                    )
        )
)

#combine items into ui
ui <- dashboardPage(skin = 'green', header, sidebar, body)