library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

load("VideoGameSalesClean.rdata")

ui <- dashboardPage(
    dashboardHeader(title = "Video Game Sales Data"),                   # Title of the dashboard
    dashboardSidebar( 
    #     sidebarPanel(
    #         tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: blue}")),
    #         menuItem('Bins',
    #         sliderInput('bins',
    #                     'Number of Bins-Year',
    #                     min=1,
    #                     max = 10,
    #                     value=5),
    #         sliderInput('bins1',
    #                     'Number of Bins-Global Sales:',
    #                     min=20,
    #                     max = 100,
    #                     value=50)
    #         )),
        column(12, h2("Game Details")), 
        menuItem('Bins',
        sliderInput('bins',
                    'Number of Bins-Year',
                    min=1,
                    max = 10,
                    value=5),
        sliderInput('bins1',
                    'Number of Bins-Global Sales:',
                    min=20,
                    max = 100,
                    value=50)
        ),
        menuItem('Year',
        sliderInput("yearofrelease", h4("Year"),                        # Element 1: year selection
                        min = 1985, max = 2020,                      
                        value = c(1985, 2020), step = 5, sep = "")
        ),
        menuItem('Sales Details',
        sliderInput("globalsales",h4('Global Sales'),
                    min=0, max=85,
                    value=c(0,85),step=5, sep = '')
        ),
        menuItem('Scores',
        sliderInput("criticscore",h4('Critic Score'),
                    min=0, max = 100,
                    value=c(0,100),step=10, sep=''),
        sliderInput("userscore",h4('User Score'),
                    min=0, max = 10,
                    value=c(0,100),step=10, sep='')
        ),
        menuItem('Game Details',
        checkboxGroupInput("platform", h4("Platform"),               # Element 2: season selection
                           choices = list("3DS" = "3DS", 
                                          "DS" = "DS",
                                          'DC'='DC',
                                          'GBA'='GBA',
                                          'GC'='GC',
                                          'PC'='PC',
                                          'PS'='PS',
                                          'PS2'='PS2',
                                          'PS3'='PS3',
                                          'PS4'='PS4',
                                          'PSP'='PSP',
                                          'PSV'='PSV',
                                          'Wii'='Wii',
                                          'WiiU'='WiiU',
                                          'X'='X',
                                          'X360'='X360',
                                          'XOne'='XOne'),
                           selected = c("3DS", "DS","DC","GBA","GC","PC","PS","PS2","PS3","PS4","PSP","PSV","Wii","WiiU","X","X360","XOne")),
        checkboxGroupInput("genre", h4("Genre"),               # Element 2: season selection
                           choices = list('Action'="Action",
                                          'Adventure'="Adventure",
                                          'Fighting'="Fighting",
                                          'Misc'="Misc",
                                          'Platform'="Platform",
                                          'Puzzle'="Puzzle",
                                          'Racing'="Racing",
                                          'Role-Playing'="Role-Playing",
                                          'Shooter'="Shooter",
                                          'Simulation'="Simulation",
                                          'Sports'="Sports",
                                          'Strategy'="Strategy"),
                           selected = c('Action','Adventure','Fighting','Misc','Platform','Puzzle','Racing','Role-Playing','Shooter','Simulation','Sports','Strategy')),
        checkboxGroupInput("rating",h4("Rating"),
                           choices=list('AO'='AO',
                                        'E'='E',
                                        'E10+'='E10+',
                                        'K-A'='K-A',
                                        'M'='M',
                                        'RP'='RP',
                                        'T'='T'),
                           selected = c('AO'='AO','E'='E','E10+'='E10+','K-A'='K-A','M'='M','RP'='RP','T'='T'))),
    numericInput('max_genre',h4('Max Number of Genre'),
                 min=2,max=12,step=1,value=12),
    numericInput('max_platform',h4('Max Number of Platform'),
                 min=2,max=17,step=1,value=17),
    numericInput('max_publisher',h4('Max Number of Publisher'),
                 min=2,max=20,step=1,value=10)
    ),
    
    ####### NEW ELEMENTS BELOW !!!!!
    dashboardBody(
        infoBoxOutput('box1',width = 5),
        infoBoxOutput('box2',width = 5),
        infoBoxOutput('box7',width=4),
        infoBoxOutput('box8',width=4),
        fluidRow(
            tabBox(
                title='Visuals',height= '870px', width=12,
                tabPanel('Table Overviews',
                         DT::dataTableOutput("table_1")
                        ),
                tabPanel('Year Release Overview',
                         plotOutput('plot',height=300),
                         plotlyOutput('plot1',height=300),
                         plotlyOutput('plot4',height=300),
                         plotlyOutput('plot9',height=300)
                         ),
                tabPanel('Global Sales Overview',
                         plotOutput('sales',height=300),
                         infoBoxOutput('box3',width=5),
                         infoBoxOutput('box4',width=5),
                         infoBoxOutput('box5',width=5),
                         infoBoxOutput('box6',width=5)
                         ),
                tabPanel('Global Sales Detail',
                         
                         plotlyOutput('plot2',height=300),
                         plotlyOutput('plot3',height=300),
                         plotlyOutput('plot10',height=300)
                         ),
                
                tabPanel('User vs Critic Scores',
                         
                         plotlyOutput('plot5',height=300),
                         plotlyOutput('plot6',height=300),
                         plotlyOutput('plot7',height=300),
                         plotlyOutput('plot8',height=300),
                         plotlyOutput('plot11',height=300),
                         plotlyOutput('plot12',height=300))
            )
        )
        
    )
)

server <- function(input, output){
    ####### NEW ELEMENTS BELOW !!!!!
    
    data <- reactive({                # Creates the dynamic data
        x %>%                  # Filter years, seasons & gender
            filter(Genre%in% input$genre,
                   Rating%in% input$rating,
                   Global_Sales >=input$globalsales[1],
                   Global_Sales <= input$globalsales[2],
                   Critic_Score >=input$criticscore[1],
                   Critic_Score <= input$criticscore[2],
                   User_Score >=input$userscore[1],
                   User_Score <= input$userscore[2],
                   Year_of_Release >= input$yearofrelease[1],
                   Year_of_Release<=input$yearofrelease[2],
                   Platform%in% input$platform) 
    })
    
    output$box1 <-renderValueBox({
        VideoGameSalesClean<-data()
        valueBox(
            value = sum(VideoGameSalesClean$Global_Sales),
            subtitle = 'Game Revenue',
            color = 'teal',
            width =5
        )
    })
    
    output$box2 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv2 <-round(mean(VideoGameSalesClean$Global_Sales),4)
        valueBox(
            value=boxv2,
            subtitle="Global Sales Average (M)",
            color='teal',
            width = 5
        )
    })
    output$box3 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv3 <-round(mean(VideoGameSalesClean$NA_Sales),4)
        valueBox(
            value=boxv3,
            subtitle="NA Sales Average (M)",
            color='aqua',
            
            width = 5
        )
    })
    output$box4 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv4<-round(mean(VideoGameSalesClean$EU_Sales),4)
        valueBox(
            value=boxv4,
            subtitle="EU Sales Average (M)",
            color='aqua',
            width = 5
        )
    })
    output$box5 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv5<-round(mean(VideoGameSalesClean$JP_Sales),4)
        valueBox(
            value=boxv5,
            subtitle="JP Sales Average (M)",
            color='aqua',
            width = 5
        )
    })
    output$box7 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv7<-round(mean(VideoGameSalesClean$User_Score)*10,2)
        valueBox(
            value=boxv7,
            subtitle="Average User Score",
            color='aqua',
            width = 5
        )
    })
    
    output$box8 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv8 <-round(mean(VideoGameSalesClean$Critic_Score),2)
        valueBox(
            value= boxv8,
            subtitle="Average Critic Score",
            color='aqua',
            width = 5
        )
    })
    
    output$box6 <-renderValueBox({
        VideoGameSalesClean<-data()
        boxv6<-round(mean(VideoGameSalesClean$Other_Sales),4)
        valueBox(
            value=boxv6,
            subtitle="Other Sales Average (M)",
            color='aqua',
            width = 5
        )
    })

    filter_genre<- reactive({
        tmp <-data()%>%
            group_by(Genre)%>%
            summarise(nb_games = n())%>%
            arrange(desc(nb_games))%>%
            head(input$max_genre)
    })
    
    filter_platform<- reactive({
        tmp1 <-data()%>%
            group_by(Platform)%>%
            summarise(nb_games = n())%>%
            arrange(desc(nb_games))%>%
            head(input$max_platform)
    })
    
    filter_publisher<- reactive({
        tmp2 <-data()%>%
            group_by(Publisher)%>%
            summarise(nb_games = n())%>%
            arrange(desc(nb_games))%>%
            head(input$max_publisher)
    })
    
    output$plot1 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Genre %in% filter_genre()$Genre)
        
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Year_of_Release, fill=Genre,))+geom_bar(width = 1.4)+xlab('Year')+ggtitle('Years and Genre'))
        })
        
    output$plot2 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Genre %in% filter_genre()$Genre)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Global_Sales, fill=Genre))+geom_bar(width = 1.4)+xlab('Global Sales (M)')+ggtitle('Global Sales and Genre'))
        
        })
    
    output$plot3 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Platform %in% filter_platform()$Platform)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Global_Sales, fill=Platform))+geom_bar(width = 1.4)+xlab('Global Sales (M)')+ggtitle('Global Sales and Platform'))
    })

    output$plot4 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Platform %in% filter_platform()$Platform)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Year_of_Release, fill=Platform))+geom_bar(width = 1.4)+xlab('Year')+ggtitle('Year and Platform'))
    })
    
    output$plot5 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Platform %in% filter_platform()$Platform)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=User_Score, fill=Platform))+geom_bar(width = 1.4)+xlab('User Score')+ggtitle('User Score and Platform'))
    })
    
    output$plot6 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Platform %in% filter_platform()$Platform)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Critic_Score, fill=Platform))+geom_bar(width = 1.4)+xlab('Critic Score')+ggtitle('Critic Score and Platform'))
    })
    
    output$plot7 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Genre %in% filter_genre()$Genre)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=User_Score, fill=Genre))+geom_bar(width = 1.4)+xlab('User Score')+ggtitle('User Score and Genre'))
    })
    
    output$plot8 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Genre %in% filter_genre()$Genre)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Critic_Score, fill=Genre))+geom_bar(width = 1.4)+xlab('Critic Score')+ggtitle('Critic Score and Genre'))
    })
    
    output$plot9 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Publisher %in% filter_publisher()$Publisher)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Year_of_Release, fill=Publisher))+geom_bar(width = 1.4)+xlab('Year')+ggtitle('Release Year and Publisher'))
    })
    
    output$plot10 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Publisher %in% filter_publisher()$Publisher)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Global_Sales, fill=Publisher))+geom_bar(width = 1.4)+xlab('Global Sales (M)')+ggtitle('Global Sales and Publisher'))
    })
    
    output$plot11 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Publisher %in% filter_publisher()$Publisher)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=User_Score, fill=Publisher))+geom_bar(width = 1.4)+xlab('User Score')+ggtitle('User Score and Genre'))
    })
    
    output$plot12 <-renderPlotly({
        VideoGameSalesClean <- data()%>% filter(Publisher %in% filter_publisher()$Publisher)
        ggplotly(VideoGameSalesClean %>% ggplot(aes(x=Critic_Score, fill=Publisher))+geom_bar(width = 1.4)+xlab('Critic Score')+ggtitle('Critic Score and Genre'))
    })
    
    output$table_1 <- DT::renderDataTable(data()) # Create the output object!
    output$plot<-renderPlot({
        release<-data()[,4]
        bins<-seq(min(release),max(release),length.out = input$bins+1)
        hist(release,breaks=bins,
             xlab = 'Year',
             main= 'Game Years Released',
             col='darkgray',
             border='white')
            
    })
    output$sales<-renderPlot({
        salesplot<-data()[,11]
        bins1<-seq(min(salesplot),max(salesplot),length.out = input$bins1+1)
        hist(salesplot,breaks=bins1,
             xlab = 'Year',
             main= 'Global Sales Counts',
             col='darkgray',
             border='white')
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)