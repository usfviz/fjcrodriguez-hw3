#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#You must implement three different multivariate visualization techniques:

# Technique 1: Heatmap -or- Bubble Plot
# Technique 2: Scatterplot Matrix -or- Small Multiples
# Technique 3: Parallel Coordinates Plot
# You may select which columns in the dataset to visualize for each technique, but you must include a minimum of four different columns on each. These techniques will be evaluated based on the correctness of your implementation and your customization of that technique.

# All plots should have some form of interactivity. I am expecting a high degree of  customization (customizing labels, grid lines, etc.).



library(shiny)
library(car)
library(ggplot2)
library(MASS)
library(plotly)
library(scales)
library(stringi)
facebook <- read.csv("dataset_Facebook.csv", sep=";")
facebook <- na.omit(facebook)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Homework 3"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Le Bubble Plot", 
        fluidRow( 
          column(12, offset = 0, plotlyOutput("bubblePlot") ), 
          column(2, offset = 0, sliderInput("likes",
                           "Likes Size",
                           min = 3,
                           max = 40,
                           value = 16,
                           sep="",
                           ticks = FALSE,
                           width = 1000)),# column2
          column(4 , offset = 0, selectInput("log", "Transformation", choices= c("Original Values","Log Transform")))
                ) # fluid row
        ), # tab Panel 1 
      
      
      tabPanel("Le Scatter Plot Matrix",  fluidRow( 
        column(12, offset = 0, plotOutput("scatterMatrix") ), 
        column(6 , offset = 0, checkboxGroupInput("columns", 
                                                  "Select Columns", 
                                                  choiceValues= c("Lifetime.Post.Total.Reach", 
                                                                  "Lifetime.Post.Total.Impressions",
                                                                  "Lifetime.Engaged.Users"
                                                                  ,"Lifetime.Post.Consumers"
                                                                  ,"Lifetime.Post.Consumptions"
                                                                  ,"Lifetime.Post.Impressions.by.people.who.have.liked.your.Page"
                                                                  ,"Lifetime.Post.reach.by.people.who.like.your.Page"
                                                                  ,"Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post"
                                                                  ,"like"
                                                                  ,"share"
                                                                  ,"Total.Interactions"
                                                                  , "comment"),
                                                  choiceNames = c("Lifetime Post Total Reach", "Lifetime Post Total Impressions"
                                                                  ,"Lifetime Engaged Users"
                                                                  ,"Lifetime Post Consumers"
                                                                  ,"Lifetime Post Consumptions"
                                                                  ,"Lifetime Post Impressions (Liked)"
                                                                  ,"Lifetime Post Reach (Liked)"
                                                                  ,"Lifetime Like and Engagement"
                                                                  ,"Likes"
                                                                  ,"Shares"
                                                                  ,"Total Interactions"
                                                                  ,"Comments"),
                                                  selected = c("Lifetime.Post.Total.Reach", 
                                                               "Lifetime.Post.Total.Impressions")
                                           )
              ), # column 2
        column(4 , offset = 0, selectInput("spm_color", "Select A Color (Because Why Not)", choices = c("Forestgreen","Red", "Blue", "Green", "Yellow", "Purple", "Dodgerblue"), selected = "Forestgreen"))
      ) # fluid row
      ),
      
      
      
      tabPanel("Le Parallel Coordinates Plot", 
               fluidRow(
                 column(12, plotOutput("parallel") ),
                 column(6 , offset = 0, checkboxGroupInput("parcolumns", 
                                                           "Select Columns", 
                                                           choiceValues= c("Lifetime.Post.Total.Reach", 
                                                                            "Lifetime.Post.Total.Impressions",
                                                                            "Lifetime.Engaged.Users"
                                                                            ,"Lifetime.Post.Consumers"
                                                                            ,"Lifetime.Post.Consumptions"
                                                                            ,"Lifetime.Post.Impressions.by.people.who.have.liked.your.Page"
                                                                            ,"Lifetime.Post.reach.by.people.who.like.your.Page"
                                                                            ,"Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post"
                                                                            ,"like"
                                                                            ,"share"
                                                                            ,"Total.Interactions"
                                                                            , "comment"),
                                                           choiceNames = c("Lifetime Post Total Reach", "Lifetime Post Total Impressions"
                                                                           ,"Lifetime Engaged Users"
                                                                           ,"Lifetime Post Consumers"
                                                                           ,"Lifetime Post Consumptions"
                                                                           ,"Lifetime Post Impressions (Liked)"
                                                                           ,"Lifetime Post Reach (Liked)"
                                                                           ,"Lifetime Like and Engagement"
                                                                           ,"Likes"
                                                                           ,"Shares"
                                                                           ,"Total Interactions"
                                                                           ,"Comments"),
                                                           selected = c("Lifetime.Post.Total.Reach", 
                                                                        "Lifetime.Post.Total.Impressions",
                                                                        "Lifetime.Engaged.Users")
                                                          
                                                          ) # checkbox input
                        ) # column 1
                 ) # end fluid row
               ) # end tab panel  
      
    ),# end tabsetPanel
    style='width: 900px; height: 1000px'
  ) # main panel
  
) # fluid page

theme1 <- theme( panel.grid.major=element_line(color = "grey90", size = 0.5),
                 panel.background = element_rect(fill = "white",color = "grey50", size=0.5),
                 axis.title = element_text(size=16,face = 'bold')
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bubblePlot <- renderPlotly({
    p <- ggplot(data=facebook) 
    if (input$log == "Log Transform"){
      p <- p + geom_point(aes(x=log(Lifetime.Post.Total.Reach), y=log(Lifetime.Post.Total.Impressions), fill=as.factor(Paid), size = like), color='black', stroke=0.2,alpha=.8, pch=21, show.legend = F)
      p <- p + geom_hline(yintercept=mean(log(facebook$Lifetime.Post.Total.Impressions)), alpha= 0.5, size=0.25)
      p <- p + geom_vline(xintercept=mean(log(facebook$Lifetime.Post.Total.Reach)), alpha= 0.5, size=0.25)
      p <- p + xlab("Lifetime Post Total Reach (Log)") + ylab("Life Time Total Impressions (Log)")
    }else{
      p <- p + geom_point(aes(x=Lifetime.Post.Total.Reach, y=Lifetime.Post.Total.Impressions, fill=as.factor(Paid), size = like), color='black', stroke=0.2,alpha=.8, pch=21, show.legend = F)
      p <- p + geom_hline(yintercept=mean(facebook$Lifetime.Post.Total.Impressions), alpha= 0.5, size=0.25)
      p <- p + geom_vline(xintercept=mean(facebook$Lifetime.Post.Total.Reach), alpha= 0.5, size=0.25)
      p <- p + xlab("Lifetime Post Total Reach") + ylab("Life Time Total Impressions")
    }
    
    p <- p + scale_x_continuous(labels = comma)
    p <- p + scale_y_continuous(labels = comma)
    p <- p + scale_size(range = c(1,input$likes))
    p <- p + scale_fill_manual(values=c("dodgerblue", "indianred"))
    p <- p + theme1
    p <- ggplotly(p, tooltip=c("size", "fill", 'x', 'y')) 
    hide_legend(p)
    
    # add the ability to choose whether the plot should log transformed or not
    # add variable for quadrant so that the color/fill/alpha is adjusted
    # 
    
  })
  
  output$scatterMatrix <- renderPlot({
    scatterplotMatrix(facebook[,input$columns], 
                      col = stri_trans_tolower(input$spm_color),
                      diagonal = "histogram",
                      pch=1,
                      legend.plot = T, ellipse=T)
  })


  output$parallel <- renderPlot({
  parcoord(log(facebook[,input$parcolumns]), col=facebook$Paid+3)
    
})

}
# Run the application 
shinyApp(ui = ui, server = server)

