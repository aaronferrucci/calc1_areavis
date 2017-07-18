library(shiny)
library(ggplot2)

source("bounds.R")

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing a Riemann Sum"),
   
   # Sidebar with a slider input for number of rectangles 
   sidebarLayout(
      sidebarPanel(
         sliderInput("rects",
                     "Number of rectangles:",
                     min = 1,
                     max = 64,
                     value = 31)
      ),
      
      # Show the graph
      mainPanel(
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$plot <- renderPlot({
    # plot object with a null data item
    p <- ggplot(data = data.frame(x2= 0), mapping = aes(x = x2))
     
    n = input$rects
    
    # Compute the rectangle bounds and the approximating area
    w <- (xmax - xmin) / n
    x1 <- xmin + c(0:(n-1)) * w
    x2 <- x1 + w
    y1 <- f2(x1)
    y2 <- f1(x1)
    
    rects <- data.frame(x1 = x1, x2 = x2, y1 = y1, y2 = y2, t=rep('a', n))
    area <- sum((rects$x2 - rects$x1) * (rects$y2 - rects$y1))
    
    # Plot the functions
    p2 <- p + scale_x_continuous(limits=c(xmin, xmax + 0.1)) + stat_function(fun=f1) + stat_function(fun=f2) +
      ggtitle(bquote(Area~between~f1~and~f2~is~approximately~.(area))) + guides(fill=F)
    # final plot with rectangles
    p2 + geom_rect(data=rects, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
