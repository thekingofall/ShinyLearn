#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 5000,
                        value = 1000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        library(data.table)
        a=fread('gene2pubmed.gz',data.table = F)
        head(a)
        a=a[a$`#tax_id`==9606,]
        head(a)
        library("tm")
        library("SnowballC")
        #install.packages("SnowballC")
        library("wordcloud")
        library("RColorBrewer")
        tb=as.data.frame(table(a$GeneID))
        head(tb)
        tb=tb[order(tb$Freq,decreasing = T),]
        head(tb)
        head(tb,100)
        colnames(tb)[1]="gene_id"
        set.seed(1234)

        library(org.Hs.eg.db)
        ids=toTable(org.Hs.egSYMBOL)
        head(ids)
        tbs=merge(ids,tb,by='gene_id')
        wordcloud(words = tbs$symbol, freq = tbs$Freq, min.freq = input$bins,
                  max.words=200, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Accent"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
