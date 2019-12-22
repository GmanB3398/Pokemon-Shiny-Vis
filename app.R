load("poke_data.rdata")
if (!require(shiny)){
install.packages("shiny")
library(shiny)}
if (!require(png)){
install.packages("png")
library(png)}
if (!require(ggplot2)){
install.packages("ggplot2")
library(ggplot2)}
if (!require(ggformula)){
install.packages("ggformula")
library(ggformula)}
if (!require(devtools)){
install.packages("devtools")
library(devtools)}
if (!require(devtools)){
install.packages("devtools")
library(devtools)}
if (!require(reshape2)){
install.packages("reshape2")
library(reshape2)}
if (!require(magick)){
install.packages("magick")
library(magick)}
if (!require(tm)){
install.packages("tm")
library(tm)}
if (!require(wordcloud2)){
install.packages("wordcloud2")
library(wordcloud2)}
if (!require(httr)){
install.packages("httr")
library(httr)}
if (!require(dplyr)){
install.packages("dplyr")
library(dplyr)}
if (!require(data.tree)){
install.packages("data.tree")
library(data.tree)}
if (!require(igraph)){
install.packages("igraph")
library(igraph)}
if (!require(visNetwork)){
install.packages("visNetwork")
library(visNetwork)}
if (!require(circlepackeR)){
    devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)}
# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Explore A Pokemon's Stats",
                 selectInput(inputId = "WhichPoke", label = "Which Pokemon's Stats?", choices= gen1$name, multiple = F, selected = gen1$name[1]),
                 radioButtons(inputId = "WhichStat", label = "Which Stat to Look at?",choices = c("speed", "special-defense", "special-attack",
                                                                                                  "defense", "attack", "hp"),selected = "attack"),
                 imageOutput(outputId = "SpriteExplore", height = "100px"),
                 plotOutput(outputId = "Statistic")
                 
        ),
        
        tabPanel("Compare Pokemon's Stats",
                 fluidRow(
                     column(width=6, imageOutput(outputId = "SpriteCompare1", height = "100px"),
                            column(width = 6, plotOutput("Comparepoke1", width = "400px"), 
                                   fluidRow(column(6,"Par1",selectInput(inputId = "WhichPoke1", label = "Which Pokemon to Compare?", choices= gen1$name, multiple = F, selected = gen1$name[1])) 
                                   ))), 
                     column(width=6, imageOutput(outputId = "SpriteCompare2", height = "100px"),column(6, plotOutput("Comparepoke2", width = "400px"), 
                                                                                                       fluidRow(column(6, "Par 2",selectInput(inputId = "WhichPoke2", label = "Which Pokemon to Compare?", choices= gen1$name, multiple = F, selected = gen1$name[1]))))))
        ),
        tabPanel("Pokemon Type Distribution",
                 circlepackeROutput("PokemonTypes",width = "700px", height="700px")
                 
        ),
        tabPanel("Pokemon Moves",
                 visNetworkOutput("moves"),
                 #selectInput(inputId = "movep", label = "Which Pokemon's Moves?", choices=names(pokemonmoves), multiple = F, selected = "mew"),
                 #tableOutput("moveta")
                 
        ),
        
        tabPanel("Flavor Text Analysis",
                 selectInput(inputId = "WhichPokeF", label = "Which Pokemon's WordCloud?", choices= gen1Flavourtext$name, multiple = T, selected = gen1$name[7:9]),
                 radioButtons(inputId = "All", label = "For All Pokemon",choices = c("Yes", "No"),selected = "No"),
                 imageOutput(outputId = "SpriteCloud", height = "100px"),
                 wordcloud2Output(outputId = "WordCloud")
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output){
    
    # output$moveta= renderTable({
    #     movelist[movelist$name%in% pokemonmoves[,input$movep],-1]
    #     # dplyr::filter(movelist, name %in% pokemonmoves[,input$movep])
    # })
    
    output$moves=renderVisNetwork({
        visNetwork(nodes = nodes,edges =  edges,main = "Pokemon Network with Moves as Edges")%>%
            visPhysics(enabled=T, stabilization = F, maxVelocity = 500, repulsion= list(nodeDistance=100, centralGravity=0.1), solver = "repulsion")%>%
            visEdges(smooth = FALSE) %>%
            visOptions(highlightNearest = TRUE)%>%
            visInteraction(navigationButtons = TRUE,keyboard = T) %>%
            visIgraphLayout("layout.auto")
        
        
    })
    
    output$WordCloud=renderWordcloud2({
        if (input$All=="Yes"){
            wordcloud2(createWordCloud(text=gen1Flavourtext$text))
        }
        else{
            wordcloud2(createWordCloud(text=filter(gen1Flavourtext, name %in% c(input$WhichPokeF))$text))
        }
    })
    
    output$PokemonTypes=renderCirclepackeR({
        circlepackeR(types, size="size", "hsl(0, 100%, 50%)")
    })
    
    output$SpriteExplore=renderImage({
        image=image_read(as.character(filter(gen1,name==input$WhichPoke)$picture))
        image2=image %>% image_write(tempfile(fileext="png"), format='png')
        list(src=image2, contentType="image/png")
    })
    
    output$SpriteCompare1=renderImage({
        image=image_read(as.character(filter(gen1,name==input$WhichPoke1)$picture))
        image2=image %>% image_write(tempfile(fileext="png"), format='png')
        list(src=image2, contentType="image/png")
    })
    
    output$SpriteCompare2=renderImage({
        image=image_read(as.character(filter(gen1,name==input$WhichPoke2)$picture))
        image2=image %>% image_write(tempfile(fileext="png"), format='png')
        list(src=image2, contentType="image/png")
    })
    
    output$Comparepoke1=renderPlot({
        ggplot(data=gen1long)+
            geom_bar(stat="identity",width=1, aes(x="", y=eval(as.symbol(input$WhichPoke1)), fill=row.names(gen1long)))+
            coord_polar("y", start=0)+
            theme_minimal()+
            xlab("")+
            ylab(input$Whichpoke1)+
            theme(legend.title=element_blank())
        
    })
    output$Comparepoke2=renderPlot({
        ggplot(data=gen1long)+
            geom_bar(stat="identity",width=1, aes(x="", y=eval(as.symbol(input$WhichPoke2)), fill=row.names(gen1long)))+
            coord_polar("y", start=0)+
            theme_minimal()+
            xlab("")+
            ylab(input$Whichpoke2)+
            theme(legend.title=element_blank())
    })
    output$Statistic=renderPlot({
        ggplot(data=gen1)+
            geom_histogram(data=gen1, aes(x=eval(as.symbol(input$WhichStat))), bins = 12, color="black", fill="firebrick")+
            geom_vline(xintercept =as.numeric(trimws(gen1long[input$WhichStat,input$WhichPoke],which = "both")),size=2, data=gen1long)+
            #geom_text(label=paste(input$WhichStat, "=", trimws(gen1long[input$WhichStat,input$WhichPoke],which = "both")))
            theme_classic()+
            xlab(input$WhichStat)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
