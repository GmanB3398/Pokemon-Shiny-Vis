library(png)
require(ggplot2)
require(ggformula)
require(shiny)
require(devtools)
require(reshape2)
require(magick)
require(tm)
require(wordcloud2)
library(httr)
require(dplyr)
require(circlepackeR)
require(data.tree)
require(dplyr)
require(reshape2)
library(igraph)
library(visNetwork)

# base="http://pokeapi.co/api/v2/"
# pokemon="diglett"
# url=paste0(base,"pokemon/",pokemon)
# s=GET(url)
# w= content(s)
# w$sprites$front_default
# char=GET("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/4.png")
#   w2=content(char ,as ="raw", type = "image/")
# readPNG(w2)
# charimg<- readPNG(getURLcontent("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/4.png"))


base="http://pokeapi.co/api/v2/"
pokemon=4
url=paste0(base,"pokemon/",pokemon)
s=GET(url)
w= content(s)

dex=GET("http://pokeapi.co/api/v2/pokedex/2/")
dex2=content(x = dex)
dex3=content(GET(dex2$pokemon_entries[[4]]$pokemon_species$url))


#THERE ARE 737 MOVES
w2$name
movestats=data.frame(as.numeric(w2$id), as.character(w2$name), as.character(w2$type$name), as.numeric(w2$power), as.numeric(w2$accuracy),as.character(w2$damage_class$name), w2$effect_changes, as.character(w2$stat_changes), as.character(w2$effect_chance), w2$pp, stringsAsFactors = F)

ifnull0 <-function(Null){
  if(is.null(Null)){
    return(0)
  }
  else if(is.na(Null)){
    return(0)
  }
  else{
    return(Null)
  }
}

ifnulls <-function(Null){
  if(is.null(Null)){
    return("")
  }
  else if(is.na(Null)){
    return("")
  }
  else{
    return(Null)
  }
}

getpokemonmoves<- function(poke){
  base="http://pokeapi.co/api/v2/"
  url=paste0(base, "pokemon/", tolower(poke), "/")
  s=GET(url)
  w= content(s)
  pokenow=c()
  for (i in 1:length(w$moves)){
    pokenow=c(pokenow,as.character(w$moves[[i]]$move$name))
  }
  return(pokenow)
}


pokemonmoves= matrix(ncol=151, nrow=246)
pokemonmoves=as.data.frame(pokemonmoves)
names(pokemonmoves)=c(1:151)
for (i in c(1:151)){
  print(i)
  poke=getpokemonstats(i)
  pokemovelist=getpokemonmoves(i)
  while(length(pokemovelist)<246){
    print(length(pokemovelist))
    pokemovelist=append(pokemovelist, NA)
  }
  pokemonmoves[,i]<- as.character(pokemovelist)
  names(pokemonmoves)[i]=poke$name
}
names(pokemonmoves)=gen1$name


getattackmoves <- function(move){
  base="http://pokeapi.co/api/v2/"
  url=paste0(base, "move/", tolower(move), "/")
  s=GET(url)
  w2= content(s)
  movestats=data.frame(w2$id, w2$name, ifnulls(w2$type$name), as.numeric(ifnull0(w2$power)), as.numeric(ifnull0(w2$accuracy)), as.character(ifnulls(w2$damage_class$name)), as.numeric(ifnull0(w2$pp)), stringsAsFactors = F)
  names(movestats)=c("id", "name", "type", "power", "accuracy", "class", "pp")
  return(movestats)
}
w2$id
move=paste0(base,"move/14")
s2=GET(move)
w2=content(s2)

movestats=data.frame(w2$id, w2$name, w2$type$name, as.numeric(ifnull0(w2$power)), as.numeric(ifnull0(w2$accuracy)), as.character(ifnulls(w2$damage_class$name)), as.numeric(ifnull0(w2$pp)), stringsAsFactors = F)
names(movestats)=c("id", "name", "type", "power", "accuracy", "class", "pp")


movelist=data.frame(id=numeric(), name=character(), type=character(), power=numeric(), accuracy=numeric(), class=character(), pp=numeric())
for (j in c(697:719)){
  movenow=getattackmoves(j)
  names(movenow)=c("id", "name", "type", "power", "accuracy", "class", "pp")
  movelist=rbind(movelist, movenow)
  print(j)
  print(movenow$name)
}
getattackmoves(4)$w2.name

getpokemonstats <- function(poke){
  base="http://pokeapi.co/api/v2/"
  url=paste0(base, "pokemon/", tolower(poke), "/")
  s=GET(url)
  w= content(s, "parsed")
  pokemonstats=data.frame(w$id,w$name, w$stats[[1]]$base_stat,
                          w$stats[[2]]$base_stat,w$stats[[3]]$base_stat,
                          w$stats[[4]]$base_stat,w$stats[[5]]$base_stat,
                          w$stats[[6]]$base_stat, w$sprites$front_default,
                          w$types[[1]]$type$name, 
                          if(class(try(w$types[[2]]$type$name,silent = T))=="try-error"){NA} else {w$types[[2]]$type$name})
  names(pokemonstats)=c("id","name", "speed", "special-defense", "special-attack",
                        "defense", "attack", "hp", "picture","type", "type2")
  return(pokemonstats)
}



# charizard=getstats(9)

gen1= data.frame(stringsAsFactors = F)
for (i in c(1:151)){
  pokenow=getpokemonstats(i)
  gen1=rbind(gen1, pokenow)
  print(i)
}

# stats=c("id","name", "speed", "special-defense", "special-attack",
#                       "defense", "attack", "hp", "picture","type", "type2")

gen1long=t(gen1)
colnames(gen1long)=gen1long["name",]

readPNG("http://pokeapi.co/media/sprites/pokemon/6.png")
readPNG(source=as.character(gen1$picture[6]))
chari=image_read("http://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/6.png")
chari


image=image_read(as.character(filter(gen1, name==input$WhichPoke)$picture))
list(src=chari, contentType="image/png")


#############################
#First Visualization
#############################
#The idea here is to show where each Pokemon lies in relation to all of
#other pokemon in a histogram, there's still a bug in the code which I haven't
#been able to figure out, and so an example is below the shiny app

#############################
#Second Visualization
#############################
# This idea takes two specific pokemon and compares their specific
# stat distributions. One pokemon of interest is Chansey, which has a
# high hp stat, but low stats otherwise. I would like to add hovertext
# to this in the future with information on each stat.



#############################
#Fifth Visualization
#############################
# While word clouds are not always the most informative use of visualization,
# they do make for a pretty picture. This idea allows the user to choose a selection
# of pokemon and see a word cloud for their english pokedex entries. The challenging
# part of this visualization was iterating through the api in order to scrape the entries


gen1long=t(gen1)
colnames(gen1long)=gen1long["name",]
gen1long=gen1long[3:8,]
gen1long=mapply(gen1long, FUN=as.numeric)
gen1long=mapply(gen1long, FUN=as.numeric)
gen1long=matrix(gen1long, nrow=6, ncol=151)
gen1long=as.data.frame(gen1long)
rownames(gen1long)=c("speed", "special-defense", "special-attack","defense", "attack", "hp")
colnames(gen1long)=gen1$name


ui=fluidPage(
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
           selectInput(inputId = "movep", label = "Which Pokemon's Moves?", choices=names(pokemonmoves), multiple = F, selected = "mew"),
           tableOutput("moveta")
    
  ),
  
  tabPanel("Flavor Text Analysis",
           selectInput(inputId = "WhichPokeF", label = "Which Pokemon's WordCloud?", choices= gen1Flavourtext$name, multiple = T, selected = gen1$name[7:9]),
           radioButtons(inputId = "All", label = "For All Pokemon",choices = c("Yes", "No"),selected = "No"),
           imageOutput(outputId = "SpriteCloud", height = "100px"),
           wordcloud2Output(outputId = "WordCloud")
            )
))

server=function(input, output){
  
  output$moveta= renderTable({
    filter(movelist, name %in% pokemonmoves[,input$movep])
  })
  
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
      geom_histogram(data=gen1,aes(x=eval(as.symbol(input$WhichStat))), binwidth = 10, fill="red", color="black")+
      geom_vline(xintercept =as.numeric(trimws(gen1long[input$WhichStat,input$WhichPoke],which = "both")), data=gen1long)+
      #geom_text(label=paste(input$WhichStat, "=", trimws(gen1long[input$WhichStat,input$WhichPoke],which = "both")))
      theme_classic()+
      xlab(input$WhichStat)
})
}
shinyApp(ui=ui, server=server)



#In the final project, I plan on integrating all of these different
#graphs into one Shiny app.

#Sunburst/CirclePacked plot by type?


gen1pack= gen1

for(i in 1:length(gen1pack$type2)){
gen1pack$type2x[i]=ifelse(test = is.na(as.character(gen1pack$type2[i])), yes = as.character(gen1pack$type[i]), no= as.character(gen1pack$type2[i]))
}
gen1pack$pathString<-paste("All", gen1pack$type2x, gen1pack$type, gen1pack$name, sep="/") 
gen1pack$size=1

types<-as.Node(gen1pack)

circlepackeR(types, size="size",color_min = "hsl(0, 100%, 50%)")





#Word Cloud of Flavor Text?


h=GET("http://pokeapi.co/api/v2/pokemon-species/1")
w2=content(h,"parsed")
w2$name
h2$names

textt=w2$flavor_text_entries[[4]]$flavor_text


getflavourText<- function(poke){
  base="http://pokeapi.co/api/v2/"
  url=paste0(base, "pokemon-species/", tolower(poke), "/")
  s=GET(url)
  w2= content(s, "parsed")
  flavtext=data.frame(w2$name, c(""))
  colnames(flavtext)= c("name", "text")
  t=""
  #Create long string with just words of pokedex entry
  for (i in 1:length(w2$flavor_text_entries)){
    if (w2$flavor_text_entries[[i]]$language$name== "en"){
      t=paste(t, w2$flavor_text_entries[[i]]$flavor_text, sep = " ")
    }
  }
 flavtext$text=t
  return(flavtext)
}

gen1Flavourtext= data.frame(stringsAsFactors = F)
for (i in c(1:151)){
  pokenow=getflavourText(i)
  gen1Flavourtext=rbind(gen1Flavourtext, pokenow)
  print(pokenow$name)
}


bulbasaur=Corpus(VectorSource(gen1Flavourtext$text[25]))

createWordCloud=function(text, remove=c("")){
r=Corpus(VectorSource(text))
r<-tm_map(r,removeWords, stopwords(c("english")))
r<-tm_map(r,removeWords, remove)
rtm=TermDocumentMatrix(r)
m=as.matrix(rtm)
v=sort(rowSums(m), decreasing = T)
d=data.frame(word=names(v), freq=v)
return(d)
}

wordcloud2(createWordCloud(gen1Flavourtext$text[4:6]))

#Visualization of Moves/Data Art with Pokemon Colors
library(igraph)

df=data.frame(from=rep(as.character(movelist$name), 151), to=rep(names(pokemonmoves), each=719))
value=c()
for (i in 1:length(df$from)){
  cat(paste(i, "\n"))
  if (df$from[i] %in% pokemonmoves[,as.character(df$to[i])]){
    value=append(value, 1)
  }
  else{
    value=append(value, 0)
  }
}
df$value=value

df3=data.frame(from=rep(as.character(names(pokemonmoves)), 151), to= rep(as.character(names(pokemonmoves)), each=151))
df3$from2=rep(c(1:151), 151)
df3$to2=rep(c(1:151), each=151)
df3=filter(df3, from!=to)

val=c()
accv=c()
acc2v=c()
for (i in 1:length(df3$from)){
  pok1<- as.character(df3$from[i])
  cat(paste(pok1, i, "\n"))
  pok2<- as.character(df3$to[i])
  acc=0
  acc2=0
  for (j in 1:length(pokemonmoves[,pok1])){
    if(is.na(pokemonmoves[j,pok1])){
      
    }
    else if (pokemonmoves[j,pok1] %in% pokemonmoves[,pok2]){
      acc=acc+1
      acc2=acc2+1
    }
    else{
      acc2=acc2+1
      }
  }
val=append(val, acc/acc2)
accv=append(accv, acc)
acc2v=append(acc2v, acc2)
}
df3$val=val
df3$acc=accv
df3$acc2=acc2v
nodes=data.frame(id=unique(df3$from2), label=as.character(unique(df3$from)), 
                 shape="image", image=c(as.character(gen1$picture[2:151]), as.character(gen1$picture[1])), title=unique(df3$from))
edges=data.frame(from=df3$from2, to=df3$to2, width=df3$val/mean(df3$val), title=paste("Moves in Common:", accv))
edges=filter(edges, width>1.5)
visNetwork(nodes = nodes,edges =  edges,main = "Pokemon Network with Moves as Edges")%>%
  visPhysics(stabilization = T, enabled=T,maxVelocity = 1,repulsion= list(nodeDistance=100,centralGravity=0))%>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE)%>%
  visIgraphLayout("layout.gem",physics=F)




#Choose one pokemon and see which others cna learn the same moves?

df4=data.frame(from=rep(as.character(names(pokemonmoves)), 151), to= rep(as.character(names(pokemonmoves)), each=151))
df4$from2=rep(c(1:151), 151)
df4$to2=rep(c(1:151), each=151)
df4=filter(df4, from!=to, from=="pikachu")

val2=c()
for (i in 1:length(df4$from)){
  pok1<- as.character(df4$from[i])
  cat(paste(pok1, i, "\n"))
  pok2<- as.character(df4$to[i])
  acc=0
  for (j in 1:length(pokemonmoves[,pok1])){
    if (pokemonmoves[j,pok1] %in% pokemonmoves[,pok2]){
      acc=acc+1
    }
  }
  va2l=append(val2, acc)
}
df4$val=val2

nodes=data.frame(id=unique(c(df4$from2, df4$to2)), label=as.character(unique(c(df4$from, df4$to))), 
                 shape="image", image=c(as.character(gen1$picture)), value=10000, title=paste(as.character(df4$from)))
edges=data.frame(from=df4$from2, to=df4$to2, title= paste("<p>",3))
visNetwork(nodes, edges, height = "500px", width = "100%")%>%
  visPhysics(stabilization = FALSE)%>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = list(enabled=T, hover=T) ) %>%
  visIgraphLayout()

x=graph_from_data_frame(select(df3, from, to, val),vertices = unique(df3$from), directed=F)



plot(x, arrow.mode=0)
V(x)$type
print(V(x))
