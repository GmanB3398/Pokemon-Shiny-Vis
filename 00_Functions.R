# All Useful Functions and Packages
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

getattackmoves <- function(move){
  base="http://pokeapi.co/api/v2/"
  url=paste0(base, "move/", tolower(move), "/")
  s=GET(url)
  w2= content(s)
  movestats=data.frame(w2$id, w2$name, ifnulls(w2$type$name), as.numeric(ifnull0(w2$power)), as.numeric(ifnull0(w2$accuracy)), as.character(ifnulls(w2$damage_class$name)), as.numeric(ifnull0(w2$pp)), stringsAsFactors = F)
  names(movestats)=c("id", "name", "type", "power", "accuracy", "class", "pp")
  return(movestats)
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
