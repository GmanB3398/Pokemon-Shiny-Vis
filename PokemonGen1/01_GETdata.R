# Initialize Datasets
source(file = "00_Functions.R")

gen1= data.frame(stringsAsFactors = F)
for (i in c(1:151)){
  pokenow=getpokemonstats(i)
  gen1=rbind(gen1, pokenow)
  print(i)
}

movelist=data.frame(id=numeric(), name=character(), type=character(), power=numeric(), accuracy=numeric(), class=character(), pp=numeric())
for (j in c(1:728)){
  movenow=getattackmoves(j)
  names(movenow)=c("id", "name", "type", "power", "accuracy", "class", "pp")
  movelist=rbind(movelist, movenow)
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

gen1long=t(gen1)
colnames(gen1long)=gen1long["name",]
gen1long=gen1long[3:8,]
gen1long=mapply(gen1long, FUN=as.numeric)
gen1long=mapply(gen1long, FUN=as.numeric)
gen1long=matrix(gen1long, nrow=6, ncol=151)
gen1long=as.data.frame(gen1long)
rownames(gen1long)=c("speed", "special-defense", "special-attack","defense", "attack", "hp")
colnames(gen1long)=gen1$name

gen1Flavourtext= data.frame(stringsAsFactors = F)
for (i in c(1:151)){
  pokenow=getflavourText(i)
  gen1Flavourtext=rbind(gen1Flavourtext, pokenow)
  print(pokenow$name)
}

gen1pack= gen1

for(i in 1:length(gen1pack$type2)){
  gen1pack$type2x[i]=ifelse(test = is.na(as.character(gen1pack$type2[i])), yes = as.character(gen1pack$type[i]), no= as.character(gen1pack$type2[i]))
}
gen1pack$pathString<-paste("All", gen1pack$type2x, gen1pack$type, gen1pack$name, sep="/") 
gen1pack$size=1

types<-as.Node(gen1pack)

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
