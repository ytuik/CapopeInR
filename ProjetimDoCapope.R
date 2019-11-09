#capope[,1] = Grupos, capope[,2] = N album, capope[,3] = anos, capope[,4] = empresas
#capope[,5] = vendas

#Questao 1
capope = read.csv("Capope.csv", header = TRUE)
print(capope)

#######################################################################################################
#Questao 2
vVenda = capope[,5]

#Calcula a media
media = mean(vVenda)
cat("A media ?: ",media,"\n")

#Calcula o Desvio padrao
desviop = sd(vVenda)
cat("O Desvio padrao ?:", desviop,"\n")

#Calcula a Moda
moda <- function(x){
  uniqv <- unique(x) #uniqv armazena todos os valores sem repeticao
  vrau = which.max(tabulate(match(x,uniqv, nomatch = 0))) #match compara os numeros e retorna um vetor quando encontra um igual, armazena numa tabela e depois retorna o maior vetor nessa tabela
    if (vrau <= 1){
      return("Nao existe moda pois todos os numeros sao diferentes")
    }else{
      return(uniqv[vrau])
    }
      
}
  
modaV = moda(vVenda)
cat("A moda e:", modaV, "\n")


#######################################################################################################

#Questao 3
vGrupos = capope[,1]
vAno = capope[,3]
ComebackVemForte <- function(grupo,ano,parametro){ #Metodo para pegar os grupos que lan?aram um album no ano especificado
  contador = 1
  vetor = c()
  for (x in ano){
    if(x == parametro ){
      vetor = c(vetor, as.character(grupo[contador]))
    }
    contador = contador + 1
    
  }
  return(vetor)
}
Gin2018 = ComebackVemForte(vGrupos,vAno,"2018")
Gin2019 = ComebackVemForte(vGrupos,vAno,"2019")
a = (match(Gin2018,Gin2019)) #Comparacao para ver se algum grupo se repete nos dois anos
resultado = c()
for(x in a){
  resultado = c(resultado, as.character(Gin2019[x])) 
}
resultado =  unique(na.exclude(resultado))
sResul = c()
contador = 1
for(x in resultado){
  sResul = c(sResul,as.character(resultado[contador]),",")
  contador = contador + 1;
}
cat("Os seguintes grupos tiveram albuns lancados nos dois anos :",sResul,"\n")

#######################################################################################################

#Quest?o 4
vGrupos = capope[,1]
vVendas = capope[,5]


oMelhorGrupoLeva <- function(vGrupos,vVendas){
vUnicos = unique(vGrupos)
bestD = 999999999999999999
nGrupo = ""
actualD = 0

a = duplicated(vGrupos)
duplicado = c()
contador = 1
for(x in a){
  if(x == TRUE){
    duplicado = c(duplicado, as.character(vGrupos[contador]))
  }
  contador = contador + 1
}
duplicado = unique(duplicado)


for (x in duplicado){
  Venda = c()
  actualD = 0
  i = 1
  for(y in vGrupos){
    if (x == y){
      Venda = c(Venda, vVendas[i])
    }
    i = i+1
  }
  actualD = sd(Venda)
  if (is.na(actualD)){
    actualD = 0
  }
  if(actualD < bestD){
    bestD = actualD;
    nGrupo = as.character(x)
  }
}
return(nGrupo)
}
resultado = oMelhorGrupoLeva(vGrupos,vVendas)
cat("O grupo com o menor desvio padr?o nas vendas foi o: ",resultado)


#######################################################################################################

#Quest?o 5
vGrupos = capope[,1]
vAlbum = capope[,2]
vAno = capope[,3]
vVendas = capope[,5]
x = "2018"

hitou <- function(grupo,album,venda,ano,parametro){
  iOrder = sort(grupo)
  gHit = ""
  aHit = ""
  bSale = 0
  i = 1
  for (x in iOrder){
    aSale = venda[i]
    if(aSale > bSale && ano[i] == parametro){
      
      bSale = aSale
      gHit = as.character(grupo[i])
      aHit = as.character(album[i])
    }
    i = i + 1
  }
  saida = c(gHit, aHit)
  return(saida)
}
resultado1 = hitou(vGrupos,vAlbum,vVenda,vAno,x)

flopou <- function(grupo,album,venda,ano,parametro){
  iOrder = sort(grupo)
  gFlop = ""
  aFlop = ""
  wSale = 999999999999
  i = 1
  for (x in iOrder){
    aSale = venda[i]
    if(aSale < wSale && ano[i] == parametro){
      wSale = aSale
      gFlop = as.character(grupo[i])
      aFlop = as.character(album[i])
    }
    i = i + 1
  }
  saida = c(gFlop, aFlop)
  return(saida)
}
resultado2 = flopou(vGrupos,vAlbum,vVenda,vAno,x)
cat("No ano ",x,"o album que mais vendeu foi '",resultado1[2],"' do grupo ",resultado1[1])
cat("No ano ",x,"o album que menos vendeu foi '",resultado2[2],"' do grupo ",resultado2[1])

#Questão 6

artistas = capope[,1]
ano = capope[,3]
threshold <- 1
a = Filter(function (elem) length(which(artistas == elem)) <= threshold, artistas)

printarAno <- function(artistas, ano) {
  for (x in a) {
    contador = 1
    for (y in artistas) {
      if (x == y) {
        cat(x, ":" ,ano[contador], "\n")
      }
      contador = contador + 1
    }
  }
}
resultado = printarAno(artistas, ano)

# Questão 7
empresass = capope[,4]
ArtistasSemRepetir <- data.frame(artistas, empresass)
ArtistasSemRepetir = ArtistasSemRepetir[!duplicated(ArtistasSemRepetir$artistas),]
eUnicas= unique(empresass)
n <- length(eUnicas)
Empresa <- numeric(n)
Quantidade <- numeric(n)
i = 1

for (x in eUnicas){
  count = 0
  for(y in ArtistasSemRepetir[,2]){
    if (x == y) {
      count = count + 1
    }
  }
  Empresa[i] <- x
  Quantidade[i] <- count
  i = i + 1
}

df1 <- data.frame(Empresa, Quantidade)
df1Ordered = df1[order(df1$Quantidade, decreasing=TRUE),]
print(df1Ordered)

# Questão 8
famosinhos <- function(artistas){
  frequencia = (tabulate(match(artistas,unique(artistas), nomatch = 0)))
  var = (max(frequencia))
  vetorPosicao = c()
  posicao = 1
  aux = 1
  for(x in frequencia){
    if(x == var){
      vetorPosicao[aux] = posicao
      aux = aux + 1
    }
    posicao = posicao + 1
  }
  grupo = c()
  for (x in vetorPosicao){
    grupo = c(grupo, as.character(artistas[x]))
  }
    return (grupo)
}

vVenda = capope[,5]
vGrupo = capope[,1]
Artistas = famosinhos(vGrupo)
Valores = c()
count2 = 1
for (x in Artistas) {
  count = 1
  valor = 0
  for (y in vGrupo) {
    if (x == y) {
      valor = valor + vVenda[count]
    }
    count = count + 1
  }
  Valores[count2] = valor
  count2 = count2 + 1
}
dfFamosinhos = data.frame(Artistas, Valores)
dfFamosinhosOrdenado = dfFamosinhos[order(dfFamosinhos$Valores, decreasing=TRUE),]
print(dfFamosinhosOrdenado)

#Questao 9
vAlbuns = capope[,2]
maisVendidosPorEmpresa <- function(vEmpresa, vVendas) {
  vAlbunsMaisVendidos = c()
  contadorNovoVetor = 1
  for (x in vEmpresa) {
    contadorEmpresa = 1
    contadorVenda = 1
    posicaoMaiorVenda = 1
    maiorVenda = 0
    for (y in vVendas) {
      if (capope[,4][contadorEmpresa] == x && vVendas[contadorVenda] > maiorVenda) {
        posicaoMaiorVenda = contadorVenda
        maiorVenda = vVendas[contadorVenda]
        contadorEmpresa = contadorEmpresa + 1
        contadorVenda = contadorVenda + 1
      } else {
        contadorEmpresa = contadorEmpresa + 1
        contadorVenda = contadorVenda + 1
      }
    }
    vPosicaoMaisVendidos[contadorNovoVetor] = posicaoMaiorVenda
    vAlbunsMaisVendidos = c(vAlbunsMaisVendidos, as.character(vAlbuns[posicaoMaiorVenda]))
    contadorNovoVetor = contadorNovoVetor + 1
  }
  return(vAlbunsMaisVendidos)
}
Empresa = capope[,4]
Empresa = sort(vEmpresa, decreasing = FALSE)
Empresa = unique(vEmpresa)
vVendas = capope[,5]
Albuns = maisVendidosPorEmpresa(Empresa, vVendas)

#Pegandos os artistas e albuns
Artista = c()
Valor = c()
contadorVArtistas = 1
contadorPosicao = 1
for (x in Albuns) {
  for (y in capope[,2]) {
    if (x == y) {
      Artista = c(Artista, as.character(capope[,1][contadorVArtistas]))
      Valor[contadorPosicao] = capope[,5][contadorVArtistas]
    }
    contadorVArtistas = contadorVArtistas + 1
  }
  contadorVArtistas = 1
  contadorPosicao = contadorPosicao + 1
}

#Fazendo o DataFrame
dfAlbunsMaisVendidos = data.frame(Empresa, Artista, Albuns, Valor)
dfAlbunsMaisVendidosOrdenado = dfAlbunsMaisVendidos[order(dfAlbunsMaisVendidos$Valor, decreasing = TRUE),]
print(dfAlbunsMaisVendidosOrdenado)

#############################################################################################
#Quest?o 10


ComebackDoWaveform <-function(Empresa){
vEmpresa = capope[,4]
  QAlbum2018 = 0
  QAlbum2019 = 0
  contador = 1
  counter = 0
  OdeioHistogramas = c()
  for (i in vEmpresa){
    if(i == Empresa && ano[contador] == "2018"){
      QAlbum2018 = QAlbum2018 + 1
    }
    else if(i == Empresa && ano[contador] == "2019"){
      QAlbum2019 = QAlbum2019 + 1
    }
    contador = contador+1
  }
  total = c(QAlbum2018 , QAlbum2019)
  print(total)
  
  for(x in total){#loop para fazer o vetor que o metodo hist vai entender
    counter = counter + 1
    if (total[counter] != 0) {
      for(y in 1:total[counter]){
        OdeioHistogramas = c(OdeioHistogramas,counter)
      }
    }
  }
  print(OdeioHistogramas)
  
  
   vamo<- hist(OdeioHistogramas,breaks = 2, main = Empresa,xlab = "Ano", col = "darkslategray1", ylab = "Qtd de Album")
  return(vamo)
}

print(ComebackDoWaveform("SM"))