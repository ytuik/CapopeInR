#capope[,1] = Grupos, capope[,2] = N album, capope[,3] = anos, capope[,4] = empresas
#capope[,5] = vendas

#Questão 1
capope = read.csv("Capope.csv", header = TRUE)
print(capope)

#######################################################################################################
#Questão 2
vVenda = capope[,5]

#Calcula a media
media = mean(vVenda)
cat("A média é: ",media,"\n")

#Calcula o Desvio padrão
desviop = sd(vVenda)
cat("O Desvio padrão é:", desviop,"\n")

#Calcula a Moda
moda <- function(x){
  uniqv <- unique(x) #uniqv armazena todos os valores sem repetição
  vrau = which.max(tabulate(match(x,uniqv, nomatch = 0))) #match compara os numeros e retorna um vetor quando encontra um igual, armazena numa tabela e depois retorna o maior vetor nessa tabela
    if (vrau <= 1){
      return("Não existe moda pois todos os numeros são diferentes")
    }else{
      return(uniqv[vrau])
    }
      
}
  
modaV = moda(vVenda)
cat("A moda é:", modaV, "\n")


#######################################################################################################

#Questão 3
vGrupos = capope[,1]
vAno = capope[,3]
ComebackVemForte <- function(grupo,ano,parametro){ #Metodo para pegar os grupos que lançaram um album no ano especificado
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
a = (match(Gin2018,Gin2019)) #Comparação para ver se algum grupo se repete nos dois anos
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
cat("Os seguintes grupos tiveram albuns lançados nos dois anos :",sResul,"\n")

#######################################################################################################

#Questão 4
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
cat("O grupo com o menor desvio padrão nas vendas foi o: ",resultado)


#######################################################################################################

#Questão 5
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

#######################################################################################################


