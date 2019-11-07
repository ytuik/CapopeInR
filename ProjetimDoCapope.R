#capope[,1] = Grupos, capope[,2] = N album, capope[,3] = anos, capope[,4] = empresas
#capope[,5] = vendas

#Questão 1
capope = read.csv("Capope.csv", header = TRUE)
print(capope)

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
  uniqv[which.max(tabulate(match(x,uniqv, nomatch = 0)))] #match compara os numeros e retorna um vetor quando encontra um igual, armazena numa tabela e depois retorna o maior vetor nessa tabela
}
modaV = moda(vVenda)
cat("A moda é:", modaV, "\n")
#Consertar a moda, pq funciona pra quando tem moda,quando n tem ele buga


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


#Questão 4
vGrupos = capope[,1]
vVendas = capope[,5]
oMelhorGrupoLeva <- function(vGrupos,vVendas){
iOrder = sort(unique(vGrupos))
bestD = 999999999999999999
nGrupo = ""
actualD = 0
for (x in iOrder){
  Venda = c()
  actualD = 0
  i = 1
  for(y in vGrupos){
    if (x == y){
      Venda = c(Venda, vVendas[i])
      print(Venda)
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
print(resultado)
#tbm ta bugada essa merda


#Questão 5
vGrupos = capope[,1]
vAlbum = capope[,2]
vVendas = capope[,5]

hitou <- function(grupo,album,venda,ano){
  iOrder = sort((grupo))
  gHit = ""
  aHit = ""
  bSale = 0
  for (x in iOrder){
    i = 1
    Venda = venda[i]
    if(venda > bSale){
      bSale = venda
      gHit = as.character(grupo[i])
      aHit = as.character(album[i])
    }
  }
  saida = c(gHit, aHit)
  return(saida)
}
resultado1 = hitou(vGrupos,vAlbum,vVenda,"2018")
print(resultado1)
  

