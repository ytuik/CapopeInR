#questao1
ccdc = read.csv("GOT.csv", header = TRUE)
print("Questão 1:")
print(ccdc)



#questao2
vNota = ccdc[,3]
#calcula a media
media = mean(vNota)
cat("Questão2, a média é: ",media,"\n")
#calcula desvio padrao
desviop = sd(vNota)
cat("Questão2, o desvio padrão é: ",desviop,"\n")
#calcula moda
moda <- function(x){
  uniqv <- unique(x) #uniqv armazena todos os valores sem repetição
  uniqv[which.max(tabulate(match(x,uniqv)))] #match compara os numeros e retorna um vetor quando encontra um igual, armazena numa tabela e depois retorna o maior vetor nessa tabela
}
cat("Questão2, a moda é: ",moda(vNota),"\n")



#questao3
vAud = ccdc[,5]
#calcula a media
media = mean(vAud)
cat("Questão3, a média é: ",media,"\n")
#calcula desvio padrao
desviop = sd(vAud)
cat("Questão3, o desvio padrão é: ",desviop,"\n")
#calcular mediana
mediana = median(vAud)
cat("Questão3, a mediana é: ",mediana,"\n")



#questao4
vEpi = ccdc[,2]
vNota = ccdc[,3]

epi9 <- function(epi,nota){
  contador = 1
  vetor = c()
  for(x in nota){
    if(x>=9){
      vetor = c(vetor,as.character(epi[contador]))
    }
    contador = contador + 1
  }
  return(vetor)
}
print("Questão4, os episodios são: ")
print(epi9(vEpi,vNota))


#Questao5 parte1
menorEmaiorNota <- function(arr){
  resultadoTitulo = c()
  resultadoNota = c()
  resultadoTemporada = c()
  
  temp = arr[,1]
  maior = 0
  #pega o numero de temporadas
  for(x in temp){
    if(x>maior){
      maior=x;
    }
  }
  
  #pegar cada temporada
  x=1
  while(x <= maior){ #pegar as notas de toda temporada
    contador = 0
    notas = c()
    #pega as notas de cada temporada
    indextemporada = 0
    for(y in arr[,3]){
      contador = contador + 1
      if(arr[contador,1]==x){
        notas = c(notas,y)
      }
      if(arr[contador,1]==x && indextemporada==0){
        indextemporada = contador
      }
    }
    
    counter = 0
    alta = 0
    indexAlta = -1
    baixa = 99999999999999999
    indexBaixa = -1
    for(z in notas){
      counter = counter + 1
      if(z>alta){
        alta = z
        indexAlta = counter
      }
      if(z<baixa){
        baixa = z
        indexBaixa = counter
      }
    }
    #soma com o indice que se inicia a temporada
    indexAlta = indexAlta + indextemporada - 1
    indexBaixa = indexBaixa + indextemporada - 1
    
    resultadoTitulo = c(resultadoTitulo,as.character(arr[indexBaixa,2]),as.character(arr[indexAlta,2]))
    resultadoNota = c(resultadoNota,arr[indexBaixa,3],arr[indexAlta,3])
    resultadoTemporada = c(resultadoTemporada,arr[indexBaixa,1],arr[indexAlta,1])
    x = x + 1
  }
  resultadoOficial = data.frame(TITULO=c(resultadoTitulo),NOTA=c(resultadoNota),TEMPORADA=c(resultadoTemporada))
  return(resultadoOficial)
}
print(menorEmaiorNota(ccdc))


#questao 6
mDPtemp <- function (arr){
  temp = arr[,1]
  maior = 0
  #pega o numero de temporadas
  for(x in temp){
    if(x>maior){
      maior=x;
    }
  }
  temp = 0
  menor = 999999999999
  dpatual = 0
  while(maior!=0){
    #percorre toda a coluna de temporada
    contador = 0
    dp = c()
    for(x in arr[,1]){
      contador = contador + 1
      #armazena em dp um vetor das audiencias da temporada
      if(x == maior){
        dp = c(dp, arr[contador,5])
      }
    }
    
    #calcula o dp da temporada atual
    dpatual = sd(dp)
    
    if(dpatual<menor){
      menor = dpatual
      temp = maior
    }
    maior=maior-1;
  }
  return(temp)
}
mDPtemp(ccdc)


#questao7

vPersonagens = ccdc[,4]

mediaB <- function(z){
  contador = 0
  counter = 0
  nota = c()
  perso = c()
  perso = strsplit(as.character(z),",")
  #para cada grupo de personagem que compoe o episodio
  for (x in perso){
    counter = counter + 1
    #conferir se tem o personagem pra cada grupo
    for(y in x){
      if (as.character(y) == as.character("Brienne of Tarth(Gwendoline Christie)")){
        nota = c(nota, ccdc[counter,3])
        contador = contador + 1;
      }
    }
  }
  return(mean(nota));
}

print(mediaB(vPersonagens))


#Questão 8

AndWeNeverSeeHimAgain <- function(a){
  #Inicializando variaveis
  perso = a[,4]
  counter = 0
  counter2 = 0
  resultado = c()
  arr = c()
  uni = c()
  vrau = c("")
  var = FALSE
  
  for(x in perso){#loop para criar um vetor com todos os personagens
    counter = counter + 1
    if(a[counter,1] == 4){
      arr = c(arr, as.character(a[counter,4]))
    }
  }
  aux = strsplit(as.character(arr),",")#deixando o vetor bonitinho
  unico = TRUE
  
  
  for (i in aux){#loop para deixar o vetor anterior unidimensional
    for (j in i){
      uni = c(uni,j)
    }
  }
  #metodo que pega todos os personagens duplicados e os não duplicados(o que inclui a primeira aparição dos duplicados)
  #com esses dois vetores da pra fazer um match e ver as variaveis que so aparecem no vetor de não duplicados    
  v <- as.character(uni)[!(as.character(uni) %in% as.character(uni)[duplicated(as.character(uni))])]
  return(v)
  
}

print(AndWeNeverSeeHimAgain(ccdc))


#Questao9

FazOHistoagramaBem <- function(a,b) {
  #Inicializando variaveis
  pessoa <- as.character(b)
  perso = a[,4]
  counter2 = 0
  counter3 = 0
  total = c(0,0,0,0,0,0,0,0)
  arr = c()
  OdeioHistogramas = c()
  
  
  for(i in total){#loop para iterar entre as temporadas
    counter = 0
    counter2 = counter2 + 1
    for(x in perso){#loop para fazer um vetor com os personagens de cada episodio
      counter = counter + 1
      if(as.character(a[counter,1]) == as.character(counter2)){
        arr = c(arr, as.character(a[counter,4]))
      }
    }
    arr = strsplit(as.character(arr), ",")#deixando o array legivel
    
    for(x in arr){#loop para checar se o personagem passado aparece na temporada
      for (y in x){
        if(as.character(y) == pessoa){
          total[counter2] = total[counter2] + 1
        }
      }
    }
  }
  
  for(x in total){#loop para fazer o vetor que o metodo hist vai entender
    counter3 = counter3 + 1
    if (total[counter3] != 0) {
      for(y in 1:total[counter3]){
        OdeioHistogramas = c(OdeioHistogramas,counter3)
      }
    }
  }
  
  saidemonho <- hist(OdeioHistogramas,breaks = c(0,1,2,3,4,5,6,7,8), main = pessoa,xlab = "Temporada", col = "lightpink1", ylab = "Ocorrencia")
  return(saidemonho)
}
print(FazOHistoagramaBem(ccdc,"Bran Stark(Isaac Hempstead)"))