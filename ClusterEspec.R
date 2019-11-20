#Entrada de dados, matriz de adjacencia 
S<- ad

#Definição das constantes
TAM <- dim(S)[1]  #N de Nós
NCLUST <- 2       #N de Clusters desejados

#Criação da Matrix de Grau
D <- matrix(0,TAM,TAM)
for(i in 1:TAM)
  D[i,i] <- sum(S[i,])

#Calculo Matrix Laplaciana
Laplacian <- D - S

#Calculo da D^(-1/2)
D_sqrt = sqrt(D)#D^(1/2)
for(i in 1:TAM)
  for(j in 1:TAM)
    D_norm[i,j] <- if (D_sqrt[i,j]==0.0) 0 else 1/D_sqrt[i,j] #Aqui fazemos 1/D^(1/2) porem evitando divisao por 0

#Calculo da Laplaciana Normalizada
N_L <- as.matrix(D_norm) %*% as.matrix(Laplacian)%*% as.matrix(D_norm)

#Calculo dos AutoValores da Laplaciana Normalizada
ev <- eigen(N_L)
ev_vector <- ev$vectors[,1:TAM]

#Normalização em linhas do AutoVetores
for(i in 1:TAM){
  ev_vector[i,] <- ev_vector[i,] / sqrt(sum(ev_vector[i,]^2))
}

#K-means na matriz dos autovetores Normalizados
e <- kmeans(ev_vector,NCLUST)$cluster #Esse eh o resultado mais importante

#Resultado dos Cluesters criados
table(e,as.numeric(1:TAM)) #Eh uma forma de visualizar e

#Associando cada ponto a um coordenada x,y entrada no arquivo pos
p <- pos

#Criando uma matriz que guarda as interconexões entre postos
t<-1
conec<-matrix(0,2,TAM*TAM) #duas linhas: linha do x e linha do y; TAM*TAM colunas eh o tamanho maximo possivel
for(i in 1:TAM)
  for(j in 1:TAM)
    if (S[i,j] ==1) {
      conec[1,t]=p[i,1]
      conec[2,t]=p[i,2];
      t<-t+1;
      conec[1,t]=p[j,1]
      conec[2,t]=p[j,2]
      t<-t+1
    }
conex<-matrix(0,2,t-1) #criamos uma matrix menor agora que já sabemos a quantidade de conexoes
for(i in 1:2)
  for(j in 1:t-1)
    conex[i,j]<-conec[i,j]


#plot(PC[,1],PC[,2],col=c("red","blue"))

#Plot dos resultados
plot(p[,1], p[,2],col=ifelse(e==1, "red", "blue") ) #Plot dos pontos nas suas coordenadas x,y com a sua cor levando em conta seu cluster
i=1
for(x in 1:((t-1)/2)){ #Laco para plot das linhas de conexao
  lines(conex[1,i:(i+1)],conex[2,i:(i+1)],col="green")
  i<-i+2
}

