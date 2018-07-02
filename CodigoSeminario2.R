library(dplyr) 
library(cluster) 
library(Rtsne) 
library(ggplot2) # 2D Plot
library(varhandle) # unfactor
library(microbenchmark) # Analisis de tiempos
library(magrittr) # Pipes
library(rgl) # 3D Plot
library(fpc)
library(rbenchmark)
library(tibble)

library(ggplot2)
library(ggalt)
library(ggfortify)
library(tidyr)
library(pryr) # Obtener memoria usada, pesos etc
library(dummies)
library(ggmosaic)
library(factoextra)
library(reshape2)

#======================================================================================

#======================================================================================
#======================================================================================
# Leer datos 

Data<- 
  read.csv("bank.csv")

#======================================================================================
#======================================================================================
# Elección de variables.

Data <- 
  Data %>% 
  select(age, job, marital, education,
         balance,# ingreso
         housing, loan,y # Tenencia de producto
  ) 

#======================================================================================
#======================================================================================
# K-MEANS 

#======================================================================================
#======================================================================================
# Variables dummy

Data2 <- Data %>% 
  dummy.data.frame( all = TRUE)

#======================================================================================
#======================================================================================
# Estandarizando las variables. 

Data3 <- Data2 %>%
          scale()

colMeans(Data3)  # Media es cero luego de escalar
apply(Data3, 2, sd) # desviacion estandar es 1

#=======================================================================================

pc <- prcomp(Data3, scale=T)

summary(pc)

round(cov(Data3),1)


plot(pc, type='l')
summary(pc)

Plot <- data.frame(comp=1:27, varianza=pc$sdev)

  ggplot(data=Plot, 
         aes(x=comp, y=varianza)) +
  geom_line()+
  geom_point() +
  xlab("Número de Componente") +
  ylab("Varianza") +
  theme(legend.position="top") + 
  geom_vline(xintercept = 8,color = "#C00000")

fviz_screeplot(pc, ncp=27, addlabels=T, ggtheme = theme_gray())
?fviz_screeplot


comp <- data.frame(pc$x[,1:8])


#===================================================================================
#===================================================================================
# K-means

data <- scale(comp)
#Get a distance matrix
d<-dist(data,method = "euclidean")
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(data, 
                                     centers=i)$withinss)

wss_df <- data.frame(ind=1:20, wss=wss)

ggplot(data=wss_df, 
       aes(x=ind, y=wss)) +
  geom_line()+
  geom_point(aes(y = wss)) +
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c('#C00000','#2F4F4F'))+
  scale_size_manual(values=c(10, 10))+
  ggtitle("Método de suma de cuadrados dentro de clusters") + 
  xlab("Número de clusters") +
  ylab("suma de cuadrados dentro de clusters") +
  theme(legend.position="top")+
  geom_vline(xintercept = 11,color = "#C00000")


n <- 11

fit1 <- kmeans(data,n)

tsne_model_1 = Rtsne(as.matrix(data), check_duplicates=FALSE, 
                     pca=TRUE, perplexity=30, theta=0.5, dims=2)
d_tsne_1 = as.data.frame(tsne_model_1$Y) 

d_tsne_1$Grupos = factor(fit1$cluster)

ggplot(d_tsne_1)+
  geom_point(aes(x=V1, y=V2, color=Grupos))+
  xlab("Comp1") + ylab("Comp2") +
  ggtitle("Clustering con K-means y PCA") 

#======================================================
# comparando algoritmo sin PCA y algoritmo con PCA

Data1<- 
  read.csv("Kmeans.csv") # Sin PCA

Data1 <- cbind(dni=1:4521, Data1)


Data2 <- cbind(dni=1:4521, Grupos_kmean_pca=d_tsne_1$Grupos) %>% data.frame()# Con PCA

Data <- 
  Data1 %>% 
  left_join(
    
    Data2
  )

table(Data$GrupoKmeans,Data$Grupos_kmean_pca) %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq,  x = product(Var2), fill=Var1)) +
  theme(axis.text.x=element_text(angle=0, hjust= .5))+
  labs(x="K-means con PCA", y=" ",  title="K-means vs K-means con PCA")+
  guides(fill=guide_legend(title = "K-means", reverse = TRUE)) 

#===================================================================================
# Resumen de grupos

Data %>% 
  group_by(Grupos_kmean_pca) %>% 
  #group_by(GrupoKmedoid) %>% 
  summarise(
    CantidadClientes = n(),
    Edad = median(age),
    Perc_admin = sum(job=="admin.")/CantidadClientes,
    Perc_blue = sum(job=="blue-collar")/CantidadClientes,
    Perc_entr = sum(job=="entrepreneur")/CantidadClientes,
    Perc_hous = sum(job=="housemaid")/CantidadClientes,
    Perc_manag = sum(job=="management")/CantidadClientes,
    Perc_tech = sum(job=="technician")/CantidadClientes,
    Perc_ret = sum(job=="retired")/CantidadClientes,
    Perc_unem = sum(job=="unemployed")/CantidadClientes,
    Perc_stu = sum(job=="student")/CantidadClientes,
    
    Perc_divorced = sum(marital=="divorced")/CantidadClientes,
    Perc_married = sum(marital=="married")/CantidadClientes,
    Perc_single = sum(marital=="single")/CantidadClientes,
    
    Perc_prim = sum(education=="primary")/CantidadClientes,
    Perc_sec = sum(education=="secondary")/CantidadClientes,
    Perc_tert = sum(education=="tertiary")/CantidadClientes,
    
    balance = median(balance),
    
    Perc_housing = sum(housing=="yes")/CantidadClientes,
    
    Perc_loan = sum(loan=="yes")/CantidadClientes,
    
    Perc_depos = sum(y=="yes")/CantidadClientes
    
  ) %>% 
  data.frame() %>% 
  write.csv("Summary_KmeansConPCA.csv",
            row.names = F)

Data %>% 
write.csv("kmeansPCA.csv")



#===================================================================================
#===================================================================================
# K-medoids con PCA

#======================================================================================
#======================================================================================
# Variables dummy

Data2 <- Data %>% 
  dummy.data.frame( all = TRUE)

#======================================================================================
#======================================================================================
# Estandarizando las variables. 

Data3 <- Data2 %>%
  scale()

colMeans(Data3)  # Media es cero luego de escalar
apply(Data3, 2, sd) # desviacion estandar es 1

#=======================================================================================

pc <- prcomp(Data3, scale=T)

summary(pc)

round(cov(Data3),1)


plot(pc, type='l')
summary(pc)

Plot <- data.frame(comp=1:27, varianza=pc$sdev)

ggplot(data=Plot, 
       aes(x=comp, y=varianza)) +
  geom_line()+
  geom_point() +
  xlab("Número de Componente") +
  ylab("Varianza") +
  theme(legend.position="top") + 
  geom_vline(xintercept = 8,color = "#C00000")

fviz_screeplot(pc, ncp=27, addlabels=T, ggtheme = theme_gray())

comp <- data.frame(pc$x[,1:8])

#==========
#start.time <- Sys.time()
Data<- read.csv("bank.csv")

gower_dist <- daisy(comp,
                    metric = "gower",
                    type = list(logratio = 3))

gower_mat <- as.matrix(gower_dist)

#  silhouette para muchos k usando PAM

sil_width <- c(NA)

for(i in 2:15){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Ploteo sihouette (Mientras mas mejor)
# 7 
fviz_nbclust(comp, pam, method = "silhouette")+
  theme_classic()

# plot(1:15, sil_width,
#      xlab = "Numero de clusters",
#      ylab = "Ancho Silhouette")
# lines(1:15, sil_width)


pam_fit <- pam(gower_dist, diss = TRUE, k = 7)

pam_results <- Data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#================================================================================
# comparando algoritmo sin PCA y algoritmo con PCA

Data1<- 
  read.csv("Kmedoids.csv") 

Data1 <- cbind(dni=1:4521, Data1)


Data2 <- cbind(dni=1:4521, Grupos_kmedoids_pca=tsne_data$cluster) %>% data.frame()

Data <- 
  Data1 %>% 
  left_join(
    
    Data2
  )

table(Data$GrupoKmedoid,Data$Grupos_kmedoids_pca) %>% 
  ggplot() +
  geom_mosaic(aes(weight = Freq,  x = product(Var2), fill=Var1)) +
  theme(axis.text.x=element_text(angle=0, hjust= .5))+
  labs(x="K-medoids con PCA", y=" ",  title="K-medoids vs K-medoids con PCA")+
  guides(fill=guide_legend(title = "K-medoids", reverse = TRUE)) 




Data %>% 
  group_by(Grupos_kmedoids_pca) %>% 
  #group_by(GrupoKmedoid) %>% 
  summarise(
    CantidadClientes = n(),
    Edad = median(age),
    Perc_admin = sum(job=="admin.")/CantidadClientes,
    Perc_blue = sum(job=="blue-collar")/CantidadClientes,
    Perc_entr = sum(job=="entrepreneur")/CantidadClientes,
    Perc_hous = sum(job=="housemaid")/CantidadClientes,
    Perc_manag = sum(job=="management")/CantidadClientes,
    Perc_tech = sum(job=="technician")/CantidadClientes,
    Perc_ret = sum(job=="retired")/CantidadClientes,
    Perc_unem = sum(job=="unemployed")/CantidadClientes,
    Perc_stu = sum(job=="student")/CantidadClientes,
    
    Perc_divorced = sum(marital=="divorced")/CantidadClientes,
    Perc_married = sum(marital=="married")/CantidadClientes,
    Perc_single = sum(marital=="single")/CantidadClientes,
    
    Perc_prim = sum(education=="primary")/CantidadClientes,
    Perc_sec = sum(education=="secondary")/CantidadClientes,
    Perc_tert = sum(education=="tertiary")/CantidadClientes,
    
    balance = median(balance),
    
    Perc_housing = sum(housing=="yes")/CantidadClientes,
    
    Perc_loan = sum(loan=="yes")/CantidadClientes,
    
    Perc_depos = sum(y=="yes")/CantidadClientes
    
  ) %>% 
  data.frame() %>% 
  write.csv("Summary_KmedoidsConPCA.csv",
            row.names = F)

Data %>% 
  write.csv("kmedoidsPCA.csv")
