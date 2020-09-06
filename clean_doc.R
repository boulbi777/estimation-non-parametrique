#les librairies à importer
library(kedd)
library(magrittr)
library(plotly)
library(mice)
library(xtable)

#Fixer le chiffre générateur pour un souci de replicabilité de notre travail
set.seed(54)

########## Partie A ###############

############ Question 1 ##############

#Create function to generate data
generate_X = function(N){
  
  components <- sample(1:2,prob=c(1/3,2/3),size=N,replace=TRUE)
  mus <- c(-5,2)
  sds <- sqrt(c(1,9))
  
  x <- rnorm(n=N,mean=mus[components],sd=sds[components])
  
  return(x)
}

# Simulation
N <- 10000 # taille de l'échantillon

# Génération de l'échantillon
X = generate_X(N)

# Fonction d'Estimateur de Parzen
f_hat_function = function(n,h,K,X){
  return (function(x) mean(K((x-X)/h))/h)
}

#estimateur de parzen-rosenblatt pour différentes valeurs de h
f_hat = f_hat_function(n = N, h=1, K = dnorm, X=X)
f_hat_0 = f_hat_function(n = N, h=0.164, K = dnorm, X=X)
f_hat_1 = f_hat_function(n = N, h=0.75, K = dnorm, X=X)
f_hat_2 = f_hat_function(n = N, h=0.998, K = dnorm, X=X)
f_hat_3 = f_hat_function(n = N, h=1.01, K = dnorm, X=X)
f_hat_4 = f_hat_function(n = N, h=1.013, K = dnorm, X=X)


######## plot des graphiques #####
X_sorted = sort(X)
f_true = function(x) 1/3 * dnorm(x, mean=-5, sd=1) + 2/3 * dnorm(x, mean=2, sd=3)
y <- f_true(X_sorted)

#Calcul des valeurs des estimateurs en tous les points de X 
y_hat = numeric()
y_hat_0 = numeric()
y_hat_1 = numeric()
y_hat_2 = numeric()
y_hat_3 = numeric()
y_hat_4 = numeric()
for(i in 1:N){
  y_hat[i] = f_hat(X_sorted[i])
  y_hat_0[i] = f_hat_0(X_sorted[i])
  y_hat_1[i] = f_hat_1(X_sorted[i])
  y_hat_2[i] = f_hat_2(X_sorted[i])
  y_hat_3[i] = f_hat_3(X_sorted[i])
  y_hat_4[i] = f_hat_4(X_sorted[i])
}


# Plot des graphiques de manière superposée
plot(NA, xlim = range(X_sorted), ylim = range(y, y_hat), 
     xlab = "x", ylab = "density")
lines(X_sorted, y, col = "black", lwd = 5)
lines(X_sorted, y_hat, type = 'l', col = "tan2", lty=2, lwd=2)
lines(X_sorted, y_hat_0, col = "green" , lty=2, lwd=2)
lines(X_sorted, y_hat_1, type = 'l', col = "red" , lty=3, lwd=2)
lines(X_sorted, y_hat_2, col = "yellow" , lty=4, lwd=2)
lines(X_sorted, y_hat_3, type = 'l', col = "brown", lty=5, lwd=2)
lines(X_sorted, y_hat_4, col = "blue" , lty=6, lwd=2)
legend("topright", col=c("black", "tan2", "green", "red", "yellow", "brown", 
                         "blue"),
       legend = c("true_density", "h=0.3", "h=2", "MISE", "MSE", "MSE_1", "MSE_2"), 
       lty = 1:6, cex=0.9)
 

###### Question 2.b.  VALIDATION CROISEE #########

# Création d'une fonction pour diviser la base en train|test selon
# la méthode du leave one out
preprocess = function(h_value, N, X_sorted, y){
  y_pred = numeric()
  y_true = numeric()
  
  for(i in 1:N){
    X_train = X_sorted[-i]
    X_test  = X_sorted[i] 
    y_train = y[-i]
    y_test  = y[i]
    
    # Training and pred
    f_new_hat <- f_hat_function(n = N, h=h_value, K = dnorm, X=X_train)
    y_pred[i] = f_new_hat(X_test)
    y_true[i] = f_true(X_test) 
  }
  
  return  (list("y_true" = y_true, "y_pred" = y_pred))
}

#fonction pour faire la validation croisée selon les 3 différents critères de minimisation
h_cross_val = function(type, h_range){
  MSE_values = numeric(length = length(h_range))
  k = 0 #index for MSE_values
  
  if(type == "MSE_0"){
    for(h_value in h_range){
      k = k+1
      y_values = preprocess(h_value, N, X_sorted = X_sorted, y = y)
      y_true = y_values$y_true
      y_pred = y_values$y_pred
      # 
      MSE_values[k] = mean((y_true - y_pred)^2)
      print(paste(' ########  h = ', h_value, ' ##############', sep=""))
      print(paste("MSE_0 = ", MSE_values[k] , sep = ""))
      
    }
  }
  else if(type == "MSE_1"){
    for(h_value in h_range){
      k = k+1
      y_values = preprocess(h_value, N, X_sorted = X_sorted, y = y)
      y_true = y_values$y_true
      y_pred = y_values$y_pred

      # 
      MSE_values[k] = mean((y_true - y_pred)^2 * y_true)
      print(paste(' ########  h = ', h_value, ' ##############', sep=""))
      print(paste("MSE_1 = ", MSE_values[k] , sep = ""))
      
    }
  }
  else{
    for(h_value in h_range){
      k = k+1
      y_values = preprocess(h_value, N, X_sorted = X_sorted, y = y)
      y_true = y_values$y_true
      y_pred = y_values$y_pred

      # 
      MSE_values[k] = mean((y_true - y_pred)^2 * y_true^2)
      print(paste(' ########  h = ', h_value, ' ##############', sep=""))
      print(paste("MSE_2 = ", MSE_values[k] , sep = ""))
      
    }
  }
  print(paste("h_min = ", h_range[which(MSE_values == min(MSE_values))] , sep = ""))
  print(paste("CV_min = ", MSE_values %>% min() %>% formatC(format = "e"), sep = ""))
  return(MSE_values)
}


############################################################
#Choix optimal de h, on défnit h_range pour tester les différentes valeurs de h dans cet intervalle.
h_range = seq(0.05, 2.5, by = 0.001)

############### 1. MISE ##############"

# Calcul des valeurs de validation croisée
MISE_values = numeric()
k = 0 #index for RMSE_values
for(h_value in h_range){
  k = k+1

  f_new_hat <- f_hat_function(n = N, h=h_value, K = dnorm, X=X)
  integ = integrate(function(x) (f_new_hat(x) - f_true(x))^2, -Inf,Inf)$value

  MISE_values[k] = integ
  print(paste(' ########  h = ', h_value, ' ##############', sep=""))
  print(paste("MISE = ", integ, sep = ""))

}

# Valeur minimum
print(paste("h_min = ", h_range[which(MISE_values == min(MISE_values))] , sep = ""))
print(paste("CV_min = ", MISE_values %>% min() %>% formatC(format = "e"), sep = ""))

#Graphique
plot(h_range, MISE_values, col = '#d7191c', type='o', main = "h_min par CV selon MISE")
abline(v = h_range[which(MISE_values == min(MISE_values))])

#### conclusion 
# On prend h = 0.75

############## 2. MSE_0  ###############
# Validation croisée
MSE_values = h_cross_val(type = "MSE_0", h_range=h_range)


# Plot du graphique
plot(x=h_range, y=MSE_values, col = '#fdae61', type='o', main = "h_min par VC selon MSE")
abline(v = h_range[which(MSE_values == min(MSE_values))])
#### conclusion 
# On prend h = 0.988

##### 3. MSE_1 #############

# Validation croisée
MSE_values = h_cross_val(type = "MSE_1", h_range=h_range)

# Plot du graphique
plot(h_range, MSE_values, col = '#abdda4', type='o', main = "h_min par VC selon MSE_1")
abline(v = h_range[which(MSE_values == min(MSE_values))])

#Conclusion
# On prend h = 1.01

##### 4. MSE_2 #############

# Validation croisée
MSE_values = h_cross_val(type = "MSE_2", h_range=h_range)

# Plot du graphique
plot(h_range, MSE_values, col = '#2b83ba', type='o', main = "h_min par VC selon MSE_2")
abline(v = h_range[which(MSE_values == min(MSE_values))])

#Conclusion
# On prend h = 1.013

# fonction de représentation graphique des 4 estimateurs
plot_estimators = function(f_hat, X_sorted, title, h){
  
  y <- f_true(X_sorted)
  y_hat = numeric()
  for(i in 1:N){
    y_hat[i] = f_hat(X_sorted[i])
  }
  
  plot(NA, xlim = range(X_sorted), ylim = range(y, y_hat), 
       xlab = "x", ylab = paste(title,"density", sep=" "))
  lines(X_sorted, y, col = "black")
  lines(X_sorted, y_hat, type = 'l', col = "red")
  legend("topright", col=c("black", "red"), 
         legend = c("true_density", paste("estim_h = ", h, sep = "")), 
         lty = c(1,1),
         cex = 0.6)
}

# Fenêtre h des 4 estimateurs

#1er estimateur : h=0.75
f_hat = f_hat_function(n = N, h=0.75, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title = "MISE", h=0.75 )

#2e estimateur : h=0.988
f_hat = f_hat_function(n = N, h=0.988, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title = "MSE", h=0.988)

#3e estimateur : h=1.01
f_hat = f_hat_function(n = N, h=1.01, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title = "MSE_1", h=1.01 )

#1er estimateur : h=1.013
f_hat = f_hat_function(n = N, h=1.013, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title = "MSE_2", h=1.013 )


# on peut également utiliser la fonction optim pour trouver le résultat.
# D'autres choix de h sont également donnés sur R directement (package "kedd")
# Les critères de minimisation n'étant pas forcément les mêmes, 
# nous les affichons à titre indicatif

h.amise(X_sorted,deriv.order = 2)
h.amise.default()
h.mcv(X_sorted , lower = 0.05, upper = 1)
h.bcv(X_sorted, lower = 0.05, upper = 1)
h.tcv(X_sorted, lower = 0.05, upper = 1)
h.mlcv(X_sorted, lower = 0.05, upper = 1)
h.ucv(X_sorted, lower = 0.05, upper = 1)


#################### Question 2 ###############

#regle de la pouce

h_hat = 1.059*sd(X)*(N^(-1/5))
h_hat

# conclusion : on trouve h_min = 1.818194

# Affichage avec la valeur de h trouvéde prédemment
f_hat = f_hat_function(n = N, h=1.818194, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title  = "MISE", h=1.818194)


## cross val : minimiser J(h) = integrale(f_hat^2) - 2/n * sum(f_hat(X_i))

#Fonction de validation croisée pour minimiser le critèree J(h)
f_hat_function_2 = function(x,n,h,K,X){
  # Vecteur de densité calculé en chaque point
  dens_hat = numeric(length = length(X))
  
  for(i in 1:length(X)){
    dens_hat[i] = K((x-X[i])/h)
  }
  
  return (mean(dens_hat)/h)
}

#Calcul de J(h) pour différentes valeurs de h
J_h_cv = numeric(length(h_range))
for (k in 1:length(h_range)){
  res.hatfsquare = function(x) f_hat_function_2(x=x, n = N, h=h_range[k], K = dnorm, X=X)^2
  res.hatf_i_remove = numeric(length(X))
  for(i in 1:length(X)){
    res.hatf_i_remove[i] = f_hat_function_2(x = X[i], n = N, h=h_range[k], K = dnorm, X=X[-i]) 
    #print()
  }
  J_h = integrate(res.hatfsquare %>% Vectorize(),-Inf,Inf)$value - (2/N)*sum(res.hatf_i_remove)
  print(paste("######### h = ", h), sep="")
  print(J_h)
  
  J_h_cv[k] = J_h
}
# Affichage de la valeur minimale
h_range[which(J_h_cv == min(J_h_cv))]

# Conclusion : on trouve h=0.67

# Affichage du graphe
f_hat = f_hat_function(n = N, h=0.67, K = dnorm, X=X)
plot_estimators(f_hat, X_sorted, title  = "MISE", h=0.67 )

## commentaire : la deuxieme methode est nettement mieux avec h = 0.67

# Plot des 2 courbes
f_hat = f_hat_function(n = N, h=1.818194, K = dnorm, X=X)
f_hat_2 = f_hat_function(n = N, h=0.67, K = dnorm, X=X)
f_hat_0 = f_hat_function(n = N, h=0.75, K = dnorm, X=X) #Ancienne valeur de MISE

# Coparaison des méthodes : faire un graphe
y <- f_true(X_sorted)
y_hat = numeric()
y_hat_2 = numeric()
y_hat_0 = numeric()
for(i in 1:N){
  y_hat[i] = f_hat(X_sorted[i])
  y_hat_0[i] = f_hat_0(X_sorted[i])
  y_hat_2[i] = f_hat_2(X_sorted[i])
}


plot(NA, xlim = range(X_sorted), ylim = range(y, y_hat, y_hat_2, y_hat_0), 
     xlab = "x", ylab = "density")
lines(X_sorted, y, col = "black", lwd=4)
lines(X_sorted, y_hat, type = 'l', col = "#abdda4")
lines(X_sorted, y_hat_2, type = 'l', col = "#2b83ba")
lines(X_sorted, y_hat_0, type = 'l', col = "red")
legend("topright", col=c("black", "#abdda4", "#2b83ba", "red"), 
       legend = c("true_density", "pouce h = 1.81", "J_h=0.67", "MISE h = 0.75"), 
       lty = c(1,1),
       cex = 1)


#### Estimation du MSE ########

# Provient de la doc : 
#https://pdfs.semanticscholar.org/ef3e/6af6b3240ee3cf2eda40ddb690fd57bd2d12.pdf
#page 4 à 7

# Let's define the beta function
beta_function = function(t,a,b){
  return(
    t^(a-1) * (1-t)^(b-1)
  )
}

#Calcul des w_j_r
compute_w.j_r = function(r,n,t){
  w.j_r = numeric(n)
  for(j in 1:n){
    
    # w.j_r.1 = function(x) beta_function(t = x, a=r, b=n-r-1) %>% 
    #           integrate(lower = 0, upper = j/n)$value 
    #print(j)
    w.j_r.1 = integrate(
      function(x) beta_function(t = x, a=r, b=n-r+1), lower = 0, upper = j/n
    )$value 
    
    w.j_r.2 = integrate(
                function(x) beta_function(t = x, a=r, b=n-r+1), lower = 0, upper = (j-1)/n
              )$value 
    
    w.j_r[j] = w.j_r.1 - w.j_r.2
  }
  #print(w.j_r)
  return(w.j_r)
}

# Calcul de la fonction estimée de la MSE
MSE_hat_function = function(x, K, X, h, n){
  #Sort the X
  X = X %>% sort()
  
  Y_i = numeric(n)
    
  for(i in 1:n) Y_i[i] = K((x-X[i])/h)
  print(mean(Y_i))
  f.hat.x_h = (1/(n*h)) * sum(Y_i)
  
  
  #Calcul des mu_hat
  mu.hat = numeric(n)
  for(r in 1:n){
    mu.hat[i] = sum(compute_w.j_r(r,n) * Y_i) 
  }
  
  #Calcul de sigma_2_hat
  sigma_2_hat = numeric(n)
  for(r in 1:n){
    sigma_2_hat[i] = sum(compute_w.j_r(r,n) * (Y_i - mu.hat)^2) 
  }
  
  
  # mu.hat_f.hat
  mu.hat_f.hat = sum(mu.hat) / (n*h)
  
  # sigma_2_hat.f.hat
  sigma_2_hat.f.hat = sum(sigma_2_hat) / (n*h)^2
  
  return(sigma_2_hat.f.hat + (mu.hat_f.hat - f.hat.x_h)^2)
}

#test de la fonction pour différentes valeurs de h
MSE_hat_function(x=1, K=dnorm, X=X_sorted, h=1000, n=N)

  y_hat_a.2 = numeric(N)
y_hat_a.2_0 = numeric(N)
y_hat_a.2_1 = numeric(N)
for(i in 1:N){
  y_hat_a.2[i] = MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=1, n=N)
  y_hat_a.2_0[i] = MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=0.5, n=N)
  y_hat_a.2_1[i] = MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=1.5, n=N)
}

plot(X_sorted, f_true(X_sorted), col='black', type = 'l',
     ylim=range(y_hat_a.2, y_hat_a.2_0, y_hat_a.2_1, f_true(X_sorted)))
lines(X_sorted, y_hat_a.2, type = 'l', col='red', 
     ylim=range(y_hat_a.2, y_hat_a.2_0, y_hat_a.2_1, f_true(X_sorted)))

lines(X_sorted, y_hat_a.2_0, type = 'l', col=' blue', 
      ylim=range(y_hat_a.2, y_hat_a.2_0, y_hat_a.2_1, f_true(X_sorted)))
lines(X_sorted, y_hat_a.2_1, type = 'l', col='#00ffff', 
      ylim=range(y_hat_a.2, y_hat_a.2_0, y_hat_a.2_1, f_true(X_sorted)))
legend("topright", col=c("black", "red","blue", "#00ffff"),
       legend = c("true_dens", "MSE.h=1", "MSE.h=0.5", "MSE.h=1.5"), lty=c(1,1),cex=1)


#optimisation
optim(
  par = c(0),
  fn = function(x) MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=1, n=N)
)

h_min = numeric(N)
for (i in 1:N){
  h_min_ = optimize(
    f = function(h) MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=h, n=N),
    interval = range(h_range)
  )
  print(h_min_)
  h_min[i] = h_min_$minimum
}


h_min 

## 2e maniere d'optimiser : prendre h de 0 à l'infini

h_min.2 = numeric(N)
for (i in 1:10){
  h_min_ = optim(
    par = 0.5, 
    fn = function(h) MSE_hat_function(x=X_sorted[i], K=dnorm, X=X_sorted, h=h, n=N),
    lower = 0, upper = 10, 
    method = "L-BFGS-B"
  )
  print(h_min_)
  h_min.2[i] = h_min_$par
}

X_sorted[1]

for(h_val in seq(0.1,2000, length.out = 10)){
  #print(h_val)
  print(MSE_hat_function(x=X_sorted[1], K=dnorm, X=X_sorted, h=h_val, n=N))
}

## Affichage en 3D de la dfonction

# plotly 3D plot
x_vec = X_sorted[sample(1:N,size = 80,replace = FALSE) %>% sort()]
y_vec = seq(0.1,100, length.out = 30)
z_vec = numeric(length(x_vec) * length(y_vec))
k=0
for(x in x_vec){
  for(j in y_vec){
    k = k+1
    z_vec[k] = MSE_hat_function(x=x, K=dnorm, X=X_sorted, h=j, n=N)
  }
}

data <- list(
  x = x_vec,
  y = y_vec,
  z = matrix(z_vec, nrow=length(x_vec)),
  type = "surface")

# Affichage du plot
(p <- plot_ly(x = data$x, y = data$y, z = data$z) %>% add_surface())


### Partie B ##################
X2 = read.table("dens.txt") %>% sapply(as.numeric) %>% as.vector()
summary(X2)
#dev.off()
# Affichage de l'histogramme
(res = hist(X2, breaks = 65, col="#00ffff", freq = FALSE, xlab ="X" , main = "Histogramme de X"))
X2_sorted = X2 %>% sort()
N2 = length(X2)

# Estimation h par validation croisée biaisée pour la MISE
h.bcv(X2_sorted, lower = 0.05, upper = 1)

# Conclusion : on retrouve h = 0.3871074

# La courbe des fréquences de l'histogramme
breaks = numeric()
for(i in 1:(length(res$breaks) - 1)){
  breaks[i] = (res$breaks[i] + res$breaks[i+1])/2
}

y_hat_b = numeric()
for(i in 1:length(breaks)){
  y_hat_b[i] = f_hat_function_2(x = breaks[i], n = N2, h=0.3871074, K = dnorm, X=X2_sorted)
}

plot(NA, xlim = range(X2), ylim = range(res$density), ylab = "Density", xlab = "X")
lines(x = breaks, y = res$density, col = 'black', type = 'l')
lines(density(X2_sorted, bw = 0.3871074), col='red')
#lines(x=breaks,y_hat_b, col="red", type='l', lty=1, lwd=2)
legend("topright", col = c("black", "red"),
       legend = c("freq_curve", "estim_freq.h=0.3871074"), lty=c(1,1),cex=1)



###### Partie C  ############
# Lecture des données
reg = read.table("reg.txt") 
summary(reg)
names(reg) = c("X", "y")

# Affichage des points en 2D
plot(reg$X, reg$y, col="blue")

# Trier les points pour l'affichage plus tard
X3_sorted = reg$X %>% sort()
Y3_sorted = reg$y %>% sort()
N3 = nrow(reg)

# Fonction de regression de Nadaraya-Watson
r_hat_fun = function(x, X, y, K, h, N){
  w_i = numeric(N)
  
  for(i in 1:N){
    w_i[i] = K((x - X[i])/h)
  }
  
  denom = sum(K((x - X)/h))
  
  w_i = w_i/denom
  
  return(sum(w_i*y))
}

# Fonction en x
r_hat = function(x) r_hat_fun(x, X=reg$X, y=reg$y, h = 0.021, K = dnorm, N = N3)

# Ajout de la regression de nadaraya pour h=
lines(reg$X %>% sort(), reg$X %>% sort() %>% sapply(r_hat), type="l", col='black', lwd=3)

#Trouver le bon h avec MISE avec validation croisée
h_range2 = seq(0.001,1,by = 0.001)
MISE_values2 = numeric(N3)
MISE_reg_values = numeric(length(h_range2))
for(k in 1:length(h_range2)){
  for(i in 1:N3){
    MISE_values2[i] = (Y3_sorted[i] - r_hat_fun(x = X2_sorted[i], X=X2_sorted[-i], 
                                                y=Y3_sorted, h = h_range2[k], 
                                                K = dnorm, N = N3))^2
  }
  print(paste("##### h = ", h_range2[k], sep=''))
  MISE_reg_values[k] = mean(MISE_values2)
  print(MISE_reg_values[k])
}

print(paste("h_min = ", h_range2[which(MISE_reg_values == min(MISE_reg_values, na.rm = T))]))

# Utilisation de h_min trouvée pour ploter l'estimateur de nadaraya
r_hat = function(x) r_hat_fun(x, X=reg$X, y=reg$y, h = 0.21, K = dnorm, N = N3)
plot(h_range2, MISE_values2)

################ Partie D ####################
#Lecture des données
dta = read.table("reglinmd.txt")

# regression MCO classique qui supprime automatiquement
# les valeurs manquantes
summary(lm(Y ~., data=dta))

# Profiling des données manquantes
md.pattern(dta)

# Imputation des données par le package "MICE"
dta_imputed = mice(data = dta, method = 'pmm', seed = 2)
fit.mi = with(data=dta_imputed, exp = lm(Y~., data = dta_imputed$data))
combFit = pool(fit.mi) 
summary(combFit)

# Ecriture de la table en latex
xtable(summary(combFit), type = "latex")

