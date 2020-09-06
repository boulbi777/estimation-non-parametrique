# Estimations par modèles non paramétriques

Le but de ce projet est de proposer, à partir de différents critères, des estimations non paramétriques d’une densité de probabilité ou d’une fonction de régression par la méthode du noyau de convolution.

### Partie 1
La première partie du document est consacrée à l’estimation de la densité de probabilité (loi issue du mélangede deux lois gaussiennes) à partir de l’estimateur de Parzen-Rozenblatt. Cet estimateur est déterminé à partirde la fenêtre h qui minimise des critères qui sont cités dans la suite du document et selon le cas où la loi dela variable aléatoire soit connue ou non.

### Partie 2
La  deuxième  partie  utilise  l’estimateur  de  Nadaraya-Watson  pour  estimer  la  fonction  de  régression.  Cettepartie est subdividée en deux blocs. Le premier détermine l’estimateur de Nadaraya-Watson lorsque toutesles données sont présentes et le second lorsque nous sommes en présence de données manquantes aussi biendans la variable dépendante que dans les régresseurs.

Tous les résultats présents dans ce document sont obtenu suite à desimplémentations manuellesde fonc-tions d’estimations sur le logiciel `R`.







