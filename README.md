# Estimations par modèles non paramétriques

Le but de ce projet est de proposer, à partir de différents critères, des estimations non paramétriques d’une densité de probabilité ou d’une fonction de régression par la méthode du noyau de convolution. Le lien de l'étude complète est : https://www.academia.edu/44027401/R%C3%A9gression_et_Estimation_non_param%C3%A9trique_par_la_m%C3%A9thode_des_noyaux_cas_pratique_

### Partie 1
La première partie du projet est consacrée à l’estimation de la densité de probabilité (loi issue du mélange de deux lois gaussiennes) à partir de l’estimateur de **Parzen-Rozenblatt**. Cet estimateur est déterminé à partirde la fenêtre h qui minimise des critères qui sont cités dans la suite du document et selon le cas où la loi dela variable aléatoire soit connue ou non.

### Partie 2
La  deuxième  partie  utilise  l’estimateur  de  **Nadaraya-Watson**  pour  estimer  la  fonction  de  régression.  Cette partie est subdividée en deux blocs. Le premier détermine l’estimateur de Nadaraya-Watson lorsque toutes les données sont présentes et le second lorsque nous sommes en présence de données manquantes aussi bien dans la variable dépendante que dans les régresseurs.

Tous les résultats présents dans ce projet sont obtenus suite à des implémentations manuelles de fonctions d’estimations sur le logiciel `R`.

