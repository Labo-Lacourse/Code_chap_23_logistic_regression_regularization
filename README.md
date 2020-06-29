English will follow

# Algorithmes d’apprentissage et modèles statistiques
## Un exemple de régression logistique régularisée et de validation croisée pour prédire le décrochage scolaire
### Dans M. Corbière & N. Larivière (Eds.), Méthodes qualitatives, quantitatives et mixtes dans la recherche en sciences humaines, sociales et de la santé, 2e édition. Québec, QC : PUQ. (sous presse)
Contact: eric.lacourse@umontreal.ca
### Auteurs: Éric Lacourse, Charles-Édouard Giguère et Véronique Dupéré
#### Analyste: Charles-Édouard Giguère
#### Transcription du code: Clémentine Courdi

Pour citation:
Lacourse, E., Giguére, C.E., & Dupéré, V. (2020). Algorithmes d’apprentissage et modèles statistiques: Un exemple de régression logistique régularisée et de validation croisée pour prédire le décrochage scolaire. Dans M. Corbière & N. Larivière (Eds.), Méthodes qualitatives, quantitatives et mixtes dans la recherche en sciences humaines, sociales et de la santé, 2e édition. Québec, QC : PUQ. 

Le chapitre présente les avantages de l'utilisation de la régularisation dans l'analyse de régression linéaire et logistique. Afin d'illustrer les techniques de régularisation, nous donnons un exemple de régression logistique régularisée avec validation croisée cherchant à prédire le décrochage scolaire chez des élèves du secondaire au Québec. Avec 25 variables prédictives et un échantillon simulé de 1000 cas, les résultats de la régression logistique ordinaire sont comparés avec ceux de la régression avec régularisation ridge, lasso et elastic-net. 

Ce dépôt contient le code utilisé pour obtenir les résultats présentés dans la partie 2 du chapitre « Algorithmes d’apprentissage et modèles statistiques: Un exemple de régression logistique régularisée et de validation croisée pour prédire le décrochage scolaire ». Le code est disponibles en trois format:

    > Google Colab avec R magic
    
    > Environnement R dans Jupyter Notebook 
    
    > Format original du script dans R Studio en R Markdown
    
    
Les trois formats contiennent le même code, sauf à quelques exceptions près lorsque le code a dû être modifié pour s'adapter au format en question. Les résultats demeurent les mêmes peu importe le format de code utilisé.

##### Pour aller plus loin:

Voici quelques liens utiles pour approfondir votre compréhenison des concepts présentés dans le chapitre et utilisé dans l'analyse.

https://www.youtube.com/channel/UCtYLUTtgS3k1Fg4y5tAhLbw (Staquest machine learning serie, en anglais)

https://fr.wikipedia.org/wiki/Lasso_(statistiques)

https://fr.wikipedia.org/wiki/R%C3%A9gularisation_de_Tikhonov

https://en.wikipedia.org/wiki/Elastic_net_regularization (en anglais seulement)



# Machine learning algorithms and statistical models
## An example of regularization and cross validation in logistic regression to predict high school dropout in Quebec
### In M. Corbière & N. Larivière (Eds.), Qualitative, quantitative and mixed methods in human, social and health science research, 2nd edition. Québec, QC : PUQ. (in press)
Contact: eric.lacourse@umontreal.ca
### Authors: Éric Lacourse, Charles-Édouard Giguère et Véronique Dupéré
#### Analyst: Charles-Édouard Giguère
#### Code transcription: Clémentine Courdi

Cite as:
Lacourse, E., Giguére, C.E., & Dupéré, V. (2020). Algorithmes d’apprentissage et modèles statistiques: Un exemple de régression logistique régularisée et de validation croisée pour prédire le décrochage scolaire. In M. Corbière & N. Larivière (Eds.), Méthodes qualitatives, quantitatives et mixtes dans la recherche en sciences humaines, sociales et de la santé, 2nd edition. Québec, QC : PUQ. 

This chapter presents the advantages of using regularization methods in linear and logistic regression analysis. To illustrate regularization and cross-validation technics, we give an example of regularized logistic regression looking to predict dropout status for Quebec high-school students. With 25 predictive variables and a simulated sample of 1000 observations, the results of the classic logistic regression are compared  with those of ridge, lasso and elastic-net regression.

This repository contains the code used to obtain the results presented in the second part of the chapter "Machine learning algorithms and statistical models: An example of regularization and cross validation in logistic regression to predict high school dropout in Quebec". The code is available in three formats:

    > Google Colab with R magic
    
    > R environment in Jupyter Notebook 
    
    > Original script format in R Studio as R Markdown
    
All three formats contain the same code, except for some instances when the code had to be modified to be adapted to said format. The results stay the same regardkess of the code format used.

##### Readings and ressources:

Here are a few links to different types of ressources to help deepen your understanding of the concepts presented in the chapter and used in the analysis.

https://www.youtube.com/channel/UCtYLUTtgS3k1Fg4y5tAhLbw (Staquest machine learning serie)

https://en.wikipedia.org/wiki/Lasso_(statistics)

https://en.wikipedia.org/wiki/Tikhonov_regularization

https://en.wikipedia.org/wiki/Elastic_net_regularization



