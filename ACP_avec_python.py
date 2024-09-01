import pandas as pd  # pour importer les data
import numpy as np
#import matplotlib as mp

# Chager les données sur les performances d'athlètes olympques
"""
 Ce jeu de données contient 41 entrées, qui décrivent chacune les performances d'un athlète à une compétition de décathlon.
 Nous allons faire une ACP des dix variables décrivant les performances à chacune des épreuves du décathlon 
 (100 mètres, saut en longueur, lancer de poids, saut en hauteur, 400 mètres, 110 mètres haies, lancer de disque, saut à la perche, javelot et 1 500 mètres)
"""
data = pd.read_csv('decathlon.txt', sep= '\t')
print(data.head())

# enlevons les colonnes qui ne correspondent pas à des performances

my_data = data.drop(['Rank', 'Points', 'Competition'], axis=1)

# Convertir les data en un array numpy que je vais appeler X

X = my_data.values
print(X.shape)

# nous avons fait l'hypothèse que les données étaient centrées. Nous allons donc commencer par les standardiser.
# Pour selon on utilise sklearn avec sa classe preprocessing
from sklearn import preprocessing
from sklearn import preprocessing

std_scale = preprocessing.StandardScaler().fit(X)
X_scaled = std_scale.transform(X)

# Je peux maintenant faire mon ACP tjrs en utilisant sklearn avec sa classe decomposition
# Calculons maintenant les deux premières composantes principales :

from sklearn import decomposition
pca = decomposition.PCA(n_components=2)
pca.fit(X_scaled)

#  Maintenant on avoir le pourcentage de variance expliquée par chacune des composantes comme suit
print(pca.explained_variance_ratio_)
print(pca.explained_variance_ratio_.cumsum())  # pourcentage cumulé
"""La première composante explique environ un tiers de la variance observée dans les données, 
    et la deuxième 17,3 %. Au total, ces deux composantes expliquent 50 % de la variance totale, 
    en utilisant seulement un cinquième des dimensions initiales.
"""
# représenter chaque athlète/compétition selon ces deux dimensions uniquement,
# et colorer chacun des points correspondants en fonction du classement de l'athlète lors de cette compétition.

import matplotlib.pyplot as plt


# projeter X sur les composantes principales
X_projected = pca.transform(X_scaled)

# afficher chaque observation
plt.scatter(X_projected[:, 0], X_projected[:, 1],
    # colorer en utilisant la variable 'Rank'
    c=data.Rank)

plt.xlim([-5.5, 5.5])
plt.ylim([-4, 4])
plt.colorbar()
plt.show()

"""Pour mieux comprendre ce que capturent ces composantes principales, nous pouvons utiliser pca.components_  , 
qui nous donne les coordonnées des composantes principales dans l'espace initial (celui à 10 variables). 
Nous allons afficher, pour chacune des 10 performances, un point dont l'abscisse sera sa contribution à la première PC, 
et l'ordonnée sa contribution à la deuxième PC
"""
pcs = pca.components_

for i, (x, y) in enumerate(zip(pcs[0, :], pcs[1, :])):
    # Afficher un segment de l'origine au point (x, y)
    plt.plot([0, x], [0, y], color='k')
    # Afficher le nom (data.columns[i]) de la performance
    plt.text(x, y, data.columns[i], fontsize='14')

# Afficher une ligne horizontale y=0
plt.plot([-0.7, 0.7], [0, 0], color='grey', ls='--')

# Afficher une ligne verticale x=0
plt.plot([0, 0], [-0.7, 0.7], color='grey', ls='--')

plt.xlim([-0.7, 0.7])
plt.ylim([-0.7, 0.7])
plt.show()

