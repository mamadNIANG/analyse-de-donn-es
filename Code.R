install.packages("FactoMineR")
install.packages("factoextra")
install.packages("cluster")
install.packages("tidyr")


#Exo1 

#IMPORTATION DE LA BASE DE DONNEES

datafr<- read.csv("C:/Users/dionc/Desktop/Projet_analyse_donnees/Donnees/bdf.csv",sep=':')

#On commence par suprimer toutes les lignes relatif aux données manquantes de nos 6 variables

library(tidyr)
datafr=drop_na(datafr,DOMTRAV,TYPMEN2,CC,REVTOT,DIPLOPR,DIPLOCJ)

#On remplace les valeurs de DOMTRAV afin que la variable prenne 1 comme proba de succès et 0 sinon
datafr$DOMTRAV[datafr$DOMTRAV==2]<-0


#On décrit nos variables afin qu'elle soit considérés comme qualititives
datafr$qualTypmen2=as.factor(datafr$TYPMEN2)
datafr$qualCC=as.factor(datafr$CC)
datafr$qualDIPLOPR=as.factor(datafr$DIPLOPR)
datafr$qualDIPLOCJ=as.factor(datafr$DIPLOCJ)
datafr$qualDOMTRAV=as.factor(datafr$DOMTRAV)


#On réalise une régression logistique avec le modèle additif
mod0<-glm(qualDOMTRAV~qualTypmen2+qualCC+REVTOT+qualDIPLOPR+qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)


#modele multiplicatif
modm<-glm(qualDOMTRAV~qualTypmen2*qualCC*REVTOT*qualDIPLOPR*qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)


#On fait alors plusieurs regressions logistiques en tennant compte de la dépendance ou non de certaines variables 

#Un modèle avec dépendance entre le revenu et le degré d'urbanisation
mod1<-glm(qualDOMTRAV~qualTypmen2+qualCC*REVTOT+qualDIPLOPR+qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)



#Un Modèle avec dépendance entre les diplomes et la taille de la famille
mod2<-glm(qualDOMTRAV~qualTypmen2*qualDIPLOPR+qualCC+REVTOT+qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)


#modèle avec dépendance entre le revenu et la taille de la famille
mod3<-glm(qualDOMTRAV~qualCC+qualTypmen2*REVTOT+qualDIPLOPR+qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)


#Un modèle avec dépendance entre le revenu et les diplomes 
mod4<-glm(qualDOMTRAV~qualTypmen2+qualCC+REVTOT*(qualDIPLOPR+qualDIPLOCJ),family=quasibinomial(logit),datafr,weights = COEF)


#Un modèle avec dépendance entre le revenu, le degré d'urbanisation et la taille de la famille
mod5<-glm(qualDOMTRAV~qualTypmen2*qualCC*REVTOT+qualDIPLOPR+qualDIPLOCJ,family=quasibinomial(logit),datafr,weights = COEF)


#On effectue des tests de rapport de vraisemblance pour connaitre quel modèle explique au mieux notre variable qualDOMTRAV

X2test1<-deviance(mod0)-deviance(mod1)
proba<-1-pchisq(X2test1,df.residual(mod0)-df.residual(mod1))
cat("\n Test du rapport de vraisemblance :\n",X2test1," p-value : ",proba,"\n")

#On rejette mod0 au seuil de 5%

#On effectue un test de rapport de vraisemblance entre le modèle 1 et 2

X2test2<-deviance(mod2)-deviance(mod1)
proba<-1-pchisq(X2test2,df.residual(mod2)-df.residual(mod1))
cat("\n Test du rapport de vraisemblance :\n",X2test2," p-value : ",proba,"\n")

#On rejette mod2 au seuil de 5%

#On effectue un test de rapport de vraisemblance entre mod3 et les autres modeles 

X2test3<-deviance(mod3)-deviance(mod1)
proba<-1-pchisq(X2test3,df.residual(mod3)-df.residual(mod1))
cat("\n Test du rapport de vraisemblance :\n",X2test3," p-value : ",proba,"\n")

#On ne rejette pas mod3 au seuil de 5%

X2test4<-deviance(mod3)-deviance(mod4)
proba<-1-pchisq(X2test4,df.residual(mod3)-df.residual(mod4))
cat("\n Test du rapport de vraisemblance :\n",X2test4," p-value : ",proba,"\n")

#On ne rejette pas mod3 au seuil de 5%

X2test5<-deviance(mod3)-deviance(mod5)
proba<-1-pchisq(X2test5,df.residual(mod3)-df.residual(mod5))
cat("\n Test du rapport de vraisemblance :\n",X2test5," p-value : ",proba,"\n")

#On ne rejette pas mod3 au seuil de 5%

X2test5<-deviance(mod3)-deviance(modmulti)
proba<-1-pchisq(X2test5,df.residual(mod3)-df.residual(mod5))
cat("\n Test du rapport de vraisemblance :\n",X2test5," p-value : ",proba,"\n")
#On ne rejette pas le mod3 au seuil de 5%



#On conclut donc que c'est le modèle "mod3" qui explique au mieux l'emploie d'un ou une employée de maison

summary(mod3)

#les variables qui sont Significatives sont :
#Intercept (NON interprétable)
#qualCC2 
#qualcc4 
#qualcc5
#REVTOT
#qualTypmen 4
#qualDIPLOPR1 
#qualDIPLOPR5
#qualDIPLOPR6 
#qualDIPLOPR7

#Les ménages qui emploi un ou une employée de maison sont les ménages avec des revenus élévés, diplomés, habitant dans des zone très urbanisés et ayant  deux enfants.

#########################################################################################################################################################################################"

#EXO 2

villes_donnees=read.table("C:/Users/dionc/Desktop/Projet_analyse_donnees/Donnees/villes.txt",header=TRUE)
villes_donnees


#1) Pourcentage d'inertie expliquée par le premier plan factoriel : 

library(FactoMineR)
library(factoextra)

villes_donnees.pca<-PCA(villes_donnees,quanti.sup=13:16, graph = FALSE)
villes_donnees.pca$eig 

#Le pourcentage d'inertie expliquée par le 1er plan factoriel est la somme des variance expliqué par les 2 premiers facteurs soit environ 99%


#Représentation des individus sur le premier plan factoriel  et du cercle des corrélations

plot(villes_donnees.pca,choix="ind", habillage=13, cex=0.7)

plot(villes_donnees.pca,choix="var", habillage=13, cex=0.7)


#cos2 des villes 

villes_donnees.pca$ind$cos2[,1:2]

# Partie 2 classification
# classification (k-means) 

km <- kmeans(villes_donnees,5,nstart=10)
km


#2) Classficiation hiérarchique - dendrogramme 

library(cluster)

classi_villes=hclust(dist(villes_donnees),method="ward.D")
classi_villes

plot(classi_villes)



################################################################################################################################################

# EXO3


#Chargement des données
univ<-read.csv(file="C:/Users/dionc/Desktop/Projet_analyse_donnees/universite.csv", sep=";", row.names = 1)


#ACP
library(FactoMineR)
library(factoextra)

res.ca<-CA(univ, col.sup=7:12, graph=TRUE)

eig.val <- get_eigenvalue(res.ca)
eig.val

#Le pourcentage d’inertie expliqué par le premier plan factoriel est la somme de la variance expliquée par les deux premiers facteurs soit environ 86.23 %


#le 1er axe (verticale) oppose les cursus littéraires et les cursus scientifiques
#le second axe (horizontale) oppose les études longues aux études courtes


#calcul des cos2 pour voir si les points sont bien représentées ou pas
res.ca$col$cos2
res.ca$row$cos2


