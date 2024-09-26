***********************************************************************************
				* TP ECONOMETRIE DES VARIABLES QUALITATIVES *
							* AMADOU Moussa ISE2 *
***********************************************************************************

*******************________________DEBUT____________________***********************


clear all // vider la mémoire si necessaire 
set more off // Pas d'interruption avant la fin du .do

********************REPERTOIRE DE TRAVAIL***************************************** 

cd "D:\ISE2_2023-2024\SEMESTRE2\PROJET\varqual"

*********************************************************************************


***************Chargement de la base de données**********************************
/* 
La base téléchargée sur Afrobaromètre étant sous format spss, nous allons l'importer puis après la sauvegardée sous format stata.
*/

import spss "NGR_R9.data_.final_.wtd_release.14Feb23_updated.21Feb23.sav"

*******************SAUVEGARDE SOUS FORMAT STATA********************************

save "donnee_round9_Niger.dta"

******importation à nouveau de notre base stataisticien************************

use "donnee_round9_Niger.dta",clear

* Apurement de la base de données
*avant de commencer les analyses, interessons nous d'abord sur la qualité de la 
*base de données que nous allons utiliser

duplicates report 
duplicates drop

*________________Résumé sommaire des variables de la base______________*********
 
describe
summarize
codebook

/*______________________Partie1: (A)____________________________________________

*******************************************************************************
*/


/*_________________________________question n°1_________________________________

construire une variable dichotomique qui permet d’identifier les personnes vaccinées 
contre la Covid-19
*/*______________________________________________________________________________

**Tabulation de la variable Q58A à l'aide de la commande tab******************** 

tab Q58A
tab Q58A, nol // nol pour dire no label

/*commentaire:

La question Q58A. Avez-vous reçu une Statut_Vaccination contre la COVID-19, une 
ou deux doses ? 
a deux modalités à savoir 0 = Non et 1 = Oui. Ainsi on construit là variable à
 l'aide de la commande gen

*/

**Création de la variable dichotomique Statut_Vaccination

gen Statut_Vaccination = (Q58A==1)

**Labelisation de la variable dichotomique avec la commande label define et label values

label define yesno 1"Vaccinéé" 0"Non Vaccinée"
label values Statut_Vaccination yesno

** Tabulation de la variable dichotomique Statut_Vaccination  en spécifiant la pondération

tab Statut_Vaccination 


/*______________________________question n°2______________________________________

Présenter un tableau descriptif des différentes variables que vous jugez pertinentes
 pour étudier les raisons de la Statut_Vaccination
*/*________________________________________________________________________________


**Recodage de la variable age en classe d'age. Effectuons une tabulation de la variable age (Q1) afin de s'assurer de l'exhaustivité 

tab Q1 

/* commentaire :

On remarque qu'il n'y a pas la modalité 9999 c'est à dire ceux qui ne connaissent pas leur age.
 Ainsi en se basant sur le recodage de class d'age de Afrobarometre on a le recodage suivant : 
 
*/

recode Q1 (18/25=1 "18-25 ans")(26/35=2 "26-35 ans" )(36/45=3 "36-45 ans")(46/55=4 "46-55 ans")(56/65=5 "56-65 ans")(66/103=6 "plus de 66 ans"), gen(classe_age)

**Recodeage de la variable niveau d'instruction 
/*
En se servant du rapport final sur les données afrobaromètres round9 du Niger, nous avons opter pour ce recodage
*/
recode Q94 (0=1 "Aucune éducation formelle") (1/3=2 "Primaire") (4/5=3 "Secondaire") (6/9=4 "Post secondaire"), gen(education)

** Labelisation des variables choisies afin de faciliter la lecture 

label var THISINT "sexe"
label var URBRUR "milieu de résidence"
label var education "niveau d'instruction"
label var Q59 "confiance au gouvernement d'assurer que le vaccin de la covid 19 est sûr"
label var classe_age "classe d'âge"
label var Q57A "est tombé malade de la covid 19"

** RENONMONS CERATAINES VARIABLES à l'aide de la commande rename ou en forme abregée ren afin de ne pas se perdre avec le nom des variables.

ren THISINT sexe
ren URBRUR milieu
ren Q59 confiance_gouvernement
ren Q57A malade_de_covi_19

*______________ Statistiques descriptives des variables choisies___________________
*_____________________________________________________________________________

tab sexe
tab milieu 
tab classe_age 
tab education
tab confiance_gouvernement 
tab malade_de_covi_19 

**Tableau croisé et test de chi2 des variables jugées pertinentes avec la variable Statut_Vaccination et exportation vers excel

foreach var of varlist sexe milieu classe_age education confiance_gouvernement {
	tabout `var' Statut_Vaccination using"Tableaux_croise_descriptif.xls",append c(freq row) stats(chi2)
}


/*______________________________question n°3____________________________________

Estimer puis présenter dans un tableau synthétique les coefficients des modèles logit, 
probit et OLS du statut de Statut_Vaccination. 
*/*______________________________________________________________________________

***Creation d'une variable globale Statut_Vaccination qui prends les variables pertinentes. 
*Ici l'intérêt est qu'on a plus besoin de spécifier toutes les variables d'intérêts, on aura juste besoin d'appeler la variable globale crée.

global explicatives sexe milieu classe_age education confiance_gouvernement malade_de_covi_19

***pour tenir compte de l'heterocedasticite rendre robuste comparaison entre les modèles, ci pour ajouter les intervalle de confiance pour exporter outreg2

*Pour le modèle logit : 

logit Statut_Vaccination $explicatives, robust 

outreg2 using "resultats_model.xls", replace bfmt() ctitle("modele logit") dec(3) alpha(0.01,0.05) symbol(**,*) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)') ci

*Pour le modèle probit :

probit Statut_Vaccination $explicatives, robust

outreg2 using "resultats_model.xls", append bfmt() ctitle("modele probit") dec(3) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)',Pseudo R-squared, `e(r2_p)') ci

*Pour le modèle OLS : 

regress Statut_Vaccination $explicatives, robust

outreg2 using "resultats_model.xls", append bfmt() ctitle("modele OLS") dec(3) addstat(F test , e(p)) ci

/*_____________________________________________________________________________

Lequel des trois semblent meilleurs ? Pourquoi ?

-> Concernant la significativité globale du modèle : les modèles logit et probit sont globalement significatif 
car prob > chi2 est inférieure à 5%. De même le modèle ols est globalement significatif (prob>F est inférieure à 5%). 

-> Concernant la significativité des coefficients : Pour les modèles logit, probit et ols 
tous les coefficients sont significatifs au seuil de 5% à l'exception des variables sexe, niveau d'instruction, 
emploi principale, confiance au guide religieux et les malades du covid19.

-> Concernant le R2 et le pseudo R2 : le modèle logit a un pseudo R2 0,0869 tant 
disque le modèle probit a un pseudo R2 0,0864 et le modèle ols un R2 de 0,1076.

La spécification du modèle ols pour estimer un modèle dichotomique pose
plusieurs problèmes : La spécification est de nature sensible au codage

En se référent à la littérature on utilise généralement le modèle logit pour les 
phénomènes rare et probit pour les phénomènes normaux. Ainsi la pandémie de covid19
 étant un phénomène rare par conséquent son vaccin l'est également.

Au vu de tout ce qui précède on retient le modèle logit.

*/*_____________________________________________________________________________


/*_________________________question n°4________________________________________

Estimer les Odds ratio et les effets marginaux de la variable « Statut_Vaccination» en
utilisant un logit. Quel est le taux de bonne prédiction de votre modèle à 50%, 70%
et 80% ? Interpréter les résultats (maximum 20 lignes)

*/*_____________________________________________________________________________

***************** Estimation du modèle***************************************

logit Statut_Vaccination sexe milieu classe_age education confiance_gouvernement malade_de_covi_19, robust

outreg2 using "logit.xls", bfmt() ctitle(modele de base coefficients) rdec(3) pdec(3) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)') ci replace 

***********************************************************************************

******Ici nous allons dans un premier temps estimer le modèle sans fixer une modalité de référence 
logit Statut_Vaccination sexe milieu classe_age education confiance_gouvernement malade_de_covi_19, or robust

*==Exportation des résultats vers excel
outreg2 using "result_log.xls", eform ctitle(Odds ratio) rdec(3) pdec(3) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)',Pseudo R-squared, `e(r2_p)') replace 

******* Estimation des efftes marginaux 
mfx compute 

outreg2 using "result_log.xls", mfx ctitle(Effet marginal) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)',Pseudo R-squared, `e(r2_p)') append

***** Estimation du modèle en fixant la première modalité comme référence  

logit Statut_Vaccination i.(sexe milieu classe_age education confiance_gouvernement malade_de_covi_19), or robust

** Tests de validation du modèle 
estat gof // Test d'adéquation de Hosmer et Lemeshow. La commande lfit teste l'adéquation du modèle 
fitstat  // détails sur R2 McFadden, Nagelkerk, etc.
lroc // La courbe de roc
***Quel est le taux de bonne prédiction de votre modèle à 50%, 70% et 80% ?
estat classification, cutoff(0.5) // taux de prédiction du modèle 50% 
estat classification, cutoff(0.7) // taux de prédiction du modèle  70%
estat classification, cutoff(0.8) // taux de prédiction du modèle 80%
*==Exportation des résultats vers excel
outreg2 using "res.xls", eform ctitle(Odds ratio) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **)  replace 

******* Estimation des efftes marginaux 
margins, dydx(_all) post
outreg2 using "res.xls",  ctitle(margins) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **)  append

/*______________________question n°5____________________________________________

Faire un tirage aléatoire stratifié (suivant le milieu de résidence) parmi les non
vaccinés pour obtenir un nouvel échantillon dans lequel les personnes vaccinées 
représenteraient 40%. (NB. Dans le cas où le taux de Statut_Vaccination
dépasse 40% choisir un seuil raisonnable à justifier et procéder au tirage)

*/*______________________________________________________________________________

tab Statut_Vaccination

* Les personnes vaccinées représentent 40,17% de l'échantillon(482). Ainsi en se
* basant sur le rapport l'unicef <<country office annual report 200>> on retient un taux raisonnable de 45% des personnes vaccinées dans le nouvel echantillon.

* Determinons alors la taille de l'echantillon :
*Soit n la taille du nouvel echantillon on a:(482/n)*100=45% -> n=(482*100)/45=1077,1111  
display (482*100)/45

*La taille du nouvel echantillon est donc sensiblement égal à 1071

* taille des personnes non vaccinées à tirer est : 1077-482 = 589
display 1071-482

* On aura donc à tirer 589 personnes parmi les non vaccinées.

*** Procédure du tirage aleatoire stratifié par milieu.

ta milieu Statut_Vaccination, nofreq col

* Nous avons 22,84% de personnes non vaccinées en milieu urbain et 77,16% en milieu rural.

*Ainsi en milieu urbain on aura a tirer 589*22,84/100 = 134 et en milieu rural on aura 589*77,16/100 = 455

sort milieu // trier d'abord la base suivant le milieu de résidence 

*** Milieu urbain 
sample  134 if Statut_Vaccination==0  & milieu==1 , count
 
*** Milieu rural
sample  455 if Statut_Vaccination==0  & milieu==2 , count

**Tabulation entre le milieu de résidence et le statut de vaccination
tab milieu  Statut_Vaccination
****************************************************************************
****************************************************************************
*__________________________question n°6________________________________________

/*

Refaire l'estimation de la question 2. Comparer dans un tableau les coefficients
des deux modèles logit. 

*/*________________________________________________________

logit Statut_Vaccination sexe milieu classe_age education confiance_gouvernement malade_de_covi_19, robust

outreg2 using "logit.xls", bfmt() ctitle(modele rechantillonnage coefficients) rdec(3) pdec(3) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)') ci append

/*_________________________question n°7_________________________________________

Faire une estimation séparée (ensemble, urbain, rural) et présenter dans un
même graphique les résultats des odds ratio et exportation des résultats dans un tableau 
*/*_______________________________________________________________________________

logit Statut_Vaccination sexe milieu classe_age education confiance_gouvernement malade_de_covi_19, or robust // ensemble

outreg2 using "ex.xls", eform ctitle(Odds ratio) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **)  replace 

logit Statut_Vaccination sexe classe_age education confiance_gouvernement malade_de_covi_19 if milieu == 1 , or robust // urbain

outreg2 using "ex.xls", eform ctitle(Odds ratio) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **)  append 

logit Statut_Vaccination sexe classe_age education confiance_gouvernement malade_de_covi_19 if milieu == 2 ,or robust  // rural

outreg2 using "ex.xls", eform ctitle(Odds ratio) bdec(3) sdec(3) alpha(0.01, 0.05) symbol(***, **)  append 

/*____________________________question n°8_______________________________________. 

Y a-t-il une variable susceptible d'être endogène au modèle ? Si oui, quelle
solution vous proposez ?

*/*_______________________________________________________________________________

logit malade_de_covi_19 sexe milieu classe_age education confiance_gouvernement , robust 
** Aucune variable n'est susceptible d'être endogène au modèle

/*___________________________Partie1: (B)________________________________________

Cette partie s'appuie sur la base de données « base_estimation_enfants.dta »
L'objectif est de modéliser l'effet de l'alphabétisation des femmes sur la santé
infantile. Les indicateurs de santé infantile sont « stunted_growth ; Underweight ;
emaciation)
*/*______________________________________________________________________________


/*___________________________question: 1_______________________________________

Estimer séparément l'effet de la variable « literacy » sur les indicateurs de
santé infantile. Proposer des variables de contrôle pour affiner vos résultats.
Présenter les résultats dans un tableau synthétique. Commenter 
*/*___________________________________________________________________________

* Importation de la base de données

use "base_estimation_enfants.dta",clear
*avant de commencer les analyses, interessons nous d'abord sur la qualité de la 
*base de données que nous allons utiliser
duplicates report 
duplicates drop
*Describtion de la base
desc
** Labelisation des variables afin de faciliter la lecture 
label var literacy "alphabétisation"
label var stunted_growth "retard de croissance"
label var emaciation "amaigrissement"
label var Underweight "insuffisance pondérale"

*Tabulation les indicateurs de santé infantile. 
tab stunted_growth
tab emaciation
tab Underweight

*Effet de la variable « literacy » sur le retard de croissance (stunted_growth)

logit stunted_growth i.literacy

*==Exportation des résultats vers excel
outreg2 using "effet.xls", bfmt() ctitle(coefstunted_growth) bdec(3)  alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  replace

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel


** Commentaire 

*Le retard de croissance à tendance à diminuer plus on est alphabète contrairement. 

*Effet de la variable « literacy » sur l'amaigrissement (emaciation)

logit emaciation i.literacy

*==Exportation des résultats vers excel
outreg2 using "effet.xls", bfmt() ctitle(coefemaciation)  bdec(3)  alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  append

*commentaire

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel
 

*Effet de la variable « literacy » sur l'insuffisance pondérale (Underweight)

logit Underweight i.literacy

*==Exportation des résultats vers excel
outreg2 using "effet.xls", bfmt() ctitle(coefUnderweight) bdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  append

*commentaire

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel

*variables de contrôle pour affiner vos résultats
* Renomons et labelisons les variables de controle
ren v701 nivinst
label var nivinst "niveau d'instruction"
label var residential_zone "milieu de residence"
** Nous avons retenu ici le niveau d'instruction et le milieu de residence 

logit stunted_growth i.literacy i.nivinst i.residential_zone

outreg2 using "control.xls", bfmt() ctitle(coefstunted_growth)  bdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  replace

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel

logit emaciation i.literacy i.nivinst i.residential_zone

outreg2 using "control.xls", bfmt() ctitle(coefemaciation)  bdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  append

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel

*Effet de la variable « literacy » sur l'insuffisance pondérale (Underweight)

logit Underweight i.literacy i.nivinst i.residential_zone

*==Exportation des résultats vers excel
outreg2 using "control.xls", bfmt() ctitle(coefUnderweight) bdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)', Pseudo R-squared, `e(r2_p)')  append

fitstat // Test d'hypotèses. On fera un clic droit pour copier le tableau vers excel

** lien entre les variables dependantes : on fait un clic droit pour importer les tableaux vers excel

tab stunted_growth Underweight, chi2
tab Underweight emaciation, chi2
tab stunted_growth emaciation, chi2

**commentaire: 	Le tableau ci-dessous présente un croisement entre les variables dépendnates et le test de chi2 réalisé. Au vu des résultats les p_values étant toutes inférieur à 5% on en deduit que les variables dépendantes sont liées au seuil fixé.

**== Si oui, quel type de modèle permet de prendre en compte cela ? 

/*

Pour prendre en compte le lien entre les variables dépendantes, il convient d'utiliser un modèle multivarié permettrait de prendre en compte ce lien, comme un modèle probit multivarié ou un modèle logistique multinomial. Dans le cadre de notre travail nous utilisons un modèle probit multivarié

*/

**== Estimer l'effet de la variable « literacy » sur la santé infantile dans le cadre d'un modèle qui prend en compte l'interdépendance potentielle entre les variables dépendantes. 

mvprobit (stunted_growth=literacy) (Underweight=literacy) (emaciation=literacy),robust
*exportation vers excel
outreg2 using "multiva.xls", bfmt() ctitle(coef) bdec(3) alpha(0.01, 0.05) symbol(***, **) addstat(wald chi2, `e(chi2)', Prob>chi2, `e(p)')  
*commentaire:

/*
la regression a été réalisée sur 5045 observations, le modèle est glbalement significatif. La variable literay est significatif au seuil de 5% sur les variables dépendantes stunted\_growth(retard de croissance) et  Underweight (insuffisance pondérale). Cependant elle est significatif de 1% sur la variable  dépendante emaciation (amaigrissement).

*/


*____________________Partie 2 : modèle multinomial______________________________
*_______________________________________________________________________________



*Cette partie s'appuie sur la base de données « base_contrat.dta »

clear all // vider la mémoire si necessaire 

* Importation de la base 

use "base_contrat.dta" , clear

* Avant tout voyons s'il n'y a pas de valeurs manquantes 

misstable summarize // Variables ayant des valeurs manquantes 
*commentaire: On peut voir que les b4 nivinst et malade ont des valeurs manquantes.

/*_________________________question n°1_________________________________________

On s'intéresse à la nature du contrat des travailleurs captée par la variable
« type_contrat ». Sous quelles conditions, il est possible de modéliser cette
variable.

*/*______________________________________________________________________________
 
tab type_contrat // pour voir les modalités de la variables 
*commentaire  Le tableau ci-dessous nous présente la variavle type\_contrat. Il ressort de ce tableau que cette variable est catégorielle avec 4 modalités. Nous somme dans le cadre d'un modèle multinomial non ordonné. Ainsi ce modèle repose sur les hypothèses suivantes: 
*-> l'Indépendence des Alternatives Non Pertinentes (IANP ou IIA en anglais pour Independance of Irrelevant Alternative). La condition est que le rapport de probabilités entre deux modalités est indépendant des autres modalités de la variable d'intérêt. 
*-> les co-variables sont les mêmes quelle que soit la modalité de y considéré (x dépend de i mais pas de j)

/*_________________________question n°2__________________________________________

 Faire un tableau synthétique des différentes variables que vous jugez pertinentes. 
 
*/*___________________________________________________________________________

des b2 b3 b4 nivinst malade p0 nbr_enfant
*commentaire: Le tableau ci-dessous présente une description des variables jugées indépendantes. Nous avons retenus éssentiellement le sexe, l'âge, la situation matrimoniale, le niveau d'instruction, la maladie, l'incidence de pauvreté et enfin le nombre d'enfant

*3. Estimer et présenter le modèle logit multinomial en fixant comme modalité de référence « CDI ».

mlogit type_contrat i.b2 b3 i.b4 i.nivinst i.malade nbr_enfant i.p0 , b(1)

eststo multinomial

outreg2 [multinomial] using "qual.xls", ctitle(" (1)") alpha(0.01, 0.05) symbol(***, **) aster label addstat(Pseudo R2, e(r2_p), Observations, e(N)) nocon nor2 word excel dec(3) noobs replace

*4. Estimer et présenter dans un tableau les risques relatifs. Commenter

mlogit type_contrat i.b2 b3 i.b4 i.nivinst i.malade i.p0 nbr_enfant,vce(robust) b(1) rrr

** Exportation vers excel
outreg2 using myfile.xls, stnum(replace coef=exp(coef), replace se=coef*se)
 
*5. Estimer et présenter dans un tableau les effets marginaux. Commenter

***Pour ce faire transformons les variables catégorielles en bianire. 

foreach x of varlist b2 b4 nivinst malade p0{
	tab `x', gen(`x')
	}


mlogit type_contrat b22 b42 b43 b44 b45 b46 b47 b48 b49 nivinst2 nivinst3 nivinst4 nivinst5 malade2 b3 nbr_enfant p02, baseoutcome(1) 

mfx compute,  predict(outcome(1)) // sortie des effets marginaux
outreg2 using resultat_mlogit.xls, mfx ctitle(effets marginaux )

*6. Effectuer le test IIA. Commenter 

mlogit type_contrat i.b2 b3 i.b4 i.nivinst i.malade nbr_enfant i.p0 , b(1)
mlogtest, all // pour avoir les test iia ou encore mlogtest, wald


* L'hypotèse IIA n'est donc pas vérifiée. Ainsi, on peut conclure que la probabilité qu'un individu prenne un caractère n'est pas indépendante d'un autre. Elle a tendance à varier et donc être peu fiable. Ainsi, le modèle multinomial n'est donc pas commode pour cette étude.  

*____________________Partie 3 : modèle Tobit et Heckman_________________________
*_______________________________________________________________________________


**Cette partie s'appuie sur la base de données « base_labor_market_estimation.dta» on utilisera le log du revenu****

clear all // vider la mémoire si necessaire 

use "base_labor_market_estimation.dta", clear

misstable summarize // Variables ayant des valeurs manquantes 
*les variables year_exp et ln_wage ont des valeurs manquantes.
*Etant donné qu'il n'y a pas de métadonnées sur la base de données pour mieux comprendre la base, nous avions opter pour une imputation des valeurs manquantes par 0 pour les variables ln-wage et year-exp

**1. On s'intéresse à identifier les déterminants de la rémunération captée par la variable « revenu » dans la base de données. Quel problème de modélisation soulève cette variable ? (maximum 5 lignes)

*labelisation de la variable labor_part
label var labor_part "partie travail"
* recodage de la variable labor_part
recode labor_part (0=0 ) (1=1 )
*labelisation
label define labor 0 "non_travailleur" 1 "travailleur"
label values labor_part labor

* tabulation entre labor_part et revunu
tab revenu if labor_part==0 // on 615 individus non travailluers

**2. Faire un tableau synthétique des différentes variables que vous jugez pertinentes pour le problème posé. Commenter (maximum 10 lignes).

describe age sexe revenu niv_educ nbr_enfant stat_matrim labor_part health_problem nbr_year_educ

**3. Estimer un modèle linéaire sur l'ensemble de l'échantillon puis dans le souséchantillon des travailleurs Que constatez-vous ? 

* Avant de commencer faisons quelques transformations.
replace ln_wage=0 if ln_wage==. // comme les non travailleurs n'ont pas de revenu on leur revenu par remplace par 0.
replace year_exp=0 if year_exp==. // on remplace également les valeurs manquantes pour year_exp par 0.

* Estimation du modele lineaire sur l'ensemble de l'echantillon
reg ln_wage age sexe nbr_enfant milieu niv_educ stat_matrim labor_part health_problem year_exp nbr_year_educ region

outreg2 using ols.xls, ctitle (coef_ensemble de l'echantillon) replace 
* Estimation du modele lineaire sur l'echantillon des travailleurs 
reg ln_wage age sexe nbr_enfant milieu niv_educ stat_matrim labor_part health_problem year_exp nbr_year_educ region if labor_part == 1 

outreg2 using ols.xls, ctitle (coef_echantillon des travailleurs) append 
** Prédire les revenus pour les non travailleurs ? 
predict revenu_predit if labor_part == 0 // prédiction du revenu des non travailleurs 
gen revenu_pred = exp(revenu_predit) // pour avoir la vraie valeur prédite
sum revenu_pred
**4. Estimer un modèle de sélection de Heckman avec les mêmes variables. Interpréter le ratio de Mills. Puis commenter les résultats. (maximum 10 lignes)

* Les variables significatives qui expliquenet le revenu  sont : l'age, le sexe, region,le milieu, health_problem,nbr_enfant, le nombre d'années d'expériences, la region et labor_part

gen travailleur = (ln_wage>0)

heckman ln_wage sexe age milieu year_exp health_problem region labor_part nbr_enfant, select(travailleur=stat_matrim niv_educ nbr_year_educ ) twostep

// Le coefficient de l'inverse de Mills est significatif, alors, la base présente un biais de sélection; par suite, il est préférable de modéliser le phénomène à partie de la base avec le modèle Hecman à deux étapes. On peut donc dire que le modèle de regression simple n'est pas approprié. 
** Prédire les revenus pour les non travailleurs ? 
predict  salaire_predict if travailleur==0 // prédiction du revenu des non travailleurs 
gen salaire_pred = exp(salaire_predict) // pour avoir la vraie valeur prédite

sum salaire_pred

*5-Est-ce que le fait d'être marié et le fait d'avoir un enfant sont-elles endogènes ? Justifier ? Quelles solutions préconisez-vous s'il y en a ?
 
*Le fait d'être marié et d'avoir un enfant peuvent être endogènes, c'est-à-dire que ces variables peuvent être corrélées avec des facteurs non observés qui influencent également le revenu. Pour traiter ce problème, on pourrait envisager d'utiliser des variables instrumentales qui seraient corrélées avec le statut marital et le nombre d'enfants, mais pas avec le revenu.