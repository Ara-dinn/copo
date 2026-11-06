# traitement COPO questionnaire ------

# packages -----
library(tidyverse)
library(questionr)
library(ggplot2)
install.packages("fastDummies")
library(fastDummies)
library(FactoMineR)
library(explor)

# données -----

landes <- read.csv2("2.données/fred-cd_COPO_6-5-2026_14_37.csv") # on a les communes des landes
tot <- read.csv2("2.données/fred-cd_COPO_8-25-2026_10_21.csv") # on a les communes des meuses

str(tot)

# remplacer les premeiers 98 lignes en "tot" par les donnes de "landes". 

copo_meuse<-  tot %>%  filter(N.Obs>=99) # 
copo <-  rbind(landes, copo_meuse) # on rassemble pour avoir les communes des deux terrains

# 
# write.csv2(copo, "2.données/prod/copo_complet.csv")
copo <- read.csv2("copo_complet.csv")


# copo <- copo %>%  rename (
#  nouveau = ancien)

# Rename des variables ---------
# Renomage des colonnes ----------
# Colonne 1 : identifiant de la variable
copo <- copo %>%  rename (id = N.Obs) 

# Suite renomage des colonnes
# Colonne 2 : consentement
copo <- copo %>%  rename (consent = X1..Ce.questionnaire.est.anonyme.et.ne.permet.pas.de.vous.identifier._Les_données.collectées.seront.traitées.de.manière.confidentielle.et.uniquement.dans_le.cadre.de.cette.étude.__.Nous.vous.informons.que.votre.participation.est.entièrement.volontaire...vous_pouvez.refuser.d.y.participer.sans.aucune.conséquence.__.Les.données.personnelles.collectées.sont.utilisées.aux.fins.du.projet.COPO._Elles.sont.utilisées.par.les.ingénieures.et.chercheurs.du.projet.et.seront_conservées.pour.une.durée.de.deux.ans..Le.traitement.des.données.repose.sur.une_mission.d.intérêt.public..conformément.à.l.article.6.1.e.du.RGPD.et.à.l.article_L123.2.du.Code.de.l.éducation.__.Conformément.à.la.Loi.Informatique.et.Libertés.du.6.janvier.1978.modifiée..vous_pouvez.accéder.et.obtenir.copie.des.données.vous.concernant..les.faire_rectifier.ou.les.faire.effacer..Vous.disposez.également.d.un.droit_d.opposition..de.limitation.du.traitement.de.vos.données.ainsi.que.du.droit.à_la.portabilité._Consentez.vous.à.ce.que.vos.réponses.soient_collectées..traitées.et.utilisées.dans.le.cadre.de.cette.enquête..conformément_à.la.réglementation.applicable.en.matière.de.protection.des.données..)

# Colonne 3 : prénom de l'enquêteur, enquêtrice
copo <- copo %>%  rename (prenom_enq = X2..Prénom.de.l.enquêteur)

# Colonne 4 : commune de résidence
copo <- copo %>%  rename (commune = X3..Quelle.est.votre.commune.de.résidence..)

# Colonne 5 : nombre de mois par an dans cette résidence
copo <- copo %>%  rename (mois_res = X4..Combien.de.mois..par.an.habitez.vous.dans.cette.commune..)

# Colonne 6 : année de naissance
copo <- copo %>%  rename (ann_naissance = X5..Quelle.est.votre.année.de.naissance..)

# Colonne 7 : genre
copo <- copo %>%  rename (genre = X6..Vous.êtes..)

# Colonne 8 : statut d'occupation du logement
copo <- copo %>%  rename (stat_occup = X7..Quel.est.votre.statut.d.occupation.de.votre.logement..)

# Colonne 9 : autre statut d'occupation
copo <- copo %>%  rename (autre_occup = X8..Autre..)

# Colonne 10 : modes de chauffages domestiques
copo <- copo %>%  rename (mode_chauffage = X9..Pouvez.vous.nous.indiquer.quel.est.ou.quels.sont.vos.modes.de.chauffage.domestiques..)

# Colonne 11 : autre mode de chauffage
copo <- copo %>%  rename (autre_mode_chauff = X10..autre.mode.de.chauffage..)

# Colonne 12 : nombre de stères de bois consommé par hiver
copo <- copo %>%  rename (stere = X11..Combien.de.stères.estimez.vous.brûler.en.moyenne.par.hiver..)

# Colonne 13 : mode d'approvisionnement en bois de chauffage
copo <- copo %>%  rename (appro_bois = X12..Comment.vous.procurez.vous.le.bois.de.chauffage.de.votre.foyer..)

# Colonne 14 : autre mode d'approvisionnement en bois
copo <- copo %>%  rename (autre_appro = X13..autre..)

# Colonne 15 : personnes en charge de l'approvisionnement en bois dans le foyer
copo <- copo %>%  rename (pers_appro = X14..Pour.votre.foyer..qui.s.occupe.de.couper..façonner..stocker.le.bois..)

# Colonne 16 : autre personne qui se charge de l'approvisionnement en bois
copo <- copo %>%  rename (autre_pers_appro = X15..autre..)

# Colonne 17 : nombre de jours consacrés à l'approvisionnement en bois par hiver dans le foyer
copo <- copo %>%  rename (jours_appro = X16..Dans.votre.foyer..combien.de.jours.sont.consacrés.par.an.pour.votre.approvisionnement.en.bois..__.en.comptant.la.coupe.en.forêt..le.façonnage..le.débardage..le.re.sciage.et.le.stockage.)

# Colonne 18 : cadre d'apprentissage à faire son bois
copo <- copo %>%  rename (apprentissage_bois = X17..Dans.quel.cadre.la.personne.qui.se.charge.principalement.de.faire.le.bois.pour.votre.foyer..a.t.elle.apprise.à.le.faire..)

# Colonne 19 : autre cadre d'apprentissage
copo <- copo %>%  rename (autre_apprentissage = X18..Autre..)

# Colonne 20 : équipements utilisés pour faire le bois
copo <- copo %>%  rename (equipement_utilise = X19.._Parmi.les.équipements.suivants..lesquels.sont.utilisés.dans.votre.foyer.pour.l.approvisionnement.en.bois.._._...Lecture.des.modalités.)

# Colonne 21 : autre équipement
copo <- copo %>%  rename (autre_equipement_util = X20..Autre..)

# Colonne 22 : équipement possédés pour faire du bois
copo <- copo %>%  rename (equipement_possede = X21..Parmi.les.équipements.utilisés.lesquels.possédez.vous..)

# colonne 23 : autre équipement possédé
copo <- copo %>%  rename (autre_equipement_possede = X22..Autre..)

# Colonne 24 : personne qui se charge de faire du feu
copo <- copo %>%  rename (pers_feu = X23..Dans.votre.foyer..qui.s.occupe.de.faire.du.feu..aller.chercher.le.bois..allumer.le_feu...)

# colonne 25 : autre personne qui se charge de faire du feu
copo <- copo %>%  rename (autre_pers_feu = X24..autre..)

# Colonne 26 : minutes par jour d'hiver consacrées à faire du feu
copo <- copo %>%  rename (minutes_feu = X25..Dans.votre.foyer..combien.de.temps.par.jour.d.hiver.est.consacré.à.faire.du.feu_.aller.chercher.le.bois..allumer.le.feu...)

# Colonne 27: Connaissance du montat des dépenses énergétiques du foyer
copo <- copo %>%  rename (connaissance_depense_ener = X26..Est.ce.que.vous.avez.une.idée.du.montant.des.dépenses.énergétiques.de.votre.foyer..électricité..gaz..fioul..pellet...)

# Colonne 28 : Mensualisation des factures d'électricité (oui/non)
copo <- copo %>%  rename (mensual_elec = X27..Est.ce.que.vous.avez.mensualisé.le.paiement.de.vos.factures.d.électricité..)

# Colonne 29 : Estimation du montant facture mensuelle d'électricité
copo <- copo %>%  rename (facture_mens_elec = X28..A.combien.estimez.vous.le.montant.mensuel.de.vos.factures_d.électricité..)

# Colonne 30 : Estimation du montant des factures annuelles d'électricité
copo <- copo %>%  rename (facture_ann_elec = X29..A.combien.estimez.vous.le.montant.annuel.de.vos.factures_d.électricité..)

# Colonne 31 : Mensualisation des facture de gaz (oui/non)
copo <- copo %>%  rename (mensual_gaz = X30..Est.ce.que.vous.avez.mensualisé.le.paiement.de.vos.factures.de.gaz..)

# Colonne 32 : Estimation du montant facture mensuelle de gaz
copo <- copo %>%  rename (facture_mens_gaz = X31..A.combien.estimez.vous.le.montant.mensuel.de.vos.factures_de.gaz..)

# Colonne 33  : Estimation du montant des factures annuelles de gaz
copo <- copo %>%  rename (facture_ann_gaz = X32..A.combien.estimez.vous.le.montant.annuel.de.vos.factures_de.gaz..)

# Colonne 34 : Estimation du montant des factures annuelles de fioul
copo <- copo %>%  rename (facture_ann_fioul = X33..A.combien.estimez.vous.le.montant.annuel.de.vos.factures.de_fioul..)

# Colonne 35 : Estimation du montant des factures annuelles de pellet
copo <- copo %>%  rename (facture_ann_pellet = X34..A.combien.estimez.vous.le.montant.annuel.de.vos.factures.de.pellet..)

# Colonne 36 : Equipement du logement en panneaux solaire pour production d'électricité (oui/non)
copo <- copo %>%  rename (panneaux_PV = X35..Avez.vous.équipé.votre.logement.de.panneaux.solaires.domestiques..pour.la.production.d.électricité...)

# Colonne 37 : Equipement du logement en double vitrage
copo <- copo %>%  rename (double_vitrage = X36..Est.ce.que.les.fenêtres.de.votre.maison.sont.en.double.vitrage..)

# Colonne 38 : Pensez-vous que votre logement est bien isolé ?
copo <- copo %>%  rename (bonne_isolation = X37..Pensez.vous.que.votre.maison.est.bien.isolée..)

# Colonne 39 : Travaux d'isolation depuis l'entrée dans le logement
copo <- copo %>%  rename (travaux_isol = X38..Vous.avez.réalisé.des.travaux.d.isolation.dans.votre.logement.depuis.que.vous_l.habitez..)

# Colonne 40 : Aide à la rénovation
copo <- copo %>%  rename (aide_renov = X39..Avez.vous.bénéficié.d.une.aide.à.la.rénovation.énergétique.de.votre.logement..)

# Colonne 41 : Modification d'un mode de chauffage
copo <- copo %>%  rename (modif_chauffage = X40..Avez.vous.modifié.au.moins.l.un.des.modes.de.chauffage.depuis.votre.entrée.dans.la.maison..)

# Colonne 42 : Date de la dernière modification du mode de chauffage
copo <- copo %>%  rename (date_modif = X41..De.quand.date.la.dernière.modification..)

# Colonne 43 : Modes de chauffage à l'entrée dans le logement
copo <- copo %>%  rename (chauff_entree = X42.._Quels.étaient.les.modes.de.chauffage.à.votre.entrée.dans.le.logement.._...Lecture.des.modalités.)

# Colonne 44 : Autre mode de chauffage à l'entrée dans le logement
copo <- copo %>%  rename (autre_chauff_entree = X43..autre.mode.de.chauffage..)

# Colonne 45 : Raisons du changement de mode de chauffage effectué
copo <- copo %>%  rename (raison_chgt_chauff = X44..Pour.quelles.raisons.avez.vous.changé.de.mode.de.chauffage....par.ordre.de.préférence.)

# Colonne 46 : Autre raison de changement de mode de chauffage
copo <- copo %>%  rename (autre_raison_chgt_chauff = X45..autre..)

# Colonne 47 : Souhait de changement de monde de chauffage (oui/non)
copo <- copo %>%  rename (souhait_chgt_chauff = X46..Souhaiteriez.vous.changer.un.des.modes.de.chauffage..)

# Colonne 48 : Mode de chauffage désiré si changement envisagé
copo <- copo %>%  rename (chauff_souhait = X47..Pour.quel.mode.de.chauffage.opteriez.vous..)

# Colonne 49 : Autre mode de chauffage désiré
copo <- copo %>%  rename (autre_chauff_souhait = X48..autre.mode.de.chauffage..)

# Colonne 50 : Raisons du souhait de changement de mode de chauffage
copo <- copo %>%  rename (raison_souhait_chauff = X49..Pour.quelles.raisons.souhaiteriez.vous.changer.de.mode.de.chauffage....par.ordre.de.préférence.)

# Colonne 51 : Autre raison du souhait de changement de mode de chauffage
copo <- copo %>%  rename (autre_raison_souhait = X50..autre..)

# Colonne 52: Fréquentation de la forêt au moins une fois par an (oui/non)
copo <- copo %>%  rename (frequentation_foret = X51..Est.ce.que.vous.vous.rendez.en.forêt.au.moins.une.fois.par.an..)

# Colonne 53 : Type de foret fréquentée
copo <- copo %>%  rename (regime_foret_freq = X52..Quel.type.de.forêt.fréquentez.vous..)

# Colonne 54 : Activités en foret
copo <- copo %>%  rename (activite_foret = X53..Est.ce.que.vous.réalisez.une.ou.plusieurs.activités.suivantes..quand.vous.vous.rendez.en.forêt..)

# Colonne 55 : Autre activités en forêt
copo <- copo %>%  rename (autre_activite_foret = X54..autre.activité..)

# Colonne 56 : Fréquence de fréquentation de la forêt
copo <- copo %>%  rename (freq_foret = X55..A.quelle.fréquence.vous.vous.rendez.en.forêt..)

# Colonne 57 : Compagnie en forêt
copo <- copo %>%  rename (compagnie_foret = X56..Avec.quelles.personnes.vous.vous.rendez.en.forêt...Pouvez.vous.hiérarchiser.en.fonction.de.la.fréquence.)

# Colonne 58 : Autre compagnie en forêt
copo <- copo %>%  rename (autre_compagnie_foret = X57..Autre..)

# Colonne 59 : Moment de fréquentation de la forêt
copo <- copo %>%  rename (moment_foret = X58..A.quel.moment.vous.rendez.vous.en.forêt..)

# Colonne 60 : Saison de fréquentation de la forêt
copo <- copo %>%  rename (saison_foret = X59..Quelle.est.la.saison.à.laquelle.vous.vous.rendez.le.plus.en.forêt..)

# Colonne 61 : Possession d'une parcelle de forêt
copo <- copo %>%  rename (possession_foret = X60..Est.ce.que.vous.ou.votre.conjoint.e.possédez.une.ou.plusieurs.parcelles.de.bois..)

# Colonne 62 : Superficie des parcelles de forêt possédées (en Ha)
copo <- copo %>%  rename (surface_foret = X61..Si.oui..quelle.est.sa.superficie..ou.les.superficies.cumulées.si.plusieurs_parcelles...)

# Colonne 63 : Habitation soumise aux obligation de débroussaillement
copo <- copo %>%  rename (obl_debroussail = X62..Est.ce.que.votre.habitation.est.soumise.aux.obligations.de.debroussaillement.liées.à.l.exposition.au.risque.de.feu.de.forêt..)

# Colonne 64 : Gène par d'autres usagers en forêt
copo <- copo %>%  rename (gene_foret = X63..Est.ce.qu.en.forêt..vous.avez.déjà.été.gênés.par.d.autres.usagers..)

# Colonne 65 : Activités des usagers qui gènent en forêt
copo <- copo %>%  rename (activite_gene_foret = X64..Quelles.étaient.les.activités.de.ces.usagers..)

# Colonne 66 : Autre activité gènante en forêt
copo <- copo %>%  rename (autre_activite_gene_foret = X65..autre..)

# colonne 67 : Mobilisation contre des projets qui menancent la foret
copo <- copo %>%  rename (mob_foret = X66..Est.ce.que.vous.avez.déjà.été.mobilisés.contre.un.projet.menaçant.une.parcelle.forestière..)

# Colonne 68 : Type de projet contre lequel vous vous êtes mobilisé
copo <- copo %>%  rename (projet_mob_foret = X67..Si.oui..contre.quel.type.d.activité.vous.êtes.vous.mobilisés..)

# Colonne 69 : Autre type de projet/mobilisation
copo <- copo %>%  rename (autre_projet_mob_foret =X68..Autre..)

# Colonne 70 : Nombre d'adultes dans le foyer
copo <- copo %>%  rename (adulte = X69..Adultes)

# Colonne 71 : Nombre d'enfants mineurs dans le foyer
copo <- copo %>%  rename (enfant = X70..Enfants..mineurs.)

# Colonne 72 : Habitat avec un.e conjoint.e (oui/non)
copo <- copo %>%  rename (conjoint = X71..Habitez.vous.avec.un.e.conjoint.e..)

# Colonne 73 : Superficie du logement
copo <- copo %>%  rename (superficie_logement = X72.._Quelle.est.la.superficie.de.votre.logement.._.Hors.jardin.et.autres.dépendances.)

# Colonne 74 : Terrain ou jardin attenant au logement (oui/non)
copo <- copo %>%  rename (jardin = X73..Est.ce.qu.il.y.a.un.jardin.ou.un.terrain.attenant.à.votre.logement..)

# Colonne 75 : Superficie jardin ou terrain
copo <- copo %>%  rename (superficie_jardin = X74..Si.oui..quelle.est.sa.superficie..)

# Colonne 76 : Nombre d'années de résidence dans le logement actuel
copo <- copo %>%  rename (ann_residence = X75..Depuis.quand.habitez.vous.dans.votre.foyer.actuel..)

# Colonne 77 : Lieu de la résidence précédent l'arrivée dans le logement
copo <- copo %>%  rename (lieu_anc_residence = X76..Avant.d.arriver.dans.ce.logement..vous.habitiez..)

# Colonne 78 : Commune de la résidence précédent l'arrivée dans le logement
copo <- copo %>%  rename (commune_anc_residence = X77..Précisez.la.commune)

# Colonne 79 : Enfance dans la commune ou commune proche (oui/non)
copo <- copo %>%  rename (enfance_commune = X78..Avant.vos.18.ans.avez.vous.vécu.dans.votre.commune.de.résidence.actuelle.ou.dans.une.commune.proche..environ.25.km..)

# Colonne 80 : Nombre d'année d'enfance dans la commune
copo <- copo %>%  rename (ann_enfance_commune = X79..Avant.18.ans..combien.d.années.avez.vous.vécu.dans.la.commune.de.résidence.actuelle.ou.dans.une.commune.proche..)

# Colonne 81 : Enfance en ville ou à la campagne
copo <- copo %>%  rename (enfance_villecamp = X80..Avez.vous.grandi..)

# Colonne 82 : Situation actuelle par rapport à l'emploi
copo <- copo %>%  rename (situation_emploi = X81..Quelle.est.actuellement.votre.situation.principale.vis.à.vis.de.l.emploi..)

# Colonne 83 : Autre situation par rapportà l'emploi
copo <- copo %>%  rename (autre_situation_emploi = X82..Autre..)

# Colonne 84 : Profession ou dernière profession (PCS niveau 1)
copo <- copo %>%  rename (pcs_niv1 = X83..Quelle.est.votre.profession.actuelle..ou.votre.dernière.profession..)

# Colonne 85 : Groupe 1, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp1_niv2 = X84..Si.vous.êtes.agriculteur.rice.exploitant.e..pouvez.vous.préciser.votre.profession..)

# Colonne 86 : Groupe 1, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp1_niv2_autre = X85..Agriculteur_Autre..)

# Colonne 87 : Groupe 2, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp2_niv2 = X86..Si.vous.êtes.artisan.e.commerçant.e..pouvez.vous.préciser.votre.profession..)

# Colonne 88 : Groupe 2, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp2_niv2_autre = X87..Artisan.Commerçant_Autre..)

# Colonne 89 : Groupe 3, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp3_niv2 = X88..Si.vous.êtes.cadre..pouvez.vous.préciser.votre.profession..)

# Colonne 90 : Groupe 3, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp3_niv2_autre = X89..Cadre_Autre..)

# Colonne 91 : Groupe 4, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp4_niv2 = X90..Si.vous.appartenez.à.une.profession.intermédiaire..pouvez.vous.préciser.votre.profession..)

# Colonne 92 : Groupe 4, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp4_niv2_autre = X91..Profession.Intermédiaire_Autre..)

# Colonne 93 : Groupe 5, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp5_niv2 = X92..Si.vous.êtes.employé.e..pouvez.vous.préciser.votre.profession..)

# Colonne 94 : Groupe 5, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp5_niv2_autre = X93..Employé_Autre..)

# Colonne 95 : Groupe 6, PCS niveau 2 (environ)
copo <- copo %>%  rename (pcs_gp6_niv2 = X94..Si.vous.êtes.ouvrier.ère..pouvez.vous.préciser.votre.profession..)

# Colonne 96 : Groupe 6, PCS niveau 2, autre
copo <- copo %>%  rename (pcs_gp6_niv2_autre = X95..Ouvrier_Autre..)

# Colonne 97 : Situation actuelle du conjoint.e par rapport à l'emploi
copo <- copo %>%  rename (situation_emploi_conj = X96..Quelle.est.actuellement.la.situation.principale.de_votre.conjoint.ou.de.votre.conjointe.vis.à.vis.de.l.emploi..)

# Colonne 98 : Autre situation du conjoint.e par rapport à l'emploi
copo <- copo %>%  rename (autre_situation_emploi_conj = X97..Autre..)

# Colonne 99 : Profession ou dernière profession du conjoint (PCS niveau 1)
copo <- copo %>%  rename (pcs_niv1_conj = X98..Quelle.est.la.profession.actuelle..ou.votre.dernière.profession.de.votre.conjoint.e.)

# Colonne 100 : Groupe 1, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp1_niv2_conj = X99..Si.votre.conjoint.e.est.agriculteur.rice.exploitant.e..pouvez.vous.préciser.sa.profession..)

# Colonne 101 : Groupe 1, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp1_niv2_autre_conj = X100..Agriculteur_Autre..)

# Colonne 102 : Groupe 2, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp2_niv2_conj = X101..Si.votre.conjoint.e.est.artisan.e.commerçant.e..pouvez.vous.préciser.sa.profession..)

# Colonne 103 : Groupe 2, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp2_niv2_autre_conj = X102..Artisan.Commerçant_Autre..)

# Colonne 104 : Groupe 3, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp3_niv2_conj = X103..Si.votre.conjoint.e.est.cadre..pouvez.vous.préciser.sa.profession..)

# Colonne 105 : Groupe 3, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp3_niv2_autre_conj = X104..Cadre_Autre..)

# Colonne 106 : Groupe 4, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp4_niv2_conj = X105..Si.votre.conjoint.e.est.appartenez.à.une.profession.intermédiaire..pouvez.vous.préciser.sa.profession..)

# Colonne 107 : Groupe 4, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp4_niv2_autre_conj = X106..Profession.Intermédiaire_Autre..)

# Colonne 108 : Groupe 5, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp5_niv2_conj = X107..Si.votre.conjoint.e.est.employé.e..pouvez.vous.préciser.sa.profession..)

# Colonne 109 : Groupe 5, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp5_niv2_autre_conj = X108..Employé_Autre..)

# Colonne 110 : Groupe 6, PCS niveau 2 (environ) pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp6_niv2_conj = X109..Si.votre.conjoint.e.est.ouvrier.ère..pouvez.vous.préciser.sa.profession..)

# Colonne 111 : Groupe 6, PCS niveau 2, autre pour le conjoint ou la conjointe
copo <- copo %>%  rename (pcs_gp6_niv2_autre_conj = X110..Ouvrier_Autre..)

# Colonne 112 : Revenus mensuels
copo <- copo %>%  rename (revenus = X111..Quels.sont..en.moyenne..vos.revenus.mensuels..net...)

# Colonne 113 : Revenus mensuels du conjoint.e
copo <- copo %>%  rename (revenus_conj = X112..Quels.sont..en.moyenne..les.revenus.mensuels..net..de.votre.conjoint.ou.de.votre.conjointe..)

# Colonne 114 : Contact
copo <- copo %>%  rename (contact = X113..Accepteriez.vous.d.être.recontacté.e.pour.un.entretien.dans.le.cadre.de.cette.étude...Si.oui..merci.d.indiquer.une.adresse.e.mail.ou.un.numéro.de.téléphone.)

# Colonne 115 : Commentaire
copo <- copo %>%  rename (commentaire = X114..Commentaire.enquêteur)

# Colonne 116 : Clé

# Colonne 117 : Date de saisie de la donnée
copo <- copo %>%  rename (date_questionnaire = X116..Date.de.saisie)

# Colonne 118 : Date de dernière modification de la ligne
copo <- copo %>%  rename (date_modif_ligne = X117..Date.de.dernière.modification)

# Colonne 119 : Date du dernier enregistrement de la ligne
copo <- copo %>%  rename (date_enreg_ligne = X118..Date.de.dernier.enregistrement)

# Colonne 120 : temps de saisi du questionnaire
copo <- copo %>%  rename (tps_reponse = X119..Temps.de.saisie)

names(copo)


# verification de quotas : échantillonnage -------

names(copo)

# Creer variable indicatrice du terrain -----

copo <- copo %>% mutate(terrain = case_when(id <= 98 ~ "Landes", T ~ "Meuse"))
freq(copo$terrain)

# Nettoyer les individus qui n'ont pas complété le questionnaire -----

freq(copo$prenom_enq)
copo <- copo %>% filter(! (id %in% c(119:121)))

# Verifier fiabilité de l'echantillon -----
# GENRE

freq(copo$genre[copo$terrain == "Landes"])
freq(copo$genre[copo$terrain == "Meuse"])

# AGE
copo <- copo %>% mutate(age = 2026-ann_naissance)
copo <- copo %>% mutate(cl_age = case_when(age <=24 ~ "18-24",
                                           age <=39 ~ "25-39",
                                           age <=54 ~ "40-54",
                                           age <=64 ~ "55-64",
                                           age <=79 ~ "65-79",
                                           T ~ "80 ans ou plus"))


freq(copo$cl_age[copo$terrain == "Landes"])

freq(copo$cl_age[copo$terrain == "Meuse"])

#export Louis
write.csv2(copo, "2.données/prod/copo_michemin.csv")

# Recoder les pcs -----

# Les VIDES ne sont pas de NA's !!!!
# AHAAAAAA

copo[85:96][copo[85:96] == ""] <- NA


str(copo)
copo <- copo %>%
  mutate(pcs_niv2 = coalesce(pcs_gp1_niv2, pcs_gp2_niv2, pcs_gp3_niv2, pcs_gp4_niv2, pcs_gp5_niv2, pcs_gp6_niv2))

freq(copo$pcs_niv2)


# variable niveau2 autre 
copo <- copo %>% 
  mutate(pcs_niv2_autre = coalesce(pcs_gp1_niv2_autre, pcs_gp2_niv2_autre, pcs_gp3_niv2_autre,
                             pcs_gp4_niv2_autre, pcs_gp5_niv2_autre, pcs_gp6_niv2_autre))

freq(copo$pcs_niv2_autre)

# Création sous-base pour analyser les PCS et moidifications -----
# J'ai repris celles faites par Lison d'abord puis en ordre
#Il faut refaire la même procédure pou les conjoint.e.s

sous_copo <- copo %>%  select(id,situation_emploi, pcs_niv1, pcs_niv2, pcs_niv2_autre, commentaire)

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 13 ~ "22_Commerçants et assimilés (commerçant, restaurateur, buraliste, gérant d’hôtel ou d’autres hébergement…)", 
                      T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 14 ~ "63/68B ouvrier qualifié et peu qualifié de l’artisanat (BTP, restauration, alimentation, arts du spectacle", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 14 ~ "6. Ouvriers", 
                               T ~ pcs_niv1 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 18 ~ "43 Professions intermédiaires de la santé et du travail social (infirmier, sage-femme, cadre de santé, kiné, préparateur en pharmacie, éducateur spécialisé, éducateur, animateur en centre de loisir…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 18 ~ "4. Professions intermédiaires", 
                               T ~ pcs_niv1_rec ))

# Profession 22D6 pour 'Indépendantes d'autres prestations de service'
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 69 ~ "22_Commerçants et assimilés (commerçant, restaurateur, buraliste, gérant d’hôtel ou d’autres hébergement…)", 
                               T ~ pcs_niv2 ))


# Profession 52A1 : Agents / Agentes d'accueil et de guichet des administrations publiques
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 70 ~ "52 Employé administratif de la fonction publique (agent d’accueil, adjoint administratif, facteur, agent de nettoyage, de surveillance, de restauration, brancardier, veilleur de nuit…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 70 ~ "5. Employés", 
                               T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 77 ~ "56 Personnels des services directs aux particuliers (serveur, commis de cuisine, veilleur de nuit, coiffeur, esthéticien, aide à domicile, agent d’entretien, concierge…)", 
                               T ~ pcs_niv2 ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 2 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 3 ~ "47A Techniciens de l'agriculture, de l'aquaculture, des forêts et de la protection de l'environnement (technicien forestier, Animateur nature, Chargé d'études en environnement, Chargé de mission énergie, Technicien des espaces naturels", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 3 ~ "4. Professions intermédiaires", 
                               T ~ pcs_niv1_rec ))

#Profession 21E4 : Esthéticiens / Esthéticiennes et autres artisans / artisanes des soins corporels
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 5 ~ "21 Artisans (BTP, garagiste, gérant de contrôle technique, transporteur, coiffeur, boulanger, boucher, charcutier, paysagiste, jardinier, menuisier, couturier, cordonnier, réparateur informatique ou électroménager, artisanat d’art…)", 
                               T ~ pcs_niv2 ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 8 ~ "37 Cadre du secteur privé (directeur administratif ou financier, DRH, cadre administratif ou commercial, juriste, chef de service, chargé de communication, directeur d’agence bancaire, directeur d’agence immobilière…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 9 ~ "38B : Ingénieurs et cadres techniques de l'agriculture, de l'aquaculture, des forêts et de la protection de l'environnement", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 23 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 23 ~ "6. Ouvriers", 
                                   T ~ pcs_niv1_rec ))


#Profession 46D1 : Chargés / Chargées de clientèle de la banque et des assurances
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 38 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 38 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


# Rubrique 485a : Agents de maîtrise et techniciens en production et distribution d'énergie, eau, chauffage
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 48 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 48 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


# Profession 354b : Artistes professionnels de la musique et du chant
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 54 ~ "35 Professions de l'information, de l'art et des spectacles (bibliothécaire, conservateur, journaliste, designer, artiste, professeur d’art, directeur artistique, régisseur…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 54 ~ "3. Cadres et professions intellectuelles supérieures", 
                                   T ~ pcs_niv1_rec ))


# Profession 22B1 : Commerçants / Commerçantes de biens pour le foyer
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 65 ~ "22 Commerçants et assimilés (commerçant, restaurateur, buraliste, gérant d’hôtel ou d’autres hébergement…)", 
                               T ~ pcs_niv2 ))



# Profession 42B2 : Surveillants / Surveillantes scolaires et assistants /assistantes d'éducation
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 80 ~ "42A : Enseignants du primaire et du secondaire", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 80 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 86 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ))


# Profession 62D3 : Ouvriers qualifiés / Ouvrières qualifiées de la composition, de l'impression et du façonnage du papier-carton
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 88 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 88 ~ "6. Ouvriers" ,
                                   T ~ pcs_niv1_rec ))


# Profession 42C2 : Formateurs / Formatrices, bibliothécaires et documentalistes (non cadres)
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 89 ~ "42C : Autre professions de la formation, de la documentation et du sport (secteur privé)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 89 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 90 ~ "56 Personnels des services directs aux particuliers (serveur, commis de cuisine, veilleur de nuit, coiffeur, esthéticien, aide à domicile, agent d’entretien, concierge…)", 
                               T ~ pcs_niv2 ))

# Profession 122b : Exploitants forestiers indépendants, de 0 à 9 salariés
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 93 ~ "10C Entrepreneurs / Entrepreneuses de la pêche et de l'aquaculture (pêcheur, ostréiculteur…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 93 ~ "1. Agriculteurs exploitants", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 96 ~ "47 Autre Technicien", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 96 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

# Profession 52B3 : Agents / Agentes de service des établissements de santé
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 97 ~ "52 Employé administratif de la fonction publique (agent d’accueil, adjoint administratif, facteur, agent de nettoyage, de surveillance, de restauration, brancardier, veilleur de nuit…)", 
                               T ~ pcs_niv2 ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 98 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 98 ~ "6. Ouvriers" ,
                                   T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 100 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 100 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 102 ~ "63/68B ouvrier qualifié et peu qualifié de l’artisanat (BTP, restauration, alimentation, arts du spectacle", 
                               T ~ pcs_niv2 ))

# extrait de la base mi parcours Leny 
# write.csv2(copo, "2.données/prod/copo_complet_recoding.csv")


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 126 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ))


# Profession 42B2 : Surveillants / Surveillantes scolaires et assistants /assistantes d'éducation
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 132 ~ "42A : Enseignants du primaire et du secondaire", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 132 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

# Profession 354b : Artistes professionnels de la musique et du chant
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 144 ~ "34 Professeurs et professions scientifiques supérieures (Enseignant du secondaire, chef d’établissement, inspecteur de l’éducation nationale, médecin hospitalier, interne, chercheur, enseignant-chercheur, ingénieur de recherche, doctorant…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 144 ~ "3. Cadres et professions intellectuelles supérieures", 
                                   T ~ pcs_niv1_rec ))




# Pour les personnes en situation d'invalidité, parfois on a noté en commentaire l'ancienne profession.
# Toutefois, pour l'instant, on n'a pas renoté leur profession dans les PCS et ça apparait toujours en NA
# A regarder, si jamais on veut aller revoir ces cas précis. A priori, il n'y en a pas bcp (un bucheon, un maçon)

# Aussi, on a eu plusieurs profession qui s'apparente au 42B (AVC, animateur périscolaire).
# On les a codé en 42A qui est de fait un 42A/42B

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 181 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 181 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 184 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 184 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 185 ~ "56 Personnels des services directs aux particuliers (serveur, commis de cuisine, veilleur de nuit, coiffeur, esthéticien, aide à domicile, agent d’entretien, concierge…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 187 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 187 ~ "6. Ouvriers", 
                                   T ~ pcs_niv1_rec ))

# Pour le 193, il a donné 2 profession en commentaire : exploitant de grande culture et cadre à Paris.
# En regardant sur internet à partir de son contact, il a effectivement ces 2 activités. On l'a laissé ds les agriculteurs dans les PCS

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 194 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 197 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 197 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 202 ~ "37 Cadre du secteur privé (directeur administratif ou financier, DRH, cadre administratif ou commercial, juriste, chef de service, chargé de communication, directeur d’agence bancaire, directeur d’agence immobilière…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 204 ~ "54 Employés administratifs (hôte d’accueil, secrétaire, agent de comptabilité, employé de banque ou d’assurance, contrôleur de train, guide touristique, assistant commercial, gestionnaire", 
                               T ~ pcs_niv2 ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 210 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 210 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 212 ~ "48A : Agents de maîtrise de l'agriculture, des travaux forestiers et de la pêche (Chef jardinier, Gérant d'exploitation agricole, Régisseur d'exploitation agricole, Agent de maîtrise d'exploitation forestière…)", 
                               T ~ pcs_niv2 ))

# Profession 46B5 : Autres professions intermédiaires des services commerciaux
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 220 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 220 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

# Profession 353c : Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 221 ~ "35 Professions de l'information, de l'art et des spectacles (bibliothécaire, conservateur, journaliste, designer, artiste, professeur d’art, directeur artistique, régisseur…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 221 ~ "3. Cadres et professions intellectuelles supérieures", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 222 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ))


# Profession 353c : Cadres artistiques et technico-artistiques de la réalisation de l'audiovisuel et des spectacles
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 230 ~ "35 Professions de l'information, de l'art et des spectacles (bibliothécaire, conservateur, journaliste, designer, artiste, professeur d’art, directeur artistique, régisseur…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 230 ~ "3. Cadres et professions intellectuelles supérieures", 
                                   T ~ pcs_niv1_rec ))



sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 248 ~ "37 Cadre du secteur privé (directeur administratif ou financier, DRH, cadre administratif ou commercial, juriste, chef de service, chargé de communication, directeur d’agence bancaire, directeur d’agence immobilière…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 249 ~ "31B Profession libérale juridique et technique (avocat, notaire, expert-comptable, autre expert, architecte, consultant…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 249 ~ "3. Cadres et professions intellectuelles supérieures", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 253 ~ "63/68B ouvrier qualifié et peu qualifié de l’artisanat (BTP, restauration, alimentation, arts du spectacle", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 254 ~ "56 Personnels des services directs aux particuliers (serveur, commis de cuisine, veilleur de nuit, coiffeur, esthéticien, aide à domicile, agent d’entretien, concierge…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 254 ~ "5. Employés", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 255 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 259 ~ "56 Personnels des services directs aux particuliers (serveur, commis de cuisine, veilleur de nuit, coiffeur, esthéticien, aide à domicile, agent d’entretien, concierge…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 261 ~ "52 Employé administratif de la fonction publique (agent d’accueil, adjoint administratif, facteur, agent de nettoyage, de surveillance, de restauration, brancardier, veilleur de nuit…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 262 ~ "45 Professions intermédiaires de la fonction publique (agent administratif, policier municipal, inspecteur de police, chef de service pénitencier, secrétaire de mairie, sous-officier de l’armée…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 262 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 264 ~ "21 Artisans (BTP, garagiste, gérant de contrôle technique, transporteur, coiffeur, boulanger, boucher, charcutier, paysagiste, jardinier, menuisier, couturier, cordonnier, réparateur informatique ou électroménager, artisanat d’art…)", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 267 ~ "54 Employés administratifs (hôte d’accueil, secrétaire, agent de comptabilité, employé de banque ou d’assurance, contrôleur de train, guide touristique, assistant commercial, gestionnaire", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 270 ~ "62/67A Ouvriers  qualifiés et peu qualifié de l’industrie (extraction, métal, électronique, énergie…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 270 ~ "6. Ouvriers", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 271 ~ "48 Autre agent de maîtrise", 
                               T ~ pcs_niv2 ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 272 ~ "42A : Enseignants du primaire et du secondaire", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 272 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 273 ~ "53 Policiers , militaires, pompiers, agents de sécurité privée (gardien de la paix, policier, gardien de prison, sous-officier, gendarme, militaire du rang, garde-pêche, garde forestier, gardien, maitre-chien…)", 
                               T ~ pcs_niv2 ))

# 273. La conjointe aussi est agente de sécurité. Elle a dû être mal codé comme lui.

sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 277 ~ "54 Employés administratifs (hôte d’accueil, secrétaire, agent de comptabilité, employé de banque ou d’assurance, contrôleur de train, guide touristique, assistant commercial, gestionnaire", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 277 ~ "5. Employés", 
                                   T ~ pcs_niv1_rec ))



# Profession 10B1 : Entrepreneurs / Entrepreneuses de travaux agricoles, ruraux et forestiers
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 278 ~ "10B Entrepreneurs / Entrepreneuses de travaux agricoles, ruraux et forestiers (bucheron, exploitant forestier, élagueur, débardeur, sylviculteur…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 278 ~ "1. Agriculteurs exploitants", 
                                   T ~ pcs_niv1_rec ))


# Profession 43C3 : Préparateurs / Préparatrices en pharmacie
sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 282 ~ "46 Professions intermédiaires en entreprise (assistant de direction, comptable, chargé de mission, chef d’équipe…)", 
                               T ~ pcs_niv2 ),
         pcs_niv1_rec =  case_when(id == 282 ~ "4. Professions intermédiaires", 
                                   T ~ pcs_niv1_rec ))


sous_copo <- sous_copo %>%  
  mutate(pcs_niv2 =  case_when(id == 284 ~ "52 Employé administratif de la fonction publique (agent d’accueil, adjoint administratif, facteur, agent de nettoyage, de surveillance, de restauration, brancardier, veilleur de nuit…)", 
                               T ~ pcs_niv2 ))


# Merger sous-copo à copo -------------------

sous_copo$pcs_niv2_rec <-  sous_copo$pcs_niv2
names(sous_copo)

sous_copo <- sous_copo %>% select(id, pcs_niv1_rec, pcs_niv2_rec) # garder les nouvelles vars recodées

copo <-  left_join(copo, sous_copo,  by = "id")

freq(copo$pcs_niv1)
freq(copo$pcs_niv1_rec)

copo$pcs_niv1_rec[copo$pcs_niv1_rec == ""] <- NA
copo$pcs_niv1_rec[copo$pcs_niv1_rec == "je ne souhaite pas répondre à cette question"] <- NA
copo$pcs_niv1_rec[copo$pcs_niv1_rec == "Sans objet"] <- NA

names(copo)


freq(copo$situation_emploi)

copo <- copo %>%  mutate(pcs_insee_retr = 
                           case_when(situation_emploi == "Retraite(e)" ~ "Retraite(e)",
                                     T ~ pcs_niv1_rec))

freq(copo$pcs_insee_retr[copo$terrain == "Landes"])
freq(copo$pcs_insee_retr[copo$terrain == "Meuse"])


# Ecrire note sur le recodage réalisée jusqu'à présent nottamaent sur PCS
# Plus realiser le rapport avec les echantillonages en les comparant avec les données INSEE

#sauvegarder la base en format leger 
# saveRDS(copo, file = "2.données/prod/copo.rds")

copo<- readRDS("copo.rds")

## Densité de population -------------
# 03 novembre
library(readxl)
insee_densite <- read_excel("densite_commune.xlsx", skip = 1 ) %>% 
  select(1:12)

dico_insee_densite <- read_excel("densite_commune.xlsx", col_names = FALSE) %>% 
  select(1:12) %>% 
  slice(1:2) %>% t(.) %>% 
  as.data.frame() %>%
  setNames(c("label", "variable")) 

rownames(dico_insee_densite)<-(1:12)
names(insee_densite)

# garder les codgeo qui peuvent nous interesser
# san ce filtre, j'aurai des doubln parmi certaines communes comme VERT, Blécourt, Saint Justin, entre autres.

insee_densite <- insee_densite %>%
  filter((CODGEO>= 40000 & CODGEO <=40999)|
                       (CODGEO >= 52000 & CODGEO <= 52999)|
                       (CODGEO >= 55000 & CODGEO <=55999))

# Comptage du nombre d'enquêté par commune, seon la densité

freq(copo$commune)
names(teste)
teste <- left_join(copo, insee_densite,
                   by = c("commune"= "LIBGEO")) %>% 
  select(-c( "PMUN21", "P1", "P2","P3","P4","P5","P6","P7"))

# Par terrain

freq(teste$LIBDENS)
freq(teste$LIBDENS[teste$terrain== "Meuse"])
freq(teste$LIBDENS[teste$terrain== "Landes"])
# Il me faut comprendre par terrain la % de chaque densité 
# pour visualiser sur quelle population nous réalisons notrea analyse

# https://www.insee.fr/fr/information/8571524
# La grille de densité est founie en 3 niveaux quand il s'agit des EPCI
# en 7 niveaux par communes
# La repartition de l'indice de  densité en 3 modalités 
# regroupe les niveaux 5, 6 et 7 qui correspondent aux options affichées par nos communes
# C-a-d selon une repartition de l'indice en 3 catégorie nos communes sont 100% "Rural"
# Cherche une grille en 7 niveaux par EPCI afin de cibler le % pour chaque niveau de ruralité



# Après la densité, on cherche à 
# par terrain
# Qui brule


names(copo)
freq(copo$stere)


copo <- copo %>% mutate(u_bois = 
                          case_when(!is.na(stere)~ "Utilisation de bois",
                                    T ~ "Pas d'utilisation de bois"))

freq(copo$u_bois) # Les 129
lprop(table(copo$terrain,copo$u_bois))

copo %>% 
  ggplot(aes(terrain, fill= u_bois))+
  geom_bar(position = "dodge")


# u_bois_bin
copo <- copo %>% mutate(u_bois_bin = 
                          case_when(u_bois == "Utilisation de bois" ~ 1,
                                    T~0))



# un modele logistique pour expliquer l'utilisation du bois
# ? Seulement le terrain ? 

m1 <- glm(u_bois_bin ~ terrain+genre, family = binomial, data = copo)

summary(m1)
odds.ratio(m1)



# Comprendre le mode chauffage

# Eclater la variable

freq(copo$mode_chauffage)

copo <- dummy_cols(copo,
                   select_columns = "mode_chauffage",
                   split = ";",                   
                   remove_selected_columns = FALSE,
                   ignore_na = TRUE)  


names(copo) # 20 nouvelles variables


# On va remplacer les 1 par oui pour chacune de ces nouvelles variables

copo <- copo %>%
  mutate(across(starts_with("mode_chauffage_") & where(is.numeric),
                ~ if_else(is.na(.), NA_character_,
                          if_else(. == 1, "Oui", "Non"))))


freq(copo$`mode_chauffage_insert`)

# Eventuellement recoder les noms de nouvelles variables

copo_acm <- copo %>% select(pcs_niv1, genre, travaux_isol, bonne_isolation,chauff_souhait,u_bois, c(137:157))

names(copo_acm)
res <- MCA(copo_acm, quali.sup =c(1:5), ncp = 10)

explor(res)









# Distribution du bois brûlé

copo %>% filter(stere != -1 & u_bois == "Utilisation de bois") %>% 
  select(stere, terrain) %>% 
  summary()

copo %>% filter(stere != -1 & u_bois == "Utilisation de bois") %>% 
  ggplot(aes(stere))+
  geom_histogram()+
  theme_bw()




# Distribution moyenne du bois brûlé par terrain

tapply(copo$stere,copo$terrain,  mean, na.rm=TRUE)


# Distribution Dautres indicateurs du bois brûlé par terrain
copo %>%
  filter(stere != -1 & u_bois == "Utilisation de bois") %>%
  group_by(terrain) %>%
  summarise(
      min     = min(stere),
      q1      = quantile(stere, 0.25),
      mediane = median(stere),
      moyenne = mean(stere),
      q3      = quantile(stere, 0.75),
      max     = max(stere))
 

# représentaiton graphique 
boxplot(stere~terrain, copo)

# boxplot amélioré

copo %>% filter(stere != -1 & u_bois == "Utilisation de bois") %>% 
  ggplot(aes(terrain, stere, fill = terrain))+
  geom_boxplot()+
  theme_bw()
  

# Bois brulé par PCS

copo %>% filter(stere != -1 & u_bois == "Utilisation de bois") %>% 
  ggplot(aes(pcs_niv1, stere, fill =pcs_niv1))+
  geom_boxplot()+
  theme_bw()


# moyenne de bois brulé selon pcs

tapply(copo$stere[copo$stere!= -1],copo$pcs_niv1[copo$stere!= -1],  mean, na.rm=TRUE)




# Parmi ceux qui brulent :
# comprendre le nb de stères brulés 
# Qui brûle le plus selon PCS 
# Ressentir isolation

