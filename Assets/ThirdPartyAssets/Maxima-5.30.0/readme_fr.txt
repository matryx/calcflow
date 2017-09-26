Ceci est la version Windows du fichier README.

Fichiers binaires inclus dans cette distribution.
-------------------------------------------------

La version pour Windows de ce paquage de Maxima inclut
des fichiers binaires d'autres projets Open Source également
disponible sur Sourceforge.

gcc: 

gcc.exe, cc1.exe et les fichiers dans les sous répertoires
lib/gcc-lib et include/ sont issue de la version mingw de gcc. 
Lequel est disponible à l'adresse http://prdownloads.sf.net/mingw/
     

binutils:

as.exe viens du portage mingw (http://www.mingw.org/) de binutils
disponible à l'adresse http://prdownloads.sf.net/mingw/


gnuplot:

Les fichiers wgnuplot.exe, wgnuplot.hlp et wgnuplot.mnu sont issus
de la distribution Windows de gnuplot disponible sur
http://gnuplot.sourceforge.net


wxMaxima:

Les fichiers dans le sou répertoire wxMaxima sont la distribution 
Windows de wxMaxima disponible sur http://wxmaxima.sourceforge.net


Interface graphique utilisateur (GUI) Maxima et pare-feux
---------------------------------------------------------

Parfois l'interface graphique utilisateur de Maxima (xmaxima ou wxMaxima) 
ne peut pas lancer Maxima ou produit un message de dépassement de temps
ou ne reçois pas de réponse pour les commandes Maxima.
Le problème est fort probablement provoqué par un pare-feux ou un
logiciel anti-virus.
La GUI de Maxima communique avec le moteur de calcul à travers une socket.
Le programme d'anti-virus et/ou le pare-feux voie cela et peut essayer de bloquer 
cette communication. (Étant donnée que certain programmes malicieux ouvrent
des sockets)

Pour résoudre le problème:

1.  Essayez de trouver le panneau de contrôle de l'anti-virus et/ou du pare-feux.

2.  Sélectionnez la GUI Maxima dans la liste des programmes bloqués
     et désactivez le blocage pour ce programme. Le programme de la
     GUI pourrait apparaître comme "Tcl/Tk" (c'est le nom de la boite à outils de
     la GUI pour xmaxima)


Prévention de l'Exécution des Données (PED) 
------------------------------------------- 

Il arrive que non seulement l'interface graphique de Maxima mais même Maxima 
en ligne de commandes ne fonctionne pas (maxima.bat se lance et quitte 
aussitôt).  Le problème vient très probablement de la PED de Windows. Quelques 
implémentations de LISP exécutent du code dans des secteurs de données de la 
mémoire. La PED de Windows le voit et le bloque (car certains programmes 
malveillants exécutent aussi du code dans des secteurs de données). 

Solution : 

Inclure le chemin complet du programme exécutable Maxima, par exemple : 
C:\Program Files\Maxima-5.12.0\lib\maxima\5.12.0\binary-gcl\maxima.exe 
dans la liste des exceptions PED 
(Panneau de configuration - > Système - > Avancé - > Performances - > PED) 


Espace d'environnement insuffisant
----------------------------------

Lorsque vous essayez d'exécuter Maxima sur une machine Windows 9x,
le message d'erreur suivant risque de s'afficher  :

    Espace d'environnement insuffisant (Out of environment space)

Le conseil suivant de la Base de Connaissance Microsoft, Article 230205
(http://support.microsoft.com/support/kb/articles/Q230/2/05.ASP) 
peut être une aide utile.

CAUSE

Ce problème peut se produire si vous ne disposez pas, dans l'environnement 
MS-DOS, de mémoire suffisante pour définir une variable d'environnement. 

RESOLUTION

Pour résoudre ce problème, augmentez l'espace par défaut disponible pour les
programmes dans l'environnement MS-DOS. Pour cela, appliquez l'une des 
méthodes suivantes.

Modifiez l'environnement pour tous les programmes MS-DOS

Pour augmenter l'espace d'environnement par défaut pour tous les programmes 
MS-DOS en cours d'exécution dans Windows, modifiez la commande Shell dans 
le fichier Config.sys. Pour cela suivez cette procédure : 

1.  Cliquez sur Démarrer et puis cliquez sur Exécuter. 

2.  Dans la zone Ouvrir, tapez sysedit et puis cliquez sur OK. 

3.  Cliquez sur la fenêtre Config.sys. 

4.  Au début de la ligne Shell= line, entrez REM, et pressez la bare d'espace 

      si la ligne Shell= line n'existe pas passez à l'étape suivante. 

5.  Appuyez sur la touche  HOME. 

6.  Tapez la ligne suivante pour créer une nouvelle commande Shell= line, et
    presser ENTER: 

      SHELL=C:\COMMAND.COM /E:4096 /P 

7.  Dans le menu Fichier, cliquez sur Enregistrer. 

8.  Dans le menu Fichier, cliquez sur Quitter. 

9.  Redémarrer l'ordinateur. 


Modifiez l'environnement pour un programme MS-DOS spécifique

Pour augmenter l'espace d'environnement par défaut pour un programme MS-DOS
spécifique, procédez comme suit : 

1.  Cliquez avec le bouton droit sur l'icône du programme maxima.bat et puis cliquez sur Propriétés. 

2.  Cliquez sur l'onglet Mémoire. 

3.  Dans la zone Environnement initial, tapez le nombre de kilo-octets (Ko) que le 
      programme requiert et puis cliquez sur OK. 

REMARQUE : 4096 KB représente le montant maximal de mémoire que vous pouvez 
allouer pour un programme MS-DOS.

