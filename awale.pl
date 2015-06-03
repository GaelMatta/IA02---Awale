%Prédicats de base :
%Concaténation :
concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

%Passage de deux sous-listes en une seule liste :
transformation([H,B],E) :- concat(H,B,E).

%Inversion des sous-listes :
inverseSousListe([H,B],[B,H]).

%Inversion de la liste :
inverse([],[]).
inverse([T|Q], LI) :- inverse(Q,L2),
                        concat(L2, [T], LI).
%Indice :
indice([T|_],1,T).
indice([_|Q],X,Y) :-
            I is X - 1,
            indice(Q,I,Y).
           
%-------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Prédicats principaux du jeu :
%Remplace la valeur d'un emplacement par une autre valeur :
remplacer(1,[_|Q],X,[X|Q]) :- !.
remplacer(N,[T|Q],X,[T|QR]) :- M is N-1,
                    remplacer(M,Q,X,QR).

%Prend les graines dans le champ adverse et les ajoutent au score :
%Si la valeur de la case est entre 2 et 3, prend les graines :                    
priseDeGraine(CA, Pdep, Score, NewCA, ValPfin, NewScore) :- 
                                ValPfin>=2,
                                ValPfin=<3,
                                NScore is Score + ValPfin,
                                remplacer(Pdep, CA, 0, NCA),
                                PdepBis is Pdep - 1,
                                indice(CA, PdepBis, Val),
                                priseDeGraine(NCA, PdepBis, NScore, NewCA, Val, NewScore).        
								
priseDeGraine(CA, 1, Score, NCA, ValPfin, NScore) :- 
                                ValPfin>=2,
                                ValPfin=<3,
                                NScore is Score + ValPfin, 
                                remplacer(1, CA, 0, NCA),!.

%Arrêt du prédicat si la valeur de la case est supérieure à 3 :								
priseDeGraine(CA, _, Score, CA, ValPfin, NewScore) :-     
                                ValPfin>3, 
								NewScore = Score, !.

%Arrêt du prédicat si la valeur de la case est inférieure à 2 :	                                
priseDeGraine(CA, _, Score, CA, ValPfin, NewScore) :-         
                                ValPfin<2,
								NewScore = Score,!.

                            
%Répartit les graines de la case sélectionnée du côté joueur :
%Condition d'arrêt lorsqu'il n'y a plus de graine à distribuer :        
repartirChampJoueur([H, B], Pdep, _, Pdep, 0, 0, [H,B],ScoreJ, ScoreJ) :- !.
%Changement de Champ lorsqu'il reste encore des graines à distribuer après avoir tout distribué sur sa partie du plateau :
repartirChampJoueur([H, B], 7, PSaut, Pfin, NbGraineDep, NbGraineFin, [NewPlateau,QR],ScoreJ, NewscoreJ) :- 
                                repartirChampAdverse([H, B], 1, PSaut, Pfin, NbGraineDep, NbGraineFin, [NewPlateau,QR],ScoreJ, NewscoreJ).
%Permet de ne pas mettre de graines dans la case de départ :                                
repartirChampJoueur([H, B], PSaut, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ) :- 
                                Position is PSaut + 1,
                                repartirChampJoueur([H, B], Position, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ).
%Repartit les graines sur le champ du joueur :
repartirChampJoueur([H, B], Pdep, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ) :- 
                                indice(B, Pdep, Val),
                                Val1 is Val + 1,
                                remplacer(Pdep, B, Val1, NewPlateau),
                                Position is Pdep + 1,
                                NewNbGraine is NbGraineDep - 1,
                                repartirChampJoueur([H, NewPlateau], Position, PSaut, Pfin, NewNbGraine, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ).
                                
%Répartit les graines de la case sélectionnée du côté adverse :
%Condition d'arrêt lorsqu'il n'y a plus de graine à distribuer :                                  
repartirChampAdverse([H, B], Pdep, _, Pdep, 0, 0, [NewH,B],ScoreJ, NewScoreJ ) :- 
								PfinBis is Pdep - 1,
								indice(H, PfinBis, ValPfin),
								priseDeGraine(H, PfinBis, ScoreJ, NewH, ValPfin, NewScoreJ),!.
%Changement de Champ lorsqu'il reste encore des graines à distribuer après avoir tout distribué sur sa partie du plateau :
repartirChampAdverse([H, B], 7, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,B1],ScoreJ, NewscoreJ) :- !,
                                repartirChampJoueur([H, B], 1, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,B1],ScoreJ, NewscoreJ).
%Repartit les graines sur le champ de l'adversaire :
repartirChampAdverse([H, B], Pdep, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewScoreJ) :- 
                                indice(H, Pdep, Val),
                                Val1 is Val + 1,    
                                remplacer(Pdep, H, Val1, NewPlateau),
                                NewNbGraine is NbGraineDep - 1,
                                Position is Pdep + 1,
                                repartirChampAdverse([NewPlateau, B], Position, PSaut, Pfin, NewNbGraine, NbGraineFin, [H1,QR],ScoreJ, NewScoreJ).
								
%Mêmes prédicats que précédemment dans le cas où le champ est vide après avoir joué :								
repartirChampJoueurBis([H, B], Pdep, _, Pdep, 0, 0, [H,B],ScoreJ, ScoreJ) :- !.
repartirChampJoueurBis([H, B], 7, PSaut, Pfin, NbGraineDep, NbGraineFin, [NewPlateau,QR],ScoreJ, NewscoreJ) :-  
                                repartirChampAdverseBis([H, B], 1, PSaut, Pfin, NbGraineDep, NbGraineFin, [NewPlateau,QR],ScoreJ, NewscoreJ).
                                
repartirChampJoueurBis([H, B], PSaut, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ) :- 
                                Position is PSaut + 1,
                                repartirChampJoueurBis([H, B], Position, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewscoreJ).

                                
                                
repartirChampAdverseBis([H, B], Pdep, _, Pdep, 0, 0, [H,B],ScoreJ, ScoreJ ) :- !.
repartirChampAdverseBis([H, B], 7, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,B1],ScoreJ, _) :- !,
                                repartirChampJoueurBis([H, B], 1, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,B1],ScoreJ, _).
repartirChampAdverseBis([H, B], Pdep, PSaut, Pfin, NbGraineDep, NbGraineFin, [H1,QR],ScoreJ, NewScoreJ) :- 
                                indice(H, Pdep, Val),
                                Val1 is Val + 1,    
                                remplacer(Pdep, H, Val1, NewPlateau),
                                NewNbGraine is NbGraineDep - 1,
                                Position is Pdep + 1,
                                repartirChampAdverseBis([NewPlateau, B], Position, PSaut, Pfin, NewNbGraine, NbGraineFin, [H1,QR],ScoreJ, NewScoreJ).
        
%Prédicat permettant de ramasser les graines :
%Condition d'arrêt :      					
ramasserTout(CJ, 1, Score, NewScore) :-
								indice(CJ, 1, Val),
								NewScore is Score + Val, !.	
%Ramasse les graines :								
ramasserTout(CJ, PDep, Score, NewScore) :-
								indice(CJ, PDep, Val),
								NScore is Score + Val,
								Position is PDep - 1,
								ramasserTout(CJ, Position, NScore, NewScore).


%---------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Prédicats sur les scores :
%Prédicat permettant de déterminer le gagnant :
scoreMax(ScoreJ1, ScoreJ2, ScoreJ1, TexteGagnant) :-
	ScoreJ1 > ScoreJ2,
	TexteGagnant = 'Bravo Joueur 1, tu as gagné !'.
	
scoreMax(ScoreJ1, ScoreJ2, ScoreJ2, TexteGagnant) :-
	ScoreJ1 > ScoreJ2,
	TexteGagnant = 'Bravo Joueur 2, tu as gagné !'.
	
scoreMax(ScoreJ1, ScoreJ2, ScoreJ1, TexteGagnant) :-
	ScoreJ1 = ScoreJ2,
	TexteGagnant = 'Egalité entre les deux joueurs'.

%Prédicat permettant de comparer 2 scores puis de passer à l'IA la case à jouer pour faire le meilleur score :	
max(Score1, NewScore, Score1, _, ACase, ACase) :- 
	Score1 > NewScore, !.
max(Score1, NewScore, NewScore, Case, _, Case) :- 
	Score1 < NewScore, !.
max(Score1, NewScore, NewScore, Case, _, Case) :- 
	Score1 = NewScore.
	
%---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Prédicats permettant l'affichage de l'Awale :
%Affiche les numéros des cases :	
afficherNumeroAwale(_) :-
            write('  1   2   3   4   5   6 \n').
%Affiche les contours du plateau :
afficherContourAwale(_) :-
            write(' --- --- --- --- --- --- \n').
%Permet d'ajouter le dernier bord du plateau :        
afficherInterieurAwale([], _) :-
            write('| \n').
%Permet de sauter une ligne et d'afficher la ligne du joueur :
afficherInterieurAwale([T|Q], 7) :-
            write('| \n'),
            afficherContourAwale(_),
            afficherInterieurAwale([T|Q],8).
%Permet d'afficher les lignes du plateau qui comportent le nombre de graines lorsque celui-ci est inférieur à 10 :            
afficherInterieurAwale([T|Q], I) :- 
            T < 10,
            write('| '),
            write(T),
            write(' '),
            I1 is I +1,
            afficherInterieurAwale(Q,I1).
%Permet d'afficher les lignes du plateau qui comportent le nombre de graines lorsque celui-ci est supérieur ou égal à 10 :                   
afficherInterieurAwale([T|Q], I) :- 
            T >= 10,
            write('| '),
            write(T),
            I1 is I +1,
            afficherInterieurAwale(Q,I1).
%Affiche le plateau de départ :
afficherAwale([], Plateau) :-
            afficherContourAwale(_),
            Plateau = [[4,4,4,4,4,4],[4,4,4,4,4,4]],
            transformation(Plateau, E),
            afficherInterieurAwale(E, 1),
            afficherContourAwale(_),
            afficherNumeroAwale(_),
            afficherScore(0,0), !.
%Affiche le plateau en cours de jeu :            
afficherAwale([T|Q],ScoreJ1,ScoreJ2) :-
            afficherContourAwale(_),
            afficherInterieurAwale([T|Q], 1),
            afficherContourAwale(_),
            afficherNumeroAwale(_),
            afficherScore(ScoreJ1,ScoreJ2), !.
%Affiche les scores des joueurs :            
afficherScore(ScoreJ1,ScoreJ2) :-
            write('\n Joueur 1 : '),
            write(ScoreJ1),
            write('\n'),
            write(' Joueur 2 : '),
            write(ScoreJ2),
            write('\n'),
            write('\n').
    
%---------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
%Affichage du menu et choix de la partie :
%Permet de lancer le menu du jeu puis de le relancer lorsque la partie se termine :
boucle_menu_mode_jeu :- repeat, menu_mode_jeu, !.
%Affiche le menu du jeu :
menu_mode_jeu :- 
    nl,
    write('1. Partie Humain / Humain'),nl,
    write('2. Partie Humain / Ordinateur'),nl,
    write('3. Partie Ordinateur / Ordinateur'),nl,
    write('4. Terminer'),nl,
    write('Choisissez un mode de jeu : '), read(Choix),nl, jouerPartie(Choix),
    Choix=4, nl.
%Lance la partie Joueur contre Joueur :    
jouerPartie(1) :-
    afficherAwale([], Plateau),
    /*menu_case(1, Plateau).*/
    menu_case([1,2], Plateau, 0, 0).
%Lance la Partie Ordi contre Joueur :	
jouerPartie(2) :-
    write('Partie Humain / Ordinateur \n'),
	afficherAwale([], Plateau),
    menu_caseIA([1,2],Plateau, 0, 0).
%Lance la Partie Ordi contre Ordi :	
jouerPartie(3) :-
    write('Partie Ordinateur / Ordinateur \n'),
	afficherAwale([], Plateau),
	menu_caseOrdi([1,2],Plateau,0,0).
%Quitte le jeu :	
jouerPartie(4) :-
    write('AU REVOIR :)').

%-------------------------------------------------------------------------------------------------------------------------------------------------------------------	
%Prédicats pour la partie joueur contre joueur :
%Condition d'arrêt lorsque le joueur 1 a au moins 25 points.
menu_case(_, _, ScoreJ1, _) :-
	ScoreJ1 >= 25,
	write('Bravo Joueur 1, tu as gagné !'), !.
%Condition d'arrêt lorsque le joueur 2 a au moins 25 points.
menu_case(_, _, _, ScoreJ2) :-
	ScoreJ2 >= 25,
	write('Bravo Joueur 2, tu as gagné !'), !.
%Condition d'arrêt lorsque le joueur 1 ne peut pas répartie de graine dans le champ adverse qui est vide :
menu_case([1|_], [[0,0,0,0,0,0],B], ScoreJ1, ScoreJ2) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreJ1, _),
	scoreMax(ScoreJ1, ScoreJ2, _, TexteGagnant),
	afficherScore(ScoreJ1,ScoreJ2),
	write(TexteGagnant), !.

%Condition d'arrêt lorsque le joueur 2 ne peut pas répartir de graine dans le champ adverse qui est vide :	
menu_case([2|_], [[0,0,0,0,0,0],B], ScoreJ1, ScoreJ2) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreJ2, _),
	scoreMax(ScoreJ1, ScoreJ2, _, TexteGagnant),
	afficherScore(ScoreJ1,ScoreJ2),
	write(TexteGagnant), !.
%Demande au joueur quelle case il veut jouer :	
menu_case(Joueur, Plateau, ScoreJ1, ScoreJ2) :-
    nl,
    nl,
    [T1,_] = Joueur,
    write('Joueur '),
    write(T1),
    write(' à vous de jouer \n'),
    write('1. Case 1'),nl,
    write('2. Case 2'),nl,
    write('3. Case 3'),nl,
    write('4. Case 4'),nl,
    write('5. Case 5'),nl,
    write('6. Case 6'),nl,    
    write('Choisissez une case : '), read(Case), nl, 
    jouerMouvement(Plateau, Case, Joueur, NewPlateau, JoueurBis, ScoreJ1, ScoreJ2, NewScoreJ1, NewScoreJ2),
    [NewH,NewB] = NewPlateau,
    inverseSousListe([NewH,NewB],[NewB,NewH]),
    inverse(NewH, NewH2),
    inverse(NewB, NewB2),
    menu_case(JoueurBis, [NewB2,NewH2], NewScoreJ1, NewScoreJ2), nl.
    
    
%Préviens que la case choisie par le joueur est vide et lui demande d'en choisir une autre :	
jouerMouvement(Plateau, Case, Joueur, [NewQ, NewT], Joueur, ScoreJ1, ScoreJ2, ScoreJ1, ScoreJ2) :-
    [_,B] = Plateau,
    indice(B,Case,Elem),
    Elem = 0,
    write('Cette case est vide, veuillez choisir une autre case'),
    write('\n'),
    [T,Q] = Plateau,
    transformation([T, Q],Plateau1),
    afficherAwale(Plateau1,ScoreJ1,ScoreJ2),
    inverseSousListe([T,Q],[Q,T]),
    inverse(T, NewT),
    inverse(Q, NewQ), !. 
%Joue la case demandé par le joueur et si sa partie du plateau est vide à la fin de son tour, lui empêche de ramasser les graines qu''il aurait pu gagner :
jouerMouvement(Plateau, Case, Joueur, [H2,NewNewB], JoueurBis, ScoreJ1, ScoreJ2, ScoreJ1, ScoreJ2) :- 
	Case>=1,
    Case=<6,
    [H,B] = Plateau,
    indice(B,Case,Elem),
    inverse(H, HI),
    repartirChampJoueurBis([HI,B], Case, Case, _, Elem, _, [H1, NewB], ScoreJ1, _),
	inverse(H1,H2),
    remplacer(Case,NewB,0,NewNewB),
	sum_list(NewNewB, Somme),	
	Somme = 0,
    transformation([H2,NewNewB],NewPlateau),
    afficherAwale(NewPlateau,ScoreJ1, ScoreJ2), 
    inverse(H2, H2Bis),
    inverse(NewNewB, NewNewB2),
    transformation([NewNewB2, H2Bis],NewPlateau2), 
    inverse(Joueur,JoueurBis),
    afficherAwale(NewPlateau2, ScoreJ1, ScoreJ2), !.
%Demande au joueur de rejouer si il peut répartir des graines dans le champ adverse et que ce dernier est vide :
jouerMouvement([[0,0,0,0,0,0],B], Case, Joueur, [NewB1,H], Joueur, ScoreJ1, ScoreJ2, ScoreJ1, ScoreJ2) :- 
    Case>=1,
    Case=<6,
    H = [0,0,0,0,0,0],
    indice(B,Case,Elem),
    repartirChampJoueur([H,B], Case, Case, _, Elem, _, [H1, _],ScoreJ1, ScoreJ1),
	H1=[0,0,0,0,0,0],
	write('Répartir à nouveau pour qu''il y ait des graines dans le champ adverse\n'),
	transformation([H,B],NewPlateau),
    afficherAwale(NewPlateau,ScoreJ1, ScoreJ2),
    inverse(B, NewB1),!.
%Joue le mouvement demandé par le joueur 1 :
jouerMouvement(Plateau, Case, [1|_], [H2,NewNewB], JoueurBis, ScoreJ1, ScoreJ2, NewScoreJ1, ScoreJ2) :- 
    Case>=1,
    Case=<6,
    [H,B] = Plateau,
    indice(B,Case,Elem),
    inverse(H, HI),
    repartirChampJoueur([HI,B], Case, Case, _, Elem, _, [H1, NewB],ScoreJ1, NewScoreJ1),
	inverse(H1,H2),
    remplacer(Case,NewB,0,NewNewB),
    transformation([H2,NewNewB],NewPlateau),
    afficherAwale(NewPlateau,NewScoreJ1, ScoreJ2), 
    inverse(H2, H2Bis),
    inverse(NewNewB, NewNewB2),
    transformation([NewNewB2, H2Bis],NewPlateau2), 
    inverse([1,2],JoueurBis),
    afficherAwale(NewPlateau2, NewScoreJ1, ScoreJ2), !.
	
%Joue le mouvement demandé par le joueur 2 :	
jouerMouvement(Plateau, Case, [2|_], [H2,NewNewB], JoueurBis, ScoreJ1, ScoreJ2, ScoreJ1, NewScoreJ2) :- 
    Case>=1,
    Case=<6,
    [H,B] = Plateau,
    indice(B,Case,Elem),
    inverse(H, HI),
    repartirChampJoueur([HI,B], Case, Case, _, Elem, _, [H1, NewB],ScoreJ2, NewScoreJ2),
    inverse(H1,H2),
    remplacer(Case,NewB,0,NewNewB),
    transformation([H2,NewNewB],NewPlateau),
    afficherAwale(NewPlateau, ScoreJ1, NewScoreJ2), 
    inverse(H2, H2Bis),
    inverse(NewNewB, NewNewB2),
    transformation([NewNewB2, H2Bis],NewPlateau2), 
    inverse([2,1],JoueurBis),
    afficherAwale(NewPlateau2, ScoreJ1, NewScoreJ2), !.
%Empêche le joueur de jouer si la case choisie est supérieure à 6 et n'existe donc pas :    
jouerMouvement(Plateau, Case, Joueur, [NewQ, NewT], Joueur, ScoreJ1, ScoreJ2, ScoreJ1, ScoreJ2) :-
    Case>6,
    write('Ceci n''est pas une case, veuillez saisir une nouvelle case'),
    write('\n'),
    [T,Q] = Plateau,
    transformation([T, Q],Plateau1),
    afficherAwale(Plateau1, ScoreJ1, ScoreJ2),
    inverseSousListe([T,Q],[Q,T]),
    inverse(T, NewT),
    inverse(Q, NewQ), !.
%Empêche le joueur de jouer si la case choisie est inférieure à 1 et n'existe donc pas :      
jouerMouvement(Plateau, Case, Joueur, [NewQ, NewT], Joueur, ScoreJ1, ScoreJ2, ScoreJ1, ScoreJ2) :-
    Case<1,
    write('Ceci n''est pas une case, veuillez saisir une nouvelle case'),
    write('\n'),
    [T,Q] = Plateau,
    transformation([T, Q],Plateau1),
    afficherAwale(Plateau1, ScoreJ1, ScoreJ2),
    inverseSousListe([T,Q],[Q,T]),
    inverse(T, NewT),
    inverse(Q, NewQ), !.
%----------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Prédicats concernant la partie Joueur VS IA :
%Condition d'arrêt du prédicat :
choixCaseIA([_,_], 7, Score1, Score1, ACase, ACase) :- !.
%Prédicat permettant de jouer une case qui remplit le plateau du Joueur si ce dernier est vide :
choixCaseIA([[0,0,0,0,0,0],B], Case, Score1, FScore, ACase, FCase) :- 
    H = [0,0,0,0,0,0],
    indice(B,Case,Elem),
    repartirChampJoueurBis([H,B], Case, Case, _, Elem, _, [H1, _],0, _),
	H1 = [0,0,0,0,0,0],
    NCase is Case + 1,
	choixCaseIA([H,B], NCase, Score1, FScore, ACase, FCase),!.
%Prédicat empêchant à l'IA de choisir une case comportant 0 graines :
choixCaseIA([H,B], Case, Score1, FScore, ACase, FCase) :- 
		indice(B,Case,0),
		NCase is Case + 1,
		choixCaseIA([H,B], NCase, Score1, FScore, ACase, FCase), !.
%Prédicat permettant à l'IA de choisir la meilleure case à jouer en comparant quelle case lui rapporte le plus de points :
choixCaseIA([H,B], Case, Score1, FScore, ACase, FCase) :- 
		indice(B,Case,Elem),
		repartirChampJoueur([H,B], Case, Case, _, Elem, _, [_, _],0, NewScoreIA),
		max(Score1, NewScoreIA, MaxScore, Case, ACase, OCase),
		NCase is Case + 1,
		choixCaseIA([H,B], NCase, MaxScore, FScore, OCase, FCase).
%Arrête le jeu si le joueur a au moins 25 graines :	
menu_caseIA(_,_, ScoreJ, _) :-
	ScoreJ >= 25,
	write('Bravo Joueur, tu as gagné !'), !.
%Arrête le jeu si l'IA a au moins 25 graines :	
menu_caseIA(_,_, _, ScoreIA) :-
	ScoreIA >= 25,
	write('Désolé Joueur, tu as perdu !'), !.
%Arrête le jeu si le joueur ne peut pas distribuer de graines sur le champ de l'IA et que ce dernier est vide :
menu_caseIA([1|_],[[0,0,0,0,0,0],B], ScoreJ, ScoreIA) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreJ, _),
	scoreMax(ScoreJ, ScoreIA, _, TexteGagnant),
	write('Voici les scores finaux'),
	afficherScore(ScoreJ,ScoreIA),
	write(TexteGagnant), !.
%Arrête le jeu si l'IA ne peut pas distribuer de graines sur le champ du joueur et que ce dernier est vide :
menu_caseIA([2|_], [[0,0,0,0,0,0],B], ScoreJ, ScoreIA) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreIA, _),
	scoreMax(ScoreJ, ScoreIA, _, TexteGagnant),
	afficherScore(ScoreJ,ScoreIA),
	write(TexteGagnant), !.
%Demande au joueur quelle case il veut jouer :	
menu_caseIA([1,_],Plateau, ScoreJ, ScoreIA) :- 
    nl,
    nl,
    [_,2] = Joueur,
    write('À vous de jouer \n'),
    write('1. Case 1'),nl,
    write('2. Case 2'),nl,
    write('3. Case 3'),nl,
    write('4. Case 4'),nl,
    write('5. Case 5'),nl,
    write('6. Case 6'),nl,    
    write('Choisissez une case : '), read(Case), nl, 
    jouerMouvement(Plateau, Case, Joueur, NewPlateau, JoueurBis, ScoreJ, ScoreIA, NewScoreJ, ScoreIA),
    [NewH,NewB] = NewPlateau,
    inverse(NewH, NewH2),
    inverse(NewB, NewB2),
    menu_caseIA(JoueurBis, [NewB2,NewH2], NewScoreJ, ScoreIA), nl.
%Fait jouer l'IA :
menu_caseIA([2,_], Plateau, ScoreJ, ScoreIA) :- 
	[H,B] = Plateau,
    inverseSousListe([H,B],[B,H]),
    inverse(H, NewH),
	jouerIA([NewH,B], NPlateau, ScoreJ, ScoreIA, ScoreJ, NewScoreIA),
	[NewB1,NewH1] = NPlateau,
    menu_caseIA([1|_],[NewB1,NewH1], ScoreJ, NewScoreIA), nl.
	
%Permet à l'IA de sélectionner une case puis déplacer les graines :
jouerIA(Plateau, [NewNewB2, H2Bis], ScoreJ, ScoreIA, ScoreJ, NewScoreIA) :-
	[H,B] = Plateau,
	choixCaseIA([H,B], 1, -1, _, 1, FCase),
    indice(B,FCase,Elem),
    repartirChampJoueur([H,B], FCase, FCase, _, Elem, _, [H1, NewB],ScoreIA, NewScoreIA),
	inverse(H1,H2),
    remplacer(FCase,NewB,0,NewNewB),
    transformation([H2,NewNewB],NewPlateau),
    afficherAwale(NewPlateau,ScoreJ, NewScoreIA), 
    inverse(H2, H2Bis),
    inverse(NewNewB, NewNewB2),
    transformation([NewNewB2, H2Bis],NewPlateau2),
    afficherAwale(NewPlateau2, ScoreJ, NewScoreIA),	!.
%----------------------------------------------------------------------------------------------------------------------------------------------------------------------
%Prédicats pour IA VS IA :
%Condition d'arrêt si le score de IA1 est au moins de 25 :
menu_caseOrdi(_,_, ScoreIA1, _) :-
	ScoreIA1 >= 25,
	write('Bravo IA1, tu as gagné !'), !.
%Condition d'arrêt si le score de IA2 est au moins de 25 :
menu_caseOrdi(_,_, _, ScoreIA2) :-
	ScoreIA2 >= 25,
	write('Bravo IA2, tu as gagné !'), !.
	
%Arrête le jeu si l'IA1 ne peut pas distribuer de graines sur le champ de l'IA2 et que ce dernier est vide :
menu_caseOrdi([1|_],[[0,0,0,0,0,0],B], ScoreIA1, ScoreIA2) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreIA1, _),
	scoreMax(ScoreIA1, ScoreIA2, _, TexteGagnant),
	write('Voici les scores finaux'),
	afficherScore(ScoreIA1,ScoreIA2),
	write(TexteGagnant), !.
%Arrête le jeu si l'IA2 ne peut pas distribuer de graines sur le champ de l'IA1 et que ce dernier est vide :
menu_caseOrdi([2|_], [[0,0,0,0,0,0],B], ScoreIA1, ScoreIA2) :-
	indice(B,1,Elem), Elem<6,
	indice(B,2,Elem1), Elem1<5,
	indice(B,3,Elem2), Elem2<4,
	indice(B,4,Elem3), Elem3<3,
	indice(B,5,Elem4), Elem4<2,
	indice(B,6,Elem5), Elem5<1,
	ramasserTout(B, 6, ScoreIA2, _),
	scoreMax(ScoreIA1, ScoreIA2, _, TexteGagnant),
	afficherScore(ScoreIA1,ScoreIA2),
	write(TexteGagnant), !.	
%Effectue le mouvement de l'IA1 :
menu_caseOrdi([1,_], Plateau, ScoreIA1, ScoreIA2) :- 
	[H,B] = Plateau,
    inverseSousListe([H,B],[B,H]),
    inverse(H, NewH),
	jouerIA([NewH,B], NPlateau, ScoreIA1, ScoreIA2, NewScoreIA1, ScoreIA2),
	[NewB1,NewH1] = NPlateau,
    menu_caseOrdi([2,_],[NewB1,NewH1], NewScoreIA1, ScoreIA2), nl.
%Effectue le mouvement de l'IA2 :
menu_caseOrdi([2|_], Plateau, ScoreIA1, ScoreIA2) :- 
	[H,B] = Plateau,
    inverseSousListe([H,B],[B,H]),
    inverse(H, NewH),
	jouerIA([NewH,B], NPlateau, ScoreIA1, ScoreIA2, ScoreIA1, NewScoreIA2),
	[NewB1,NewH1] = NPlateau,
    menu_caseOrdi([1,_],[NewB1,NewH1], ScoreIA1, NewScoreIA2), nl.
	
