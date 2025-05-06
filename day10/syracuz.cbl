       IDENTIFICATION DIVISION.
       PROGRAM-ID. syracuz.
       AUTHOR. Terry&Leocrabe225.
       DATE-WRITTEN. 06-05-2025 (fr).

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Déclaration d'une variable numérique pour saisie utilisateur.
       01 WS-NUM               PIC 9(20) VALUE 0.
      *Déclaration d'une variable poubelle pour le calcul.
       01 WS-TRASH             PIC 9(01).
      *Déclaration d'une variable pour 
      *récupérer le reste de la division.
       01 WS-REMAINDER         PIC 9(01).
      *Déclaration d'une variable pour afficher le résultat.
       01 WS-OUTPUT            PIC Z(20).
       PROCEDURE DIVISION.
      *Boucle pour empêcher la saisie inférieur à 2 ou supérieur à 1E15
      * par l'utilisateur.
           PERFORM UNTIL WS-NUM > 1 AND WS-NUM < 1000000000000000
               DISPLAY 
                 "Enter a number greater than 1 and smaller than 1E15: "
                 WITH NO ADVANCING 
      *Saisie utilisateur.
               ACCEPT WS-NUM
           END-PERFORM.
      *Boucle pour atteindre 1 en sortie finale.
           PERFORM UNTIL WS-NUM EQUAL 1
      *Effectue le calcul pour pouvoir vérifier si le nombre est pair.
               DIVIDE WS-NUM BY 2 GIVING WS-TRASH REMAINDER WS-REMAINDER
      *Si le nombre est pair alors on divise par deux
               IF WS-REMAINDER EQUAL 0 THEN
                   DIVIDE 2 INTO WS-NUM
      *Si le nombre est impair on le multiplie par 3 et on ajoute 1.
               ELSE
                   MULTIPLY 3 BY WS-NUM
                   ADD 1 TO WS-NUM
               END-IF
      *On transfert le résultat du calcul dans la variable d'affichage.
               MOVE WS-NUM TO WS-OUTPUT
      *On affiche le résultat en enlevant les 0 inutiles.
               DISPLAY FUNCTION TRIM(WS-OUTPUT)
           END-PERFORM.
           STOP RUN.
