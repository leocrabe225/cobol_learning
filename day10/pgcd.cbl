      * Demander à l'utilisateur de rentrer deux nombres entier positif,
      * et calculer le plus grand commun diviseur en utilisant
      * l'algorithme d'euclide
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgcd.
       AUTHOR. Leocrabe225&Terry.
       DATE-WRITTEN. 06-05-2025 (fr).

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Déclaration des variables numériques à saisir par l'utilisateur 
       01 NUM-1                PIC 9(10).
       01 NUM-2                PIC 9(10).


       PROCEDURE DIVISION.

      *Saisie des deux nombres par l'utilisateur

           DISPLAY "Entrez deux nombres entiers positifs non nuls".
           ACCEPT NUM-1.
           ACCEPT NUM-2.
           
      *Si l'utilisateur entre un 0 alors on stoppe le programme

           IF NUM-1 < 1 OR NUM-2 < 1 THEN
           
               DISPLAY "Un des nombres saisi est nul."

               STOP RUN

           END-IF.
       
      *Boucle tant que les deux nombres saisis ne sont pas égaux

           PERFORM UNTIL NUM-1 EQUAL NUM-2

      *A chaque fois on soustrait le plus petit nombre à l'autre nombre 

               IF NUM-1 > NUM-2 THEN
                   SUBTRACT NUM-2 FROM NUM-1
               ELSE
                   SUBTRACT NUM-1 FROM NUM-2
               END-IF
               
      *Afficher les étapes intermédiaires pour vérification 

               DISPLAY "NUM-1 : " NUM-1 " ||| NUM-2 : " NUM-2 
           END-PERFORM.

      *Affichage du pgcd des deux nombres saisis et fin de programme 

           DISPLAY "Le pgcd est : " NUM-1.
           STOP RUN.
