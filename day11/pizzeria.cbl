      * Demandez à l'utilisateur de saisir le nombre de convive, puis
      * affichez le nombre de pizzas à commander en sachant que chaque
      * convive consomme 1.1 pizzas.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pizzeria.
       AUTHOR. Levain.
       DATE-WRITTEN. 07-06-2025 (fr).
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 LABYRINTH.
           05 LABYRINTH-ROW                OCCURS 15 TIMES.
               10 LABYRINTH-CELL  PIC X(1) OCCURS 15 TIMES.

       01 CURRENT-CELL            PIC X(1).

       01 STRING-UP               PIC X(1) VALUE "W".
       01 STRING-DOWN             PIC X(1) VALUE "S".
       01 STRING-LEFT             PIC X(1) VALUE "A".
       01 STRING-RIGHT            PIC X(1) VALUE "D".
       01 STRING-WALL             PIC X(1) VALUE "*".
       01 STRING-PLAYER           PIC X(1) VALUE "x".
       01 STRING-BANANA           PIC X(1) VALUE "B".
       01 STRING-PIZZERIA         PIC X(1) VALUE "P".

       01 POS-X                   PIC 9(2).
       01 POS-Y                   PIC 9(2).
       01 TEMP-POS-X              PIC 9(2).
       01 TEMP-POS-Y              PIC 9(2).
       01 IDX                     PIC 9(2).

       01 INPUT-DIRECTION         PIC X(1).

       01 GAME-STATUS             PIC X(1) VALUE "N".
           88 GAME-FINISHED                VALUE "Y".
           88 GAME-IN-PROGRESS             VALUE "N".


       01 WS-REPONSE              PIC X(1) VALUE "N".
       01 WS-NOM-AN                  PIC X(20).

       01  WS-NOM          PIC X(20) VALUE SPACES.
       01  WS-PRENOM       PIC X(20) VALUE SPACES.
       01  WS-NB-INVITE    PIC 9(02).   
       01  FIN-SAISIE      PIC X     VALUE 'N'.
       01  WS-INTRO-STOP   PIC X(01).

       01  WS-NB-PIZZA     PIC 9(2) VALUE ZERO.

      
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-PRENOM NOT EQUAL SPACE
               DISPLAY "Quel est votre prénom ?"
               ACCEPT WS-PRENOM
           END-PERFORM
           DISPLAY "Combien êtes vous ?"
           ACCEPT WS-NB-PIZZA
           COMPUTE WS-NB-PIZZA EQUAL WS-NB-PIZZA * 1.1 + 0.9


           PERFORM 0100-SET-LABYRINTH-BEGIN
              THRU 0100-SET-LABYRINTH-END.
           PERFORM UNTIL GAME-FINISHED
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 15
                   DISPLAY LABYRINTH-ROW(IDX)
               END-PERFORM
               DISPLAY "Utiliser   W   pour bouger"
               DISPLAY "          Q D             "
               DISPLAY "           S              "
               ACCEPT INPUT-DIRECTION
               MOVE FUNCTION UPPER-CASE(INPUT-DIRECTION)
                 TO INPUT-DIRECTION
               IF INPUT-DIRECTION EQUAL STRING-UP THEN
                   SUBTRACT 1 FROM TEMP-POS-Y
               END-IF
               IF INPUT-DIRECTION EQUAL STRING-DOWN THEN
                   SUBTRACT -1 FROM TEMP-POS-Y
               END-IF
               IF INPUT-DIRECTION EQUAL STRING-LEFT THEN
                   SUBTRACT 1 FROM TEMP-POS-X
               END-IF
               IF INPUT-DIRECTION EQUAL STRING-RIGHT THEN
                   SUBTRACT -1 FROM TEMP-POS-X
               END-IF
               IF LABYRINTH-CELL(TEMP-POS-Y, TEMP-POS-X) 
                  NOT EQUAL STRING-WALL
                   MOVE CURRENT-CELL TO LABYRINTH-CELL(POS-Y, POS-X)
                   MOVE LABYRINTH-CELL(TEMP-POS-Y, TEMP-POS-X)
                     TO CURRENT-CELL
                   MOVE TEMP-POS-Y TO POS-Y
                   MOVE TEMP-POS-X TO POS-X
                   MOVE STRING-PLAYER TO LABYRINTH-CELL(POS-Y, POS-X)
                   IF CURRENT-CELL EQUAL STRING-BANANA THEN
                       PERFORM 0200-BANANA-BEGIN
                          THRU 0200-BANANA-END
                       MOVE CURRENT-CELL TO LABYRINTH-CELL(POS-Y,POS-X)
                       MOVE 2 TO POS-Y
                       MOVE 2 TO POS-X
                       MOVE POS-X TO TEMP-POS-X
                       MOVE POS-Y TO TEMP-POS-Y
                       MOVE SPACE TO CURRENT-CELL
                       MOVE STRING-PLAYER TO LABYRINTH-CELL(POS-Y,POS-X)
                   END-IF
                   IF CURRENT-CELL EQUAL STRING-PIZZERIA THEN
                       SET GAME-FINISHED TO TRUE
                       PERFORM 0300-STORY-ENDING-BEGIN
                          THRU 0300-STORY-ENDING-END
                   END-IF
               ELSE
                   MOVE POS-Y TO TEMP-POS-Y
                   MOVE POS-X TO TEMP-POS-X
               END-IF
           END-PERFORM.
           STOP RUN.
           


       0100-SET-LABYRINTH-BEGIN.
           MOVE 2 TO POS-X.
           MOVE 2 TO POS-Y.
           MOVE POS-X TO TEMP-POS-X.
           MOVE POS-Y TO TEMP-POS-Y.
           MOVE "***************" TO LABYRINTH-ROW(1).
           MOVE "*       *     *" TO LABYRINTH-ROW(2).
           MOVE "* *** *** *** *" TO LABYRINTH-ROW(3).
           MOVE "*   *   * *   *" TO LABYRINTH-ROW(4).
           MOVE "******* * * ***" TO LABYRINTH-ROW(5).
           MOVE "*         *   *" TO LABYRINTH-ROW(6).
           MOVE "* ***B******* *" TO LABYRINTH-ROW(7).
           MOVE "*   *   ***   *" TO LABYRINTH-ROW(8).
           MOVE "*** *** *P* ***" TO LABYRINTH-ROW(9).
           MOVE "* * * * * *   *" TO LABYRINTH-ROW(10).
           MOVE "* * * * * *** *" TO LABYRINTH-ROW(11).
           MOVE "*   * * * * * *" TO LABYRINTH-ROW(12).
           MOVE "* *** * * * * *" TO LABYRINTH-ROW(13).
           MOVE "*     * *     *" TO LABYRINTH-ROW(14).
           MOVE "***************" TO LABYRINTH-ROW(15).
           MOVE LABYRINTH-CELL(POS-Y, POS-X) TO CURRENT-CELL.
           MOVE STRING-PLAYER TO LABYRINTH-CELL(POS-Y, POS-X).
       0100-SET-LABYRINTH-END.
           
       0200-BANANA-BEGIN.
           DISPLAY "Oooooh".
           PERFORM 3 TIMES
               CALL "C$SLEEP" USING 1 END-CALL
               DISPLAY "."
           END-PERFORM.
           DISPLAY "Nooooon" .
           PERFORM 3 TIMES
               CALL "C$SLEEP" USING 1 END-CALL
               DISPLAY "."
           END-PERFORM.
           DISPLAY "Tu as glissé sur une banane".
           PERFORM 5 TIMES
               CALL "C$SLEEP" USING 1 END-CALL
               DISPLAY "."
           END-PERFORM.
       0200-BANANA-END.

       0300-STORY-ENDING-BEGIN.
           PERFORM UNTIL WS-REPONSE EQUAL "Y"
                   DISPLAY "Bonjour, avez-vous une commande ? (Y/N)"
                   ACCEPT WS-REPONSE
           END-PERFORM.
           DISPLAY "Super, à quel nom ?"
           PERFORM UNTIL WS-NOM-AN EQUAL WS-PRENOM
                   ACCEPT WS-NOM-AN
                   IF WS-NOM-AN NOT EQUAL WS-PRENOM THEN
                       DISPLAY "Nous n'avons pas de commande à ce nom."
                       DISPLAY "Quel est donc votre nom ?"
                   END-IF
           END-PERFORM.
           DISPLAY "Aaaah, " WS-NOM-AN.
           CALL "C$SLEEP" USING 1 END-CALL.
           DISPLAY "Très bien."
           CALL "C$SLEEP" USING 1 END-CALL.
           DISPLAY "Voici vos " WS-NB-PIZZA " pizzas !".
       0300-STORY-ENDING-END.
