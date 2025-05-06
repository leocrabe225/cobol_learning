      * Faites entrer à l'utilisateur une année, puis indiquer si elle
      * est bisextile.
      * Une année est bisextile si elle est divisible
      * par 4 mais pas par 100
      * ou si elle est divisible par 400.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. leapyear.
       AUTHOR. Leocrabe225&Anais.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ANNEE             PIC 9(4).
       01 WS-POUBELLE          PIC 9(4).
       01 WS-RESTE4            PIC 9(4).
       01 WS-RESTE100          PIC 9(4).
       01 WS-RESTE400          PIC 9(4).

       PROCEDURE DIVISION.
           DISPLAY "Tapez une année".
           ACCEPT WS-ANNEE.
           DIVIDE WS-ANNEE BY 4 GIVING WS-POUBELLE REMAINDER WS-RESTE4.
           DIVIDE WS-ANNEE BY 100 GIVING WS-POUBELLE 
              REMAINDER WS-RESTE100.
           DIVIDE WS-ANNEE BY 400 GIVING WS-POUBELLE 
              REMAINDER WS-RESTE400.

           IF (WS-RESTE4 EQUAL 0 AND WS-RESTE100 NOT EQUAL 0) 
              OR WS-RESTE400 EQUAL 0 THEN
              DISPLAY "Il est bisextile"
           ELSE
              DISPLAY "Il n'est pas bisextile"
           END-IF.

           STOP RUN.
