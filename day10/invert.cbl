       IDENTIFICATION DIVISION.
       PROGRAM-ID. invert.
       AUTHOR. Leocrabe225 &ThomasD.
       
       DATE-WRITTEN. 06-05-2025 (fr).
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Variable tampon pour stocker une lettre du mot choisi 
       01 LETTER               PIC X(1).

      *Mot saisi par l'utilisateur 
       01 WS-WORD              PIC X(20).

      *Index de début et de fin du mot choisi 
       01 IDX-START            PIC 9(2) VALUE 1.
       01 IDX-END              PIC 9(2).

       PROCEDURE DIVISION.
      *Demande de saisie du mot 
           DISPLAY "Enter a word.".
           ACCEPT WS-WORD.
      *On stocke la taille de la variable du mot choisi dans l'index de fin     
           MOVE FUNCTION LENGTH(WS-WORD) TO IDX-END.

      *Boucle tant que les index ne se croisent pas 
           PERFORM UNTIL IDX-START >= IDX-END
      *On déplace  
               MOVE WS-WORD(IDX-END:1) TO LETTER
               MOVE WS-WORD(IDX-START:1) TO WS-WORD(IDX-END:1)
               MOVE LETTER TO WS-WORD(IDX-START:1)
               ADD 1 TO IDX-START
               SUBTRACT 1 FROM IDX-END
           END-PERFORM.
           DISPLAY WS-WORD.

           STOP RUN.
