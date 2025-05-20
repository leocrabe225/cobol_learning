       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSURANCE-INPUT ASSIGN TO "data/assurances.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT INSURANCE-OUTPUT ASSIGN TO "output/assurances-out.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INSURANCE-INPUT.
       01 ISR-IN-RCD            PIC X(121).
       FD INSURANCE-OUTPUT.
       01 ISR-OUT-RCD           PIC X(125).
       WORKING-STORAGE SECTION.
       01 WS-TBL OCCURS 36 TIMES INDEXED BY IDX PIC X(121).
       77 WS-USER-INPUT                         PIC X(1).
       77 WS-HEADER PIC X(121) VALUE "Code     Contract name  Product na
      -    "me   Client name                               Status   Star
      -    "t      End        Amount".
       PROCEDURE DIVISION.
           OPEN INPUT INSURANCE-INPUT.
           PERFORM 36 TIMES
               READ INSURANCE-INPUT NOT AT END 
                   MOVE ISR-IN-RCD TO WS-TBL(IDX)
                   ADD 1 TO IDX
               END-READ
           END-PERFORM.
           CLOSE INSURANCE-INPUT.
           DISPLAY "Do you want to print record 3 & 7 (Y/N)?".
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT EQUAL "Y" THEN
               DISPLAY WS-HEADER
               DISPLAY WS-TBL(3)
               DISPLAY WS-TBL(7)
           END-IF.
           DISPLAY "Do you want to write record 3 & 7 to a file (Y/N)?".
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT EQUAL "Y" THEN
               OPEN OUTPUT INSURANCE-OUTPUT
               MOVE WS-HEADER TO ISR-OUT-RCD
               WRITE ISR-OUT-RCD
               MOVE 3 TO IDX
               PERFORM 0100-WRITE-BEGIN THRU 0100-WRITE-END
               MOVE 7 TO IDX
               PERFORM 0100-WRITE-BEGIN THRU 0100-WRITE-END
               CLOSE INSURANCE-OUTPUT
               DISPLAY "End of processing, 2 Records were exported."
           END-IF
           STOP RUN.
       0100-WRITE-BEGIN.
           MOVE WS-TBL(IDX)(1:94) TO ISR-OUT-RCD.
           MOVE "-  -       -  -" TO ISR-OUT-RCD(95:15).
           MOVE WS-TBL(IDX)(95:2) TO ISR-OUT-RCD(96:2).
           MOVE WS-TBL(IDX)(97:7)  TO ISR-OUT-RCD(99:7).
           MOVE WS-TBL(IDX)(104:2)  TO ISR-OUT-RCD(107:2).
           MOVE WS-TBL(IDX)(106:16)  TO ISR-OUT-RCD(110:16).
           WRITE ISR-OUT-RCD.
       0100-WRITE-END.