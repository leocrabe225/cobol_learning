       IDENTIFICATION DIVISION.
       PROGRAM-ID. outofsto.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-INPUT
               ASSIGN TO "data/inventaire.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT OUTOFSTOCK-OUTPUT
               ASSIGN TO "output/rupture.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-INPUT.
       01 ITEM-RECORD.
           05 ITEM-NAME             PIC X(10).
           05 ITEM-AMOUNT           PIC 9(02).

       FD OUTOFSTOCK-OUTPUT.
       01 OUTOFSTOCK-RECORD.
           05 OUTOFSTOCK-NAME       PIC X(10).

       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC 9(01) VALUE 0.
           88 WS-EOF-TRUE                  VALUE 1.
           88 WS-EOF-FALSE                 VALUE 0.

       01 WS-ITEM-TBL.
           05 WS-ITEM            OCCURS 99 TIMES.
               10 WS-ITEM-NAME   PIC X(10).
               10 WS-ITEM-AMOUNT PIC 9(02).


       01 WS-IDX                 PIC 9(02).
       01 WS-ITEM-TBL-SIZE       PIC 9(02).
       PROCEDURE DIVISION.
           MOVE 0 TO WS-IDX
           OPEN INPUT INVENTORY-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ INVENTORY-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE ITEM-NAME TO WS-ITEM-NAME(WS-IDX)
                       MOVE FUNCTION NUMVAL(ITEM-AMOUNT)
                         TO WS-ITEM-AMOUNT(WS-IDX)
               END-READ
           END-PERFORM.
           CLOSE INVENTORY-INPUT.
           MOVE WS-IDX TO WS-ITEM-TBL-SIZE.

           MOVE 1 TO WS-IDX.
           OPEN OUTPUT OUTOFSTOCK-OUTPUT
           PERFORM UNTIL WS-IDX > WS-ITEM-TBL-SIZE
               IF WS-ITEM-AMOUNT(WS-IDX) = 0 THEN
                   MOVE WS-ITEM-NAME(WS-IDX) TO OUTOFSTOCK-NAME
                   WRITE OUTOFSTOCK-RECORD
               END-IF
               DISPLAY WS-ITEM-AMOUNT(WS-IDX)
               ADD 1 TO WS-IDX
           END-PERFORM.
           CLOSE OUTOFSTOCK-OUTPUT.

           STOP RUN.
