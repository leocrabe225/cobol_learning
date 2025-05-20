       IDENTIFICATION DIVISION.
       PROGRAM-ID. doom.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 20-05-2025.
       DATE-COMPILED. null.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-IDX             PIC 9(04).
       01 WS-IDX-2             PIC 9(04).

       01 WS-USER-INPUT      PIC X(02).
       01 WS-TEST-NBR        PIC 9.9 VALUE 0.5.

       01 WS-RES-X           PIC 9(04) VALUE 500.
       01 WS-RES-Y           PIC 9(04) VALUE 100.

       01 WS-LIMIT           PIC 9(04) VALUE 10.
       01 WS-LINE            PIC X(500) VALUE ALL SPACE.
       PROCEDURE DIVISION.
      *    PERFORM 10 TIMES
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                   UNTIL WS-IDX >  WS-RES-Y
      *        PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
      *                UNTIL WS-IDX-2 > WS-RES-X
                   COMPUTE WS-LIMIT = FUNCTION RANDOM * 8
                   DISPLAY WS-LINE
                       BACKGROUND-COLOR IS WS-LIMIT
                       AT LINE NUMBER WS-IDX
      *                AT COLUMN NUMBER WS-IDX-2
                   END-DISPLAY
      *        END-PERFORM
      *        DISPLAY SPACE SCROLL DOWN 1 LINES
      *    END-PERFORM
           END-PERFORM.
      *    ACCEPT WS-USER-INPUT TIMEOUT WS-TEST-NBR.
           STOP RUN.
