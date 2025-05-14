       IDENTIFICATION DIVISION.
       PROGRAM-ID. ereport.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTS-INPUT
               ASSIGN TO "data/compte-rendu.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
           SELECT REPORTS-COPY-OUTPUT
               ASSIGN TO "output/compte-rendu-copie.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD REPORTS-INPUT.
       01 RPT-IN-RCD.
           05 RPT-IN-TXT        PIC X(50).

       FD REPORTS-COPY-OUTPUT.
       01 RPT-CPY-OUT-RCD.
           05 RPT-CPY-OUT-TXT        PIC X(50).

       WORKING-STORAGE SECTION.
       01 WS-RPT-TBL.
           05 WS-RPT OCCURS 10 TIMES.
               10 WS-RPT-TXT PIC X(50).
       
       77 WS-IDX             PIC 9(02).

       77 WS-RPT-TBL-SIZE    PIC 9(02).

       01 WS-EOF             PIC 9(01).
           88 WS-EOF-TRUE              VALUE 1.
           88 WS-EOF-FALSE             VALUE 0.

       PROCEDURE DIVISION.
           SET WS-EOF-FALSE TO TRUE.
           MOVE 0 TO WS-IDX.
           OPEN INPUT REPORTS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ REPORTS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       IF RPT-IN-TXT NOT EQUAL SPACE THEN
                           ADD 1 TO WS-IDX
                           MOVE RPT-IN-TXT TO WS-RPT-TXT(WS-IDX)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE REPORTS-INPUT.
           MOVE WS-IDX TO WS-RPT-TBL-SIZE.

           MOVE 1 TO WS-IDX.
           OPEN OUTPUT REPORTS-COPY-OUTPUT
           PERFORM UNTIL WS-IDX > WS-RPT-TBL-SIZE
               MOVE WS-RPT-TXT(WS-IDX) TO RPT-CPY-OUT-TXT
               WRITE RPT-CPY-OUT-RCD
               ADD 1 TO WS-IDX
           END-PERFORM.
           CLOSE REPORTS-COPY-OUTPUT.

           STOP RUN.
