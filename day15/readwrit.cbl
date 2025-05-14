       IDENTIFICATION DIVISION.
       PROGRAM-ID. readwrit.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GENS-INPUT
               ASSIGN TO "data/gens.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT GENS-OUTPUT
               ASSIGN TO "output/gens-output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT GENS-REVERSE-OUTPUT
               ASSIGN TO "output/gens-reverse-output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD GENS-INPUT.
       01 GENS-IN-RECORD.
           05 GENS-IN-NAME       PIC X(12).
           05 GENS-IN-FNAME      PIC X(12).

       FD GENS-OUTPUT.
       01 GENS-OUT-RECORD.
           05 GENS-OUT-NAME      PIC X(12).
           05 GENS-OUT-FNAME     PIC X(12).

       FD GENS-REVERSE-OUTPUT.
       01 GENS-REV-OUT-RECORD.
           05 GENS-REV-OUT-NAME  PIC X(12).
           05 GENS-REV-OUT-FNAME PIC X(12).

       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC 9(01) VALUE 0.
           88 WS-EOF-TRUE                  VALUE 1.
           88 WS-EOF-FALSE                 VALUE 0.
       
       01 GENS-TABLE.
           05 GENS OCCURS 10 TIMES.
               10 GENS-NAME      PIC X(12).
               10 GENS-FNAME     PIC X(12).


       77 WS-MAX-TABLE-SIZE      PIC 9(02) VALUE 10.
       77 WS-IDX                 PIC 9(02).
       77 WS-TABLE-SIZE          PIC 9(02).
       PROCEDURE DIVISION.
           MOVE 0 TO WS-IDX.
           OPEN INPUT GENS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ GENS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       IF WS-IDX < WS-MAX-TABLE-SIZE THEN
                           ADD 1 TO WS-IDX
                           MOVE GENS-IN-NAME TO GENS-NAME(WS-IDX)
                           MOVE GENS-IN-FNAME TO GENS-FNAME(WS-IDX)
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE GENS-INPUT.
           MOVE WS-IDX TO WS-TABLE-SIZE.

           OPEN OUTPUT GENS-OUTPUT.
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                   UNTIL WS-IDX > WS-TABLE-SIZE
               MOVE GENS-NAME(WS-IDX) TO GENS-OUT-NAME
               MOVE GENS-FNAME(WS-IDX) TO GENS-OUT-FNAME
               WRITE GENS-OUT-RECORD
           END-PERFORM.
           CLOSE GENS-OUTPUT.

           OPEN OUTPUT GENS-REVERSE-OUTPUT.
           PERFORM VARYING WS-IDX FROM WS-TABLE-SIZE BY -1
                   UNTIL WS-IDX EQUAL 0
               MOVE GENS-NAME(WS-IDX) TO GENS-REV-OUT-NAME
               MOVE GENS-FNAME(WS-IDx) TO GENS-REV-OUT-FNAME
               WRITE GENS-REV-OUT-RECORD
           END-PERFORM.
           CLOSE GENS-REVERSE-OUTPUT.
           STOP RUN.
       