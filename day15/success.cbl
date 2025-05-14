       IDENTIFICATION DIVISION.
       PROGRAM-ID. success.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 14-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENTS-INPUT
               ASSIGN TO "data/eleves.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT SUCCESS-OUTPUT
               ASSIGN TO "output/reussite.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENTS-INPUT.
       01 STUD-RECORD.
           05 STUD-NAME          PIC X(10).
           05 STUD-GRADE         PIC 9(02).

       FD SUCCESS-OUTPUT.
       01 SUCCESS-RECORD.
           05 SUCCESS-NAME       PIC X(10).
           05 SUCCESS-GRADE      PIC 9(02).

       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC 9(01) VALUE 0.
           88 WS-EOF-TRUE                  VALUE 1.
           88 WS-EOF-FALSE                 VALUE 0.

       01 WS-STUD-TBL.
           05 WS-STUD            OCCURS 99 TIMES.
               10 WS-STUD-NAME   PIC X(10).
               10 WS-STUD-GRADE  PIC 9(02).


       01 WS-IDX                 PIC 9(02).
       01 WS-STUD-TBL-SIZE       PIC 9(02).
       01 WS-TARGET-GRADE        PIC 9(02) VALUE 10.
       PROCEDURE DIVISION.
           MOVE 0 TO WS-IDX
           OPEN INPUT STUDENTS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ STUDENTS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE STUD-NAME TO WS-STUD-NAME(WS-IDX)
                       MOVE FUNCTION NUMVAL(STUD-GRADE)
                         TO WS-STUD-GRADE(WS-IDX)
               END-READ
           END-PERFORM.
           CLOSE STUDENTS-INPUT.
           MOVE WS-IDX TO WS-STUD-TBL-SIZE.

           MOVE 1 TO WS-IDX.
           OPEN OUTPUT SUCCESS-OUTPUT
           PERFORM UNTIL WS-IDX > WS-STUD-TBL-SIZE
               IF WS-STUD-GRADE(WS-IDX) > 10 THEN
                   MOVE WS-STUD-NAME(WS-IDX) TO SUCCESS-NAME
                   MOVE WS-STUD-GRADE(WS-IDX) TO SUCCESS-GRADE
                   WRITE SUCCESS-RECORD
               END-IF
               ADD 1 TO WS-IDX
           END-PERFORM.
           CLOSE SUCCESS-OUTPUT.

           STOP RUN.
           