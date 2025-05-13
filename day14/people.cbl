       IDENTIFICATION DIVISION.
       PROGRAM-ID. people.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 13-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PEOPLE-FILE
               ASSIGN TO "personnes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD PEOPLE-FILE.
       01 F-PEOPLE-RECORD.
           05 F-PEOPLE-NAME                  PIC X(15).
           05 F-PEOPLE-FIRST-NAME            PIC X(15).
           05 F-PEOPLE-BIRTHDATE.
               10 F-PEOPLE-BIRTHDATE-MONTH   PIC 9(02).
               10 F-PEOPLE-BIRTHDATE-DAY     PIC 9(02).
               10 F-PEOPLE-BIRTHDATE-YEAR    PIC 9(04).
               
       WORKING-STORAGE SECTION.
       01 WS-EOF                             PIC X(01) VALUE "N".
           88 WS-EOF-TRUE                              VALUE "Y".
           88 WS-EOF-FALSE                             VALUE "N".

       01 WS-PEOPLE-O.
           05 WS-PEOPLE-O-NAME               PIC X(15).
           05 FILLER                         PIC X(01) VALUE SPACE.
           05 WS-PEOPLE-O-FIRST-NAME         PIC X(15).
           05 FILLER                         PIC X(01) VALUE SPACE.
           05 WS-PEOPLE-O-BIRTHDATE.
               10 WS-PEOPLE-O-BIRTHDATE-DAY   PIC 9(02).
               10 FILLER                      PIC X(01) VALUE "/".
               10 WS-PEOPLE-O-BIRTHDATE-MONTH PIC 9(02).
               10 FILLER                      PIC X(01) VALUE "/".
               10 WS-PEOPLE-O-BIRTHDATE-YEAR  PIC 9(04).
           05 FILLER                          PIC X(01) VALUE SPACE.
           05 WS-PEOPLE-O-AGE                 PIC 9(03).
           05 FILLER                          PIC X(10)
                                                    VALUE" years old".

       01 WS-PEOPLE-TABLE.
           05 WS-PEOPLE OCCURS 10 TIMES.
               10 WS-PEOPLE-NAME               PIC X(15).
               10 WS-PEOPLE-FIRST-NAME         PIC X(15).
               10 WS-PEOPLE-BIRTHDATE.
                   15 WS-PEOPLE-BIRTHDATE-DAY   PIC 9(02).
                   15 WS-PEOPLE-BIRTHDATE-MONTH PIC 9(02).
                   15 WS-PEOPLE-BIRTHDATE-YEAR  PIC 9(04).
       
       77 WS-IDX                               PIC 9(02).
       77 WS-NAME-INPUT                        PIC X(15).
       77 WS-DATE                              PIC 9(08).
       77 WS-DATE-2                            PIC 9(08).

       01 WS-MATH-DATE.
           05 WS-MATH-DATE-YEAR                PIC 9(04).
           05 WS-MATH-DATE-MONTH               PIC 9(02).
           05 WS-MATH-DATE-DAY                 PIC 9(02).

       PROCEDURE DIVISION.
           OPEN INPUT PEOPLE-FILE.

           MOVE 0 TO WS-IDX.
           PERFORM UNTIL WS-EOF-TRUE
               READ PEOPLE-FILE
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX 
                       MOVE F-PEOPLE-NAME 
                         TO WS-PEOPLE-NAME(WS-IDX)
                       MOVE F-PEOPLE-FIRST-NAME 
                         TO WS-PEOPLE-FIRST-NAME(WS-IDX)
                       MOVE F-PEOPLE-BIRTHDATE-DAY
                         TO WS-PEOPLE-BIRTHDATE-DAY(WS-IDX)
                       MOVE F-PEOPLE-BIRTHDATE-MONTH
                         TO WS-PEOPLE-BIRTHDATE-MONTH(WS-IDX)
                       MOVE F-PEOPLE-BIRTHDATE-YEAR
                         TO WS-PEOPLE-BIRTHDATE-YEAR(WS-IDX)
               END-READ
           END-PERFORM.
           CLOSE PEOPLE-FILE.

           DISPLAY "Enter a name : " WITH NO ADVANCING .
           ACCEPT WS-NAME-INPUT.
           MOVE 1 TO WS-IDX
           PERFORM UNTIL WS-NAME-INPUT EQUAL WS-PEOPLE-NAME(WS-IDX)
                      OR WS-IDX > 10
               ADD 1 TO WS-IDX
           END-PERFORM.
           IF WS-IDX <= 10 THEN
               MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE

               MOVE WS-PEOPLE-BIRTHDATE-DAY(WS-IDX)
                 TO WS-MATH-DATE-DAY
               MOVE WS-PEOPLE-BIRTHDATE-MONTH(WS-IDX)
                 TO WS-MATH-DATE-MONTH
               MOVE WS-PEOPLE-BIRTHDATE-YEAR(WS-IDX)
                 TO WS-MATH-DATE-YEAR
               
               MOVE WS-MATH-DATE TO WS-DATE-2
               COMPUTE WS-PEOPLE-O-AGE = (WS-DATE-2 - WS-DATE)/10000

               MOVE WS-PEOPLE-NAME(WS-IDX)
                 TO WS-PEOPLE-O-NAME
               MOVE WS-PEOPLE-FIRST-NAME(WS-IDX)
                 TO WS-PEOPLE-O-FIRST-NAME
               MOVE WS-PEOPLE-BIRTHDATE-DAY(WS-IDX)
                 TO WS-PEOPLE-O-BIRTHDATE-DAY
               MOVE WS-PEOPLE-BIRTHDATE-MONTH(WS-IDX)
                 TO WS-PEOPLE-O-BIRTHDATE-MONTH
               MOVE WS-PEOPLE-BIRTHDATE-YEAR(WS-IDX)
                 TO WS-PEOPLE-O-BIRTHDATE-YEAR
               DISPLAY WS-PEOPLE-O
           ELSE
               DISPLAY "There is no one with this name"
           END-IF.
           STOP RUN.
