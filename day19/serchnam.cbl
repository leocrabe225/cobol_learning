       IDENTIFICATION DIVISION.
       PROGRAM-ID. serchnam.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 20-05-2025 (fr).
       DATE-COMPILED. null.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLASS-INPUT
               ASSIGN TO "data/input-classes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLASS-INPUT.
       01 CLASS-RCD.
           05 CLASS-NAME               PIC X(03).
           05 FILLER                   PIC X(02).
           05 STUD-NAME                PIC X(08).
           05 FILLER                   PIC X(01).
           05 STUD-FNAME               PIC X(08).

       WORKING-STORAGE SECTION.
       01 WS-TBL-STUDENTS.
           10 WS-STUD    OCCURS 12 TIMES.
               15 WS-CLASS-NAME        PIC X(03).
               15 WS-STUD-NAME         PIC X(08).
               15 WS-STUD-FNAME        PIC X(08).

       01 WS-STUDENT-OUTPUT.
           05 FILLER                   PIC X(05) VALUE "Class".
           05 FILLER                   PIC X(03) VALUE " : ".
           05 WS-CLASS-NAME-OUTPUT     PIC X(03).
           05 FILLER                   PIC X(03) VALUE " | ".
           05 FILLER                   PIC X(04) VALUE "Name".
           05 FILLER                   PIC X(03) VALUE " : ".
           05 WS-STUDENT-NAME-OUTPUT   PIC X(08).
           05 FILLER                   PIC X(03) VALUE " | ".
           05 FILLER                   PIC X(10) VALUE "First name".
           05 FILLER                   PIC X(03) VALUE " : ".
           05 WS-STUDENT-FNAME-OUTPUT  PIC X(08).

       77 WS-IDX-1                     PIC 9(02).

       77 WS-USER-INPUT                PIC X(15).

       COPY "../copybooks/eof.cbl".

       PROCEDURE DIVISION.

           PERFORM 0100-READ-CLASS-FILE-BEGIN
              THRU 0100-READ-CLASS-FILE-END.

           PERFORM 0200-USER-INPUT-BEGIN
              THRU 0200-USER-INPUT-END.

           PERFORM 0300-DISPLAY-MATCHING-STUDENTS-BEGIN
              THRU 0300-DISPLAY-MATCHING-STUDENTS-END.

           STOP RUN.
           
       0100-READ-CLASS-FILE-BEGIN.
           MOVE 0 TO WS-IDX-1.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT CLASS-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ CLASS-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX-1
                       MOVE CLASS-NAME TO WS-CLASS-NAME(WS-IDX-1)
                       MOVE STUD-NAME  TO WS-STUD-NAME(WS-IDX-1)
                       MOVE STUD-FNAME TO WS-STUD-FNAME(WS-IDX-1)
               END-READ
           END-PERFORM.
           CLOSE CLASS-INPUT.
       0100-READ-CLASS-FILE-END.

       0200-USER-INPUT-BEGIN.
           DISPLAY 
               "Which name do you want to search in the classes ? : "
               WITH NO ADVANCING.
           ACCEPT WS-USER-INPUT.
       0200-USER-INPUT-END.

       0300-DISPLAY-MATCHING-STUDENTS-BEGIN.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 12
               IF WS-USER-INPUT EQUAL WS-STUD-NAME(WS-IDX-1) THEN
                   PERFORM 0310-MOVE-STUDENT-TO-OUTPUT-BEGIN
                      THRU 0310-MOVE-STUDENT-TO-OUTPUT-END
                   DISPLAY WS-STUDENT-OUTPUT
               END-IF
           END-PERFORM.
       0300-DISPLAY-MATCHING-STUDENTS-END.

       0310-MOVE-STUDENT-TO-OUTPUT-BEGIN.
           MOVE WS-CLASS-NAME(WS-IDX-1) TO WS-CLASS-NAME-OUTPUT.
           MOVE WS-STUD-NAME(WS-IDX-1)  TO WS-STUDENT-NAME-OUTPUT.
           MOVE WS-STUD-FNAME(WS-IDX-1) TO WS-STUDENT-FNAME-OUTPUT.
       0310-MOVE-STUDENT-TO-OUTPUT-END.
