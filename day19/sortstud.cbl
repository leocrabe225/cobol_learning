       IDENTIFICATION DIVISION.
       PROGRAM-ID. sortstud.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 20-05-2025 (fr).
       DATE-COMPILED. null.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STUDENTS-OUTPUT
               ASSIGN TO "output/students.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT SORT-STUDENTS-OUTPUT
               ASSIGN TO WRK.

       DATA DIVISION.
       FILE SECTION.
       FD STUDENTS-OUTPUT.
       COPY "copybooks/stud.cpy" REPLACING ==:TAG:== BY ==F==.
               
       SD SORT-STUDENTS-OUTPUT.
       COPY "copybooks/stud.cpy" REPLACING ==:TAG:== BY ==SORT==.
            
       WORKING-STORAGE SECTION.
       01 WS-TBL-STUDENTS.
           05 WS-CLASS      OCCURS 2 TIMES. *> 1 = CM1, 2 = CM2
              10 WS-CLASS-NAME        PIC X(03).
              10 WS-STUD    OCCURS 6 TIMES.
                  15 WS-STUD-NAME     PIC X(15).
                  15 WS-STUD-FNAME    PIC X(15).

       77 WS-IDX-1                    PIC 9(01).
       77 WS-IDX-2                    PIC 9(01).

       01 WS-STUDENT-OUTPUT.
           05 WS-CLASS-NAME-OUTPUT     PIC X(03).
           05 FILLER                   PIC X(03) VALUE " | ".
           05 WS-STUDENT-NAME-OUTPUT   PIC X(08).
           05 FILLER                   PIC X(03) VALUE " | ".
           05 WS-STUDENT-FNAME-OUTPUT  PIC X(08).

       77 WS-USER-INPUT               PIC X(15).

       PROCEDURE DIVISION.
           PERFORM 0100-INITIALIZE-BEGIN
              THRU 0100-INITIALIZE-END.

           PERFORM 0200-INPUT-STUDENTS-BEGIN
              THRU 0200-INPUT-STUDENTS-END.
       
           PERFORM 0300-DISPLAY-STUDENTS-BEGIN
              THRU 0300-DISPLAY-STUDENTS-END.
       
           PERFORM 0400-WRITE-STUDENTS-FILE-BEGIN
              THRU 0400-WRITE-STUDENTS-FILE-END.

           PERFORM 0500-SORT-STUDENTS-BEGIN
              THRU 0500-SORT-STUDENTS-END.
           
           STOP RUN.

       0100-INITIALIZE-BEGIN.
           MOVE "CM1" TO WS-CLASS-NAME(1).
           MOVE "CM2" TO WS-CLASS-NAME(2).
       0100-INITIALIZE-END.

       0200-INPUT-STUDENTS-BEGIN.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 2
               PERFORM VARYING WS-IDX-2 FROM 1 BY 1 UNTIL WS-IDX-2 > 6
                   DISPLAY "Enter the name and first name of student"
                       SPACE WS-IDX-2 " of " WS-CLASS-NAME(WS-IDX-1) "."
                   DISPLAY "Name : "
                       WITH NO ADVANCING
                   ACCEPT WS-USER-INPUT
                   MOVE WS-USER-INPUT 
                       TO WS-STUD-NAME(WS-IDX-1,WS-IDX-2)
                   DISPLAY "First Name : "
                       WITH NO ADVANCING
                   ACCEPT WS-USER-INPUT
                   MOVE WS-USER-INPUT
                       TO WS-STUD-FNAME(WS-IDX-1, WS-IDX-2)
               END-PERFORM
           END-PERFORM.
       0200-INPUT-STUDENTS-END.

       0300-DISPLAY-STUDENTS-BEGIN.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 2
               PERFORM VARYING WS-IDX-2 FROM 1 BY 1 UNTIL WS-IDX-2 > 6
                   DISPLAY "Class : " WS-CLASS-NAME(WS-IDX-1) " | "
                           "Name : " WS-STUD-NAME(WS-IDX-1,WS-IDX-2)
                           " | "
                           "First Name : "
                           WS-STUD-FNAME(WS-IDX-1, WS-IDX-2)
               END-PERFORM
           END-PERFORM.
       0300-DISPLAY-STUDENTS-END.
       
       0400-WRITE-STUDENTS-FILE-BEGIN.
           OPEN OUTPUT STUDENTS-OUTPUT.
           PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 2
               MOVE WS-CLASS-NAME(WS-IDX-1) TO WS-CLASS-NAME-OUTPUT
               PERFORM VARYING WS-IDX-2 FROM 1 BY 1 UNTIL WS-IDX-2 > 6
                   MOVE WS-STUD-NAME(WS-IDX-1, WS-IDX-2)
                       TO WS-STUDENT-NAME-OUTPUT
                   MOVE WS-STUD-FNAME(WS-IDX-1, WS-IDX-2)
                       TO WS-STUDENT-FNAME-OUTPUT
                   MOVE WS-STUDENT-OUTPUT
                       TO F-STUDENTS-RECORD
                   WRITE F-STUDENTS-RECORD
               END-PERFORM
           END-PERFORM.
           CLOSE STUDENTS-OUTPUT.
       0400-WRITE-STUDENTS-FILE-END.

       0500-SORT-STUDENTS-BEGIN.
           SORT SORT-STUDENTS-OUTPUT
               ON ASCENDING KEY SORT-STUDENT-FULL-NAME
               USING STUDENTS-OUTPUT
               GIVING STUDENTS-OUTPUT.
       0500-SORT-STUDENTS-END.
