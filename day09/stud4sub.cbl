       IDENTIFICATION DIVISION.
       PROGRAM-ID. stud4sub.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 05-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENT-TABLE.
          05 STUDENT           OCCURS 3 TIMES.
             10 STUDENT-NAME   PIC X(20).
             10 STUDENT-GRADE  PIC 9(2) OCCURS 4 TIMES.

       01 SUBJECT-TABLE.
          05 SUBJECT-NAME      PIC X(20) OCCURS 4 TIMES.

       01 TOTAL                PIC 9(3).
       01 IDX-1                PIC 9.
       01 IDX-2                PIC 9.
       PROCEDURE DIVISION.
           MOVE "French" TO SUBJECT-NAME(1).
           MOVE "Physics" TO SUBJECT-NAME(2).
           MOVE "Math" TO SUBJECT-NAME(3).
           MOVE "English" TO SUBJECT-NAME(4).
           PERFORM VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 > 3
               DISPLAY "Enter the student's name : " 
                   WITH NO ADVANCING
               ACCEPT STUDENT-NAME(IDX-1)
               PERFORM VARYING IDX-2 FROM 1 BY 1 UNTIL IDX-2 > 4
                   DISPLAY "Enter " STUDENT-NAME(IDX-1) "'s "
                       SUBJECT-NAME(IDX-2) " note : "  
                       WITH NO ADVANCING
                   ACCEPT STUDENT-GRADE(IDX-1, IDX-2)
               END-PERFORM
           END-PERFORM.

           PERFORM VARYING IDX-2 FROM 1 BY 1 UNTIL IDX-2 > 4
               MOVE 0 TO TOTAL
               PERFORM VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 > 3
                   ADD STUDENT-GRADE(IDX-1, IDX-2) TO TOTAL
               END-PERFORM
               DIVIDE 3 INTO TOTAL
               DISPLAY SUBJECT-NAME(IDX-2) "'s average : " TOTAL
           END-PERFORM.
           STOP RUN.
