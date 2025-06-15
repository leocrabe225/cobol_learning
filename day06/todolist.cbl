       IDENTIFICATION DIVISION.
       PROGRAM-ID. todolist.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 29-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STRING-ADD           PIC X(3) VALUE "ADD".
       01 STRING-DISPLAY       PIC X(7) VALUE "DISPLAY".
       01 STRING-REMOVE        PIC X(6) VALUE "REMOVE".
       01 STRING-QUIT          PIC X(4) VALUE "QUIT".
       01 INPUT1               PIC X(50) VALUE ZEROS.
       01 INPUT-INDEX          PIC 9(1).
       01 NEW-VALUE            PIC X(50).

       01 TASK0                PIC X(50).
       01 TASK1                PIC X(50).
       01 TASK2                PIC X(50).
       01 TASK3                PIC X(50).
       01 TASK4                PIC X(50).
       PROCEDURE DIVISION.
           PERFORM UNTIL INPUT1 EQUAL STRING-QUIT
               DISPLAY "Type ADD / DISPLAY / REMOVE / QUIT."
               ACCEPT INPUT1
               EVALUATE TRUE
                   WHEN INPUT1 EQUAL STRING-ADD
                       PERFORM 0100-DISPLAY-TASKS-START
                          THRU 0100-DISPLAY-TASKS-END
                       DISPLAY "Type a task number to add to this slot."
                       PERFORM 0200-GET-INDEX-START
                          THRU 0200-GET-INDEX-END
                       DISPLAY "Enter the new task."
                       ACCEPT NEW-VALUE
                       PERFORM 0300-SET-VALUE-START
                          THRU 0300-SET-VALUE-END

                   WHEN INPUT1 EQUAL STRING-DISPLAY
                       PERFORM 0100-DISPLAY-TASKS-START
                          THRU 0100-DISPLAY-TASKS-END

                   WHEN INPUT1 EQUAL STRING-REMOVE
                       PERFORM 0100-DISPLAY-TASKS-START
                          THRU 0100-DISPLAY-TASKS-END
                       DISPLAY "Type a task number to remove it."
                       PERFORM 0200-GET-INDEX-START
                          THRU 0200-GET-INDEX-END
                       MOVE " " TO NEW-VALUE
                       PERFORM 0300-SET-VALUE-START
                          THRU 0300-SET-VALUE-END
                       
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
           
       0100-DISPLAY-TASKS-START.
           DISPLAY "0 - " TASK0.
           DISPLAY "1 - " TASK1.
           DISPLAY "2 - " TASK2.
           DISPLAY "3 - " TASK3.
           DISPLAY "4 - " TASK4.
       0100-DISPLAY-TASKS-END.

       0200-GET-INDEX-START.
           ACCEPT INPUT-INDEX.
           IF INPUT-INDEX > 4 THEN
               MOVE 0 TO INPUT-INDEX
           END-IF.
       0200-GET-INDEX-END.

       0300-SET-VALUE-START.
           EVALUATE TRUE
               WHEN INPUT-INDEX EQUAL 0
                   MOVE NEW-VALUE TO TASK0
               WHEN INPUT-INDEX EQUAL 1
                   MOVE NEW-VALUE TO TASK1
               WHEN INPUT-INDEX EQUAL 2
                   MOVE NEW-VALUE TO TASK2
               WHEN INPUT-INDEX EQUAL 3
                   MOVE NEW-VALUE TO TASK3
               WHEN INPUT-INDEX EQUAL 4
                   MOVE NEW-VALUE TO TASK4
           END-EVALUATE.
       0300-SET-VALUE-END.
