       IDENTIFICATION DIVISION.
       PROGRAM-ID. tabltodo.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 30-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Constants strings
       01 STRING-ADD           PIC X(3) VALUE "ADD".
       01 STRING-INSERT        PIC X(6) VALUE "INSERT".
       01 STRING-REPLACE       PIC X(7) VALUE "REPLACE".
       01 STRING-DISPLAY       PIC X(7) VALUE "DISPLAY".
       01 STRING-REMOVE        PIC X(6) VALUE "REMOVE".
       01 STRING-QUIT          PIC X(4) VALUE "QUIT".
       01 STRING-NO-TASK       PIC X(50)
           VALUE "There are no tasks in the list.".
       01 STRING-MAX-SIZE PIC X(50) VALUE "The list cannot be longer".

      * Inputs
       01 INPUT1               PIC X(50) VALUE SPACE.
       01 INPUT-INDEX          PIC 9(2).
       01 NEW-VALUE            PIC X(50).

       01 TASK-TABLE.
           05 TASK-A           PIC X(50) OCCURS 1 TO 99 TIMES
           DEPENDING ON TABLE-SIZE.
           
       01 MAX-TABLE-SIZE       PIC 9(2) VALUE 5.
       01 TABLE-SIZE           PIC 9(2) VALUE ZERO.
       01 ITERATOR             PIC 9(3).
       01 ITERATOR2            PIC 9(3).
       PROCEDURE DIVISION.
      * Main loop
           PERFORM UNTIL INPUT1 EQUAL STRING-QUIT
      * Checking whether the table is empty, to remove unaccessible
      * commands
               IF TABLE-SIZE NOT EQUAL ZERO THEN
                   DISPLAY "Type " STRING-ADD " / "
                                   STRING-INSERT " / "
                                   STRING-REPLACE " / "
                                   STRING-DISPLAY " / "
                                   STRING-REMOVE " / "
                                   STRING-QUIT "."
               ELSE
                   DISPLAY "Type " STRING-ADD " / "
                                   STRING-QUIT "."
               END-IF

               ACCEPT INPUT1
      
      * Evaluate to check for valid commands
               EVALUATE INPUT1
      * ADD command, appends a task to the top of the list.
                   WHEN STRING-ADD
                       IF TABLE-SIZE < MAX-TABLE-SIZE THEN
                         ADD 1 TO TABLE-SIZE
                         MOVE TABLE-SIZE TO INPUT-INDEX
                         DISPLAY "Type your new task"
                         ACCEPT NEW-VALUE
                         PERFORM 0300-SET-VALUE-START
                            THRU 0300-SET-VALUE-END
                       ELSE
                         DISPLAY STRING-MAX-SIZE
                       END-IF
      * INSERT command, inserts a task at a given index in the list.
                   WHEN STRING-INSERT
                       IF TABLE-SIZE NOT EQUAL 0 THEN
                         IF TABLE-SIZE < MAX-TABLE-SIZE
                           PERFORM 0100-DISPLAY-TASKS-START
                              THRU 0100-DISPLAY-TASKS-END
                           DISPLAY 
                               "Type a task number to insert before it."
                           PERFORM 0200-GET-INDEX-START
                              THRU 0200-GET-INDEX-END
                           DISPLAY "Type your new task"
                           ACCEPT NEW-VALUE
                           PERFORM 0500-SHIFT-TASKS-UP-START
                              THRU 0500-SHIFT-TASKS-UP-END
                           PERFORM 0300-SET-VALUE-START
                              THRU 0300-SET-VALUE-END
                         ELSE
                           DISPLAY STRING-MAX-SIZE
                         END-IF
                       ELSE
                         DISPLAY STRING-NO-TASK
                       END-IF

      * REPLACE command, replaces the task at a given index with a new
      * task.
                   WHEN STRING-REPLACE
                       IF TABLE-SIZE NOT EQUAL 0 THEN
                         PERFORM 0100-DISPLAY-TASKS-START
                            THRU 0100-DISPLAY-TASKS-END
                         DISPLAY 
                             "Type a task number to replace this slot."
                         PERFORM 0200-GET-INDEX-START
                            THRU 0200-GET-INDEX-END
                         DISPLAY "Enter the new task."
                         ACCEPT NEW-VALUE
                         PERFORM 0300-SET-VALUE-START
                            THRU 0300-SET-VALUE-END
                       ELSE
                         DISPLAY STRING-NO-TASK
                       END-IF

      * DISPLAY command, displays all tasks in the list.
                   WHEN STRING-DISPLAY
                       IF TABLE-SIZE NOT EQUAL 0 THEN 
                         PERFORM 0100-DISPLAY-TASKS-START
                            THRU 0100-DISPLAY-TASKS-END
                       ELSE
                         DISPLAY STRING-NO-TASK
                       END-IF

      * REMOVE command, removes the task at a given index, and collapses
      * the list so there is no empty space
                   WHEN STRING-REMOVE
                       IF TABLE-SIZE NOT EQUAL 0 THEN 
                         PERFORM 0100-DISPLAY-TASKS-START
                            THRU 0100-DISPLAY-TASKS-END
                         DISPLAY "Type a task number to remove it."
                         PERFORM 0200-GET-INDEX-START
                            THRU 0200-GET-INDEX-END
                         MOVE " " TO NEW-VALUE
                         PERFORM 0300-SET-VALUE-START
                            THRU 0300-SET-VALUE-END
                         PERFORM 0400-SHIFT-TASKS-DOWN-START
                            THRU 0400-SHIFT-TASKS-DOWN-END
                       ELSE
                         DISPLAY STRING-NO-TASK
                       END-IF
                       
               END-EVALUATE
           END-PERFORM.
           STOP RUN.
           
      * Displays all tasks with the index in front.
       0100-DISPLAY-TASKS-START.
           PERFORM VARYING ITERATOR FROM 1 BY 1
                   UNTIL ITERATOR > TABLE-SIZE
               DISPLAY ITERATOR " - " TASK-A(ITERATOR)
           END-PERFORM.
       0100-DISPLAY-TASKS-END.

      * Accepts an index from the user, and then checks whether it fits
      * the current bounds of the task table.
       0200-GET-INDEX-START.
           ACCEPT INPUT-INDEX.
           PERFORM UNTIL
               INPUT-INDEX NOT > TABLE-SIZE AND
               INPUT-INDEX NOT EQUAL 0
               DISPLAY "Not in the accepted range, try again."
               ACCEPT INPUT-INDEX
           END-PERFORM.
       0200-GET-INDEX-END.

      * Moves the new task to the table, can also be used with an empty
      * task to act as a removal
       0300-SET-VALUE-START.
           MOVE NEW-VALUE TO TASK-A(INPUT-INDEX).
       0300-SET-VALUE-END.

      * Shift all tasks down by one in the table, used to close the gap
      * left by the removal of a task.
       0400-SHIFT-TASKS-DOWN-START.
           MOVE INPUT-INDEX TO ITERATOR.
           ADD 1 TO ITERATOR.
           PERFORM UNTIL ITERATOR > TABLE-SIZE
               MOVE TASK-A(ITERATOR) TO TASK-A(INPUT-INDEX)
               ADD 1 TO INPUT-INDEX
               ADD 1 TO ITERATOR
           END-PERFORM.
           SUBTRACT 1 FROM TABLE-SIZE.
       0400-SHIFT-TASKS-DOWN-END.

      * Shifts all tasks up by one in the table, used to leave room for
      * the insertion of a new task.
       0500-SHIFT-TASKS-UP-START.
           ADD 1 TO TABLE-SIZE.
           MOVE TABLE-SIZE TO ITERATOR.
           MOVE TABLE-SIZE TO ITERATOR2.
           SUBTRACT 1 FROM ITERATOR2.
           PERFORM UNTIL ITERATOR EQUAL INPUT-INDEX
               MOVE TASK-A(ITERATOR2) TO TASK-A(ITERATOR)
               SUBTRACT 1 FROM ITERATOR
               SUBTRACT 1 FROM ITERATOR2
           END-PERFORM.
       0500-SHIFT-TASKS-UP-END.
