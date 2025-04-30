       IDENTIFICATION DIVISION.
       PROGRAM-ID. testable.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 30-04-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TASK-TABLE.
           05 TASK-A           PIC X(50) OCCURS 1 TO 5 TIMES
           DEPENDING ON TABLE-SIZE.
       01 TABLE-SIZE           PIC 9(1).

      * This is a very interesting program for anyone wondering how
      * (dynamic) tables work in COBOL (at least GnuCOBOL) internally.
      * Each step is designed in a way that uncovers a new internal
      * property of tables.
       PROCEDURE DIVISION.

      * Does writing into a table with an unitialized TABLE-SIZE
      * create an issue?
           DISPLAY "Step 1".
           MOVE "THIS IS I" TO TASK-A(2).
           DISPLAY TASK-TABLE.
      * No, but the DISPLAY doesn't show anything.

      * Does changing the TABLE-SIZE during the program works?
      * Can you then assign a value to a table(index)?
           DISPLAY "Step 2".
           MOVE 2 TO TABLE-SIZE.
           MOVE "THIS IS I" TO TASK-A(2).
           DISPLAY TASK-TABLE.
      * Yes, changing the size dynamically works.
      * You can then indeed assigne a value to a table(index).

      * What happens if I change the size back to something smaller than
      * the furthest it stored something?
      * Can I continue assigning values afterwards?
           DISPLAY "Step 3".
           MOVE 1 TO TABLE-SIZE.
           DISPLAY TASK-TABLE.
           MOVE "THIS IS I" TO TASK-A(1).
           DISPLAY TASK-TABLE.
      * It just doesn't show the out of bounds value anymore
      * Yes, assigning values still work

      * What if I go back to a bigger size? Do previously set values
      * reappear?
           DISPLAY "Step 4".
           MOVE 2 TO TABLE-SIZE.
           DISPLAY TASK-TABLE.
      * Yes they do! It appears they are stored whether the current size
      * of the table is big enough or not

      * Ok, what happens if I set a size bigger than the limit set on
      * line 10? (OCCURS 1 TO 5 TIMES)
           DISPLAY "Step 5".
           MOVE "THIS IS I" TO TASK-A(4).
           MOVE 6 TO TABLE-SIZE.
           DISPLAY TASK-TABLE.
      * That's odd.. For some reason it doesn't crash, but an other line
      * is written, with the new size of the table

      * Does the position of the size in the display change depending
      * on said size? 
           DISPLAY "Step 6".
           MOVE 7 TO TABLE-SIZE.
           DISPLAY TASK-TABLE.
      * Is doesn't appear to, as they look aligned with one another

           DISPLAY "END".
           STOP RUN.
      * Conclusion :
      * These tests show that the memory is allocated depending on the
      * limits set on line 10, as the values are not removed even if the
      * size is set to a smaller number than the index.
      * So if memory optimization is a concern, keep that in mind.
