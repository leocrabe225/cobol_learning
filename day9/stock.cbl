       IDENTIFICATION DIVISION.
       PROGRAM-ID. stock.
       AUTHOR. Leocrabe225.
       DATE-WRITTEN. 05-05-2025 (fr).
       DATE-COMPILED. null.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PRODUCT-TABLE.
          05 PRODUCT           OCCURS 4 TIMES.
             10 PRODUCT-NAME   PIC X(20).
             10 PRODUCT-UNIT-PRICE PIC 9(2).
             10 PRODUCT-SOLD   PIC 9(2).

       01 TOTAL                PIC 9(6).
       01 PRODUCT-TOTAL        PIC 9(5).
       01 IDX-1                PIC 9.
       PROCEDURE DIVISION.
           
           PERFORM VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 > 4
               DISPLAY "Enter the product's name : " 
                   WITH NO ADVANCING
               ACCEPT PRODUCT-NAME(IDX-1)
               DISPLAY "Enter the product's price : "
                   WITH NO ADVANCING
               ACCEPT PRODUCT-UNIT-PRICE(IDX-1)
               DISPLAY "Enter the amount sold : "
                   WITH NO ADVANCING 
               ACCEPT PRODUCT-SOLD(IDX-1)
           END-PERFORM.

           MOVE 0 TO TOTAL.
           PERFORM VARYING IDX-1 FROM 1 BY 1 UNTIL IDX-1 > 4
               MULTIPLY PRODUCT-SOLD(IDX-1) BY PRODUCT-UNIT-PRICE(IDX-1)
                   GIVING PRODUCT-TOTAL
               ADD PRODUCT-TOTAL TO TOTAL 
               DISPLAY PRODUCT-NAME(IDX-1) " were sold "
                   PRODUCT-SOLD(IDX-1) " times at "
                   PRODUCT-UNIT-PRICE(IDX-1) " for a total of $"
                   PRODUCT-TOTAL "."
           END-PERFORM.
           DISPLAY "$" TOTAL " worth of goods were sold this week."
           STOP RUN.
