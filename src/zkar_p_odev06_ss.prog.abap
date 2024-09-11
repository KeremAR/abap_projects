*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV06_SS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tarih  TYPE datum default sy-datum MODIF ID gr1 OBLIGATORY.


SELECTION-SCREEN END OF BLOCK selection.
SELECTION-SCREEN PUSHBUTTON 1(25) but1 USER-COMMAND but1.
