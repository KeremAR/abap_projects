*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV04_SS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
PARAMETERS: p_skod TYPE bkpf-bukrs DEFAULT '1101' MODIF ID gr1 OBLIGATORY,
            p_myil TYPE bkpf-gjahr DEFAULT '2019' MODIF ID gr1 OBLIGATORY.
SELECT-OPTIONS: so_donem FOR gs_bkpf-monat MODIF ID gr1,
                so_satci FOR gs_bseg-lifnr MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK selection.

SELECTION-SCREEN BEGIN OF BLOCK selection2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN PUSHBUTTON 1(25) but1 USER-COMMAND but1.
SELECTION-SCREEN PUSHBUTTON 30(25) but2 USER-COMMAND but2.
SELECTION-SCREEN END OF BLOCK selection2.
