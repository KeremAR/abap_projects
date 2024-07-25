*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV05_SS
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs  TYPE t001-bukrs DEFAULT 'MR01' MODIF ID gr1 OBLIGATORY,
            p_gjahr   TYPE gjahr DEFAULT '2020' MODIF ID gr1 obligatory,
            p_monat TYPE monat DEFAULT sy-datum+4(2) MODIF ID gr1. "OBLIGATORY.
SELECT-OPTIONS:s_hbkid FOR gs_t012-hbkid MODIF ID gr1,
                s_waers FOR gs_t012k-waers MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK selection.
