*&---------------------------------------------------------------------*
*& Report ZKA_P_ODEV01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zka_p_odev01.

DATA: gv_kenarcm TYPE i,
      gv_alan    TYPE i,
      gv_cevre   TYPE i.


SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_kare   AS CHECKBOX,
            p_dikdrt AS CHECKBOX,
            p_ucgen  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK blk01.

SELECTION-SCREEN BEGIN OF BLOCK blk02 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_alan  RADIOBUTTON GROUP gr1,
            p_cevre RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK blk02.

PARAMETERS: p_num1 TYPE i.


START-OF-SELECTION.

  BREAK-POINT.
  gv_kenarcm = p_num1.

  IF p_kare = 'X'.
    IF p_alan = 'X'.
      gv_alan = gv_kenarcm ** 2.
*      WRITE: / 'Şekil alan:', / 'Kare', gv_alan.
    ELSEIF p_cevre = 'X'.
      gv_cevre = 4 * gv_kenarcm.
      WRITE: / 'Şekil çevre:', / 'Kare', gv_cevre.
    ENDIF.
  ELSEIF p_dikdrt = 'X'.
    IF p_alan = 'X'.
      gv_alan = gv_kenarcm * gv_kenarcm.
      WRITE: / 'Şekil alan:', / 'Dikdörtgen', gv_alan.
    ELSEIF p_cevre = 'X'.
      gv_cevre = 4 * gv_kenarcm.
      WRITE: / 'Şekil çevre:', / 'Dikdörtgen', gv_cevre.
    ENDIF.
  ELSEIF p_ucgen = 'X'.
    IF p_alan = 'X'.
      gv_alan =  gv_kenarcm ** 2 / 2.
      WRITE: / 'Şekil alan:', / 'Üçgen', gv_alan.
    ELSEIF p_cevre = 'X'.
      gv_cevre = 3 * gv_kenarcm.
      WRITE: / 'Şekil çevre:', / 'Üçgen', gv_cevre.
    ENDIF.
  ELSE.
    WRITE: / 'Lütfen bir şekil seçin.'.
  ENDIF.



END-OF-SELECTION.
  BREAK-POINT.

  WRITE: / 'Şekil alan:', / 'Kare', gv_alan.
