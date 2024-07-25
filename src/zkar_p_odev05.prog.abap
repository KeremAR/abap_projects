*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV05
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev05.

INCLUDE zkar_p_odev05_top.
INCLUDE zkar_p_odev05_pbo.
INCLUDE zkar_p_odev05_pai.
INCLUDE zkar_p_odev05_ss.
INCLUDE zkar_p_odev05_frm.



START-OF-SELECTION.
  PERFORM get_data.
  PERFORM set_fcat.
  PERFORM set_layout.

  CALL SCREEN 0100.
