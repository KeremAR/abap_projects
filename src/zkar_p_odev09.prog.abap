*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV09
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev09.

INCLUDE zkar_p_odev09_cldef.
INCLUDE zkar_p_odev09_top.
INCLUDE zkar_p_odev09_ss.
INCLUDE zkar_p_odev09_pbo.
INCLUDE zkar_p_odev09_pai.
INCLUDE zkar_p_odev09_climp.

LOAD-OF-PROGRAM.
  lcl_main=>load_of_program( ).

INITIALIZATION.

  lcl_main=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_main=>at_selection_screen_output( ).

AT SELECTION-SCREEN.
  lcl_main=>at_selection_screen( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_main=>file( ).

START-OF-SELECTION.
  lcl_main=>start_of_selection( ).

END-OF-SELECTION.
  lcl_main=>end_of_selection( ).
