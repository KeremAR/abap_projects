*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV09_CLDEF
*&---------------------------------------------------------------------*


CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: app TYPE REF TO lcl_main.
    CLASS-METHODS:
      app_instance RETURNING VALUE(go_main) TYPE REF TO lcl_main,
      load_of_program,
      initialization,
      at_selection_screen_output,
      at_selection_screen,
      start_of_selection,
      end_of_selection,
      file.

    METHODS:
      get_data,
      excel_to_alv,
      kaydet,
      excel_create,
      bapi_on,
      bapi_gercek.
ENDCLASS.

CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      set_layout,
      set_fc,
      display_alv.


ENDCLASS.
