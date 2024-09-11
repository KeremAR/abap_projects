*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV07
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev07.



DATA(gc_plan) = new zkar_cl_odev7( ).

START-OF-SELECTION.



CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
* SET TITLEBAR 'xxx'.

ENDMODULE.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.

    WHEN '&BAKIM1'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZKAR_T_personel'
        EXCEPTIONS
          OTHERS    = 1.

    WHEN '&BAKIM2'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZKAR_T_SIRKET'
        EXCEPTIONS
          OTHERS    = 1.
  ENDCASE.
ENDMODULE.
