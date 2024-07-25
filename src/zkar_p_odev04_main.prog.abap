*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV04_MAIN
*&---------------------------------------------------------------------*

INITIALIZATION.
*  but1 = 'tabloyu goruntule'.
*  but2 = 'tabloyu degistir'.


  CONCATENATE icon_display ' Tabloyu Goruntule' INTO but1 SEPARATED BY space.
  CONCATENATE icon_print ' Tabloyu Degistir' INTO but2 SEPARATED BY space.


AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BUT1'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'S'
          view_name = 'zkar_odev4_bakim'.

    WHEN 'BUT2'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'zkar_odev4_bakim'.

  ENDCASE.


 START-OF-SELECTION.
  PERFORM get_data.
  PERFORM set_fc.
  PERFORM set_layout.
  PERFORM display_alv.
