*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev06.

INCLUDE zkar_p_odev06_top.
INCLUDE zkar_p_odev06_ss.
INCLUDE zkar_p_odev06_class.
INCLUDE zkar_p_odev06_pbo.
INCLUDE zkar_p_odev06_pai.


INITIALIZATION.
  CONCATENATE icon_mail 'Kullanicilar' INTO but1 SEPARATED BY space.

AT SELECTION-SCREEN.

  lv_date = p_tarih.
  lv_temp_date = lv_date.

  " Haftanın gününü kontrol et
  CALL FUNCTION 'DAY_IN_WEEK'
    EXPORTING
      datum = lv_temp_date
    IMPORTING
      wotnr = lv_day_of_week.

  " lv_day_of_week 1 = Pazartesi, 7 = Pazar
  IF lv_day_of_week = 6. " Cumartesi
    lv_temp_date = lv_temp_date - 1. " Cuma gününe ayarla
  ELSEIF lv_day_of_week = 7. " Pazar
    lv_temp_date = lv_temp_date - 2. " Cuma gününe ayarla
  ENDIF.

  lv_date = lv_temp_date.

* Assign date parts based on the adjusted date
  lv_yil_ay = lv_date+0(6).
  lv_gun = |{ lv_date+6(2) }{ lv_date+4(2) }{ lv_date+0(4) }| .
  lv_url = |{ base_url }{ lv_yil_ay }/{ lv_gun }.xml|.

  MESSAGE lv_url TYPE 'S'.

  CASE sy-ucomm.
    WHEN 'BUT1'.
       CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'zkar_mail_odev06'.

  ENDCASE.

START-OF-SELECTION.

 DATA:lo_example TYPE REF TO cl_fetch_tcmb.
  CREATE OBJECT lo_example.
  lo_example->fetch( ).
  CALL  SCREEN 0100.

END-OF-SELECTION.

IF sy-batch EQ 'X'.
    p_tarih = sy-datum.
    PERFORM send_emails.
    endif.
