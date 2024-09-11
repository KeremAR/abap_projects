*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV08_BUTTON2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev08_button1.

TABLES: zkar_t_malzeme2.

DATA: gt_list    TYPE TABLE OF  zkar_t_malzeme2,
      lv_malzeme TYPE  zkar_t_malzeme2,
      gs_list    LIKE LINE OF  gt_list.

DATA: gv_malzemeid   TYPE zkar_t_malzeme2-malzeme_id,
      gv_malzemedesc TYPE zkar_t_malzeme2-malzeme_desc,
      gv_malzemetur  TYPE zkar_t_malzeme2-malzeme_tur,
      gv_malgrup     TYPE zkar_t_malzeme2-mal_grup,
      gv_olcubirim   TYPE zkar_t_malzeme2-olcu_birim,
      gv_parabirim   TYPE zkar_t_malzeme2-para_birimi,
      gv_fiyat       TYPE zkar_t_malzeme2-fiyat,
      gv_malzemenum  TYPE zkar_t_malzeme2-malzeme_num,
      gv_toplam      TYPE zkar_t_malzeme2-toplam.

DATA: selected_malid  TYPE matnr,
      lv_padded_value TYPE char18.

DATA(gc_sinav) = NEW zkar_cl_odev8( ).




START-OF-SELECTION.

  gc_sinav->satin_alma_girisi( ).
  CALL SCREEN 0200.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0100'.
* SET TITLEBAR 'xxx'.
ENDMODULE.


MODULE check_matnr INPUT.

  lv_padded_value = condense( gv_malzemeid ).
  lv_padded_value = |{ lv_padded_value ALPHA = IN }|.

  SELECT SINGLE
         mara~meins AS olcu_birim,
         mara~mtart AS malzeme_tur,
         mara~matkl AS mal_grup,
         makt~maktx AS malzeme_desc
    INTO (@lv_malzeme-olcu_birim,
          @lv_malzeme-malzeme_tur,
          @lv_malzeme-mal_grup,
          @lv_malzeme-malzeme_desc)
    FROM mara
    INNER JOIN makt ON makt~matnr EQ mara~matnr
   WHERE mara~matnr EQ @lv_padded_value.

  gv_olcubirim   = lv_malzeme-olcu_birim.
  gv_malzemetur  = lv_malzeme-malzeme_tur.
  gv_malgrup     = lv_malzeme-mal_grup.
  gv_malzemedesc = lv_malzeme-malzeme_desc.
  gv_parabirim = 'TRY'.

  gv_toplam = gv_fiyat * gv_malzemenum.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&KAYDET'.
      gc_sinav->kaydet(
        EXPORTING
          iv_malzeme_id   =   gv_malzemeid
          iv_malzeme_desc =   gv_malzemedesc
          iv_malzeme_num  =   gv_malzemenum
          iv_olcu_birim   =   gv_olcubirim
          iv_malzeme_tur  =   gv_malzemetur
          iv_mal_grup     =   gv_malgrup
          iv_fiyat        =   gv_fiyat
          iv_para_birimi  =   gv_parabirim
          iv_toplam       =   gv_toplam
      ).
    WHEN '&TEMIZLE'.
      gc_sinav->temizle( ).

    WHEN '&RAPOR'.
      gc_sinav->rapor( ).
      CALL SCREEN 0300.
    WHEN '&ONAY'.
      gc_sinav->onaya_gonder( ).
  ENDCASE.
ENDMODULE.

MODULE status_0300 OUTPUT.
  SET PF-STATUS '0300'.
* SET TITLEBAR 'xxx'.

ENDMODULE.


MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
