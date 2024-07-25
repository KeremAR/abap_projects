REPORT zkar_p_odev03_part2.

TABLES: zkar_kitap_t,
        zkar_ogrenci_t,
        zkar_islem_t.

TYPE-POOLS: vrm.

TYPES: BEGIN OF gty_list,
         islemno    TYPE zkar_islemno_de,
         ogrno      TYPE zkar_ogrno_de,
         kitapno    TYPE zkar_kitapno_de,
         atarih     TYPE zkar_atarih_de,
         vtarih     TYPE zkar_vtarih_de,
         ograd      TYPE zkar_ograd_de,
         ogrsoyad   TYPE zkar_ogrsoyad_de,
         kitapad    TYPE zkar_kitapad_de,
         puan       TYPE zkar_puan_de,
         yazarsoyad TYPE zkar_yazarad_de,
         yazarno    TYPE zkar_yazarno_de,
         yazarad    TYPE zkar_yazarsoyad_de,
         turno      TYPE zkar_turno_de,
         turad      TYPE zkar_turad_de,
         line_color TYPE char4,
         lights     TYPE c,
         ogrfname   TYPE string,
         yazarfname TYPE string,
         selkz      TYPE char1,

       END OF gty_list.

DATA: gt_list TYPE TABLE OF gty_list,
      gs_list TYPE gty_list.

DATA: it_makt TYPE STANDARD TABLE OF gty_list,
      wa_makt TYPE gty_list.

DATA: g_id          TYPE vrm_id,
      it_values_ktp TYPE vrm_values,
      it_values_ogr TYPE vrm_values,
      wa_values_ogr LIKE LINE OF it_values_ogr,
      wa_values_ktp LIKE LINE OF it_values_ktp.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gs_fieldcatalog LIKE LINE OF gt_fieldcatalog.

DATA: gs_layout TYPE lvc_s_layo.

DATA: gv_islemno     TYPE zkar_islem_t-islemno,
      gv_ogrencino   TYPE zkar_islem_t-ogrno,
      gv_kitapno     TYPE zkar_islem_t-kitapno,
      gv_teslimtarih TYPE zkar_islem_t-atarih,
      gv_alimtarih   TYPE zkar_islem_t-vtarih,
      gv_puan        TYPE zkar_ogrenci_t-puan.

DATA: gs_log TYPE zkar_islem_t.

DATA: gt_ogrenci TYPE TABLE OF zkar_ogrenci_t,
      gt_islem   TYPE TABLE OF zkar_islem_t,
      gt_ozet    TYPE TABLE OF zkar_ozet_t,
      gs_ozet    TYPE zkar_ozet_t,
      go_salv    TYPE REF TO cl_salv_table,
      lv_today   TYPE sy-datum,
      lo_columns TYPE REF TO cl_salv_columns_table,
      lo_column  TYPE REF TO cl_salv_column_table.

lv_today = sy-datum.

SELECT * FROM zkar_ogrenci_t INTO TABLE gt_ogrenci.
SELECT * FROM zkar_islem_t INTO TABLE gt_islem.
LOOP AT gt_ogrenci INTO DATA(gs_ogrenci).
  CLEAR gs_ozet.

  gs_ozet-ogrno = gs_ogrenci-ogrno.
  gs_ozet-ograd = gs_ogrenci-ograd.
  gs_ozet-ogrsoyad = gs_ogrenci-ogrsoyad.

  LOOP AT gt_islem INTO DATA(gs_islem) WHERE ogrno = gs_ogrenci-ogrno.
    ADD 1 TO gs_ozet-toplamkitap.

    IF gs_islem-vtarih IS INITIAL.
      ADD 1 TO gs_ozet-kalankitap.
      IF lv_today - gs_islem-atarih > 15.
        ADD 1 TO gs_ozet-gecikenkitap.
      ENDIF.
    ELSE.
      ADD 1 TO gs_ozet-teslimkitap.
    ENDIF.
  ENDLOOP.
  APPEND gs_ozet TO gt_ozet.
ENDLOOP.
DELETE gt_ozet WHERE mandt IS NOT INITIAL.

* Display the summary using ALV
cl_salv_table=>factory(
  IMPORTING
    r_salv_table = go_salv
  CHANGING
    t_table      = gt_ozet
).
LOOP AT gt_ozet INTO gs_ozet.
  CONCATENATE gs_ozet-ograd gs_ozet-ogrsoyad INTO gs_ozet-ograd SEPARATED BY ' '.
  MODIFY gt_ozet FROM gs_ozet.
ENDLOOP.
*  * Change column headers
lo_columns = go_salv->get_columns( ).
lo_columns->set_optimize( abap_true ).

lo_column ?= lo_columns->get_column( 'MANDT' ).
lo_column->set_visible( abap_false ).

lo_column ?= lo_columns->get_column( 'OGRSOYAD' ).
lo_column->set_visible( abap_false ).


lo_column ?= lo_columns->get_column( 'OGRNO' ).
lo_column->set_short_text( |Ogrenci No| ).
lo_column->set_medium_text( |Ogrenci No| ).
lo_column->set_long_text( |Ogrenci No| ).

lo_column ?= lo_columns->get_column( 'OGRAD' ).
lo_column->set_short_text( |Ad| ).
lo_column->set_medium_text( |Ad| ).
lo_column->set_long_text( |Ad| ).
lo_column->set_optimized( abap_true ).

DATA: color_red TYPE lvc_s_colo.
color_red-col = '6'.
color_red-int = '1'.
color_red-inv = '0'.

DATA: color_yellow TYPE lvc_s_colo.
color_yellow-col = '3'.
color_yellow-int = '1'.
color_yellow-inv = '0'.

DATA: color_green TYPE lvc_s_colo.
color_green-col = '5'.
color_green-int = '1'.
color_green-inv = '0'.



lo_column ?= lo_columns->get_column( 'TOPLAMKITAP' ).
lo_column->set_short_text( |Toplam Kitap| ).
lo_column->set_medium_text( |Toplam Kitap| ).
lo_column->set_long_text( |Toplam Kitap| ).

lo_column ?= lo_columns->get_column( 'KALANKITAP' ).
lo_column->set_short_text( |Kalan Kitap| ).
lo_column->set_medium_text( |Kalan Kitap| ).
lo_column->set_long_text( |Kalan Kitap| ).
lo_column->set_color( color_yellow ).

lo_column ?= lo_columns->get_column( 'GECIKENKITAP' ).
lo_column->set_short_text( |Geciken kitap| ).
lo_column->set_medium_text( |Geciken kitap| ).
lo_column->set_long_text( |Geciken kitap| ).
lo_column->set_color( color_red ).

lo_column ?= lo_columns->get_column( 'TESLIMKITAP' ).
lo_column->set_short_text( |Teslim kitap| ).
lo_column->set_medium_text( |Teslim kitap| ).
lo_column->set_long_text( |Teslim kitap| ).
lo_column->set_color( color_green ).



SELECTION-SCREEN BEGIN OF BLOCK blk_filter WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_ktpno FOR zkar_kitap_t-kitapno MODIF ID gr1,
                so_ogrno  FOR zkar_ogrenci_t-ogrno MATCHCODE OBJECT zkar_sh_ogrno  MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK blk_filter.

SELECTION-SCREEN BEGIN OF BLOCK blk_filter2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_kitapn TYPE zkar_kitap_t-kitapno AS LISTBOX VISIBLE LENGTH 30 MODIF ID gr2,
            p_ogrno  TYPE zkar_ogrenci_t-ogrno AS LISTBOX VISIBLE LENGTH 30 MODIF ID gr2.
SELECTION-SCREEN END OF BLOCK blk_filter2.

AT SELECTION-SCREEN OUTPUT.


  PERFORM get_islem_num.

  CLEAR: it_values_ktp, it_values_ogr,  wa_values_ktp, wa_values_ogr.

  SELECT kitapno
    FROM zkar_kitap_t
    INTO CORRESPONDING FIELDS OF TABLE it_makt.


  LOOP AT it_makt INTO wa_makt.
    wa_values_ktp-key = wa_makt-kitapno.
    wa_values_ktp-text = wa_makt-kitapad.
    APPEND wa_values_ktp TO it_values_ktp.
    CLEAR wa_values_ktp.
  ENDLOOP.

  g_id = 'P_KITAPN'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_id
      values = it_values_ktp
*       EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  SELECT ogrno
    FROM zkar_ogrenci_t
    INTO CORRESPONDING FIELDS OF TABLE it_makt.

  LOOP AT it_makt INTO wa_makt.
    wa_values_ogr-key = wa_makt-ogrno.
    wa_values_ogr-text = wa_makt-ograd.
    APPEND wa_values_ogr TO it_values_ogr.
    CLEAR wa_values_ogr.
  ENDLOOP.

  g_id = 'P_OGRNO'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_id
      values = it_values_ogr
*       EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.




*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .
  SELECT
         zkar_ogrenci_t~ograd
         zkar_ogrenci_t~ogrsoyad
         zkar_islem_t~islemno
         zkar_islem_t~ogrno
         zkar_islem_t~kitapno
         zkar_islem_t~atarih
         zkar_islem_t~vtarih
         zkar_ogrenci_t~puan
         zkar_kitap_t~kitapad
         zkar_yazar_t~yazarad
         zkar_yazar_t~yazarno
         zkar_yazar_t~yazarsoyad
         zkar_tur_t~turno
         zkar_tur_t~turad
     FROM zkar_islem_t
    INNER JOIN zkar_ogrenci_t ON zkar_ogrenci_t~ogrno EQ zkar_islem_t~ogrno
    INNER JOIN zkar_kitap_t ON zkar_kitap_t~kitapno EQ zkar_islem_t~kitapno
    INNER JOIN zkar_yazar_t ON zkar_yazar_t~yazarno EQ zkar_kitap_t~yazarno
    INNER JOIN zkar_tur_t ON zkar_tur_t~turno EQ zkar_kitap_t~turno
    INTO CORRESPONDING FIELDS OF TABLE gt_list.


  LOOP AT gt_list INTO gs_list.
    IF so_ktpno IS NOT INITIAL.
      IF gs_list-kitapno IN so_ktpno.
        CONTINUE.
      ELSE.
        DELETE gt_list INDEX sy-tabix.
      ENDIF.
    ENDIF.

    IF so_ogrno IS NOT INITIAL.
      IF gs_list-ogrno IN so_ogrno.
        CONTINUE.
      ELSE.
        DELETE gt_list INDEX sy-tabix.
      ENDIF.
    ENDIF.

    IF p_kitapn IS NOT INITIAL AND gs_list-kitapno NE p_kitapn.
      DELETE gt_list INDEX sy-tabix.
    ENDIF.
    IF p_ogrno IS NOT INITIAL AND gs_list-ogrno NE p_ogrno.
      DELETE gt_list INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout .
*  gs_layout-window_titlebar = 'REUSE ALV ODEV'.
**  gs_layout-zebra = abap_true.
*  gs_layout-colwidth_optimize = abap_true.
*  gs_layout-info_fieldname   = 'LINE_COLOR'.
*  gs_layout-lights_fieldname = 'lights'.
  gs_layout-box_fname = 'selkz'.
  gs_layout-excp_fname     = 'lights'.
  gs_layout-info_fname      = 'LINE_COLOR'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SUB_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcatalog
    TABLES
      t_outtab                 = gt_list.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FC_SUB
*&---------------------------------------------------------------------*
FORM set_fc_sub USING p_fieldname

                      p_coltext
                      p_key
                      p_col_pos
                      p_emphasize.
  CLEAR: gs_fieldcatalog.
  gs_fieldcatalog-fieldname = p_fieldname.
  gs_fieldcatalog-coltext = p_coltext.
  gs_fieldcatalog-key       = p_key.
  gs_fieldcatalog-col_pos   = p_col_pos.
  gs_fieldcatalog-emphasize = p_emphasize.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FC
*&---------------------------------------------------------------------*
FORM set_fc .
  LOOP AT gt_list INTO gs_list.
    CONCATENATE gs_list-ograd gs_list-ogrsoyad INTO gs_list-ogrfname SEPARATED BY ' '.
    MODIFY gt_list FROM gs_list.
  ENDLOOP.

  LOOP AT gt_list INTO gs_list.
    CONCATENATE gs_list-yazarad gs_list-yazarsoyad INTO gs_list-yazarfname SEPARATED BY ' '.
    MODIFY gt_list FROM gs_list.
  ENDLOOP.
  LOOP AT gt_list INTO gs_list.

    IF ( sy-datum - gs_list-atarih ) > 15 AND gs_list-vtarih IS INITIAL .         " 15 güngeçmiş ise ve kitap kabulu yapılmamış ise kırmızı
      gs_list-line_color = 'C600'.
      gs_list-lights = '1'.  "kirmizi
      MODIFY gt_list FROM gs_list.
    ELSEIF gs_list-vtarih IS INITIAL .
      gs_list-lights = '2'.  "sari
      MODIFY gt_list FROM gs_list.
    ELSE.
      gs_list-lights = '3'.  "yeşil
      MODIFY gt_list FROM gs_list.
    ENDIF.
  ENDLOOP.
  PERFORM:
           set_fc_sub USING 'lights' 'LIGHTS'   '' '0' '',
           set_fc_sub USING 'islemno' 'ISLEM NO'   '' '0' '',
           set_fc_sub USING 'OGRNO'   'OGR NO'   '' '2' '',
           set_fc_sub USING 'ogrfname'   'OGRENCI AD SOYAD'   '' '2' '',
           set_fc_sub USING 'KITAPno'  'KITAP NO'  '' '5' 'C311',
           set_fc_sub USING 'KITAPAD'  'KITAP AD'  '' '5' 'C300',
           set_fc_sub USING 'YAZARNO'  'YAZAR NO'  '' '5' 'C311',
           set_fc_sub USING 'yazarfname'   'YAZAR AD SOYAD'   '' '5' 'C300',
           set_fc_sub USING 'TURNO'   'TUR NO' '' '5' 'C311',
           set_fc_sub USING 'TURAD'  'TUR AD'  '' '5' 'C300',
           set_fc_sub USING 'ATARIH' 'ALIS TARIH'  '' '5' '',
           set_fc_sub USING 'VTARIH' 'VERIS TARIH'   '' '5' ''.



ENDFORM.


START-OF-SELECTION.
*  CALL SCREEN 0100.

  PERFORM get_data.
  PERFORM set_layout.
  PERFORM set_fc.
  PERFORM display_alv.

FORM sub_pf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'GUI_STATUS'.

ENDFORM.





**  *&---------------------------------------------------------------------*
**& Module STATUS_0100 OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100'.
  LOOP AT SCREEN.
    IF screen-name = 'GV_ISLEM_NO'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.

**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_0100  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      IF gv_teslimtarih > sy-datum.
        MESSAGE 'Kitap teslim tarihi bu günün tarihinden büyük olamaz' TYPE 'I'.
        RETURN.
      ELSE.

        IF  gv_teslimtarih IS INITIAL OR
             gv_kitapno IS INITIAL OR
             gv_ogrencino IS INITIAL.
          MESSAGE ' İslem Lütfen tüm parametreleri doldurunuz.' TYPE 'I'.
          RETURN.
        ENDIF.
      ENDIF.

      DATA: gv_ogrpuan TYPE zkar_ogrenci_t-puan.

      SELECT puan
        FROM zkar_ogrenci_t
        INTO @gv_ogrpuan
        WHERE ogrno EQ @gv_ogrencino.

        DATA: gv_kitpsayi TYPE i.
*              lv_today2    TYPE sy-datum.
*        lv_today2 = sy-datum.
        gv_kitpsayi = 0.

        SELECT SINGLE COUNT( * )
        FROM zkar_islem_t
        WHERE vtarih IS INITIAL AND
              ogrno EQ @gv_ogrencino
        INTO @gv_kitpsayi.
      ENDSELECT.

      IF gv_ogrpuan < 50.
        MESSAGE 'Öğrenci puanı 50 altında kitap alamaz.' TYPE 'I'.
        RETURN.
      ELSEIF gv_ogrpuan >= 50 AND gv_ogrpuan < 70.
        IF gv_kitpsayi >= 3.
          MESSAGE 'Ogrenci Puanı 70 altında 3 den fazla kitap alamaz.' TYPE 'I'.
          RETURN.
        ELSE.
          MESSAGE 'Islem Basarili' TYPE 'S'.
        ENDIF.
      ENDIF.



    CLEAR gs_log.
    gs_log-islemno = gv_islemno.
    gs_log-kitapno = gv_kitapno.
    gs_log-ogrno = gv_ogrencino.
    gs_log-atarih = gv_teslimtarih.

    DATA: temp_ograd      TYPE zkar_ograd_de,
          temp_ogrsoyad   TYPE zkar_ogrsoyad_de,
          temp_kitapno    TYPE zkar_kitapno_de,
          temp_kitapad    TYPE zkar_kitapad_de,
          temp_yazarno    TYPE zkar_yazarno_de,
          temp_yazarad    TYPE zkar_yazarad_de,
          temp_yazarsoyad TYPE zkar_yazarsoyad_de,
          temp_turno      TYPE zkar_turno_de,
          temp_turad      TYPE zkar_turad_de.

    SELECT SINGLE ograd, ogrsoyad
     FROM zkar_ogrenci_t
     WHERE ogrno = @gv_ogrencino
      INTO (@temp_ograd, @temp_ogrsoyad).
      CONCATENATE temp_ograd temp_ogrsoyad INTO temp_ograd SEPARATED BY space.

      SELECT SINGLE kitapad, yazarno, turno
      FROM zkar_kitap_t
      WHERE kitapno = @gv_kitapno
      INTO (@temp_kitapad, @temp_yazarno, @temp_turno).

        SELECT SINGLE yazarad, yazarsoyad, yazarno
        FROM zkar_yazar_t
        WHERE yazarno = @temp_yazarno
        INTO (@temp_yazarad, @temp_yazarsoyad, @temp_yazarno).
          CONCATENATE temp_yazarad temp_yazarsoyad INTO temp_yazarad SEPARATED BY ' '.

          SELECT SINGLE turno, turad
          FROM zkar_tur_t
          WHERE turno = @temp_turno
          INTO (@temp_turno, @temp_turad).

            CLEAR gs_list.
            IF sy-datum - gv_teslimtarih > 15.
              gs_list-line_color = 'C600'.
              gs_list-lights = 1.

            ELSE.
              gs_list-lights = 2.
            ENDIF.

            gs_list-islemno = gv_islemno.
            gs_list-atarih = gv_teslimtarih.
            gs_list-kitapno = gv_kitapno.
            gs_list-ogrno = gv_ogrencino.
            gs_list-ogrfname = temp_ograd.
            gs_list-kitapad = temp_kitapad.
            gs_list-yazarfname = temp_yazarad.
            gs_list-yazarno = temp_yazarno.
            gs_list-turno = temp_turno.
            gs_list-turad = temp_turad.
            INSERT zkar_islem_t FROM gs_log.
            APPEND gs_list TO gt_list.

*      COMMIT WORK AND WAIT.
            IF sy-subrc = 0.
              MESSAGE 'Veri başarıyla kaydedildi.' TYPE 'I'.
*        PERFORM refresh_alv.
              LEAVE TO SCREEN 0.
            ELSE.
              MESSAGE 'Veri kaydedilirken hata oluştu.' TYPE 'E'.
            ENDIF.


        ENDCASE.
ENDMODULE.



FORM user_command USING b_ucomm LIKE sy-ucomm
      rs_selfield TYPE slis_selfield.


  CASE b_ucomm.
    WHEN '&KITAPVER'.
      CALL SCREEN 0100 STARTING AT 10 10
                         ENDING AT 70 30.
      rs_selfield-refresh = abap_true.
    WHEN '&KITAPAL'.
      READ TABLE gt_list INTO gs_list WITH KEY selkz = 'X'.
      IF sy-subrc = 0.
        PERFORM fill_screen_fields USING rs_selfield-tabindex.
        CALL SCREEN 0200 STARTING AT 10 10
                           ENDING AT 70 30.
      ELSE.
        MESSAGE 'Geçerli bir satır seçin.' TYPE 'I'.
      ENDIF.
      sy-subrc = 0.
      rs_selfield-refresh = abap_true.
    WHEN '&SIL'.
      READ TABLE gt_list INTO gs_list WITH KEY selkz = 'X'.
      IF sy-subrc = 0.
        DATA: ans TYPE c.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Silme Onayı'
            text_question         = 'İşlem silinsin mi?'
            text_button_1         = 'Evet'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Hayır'
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ' '
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = ans.
        IF ans = '1'.
          LOOP AT gt_list INTO gs_list WHERE selkz = 'X'.
            DELETE gt_list WHERE islemno = gs_list-islemno.
            DELETE FROM zkar_islem_t WHERE islemno = gs_list-islemno.
*            IF sy-subrc = 0.
*
*
**              COMMIT WORK AND WAIT.
*              MESSAGE 'İşlem silindi.' TYPE 'I'.
**              PERFORM refresh_alv.
*            ELSE.
*              MESSAGE 'Veri silinirken hata oluştu: ' && sy-subrc TYPE 'E'.
*            ENDIF.
            MESSAGE 'Islem Silindi.' TYPE 'I'.
          ENDLOOP.
          rs_selfield-refresh = 'X'.
        ELSE.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE 'Seçilen işlem yok.' TYPE 'E'.
      ENDIF.
    WHEN '&GUNCELLE'.
      READ TABLE gt_list INTO gs_list WITH KEY selkz = 'X'.
      IF sy-subrc = 0.
        PERFORM fill_screen_fields2 USING rs_selfield-tabindex.
        CALL SCREEN 0400 STARTING AT 10 10
                           ENDING AT 70 30.
      ELSE.
        MESSAGE 'satir seciniz.' TYPE 'I'.
      ENDIF.
      rs_selfield-refresh = abap_true.

    WHEN '&RAPOR'.
      go_salv->display( ).
*      CALL SCREEN 0300 STARTING AT 10 10
*                         ENDING AT 70 30.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ISLEM_NUM
*&---------------------------------------------------------------------*
FORM get_islem_num.
  SELECT MAX( islemno )
    FROM zkar_islem_t
    INTO @DATA(max_islemno).

    IF max_islemno IS NOT INITIAL.
      gv_islemno = max_islemno + 1.
    ELSE.
      gv_islemno = 1.
    ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
*      UPDATE zkar_islem_t
*     SET kitapno = @gv_kitapno,
*         ogrno = @gv_ogrencino,
*         vtarih = @gv_alimtarih
*     WHERE islemno = @gv_islemno.

      UPDATE zkar_islem_t SET vtarih = gv_alimtarih
      WHERE islemno EQ gv_islemno.

      LOOP AT gt_list INTO gs_list WHERE islemno = gv_islemno.
        gs_list-vtarih = gv_alimtarih.
        gs_list-kitapno = gv_kitapno.
        gs_list-atarih = gv_teslimtarih.
        gs_list-ogrno = gv_ogrencino.
        MODIFY gt_list FROM gs_list TRANSPORTING: vtarih, lights, line_color WHERE islemno = gv_islemno.
      ENDLOOP.

*      COMMIT WORK AND WAIT.
      IF sy-subrc = 0.
        MESSAGE 'Veri başarıyla kaydedildi.' TYPE 'I'.
*        PERFORM refresh_alv.
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Veri kaydedilirken hata oluştu.' TYPE 'E'.
      ENDIF.
      .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0200'.
  SET TITLEBAR 'TITLE_0200'.
ENDMODULE.

FORM fill_screen_fields USING p_tabindex TYPE i.
  READ TABLE gt_list INDEX p_tabindex INTO gs_list.
  IF sy-subrc = 0.
    gv_islemno = gs_list-islemno.
    gv_kitapno = gs_list-kitapno.
    gv_ogrencino = gs_list-ogrno.
    gv_alimtarih = sy-datum.


  ENDIF.

ENDFORM.
FORM fill_screen_fields2 USING p_tabindex TYPE i.
  READ TABLE gt_list INDEX p_tabindex INTO gs_list.
  IF sy-subrc = 0.
    gv_islemno = gs_list-islemno.
    gv_kitapno = gs_list-kitapno.
    gv_ogrencino = gs_list-ogrno.
    gv_teslimtarih = gs_list-atarih.
    gv_alimtarih = gs_list-vtarih.


  ELSE.
    MESSAGE 'Tablodan veri okuma başarısız.' TYPE 'E'. " Tablodan veri okuma başarısızsa mesaj göster
  ENDIF.



ENDFORM.
*
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'STATUS_0400'.
  SET TITLEBAR 'TITLE_0400'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      UPDATE zkar_islem_t
     SET kitapno = @gv_kitapno,
         ogrno = @gv_ogrencino,
         atarih = @gv_teslimtarih,
         vtarih = @gv_alimtarih
     WHERE islemno = @gv_islemno.


      SELECT SINGLE ograd, ogrsoyad
        FROM zkar_ogrenci_t
        WHERE ogrno = @gv_ogrencino
        INTO (@temp_ograd, @temp_ogrsoyad).
        CONCATENATE temp_ograd temp_ogrsoyad INTO temp_ograd SEPARATED BY ' '.

        SELECT SINGLE kitapad, yazarno, turno
         FROM zkar_kitap_t
         WHERE kitapno = @gv_kitapno
         INTO (@temp_kitapad, @temp_yazarno, @temp_turno).

          SELECT SINGLE yazarad, yazarsoyad, yazarno
         FROM zkar_yazar_t
         WHERE yazarno = @temp_yazarno
         INTO (@temp_yazarad, @temp_yazarsoyad, @temp_yazarno).
            CONCATENATE temp_yazarad temp_yazarsoyad INTO temp_yazarad SEPARATED BY ' '.

            SELECT SINGLE turno, turad
                    FROM zkar_tur_t
                    WHERE turno = @temp_turno
                    INTO (@temp_turno, @temp_turad).

              LOOP AT gt_list INTO gs_list WHERE islemno = gv_islemno.
                gs_list-vtarih = gv_alimtarih.
                gs_list-kitapno = gv_kitapno.
                gs_list-atarih = gv_teslimtarih.
                gs_list-ogrno = gv_ogrencino.
                gs_list-ogrfname = temp_ograd.
                gs_list-kitapad = temp_kitapad.
                gs_list-yazarno = temp_yazarno.
                gs_list-yazarfname = temp_yazarad.
                gs_list-turno = temp_turno.
                gs_list-turad = temp_turad.

                MODIFY gt_list FROM gs_list TRANSPORTING: vtarih, atarih, ogrno, kitapno, ogrfname, kitapad, yazarno, yazarfname, turno, turad lights, line_color WHERE islemno = gv_islemno.
              ENDLOOP.




*      COMMIT WORK AND WAIT.
              IF sy-subrc = 0.
                MESSAGE 'Veri başarıyla kaydedildi.' TYPE 'I'.
*        PERFORM refresh_alv.
                LEAVE TO SCREEN 0.
              ELSE.
                MESSAGE 'Veri kaydedilirken hata oluştu.' TYPE 'E'.
              ENDIF.
              .
            WHEN OTHERS.
          ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'STATUS_0300'.
* SET TITLEBAR 'xxx'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

FORM refresh_alv.
  " Güncellenen veriyi iç tabloya ekleyin
  READ TABLE gt_list INTO DATA(gs_list) WITH KEY islemno = gv_islemno.
  IF sy-subrc = 0.
    gs_list-kitapno   = gv_kitapno.
    gs_list-ogrno     = gv_ogrencino.
    gs_list-atarih    = gv_teslimtarih.
    gs_list-vtarih    = gv_alimtarih.

    MODIFY gt_list FROM gs_list TRANSPORTING kitapno ogrno atarih vtarih WHERE islemno = gv_islemno.
  ELSE.
    " Yeni bir kayıt ekliyorsanız, bunu da ele alın
    CLEAR gs_list.
    gs_list-islemno = gv_islemno.
    gs_list-kitapno = gv_kitapno.
    gs_list-ogrno   = gv_ogrencino.
    gs_list-atarih  = gv_teslimtarih.
    gs_list-vtarih  = gv_alimtarih.

    APPEND gs_list TO gt_list.
  ENDIF.

  " ALV'yi yeniden çizdirin
  PERFORM display_alv.
ENDFORM.
