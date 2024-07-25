*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report  ZKAR_ISLEM_ALV
*&---------------------------------------------------------------------*
REPORT zkar_p_odev03.

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
         yazarad    TYPE zkar_yazarsoyad_de,
         line_color TYPE char4,
         lights     TYPE c,

       END OF gty_list.

DATA: gt_list TYPE TABLE OF gty_list,
      gs_list TYPE gty_list.

DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv,
      gs_fieldcatalog TYPE slis_fieldcat_alv.

DATA: gs_layout TYPE slis_layout_alv.



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
         zkar_yazar_t~yazarsoyad
     FROM zkar_islem_t
    INNER JOIN zkar_ogrenci_t ON zkar_ogrenci_t~ogrno EQ zkar_islem_t~ogrno
    INNER JOIN zkar_kitap_t ON zkar_kitap_t~kitapno EQ zkar_islem_t~kitapno
    INNER JOIN zkar_yazar_t ON zkar_yazar_t~yazarno EQ zkar_kitap_t~yazarno
    INTO CORRESPONDING FIELDS OF TABLE gt_list.

  LOOP AT gt_list INTO gs_list.
    IF gs_list-puan > 70.
      gs_list-lights = 3.

        elseIF gs_list-puan < 50.
      gs_list-lights = 1.

        else.
      gs_list-lights = 2.

    ENDIF.
    IF gs_list-vtarih < sy-datum.
      gs_list-line_color = 'C610'.

    ELSEIF gs_list-vtarih = sy-datum.
      gs_list-line_color = 'C310'.

    ELSEIF gs_list-vtarih > sy-datum.
      gs_list-line_color = 'C510'.
    ENDIF.
    MODIFY gt_list FROM gs_list.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_LAYOUT
*&---------------------------------------------------------------------*
FORM set_layout .
  gs_layout-window_titlebar = 'REUSE ALV ODEV'.
  gs_layout-zebra = abap_true.
  gs_layout-colwidth_optimize = abap_true.
  gs_layout-info_fieldname   = 'LINE_COLOR'.
  gs_layout-lights_fieldname = 'lights'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcatalog
*     i_save             = 'A'
    TABLES
      t_outtab           = gt_list.
*    EXCEPTIONS
*     program_error      = 1
*     OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FC_SUB
*&---------------------------------------------------------------------*
FORM set_fc_sub USING p_fieldname
                      p_seltext_s
                      p_seltext_m
                      p_seltext_l
                      p_key
                      p_col_pos.
  CLEAR: gs_fieldcatalog.
  gs_fieldcatalog-fieldname = p_fieldname.
  gs_fieldcatalog-seltext_s = p_seltext_s.
  gs_fieldcatalog-seltext_m = p_seltext_m.
  gs_fieldcatalog-seltext_l = p_seltext_l.
  gs_fieldcatalog-key       = p_key.
  gs_fieldcatalog-col_pos   = p_col_pos.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FC
*&---------------------------------------------------------------------*
FORM set_fc .
  PERFORM:
           set_fc_sub USING 'lights'   'lights' 'lights' 'lights' '' '0',
           set_fc_sub USING 'OGRNO'    'Öğrenci No' 'Öğrenci No' 'Öğrenci No'   '' '2',
           set_fc_sub USING 'Ograd'    'Öğrenci ad' 'Öğrenci adi' 'Öğrenci adi'   '' '2',
           set_fc_sub USING 'OGrsoyad'    'Ögrenci soyad' 'Öğrenci soyad' 'Öğrenci soyad'   '' '2',
           set_fc_sub USING 'KITAPAD'   'KITAP AD' 'KITAP ADI' 'KITAP ADI' '' '5',
           set_fc_sub USING 'YAZARAD'   'YAZAR AD' 'YAZAR ADI' 'YAZAR ADI' '' '5',
           set_fc_sub USING 'YazarSOYAD'   'Yazar SOYAD.' 'YAZAR SOYADI' 'YAZAR SOYADI' '' '5',
           set_fc_sub USING 'ATARIH'   'Alış Tarihi' 'Alış Tarihi' 'Alış Tarihi'  '' '5',
           set_fc_sub USING 'VTARIH'   'Veriş Tarihi' 'Veriş Tarihi' 'Veriş Tarihi' '' '5',
           set_fc_sub USING 'PUAN'   'PUAN' 'PUAN' 'PUAN' '' '5'.


ENDFORM.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM set_layout.
  PERFORM set_fc.
  PERFORM display_alv.
