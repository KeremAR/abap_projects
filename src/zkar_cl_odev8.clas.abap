class ZKAR_CL_ODEV8 definition
  public
  final
  create public .

public section.

  data PICTURE_CONTROL type ref to CL_GUI_PICTURE .
  data GO_CONT2 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_CONT1 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_HTML type ref to CL_GUI_HTML_VIEWER .
  data LO_DOCK type ref to CL_GUI_DOCKING_CONTAINER .
  data LO_CONT type ref to CL_GUI_CONTAINER .
  data GO_ALV type ref to CL_GUI_ALV_GRID .
  data GO_CONT type ref to CL_GUI_CUSTOM_CONTAINER .
  data GT_FCAT type LVC_T_FCAT .
  data GS_FCAT type LVC_S_FCAT .
  data GS_LAYOUT type LVC_S_LAYO .
  data GT_MALZEME type ZKAR_MALZEME_TT .
  data CC_ALV type ref to CL_GUI_CONTAINER .
  data SELECTED_MALZEMID type MATNR .
  data GV_MALZEMEID type MATNR .
  data GO_CONT4 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_CONT3 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_ALV2 type ref to CL_GUI_ALV_GRID .
  data GT_SELECTED_MALZEME type ZKAR_MALZEME_TT .
  data GT_MALZEME2 type ZKAR_MALZEME_TT .
  data GO_DOCU type ref to CL_DD_DOCUMENT .
  data CONT_TOP type ref to CL_GUI_CONTAINER .
  data CONT_BOTTOMM type ref to CL_GUI_CONTAINER .
  data GO_CONT5 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_CONT6 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_ALV3 type ref to CL_GUI_ALV_GRID .
  data GT_MALZEME3 type ZKAR_MALZEME_TT .
  data GO_ALV4 type ref to CL_GUI_ALV_GRID .

  methods CONSTRUCTOR .
  methods BUTTON .
  methods LOGO .
  methods CREATE_CONTAINER .
  methods SATIN_ALMA_GIRISI .
  methods SATIN_ALMA_ONAYI .
  methods MUHASEBE_ODEME .
  methods ONAYCI_BAKIM_TABLOSU .
  methods ON_SAPEVENT
    for event SAPEVENT of CL_GUI_HTML_VIEWER
    importing
      !ACTION
      !FRAME
      !GETDATA
      !POSTDATA
      !QUERY_TABLE .
  methods GET_DATA
    importing
      !IV_MALZEME_ID type ZKAR_MATNR_DE .
  methods ALV_BUTTON
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ALV_USERCOMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods DATA_CHANGED
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID .
  methods MAIL .
  methods ALV_BUTTON2
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ALV_USERCOMMAND2
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods GET_DATA2 .
  methods GET_DATA3
    importing
      !IV_BUY_NO type INT4
      !IV_IBAN type ZKAR_IBAN_DE .
  methods ALV_BUTTON3
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods ALV_USERCOMMAND3
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods SATIN_ALMA_ONAYI1 .
  methods ALV_BUTTON4
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods ALV_USERCOMMAND4
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods GET_DATA4
    importing
      !IV_MALZEME_ID type ZKAR_MATNR_DE optional
      !IV_BUY_NO type INT4 .
  methods KAYDET
    importing
      !IV_MALZEME_ID type ZKAR_MATNR_DE
      !IV_MALZEME_DESC type MAKTX
      !IV_MALZEME_NUM type INT4
      !IV_OLCU_BIRIM type MEINS
      !IV_MALZEME_TUR type MTART
      !IV_MAL_GRUP type MATKL
      !IV_FIYAT type WRBTR
      !IV_PARA_BIRIMI type WAERS
      !IV_TOPLAM type INT4 .
  methods AUTHORIZATION_YONETICI .
  methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO .
  methods AUTHORIZATION_MUHASEBECI .
  methods TOP_OF_PAGE
    for event TOP_OF_PAGE of CL_GUI_ALV_GRID
    importing
      !E_DYNDOC_ID
      !TABLE_INDEX .
  methods ONAYLA .
  methods REDDET .
  methods MUHASEBE_ODEME1 .
  methods GET_DATA5 .
  methods HANDLE_HOTSPOT_CLICK2
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_ROW_NO .
  methods BANKAYA_GONDER .
  methods RAPOR .
  methods TEMIZLE .
  methods ONAYA_GONDER .
protected section.
private section.

  data GT_RAPOR type ZKAR_MALZEME_TT .
ENDCLASS.



CLASS ZKAR_CL_ODEV8 IMPLEMENTATION.


  method ALV_BUTTON.

      DATA: ls_toolbar TYPE stb_button.

    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.

    CLEAR: ls_toolbar.

*    ls_toolbar-function = '&EKLE'.
*    ls_toolbar-text = 'Satir Ekle'.
*    ls_toolbar-icon = '@17@'.
*    ls_toolbar-quickinfo = 'Satir Ekleme Islemi'.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = '&SIL'.
    ls_toolbar-text = 'Satir Sil'.
    ls_toolbar-icon = '@18@'.
    ls_toolbar-quickinfo = 'Silme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.


*    ls_toolbar-function = '&KAYDET'.
*    ls_toolbar-text = 'Kaydet'.
*    ls_toolbar-icon = '@2L@'.
*    ls_toolbar-quickinfo = 'Kaydetme Islemi'.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = '&ONAY'.
    ls_toolbar-text = 'Onaya Gonder'.
    ls_toolbar-icon = '@1S@'.
    ls_toolbar-quickinfo = 'Onaya Gonderme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.


  endmethod.


  method ALV_BUTTON2.

      DATA: ls_toolbar TYPE stb_button.

*    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.

    CLEAR: ls_toolbar.

    ls_toolbar-function = '&ONAYLA'.
    ls_toolbar-text = 'ONAYLA'.
    ls_toolbar-icon = '@17@'.
    ls_toolbar-quickinfo = 'Satin Almayi Onayla'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = '&REDDET'.
    ls_toolbar-text = 'REDDET'.
    ls_toolbar-icon = '@18@'.
    ls_toolbar-quickinfo = 'Satin Almayi Reddet'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  endmethod.


  METHOD alv_button3.

    DATA: ls_toolbar TYPE stb_button.

*    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.
*    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.

    CLEAR: ls_toolbar.

    ls_toolbar-function = '&BANKA'.
    ls_toolbar-text = 'BANKAYA GONDER'.
    ls_toolbar-icon = '@17@'.
    ls_toolbar-quickinfo = 'Bankaya Gonder'.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  method ALV_BUTTON4.

    DATA: ls_toolbar TYPE stb_button.

*    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.

    CLEAR: ls_toolbar.

*    ls_toolbar-function = '&DETAY'.
*    ls_toolbar-text = 'Detayli Goster'.
**    ls_toolbar-icon = '@17@'.
*    ls_toolbar-icon = '@59@'.
*    ls_toolbar-quickinfo = 'Detayli Goster'.
*    APPEND ls_toolbar TO e_object->mt_toolbar.
  endmethod.


METHOD alv_usercommand.
  DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
        gs_list          TYPE zkar_t_malzeme2,
        gt_list          TYPE TABLE OF zkar_t_malzeme2,
        lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  LIKE LINE OF lt_selected_rows,
        lv_index         TYPE sy-tabix,
        answer           TYPE char1.

  DATA: go_gbt       TYPE REF TO cl_gbt_multirelated_service,
        go_bcs       TYPE REF TO cl_bcs,
        go_doc_bcs   TYPE REF TO cl_document_bcs,
        go_recipient TYPE REF TO if_recipient_bcs,
        gt_soli      TYPE TABLE OF soli,
        gs_soli      TYPE soli,
        gv_status    TYPE bcs_rqst.

  DATA: lv_number TYPE numc10.

  DATA:
        table        TYPE REF TO cl_salv_table.

  DATA: lt_content TYPE soli_tab.
  DATA: gv_content TYPE string.

  DATA: lt_selected_users TYPE TABLE OF zkar_t_onayci,
        lt_emails         TYPE TABLE OF ad_smtpadr,
        lv_email          TYPE AD_SMTPADR.

  CASE e_ucomm.
    WHEN '&SIL'.
       DELETE FROM zkar_t_malzeme2
          WHERE status = 0.

      CALL METHOD go_alv->get_selected_rows
        IMPORTING
          et_index_rows = lt_selected_rows.
      DESCRIBE TABLE lt_selected_rows LINES lv_index.

      IF lv_index IS INITIAL.
        MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
        RETURN.
      ENDIF.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = ' deneme'
          text_question         = 'Islemi silmek istedigine emin misin?'
          text_button_1         = 'EVET'(001)
          icon_button_1         = 'ICON_CHECKED '
          text_button_2         = 'HAYIR'(002)
          icon_button_2         = 'CON_CANCEL '
          display_cancel_button = 'X'
          popup_type            = 'ICON_MESSAGE_ERROR'
        IMPORTING
          answer                = answer.
      CHECK answer EQ 1.

      LOOP AT lt_selected_rows INTO ls_selected_row.
        lv_index = ls_selected_row-index.
        READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
        IF sy-subrc = 0.
          DELETE FROM zkar_t_malzeme2
          WHERE malzeme_id = gs_malzeme-malzeme_id
          AND malzeme_desc = gs_malzeme-malzeme_desc
          AND malzeme_num = gs_malzeme-malzeme_num
          AND malzeme_tur = gs_malzeme-malzeme_tur
          AND olcu_birim = gs_malzeme-olcu_birim
          AND para_birimi = gs_malzeme-para_birimi
          AND fiyat = gs_malzeme-fiyat
          AND mal_grup = gs_malzeme-mal_grup.

          DELETE gt_malzeme INDEX lv_index.
        ENDIF.
      ENDLOOP.
*      get_data( ).
      go_alv->refresh_table_display( ).

    WHEN '&ONAY'.
       DELETE FROM zkar_t_malzeme2
          WHERE status = 0.


      SELECT *
              FROM zkar_t_onayci
       INTO TABLE lt_selected_users
       WHERE muhasebeci = 'X'.

      LOOP AT lt_selected_users INTO DATA(ls_user).
        APPEND ls_user-mail TO lt_emails.
      ENDLOOP.

*      CALL METHOD go_alv->get_selected_rows
*        IMPORTING
*          et_index_rows = lt_selected_rows.
*      IF lt_selected_rows IS INITIAL.
*        MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
*        RETURN.
*      ENDIF.

*      LOOP AT lt_selected_rows INTO ls_selected_row.
*        lv_index = ls_selected_row-index.
*        READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
*        IF sy-subrc = 0.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZKAR_BUYNO'
            IMPORTING
              number      = lv_number.
          IF sy-subrc = 0.
             LOOP AT gt_malzeme INTO gs_malzeme.
            gs_malzeme-buy_no = lv_number.
            gs_malzeme-status = '1'.
            gs_malzeme-light = icon_green_light.

            MODIFY gt_malzeme  FROM gs_malzeme .

            READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id .
            IF sy-subrc = 0.
              gs_list-buy_no = lv_number.
              MODIFY gt_list FROM gs_list .
            ELSE.
              MOVE-CORRESPONDING gs_malzeme TO gs_list.
              gs_list-buy_no = lv_number.
              APPEND gs_list TO gt_list.
            ENDIF.

            ENDLOOP.
            MODIFY zkar_t_malzeme2 FROM TABLE gt_list.

          ENDIF.
*        ENDIF.

        gv_content ='<!DOCTYPE html> '
  &&'<html> '
  &&'<head>'
  &&'<meta charset="utf-8">'
  &&'<style>'
  &&'th{'
  &&'background-color: lightgreen;'
  &&'border: 2px solid;'
  &&'}'
  &&'td{'
  &&'background-color: lightblue;'
  &&'border: 1px solid;'
  &&'}'
  &&'</style>'
  &&'</head>'
  &&'<body>'
  &&'<p>Sayın Yönetici,</p>'
  &&'<p>' && gs_list-buy_no && '  satin alma numarasıyla  ' && ' '  && gs_list-malzeme_desc && '  ürününe satin alma girişi yapılmıştır. Onayınızı beklemektedir.</p>'
  &&'<p>Bilginize,</p>'
  &&'<br><br>'
  &&'<table>'
  &&'<tr>'
  &&'<th>Malzeme ID</th>'
  &&'<th>Aciklama</th>'
  &&'<th>Malzeme Turu</th>'
  &&'<th>Mal Grubu</th>'
  &&'<th>Olcu Birimi</th>'
  &&'<th>Sayisi</th>'
  &&'<th>Birim Fiyati</th>'
  &&'<th>Toplam Fiyati</th>'
  &&'</tr>'.

        LOOP AT gt_list INTO gs_list.
          gv_content = gv_content &&'<tr>'
          &&'<td>' && gs_list-malzeme_id && '</td>'
          &&'<td>' && gs_list-malzeme_desc && '</td>'
          &&'<td>' && gs_list-malzeme_tur && '</td>'
          &&'<td>' && gs_list-mal_grup && '</td>'
          &&'<td>' && gs_list-olcu_birim && '</td>'
          &&'<td>' && gs_list-malzeme_num && '</td>'
          &&'<td>' && gs_list-fiyat && '</td>'
          &&'<td>' && gs_list-toplam && '</td>'
          &&'</tr>'.
        ENDLOOP.

        gv_content = gv_content &&'</table>'
          &&'</body>'
          &&'</html>'.

        CREATE OBJECT go_gbt.

        gt_soli = cl_document_bcs=>string_to_soli( gv_content ).

        CALL METHOD go_gbt->set_main_html
          EXPORTING
            content = gt_soli.

        go_doc_bcs = cl_document_bcs=>create_from_multirelated(
                       i_subject          = 'Satin Alma Girisi Hk.'
                       i_multirel_service = go_gbt ).


          LOOP AT lt_emails INTO lv_email.
            go_recipient = cl_cam_address_bcs=>create_internet_address(
                             i_address_string = lv_email  ).
          ENDLOOP.

          go_bcs = cl_bcs=>create_persistent( ).
          go_bcs->set_document( i_document = go_doc_bcs ).
          go_bcs->add_recipient( i_recipient = go_recipient ).

*        gv_status = 'N'.
*        CALL METHOD go_bcs->set_status_attributes
*          EXPORTING
*            i_requested_status = gv_status.

          go_bcs->send( ).
          COMMIT WORK.

*        ENDLOOP.

*      get_data( ).
        go_alv->refresh_table_display(
  EXPORTING
    i_soft_refresh = abap_true ).

    ENDCASE.





*        DATA: msg TYPE REF TO cl_bcs_message.
*        CREATE OBJECT msg.
*
*        TRY.
*            CALL METHOD cl_salv_table=>factory
*              IMPORTING
*                r_salv_table = table
*              CHANGING
*                t_table      = gt_list.
*          CATCH cx_salv_msg .
*        ENDTRY.
*
*        DATA v_xstring TYPE xstring.
*        v_xstring = table->to_xml( if_salv_bs_xml=>c_type_xlsx ).
*
*        msg->add_attachment(
*  EXPORTING
*    iv_doctype      =    'EXT'
**    iv_description  =
*    iv_filename     =    'satin_alma.xlsx'
**    iv_codepage     =   '
**    iv_contents_txt =
*    iv_contents_bin =     v_xstring
**    iv_content_id   =
*).
*        DATA: lv_body TYPE string.
*        lv_body = |Sayin yonetici, | &&
*                                cl_abap_char_utilities=>cr_lf && " New line
*                                |'{ gs_list-buy_no }' Satin alma numarasiyla '{ gs_list-malzeme_desc }' urunune satin alma girisi yapilmistir | &&
*                                cl_abap_char_utilities=>cr_lf && " New line
*                                |Onayinizi beklemektedir| &&
*                                cl_abap_char_utilities=>cr_lf && " New line
*                                |Bilginize|.

*        msg->set_subject( |Satin Alma Girisi Hk.| ).
*
*        msg->set_main_doc(
**        iv_contents_txt = lv_body
*        iv_contents_txt = gv_content
*
*        ).
*        msg->add_recipient( 'test@example.com' ).
*        msg->set_sender( iv_address = 'no-reply@ides.com' ).
*
*        TRY.
*            msg->send( ).
*            MESSAGE 'Mail gonderimi basarili.' TYPE 'I'.
*          CATCH cx_bcs.
*            MESSAGE 'Mail gonderimi basarisiz.' TYPE 'E'.
*        ENDTRY.
  ENDMETHOD.


  METHOD alv_usercommand2.

    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
          gs_list          TYPE zkar_t_malzeme2,
          gt_list          TYPE TABLE OF zkar_t_malzeme2,
          lt_selected_rows TYPE lvc_t_row,
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1,
          table            TYPE REF TO cl_salv_table,
          lv_body          TYPE string,
          msg              TYPE REF TO cl_bcs_message,
          lv_bin_filesize  TYPE i,
          lv_bin_file      TYPE xstring.

    DATA: fm_name TYPE rs38l_fnam,
          it_itab TYPE zkar_malzeme_tt.

    DATA: lt_binary_tab TYPE solix_tab,
*              lv_bin_filesize   TYPE so_obj_len,
          lv_bin_xstr   TYPE xstring,
          lt_lines_pdf  TYPE TABLE OF tline,
          lt_text       TYPE TABLE OF tline,
          ls_text       TYPE tline.

    DATA: lo_bcs           TYPE REF TO cl_bcs,
          lo_sapuser       TYPE REF TO cl_sapuser_bcs,
          lo_external_user TYPE REF TO cl_cam_address_bcs,
          lo_doc_bcs       TYPE REF TO cl_document_bcs,
          lo_recep         TYPE REF TO if_recipient_bcs,
          lo_cx_bcx        TYPE REF TO cx_bcs.

    DATA: ls_control_parameters TYPE ssfctrlop,
          ls_output_options     TYPE ssfcompop,
          ls_output_info        TYPE ssfcrescl,
          ls_job_output_info    TYPE ssfcrescl.

    DATA: lv_date_formatted TYPE string,
          lv_filename       TYPE string.

    DATA: lt_selected_users TYPE TABLE OF zkar_t_onayci,
          lt_emails         TYPE TABLE OF ad_smtpadr,
          lv_email          TYPE bcs_address.

    CASE e_ucomm .
      WHEN '&REDDET'.
        authorization_yonetici( ).


*        CALL METHOD go_alv->get_selected_rows
*          IMPORTING
*            et_index_rows = lt_selected_rows.
*
*        DESCRIBE TABLE lt_selected_rows LINES lv_index.

*        IF lv_index IS INITIAL.
*          MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
*          RETURN.
*        ENDIF.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = ' deneme'
            text_question         = 'Islemi reddetmek istedigine emin misin?'
            text_button_1         = 'EVET'(001)
            icon_button_1         = 'ICON_CHECKED '
            text_button_2         = 'HAYIR'(002)
            icon_button_2         = 'CON_CANCEL '
            display_cancel_button = 'X'
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = answer.
        CHECK answer EQ 1.

*        LOOP AT lt_selected_rows INTO ls_selected_row.
*          lv_index = ls_selected_row-index.
*          READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
        LOOP AT gt_malzeme INTO gs_malzeme.
          gs_malzeme-status = 4.
          gs_malzeme-light = icon_red_light.
          MODIFY gt_malzeme FROM gs_malzeme .
          READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id.
          IF sy-subrc = 0.
            gs_list-status = gs_malzeme-status.
            MODIFY gt_list FROM gs_list.
          ELSE.
            MOVE-CORRESPONDING gs_malzeme TO gs_list.
            gs_list-status = gs_malzeme-status.
            APPEND gs_list TO gt_list.
          ENDIF.
          endloop.
          MODIFY zkar_t_malzeme2 FROM TABLE gt_list.
*        ENDLOOP.

*        MODIFY zkar_t_malzeme FROM TABLE gt_list.
        go_alv->refresh_table_display( ).

*        DELETE gt_malzeme WHERE status = 4.

*        go_alv->refresh_table_display( ).

      WHEN '&ONAYLA' .
        authorization_yonetici( ).
        SELECT *
               FROM zkar_t_onayci
        INTO TABLE lt_selected_users
        WHERE yonetici = 'X'.

        LOOP AT lt_selected_users INTO DATA(ls_user).
          APPEND ls_user-mail TO lt_emails.
        ENDLOOP.


        DATA(lv_day)   = sy-datum+6(2).
        DATA(lv_month) = sy-datum+4(2).
        DATA(lv_year)  = sy-datum(4).
        CONCATENATE lv_day lv_month lv_year INTO lv_date_formatted SEPARATED BY '/'.

        MESSAGE lv_date_formatted TYPE 'S'.

*        CALL METHOD go_alv->get_selected_rows
*          IMPORTING
*            et_index_rows = lt_selected_rows.
*        IF lt_selected_rows IS INITIAL.
*          MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
*          RETURN.
*        ENDIF.
*
*        LOOP AT lt_selected_rows INTO ls_selected_row.
*          lv_index = ls_selected_row-index.
*          READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.

        IF sy-subrc = 0.
          LOOP AT gt_malzeme INTO gs_malzeme.
            gs_malzeme-status = '2'.
            gs_malzeme-light = icon_green_light.
            lv_filename = |{ lv_date_formatted }_{ gs_malzeme-buy_no }.pdf|.

            MODIFY gt_malzeme  FROM gs_malzeme .

            READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id.
            IF sy-subrc = 0.
              gs_list-status = '2'.
              MODIFY gt_list FROM gs_list.
            ELSE.
              MOVE-CORRESPONDING gs_malzeme TO gs_list.
              gs_list-status = '2'.
              APPEND gs_list TO gt_list.
            ENDIF.
          ENDLOOP.
          MODIFY zkar_t_malzeme2 FROM TABLE gt_list.

*            DELETE gt_malzeme WHERE status = 2.

          DATA: lt_temp_list TYPE  zkar_malzeme_tt,
                ls_temp_list LIKE LINE OF lt_temp_list.

          LOOP AT gt_list INTO gs_list.
            MOVE-CORRESPONDING gs_list TO ls_temp_list.
            APPEND ls_temp_list TO lt_temp_list.
          ENDLOOP.

          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              formname = 'ZKAR_SF_02'
            IMPORTING
              fm_name  = fm_name.

          ls_control_parameters-preview = abap_true.
          ls_control_parameters-no_dialog = abap_true.
          ls_control_parameters-getotf = 'X'.
*            ls_output_options-tdnoprint = ''.
          ls_output_options-tddest = 'LP01' .
*            ls_control_parameters-no_open = 'X'.




          CALL FUNCTION '/1BCDWB/SF00000155'
            EXPORTING
              control_parameters = ls_control_parameters
              output_options     = ls_output_options
              user_settings      = ' '
              gs_malzeme         = gs_malzeme
              lt_temp_list       = lt_temp_list
            IMPORTING
              job_output_info    = ls_job_output_info.

          IF sy-subrc <> 0.
            MESSAGE 'SmartForm fail' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.

          IF ls_job_output_info-otfdata[] IS INITIAL.
            MESSAGE 'İçeriğe ulaşalımadı.' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.

          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              format                = 'PDF'
            IMPORTING
              bin_filesize          = lv_bin_filesize
              bin_file              = lv_bin_file
            TABLES
              otf                   = ls_job_output_info-otfdata
              lines                 = lt_lines_pdf
            EXCEPTIONS
              err_max_linewidth     = 1
              err_format            = 2
              err_conv_not_possible = 3
              OTHERS                = 4.

          IF sy-subrc IS INITIAL.
            CREATE OBJECT msg.
            msg->add_attachment(
                  EXPORTING
                    iv_doctype      = 'PDF'
*                      iv_description  = 'SAS Belgesi Hk.'
                    iv_filename     = lv_filename
                    iv_contents_bin = lv_bin_file ).
          ENDIF.

          lv_body = |Sayin Muhasebe Yetkilisi, | &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |'{ gs_list-buy_no }' Satin alma numarasiyla satin alma girisi onaylanmistir. | &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |Satin alma icin odemeniz beklenmektedir| &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |Bilginize|.

          msg->set_subject( |Satin Alma Onaylandi.| ).
          msg->set_main_doc( iv_contents_txt = lv_body ).
          LOOP AT lt_emails INTO lv_email.
            msg->add_recipient(
            EXPORTING
              iv_address      =    lv_email          ).
          ENDLOOP.
*            msg->add_recipient( 'test@example.com' ).
          msg->set_sender( iv_address = 'no-reply@ides.com' ).

          TRY.
              msg->send( ).
              MESSAGE 'Mail gonderimi basarili.' TYPE 'I'.
            CATCH cx_bcs.
              MESSAGE 'Mail gonderimi basarisiz.' TYPE 'E'.
          ENDTRY.

        ENDIF.
*        ENDLOOP.

        go_alv->refresh_table_display(
          EXPORTING
            i_soft_refresh = abap_true ).


    ENDCASE.
  ENDMETHOD.


  METHOD alv_usercommand3.

    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
          gs_list          TYPE zkar_t_malzeme2,
          gt_list          TYPE TABLE OF zkar_t_malzeme2,
          lt_selected_rows TYPE lvc_t_row,
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1.

    DATA: lv_date_formatted TYPE string,
          lv_time_formatted TYPE string,
          lv_filename       TYPE string,
          lv_full_path      TYPE string,
          lv_line           TYPE string,
          lt_data           TYPE TABLE OF string.

    DATA(lv_day)   = sy-datum+6(2).
    DATA(lv_month) = sy-datum+4(2).
    DATA(lv_year)  = sy-datum(4).
    CONCATENATE lv_day lv_month lv_year INTO lv_date_formatted SEPARATED BY '-'.

    DATA(lv_hour)    = sy-uzeit+0(2).
    DATA(lv_minute)  = sy-uzeit+2(2).
    CONCATENATE lv_hour lv_minute INTO lv_time_formatted SEPARATED BY '-'.

*    DATA : lt_values_tab   TYPE TABLE OF dd07v,
*           ls_value     TYPE dd07v,
*           lv_iban_desc TYPE string.
*
*    CALL FUNCTION 'GET_DOMAIN_VALUES'
*      EXPORTING
*        domname    = 'ZKAR_IBAN_DO'
*        text       = 'X'
*      TABLES
*        values_tab = lt_values_tab.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

    CASE e_ucomm.
      WHEN '&BANKA' .
        authorization_muhasebeci( ).

        CALL METHOD go_alv->get_selected_rows
          IMPORTING
            et_index_rows = lt_selected_rows.
        IF lt_selected_rows IS INITIAL.
          MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
          RETURN.
        ENDIF.
        LOOP AT lt_selected_rows INTO ls_selected_row.
          lv_index = ls_selected_row-index.
          READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
          IF sy-subrc = 0.
            IF gs_malzeme-iban eq 'Seciniz. '.
               MESSAGE 'IBAN seçilmedi. Lütfen bir IBAN seçiniz.' TYPE 'I'.
          RETURN.

            ENDIF.


*          READ TABLE lt_values_tab INTO ls_value WITH KEY domvalue_l = gs_malzeme-iban.
*          IF sy-subrc = 0.
*            lv_iban_desc = ls_value-ddtext.  " IBAN description
*          ELSE.
*            lv_iban_desc = 'Unknown IBAN'.
*          ENDIF.

            gs_malzeme-status = '3'.
            gs_malzeme-light = icon_green_light.

            lv_filename = |{ lv_date_formatted }_{ lv_time_formatted }_{ gs_malzeme-buy_no }.txt|.
            CONCATENATE 'C:\usr\sap\DIR_BANKA\' lv_filename INTO lv_full_path.
            CLEAR: lt_data.
            lv_line = |{ gs_malzeme-malzeme_id } # { gs_malzeme-malzeme_desc } # { gs_malzeme-malzeme_num } # { gs_malzeme-olcu_birim } # { gs_malzeme-malzeme_tur } # { gs_malzeme-mal_grup } # { gs_malzeme-fiyat } # { gs_malzeme-para_birimi } # {
                         gs_malzeme-toplam } # {
                         gs_malzeme-buy_no } # { gs_malzeme-status } # { gs_malzeme-iban }|.
            APPEND lv_line TO lt_data.

            DATA: v_dir_server TYPE sapb-sappfad,
                  v_dir_client TYPE sapb-sappfad.

            v_dir_server = 'C:\usr\sap\DIR_BANKA\'.
            v_dir_client = lv_full_path.

            OPEN DATASET lv_full_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
            IF sy-subrc <> 0.
              MESSAGE 'dosya acilamadi' TYPE 'I'.
            ENDIF.
            LOOP AT lt_data INTO lv_line .
              TRANSFER lv_line TO lv_full_path.

            ENDLOOP.

            CLOSE DATASET lv_full_path.
            IF sy-subrc = 0.
              MESSAGE 'Dosya başarıyla kaydedildi' TYPE 'I'.
            ELSE.
              MESSAGE 'Dosya kapatılamadı' TYPE 'E'.
            ENDIF.

            MODIFY gt_malzeme  FROM gs_malzeme INDEX lv_index .

            READ TABLE gt_list INTO gs_list INDEX lv_index.
            IF sy-subrc = 0.
              gs_list-status = '3'.

              MODIFY gt_list FROM gs_list INDEX lv_index.
            ELSE.
              MOVE-CORRESPONDING gs_malzeme TO gs_list.
              gs_list-status = '3'.
              APPEND gs_list TO gt_list.

            ENDIF.
            MODIFY zkar_t_malzeme2 FROM TABLE gt_list.
*            DELETE gt_malzeme WHERE status = 3.
          ENDIF.
        ENDLOOP.

        go_alv->refresh_table_display(
      EXPORTING
        i_soft_refresh = abap_true ).

    ENDCASE.
  ENDMETHOD.


  METHOD alv_usercommand4.

*
*    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
*          gs_list          TYPE zkar_t_malzeme,
*          gt_list          TYPE TABLE OF zkar_t_malzeme,
*          lt_selected_rows TYPE lvc_t_row,
*          ls_selected_row  LIKE LINE OF lt_selected_rows,
*          lv_index         TYPE sy-tabix.
*
*    CASE e_ucomm.
*      WHEN '&DETAY'.
*        CALL METHOD go_alv2->get_selected_rows
*          IMPORTING
*            et_index_rows = lt_selected_rows.
*
*        DESCRIBE TABLE lt_selected_rows LINES lv_index.
*        IF lv_index IS INITIAL.
*          MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
*          RETURN.
*        ENDIF.
*
*        LOOP AT lt_selected_rows INTO ls_selected_row.
*          lv_index = ls_selected_row-index.
*          READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
*          IF sy-subrc = 0.
*            CLEAR gt_list.
*              gs_list-malzeme_id = gs_malzeme-malzeme_id.
*          ENDIF.
*        ENDLOOP.
*
*        CALL METHOD get_data4
*        EXPORTING
*          iv_malzeme_id = gs_list-malzeme_id.
*
**      CALL METHOD satin_alma_onayi.
*
*    ENDCASE.
  ENDMETHOD.


  METHOD authorization_muhasebeci.


    SELECT zusername
  FROM zkar_t_onayci
  INTO TABLE @DATA(selected_users)
  WHERE muhasebeci = 'X'.

    DATA: lv_user_found TYPE abap_bool.
    lv_user_found = abap_false.

    LOOP AT selected_users INTO DATA(ls_user).
      IF ls_user-zusername = sy-uname.
        lv_user_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_user_found = abap_false.
      MESSAGE 'Yetkiniz yok!' TYPE 'E' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD AUTHORIZATION_YONETICI.

    SELECT zusername
  FROM zkar_t_onayci
  INTO TABLE @DATA(selected_users)
  WHERE yonetici = 'X'.

    DATA: lv_user_found TYPE abap_bool.
    lv_user_found = abap_false.

    LOOP AT selected_users INTO DATA(ls_user).
      IF ls_user-zusername = sy-uname.
        lv_user_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_user_found = abap_false.
      MESSAGE 'Yetkiniz yok!' TYPE 'E' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD bankaya_gonder.


    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
          gs_list          TYPE zkar_t_malzeme2,
          gt_list          TYPE TABLE OF zkar_t_malzeme2,
          lt_selected_rows TYPE lvc_t_row,
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1.

    DATA: lv_date_formatted TYPE string,
          lv_time_formatted TYPE string,
          lv_filename       TYPE string,
          lv_full_path      TYPE string,
          lv_line           TYPE string,
          lt_data           TYPE TABLE OF string.

    DATA(lv_day)   = sy-datum+6(2).
    DATA(lv_month) = sy-datum+4(2).
    DATA(lv_year)  = sy-datum(4).
    CONCATENATE lv_day lv_month lv_year INTO lv_date_formatted SEPARATED BY '-'.

    DATA(lv_hour)    = sy-uzeit+0(2).
    DATA(lv_minute)  = sy-uzeit+2(2).
    CONCATENATE lv_hour lv_minute INTO lv_time_formatted SEPARATED BY '-'.

    authorization_muhasebeci( ).



    LOOP AT gt_malzeme INTO gs_malzeme.
       IF gs_malzeme-iban EQ 'Seciniz.'.
      MESSAGE 'IBAN seçilmedi. Lütfen bir IBAN seçiniz.' TYPE 'I'.
      RETURN.

    ENDIF.

      gs_malzeme-status = '3'.
      gs_malzeme-light = icon_green_light.

      lv_filename = |{ lv_date_formatted }_{ lv_time_formatted }_{ gs_malzeme-buy_no }.txt|.
      CONCATENATE 'C:\usr\sap\DIR_BANKA\' lv_filename INTO lv_full_path.
*            CLEAR: lt_data.
      lv_line = |{ gs_malzeme-malzeme_id } # { gs_malzeme-malzeme_desc } # { gs_malzeme-malzeme_num } # { gs_malzeme-olcu_birim } # { gs_malzeme-malzeme_tur } # { gs_malzeme-mal_grup } # { gs_malzeme-fiyat } # { gs_malzeme-para_birimi } # {
                   gs_malzeme-toplam } # {
                   gs_malzeme-buy_no } # { gs_malzeme-status } # { gs_malzeme-iban }|.
      APPEND lv_line TO lt_data.

      DATA: v_dir_server TYPE sapb-sappfad,
            v_dir_client TYPE sapb-sappfad.

      v_dir_server = 'C:\usr\sap\DIR_BANKA\'.
      v_dir_client = lv_full_path.

      OPEN DATASET lv_full_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc <> 0.
        MESSAGE 'dosya acilamadi' TYPE 'I'.
      ENDIF.
      LOOP AT lt_data INTO lv_line .
        TRANSFER lv_line TO lv_full_path.

      ENDLOOP.

      CLOSE DATASET lv_full_path.
      IF sy-subrc = 0.
        MESSAGE 'Dosya başarıyla kaydedildi' TYPE 'I'.
      ELSE.
        MESSAGE 'Dosya kapatılamadı' TYPE 'E'.
      ENDIF.

      MODIFY gt_malzeme  FROM gs_malzeme .

      READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id.
      IF sy-subrc = 0.
        gs_list-status = '3'.

        MODIFY gt_list FROM gs_list.
      ELSE.
        MOVE-CORRESPONDING gs_malzeme TO gs_list.
        gs_list-status = '3'.
        APPEND gs_list TO gt_list.

      ENDIF.
    ENDLOOP.
    MODIFY zkar_t_malzeme2 FROM TABLE gt_list.

    go_alv->refresh_table_display(
  EXPORTING
    i_soft_refresh = abap_true ).



  ENDMETHOD.


  METHOD button.
  DATA:
*        lo_html TYPE REF TO cl_gui_html_viewer,
        t_event_tab TYPE TABLE OF cntl_simple_event,  " Updated type
        ls_event TYPE cntl_simple_event,
        lt_html TYPE TABLE OF w3html,
        doc_url(80).

  " Create HTML Viewer
  CREATE OBJECT Go_html
    EXPORTING
      parent = GO_CONT1.

  IF sy-subrc <> 0.
    MESSAGE 'Error in the HTML control' TYPE 'S'.
    RETURN.
  ENDIF.

  " Register SAP Event
  ls_event-eventid = Go_html->m_id_sapevent.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO t_event_tab.

  Go_html->set_registered_events(
      EXPORTING
        events = t_event_tab ).

  SET HANDLER on_sapevent FOR Go_html.

  " Build HTML
  DEFINE add_html.
    append &1 to lt_html.
  END-OF-DEFINITION.

  add_html:
    '<html>',
    '<style type="text/css">',
    'HTML { overflow: auto; height: 100%;}',
    'body{margin: 0;  padding: 0; background: white; }',
    'input.fbutton{',
    ' font-size:16px;',
    ' font-weight:bold;',
    ' width:300px;',
    '	height:100px;',
    ' background-color: orange',
    '	border-style:double;',
    '	cursor: pointer;}',
    '.divcl{',
    'display: flex;',
    'flex-direction: column;',
    'align-items: center;',
    '</style>',
    '<body>',
    '<div class="divcl">',
    '<FORM name="zzhtmlbutton"',
    '       method=post',
    '       action=SAPEVENT:MY_EVENT_1>',
    '<input type=submit name="Satın Alma Girişi"',
    '      class="fbutton" value="Satın Alma Girişi"',
    '      title="">',
    '</form>',
    '<FORM name="zzhtmlbutton"',
    '       method=post',
    '       action=SAPEVENT:MY_EVENT_2?PARAM1=Hello&PARAM2=Zevolving>',
    '<input type=submit name="Satın Alma Onay Programı"',
    '      class="fbutton" value="Satın Alma Onay Programı"',
    '      title="">',
    '</form>',
       '<FORM name="zzhtmlbutton"',
    '       method=post',
    '       action=SAPEVENT:MY_EVENT_3?PARAM1=Hello&PARAM2=Zevolving>',
    '<input type=submit name="Muhasebe Odeme Programı"',
    '      class="fbutton" value="Muhasebe Odeme Programı"',
    '      title="">',
    '</form>',
       '<FORM name="zzhtmlbutton"',
    '       method=post',
    '       action=SAPEVENT:MY_EVENT_4?PARAM1=Hello&PARAM2=Zevolving>',
      '<input type=submit name="Onayci Bakim Tablosu"',
    '      class="fbutton" value="Onayci Bakim Tablosu"',
    '      title="">',
    '</form>',
    '<FORM name="zzhtmlbutton"',
    '       method=post',
    '       action=SAPEVENT:MY_EVENT_5?PARAM1=Hello&PARAM2=Zevolving>',
      '<input type=submit name="RAPOR"',
    '      class="fbutton" value="RAPOR"',
    '      title="">',
    '</form>',
    '</div>',
    '</body>',
    '</html>'.

  " Load HTML into HTML Viewer
  Go_html->load_data(
    IMPORTING
      assigned_url = doc_url
    CHANGING
      data_table   = lt_html ).

  Go_html->show_url( url = doc_url ).

  Go_html->set_ui_flag( Go_html->uiflag_no3dborder ).

ENDMETHOD.


  METHOD constructor.
    create_container( ).
    logo( ).
    button( ).

*    SELECT zusername
*         FROM zkar_t_onayci
*           INTO TABLE @DATA(selected_users)
*               WHERE checkbox = 'X'.

*    IF sy-uname NOT IN selected_users.
*      MESSAGE 'Bu programı çalıştırma yetkiniz yok.' TYPE 'E'.
*      LEAVE PROGRAM.
*    ENDIF.
  ENDMETHOD.


  METHOD create_container.

    CREATE OBJECT go_cont1
      EXPORTING
        container_name = 'CONT_1'.

    CREATE OBJECT go_cont2
      EXPORTING
        container_name = 'CONT_2'.

    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CC_ALV'.

     CREATE OBJECT go_cont4
      EXPORTING
        container_name = 'CONT_4'.

      CREATE OBJECT go_cont3
      EXPORTING
        container_name = 'CONT_3'.

       CREATE OBJECT go_cont5
      EXPORTING
        container_name = 'CONT_5'.

      CREATE OBJECT go_cont6
      EXPORTING
        container_name = 'CONT_6'.

*      create object go_docu
*        EXPORTING
*          style            =    'ALV_GRID'
 .

  ENDMETHOD.


  METHOD data_changed.

    DATA: lv_malzeme      LIKE LINE OF gt_malzeme,
          gs_malzeme      LIKE LINE OF gt_malzeme,
          ls_data         TYPE mara,
          lv_padded_value TYPE char18.

    CLEAR gs_malzeme.

    LOOP AT gt_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).
      selected_malzemid = <fs_malzeme>-malzeme_id.
      "Sıfır ile doldurulmuş 18 karakterli değeri oluştur
      lv_padded_value = condense( selected_malzemid ).
      lv_padded_value = |{ lv_padded_value ALPHA = IN }|.

      SELECT
         mara~meins AS olcu_birim,
         mara~mtart AS malzeme_tur,
         mara~matkl AS mal_grup,
         makt~maktx AS malzeme_desc
       FROM mara
       INNER JOIN makt ON makt~matnr EQ mara~matnr
       INTO CORRESPONDING FIELDS OF @lv_malzeme
       WHERE mara~matnr EQ @lv_padded_value.
      ENDSELECT.

      <fs_malzeme>-malzeme_tur       = lv_malzeme-malzeme_tur.
      <fs_malzeme>-olcu_birim         = lv_malzeme-olcu_birim.
      <fs_malzeme>-mal_grup         = lv_malzeme-mal_grup.
      <fs_malzeme>-malzeme_desc        = lv_malzeme-malzeme_desc.
      <fs_malzeme>-toplam        = <fs_malzeme>-malzeme_num * <fs_malzeme>-fiyat.
    ENDLOOP.
    go_alv->refresh_table_display(
     EXPORTING
*            is_stable      =
       i_soft_refresh = abap_true ).


  ENDMETHOD.


  METHOD get_data.

    SELECT
      malzeme_id,
      malzeme_desc,
      malzeme_num,
      olcu_birim,
      malzeme_tur,
      mal_grup,
      fiyat,
      para_birimi,
      toplam,
      buy_no,
      status
      FROM zkar_t_malzeme2
      INTO TABLE @DATA(t_malzeme)
      WHERE malzeme_id = @iv_malzeme_id and
            status = 0.

    LOOP AT t_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).
      APPEND INITIAL LINE TO gt_malzeme ASSIGNING FIELD-SYMBOL(<malzeme>).
      <malzeme> = CORRESPONDING #( <fs_malzeme> ).
      IF <malzeme>-status EQ 0.
        <malzeme>-light = icon_yellow_light.
        <malzeme>-para_birimi = 'TRY'.
      ELSEIF <malzeme>-status EQ 4.
        <malzeme>-light = icon_red_light.
      ELSE.
        <malzeme>-light = icon_green_light.
      ENDIF.
    ENDLOOP.

    CALL METHOD satin_alma_girisi.



  ENDMETHOD.


METHOD get_data2.
  DATA:
    lv_buy_no     TYPE zkar_t_malzeme2-buy_no,
    lv_min_buy_no TYPE zkar_t_malzeme2-buy_no.

*clear: gt_malzeme2.
  SELECT
    malzeme_id,
    malzeme_desc,
    malzeme_num,
    olcu_birim,
    malzeme_tur,
    mal_grup,
    fiyat,
    para_birimi,
    toplam,
    buy_no,
    status
    FROM zkar_t_malzeme2
    INTO TABLE @DATA(t_malzeme)
    WHERE status = '1'.


  DATA: lt_unique_malzeme TYPE TABLE OF zkar_t_malzeme2,
        ls_malzeme        TYPE zkar_t_malzeme2.

  LOOP AT t_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).

    READ TABLE lt_unique_malzeme WITH KEY buy_no = <fs_malzeme>-buy_no TRANSPORTING NO FIELDS.

    IF sy-subrc <> 0.
       IF lv_min_buy_no IS INITIAL OR <fs_malzeme>-buy_no < lv_min_buy_no.
        lv_min_buy_no = <fs_malzeme>-buy_no.
      ENDIF.

      MOVE-CORRESPONDING <fs_malzeme> TO ls_malzeme.
      APPEND ls_malzeme TO lt_unique_malzeme.

      APPEND INITIAL LINE TO gt_malzeme2 ASSIGNING FIELD-SYMBOL(<malzeme>).
      <malzeme> = CORRESPONDING #( ls_malzeme ).


      IF <malzeme>-status EQ 1.
        <malzeme>-light = icon_yellow_light.
        <malzeme>-para_birimi = 'TRY'.
      ELSEIF <malzeme>-status EQ 4.
        <malzeme>-light = icon_red_light.
      ELSE.
        <malzeme>-light = icon_green_light.
      ENDIF.
    ENDIF.
  ENDLOOP.

  satin_alma_onayi1( ).

  IF lv_min_buy_no IS NOT INITIAL.
    CALL METHOD get_data4
      EXPORTING
        iv_buy_no = lv_min_buy_no.
  ENDIF.

ENDMETHOD.


  METHOD get_data3.

    CLEAR gt_malzeme.
    SELECT
      malzeme_id,
      malzeme_desc,
      malzeme_num,
      olcu_birim,
      malzeme_tur,
      mal_grup,
      fiyat,
      para_birimi,
      toplam,
      buy_no,
      status
      FROM zkar_t_malzeme2
      INTO TABLE @DATA(t_malzeme)
      WHERE buy_no = @iv_buy_no.

    LOOP AT t_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).
      READ TABLE gt_malzeme WITH KEY malzeme_id = <fs_malzeme>-buy_no TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO gt_malzeme ASSIGNING FIELD-SYMBOL(<malzeme>).
      <malzeme> = CORRESPONDING #( <fs_malzeme> ).

*      <malzeme>-iban = 'Seciniz.'.
      <malzeme>-iban = iv_iban.

      IF <malzeme>-status EQ 2.
        <malzeme>-light = icon_yellow_light.
        <malzeme>-para_birimi = 'TRY'.
      ELSEIF <malzeme>-status EQ 4.
        <malzeme>-light = icon_red_light.
      ELSE.
        <malzeme>-light = icon_green_light.
      ENDIF.
    ENDLOOP.


    muhasebe_odeme( ).

    go_alv->refresh_table_display(
      EXPORTING
        i_soft_refresh = abap_true ).
  ENDMETHOD.


  METHOD get_data4.

*    DATA: gt_selected_malzeme TYPE TABLE OF zkar_t_malzeme.

clear gt_malzeme.
    SELECT
      malzeme_id,
      malzeme_desc,
      malzeme_num,
      olcu_birim,
      malzeme_tur,
      mal_grup,
      fiyat,
      para_birimi,
      toplam,
      buy_no,
      status
      FROM zkar_t_malzeme2
      INTO TABLE @DATA(t_malzeme)
*      WHERE malzeme_id = @iv_malzeme_id.
      WHERE buy_no = @iv_buy_no.



    LOOP AT t_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).

      READ TABLE gt_malzeme WITH KEY malzeme_id = <fs_malzeme>-buy_no TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO gt_malzeme ASSIGNING FIELD-SYMBOL(<malzeme>).
      <malzeme> = CORRESPONDING #( <fs_malzeme> ).
      IF <malzeme>-status EQ 1.
        <malzeme>-light = icon_yellow_light.
        <malzeme>-para_birimi = 'TRY'.
      ELSEIF <malzeme>-status EQ 4.
        <malzeme>-light = icon_red_light.
      ELSE.
        <malzeme>-light = icon_green_light.
      ENDIF.
    ENDLOOP.


*    gt_malzeme = gt_selected_malzeme.

    CALL METHOD satin_alma_onayi.

        go_alv->refresh_table_display(
          EXPORTING
            i_soft_refresh = abap_true ).


  ENDMETHOD.


  METHOD get_data5.

    DATA:
      lv_buy_no     TYPE zkar_t_malzeme2-buy_no,
      lv_min_buy_no TYPE zkar_t_malzeme2-buy_no.

    SELECT
      malzeme_id,
      malzeme_desc,
      malzeme_num,
      olcu_birim,
      malzeme_tur,
      mal_grup,
      fiyat,
      para_birimi,
      toplam,
      buy_no,
      status
      FROM zkar_t_malzeme2
      INTO TABLE @DATA(t_malzeme)
      WHERE status = '2'.

    DATA: lt_unique_malzeme TYPE TABLE OF zkar_t_malzeme2,
          ls_malzeme        TYPE zkar_t_malzeme2.

    LOOP AT t_malzeme ASSIGNING FIELD-SYMBOL(<fs_malzeme>).

      READ TABLE lt_unique_malzeme WITH KEY buy_no = <fs_malzeme>-buy_no TRANSPORTING NO FIELDS.

      IF sy-subrc <> 0.
        IF lv_min_buy_no IS INITIAL OR <fs_malzeme>-buy_no < lv_min_buy_no.
          lv_min_buy_no = <fs_malzeme>-buy_no.
        ENDIF.

        MOVE-CORRESPONDING <fs_malzeme> TO ls_malzeme.
        APPEND ls_malzeme TO lt_unique_malzeme.

        APPEND INITIAL LINE TO gt_malzeme2 ASSIGNING FIELD-SYMBOL(<malzeme>).
        <malzeme> = CORRESPONDING #( <fs_malzeme> ).

        <malzeme>-iban = 'Seciniz.'.

        IF <malzeme>-status EQ 2.
          <malzeme>-light = icon_yellow_light.
          <malzeme>-para_birimi = 'TRY'.
        ELSEIF <malzeme>-status EQ 4.
          <malzeme>-light = icon_red_light.
        ELSE.
          <malzeme>-light = icon_green_light.
        ENDIF.
        endif.
      ENDLOOP.

      muhasebe_odeme1( ).

      IF lv_min_buy_no IS NOT INITIAL.
    CALL METHOD get_data3
      EXPORTING
        iv_iban = 'Seciniz.'
        iv_buy_no = lv_min_buy_no.
  ENDIF.

    ENDMETHOD.


METHOD handle_hotspot_click.
    DATA: ls_row_data      TYPE lvc_s_roid,
          lv_row_index    TYPE sy-tabix,
          gs_malzeme      LIKE LINE OF gt_malzeme,
          gs_malzeme2      LIKE LINE OF gt_malzeme2,
          gs_list         TYPE zkar_t_malzeme2,
          lv_buy_no       TYPE zkar_t_malzeme2-buy_no.

    " Mevcut hücreyi al
    CALL METHOD go_alv2->get_current_cell
      IMPORTING
        es_row_no = ls_row_data.

    lv_row_index = ls_row_data-row_id.

    IF lv_row_index IS INITIAL.
      MESSAGE 'Lütfen geçerli bir satır seçiniz' TYPE 'I'.
      RETURN.
    ENDIF.

    READ TABLE gt_malzeme2 INTO gs_malzeme2 INDEX lv_row_index.
    IF sy-subrc = 0.
*      gs_list-malzeme_id = gs_malzeme2-malzeme_id.
       lv_buy_no = gs_malzeme2-buy_no.


      CALL METHOD get_data4
        EXPORTING
          iv_buy_no = lv_buy_no.
    ENDIF.




  ENDMETHOD.


METHOD handle_hotspot_click2.
    DATA: ls_row_data      TYPE lvc_s_roid,
          lv_row_index    TYPE sy-tabix,
          gs_malzeme      LIKE LINE OF gt_malzeme,
          gs_malzeme2      LIKE LINE OF gt_malzeme2,
          gs_list         TYPE zkar_t_malzeme2,
          lv_buy_no       TYPE zkar_t_malzeme2-buy_no.

    " Mevcut hücreyi al
    CALL METHOD go_alv3->get_current_cell
      IMPORTING
        es_row_no = ls_row_data.

    lv_row_index = ls_row_data-row_id.

    IF lv_row_index IS INITIAL.
      MESSAGE 'Lütfen geçerli bir satır seçiniz' TYPE 'I'.
      RETURN.
    ENDIF.

    READ TABLE gt_malzeme2 INTO gs_malzeme2 INDEX lv_row_index.
    IF sy-subrc = 0.
*      gs_list-malzeme_id = gs_malzeme2-malzeme_id.
       lv_buy_no = gs_malzeme2-buy_no.


      CALL METHOD get_data3
        EXPORTING
          iv_iban = gs_malzeme2-iban
          iv_buy_no = lv_buy_no.
    ENDIF.




  ENDMETHOD.


  METHOD kaydet.


*    authorization( ).

    DATA: gs_list TYPE zkar_t_malzeme2,
          gt_list TYPE TABLE OF zkar_t_malzeme2.

    IF iv_malzeme_id IS INITIAL.
      MESSAGE 'Malzeme ID boş olamaz!' TYPE 'E' .
    ELSEIF iv_malzeme_num IS INITIAL.
      MESSAGE 'Malzeme sayisi boş olamaz!' TYPE 'E' .
    ELSEIF iv_fiyat IS INITIAL.
      MESSAGE 'Malzeme fiyati boş olamaz!' TYPE 'E' .
    ENDIF.

    CLEAR gs_list.
    gs_list-malzeme_id   = iv_malzeme_id.
    gs_list-mal_grup     = iv_mal_grup.
    gs_list-malzeme_desc = iv_malzeme_desc.
    gs_list-malzeme_num  = iv_malzeme_num.
    gs_list-malzeme_tur  = iv_malzeme_tur.
    gs_list-olcu_birim   = iv_olcu_birim.
    gs_list-para_birimi  = iv_para_birimi.
    gs_list-fiyat        = iv_fiyat.
    gs_list-toplam       = iv_toplam.
    gs_list-status       = 0.

    DELETE FROM zkar_t_malzeme2
          WHERE status = 0.

    APPEND gs_list TO gt_list.
    MODIFY zkar_t_malzeme2 FROM TABLE gt_list.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      MESSAGE 'Data saved successfully.' TYPE 'S'.
    ELSE.
      MESSAGE 'Error saving data.' TYPE 'E'.
    ENDIF.

    get_data( iv_malzeme_id = gs_list-malzeme_id  ).

    go_alv->refresh_table_display(
      EXPORTING
        i_soft_refresh =      abap_true ).

  ENDMETHOD.


  method LOGO.

    CREATE OBJECT picture_control
      EXPORTING
        parent = go_cont2.

    CALL METHOD picture_control->load_picture_from_url
      EXPORTING
 url = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTAeXKXizA6mDeejjx8rnbSlL_JlNlvHqvBhg&s'.
call method picture_control->set_display_mode
  EXPORTING
    display_mode =    cl_gui_picture=>display_mode_normal_center  .

  ENDMETHOD.


  METHOD mail.

  ENDMETHOD.


  METHOD muhasebe_odeme.


    IF go_alv IS INITIAL.


      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_cont6.


*      CREATE OBJECT go_alv
*        EXPORTING
*          i_parent = cont_bottomm.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZKAR_MALZEME_S'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_fcat.
      LOOP AT gt_fcat INTO gs_fcat.
        CASE gs_fcat-fieldname.
          WHEN 'MALZEME_ID'.
            gs_fcat-scrtext_s = ' ID ' .
            gs_fcat-scrtext_m = ' MALZEME ID ' .
            gs_fcat-scrtext_l = ' MALZEME ID ' .
            gs_fcat-outputlen = 20.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_DESC'.
            gs_fcat-scrtext_s = ' ACIKLAMA ' .
            gs_fcat-scrtext_m = ' MALZEME ACIKLAMASI ' .
            gs_fcat-scrtext_l = ' MALZEME ACIKLAMASI ' .
            gs_fcat-outputlen = 20.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_NUM'.
            gs_fcat-scrtext_s = ' MIKTAR ' .
            gs_fcat-scrtext_m = ' MALZEME MIKTARI ' .
            gs_fcat-scrtext_l = ' MALZEME MKTARI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'OLCU_BIRIM'.
            gs_fcat-scrtext_s = ' OLCU' .
            gs_fcat-scrtext_m = ' OLCU BIRIMI ' .
            gs_fcat-scrtext_l = ' OLCU BIRIMI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_TUR'.
            gs_fcat-scrtext_s = ' M. TURU ' .
            gs_fcat-scrtext_m = ' MALZEME TUUR ' .
            gs_fcat-scrtext_l = ' MALZEME TURU ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MAL_GRUP'.
            gs_fcat-scrtext_s = ' GRUP ' .
            gs_fcat-scrtext_m = ' MAL GRUBU ' .
            gs_fcat-scrtext_l = ' MAL GRUBU ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'FIYAT'.
            gs_fcat-scrtext_s = ' FIYAT ' .
            gs_fcat-scrtext_m = ' FIYAT ' .
            gs_fcat-scrtext_l = ' FIYAT ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'PARA_BIRIMI'.
            gs_fcat-scrtext_s = ' BIRIM ' .
            gs_fcat-scrtext_m = ' PARA BIRIMI ' .
            gs_fcat-scrtext_l = ' PARA BIRIMI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'TOPLAM'.
            gs_fcat-scrtext_s = ' TOPLAM ' .
            gs_fcat-scrtext_m = ' TOPLAM TUTAR ' .
            gs_fcat-scrtext_l = ' TOPLAM TUTAR ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'BUY_NO'.
            gs_fcat-reptext   = ' SAS '.
            gs_fcat-scrtext_s = ' SAS ' .
            gs_fcat-scrtext_m = ' SAS ' .
            gs_fcat-scrtext_l = ' SAS ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.

          WHEN 'STATUS'.
            gs_fcat-no_out = abap_true.
            MODIFY gt_fcat FROM gs_fcat.
        ENDCASE.





      ENDLOOP.

      gs_layout-edit = abap_true.

*      SET HANDLER alv_button3 FOR go_alv.
*      SET HANDLER alv_usercommand3 FOR go_alv.
      SET HANDLER top_of_page FOR go_alv.


      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_MALZEME_S'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = gt_malzeme
          it_fieldcatalog  = gt_fcat.
    ENDIF.
  ENDMETHOD.


  METHOD muhasebe_odeme1.

    DATA: gt_ficat TYPE lvc_t_fcat,
          gs_ficat TYPE lvc_s_fcat.

    IF go_alv3 IS INITIAL.


      CREATE OBJECT go_alv3
        EXPORTING
          i_parent = go_cont5.


*      CREATE OBJECT go_alv
*        EXPORTING
*          i_parent = cont_bottomm.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZKAR_MALZEME_S'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_ficat.
      LOOP AT gt_ficat INTO gs_ficat.
        CASE gs_ficat-fieldname.
          WHEN 'MALZEME_ID'.
            gs_ficat-tech = 'X'.

            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_NUM'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'FIYAT'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'PARA_BIRIMI'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'TOPLAM'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_DESC'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'OLCU_BIRIM'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_TUR'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MAL_GRUP'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'BUY_NO'.
            gs_ficat-reptext   = ' SAS '.
            gs_ficat-scrtext_s = ' SAS ' .
            gs_ficat-scrtext_m = ' SAS ' .
            gs_ficat-scrtext_l = ' SAS ' .
            gs_ficat-outputlen = 10.
            gs_ficat-hotspot = abap_true.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'IBAN'.
            gs_ficat-reptext   = ' IBAN '.
            gs_ficat-scrtext_s = ' IBAN ' .
            gs_ficat-scrtext_m = ' IBAN ' .
            gs_ficat-scrtext_l = ' IBAN ' .
            gs_ficat-outputlen = 34.
            gs_ficat-edit = abap_true.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'STATUS'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.
        ENDCASE.

*        gs_ficat-hotspot = abap_true.

        MODIFY gt_ficat FROM gs_ficat.

      ENDLOOP.

      gs_layout-edit = abap_true.

*      SET HANDLER alv_button3 FOR go_alv3.
*      SET HANDLER alv_usercommand3 FOR go_alv3.
*      SET HANDLER top_of_page FOR go_alv.
      SET HANDLER handle_hotspot_click2 FOR go_alv3.


      CALL METHOD go_alv3->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_MALZEME_S'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = gt_malzeme2
          it_fieldcatalog  = gt_ficat.
    ENDIF.

  ENDMETHOD.


  method ONAYA_GONDER.
    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
        gs_list          TYPE zkar_t_malzeme2,
        gt_list          TYPE TABLE OF zkar_t_malzeme2,
        lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  LIKE LINE OF lt_selected_rows,
        lv_index         TYPE sy-tabix,
        answer           TYPE char1.

  DATA: go_gbt       TYPE REF TO cl_gbt_multirelated_service,
        go_bcs       TYPE REF TO cl_bcs,
        go_doc_bcs   TYPE REF TO cl_document_bcs,
        go_recipient TYPE REF TO if_recipient_bcs,
        gt_soli      TYPE TABLE OF soli,
        gs_soli      TYPE soli,
        gv_status    TYPE bcs_rqst.

  DATA: lv_number TYPE numc10.

  DATA:
        table        TYPE REF TO cl_salv_table.

  DATA: lt_content TYPE soli_tab.
  DATA: gv_content TYPE string.

  DATA: lt_selected_users TYPE TABLE OF zkar_t_onayci,
        lt_emails         TYPE TABLE OF ad_smtpadr,
        lv_email          TYPE AD_SMTPADR.

   DELETE FROM zkar_t_malzeme2
          WHERE status = 0.

      SELECT *
              FROM zkar_t_onayci
       INTO TABLE lt_selected_users
       WHERE muhasebeci = 'X'.

      LOOP AT lt_selected_users INTO DATA(ls_user).
        APPEND ls_user-mail TO lt_emails.
      ENDLOOP.
       CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZKAR_BUYNO'
            IMPORTING
              number      = lv_number.
          IF sy-subrc = 0.
             LOOP AT gt_malzeme INTO gs_malzeme.
            gs_malzeme-buy_no = lv_number.
            gs_malzeme-status = '1'.
            gs_malzeme-light = icon_green_light.

            MODIFY gt_malzeme  FROM gs_malzeme .

            READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id .
            IF sy-subrc = 0.
              gs_list-buy_no = lv_number.
              MODIFY gt_list FROM gs_list .
            ELSE.
              MOVE-CORRESPONDING gs_malzeme TO gs_list.
              gs_list-buy_no = lv_number.
              APPEND gs_list TO gt_list.
            ENDIF.

            ENDLOOP.
            MODIFY zkar_t_malzeme2 FROM TABLE gt_list.

          ENDIF.

        gv_content ='<!DOCTYPE html> '
  &&'<html> '
  &&'<head>'
  &&'<meta charset="utf-8">'
  &&'<style>'
  &&'th{'
  &&'background-color: lightgreen;'
  &&'border: 2px solid;'
  &&'}'
  &&'td{'
  &&'background-color: lightblue;'
  &&'border: 1px solid;'
  &&'}'
  &&'</style>'
  &&'</head>'
  &&'<body>'
  &&'<p>Sayın Yönetici,</p>'
  &&'<p>' && gs_list-buy_no && '  satin alma numarasıyla  ' && ' '  && gs_list-malzeme_desc && '  ürününe satin alma girişi yapılmıştır. Onayınızı beklemektedir.</p>'
  &&'<p>Bilginize,</p>'
  &&'<br><br>'
  &&'<table>'
  &&'<tr>'
  &&'<th>Malzeme ID</th>'
  &&'<th>Aciklama</th>'
  &&'<th>Malzeme Turu</th>'
  &&'<th>Mal Grubu</th>'
  &&'<th>Olcu Birimi</th>'
  &&'<th>Sayisi</th>'
  &&'<th>Birim Fiyati</th>'
  &&'<th>Toplam Fiyati</th>'
  &&'</tr>'.

        LOOP AT gt_list INTO gs_list.
          gv_content = gv_content &&'<tr>'
          &&'<td>' && gs_list-malzeme_id && '</td>'
          &&'<td>' && gs_list-malzeme_desc && '</td>'
          &&'<td>' && gs_list-malzeme_tur && '</td>'
          &&'<td>' && gs_list-mal_grup && '</td>'
          &&'<td>' && gs_list-olcu_birim && '</td>'
          &&'<td>' && gs_list-malzeme_num && '</td>'
          &&'<td>' && gs_list-fiyat && '</td>'
          &&'<td>' && gs_list-toplam && '</td>'
          &&'</tr>'.
        ENDLOOP.

        gv_content = gv_content &&'</table>'
          &&'</body>'
          &&'</html>'.

        CREATE OBJECT go_gbt.

        gt_soli = cl_document_bcs=>string_to_soli( gv_content ).

        CALL METHOD go_gbt->set_main_html
          EXPORTING
            content = gt_soli.

        go_doc_bcs = cl_document_bcs=>create_from_multirelated(
                       i_subject          = 'Satin Alma Girisi Hk.'
                       i_multirel_service = go_gbt ).


          LOOP AT lt_emails INTO lv_email.
            go_recipient = cl_cam_address_bcs=>create_internet_address(
                             i_address_string = lv_email  ).
          ENDLOOP.

          go_bcs = cl_bcs=>create_persistent( ).
          go_bcs->set_document( i_document = go_doc_bcs ).
          go_bcs->add_recipient( i_recipient = go_recipient ).

          go_bcs->send( ).
          COMMIT WORK.

        go_alv->refresh_table_display(
  EXPORTING
    i_soft_refresh = abap_true ).

  endmethod.


  METHOD onayci_bakim_tablosu.

    DATA: selected_users TYPE TABLE OF zkar_t_onayci.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action    = 'U'
        view_name = 'zkar_t_onayci'.

*    SELECT zusername
*      FROM zkar_t_onayci
*        INTO TABLE selected_users
*            WHERE checkbox = 'X'.

    CALL FUNCTION 'ENQUEUE_ES_PROG'
      EXPORTING
*       MODE_TRDIR     = 'E'
        name           = sy-repid
*       X_NAME         = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE  'Program baska bir kullanici tarafindan kullaniliyor.' TYPE 'I'.
      LEAVE PROGRAM.
    ENDIF.



  ENDMETHOD.


  method ONAYLA.


    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
          gs_list          TYPE zkar_t_malzeme2,
          gt_list          TYPE TABLE OF zkar_t_malzeme2,
          lt_selected_rows TYPE lvc_t_row,
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1,
          table            TYPE REF TO cl_salv_table,
          lv_body          TYPE string,
          msg              TYPE REF TO cl_bcs_message,
          lv_bin_filesize  TYPE i,
          lv_bin_file      TYPE xstring.

    DATA: fm_name TYPE rs38l_fnam,
          it_itab TYPE zkar_malzeme_tt.

    DATA: lt_binary_tab TYPE solix_tab,
*              lv_bin_filesize   TYPE so_obj_len,
          lv_bin_xstr   TYPE xstring,
          lt_lines_pdf  TYPE TABLE OF tline,
          lt_text       TYPE TABLE OF tline,
          ls_text       TYPE tline.

    DATA: lo_bcs           TYPE REF TO cl_bcs,
          lo_sapuser       TYPE REF TO cl_sapuser_bcs,
          lo_external_user TYPE REF TO cl_cam_address_bcs,
          lo_doc_bcs       TYPE REF TO cl_document_bcs,
          lo_recep         TYPE REF TO if_recipient_bcs,
          lo_cx_bcx        TYPE REF TO cx_bcs.

    DATA: ls_control_parameters TYPE ssfctrlop,
          ls_output_options     TYPE ssfcompop,
          ls_output_info        TYPE ssfcrescl,
          ls_job_output_info    TYPE ssfcrescl.

    DATA: lv_date_formatted TYPE string,
          lv_filename       TYPE string.

    DATA: lt_selected_users TYPE TABLE OF zkar_t_onayci,
          lt_emails         TYPE TABLE OF ad_smtpadr,
          lv_email          TYPE bcs_address.

        authorization_yonetici( ).
        SELECT *
               FROM zkar_t_onayci
        INTO TABLE lt_selected_users
        WHERE yonetici = 'X'.

        LOOP AT lt_selected_users INTO DATA(ls_user).
          APPEND ls_user-mail TO lt_emails.
        ENDLOOP.


        DATA(lv_day)   = sy-datum+6(2).
        DATA(lv_month) = sy-datum+4(2).
        DATA(lv_year)  = sy-datum(4).
        CONCATENATE lv_day lv_month lv_year INTO lv_date_formatted SEPARATED BY '/'.

        MESSAGE lv_date_formatted TYPE 'S'.

        IF sy-subrc = 0.
          LOOP AT gt_malzeme INTO gs_malzeme.
            gs_malzeme-status = '2'.
            gs_malzeme-light = icon_green_light.
            lv_filename = |{ lv_date_formatted }_{ gs_malzeme-buy_no }.pdf|.

            MODIFY gt_malzeme  FROM gs_malzeme .

            READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id.
            IF sy-subrc = 0.
              gs_list-status = '2'.
              MODIFY gt_list FROM gs_list.
            ELSE.
              MOVE-CORRESPONDING gs_malzeme TO gs_list.
              gs_list-status = '2'.
              APPEND gs_list TO gt_list.
            ENDIF.
          ENDLOOP.
          MODIFY zkar_t_malzeme2 FROM TABLE gt_list.

          DATA: lt_temp_list TYPE  zkar_malzeme_tt,
                ls_temp_list LIKE LINE OF lt_temp_list.

          LOOP AT gt_list INTO gs_list.
            MOVE-CORRESPONDING gs_list TO ls_temp_list.
            APPEND ls_temp_list TO lt_temp_list.
          ENDLOOP.

          CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
            EXPORTING
              formname = 'ZKAR_SF_02'
            IMPORTING
              fm_name  = fm_name.

          ls_control_parameters-preview = abap_true.
          ls_control_parameters-no_dialog = abap_true.
          ls_control_parameters-getotf = 'X'.
          ls_output_options-tddest = 'LP01' .

          CALL FUNCTION '/1BCDWB/SF00000155'
            EXPORTING
              control_parameters = ls_control_parameters
              output_options     = ls_output_options
              user_settings      = ' '
              gs_malzeme         = gs_malzeme
              lt_temp_list       = lt_temp_list
            IMPORTING
              job_output_info    = ls_job_output_info.

          IF sy-subrc <> 0.
            MESSAGE 'SmartForm fail' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.

          IF ls_job_output_info-otfdata[] IS INITIAL.
            MESSAGE 'İçeriğe ulaşalımadı.' TYPE 'E' DISPLAY LIKE 'S'.
          ENDIF.

          CALL FUNCTION 'CONVERT_OTF'
            EXPORTING
              format                = 'PDF'
            IMPORTING
              bin_filesize          = lv_bin_filesize
              bin_file              = lv_bin_file
            TABLES
              otf                   = ls_job_output_info-otfdata
              lines                 = lt_lines_pdf
            EXCEPTIONS
              err_max_linewidth     = 1
              err_format            = 2
              err_conv_not_possible = 3
              OTHERS                = 4.

          IF sy-subrc IS INITIAL.
            CREATE OBJECT msg.
            msg->add_attachment(
                  EXPORTING
                    iv_doctype      = 'PDF'
                    iv_filename     = lv_filename
                    iv_contents_bin = lv_bin_file ).
          ENDIF.

          lv_body = |Sayin Muhasebe Yetkilisi, | &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |'{ gs_list-buy_no }' Satin alma numarasiyla satin alma girisi onaylanmistir. | &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |Satin alma icin odemeniz beklenmektedir| &&
                    cl_abap_char_utilities=>cr_lf && " New line
                    |Bilginize|.

          msg->set_subject( |Satin Alma Onaylandi.| ).
          msg->set_main_doc( iv_contents_txt = lv_body ).
          LOOP AT lt_emails INTO lv_email.
            msg->add_recipient(
            EXPORTING
              iv_address      =    lv_email          ).
          ENDLOOP.
          msg->set_sender( iv_address = 'no-reply@ides.com' ).

          TRY.
              msg->send( ).
              MESSAGE 'Mail gonderimi basarili.' TYPE 'I'.
            CATCH cx_bcs.
              MESSAGE 'Mail gonderimi basarisiz.' TYPE 'E'.
          ENDTRY.

        ENDIF.


        go_alv->refresh_table_display(
          EXPORTING
            i_soft_refresh = abap_true ).

*        get_data2( ).
*
* go_alv2->refresh_table_display(
*          EXPORTING
*            i_soft_refresh = abap_true ).
  endmethod.


METHOD on_sapevent.
  CASE action.
    WHEN 'MY_EVENT_1'.
*      get_data( ).
*      satin_alma_girisi( ).
*      SET SCREEN 0200.
      CALL TRANSACTION 'ZKAR_BUTTON1'.
    WHEN 'MY_EVENT_2'.
*      get_data2( ).
*      satin_alma_onayi( ).
*      satin_alma_onayi1( ).
*      set screen 0300.
      CALL TRANSACTION 'ZKAR_BUTTON2'.
    WHEN 'MY_EVENT_3'.
*      get_data3( ).
*      muhasebe_odeme( ).
*      SET SCREEN 0400.
      CALL TRANSACTION 'ZKAR_BUTTON3'.
    WHEN 'MY_EVENT_4'.
      onayci_bakim_tablosu( ).
    WHEN 'MY_EVENT_5'.
      CALL TRANSACTION 'ZKAR_BUTTON5'.
  ENDCASE.
ENDMETHOD.


  METHOD rapor.
    DATA:
          gs_layout2 TYPE lvc_s_layo.


    SELECT * FROM zkar_t_malzeme2 INTO CORRESPONDING FIELDS OF TABLE gt_rapor.

    DATA: gt_ficat TYPE lvc_t_fcat,
          gs_ficat TYPE lvc_s_fcat.

    IF go_alv4 IS INITIAL.
      CREATE OBJECT go_alv4
        EXPORTING
          i_parent = cl_gui_container=>screen0.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZKAR_MALZEME_S'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_ficat.

      LOOP AT gt_ficat INTO gs_ficat.
        CASE gs_ficat-fieldname.
          WHEN 'BUY_NO'.
            gs_ficat-reptext   = ' SAS '.
            gs_ficat-scrtext_s = ' SAS ' .
            gs_ficat-scrtext_m = ' SAS ' .
            gs_ficat-scrtext_l = ' SAS ' .
            gs_ficat-col_pos = 1 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'STATUS'.
            gs_ficat-reptext   = ' STATUS '.
            gs_ficat-scrtext_s = ' STATUS ' .
            gs_ficat-scrtext_m = ' STATUS ' .
            gs_ficat-scrtext_l = ' STATUS ' .
            gs_ficat-col_pos = 12 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_ID'.
            gs_ficat-reptext   = ' ID '.
            gs_ficat-scrtext_s = ' ID ' .
            gs_ficat-scrtext_m = ' MALZEME ID ' .
            gs_ficat-scrtext_l = ' MALZEME ID ' .
            gs_ficat-col_pos = 2 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 20.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_DESC'.
            gs_ficat-reptext   = ' ACIKLAMA '.
            gs_ficat-scrtext_s = ' ACIKLAMA ' .
            gs_ficat-scrtext_m = ' MALZEME ACIKLAMASI ' .
            gs_ficat-scrtext_l = ' MALZEME ACIKLAMASI ' .
            gs_ficat-col_pos = 3 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 20.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_NUM'.
            gs_ficat-reptext   = ' MIKTAR '.
            gs_ficat-scrtext_s = ' MIKTAR ' .
            gs_ficat-scrtext_m = ' MALZEME MIKTARI ' .
            gs_ficat-scrtext_l = ' MALZEME MKTARI ' .
            gs_ficat-col_pos = 7.
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'OLCU_BIRIM'.
            gs_ficat-reptext   = ' OLCU B. '.
            gs_ficat-scrtext_s = ' OLCU' .
            gs_ficat-scrtext_m = ' OLCU BIRIMI ' .
            gs_ficat-scrtext_l = ' OLCU BIRIMI ' .
            gs_ficat-col_pos = 6 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_TUR'.
            gs_ficat-reptext   = ' TUR '.
            gs_ficat-scrtext_s = ' M. TURU ' .
            gs_ficat-scrtext_m = ' MALZEME TUUR ' .
            gs_ficat-scrtext_l = ' MALZEME TURU ' .
            gs_ficat-col_pos = 4 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MAL_GRUP'.
            gs_ficat-reptext   = ' GRUP '.
            gs_ficat-scrtext_s = ' GRUP ' .
            gs_ficat-scrtext_m = ' MAL GRUBU ' .
            gs_ficat-scrtext_l = ' MAL GRUBU ' .
            gs_ficat-col_pos = 5 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'FIYAT'.
            gs_ficat-reptext   = ' FIYAT '.
            gs_ficat-scrtext_s = ' FIYAT ' .
            gs_ficat-scrtext_m = ' FIYAT ' .
            gs_ficat-scrtext_l = ' FIYAT ' .
            gs_ficat-col_pos = 9 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'PARA_BIRIMI'.
            gs_ficat-reptext   = ' PARA B. '.
            gs_ficat-scrtext_s = ' BIRIM ' .
            gs_ficat-scrtext_m = ' PARA BIRIMI ' .
            gs_ficat-scrtext_l = ' PARA BIRIMI ' .
            gs_ficat-col_pos = 8 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'TOPLAM'.
            gs_ficat-reptext   = ' TOPLAM '.
            gs_ficat-scrtext_s = ' TOPLAM ' .
            gs_ficat-scrtext_m = ' TOPLAM TUTAR ' .
            gs_ficat-scrtext_l = ' TOPLAM TUTAR ' .
            gs_ficat-col_pos = 10 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'IBAN'.
            gs_ficat-reptext   = ' IBAN '.
            gs_ficat-scrtext_s = ' IBAN ' .
            gs_ficat-scrtext_m = ' IBAN ' .
            gs_ficat-scrtext_l = ' IBAN ' .
            gs_ficat-col_pos = 11 .
            gs_ficat-just = 'C'.
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN'LIGHT'.
            gs_ficat-tech = abap_true.
            MODIFY gt_ficat FROM gs_ficat.
        ENDCASE.

      ENDLOOP.
      gs_layout2-zebra = abap_true.
*      gs_layout2- = 'C'.

      CALL METHOD go_alv4->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_MALZEME_S'
          is_layout        = gs_layout2
        CHANGING
          it_outtab        = gt_rapor
          it_fieldcatalog  = gt_ficat.

    ENDIF.




  ENDMETHOD.


  method REDDET.


    DATA: gs_malzeme       LIKE LINE OF gt_malzeme,
          gs_list          TYPE zkar_t_malzeme2,
          gt_list          TYPE TABLE OF zkar_t_malzeme2,
          lt_selected_rows TYPE lvc_t_row,
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1,
          table            TYPE REF TO cl_salv_table,
          lv_body          TYPE string,
          msg              TYPE REF TO cl_bcs_message,
          lv_bin_filesize  TYPE i,
          lv_bin_file      TYPE xstring.

    DATA: fm_name TYPE rs38l_fnam,
          it_itab TYPE zkar_malzeme_tt.

    DATA: lt_binary_tab TYPE solix_tab,
*              lv_bin_filesize   TYPE so_obj_len,
          lv_bin_xstr   TYPE xstring,
          lt_lines_pdf  TYPE TABLE OF tline,
          lt_text       TYPE TABLE OF tline,
          ls_text       TYPE tline.

    DATA: lo_bcs           TYPE REF TO cl_bcs,
          lo_sapuser       TYPE REF TO cl_sapuser_bcs,
          lo_external_user TYPE REF TO cl_cam_address_bcs,
          lo_doc_bcs       TYPE REF TO cl_document_bcs,
          lo_recep         TYPE REF TO if_recipient_bcs,
          lo_cx_bcx        TYPE REF TO cx_bcs.

    DATA: ls_control_parameters TYPE ssfctrlop,
          ls_output_options     TYPE ssfcompop,
          ls_output_info        TYPE ssfcrescl,
          ls_job_output_info    TYPE ssfcrescl.

    DATA: lv_date_formatted TYPE string,
          lv_filename       TYPE string.

    DATA: lt_selected_users TYPE TABLE OF zkar_t_onayci,
          lt_emails         TYPE TABLE OF ad_smtpadr,
          lv_email          TYPE bcs_address.


        authorization_yonetici( ).


*        CALL METHOD go_alv->get_selected_rows
*          IMPORTING
*            et_index_rows = lt_selected_rows.
*
*        DESCRIBE TABLE lt_selected_rows LINES lv_index.

*        IF lv_index IS INITIAL.
*          MESSAGE 'Lutfen gecerli bir satır seçiniz' TYPE 'I'.
*          RETURN.
*        ENDIF.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = ' deneme'
            text_question         = 'Islemi reddetmek istedigine emin misin?'
            text_button_1         = 'EVET'(001)
            icon_button_1         = 'ICON_CHECKED '
            text_button_2         = 'HAYIR'(002)
            icon_button_2         = 'CON_CANCEL '
            display_cancel_button = 'X'
            popup_type            = 'ICON_MESSAGE_ERROR'
          IMPORTING
            answer                = answer.
        CHECK answer EQ 1.

*        LOOP AT lt_selected_rows INTO ls_selected_row.
*          lv_index = ls_selected_row-index.
*          READ TABLE gt_malzeme INTO gs_malzeme INDEX lv_index.
        LOOP AT gt_malzeme INTO gs_malzeme.
          gs_malzeme-status = 4.
          gs_malzeme-light = icon_red_light.
          MODIFY gt_malzeme FROM gs_malzeme .
          READ TABLE gt_list INTO gs_list WITH KEY malzeme_id = gs_malzeme-malzeme_id.
          IF sy-subrc = 0.
            gs_list-status = gs_malzeme-status.
            MODIFY gt_list FROM gs_list.
          ELSE.
            MOVE-CORRESPONDING gs_malzeme TO gs_list.
            gs_list-status = gs_malzeme-status.
            APPEND gs_list TO gt_list.
          ENDIF.
          endloop.
          MODIFY zkar_t_malzeme2 FROM TABLE gt_list.
*        ENDLOOP.

*        MODIFY zkar_t_malzeme FROM TABLE gt_list.
        go_alv->refresh_table_display( ).

*        DELETE gt_malzeme WHERE status = 4.

*        go_alv->refresh_table_display( ).


  endmethod.


  METHOD satin_alma_girisi.
*    MESSAGE 'butona basildi' TYPE 'I'.



    IF go_alv IS INITIAL.


      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_cont.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZKAR_MALZEME_S'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_fcat.
      LOOP AT gt_fcat INTO gs_fcat.
        CASE gs_fcat-fieldname.
          WHEN 'MALZEME_ID'.
            gs_fcat-scrtext_s = ' ID ' .
            gs_fcat-scrtext_m = ' MALZEME ID ' .
            gs_fcat-scrtext_l = ' MALZEME ID ' .
            gs_fcat-outputlen = 20.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_DESC'.
            gs_fcat-scrtext_s = ' ACIKLAMA ' .
            gs_fcat-scrtext_m = ' MALZEME ACIKLAMASI ' .
            gs_fcat-scrtext_l = ' MALZEME ACIKLAMASI ' .
            gs_fcat-outputlen = 20.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_NUM'.
            gs_fcat-scrtext_s = ' MIKTAR ' .
            gs_fcat-scrtext_m = ' MALZEME MIKTARI ' .
            gs_fcat-scrtext_l = ' MALZEME MKTARI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'OLCU_BIRIM'.
            gs_fcat-scrtext_s = ' OLCU' .
            gs_fcat-scrtext_m = ' OLCU BIRIMI ' .
            gs_fcat-scrtext_l = ' OLCU BIRIMI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_TUR'.
            gs_fcat-scrtext_s = ' M. TURU ' .
            gs_fcat-scrtext_m = ' MALZEME TUUR ' .
            gs_fcat-scrtext_l = ' MALZEME TURU ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MAL_GRUP'.
            gs_fcat-scrtext_s = ' GRUP ' .
            gs_fcat-scrtext_m = ' MAL GRUBU ' .
            gs_fcat-scrtext_l = ' MAL GRUBU ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'FIYAT'.
            gs_fcat-scrtext_s = ' FIYAT ' .
            gs_fcat-scrtext_m = ' FIYAT ' .
            gs_fcat-scrtext_l = ' FIYAT ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'PARA_BIRIMI'.
            gs_fcat-scrtext_s = ' BIRIM ' .
            gs_fcat-scrtext_m = ' PARA BIRIMI ' .
            gs_fcat-scrtext_l = ' PARA BIRIMI ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'TOPLAM'.
            gs_fcat-scrtext_s = ' TOPLAM ' .
            gs_fcat-scrtext_m = ' TOPLAM TUTAR ' .
            gs_fcat-scrtext_l = ' TOPLAM TUTAR ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'BUY_NO'.
            gs_fcat-reptext   = ' SAS '.
            gs_fcat-scrtext_s = ' SAS ' .
            gs_fcat-scrtext_m = ' SAS ' .
            gs_fcat-scrtext_l = ' SAS ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'STATUS'.
            gs_fcat-no_out = abap_true.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'IBAN'.
            gs_fcat-tech = abap_true.
            MODIFY gt_fcat FROM gs_fcat.
        ENDCASE.

      ENDLOOP.
*      gs_layout-cwidth_opt = abap_true.
      gs_layout-edit = abap_true.
*      gs_layout-excp_fname = 'STATUS'.

      SET HANDLER alv_button FOR go_alv.
      SET HANDLER alv_usercommand FOR go_alv.
      SET HANDLER data_changed FOR go_alv.
*      SET HANDLER data_changed_finish FOR go_alv.


      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_MALZEME_S'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = gt_malzeme
          it_fieldcatalog  = gt_fcat.
    ENDIF.



  ENDMETHOD.


  METHOD satin_alma_onayi.

    DATA: gt_ficat TYPE lvc_t_fcat,
          gs_ficat TYPE lvc_s_fcat.

    IF go_alv IS INITIAL.


      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_cont4.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZKAR_MALZEME_S'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_ficat.
      LOOP AT gt_ficat INTO gs_ficat.
        CASE gs_ficat-fieldname.
          WHEN 'MALZEME_ID'.
            gs_ficat-scrtext_s = ' ID ' .
            gs_ficat-scrtext_m = ' MALZEME ID ' .
            gs_ficat-scrtext_l = ' MALZEME ID ' .
            gs_ficat-outputlen = 20.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_DESC'.
            gs_ficat-scrtext_s = ' ACIKLAMA ' .
            gs_ficat-scrtext_m = ' MALZEME ACIKLAMASI ' .
            gs_ficat-scrtext_l = ' MALZEME ACIKLAMASI ' .
            gs_ficat-outputlen = 20.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'OLCU_BIRIM'.
            gs_ficat-scrtext_s = ' OLCU' .
            gs_ficat-scrtext_m = ' OLCU BIRIMI ' .
            gs_ficat-scrtext_l = ' OLCU BIRIMI ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_TUR'.
            gs_ficat-scrtext_s = ' M. TURU ' .
            gs_ficat-scrtext_m = ' MALZEME TUUR ' .
            gs_ficat-scrtext_l = ' MALZEME TURU ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MAL_GRUP'.
            gs_ficat-scrtext_s = ' GRUP ' .
            gs_ficat-scrtext_m = ' MAL GRUBU ' .
            gs_ficat-scrtext_l = ' MAL GRUBU ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'PARA_BIRIMI'.
            gs_ficat-scrtext_s = ' BIRIM ' .
            gs_ficat-scrtext_m = ' PARA BIRIMI ' .
            gs_ficat-scrtext_l = ' PARA BIRIMI ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'MALZEME_NUM'.
            gs_ficat-scrtext_s = ' MIKTAR ' .
            gs_ficat-scrtext_m = ' MALZEME MIKTARI ' .
            gs_ficat-scrtext_l = ' MALZEME MKTARI ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'FIYAT'.
            gs_ficat-scrtext_s = ' FIYAT ' .
            gs_ficat-scrtext_m = ' FIYAT ' .
            gs_ficat-scrtext_l = ' FIYAT ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'TOPLAM'.
            gs_ficat-scrtext_s = ' TOPLAM ' .
            gs_ficat-scrtext_m = ' TOPLAM TUTAR ' .
            gs_ficat-scrtext_l = ' TOPLAM TUTAR ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'BUY_NO'.
            gs_ficat-reptext   = ' SAS '.
            gs_ficat-scrtext_s = ' SAS ' .
            gs_ficat-scrtext_m = ' SAS ' .
            gs_ficat-scrtext_l = ' SAS ' .
            gs_ficat-outputlen = 10.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'STATUS'.
            gs_ficat-no_out = abap_true.
            MODIFY gt_ficat FROM gs_ficat.
          WHEN 'IBAN'.
            gs_ficat-tech = 'X'.
            MODIFY gt_ficat FROM gs_ficat.

        ENDCASE.

      ENDLOOP.
*      gs_layout-cwidth_opt = abap_true.
      gs_layout-edit = abap_true.
*      gs_layout-excp_fname = 'STATUS'.

*      SET HANDLER alv_button2 FOR go_alv.
*      SET HANDLER alv_usercommand2 FOR go_alv.
*      SET HANDLER data_changed FOR go_alv.
*      SET HANDLER data_changed_finish FOR go_alv.


      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_MALZEME_S'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = gt_malzeme
          it_fieldcatalog  = gt_ficat.
    ENDIF.


  ENDMETHOD.


  METHOD satin_alma_onayi1.

    DATA: gt_list TYPE TABLE OF zkar_t_malzeme2,
          gs_list TYPE zkar_t_malzeme2.

*    FIELD-SYMBOLS: <gfs_fcat> TYPE lvc_t_fcat.

    IF go_alv2 IS INITIAL.


      CREATE OBJECT go_alv2
        EXPORTING
          i_parent = go_cont3.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'zkar_malzeme_s'
          i_client_never_display = 'X'
        CHANGING
          ct_fieldcat            = gt_fcat.
      LOOP AT gt_fcat INTO gs_fcat.
        CASE gs_fcat-fieldname.
          WHEN 'MALZEME_ID'.
            gs_fcat-tech = 'X'.

            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_NUM'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'FIYAT'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'PARA_BIRIMI'.
           gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'TOPLAM'.
           gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_DESC'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'OLCU_BIRIM'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MALZEME_TUR'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'MAL_GRUP'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'BUY_NO'.
            gs_fcat-reptext   = ' SAS '.
            gs_fcat-scrtext_s = ' SAS ' .
            gs_fcat-scrtext_m = ' SAS ' .
            gs_fcat-scrtext_l = ' SAS ' .
            gs_fcat-outputlen = 10.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'IBAN'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
          WHEN 'STATUS'.
            gs_fcat-tech = 'X'.
            MODIFY gt_fcat FROM gs_fcat.
        ENDCASE.

          gs_fcat-hotspot = abap_true.

          MODIFY gt_fcat FROM gs_fcat.


      ENDLOOP.


        gs_layout-edit = abap_true.
*      gs_layout-no_toolbar = abap_true.
*      gs_layout-

        CLEAR gs_list.
        LOOP AT gt_list INTO gs_list.
          gs_list-para_birimi = 'TRY'.

          MODIFY gt_list FROM gs_list.
          MODIFY zkar_t_malzeme2 FROM gs_list.
        ENDLOOP.

*        SET HANDLER alv_button4 FOR go_alv2.
*        SET HANDLER alv_usercommand4 FOR go_alv2.
        SET HANDLER handle_hotspot_click FOR go_alv2.

        CALL METHOD go_alv2->set_table_for_first_display
          EXPORTING
            i_structure_name = 'ZKAR_MALZEME_S'
            is_layout        = gs_layout
          CHANGING
            it_outtab        = gt_malzeme2
            it_fieldcatalog  = gt_fcat.
      ENDIF.


    ENDMETHOD.


  method TEMIZLE.

    clear: gt_malzeme.
    DELETE FROM zkar_t_malzeme2
          WHERE status = 0.

    call method satin_alma_girisi.
    go_alv->refresh_table_display( ).
  endmethod.


  METHOD top_of_page.

    DATA:
          lv_text TYPE sdydo_text_element.

    lv_text = 'Deneme'.

    go_docu->add_text(
      EXPORTING
        text          =   lv_text
        sap_style     =   cl_dd_document=>heading
).

    go_docu->new_line(
*        repeat =
    ).

    CLEAR: lv_text.
    CONCATENATE 'User: ' sy-uname INTO lv_text SEPARATED BY space.
    go_docu->add_text(
      EXPORTING
        text          =     lv_text
        sap_color     =     cl_dd_document=>list_positive
        sap_fontsize  =     cl_dd_document=>medium
 ).
    go_docu->display_document(
      EXPORTING
        parent             = cont_top
      EXCEPTIONS
        html_display_error = 1
        OTHERS             = 2
    ).



  ENDMETHOD.
ENDCLASS.
