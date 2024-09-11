*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV09_CLIMP
*&---------------------------------------------------------------------*

CLASS lcl_alv IMPLEMENTATION.

  METHOD set_layout.
    gs_layout-cwidth_opt = abap_true.
  ENDMETHOD.


  METHOD set_fc.
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'RBUKRS'.
    gs_fieldcat-reptext = 'Şirket Kodu'.
    gs_fieldcat-scrtext_s = 'Şirket Kodu'.
    gs_fieldcat-scrtext_m = 'Şirket Kodu'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'KZWRS'.
    gs_fieldcat-reptext = 'Para Birimi'.
    gs_fieldcat-scrtext_s = 'Para Birimi'.
    gs_fieldcat-scrtext_m = 'Para Birimi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'KURSF'.
    gs_fieldcat-reptext = 'Kur Bilgisi'.
    gs_fieldcat-scrtext_s = 'Kur Bilgisi'.
    gs_fieldcat-scrtext_l = 'Kur Bilgisi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'BLART'.
    gs_fieldcat-reptext = 'Belge Türü'.
    gs_fieldcat-scrtext_s = 'Belge Türü'.
    gs_fieldcat-scrtext_m = 'Belge Türü'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'BUDAT'.
    gs_fieldcat-reptext = 'Kayıt Tarihi'.
    gs_fieldcat-scrtext_s = 'Kayıt Tarihi'.
    gs_fieldcat-scrtext_m = 'Kayıt Tarihi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'BLDAT'.
    gs_fieldcat-reptext = 'Belge Tarihi'.
    gs_fieldcat-scrtext_s = 'Belge Tarihi'.
    gs_fieldcat-scrtext_m = 'Belge Tarihi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'SGTXT'.
    gs_fieldcat-reptext = 'Başlık Bilgisi'.
    gs_fieldcat-scrtext_s = 'Başlık Bilgisi'.
    gs_fieldcat-scrtext_m = 'Başlık Bilgisi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'XBLNR'.
    gs_fieldcat-reptext = 'Referans'.
    gs_fieldcat-scrtext_s = 'Referans'.
    gs_fieldcat-scrtext_m = 'Referans'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'BSCHL_A'.
    gs_fieldcat-reptext = 'BA g-Göstergesi'.
    gs_fieldcat-scrtext_s = 'BA g-Göstergesi'.
    gs_fieldcat-scrtext_m = 'BA g-Göstergesi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'RACCT_A'.
    gs_fieldcat-reptext = 'Ana Hesap'.
    gs_fieldcat-scrtext_s = 'Ana Hesap'.
    gs_fieldcat-scrtext_m = 'Ana Hesap'.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.

    gs_fieldcat-fieldname = 'MWSKZ_A'.
    gs_fieldcat-reptext = 'Vergi Göstergesi'.
    gs_fieldcat-scrtext_s = 'Vergi Göstergesi'.
    gs_fieldcat-scrtext_m = 'Vergi Göstergesi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'RCNTR_A'.
    gs_fieldcat-reptext = 'Masraf Yeri'.
    gs_fieldcat-scrtext_s = 'Masraf Yeri'.
    gs_fieldcat-scrtext_m = 'Masraf Yeri'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'PRCTR_A'.
    gs_fieldcat-reptext = 'Kar Merkezi'.
    gs_fieldcat-scrtext_s = 'Kar Merkezi'.
    gs_fieldcat-scrtext_m = 'Kar Merkezi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'AUFNR_A'.
    gs_fieldcat-reptext = 'Sipariş'.
    gs_fieldcat-scrtext_s = 'Sipariş'.
    gs_fieldcat-scrtext_m = 'Sipariş'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'ZUONR_A'.
    gs_fieldcat-reptext = 'Tayin'.
    gs_fieldcat-scrtext_s = 'Tayin'.
    gs_fieldcat-scrtext_m = 'Tayin'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'SGTXT_A'.
    gs_fieldcat-reptext = 'Baslik Bilgisi'.
    gs_fieldcat-scrtext_s = 'Baslik Bilgisi'.
    gs_fieldcat-scrtext_m = 'Baslik Bilgisi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'VKORG_A'.
    gs_fieldcat-reptext = 'Satış org'.
    gs_fieldcat-scrtext_s = 'Satış org'.
    gs_fieldcat-scrtext_m = 'Satış org'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'VTWEG_A'.
    gs_fieldcat-reptext = 'Dağıtım kanalı'.
    gs_fieldcat-scrtext_s = 'Dağıtım kanalı'.
    gs_fieldcat-scrtext_m = 'Dağıtım kanalı'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'SPART_A'.
    gs_fieldcat-reptext = 'Bölüm'.
    gs_fieldcat-scrtext_s = 'Bölüm'.
    gs_fieldcat-scrtext_m = 'Bölüm'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'ARTNR_A'.
    gs_fieldcat-reptext = 'Ürün'.
    gs_fieldcat-scrtext_s = 'Ürün'.
    gs_fieldcat-scrtext_m = 'Ürün'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'WERKS_A'.
    gs_fieldcat-reptext = 'Üretim yeri'.
    gs_fieldcat-scrtext_s = 'Üretim yeri'.
    gs_fieldcat-scrtext_m = 'Üretim yeri'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'KNDNR_a'.
    gs_fieldcat-reptext = 'Müşteri'.
    gs_fieldcat-scrtext_m = 'Müşteri'.
    gs_fieldcat-scrtext_m = 'Müşteri'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'BSCHL_B'.
    gs_fieldcat-reptext = 'BA g-Göstergesi'.
    gs_fieldcat-scrtext_s = 'BA g-Göstergesi'.
    gs_fieldcat-scrtext_m = 'BA g-Göstergesi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = 'RACCT_B'.
    gs_fieldcat-reptext = 'Ana Hesap'.
    gs_fieldcat-scrtext_s = 'Ana Hesap'.
    gs_fieldcat-scrtext_m = 'Ana Hesap'.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.

    gs_fieldcat-fieldname = 'MWSKZ_B'.
    gs_fieldcat-reptext = 'Vergi Göstergesi'.
    gs_fieldcat-scrtext_s = 'Vergi Göstergesi'.
    gs_fieldcat-scrtext_m = 'Vergi Göstergesi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'RCNTR_B'.
    gs_fieldcat-reptext = 'Masraf Yeri'.
    gs_fieldcat-scrtext_s = 'Masraf Yeri'.
    gs_fieldcat-scrtext_m = 'Masraf Yeri'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'PRCTR_B'.
    gs_fieldcat-reptext = 'Kar Merkezi'.
    gs_fieldcat-scrtext_s = 'Kar Merkezi'.
    gs_fieldcat-scrtext_m = 'Kar Merkezi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'AUFNR_B'.
    gs_fieldcat-reptext = 'Sipariş'.
    gs_fieldcat-scrtext_s = 'Sipariş'.
    gs_fieldcat-scrtext_m = 'Sipariş'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'ZUONR_B'.
    gs_fieldcat-reptext = 'Tayin'.
    gs_fieldcat-scrtext_s = 'Tayin'.
    gs_fieldcat-scrtext_m = 'Tayin'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'SGTXT_B'.
    gs_fieldcat-reptext = 'Baslik Bilgisi'.
    gs_fieldcat-scrtext_s = 'Baslik Bilgisi'.
    gs_fieldcat-scrtext_m = 'Baslik Bilgisi'.
    APPEND gs_fieldcat TO gt_fieldcat.

    gs_fieldcat-fieldname = 'HSL_C'.
    gs_fieldcat-reptext = 'Tutar'.
    gs_fieldcat-scrtext_s = 'Tutar'.
    gs_fieldcat-scrtext_m = 'Tutar'.
    APPEND gs_fieldcat TO gt_fieldcat.

*    gs_fieldcat-fieldname = 'XBLNR'.
*    gs_fieldcat-scrtext_m = 'BELGE NO'.
*    APPEND gs_fieldcat TO gt_fieldcat.

  ENDMETHOD.

  METHOD display_alv.
    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CONT_1'.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_cont.

    go_alv->set_table_for_first_display(
      EXPORTING
        i_structure_name              = 'zkar_odev9_s'
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_list
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4
    ).


  ENDMETHOD.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD app_instance.
*    FREE: go_main, app.
    IF lcl_main=>app IS NOT BOUND.
      CREATE OBJECT lcl_main=>app.
    ENDIF.
    go_main = app.

  ENDMETHOD.

  METHOD load_of_program.
    lcl_main=>app = lcl_main=>app_instance( ).


  ENDMETHOD.

  METHOD initialization.
    but1 = 'excel template.'.
*    SET PF-STATUS '1000'.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      IF screen-group1 = 'GR1' OR screen-group1 = 'GR2'.
        IF p_r1 = 'X' AND screen-group1 <> 'GR1'.
          screen-active = 0.
        ELSEIF p_r2 = 'X' AND screen-group1 <> 'GR2'.
          screen-active = 0.
        ELSEIF p_r3 = 'X' AND screen-group1 <> 'GR2'.
          screen-active = 0.
        ELSE .
          screen-active = 1.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

    CLEAR: lt_listbox.
    ls_listbox-key = '01'.
    ls_listbox-text = '(40-50) Anahtarı İle Kayıt Atma'.
    APPEND ls_listbox TO lt_listbox. ls_listbox-key = '2'.

    ls_listbox-text = '(01-50) Müşteri Borç Virman'.
    APPEND ls_listbox TO lt_listbox.

    ls_listbox-text = '(15-40) Müşteri Alacak Virman'.
    APPEND ls_listbox TO lt_listbox.

    ls_listbox-text = '(25-50) Satıcı Borç Virman'.
    APPEND ls_listbox TO lt_listbox.

    ls_listbox-text = '(31-40) Satıcı Alacak Virman'.
    APPEND ls_listbox TO  lt_listbox.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'PS_PARM'
        values = lt_listbox.

  ENDMETHOD.

  METHOD at_selection_screen.
*    CASE sy-ucomm.
*      WHEN'&EXECUTE' .
*      go_main->start_of_selection( ).
*      WHEN '&BACK'.
*        LEAVE to screen 1000.
*      when '&EXCEL'.
*        GO_MAIN->excel_create( ).
*    ENDCASE.
    IF sy-ucomm EQ 'EXCEL'.

      app->excel_create( ).
    ENDIF.



  ENDMETHOD.

  METHOD start_of_selection.

    app->excel_to_alv( ).

    IF p_r2 = 'X'.
      app->get_data( ).
      app->bapi_on( ).
    ELSEIF p_r3 = 'X'.
      app->get_data( ).
      app->bapi_gercek( ).
    ENDIF.

    lcl_alv=>set_fc( ).
    lcl_alv=>set_layout( ).
    lcl_alv=>display_alv( ).


  ENDMETHOD.

  METHOD end_of_selection.
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD file.
    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        field_name = 'P_FILE'
      IMPORTING
        file_name  = p_file.

  ENDMETHOD.

  METHOD excel_to_alv.
    DATA:
      lt_excel_data TYPE TABLE OF alsmex_tabline,
      ls_excel_data TYPE  alsmex_tabline.


    FIELD-SYMBOLS: <gfv_value> TYPE any.


    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = 256
        i_end_row               = 10
      TABLES
        intern                  = lt_excel_data
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CLEAR: gs_excel, gt_excel.

    LOOP AT lt_excel_data INTO ls_excel_data.
      ASSIGN COMPONENT ls_excel_data-col OF STRUCTURE gs_excel TO <gfv_value>.
      <gfv_value> = ls_excel_data-value.
      AT END OF row.
        APPEND gs_excel TO gt_excel.
      ENDAT.
    ENDLOOP.

    LOOP AT gt_excel INTO DATA(ls_excel).

      gs_list-bschl_a  = ls_excel-bschl.
      gs_list-racct_a  = ls_excel-racct.
      gs_list-mwskz_a  = ls_excel-mwskz.
      gs_list-rcntr_a  = ls_excel-rcntr.
      gs_list-prctr_a  = ls_excel-prctr.
      gs_list-aufnr_a  = ls_excel-aufnr.
      gs_list-zuonr_a  = ls_excel-zuonr.
      gs_list-sgtxt_a  = ls_excel-sgtxt.
      gs_list-vkorg_a  = ls_excel-vkorg.
      gs_list-vtweg_a  = ls_excel-vtweg.
      gs_list-spart_a  = ls_excel-spart.
      gs_list-artnr_a  = ls_excel-artnr.
      gs_list-werks_a  = ls_excel-werks.
      gs_list-kndnr_a  = ls_excel-kndnr.

      gs_list-bschl_b  = ls_excel-bschl_a.
      gs_list-racct_b  = ls_excel-racct_a.
      gs_list-mwskz_b  = ls_excel-mwskz_a.
      gs_list-rcntr_b  = ls_excel-rcntr_a.
      gs_list-prctr_b  = ls_excel-prctr_a.
      gs_list-aufnr_b  = ls_excel-aufnr_a.
      gs_list-zuonr_b  = ls_excel-zuonr_a.
      gs_list-sgtxt_b  = ls_excel-sgtxt_a.
      gs_list-hsl_c    = ls_excel-hsl_a.
      gs_list-xblnr   = ls_excel-xblnr.


      APPEND gs_list TO gt_list.
      CLEAR gs_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD kaydet.

    DATA: lt_zkar_t_odev9 TYPE TABLE OF zkar_t_odev9,
          ls_zkar_t_odev9 TYPE zkar_t_odev9.

    LOOP AT gt_list INTO DATA(ls_list).

      MOVE-CORRESPONDING ls_list TO ls_zkar_t_odev9.
      INSERT zkar_t_odev9 FROM ls_zkar_t_odev9.
      IF sy-subrc <> 0.
        MESSAGE 'Kayıtlar başarıyla kaydedildi.' TYPE 'S'.
      ENDIF.

      CLEAR ls_zkar_t_odev9.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_data.

    gs_list-rbukrs   = p_bukrs.
    gs_list-kzwrs    = p_waers.
    gs_list-kursf    = p_kursf.
    gs_list-blart    = p_blart.
    gs_list-budat    = p_budat.
    gs_list-bldat    = p_bldat.
    gs_list-sgtxt    = p_sgtxt.
    gs_list-xblnr    = p_xblnr.

  ENDMETHOD.



  METHOD excel_create.
    DATA: ls_wwwdata_item TYPE wwwdatatab.

    SELECT SINGLE *
      INTO CORRESPONDING FIELDS OF ls_wwwdata_item
      FROM wwwdata
      WHERE objid = 'ZKAR_TEMPLATE_02'.
    IF sy-subrc = 0.
      CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
        EXPORTING
          key       = ls_wwwdata_item
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
    ELSE.
      WRITE: / 'Template not found in WWWDATA table.'.
    ENDIF.


  ENDMETHOD.

  METHOD bapi_on.

  ENDMETHOD.

  METHOD bapi_gercek.
    DATA: lv_index   TYPE i,
          i          TYPE i,
          lt_mess    TYPE TABLE OF bapiret2,
          ls_mess    TYPE bapiret2,
          lo_alv     TYPE REF TO cl_salv_table,
          lr_func    TYPE REF TO cl_salv_functions_list,
          lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column  TYPE REF TO cl_salv_column_table,
          lv_num1    TYPE num10,
          lv_num2    TYPE ce_karel.

    LOOP AT  gt_list INTO gs_list.
      REFRESH: gt_accountgl, gt_criteria, gt_return, gt_currencyamo.
      CLEAR: docheader, lv_num1, lv_num2.

      lv_index = 1.

      IF docheader IS INITIAL.
        CLEAR: docheader.

        docheader-bus_act    = 'RFBU'.
        docheader-username   = sy-uname.
        docheader-comp_code  = gs_list-rbukrs.
        docheader-doc_type   = gs_list-blart.
        docheader-doc_date   = gs_list-bldat.
        docheader-pstng_date = gs_list-budat.
        docheader-header_txt = gs_list-sgtxt.

      ENDIF.

      " First Item: Debit (AccountGL)
      CLEAR gs_accountgl.
      lv_index = lv_index + 1.
      gs_accountgl-comp_code = gs_list-rbukrs.
      gs_accountgl-itemno_acc = lv_index.
      gs_accountgl-gl_account = gs_list-racct_a.  " Map Account from ALV
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_accountgl-gl_account
        IMPORTING
          output = gs_accountgl-gl_account.

      gs_accountgl-costcenter = gs_list-rcntr_a.  " Cost Center from ALV
      gs_accountgl-profit_ctr = gs_list-prctr_a.  " Profit Center from ALV
      gs_accountgl-orderid    = gs_list-aufnr_a.  " Order Number from ALV
      APPEND gs_accountgl TO gt_accountgl.

      " Currency Amount
      CLEAR gs_currencyamo.
      gs_currencyamo-itemno_acc  = lv_index.
      gs_currencyamo-curr_type   = '00'.
      gs_currencyamo-currency    = gs_list-kursf.  " Currency
      gs_currencyamo-amt_doccur  = gs_list-hsl_c.  " Amount from ALV
      APPEND gs_currencyamo TO gt_currencyamo.

      " Second Item: Credit (AccountGL)
      CLEAR gs_accountgl.
      lv_index = lv_index + 1.
      gs_accountgl-comp_code = gs_list-rbukrs.
      gs_accountgl-itemno_acc = lv_index.
      gs_accountgl-gl_account = gs_list-racct_b.  " Credit GL Account from ALV
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_accountgl-gl_account
        IMPORTING
          output = gs_accountgl-gl_account.

      gs_accountgl-item_text = gs_list-sgtxt_b.  " Item Text
      gs_accountgl-tax_code  = gs_list-mwskz_b.  " Tax Code
      gs_accountgl-costcenter = gs_list-rcntr_b.  " Cost Center for Credit
      APPEND gs_accountgl TO gt_accountgl.

      " Currency Amount for Credit
      CLEAR gs_currencyamo.
      gs_currencyamo-itemno_acc = lv_index.
      gs_currencyamo-curr_type  = '00'.
      gs_currencyamo-currency   = gs_list-kursf.  " Currency
      gs_currencyamo-amt_doccur = gs_list-hsl_c * -1.  " Negative for Credit
      APPEND gs_currencyamo TO gt_currencyamo.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = docheader
*         CUSTOMERCPD    =
*         CONTRACTHEADER =
        IMPORTING
          obj_type       = docheader-obj_type
          obj_key        = docheader-obj_key
          obj_sys        = docheader-obj_sys
        TABLES
          accountgl      = gt_accountgl
*         ACCOUNTRECEIVABLE       =
*         ACCOUNTPAYABLE =
*         ACCOUNTTAX     =
          currencyamount = gt_currencyamo
          criteria       = gt_criteria
*         VALUEFIELD     =
*         EXTENSION1     =
          return         = gt_return
*         PAYMENTCARD    =
*         CONTRACTITEM   =
*         EXTENSION2     =
*         REALESTATE     =
*         ACCOUNTWT      =
        .

      LOOP AT gt_return TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        gs_list-belnr = docheader-obj_key(10).
        MODIFY gt_list FROM gs_list.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
