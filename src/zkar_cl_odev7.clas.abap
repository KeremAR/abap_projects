class ZKAR_CL_ODEV7 definition
  public
  final
  create public .

public section.

  data GO_ALV type ref to CL_GUI_ALV_GRID .
  data GO_CONT type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data GO_GUI1 type ref to CL_GUI_CONTAINER .
  data GT_FCAT type LVC_T_FCAT .
  data GS_FCAT type LVC_S_FCAT .
  data GS_LAYOUT type LVC_S_LAYO .
  data PICTURE_CONTROL type ref to CL_GUI_PICTURE .
  data GO_GUI2 type ref to CL_GUI_CONTAINER .
  data CALENDAR_CONTROL type ref to CL_GUI_CALENDAR .
  data GO_GUI3 type ref to CL_GUI_CONTAINER .
  data GT_PLAN type ZKAR_PLAN_TT .
  data ITAB_ALV type ZKAR_PLAN_TT .
  data GT_PLANLAR type ZKAR_PLAN_TT .
  data SELECTED_BEGIN_DATE type CNCA_UTC_DATE .
  data SELECTED_END_DATE type DATUM .
  data LV_TARIH type DATS .
  data GT_RENKLER type ZKAR_SIRKET_TT .
  data GO_CONT2 type ref to CL_GUI_CUSTOM_CONTAINER .
  data CONT_LOGO type ref to CL_GUI_CONTAINER .
  data CC_ALV type ref to CL_GUI_CONTAINER .
  data CONT_CALENDAR type ref to CL_GUI_CONTAINER .
  data GO_CONT4 type ref to CL_GUI_CUSTOM_CONTAINER .
  data GO_CONT1 type ref to CL_GUI_CUSTOM_CONTAINER .
  data CONT_CHART type ref to CL_GUI_CONTAINER .
  data GO_HTML type ref to CL_GUI_HTML_VIEWER .

  methods CONSTRUCTOR .
  methods CONTAINER .
  methods LOGO .
  methods CALENDAR .
  methods CHART .
  methods GET_DATA .
  methods ALV .
  methods DATE_SELECTED
    for event DATE_SELECTED of CL_GUI_CALENDAR
    importing
      !DATE_BEGIN
      !DATE_END
      !SELECTION_TABLE .
  methods ALV_BUTTON
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT .
  methods ALV_USERCOMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods CALENDAR_COLORED .
  methods USER_AUTH .
  methods MAIL .
protected section.
private section.
ENDCLASS.



CLASS ZKAR_CL_ODEV7 IMPLEMENTATION.


  METHOD alv.

*    CALL METHOD go_splitter->get_container
*      EXPORTING
*        row       = 2
*        column    = 2
*      RECEIVING
*        container = go_gui1.

    IF go_alv IS INITIAL.


      CREATE OBJECT go_alv
        EXPORTING
          i_parent = go_cont.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'Zkar_PLAN_s'
          i_client_never_display = abap_true
        CHANGING
          ct_fieldcat            = gt_fcat.

      LOOP AT gt_fcat INTO gs_fcat.
        CASE gs_fcat-fieldname.
          WHEN 'KULLANICIADI'.
            gs_fcat-scrtext_s = ' USERNAME ' .
            gs_fcat-scrtext_m = ' USERNAME ' .
            gs_fcat-scrtext_l = ' USERNAME ' .
*            gs_fcat-tech = abap_true.
            MODIFY gt_fcat FROM gs_fcat.

          WHEN 'SELKZ'.
            gs_fcat-tech = abap_true.
            MODIFY gt_fcat FROM gs_fcat.

        ENDCASE.
      ENDLOOP.

      gs_layout-box_fname = 'SELKZ'.
      gs_layout-edit_mode = abap_true.
      gs_layout-edit = abap_true.
      gs_layout-cwidth_opt = abap_true.
*      gs_layout-no_toolbar = abap_true.

      SET HANDLER alv_button FOR go_alv.
      SET HANDLER alv_usercommand FOR go_alv.


      CALL METHOD go_alv->set_table_for_first_display
        EXPORTING
          i_structure_name = 'ZKAR_PLAN_S'
          is_layout        = gs_layout
        CHANGING
          it_outtab        = gt_plan
          it_fieldcatalog  = gt_fcat.
else.
  go_alv->refresh_table_display(
*    EXPORTING
*      is_stable      =                  " With Stable Rows/Columns
*      i_soft_refresh =                  " Without Sort, Filter, etc.
).
    ENDIF.


  ENDMETHOD.


  METHOD alv_button.

    DATA: ls_toolbar TYPE stb_button.

    DELETE e_object->mt_toolbar WHERE function NE '&LOCAL&INSERT_ROW'.
    DELETE e_object->mt_toolbar WHERE function EQ '&LOCAL&INSERT_ROW'.

    CLEAR: ls_toolbar.

    ls_toolbar-function = '&EKLE'.
    ls_toolbar-text = 'Satir Ekle'.
    ls_toolbar-icon = '@17@'.
    ls_toolbar-quickinfo = 'Satir Ekleme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = '&SIL'.
    ls_toolbar-text = 'Satir Sil'.
    ls_toolbar-icon = '@18@'.
    ls_toolbar-quickinfo = 'Silme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.


    ls_toolbar-function = '&KAYDET'.
    ls_toolbar-text = 'Kaydet'.
    ls_toolbar-icon = '@2L@'.
    ls_toolbar-quickinfo = 'Kaydetme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    ls_toolbar-function = '&MAIL'.
    ls_toolbar-text = 'MAIL'.
    ls_toolbar-icon = '@1S@'.
    ls_toolbar-quickinfo = 'Mail Gonderme Islemi'.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.


  METHOD alv_usercommand.
    DATA: gs_plan          LIKE LINE OF gt_plan,
          gs_list          TYPE zkar_t_plan,
          lt_selected_rows TYPE lvc_t_row,  " Correct type for selected rows
          ls_selected_row  LIKE LINE OF lt_selected_rows,
          lv_index         TYPE sy-tabix,
          answer           TYPE char1.

    DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
          ls_fcat   TYPE slis_fieldcat_alv,
          gs_layout TYPE slis_layout_alv.

    TYPES: BEGIN OF zemail_structure,
             name_text TYPE adrp-name_text,
             smtp_addr TYPE adr6-smtp_addr,
           END OF zemail_structure.

    DATA: it_email     TYPE TABLE OF zemail_structure,
          rv_email     TYPE zemail_structure,
          mc_date_from TYPE adrp-date_from.

    DATA: lv_recipient TYPE string.
    DATA: lv_date_formatted TYPE string.

    CASE e_ucomm.
      WHEN '&EKLE'.
        CLEAR gs_plan.
        gs_plan-personel  = ''.
        gs_plan-departman     = ''.
        gs_plan-sirket    = ''.
        gs_plan-tarih = selected_begin_date.
        gs_plan-kullaniciadi = ''.
        APPEND gs_plan TO gt_plan.
        go_alv->refresh_table_display(
          EXPORTING
            i_soft_refresh = abap_true ).

      WHEN '&KAYDET'.
        CLEAR gs_plan.
        LOOP AT gt_plan INTO gs_plan.

          SELECT SINGLE kullaniciadi
        INTO gs_list-kullaniciadi
        FROM zkar_t_personel
        WHERE peronel = gs_plan-personel.

          IF sy-subrc <> 0.
            MESSAGE 'Personel için kullanıcı adı bulunamadı!' TYPE 'E'.
            CONTINUE.
          ENDIF.

          gs_list-personel     = gs_plan-personel .
          gs_list-departman     = gs_plan-departman  .
          gs_list-sirket       = gs_plan-sirket   .
          gs_list-tarih        = gs_plan-tarih   .

          APPEND gs_list TO gt_planlar.
        ENDLOOP.
        MODIFY zkar_t_plan FROM TABLE gt_planlar.

        get_data( ).

        go_alv->refresh_table_display( ).

        calendar_colored( ).
        chart( ).
        MESSAGE 'Kayıt İşlemi Başarıyla Tamamlanmıştır.' TYPE 'I'.

      WHEN '&SIL'.
        CALL METHOD go_alv->get_selected_rows
          IMPORTING
            et_index_rows = lt_selected_rows.  " Correct type for selected rows
        DESCRIBE TABLE lt_selected_rows LINES lv_index .

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
          READ TABLE gt_plan INTO gs_plan INDEX lv_index.
          IF sy-subrc = 0.
            " Delete from database table
            DELETE FROM zkar_t_plan
              WHERE tarih      = gs_plan-tarih
                AND kullaniciadi = gs_plan-kullaniciadi
                AND personel   = gs_plan-personel
                AND departman  = gs_plan-departman
                AND sirket     = gs_plan-sirket.

            " Delete from internal table
            DELETE gt_plan INDEX lv_index.
          ENDIF.
        ENDLOOP.
        get_data( ).

        calendar_colored( ).
        chart( ).
        go_alv->refresh_table_display( ).

     WHEN '&MAIL'.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.  " Correct type for selected rows

  IF lt_selected_rows IS INITIAL.
    MESSAGE 'No rows selected' TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT lt_selected_rows INTO ls_selected_row.
    lv_index = ls_selected_row-index.
    READ TABLE gt_plan INTO gs_plan INDEX lv_index.
    IF sy-subrc = 0.
      CLEAR rv_email.

      " Format the date correctly
      DATA(lv_day)   = gs_plan-tarih+6(2).
      DATA(lv_month) = gs_plan-tarih+4(2).
      DATA(lv_year)  = gs_plan-tarih(4).
      CONCATENATE lv_day lv_month lv_year INTO lv_date_formatted SEPARATED BY '.'.

      " Get the recipient's email address
      SELECT SINGLE adrp~name_text, adr6~smtp_addr
        FROM usr21
        LEFT JOIN adr6
          ON usr21~addrnumber = adr6~addrnumber AND
             usr21~persnumber = adr6~persnumber
        LEFT JOIN adrp
          ON usr21~persnumber = adrp~persnumber AND
             adrp~date_from   = @mc_date_from   AND
             adrp~nation      = ''
        WHERE usr21~bname = @gs_plan-kullaniciadi
          AND adr6~smtp_addr IS NOT INITIAL
        INTO @rv_email.

      IF sy-subrc = 0.
        lv_recipient = rv_email-smtp_addr.

        " Create email object
        DATA: msg TYPE REF TO cl_bcs_message.
        CREATE OBJECT msg.

        msg->set_subject( |{ gs_plan-personel } icin { lv_date_formatted } tarihli gunun plani.| ).

        DATA(lv_body) = |Sayin { gs_plan-personel }, gunun plani: | &&
                        cl_abap_char_utilities=>cr_lf && " New line
                        |Departman: { gs_plan-departman } | &&
                        cl_abap_char_utilities=>cr_lf && " New line
                        |Sirket: { gs_plan-sirket } | &&
                        cl_abap_char_utilities=>cr_lf && " New line
                        |Tarih: { lv_date_formatted } |.

        msg->set_main_doc( iv_contents_txt = lv_body ).

        msg->add_recipient( lv_recipient ).

        msg->set_sender( iv_address = 'no-reply@ides.com' ).

        TRY.
            msg->send( ).
            MESSAGE 'Mail gonderimi basarili.' TYPE 'I'.
          CATCH cx_bcs.
            MESSAGE 'Mail gonderimi basarisiz.' TYPE 'E'.
        ENDTRY.

      ELSE.
        MESSAGE 'Gecerli mail adresi bulunamadi.' TYPE 'E'.
      ENDIF.

    ENDIF.
  ENDLOOP.
endcase.

  ENDMETHOD.


  METHOD calendar.

*    CALL METHOD go_splitter->get_container
*      EXPORTING
*        row       = 2
*        column    = 1
*      RECEIVING
*        container = go_gui3.

    CREATE OBJECT calendar_control
      EXPORTING
        parent          = go_cont4        " Container
        view_style      = '1026'
        selection_style = '15'.

    DATA: it_events TYPE cntl_simple_events.

    it_events = VALUE #( ( eventid = cl_gui_calendar=>m_id_date_selected
                           appl_event = abap_true ) ).

    calendar_control->set_registered_events( events = it_events ).

    SET HANDLER date_selected FOR calendar_control.

  ENDMETHOD.


  METHOD calendar_colored.

    DATA: day_info   TYPE TABLE OF cnca_day_info,
          gs_info    LIKE LINE OF day_info,
          gs_plan    LIKE LINE OF gt_planlar,
          gs_renkler LIKE LINE OF gt_renkler.

    calendar_control->reset_day_info( ).

    CLEAR day_info.

    LOOP AT gt_planlar INTO gs_plan.
      gs_info-date = gs_plan-tarih.
      READ TABLE gt_renkler INTO gs_renkler WITH KEY departman = gs_plan-departman.
      IF sy-subrc = 0.
        gs_info-color =  gs_renkler-renk.
        gs_info-text = 'Planli'.
        APPEND gs_info TO day_info.
      ENDIF.
    ENDLOOP.

    calendar_control->set_day_info(
      EXPORTING
        day_info   =  day_info            " Table Contains Color and Tool Tip Information

    ).
  ENDMETHOD.


METHOD chart.

  DATA: doc_url(80), lt_html TYPE TABLE OF w3_html.
  TYPES: BEGIN OF ty_chart,

           sirket TYPE zkar_sirket_de,
           toplam TYPE i,
         END OF ty_chart.

  DATA: gs_chart TYPE ty_chart,
        gt_chart TYPE TABLE OF ty_chart.


  DATA: gs_personel TYPE zkar_t_plan,
        gt_personel TYPE TABLE OF zkar_t_plan,
        lv_len_per  TYPE i,
        lv_len_pln  TYPE i.


  IF go_html IS NOT BOUND.

    CREATE OBJECT go_html
      EXPORTING
        parent = go_cont1.
  ENDIF.

  SELECT * FROM zkar_t_plan INTO TABLE @gt_personel.

  LOOP AT gt_personel  INTO gs_personel.
    lv_len_per = lv_len_per + 1.
  ENDLOOP.

*    SELECT COUNT(*), sirket
*      FROM zkar_t_plan
*       WHERE zkar_t_plan~tarih = @lv_tarih
*         GROUP BY sirket INTO  TABLE @gt_chart.

  SELECT p~sirket ,COUNT(*)
       FROM @gt_planlar AS p
       WHERE p~tarih = @selected_begin_date
       GROUP BY p~sirket
    INTO TABLE @gt_chart.


  LOOP AT gt_chart INTO gs_chart.
    lv_len_pln = lv_len_pln + gs_chart-toplam.
  ENDLOOP.

  DEFINE add_html.
    APPEND &1 TO lt_html.
  END-OF-DEFINITION.

  add_html:
  '<html>',
  '<body>',
  '<h2>Personel Plan Grafiği</h2>',

  '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>',
  '<div id="piechart" style="width: 600px; height: 350px;"></div>',

  '<script>',
    'google.charts.load(''current'', {''packages'':[''corechart'']});',
    'google.charts.setOnLoadCallback(drawChart);',

    'function drawChart() {',
      'var data = google.visualization.arrayToDataTable([',
      '[''Şirket'', ''Çalışan''],'.

  CLEAR gs_chart.
  LOOP AT gt_chart INTO gs_chart.
    APPEND | [' { gs_chart-sirket } ' , { gs_chart-toplam } ] , | TO lt_html.
  ENDLOOP.

*  IF lv_len_per > lv_len_pln.
*    lv_len_per = lv_len_per - lv_len_pln.
*    APPEND | [' { 'PLANSIZ' } ' , { lv_len_per } ] , | TO lt_html.
*  ENDIF.

  add_html:
       ']);',
       'var options = {',
         'title: '' ''',
       '};',
       'var chart = new google.visualization.PieChart(document.getElementById(''piechart''));',
       'chart.draw(data, options);',
     '}',
 '</script>',
 '</body>',
 '</html> '.

  go_html->load_data(
IMPORTING
assigned_url = doc_url
CHANGING
data_table = lt_html ).

  go_html->show_url(
    EXPORTING
      url = doc_url ).

ENDMETHOD.


  METHOD constructor.

    user_auth( ).

    container( ).

    calendar( ).

    logo( ).

    get_data( ).

    calendar_colored( ).

  ENDMETHOD.


  METHOD container.

    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CC_ALV'.
*
    CREATE OBJECT go_cont2
      EXPORTING
        container_name = 'CONT_LOGO'.

    CREATE OBJECT go_cont4
      EXPORTING
        container_name = 'CONT_CALENDAR'.

        CREATE OBJECT go_cont1
      EXPORTING
        container_name = 'CONT_CHART'.

*    CREATE OBJECT go_splitter
*      EXPORTING
*        parent  = go_cont
*        rows    = 2
*        columns = 2.


  ENDMETHOD.


  METHOD date_selected.

    DATA: ls_plan LIKE LINE OF gt_plan.

    CLEAR itab_alv.

    LOOP AT gt_plan
      INTO ls_plan
      WHERE tarih BETWEEN date_begin
      AND date_end
      AND kullaniciadi = sy-uname.
      APPEND ls_plan TO itab_alv.

    ENDLOOP.

    selected_begin_date = date_begin.
    selected_end_date = date_end.
*

    alv( ).
    get_data( ).
    chart( ).


  ENDMETHOD.


  METHOD get_data.

    SELECT * FROM zkar_t_plan INTO CORRESPONDING FIELDS OF TABLE gt_planlar.
    SELECT * FROM zkar_t_sirket INTO CORRESPONDING FIELDS OF TABLE gt_renkler.

    SELECT
      kullaniciadi,
      personel,
      sirket,
      departman,
      tarih
      FROM zkar_t_plan
      INTO CORRESPONDING FIELDS OF TABLE @gt_plan
      WHERE tarih EQ @selected_begin_date.
*  WRITE: / 'Selected Begin Date:', selected_begin_date.

    alv( ).

  ENDMETHOD.


  METHOD logo.



    CREATE OBJECT picture_control
      EXPORTING
        parent = go_cont2.

    CALL METHOD picture_control->load_picture_from_url
      EXPORTING
        url = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSITUVCryKCk7vPAu0SA8ia91TAJDJnTo5z-Q&s'.

call method picture_control->set_display_mode
  EXPORTING
    display_mode =    cl_gui_picture=>display_mode_fit_center  .

  ENDMETHOD.


  method MAIL.


  endmethod.


  method USER_AUTH.

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


  endmethod.
ENDCLASS.
