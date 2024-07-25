*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV05_FRM
*&---------------------------------------------------------------------*

FORM get_data .
  SELECT
    t001~bukrs,
    t001~butxt,
    bnka~banka,
    t012~hbkid,
    t012k~hktid,
    t012t~text1,
    t012k~waers,
    t012k~hkont,
    skat~txt50
*    faglflext~tslxx
*    faglflext~hslxx
    FROM t001
    INNER JOIN t012 ON t012~bukrs = t001~bukrs
    INNER JOIN bnka ON bnka~banks = t012~banks AND
                       bnka~bankl = t012~bankl
    INNER JOIN t012k ON t012k~bukrs = t012~bukrs AND
                        t012k~hbkid = t012~hbkid
    LEFT JOIN skat ON  skat~spras = 'TR' AND
                       skat~ktopl = t001~ktopl AND
                       skat~saknr = t012k~hkont
    LEFT JOIN t012t ON t012t~bukrs = t012k~bukrs AND
                       t012t~hbkid = t012k~hbkid AND
                       t012t~hktid = t012k~hktid AND
                       t012t~spras = 'TR'
    INTO CORRESPONDING FIELDS OF TABLE @gt_struc
       WHERE t001~bukrs  EQ @p_bukrs
         AND t012~hbkid  IN @s_hbkid
         AND t012k~waers IN @s_waers.

  TYPES: BEGIN OF ty_faglflext,
           rbukrs TYPE faglflext-rbukrs,
           hslvt  TYPE faglflext-hslvt,
           hsl01  TYPE faglflext-hsl01,
           hsl02  TYPE faglflext-hsl02,
           hsl03  TYPE faglflext-hsl03,
           hsl04  TYPE faglflext-hsl04,
           hsl05  TYPE faglflext-hsl05,
           hsl06  TYPE faglflext-hsl06,
           hsl07  TYPE faglflext-hsl07,
           hsl08  TYPE faglflext-hsl08,
           hsl09  TYPE faglflext-hsl09,
           hsl10  TYPE faglflext-hsl10,
           hsl11  TYPE faglflext-hsl11,
           hsl12  TYPE faglflext-hsl12,
           tslvt  TYPE faglflext-tslvt,
           tsl01  TYPE faglflext-tsl01,
           tsl02  TYPE faglflext-tsl02,
           tsl03  TYPE faglflext-tsl03,
           tsl04  TYPE faglflext-tsl04,
           tsl05  TYPE faglflext-tsl05,
           tsl06  TYPE faglflext-tsl06,
           tsl07  TYPE faglflext-tsl07,
           tsl08  TYPE faglflext-tsl08,
           tsl09  TYPE faglflext-tsl09,
           tsl10  TYPE faglflext-tsl10,
           tsl11  TYPE faglflext-tsl11,
           tsl12  TYPE faglflext-tsl12,
           hbkid  TYPE t012k-hbkid,
           racct  TYPE faglflext-racct,
         END OF ty_faglflext.

  DATA: gt_faglflext TYPE TABLE OF ty_faglflext,
        gs_faglflext TYPE ty_faglflext.

  SELECT
    *
    FROM faglflext
    INNER JOIN t012k ON t012k~hkont = faglflext~racct
    INTO CORRESPONDING FIELDS OF TABLE gt_faglflext
      WHERE ryear = p_gjahr  AND
            rbukrs = p_bukrs AND
            rldnr = '0L'.


  LOOP AT gt_struc INTO gs_struc.

    LOOP AT gt_faglflext INTO gs_faglflext WHERE  hbkid = gs_struc-hbkid AND racct = gs_struc-hkont .

      gs_struc-hslxx = gs_struc-hslxx + gs_faglflext-hslvt + gs_faglflext-hsl01 + gs_faglflext-hsl02 +
                       gs_faglflext-hsl03 + gs_faglflext-hsl04 + gs_faglflext-hsl05 +
                       gs_faglflext-hsl06 + gs_faglflext-hsl07 + gs_faglflext-hsl08 +
                       gs_faglflext-hsl09 + gs_faglflext-hsl10 + gs_faglflext-hsl11 + gs_faglflext-hsl12.

      gs_struc-tslxx =  gs_struc-tslxx + gs_faglflext-tslvt + gs_faglflext-tsl01 + gs_faglflext-tsl02 +
                        gs_faglflext-tsl03 + gs_faglflext-tsl04 + gs_faglflext-tsl05 +
                        gs_faglflext-tsl06 + gs_faglflext-tsl07 + gs_faglflext-tsl08 +
                        gs_faglflext-tsl09 + gs_faglflext-tsl10 + gs_faglflext-tsl11 + gs_faglflext-tsl12.

    ENDLOOP.

    MODIFY gt_struc FROM gs_struc.

  ENDLOOP.
ENDFORM.

FORM set_layout .

ENDFORM.
FORM display_alv .

  CREATE OBJECT go_cont
    EXPORTING
      container_name = 'CC_ALV'.

  CREATE OBJECT go_splitter
    EXPORTING
      parent  = go_cont
      rows    = 2
      columns = 1.

*1. satir container
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_gui1.

*2. satir container
  CALL METHOD go_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_gui2.

  CREATE OBJECT go_alv
    EXPORTING
      i_parent = go_gui1.

  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout             " Layout
    CHANGING
      it_outtab       = gt_struc         " Output Table
      it_fieldcatalog = gt_fcat.

*2. satir container
  CREATE OBJECT go_html
    EXPORTING
      parent = go_gui2.

  PERFORM html_doldur.

  CALL METHOD go_html->load_data
    IMPORTING
      assigned_url = doc_url
    CHANGING
      data_table   = gt_html.

*  CALL METHOD go_html->show_url
*    EXPORTING
*      url = 'google.com'.

  CALL METHOD go_html->show_url
    EXPORTING
      url = doc_url.




ENDFORM.


FORM set_fc_sub USING p_fieldname p_scrtext_s p_scrtext_m p_scrtext_l p_key p_col_pos.
  gs_fcat-fieldname = p_fieldname.
  gs_fcat-scrtext_s = p_scrtext_s.
  gs_fcat-scrtext_m = p_scrtext_m.
  gs_fcat-scrtext_l = p_scrtext_l.
  gs_fcat-key       = p_key.
  gs_fcat-col_pos   = p_col_pos.
  gs_fcat-col_opt = abap_true.
  APPEND gs_fcat TO gt_fcat.
ENDFORM.

FORM set_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZKAR_ODEV5_S01'
    CHANGING
      ct_fieldcat            = gt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
    CASE <fcat>-fieldname.
      WHEN 'BUKRS'.
        <fcat>-tech = abap_true.
    ENDCASE.
    <fcat>-col_opt = abap_true.

  ENDLOOP.







*  PERFORM: set_fc_sub USING 'BUKRS' 'SIRKET KODU' 'SIRKET KODU' 'SIRKET KODU' 'X' '0' ,
*           set_fc_sub USING 'BUTXT' 'SIRKET KODU TANIMI' 'SIRKET KODU TANIMI' 'SIRKET KODU TANIMI' '' '1' ,
*           set_fc_sub USING 'BANKA' 'BANKA ADI' 'BANKA ADI' 'BANKA ADI' '' '2' ,
*           set_fc_sub USING 'HBKID' 'ANA BANKA' 'ANA BANKA' 'ANA BANKA' '' '3' ,
*           set_fc_sub USING 'HKTID' 'HESAP TANITICISI' 'HESAP TANITICISI' 'HESAP TANITICISI' '' '4' ,
*           set_fc_sub USING 'TEXT1' 'ANA BANKA TANIM' 'ANA BANKA TANIM' 'ANA BANKA TANIM' '' '5' ,
*           set_fc_sub USING 'WAERS' 'PARA BIRIMI' 'PARA BIRIMI' 'PARA BIRIMI' '' '6' ,
*           set_fc_sub USING 'HKONT' 'ANA HESAP' 'ANA HESAP' 'ANA HESAP' '' '7' ,
*           set_fc_sub USING 'TXT50' 'HESAP TANIMI' 'HESAP TANIMI' 'HESAP TANIMI' '' '8' ,
*           set_fc_sub USING 'TSLXX' 'BAKIYE' 'BAKIYE' 'BAKIYE' '' '9' ,
*           set_fc_sub USING 'HSLXX' 'BAKIYE(UP)' 'BAKIYE(UP)' 'BAKIYE(UP)' '' '10' .
ENDFORM.


FORM html_doldur .
  TYPES: BEGIN OF ty_chart,
           toplam TYPE i,
           banka  TYPE bnka-banka,
         END OF ty_chart.

  DATA: gt_chart TYPE TABLE OF ty_chart,
        gs_chart TYPE ty_chart.

  DEFINE add_html.
    APPEND &1 TO gt_html.
  END-OF-DEFINITION.

  add_html:
  '<html>',
  '<body>',
  '<h2>Bakiye</h2>',

 '<button onclick="location.href=''http://google.com'';"',
  'style="',
   '         background-color: #75B9BE; ',
    '        border: none; ',
     '       color: white; ',
      '      padding: 15px 15px; ',
       '     text-align: center; ',
        '    text-decoration: none; ',
         '   display: inline-block; ',
          '  font-size: 16px; ',
           ' margin: 4px 2px; ',
            'cursor: pointer; ',
            'border-radius: 8px; ',
            'box-shadow: 0 8px 8px rgba(0, 0, 0, 0.2); ',
            ' ">Google',
'</button>',

 '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>',
    '<div id="piechart" style="width: 900px; height: 500px;"></div>',

    '<script>',
      'google.charts.load(''current'', {''packages'':[''corechart'']});',
      'google.charts.setOnLoadCallback(drawChart);',

      'function drawChart() {',
        'var data = google.visualization.arrayToDataTable([',
        '[''Banka'', ''Tutar''],'.

  LOOP AT gt_struc INTO gs_struc.
    gs_chart-banka = gs_struc-banka.
    gs_chart-toplam = gs_struc-hslxx.
    COLLECT gs_chart INTO gt_chart.
  ENDLOOP.

  LOOP AT gt_chart INTO gs_chart.
    APPEND | [' { gs_chart-banka } ' , { gs_chart-toplam } ] , | TO gt_html.

  ENDLOOP.

  add_html:
        ']);',
        'var options = {',
          'title: ''Bakiye(UPB)''',
        '};',
        'var chart = new google.visualization.PieChart(document.getElementById(''piechart''));',
        'chart.draw(data, options);',
      '}',
  '</script>',
  '</body>',
  '</html> '.


ENDFORM.
