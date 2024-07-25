*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV04_FRM
*&---------------------------------------------------------------------*
FORM get_data.
  SELECT bkpf~bukrs
         bkpf~gjahr
         bkpf~monat
         bkpf~belnr
         bseg~lifnr
         bseg~wrbtr
         bseg~buzei
         bseg~sgtxt
         bseg~gsber
         bseg~shkzg
         bkpf~bldat
         bkpf~xblnr
         bseg~shkzg
         itab~iskonto
    FROM bkpf
    INNER JOIN bseg AS bseg ON bseg~bukrs = bkpf~bukrs
                           AND bseg~gjahr = bkpf~gjahr
                           AND bseg~belnr = bkpf~belnr
    INNER JOIN zkar_odev4_bakim AS itab
      ON itab~saticino EQ bseg~lifnr
    INTO CORRESPONDING FIELDS OF TABLE gt_list
    WHERE bkpf~bukrs EQ p_skod
     and   bkpf~monat IN so_donem
      AND bseg~lifnr IN so_satci
      AND bkpf~gjahr EQ p_myil.

  LOOP AT gt_list INTO gs_list.
    gs_list-nrbtr = gs_list-wrbtr * ( 1 - gs_list-iskonto / 100 ).
    gs_list-fatura = icon_print.
    MODIFY gt_list FROM gs_list.

  ENDLOOP.

ENDFORM.

FORM display_alv.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcatalog
    TABLES
      t_outtab                 = gt_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM set_fc_sub USING p_fieldname p_seltext_s p_seltext_m p_seltext_l p_key p_col_pos p_hotspot p_edit p_icon p_outputlen.
  CLEAR: gs_fieldcatalog.
  gs_fieldcatalog-fieldname = p_fieldname.
  gs_fieldcatalog-seltext_s = p_seltext_s.
  gs_fieldcatalog-seltext_m = p_seltext_m.
  gs_fieldcatalog-seltext_l = p_seltext_l.
  gs_fieldcatalog-key       = p_key.
  gs_fieldcatalog-col_pos   = p_col_pos.
  gs_fieldcatalog-hotspot   = p_hotspot.
  gs_fieldcatalog-edit      = p_edit.
  gs_fieldcatalog-icon = p_icon.
  gs_fieldcatalog-outputlen =  p_outputlen.
  APPEND gs_fieldcatalog TO gt_fieldcatalog.
ENDFORM.

FORM set_fc.
  PERFORM: set_fc_sub USING 'BUKRS' 'SIRKET KODU' 'SIRKET KODU' 'SIRKET KODU' 'X' '0' '' '' '' '',
           set_fc_sub USING 'GJAHR' 'MALI YIL' 'MALI YIL' 'MALI YIL' '' '1' '' '' '' '',
           set_fc_sub USING 'MONAT' 'DONEM' 'DONEM' 'DONEM' '' '2' '' '' '' '',
           set_fc_sub USING 'BELNR' 'BELGE NO' 'BELGE NO' 'BELGE NO' '' '3' 'X' '' '' '',
           set_fc_sub USING 'LIFNR' 'SATICI' 'SATICI' 'SATICI' '' '4' '' '' '' '',
           set_fc_sub USING 'WRBTR' 'TUTAR' 'TUTAR' 'TUTAR' '' '5' '' '' '' '',
           set_fc_sub USING 'ISKONTO' 'ISKONTO' 'ISKONTO' 'ISKONTO' '' '6' '' 'X' '' '',
           set_fc_sub USING 'NRBTR' 'yeni TUTAR' 'yeni TUTAR' 'yeni TUTAR' '' '7' '' '' '' '',
           set_fc_sub USING 'FATURA' 'fatura' 'fatura' 'fatura' '' '8' 'X' '' 'X' '30'.

ENDFORM.

FORM set_layout.
  gs_layout-window_titlebar = 'REUSE ALV'.
  gs_layout-zebra = abap_true.
  gs_layout-colwidth_optimize = abap_true.
  gs_layout-box_fieldname = 'selkz'.
ENDFORM.

FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTATUS'.
ENDFORM.

FORM user_command USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  CASE rs_selfield-fieldname.
    WHEN 'ISKONTO'.
      READ TABLE gt_list INDEX rs_selfield-tabindex INTO gs_list.

      gs_list-nrbtr = gs_list-wrbtr * ( 1 - gs_list-iskonto / 100 ).
      MODIFY gt_list FROM gs_list INDEX rs_selfield-tabindex.
      rs_selfield-refresh = abap_true.
      RETURN.
  ENDCASE.

  CASE r_ucomm.
    WHEN '&DETAIL'.
      PERFORM display_detail_popup USING rs_selfield-tabindex.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname.
          READ TABLE gt_list  INDEX rs_selfield-tabindex INTO gs_list.
        WHEN 'BELNR'.


          SET PARAMETER ID 'BLN' FIELD gs_list-belnr.
          SET PARAMETER ID 'BUK' FIELD gs_list-bukrs.
          SET PARAMETER ID 'GJR' FIELD gs_list-gjahr.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        WHEN 'FATURA'.


          PERFORM call_smartform USING gs_list.


      ENDCASE.
  ENDCASE.



ENDFORM.

FORM call_smartform USING p_row_data TYPE zkar_odev4_s01
                          .

  DATA: fm_name   TYPE rs38l_fnam,
        it_fatura TYPE zkar_odev4_tt01.

  " Get the function module name of the Smart Form
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZKAR_SF_01'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    " Handle errors here
    EXIT.
  ENDIF.

  SELECT
      bseg~wrbtr
      bseg~buzei
      bseg~sgtxt
      bseg~gsber
      bseg~shkzg
    FROM bseg
INTO CORRESPONDING FIELDS OF TABLE it_fatura
WHERE belnr EQ p_row_data-belnr
AND gjahr EQ p_row_data-gjahr
AND bukrs EQ p_row_data-bukrs.




  CALL FUNCTION fm_name
    EXPORTING
      p_row_data       = p_row_data
      it_fatura        = it_fatura
*     it_data          = it_fatura
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

ENDFORM.



* Display detail popup form
FORM display_detail_popup USING p_tabix TYPE sy-tabix.


  READ TABLE gt_list INTO gs_list WITH KEY selkz = 'X'.
  IF sy-subrc = 0.
*    DATA: lt_detail   TYPE TABLE OF gty_list,
    DATA: lt_detail   TYPE TABLE OF zkar_odev4_s01,
          ls_fieldcat TYPE slis_fieldcat_alv,
          lt_fieldcat TYPE slis_t_fieldcat_alv,
          ls_layout   TYPE slis_layout_alv.


    SELECT bkpf~bukrs
        bkpf~gjahr
        bkpf~belnr
        bseg~wrbtr
        bseg~buzei
        bseg~sgtxt
        bseg~gsber
        bseg~shkzg
   FROM bkpf
   INNER JOIN bseg AS bseg ON bseg~bukrs = bkpf~bukrs
                          AND bseg~gjahr = bkpf~gjahr
                          AND bseg~belnr = bkpf~belnr
      INTO CORRESPONDING FIELDS OF TABLE lt_detail
        WHERE bkpf~belnr EQ gs_list-belnr
        AND bkpf~gjahr EQ gs_list-gjahr
        AND bkpf~bukrs EQ gs_list-bukrs.


*    APPEND gs_list TO lt_detail.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'BUKRS'.
    ls_fieldcat-seltext_s = 'Sirket Kodu'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GJAHR'.
    ls_fieldcat-seltext_s = 'Mali Yil'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MONAT'.
    ls_fieldcat-seltext_s = 'Donem'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'BELNR'.
    ls_fieldcat-seltext_s = 'Belge No'.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'WRBTR'.
    ls_fieldcat-seltext_s = 'Tutar'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'LIFNR'.
    ls_fieldcat-seltext_s = 'Satici'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'BUZEI'.
    ls_fieldcat-seltext_s = 'Belge Kalemi'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SGTXT'.
    ls_fieldcat-seltext_s = 'Kalem Metni'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GSBER'.
    ls_fieldcat-seltext_s = 'Is Alani'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SHKZG'.
    ls_fieldcat-seltext_s = 'Borclu-Alacakli Gostergesi'.
    APPEND ls_fieldcat TO lt_fieldcat.


    CLEAR ls_layout.
    ls_layout-colwidth_optimize = abap_true.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        is_layout             = ls_layout
        it_fieldcat           = lt_fieldcat
        i_screen_start_column = 20
        i_screen_start_line   = 5
        i_screen_end_column   = 100
        i_screen_end_line     = 10
      TABLES
        t_outtab              = lt_detail
      EXCEPTIONS
        program_error         = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE 'lutfen gecerli bir satir seciniz.' TYPE 'I'.
  ENDIF.
ENDFORM.
