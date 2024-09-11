*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV06_TOP
*&---------------------------------------------------------------------*
DATA: go_alv   TYPE REF TO cl_gui_alv_grid.
TABLES: usr21, adrp, adr6, zkar_mail_odev06.

DATA:
  lv_xml_data   TYPE string,
  lt_currencies TYPE TABLE OF zkar_t_odev06.

DATA : gt_list TYPE TABLE OF zkar_t_odev06,
       gs_list TYPE zkar_t_odev06.

*DATA: lt_emails    TYPE TABLE OF zkar_mail_odev06,
*      ls_email     TYPE zkar_mail_odev06,
     data: lv_recipient TYPE string.

DATA:
  lv_yil_ay      TYPE string,
  lv_gun         TYPE string,
  lv_date        TYPE datum,
  lv_temp_date   TYPE datum,
  lv_day_of_week TYPE p LENGTH 1 DECIMALS 0,
  lv_is_weekend  TYPE abap_bool,
  base_url       TYPE string VALUE 'https://www.tcmb.gov.tr/kurlar/',
  lv_url         TYPE string.

TYPES: BEGIN OF zemail_structure,
         name_text TYPE adrp-name_text,
         smtp_addr TYPE adr6-smtp_addr,
       END OF zemail_structure.

DATA: it_email     TYPE TABLE OF zemail_structure,
      rv_email     TYPE zemail_structure,
      mc_date_from TYPE adrp-date_from.

  DATA: go_docu TYPE REF TO cl_dd_document.
