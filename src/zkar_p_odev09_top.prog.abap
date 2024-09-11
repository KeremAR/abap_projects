*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV09_TOP
*&---------------------------------------------------------------------*

TABLES: zkar_odev9_s2.
TABLES: zkar_odev9_s.

DATA: name       TYPE vrm_id,
      lt_listbox TYPE vrm_values,
      ls_listbox LIKE LINE OF lt_listbox.


DATA: go_alv      TYPE REF TO cl_gui_alv_grid,
      go_cont     TYPE REF TO cl_gui_custom_container,
      gt_fieldcat TYPE lvc_t_fcat,
      gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo.


DATA: go_main TYPE REF TO lcl_main.
create object go_main.

DATA:
  gt_raw_data TYPE truxs_t_text_data,
  gt_excel    TYPE zkar_odev9_tt02,
  gs_excel    LIKE LINE OF gt_excel.


DATA:
  gt_list TYPE zkar_odev9_tt,
  gs_list LIKE LINE OF gt_list.

DATA: gv_filename TYPE string.

DATA: docheader      LIKE bapiache09 OCCURS 0 WITH HEADER LINE,
      gt_accountgl   LIKE TABLE OF bapiacgl09,
      gs_accountgl   LIKE bapiacgl09,
      gt_accountrec  LIKE TABLE OF bapiacar09,
      gs_accountrec  LIKE bapiacar09,
      gt_accountpay  LIKE TABLE OF bapiacap09,
      gs_accountpay  LIKE bapiacap09,
      gt_accounttax  LIKE TABLE OF bapiactx09,
      gs_accounttax  LIKE bapiactx09,
      gt_currencyamo LIKE TABLE OF bapiaccr09,
      gs_currencyamo LIKE bapiaccr09,
      gt_return      LIKE TABLE OF bapiret2,
      gs_return      LIKE bapiret2,
      gt_extension2  LIKE TABLE OF bapiparex,
      gs_extension2  LIKE bapiparex,
      gt_criteria    LIKE TABLE OF bapiackec9,
      gs_criteria    LIKE bapiackec9.

data:
      lr_excel_structure      TYPE REF TO data,
      lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
      lo_table_row_descriptor TYPE REF TO cl_abap_structdescr,
      lv_content              TYPE xstring,
      lt_binary_tab           TYPE TABLE OF sdokcntasc,
      lv_length               TYPE i,
      lv_filename1            TYPE string,
      lv_path                 TYPE string,
      lv_fullpath             TYPE string.
