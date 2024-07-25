*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV05_TOP
*&---------------------------------------------------------------------*

DATA:
  gs_t012  TYPE t012,
  gs_t012k TYPE t012k.

DATA:
  gt_struc TYPE TABLE OF zkar_odev5_s01,
  gs_struc TYPE zkar_odev5_s01.

DATA: go_alv   TYPE REF TO cl_gui_alv_grid,
      go_cont  TYPE REF TO cl_gui_custom_container,
      go_cont2 TYPE REF TO cl_gui_custom_container.

DATA: go_splitter TYPE REF TO cl_gui_splitter_container,
      go_gui1     TYPE REF TO cl_gui_container,
      go_gui2     TYPE REF TO cl_gui_container.

DATA:gt_fcat   TYPE lvc_t_fcat,
     gs_fcat   TYPE lvc_s_fcat,
     gs_layout TYPE lvc_s_layo.

DATA:go_html     TYPE REF TO cl_gui_html_viewer,
     doc_url(80), gt_html TYPE TABLE OF w3_html.
