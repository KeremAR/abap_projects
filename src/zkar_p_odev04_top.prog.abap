*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV04_TOP
*&---------------------------------------------------------------------*

*TYPES: BEGIN OF gty_list,
*         bukrs   TYPE bukrs,
*         belnr   TYPE belnr_d,
*         gjahr   TYPE gjahr,
*         monat   TYPE monat,
*         lifnr   TYPE lifnr,
*         wrbtr   TYPE wrbtr,
*         selkz   TYPE char1,
*         buzei   TYPE buzei,
*         sgtxt   TYPE sgtxt,
*         gsber   TYPE gsber,
*         shkzg   TYPE shkzg,
*         iskonto TYPE zkar_iskonto_de,
*         nrbtr   TYPE wrbtr,
*       END OF gty_list
*TYPES: BEGIN OF gty_list,
*         zdata TYPE zkar_odev4_s01,
*       END OF gty_list.

DATA: gs_bkpf type bkpf,
      gs_bseg type bseg.

DATA: gt_list TYPE TABLE OF zkar_odev4_s01,
      gs_list TYPE zkar_odev4_s01.

DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv,
      gs_fieldcatalog TYPE slis_fieldcat_alv.

DATA: gs_layout TYPE slis_layout_alv.

DATA: gv_action TYPE c LENGTH 1.
