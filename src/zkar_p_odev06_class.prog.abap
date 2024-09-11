*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV06_CLASS
*&---------------------------------------------------------------------*

CLASS cl_fetch_tcmb DEFINITION.
  PUBLIC SECTION.
    DATA: lv_yil_ay     TYPE string,
          lv_gun_ay_yil TYPE string,
          http_client   TYPE REF TO if_http_client,
          ev_filelen    TYPE int4,
          ev_file       TYPE xstring,
          lv_string     TYPE string,
          lt_binarytab  TYPE TABLE OF solix.

     METHODS:
      fetch.

ENDCLASS.

CLASS cl_fetch_tcmb IMPLEMENTATION.
  METHOD fetch.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url    = lv_url
      IMPORTING
        client = http_client.
    IF sy-subrc <> 0 .
      MESSAGE 'vei cekme hatasi' TYPE 'E'.
    ENDIF.
    IF http_client IS NOT BOUND.
      MESSAGE 'HTTP istemcisi oluşturulamadı.' TYPE 'E'.
      RETURN.
    ENDIF.

    http_client->send( ).
    http_client->receive( ).
    DATA(lv_xml_response) = http_client->response->get_cdata( ).

    IF http_client IS BOUND.
      ev_file = http_client->response->get_data( ).
      ev_filelen = xstrlen( ev_file ).
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = ev_file
      TABLES
        binary_tab = lt_binarytab.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = ev_filelen
      IMPORTING
        text_buffer  = lv_string
      TABLES
        binary_tab   = lt_binarytab.

*    cl_abap_browser=>show_xml(
*   EXPORTING
*     xml_string   = lv_string ).


**-- Create the Main Factory
    DATA(lif_ixml) = cl_ixml=>create( ).

**-- Create the Initial Document
    DATA(lif_ixml_document) = lif_ixml->create_document( ).

**-- Create a Stream Factory
    DATA(lif_ixml_stream_factory) = lif_ixml->create_stream_factory( ).

**-- Create an Input Stream
    DATA(lif_ixml_istream)       = lif_ixml_stream_factory->create_istream_xstring( string = ev_file ).

**-- Create a Parser
    DATA(lif_ixml_parser) = lif_ixml->create_parser(
        document       = lif_ixml_document
        istream        = lif_ixml_istream
        stream_factory = lif_ixml_stream_factory
    ).

**-- check errors in parsing
    IF lif_ixml_parser->parse( ) <> 0.
      IF lif_ixml_parser->num_errors( ) <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    lif_ixml_istream->close( ).

***Nevigate to the 'DATA' node of xml
    DATA(lif_ixml_node) = lif_ixml_document->find_from_name( name = 'Tarih_Date' ).
***Get name of the node name will be DATA
    DATA(lv_name) = lif_ixml_node->get_name( ).
***In order to process childs of DATA node we need to create iterator using its childerns
    DATA(lif_ixml_node_list) = lif_ixml_node->get_children( ).
    DATA(lif_ixml_node_iterator) = lif_ixml_node_list->create_iterator( ).

    lif_ixml_node ?= lif_ixml_node_iterator->get_next( ).


    WHILE lif_ixml_node IS NOT INITIAL.
      lv_name = lif_ixml_node->get_name( ).

*      DATA(lif_ixml_node2) = lif_ixml_node->get_first_child( ).
      DATA(lif_ixml_node_list2) = lif_ixml_node->get_children( ).
      DATA(lif_ixml_node_iterator2) = lif_ixml_node_list2->create_iterator( ).

      CLEAR gs_list.

      DATA(attrs) = lif_ixml_node->get_attributes( ).
      DATA(attr) = attrs->get_named_item_ns( name = 'CurrencyCode' ).
      gs_list-dovizkodu = attr->get_value( ).


      DATA(lif_ixml_node2) =  lif_ixml_node_iterator2->get_next( ).
      WHILE lif_ixml_node2 IS NOT INITIAL.
        lv_name = lif_ixml_node2->get_name( ).

        CASE lv_name.
          WHEN 'Isim'.
            gs_list-isim = lif_ixml_node2->get_value( ).
          WHEN 'CurrencyName'.
            gs_list-doviz = lif_ixml_node2->get_value( ).
          WHEN 'ForexBuying'.
            gs_list-forexbuy = lif_ixml_node2->get_value( ).
          WHEN 'ForexSelling'.
            gs_list-forexsel = lif_ixml_node2->get_value( ).
          WHEN 'BanknoteBuying'.
            gs_list-bankbuy = lif_ixml_node2->get_value( ).
          WHEN 'BanknoteSelling'.
            gs_list-banksl = lif_ixml_node2->get_value( ).
        ENDCASE.

        lif_ixml_node2 ?= lif_ixml_node_iterator2->get_next( ).
      ENDWHILE.

      APPEND gs_list TO gt_list.

      lif_ixml_node ?= lif_ixml_node_iterator->get_next( ).
    ENDWHILE.

    CLEAR gs_list.
    LOOP AT gt_list INTO gs_list.
      gs_list-try = 'TRY'.
      gs_list-tarih = p_tarih.
      modify gt_list from gs_list.
      MODIFY zkar_t_odev06 FROM gs_list.

    ENDLOOP.

    CREATE OBJECT go_alv
      EXPORTING
        i_parent = cl_gui_container=>screen0.

    go_alv->set_table_for_first_display(
      EXPORTING
        i_structure_name              =      'zkar_t_odev06'
      CHANGING
        it_outtab                     =      gt_list  ).
  ENDMETHOD.



ENDCLASS.

CLASS cl_mail DEFINITION.
  PUBLIC SECTION.
    METHODS: send_email IMPORTING iv_subject   TYPE string
                                  iv_body      TYPE string
                                  iv_recipient TYPE string.

ENDCLASS.

CLASS cl_mail IMPLEMENTATION.

  METHOD send_email.

    DATA: msg TYPE REF TO cl_bcs_message.
    CREATE OBJECT msg.

    msg->set_subject( iv_subject ).
    msg->set_main_doc( iv_contents_txt = iv_body ).
    msg->add_recipient( iv_recipient ).

    DATA: table  TYPE REF TO cl_salv_table.


    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = table
          CHANGING
            t_table      = gt_list.
      CATCH cx_salv_msg .
    ENDTRY.

    DATA: lv_subject TYPE bcs_filename,
          v_xstring  TYPE xstring.

    lv_subject = |{ p_tarih } Günü Kur Bilgisi.xlsx|.
    v_xstring = table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

    msg->add_attachment(
      EXPORTING
        iv_doctype      = 'EXT'
        iv_filename     = lv_subject
        iv_contents_bin = v_xstring ).

    msg->set_sender(
      EXPORTING
        iv_address = 'no-reply@ides.com' ).

    msg->send( ).
    IF sy-subrc IS INITIAL.
      MESSAGE 'Message sent' TYPE 'S'.
    ELSE.
      MESSAGE 'Mesaj gönderilemedi.' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
