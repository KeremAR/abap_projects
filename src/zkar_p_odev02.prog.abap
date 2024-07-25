*&---------------------------------------------------------------------*
*& Report ZKAR_P_ODEV02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zkar_p_odev02.

TABLES: zkar_kitap_t,
        zkar_ogrenci_t,
        zkar_islem_t.

SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_ogr   RADIOBUTTON GROUP rgr1 DEFAULT 'X' USER-COMMAND usr1,
            p_islem RADIOBUTTON GROUP rgr1,
            p_kitap RADIOBUTTON GROUP rgr1.
SELECTION-SCREEN END OF BLOCK blk01.

*ogrenci tablosu
SELECTION-SCREEN BEGIN OF BLOCK blk_ogr WITH FRAME TITLE TEXT-002.
PARAMETERS: p_ogrno  TYPE zkar_ogrenci_t-ogrno MODIF ID gr1,
            p_ograd  TYPE zkar_ogrenci_t-ograd MODIF ID gr1,
            p_ogrsyd TYPE zkar_ogrenci_t-ogrsoyad MODIF ID gr1,
            p_cins   TYPE zkar_ogrenci_t-cinsiyet MODIF ID gr1,
            p_dtar   TYPE zkar_ogrenci_t-dtarih MODIF ID gr1,
            p_sinif  TYPE zkar_ogrenci_t-sinif MODIF ID gr1,
            p_puan   TYPE zkar_ogrenci_t-puan MODIF ID gr1.
SELECTION-SCREEN END OF BLOCK blk_ogr.

*islem tablosu
SELECTION-SCREEN BEGIN OF BLOCK blk_islem WITH FRAME TITLE TEXT-003.
PARAMETERS: p_islemn TYPE zkar_islem_t-islemno MODIF ID gr2,
            p_ogrnoi TYPE zkar_islem_t-ogrno MATCHCODE OBJECT zkar_sh_ogrno MODIF ID gr2,
            p_kitapn TYPE zkar_islem_t-kitapno MATCHCODE OBJECT zkar_sh_kitap MODIF ID gr2,
            p_atarih TYPE zkar_islem_t-atarih MODIF ID gr2,
            p_vtarih TYPE zkar_islem_t-vtarih MODIF ID gr2.
SELECTION-SCREEN END OF BLOCK blk_islem.

*KITAP TABLOSU
SELECTION-SCREEN BEGIN OF BLOCK blk_kitap WITH FRAME TITLE TEXT-004.
PARAMETERS: "p_kitapn TYPE int4,
  p_ktpno  TYPE zkar_kitap_t-kitapno MODIF ID gr3,
  p_kitpad TYPE zkar_kitap_t-kitapad MODIF ID gr3,
  p_yazarn TYPE zkar_kitap_t-yazarno MODIF ID gr3,
  p_turno  TYPE zkar_kitap_t-turno MATCHCODE OBJECT zkar_sh_turno MODIF ID gr3,
  p_sayfa  TYPE zkar_kitap_t-sayfasayisi MODIF ID gr3.
SELECTION-SCREEN END OF BLOCK blk_kitap.



INITIALIZATION.



AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF screen-group1 = 'GR1' OR screen-group1 = 'GR2' OR screen-group1 = 'GR3'.
      IF p_ogr = 'X' AND screen-group1 <> 'GR1'.
        screen-active = 0.
      ELSEIF p_islem = 'X' AND screen-group1 <> 'GR2'.
        screen-active = 0.
      ELSEIF p_kitap = 'X' AND screen-group1 <> 'GR3'.
        screen-active = 0.

      ELSE.
        screen-active = 1.
      ENDIF.
    ENDIF.

    IF screen-name = 'P_OGRNO'.
      screen-input = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
    IF screen-name = 'P_KTPNO'.
      screen-input = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
    IF screen-name = 'P_ISLEMN'.
      screen-input = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  PERFORM get_ogr_num.
  PERFORM get_kitap_num.
  PERFORM get_islem_num.


AT SELECTION-SCREEN.



START-OF-SELECTION.

  IF p_ogr = 'X'.
    " Öğrenci tablosuna veri ekleme

    IF p_ogrno IS INITIAL OR p_ograd IS INITIAL OR p_ogrsyd IS INITIAL OR
       p_cins IS INITIAL OR p_dtar IS INITIAL OR p_sinif IS INITIAL OR p_puan IS INITIAL.
      WRITE: / 'Tüm öğrenci bilgilerini doldurmanız gerekmektedir.'.
      EXIT.
    ENDIF.

    CLEAR: zkar_ogrenci_t.

    zkar_ogrenci_t-ogrno = p_ogrno.
    zkar_ogrenci_t-ograd = p_ograd.
    zkar_ogrenci_t-ogrsoyad = p_ogrsyd.
    zkar_ogrenci_t-cinsiyet = p_cins.
    zkar_ogrenci_t-dtarih = p_dtar.
    zkar_ogrenci_t-sinif = p_sinif.
    zkar_ogrenci_t-puan = p_puan.
    INSERT zkar_ogrenci_t.
    IF sy-subrc = 0.
      WRITE: / 'Öğrenci verisi başarıyla kaydedildi.'.


*      DATA: lv_ogrno TYPE zkar_ogrenci_t-ogrno.
*
*      " Number Range'den yeni öğrenci numarası al
*      CALL FUNCTION 'NUMBER_GET_NEXT'
*        EXPORTING
*          nr_range_nr             = '01'
*          object                  = 'ZKAR_OGRNO'
*        IMPORTING
*          number                  = lv_ogrno
*        EXCEPTIONS
*          interval_not_found      = 1
*          number_range_not_intern = 2
*          object_not_found        = 3
*          quantity_is_0           = 4
*          quantity_is_not_1       = 5
*          interval_overflow       = 6
*          buffer_overflow         = 7
*          OTHERS                  = 8.
*
*      IF sy-subrc <> 0.
*        WRITE: / 'Öğrenci numarası alırken hata oluştu.'.
*        EXIT.
*      ELSE.
*        p_ogrno = lv_ogrno.
*      ENDIF.
    ELSE.
      WRITE: / 'Öğrenci verisi kaydedilirken hata oluştu.'.
    ENDIF.
  ENDIF.
  IF p_kitap = 'X'.
    " Kitap tablosuna veri ekleme
    IF p_ktpno IS INITIAL OR p_kitpad IS INITIAL OR p_yazarn IS INITIAL OR
     p_turno IS INITIAL OR p_sayfa IS INITIAL.
      WRITE: / 'Tüm kitap bilgilerini doldurmanız gerekmektedir.'.
      EXIT.
    ENDIF.
    CLEAR: zkar_kitap_t.



    zkar_kitap_t-kitapno = p_ktpno.
    zkar_kitap_t-kitapad = p_kitpad.
    zkar_kitap_t-yazarno = p_yazarn.
    zkar_kitap_t-turno = p_turno.
    zkar_kitap_t-sayfasayisi = p_sayfa.
    INSERT zkar_kitap_t.
    IF sy-subrc = 0.
      WRITE: / 'Kitap verisi başarıyla kaydedildi.'.
    ELSE.
      WRITE: / 'Kitap verisi kaydedilirken hata oluştu.'.
    ENDIF.
  ENDIF.

  IF p_islem = 'X'.
    " İşlem tablosuna veri ekleme

    IF p_islemn IS INITIAL OR
       p_ogrnoi IS INITIAL OR
       p_kitapn IS INITIAL OR
      p_atarih IS INITIAL OR
       p_vtarih IS INITIAL.
      WRITE: / 'Tüm işlem bilgilerini doldurmanız gerekmektedir.'.
      EXIT.
    ENDIF.

    CLEAR: zkar_islem_t.
    " Öğrenci puanını kontrol et
    SELECT SINGLE puan INTO @DATA(lv_puan)
      FROM zkar_ogrenci_t
      WHERE ogrno = @p_ogrnoi.

    IF lv_puan <= 5.
      WRITE: / 'Öğrencinin puanı 5ten düşük. İşlem gerçekleştirilemedi.'.
      EXIT.
    ENDIF.



    zkar_islem_t-islemno = p_islemn.
    zkar_islem_t-ogrno = p_ogrnoi.
    zkar_islem_t-kitapno = p_kitapn.
    zkar_islem_t-atarih = p_atarih.
    zkar_islem_t-vtarih = p_vtarih.
    INSERT zkar_islem_t.
    IF sy-subrc = 0.
      WRITE: / 'İşlem verisi başarıyla kaydedildi.'.
    ELSE.
      WRITE: / 'İşlem verisi kaydedilirken hata oluştu.'.
    ENDIF.
  ENDIF.


FORM get_ogr_num.
  SELECT SINGLE COUNT( * )
    FROM zkar_ogrenci_t
    INTO @DATA(id_num).
  IF sy-dbcnt IS NOT INITIAL.
    p_ogrno = id_num + 1.
  ELSE.
    p_ogrno = 1.
  ENDIF.
ENDFORM.

FORM get_kitap_num.
  SELECT SINGLE COUNT( * )
    FROM zkar_kitap_t
    INTO @DATA(id_num).
  IF sy-dbcnt IS NOT INITIAL.
    p_ktpno = id_num + 1 .
  ELSE.
    p_ktpno = 1.
  ENDIF.
ENDFORM.

FORM get_islem_num.
  SELECT SINGLE COUNT( * )
    FROM zkar_islem_t
    INTO @DATA(id_num).
  IF sy-dbcnt IS NOT INITIAL.
    p_islemn = id_num + 1.
  ELSE.
    p_islemn = 1.
  ENDIF.
ENDFORM.
