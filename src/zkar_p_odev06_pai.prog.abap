*&---------------------------------------------------------------------*
*& Include          ZKAR_P_ODEV06_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' .
      SET SCREEN 0.
    WHEN '&MAIL'.
      PERFORM send_emails.
  ENDCASE.

ENDMODULE.

FORM send_emails.
  SELECT zusername
    FROM zkar_mail_odev06
    INTO TABLE @DATA(selected_users)
    WHERE checkbox = 'X'.

  IF selected_users IS INITIAL.
    MESSAGE 'No users selected' TYPE 'E'.
    RETURN.
  ENDIF.

  LOOP AT selected_users INTO DATA(selected_user).
    CLEAR rv_email.

    SELECT SINGLE adrp~name_text, adr6~smtp_addr
      FROM usr21
      LEFT JOIN adr6
        ON usr21~addrnumber = adr6~addrnumber AND
           usr21~persnumber = adr6~persnumber
      LEFT JOIN adrp
        ON usr21~persnumber = adrp~persnumber AND
           adrp~date_from   = @mc_date_from   AND
           adrp~nation      = ''
      WHERE usr21~bname = @selected_user-zusername
        AND adr6~smtp_addr IS NOT INITIAL
      INTO @rv_email.

    IF sy-subrc = 0.
      APPEND rv_email TO it_email.
      lv_recipient = rv_email-smtp_addr.

      DATA: lo_mail TYPE REF TO cl_mail.
      CREATE OBJECT lo_mail.

      lo_mail->send_email(
        EXPORTING
          iv_subject   = 'TCMB Kur Bilgileri'
          iv_body      = 'TCMB günlük kur bilgileri ektedir.'
          iv_recipient = lv_recipient ).
    ENDIF.
  ENDLOOP.
ENDFORM.
