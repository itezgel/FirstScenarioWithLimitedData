*&---------------------------------------------------------------------*
*& Report ZCANCEL_BILLING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcancel_billing.

TABLES : vbak,vbfa.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
PARAMETERS : ordtype TYPE auart OBLIGATORY DEFAULT 'ZCM'.
*PARAMETERS : ordno TYPE vbeln_va .
*PARAMETERS : vdatu TYPE edatu_vbak.
*PARAMETERS : fromdate TYPE dats,
*             todate   TYPE dats.
SELECT-OPTIONS : s_ordno for vbak-vbeln.
SELECT-OPTIONS : s_vdatu for vbak-vdatu.
SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_vbfa TYPE TABLE OF vbfa,
       ls_vbfa TYPE vbfa,
       lt_vbak TYPE TABLE OF vbak,
       ls_vbak TYPE vbak.
DATA : vbak_count    TYPE i,
       success_count TYPE i VALUE IS INITIAL,
       failure_count TYPE i VALUE IS INITIAL,
       lv_fkdat      TYPE fkdat,
       ls_datagenlog TYPE zdatagen_logging,
       lv_seqno      TYPE i.
DATA : return     TYPE TABLE OF  bapireturn1,
       ls_return  TYPE bapireturn1,
       success    TYPE TABLE OF bapivbrksuccess,
       ls_success TYPE  bapivbrksuccess.

IF s_ordno IS NOT INITIAL AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vbeln in s_ordno.
ELSEIF s_vdatu IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vdatu in s_vdatu.
ELSEIF s_erdat is not initial AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat in s_erdat.
ENDIF.
*ELSEIF vdatu IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vdatu = vdatu.
*ELSEIF fromdate IS NOT INITIAL AND todate IS NOT INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat BETWEEN fromdate AND todate.
*ELSEIF fromdate IS NOT INITIAL AND todate IS INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat EQ fromdate ."and todate.
*ENDIF.

IF lt_vbak[] IS INITIAL.
  MESSAGE 'No records found' TYPE 'I'.
  RETURN.
ELSE.
  DESCRIBE TABLE lt_vbak LINES vbak_count.
  WRITE : vbak_count , 'records found.'.
  NEW-LINE.
  LOOP AT lt_vbak INTO ls_vbak.
    lv_seqno = lv_seqno + 1.
    SELECT SINGLE * FROM vbfa INTO ls_vbfa WHERE vbelv = ls_vbak-vbeln AND vbtyp_n = 'M'.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE fkdat INTO lv_fkdat FROM vbrk WHERE vbeln = ls_vbfa-vbeln.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
        EXPORTING
          billingdocument = ls_vbfa-vbeln
          testrun         = ''
          no_commit       = ''
          billingdate     = lv_fkdat
        TABLES
          return          = return
          success         = success.

      READ TABLE return INTO ls_return WITH KEY type = 'S'  id = 'VF' number = '216'.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
        WRITE :'Success',ls_return-message.
        NEW-LINE.
        ls_datagenlog-transaction_type = 'DELET'.
        ls_datagenlog-hseq_no = lv_seqno.
        ls_datagenlog-iseq_no = lv_seqno.
        ls_datagenlog-documentno = ls_vbfa-vbeln.
*            ls_datagenlog-EXTERNAL_DOCUMENT
        ls_datagenlog-msg = ls_return-message.
        ls_datagenlog-created_on = sy-datum.
        ls_datagenlog-created_by = sy-uname.
        ls_datagenlog-result_flag = 'S'.
        success_count = success_count + 1.
        MODIFY zdatagen_logging FROM ls_datagenlog.
      ELSE.
        LOOP AT return INTO ls_return WHERE type = 'E'.
          WRITE : 'Failed:', ls_return-message.
        ENDLOOP.
        failure_count = failure_count + 1.
      ENDIF.
    ENDIF.
    CLEAR : ls_vbak,ls_vbfa,lv_fkdat,return, success.
  ENDLOOP.
ENDIF.
