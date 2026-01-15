*&---------------------------------------------------------------------*
*& Report ZCANCEL_MATDOCUMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcancel_matdocument.


TABLES : aufm.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_mblnr FOR aufm-mblnr." OBLIGATORY.
PARAMETERS : mjahr TYPE mjahr.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_aufm          TYPE TABLE OF aufm,
       ls_aufm          TYPE aufm,
       lt_return        TYPE TABLE OF bapiret2,
       ls_return        TYPE bapiret2,
       goodsmvt_headret TYPE bapi2017_gm_head_ret.

SELECT * FROM aufm INTO TABLE lt_aufm WHERE mblnr IN s_mblnr AND mjahr = mjahr.

IF lt_aufm[] IS NOT INITIAL.
  LOOP AT lt_aufm INTO ls_aufm.


  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument = ls_aufm-mblnr
      matdocumentyear  = ls_aufm-mjahr
*     GOODSMVT_PSTNG_DATE       =
*     GOODSMVT_PR_UNAME         =
*     DOCUMENTHEADER_TEXT       =
    IMPORTING
      goodsmvt_headret = goodsmvt_headret
    TABLES
      return           = lt_return
*     GOODSMVT_MATDOCITEM       =
    .
  IF goodsmvt_headret IS NOT INITIAL.
    WRITE  :'Success:' ,ls_aufm-mblnr ,'reversed with' ,goodsmvt_headret-mat_doc.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = 'X'.
*     IMPORTING
*       RETURN        =.

  ELSE.
    WRITE : 'Error:', ls_aufm-mblnr. NEW-LINE.
    LOOP AT lt_return INTO ls_return WHERE type = 'E'.
      WRITE : ls_return-message. NEW-LINE.
    ENDLOOP.
  ENDIF.
ENDLOOP.

ENDIF.
