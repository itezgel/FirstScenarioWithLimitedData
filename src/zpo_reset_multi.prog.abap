*&---------------------------------------------------------------------*
*& Report ZPO_RESET_MULTI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpo_reset_multi.

TABLES : ekko,ekbe.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_po FOR ekko-ebeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_logging TYPE TABLE OF ekko,
       ls_logging TYPE ekko,
       lv_po      TYPE ebeln.

DATA : invoice  TYPE  bapi_incinv_fld-inv_doc_no,
       inv_year TYPE  bapi_incinv_fld-fisc_year.
DATA : ls_ekbe    TYPE ekbe,
       lv_gr      TYPE mblnr,
       lv_invoice TYPE mblnr.

DATA : lt_log_gr TYPE TABLE OF zdatagen_logging,
       lt_log_in TYPE TABLE OF zdatagen_logging.
DATA : ls_log TYPE zdatagen_logging.
DATA : it_return TYPE TABLE OF bapiret2,
       ls_return TYPE bapiret2.

DATA : goodsmvt_headret TYPE  bapi2017_gm_head_ret.
DATA : process_flag TYPE char1 VALUE ''.
DATA : msg   TYPE char255.

SELECT * FROM ekko INTO TABLE lt_logging WHERE ebeln IN s_po.

IF  lt_logging[] IS NOT INITIAL.
  SORT lt_logging ASCENDING BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_logging COMPARING ebeln.
  LOOP AT lt_logging INTO ls_logging .
    CLEAR : lv_po, lv_gr, lv_invoice.
    CLEAR : process_flag.
    lv_po = ls_logging-ebeln.
***    reverse Invoice
    SELECT SINGLE * INTO ls_ekbe FROM ekbe WHERE vgabe = 2 AND ebeln = lv_po AND shkzg = 'H'. " H - reversal doc "VGABE = 2 ==> Invoice.
    IF sy-subrc IS INITIAL. " Already reveresed.
      WRITE : lv_po ,'invoice already reversed with', ls_ekbe-belnr , ls_ekbe-gjahr.
      NEW-LINE.
      process_flag = 'S'.
    ELSE.
      SELECT SINGLE * INTO ls_ekbe FROM ekbe WHERE vgabe = 2 AND ebeln = lv_po . "VGABE = 2 ==> Invoice
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
          EXPORTING
            invoicedocnumber          = ls_ekbe-belnr
            fiscalyear                = ls_ekbe-gjahr
            reasonreversal            = '01'
            postingdate               = ls_ekbe-budat
          IMPORTING
            invoicedocnumber_reversal = invoice
            fiscalyear_reversal       = inv_year
          TABLES
            return                    = it_return.
        IF invoice IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
*         IMPORTING
*           RETURN        =                  .
          WRITE : lv_po ,'invoice reversal successful agianst', invoice.
          NEW-LINE.
          process_flag = 'S'.
        ELSE.
          WRITE : 'Invoice reversal ERROR:',lv_po.
          LOOP AT it_return INTO ls_return WHERE type = 'E' OR type = 'A' .
            WRITE : ls_return-message.
          ENDLOOP.
          process_flag = 'E'.
        ENDIF.
      ENDIF.  " End of Invoice reversal
    ENDIF.  " end of reveral exists
*   ****     cancel GR
    CLEAR: ls_ekbe,it_return,invoice,inv_year.
    SELECT SINGLE * INTO ls_ekbe  FROM ekbe WHERE vgabe = 1 AND ebeln = lv_po AND shkzg = 'H'. " H - reversal doc " "VGABE = 1 ==> GR
    IF sy-subrc IS INITIAL . " already reveresed
      WRITE : lv_po ,'GR reversal already completed with ' , ls_ekbe-belnr , ls_ekbe-gjahr.
      process_flag = 'S'.
    ELSE.
      SELECT SINGLE * INTO ls_ekbe  FROM ekbe WHERE vgabe = 1 AND ebeln = lv_po . "VGABE = 1 ==> GR
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument    = ls_ekbe-belnr
            matdocumentyear     = ls_ekbe-gjahr
            goodsmvt_pstng_date = ls_ekbe-budat
*           GOODSMVT_PR_UNAME   =
*           DOCUMENTHEADER_TEXT =
          IMPORTING
            goodsmvt_headret    = goodsmvt_headret
          TABLES
            return              = it_return.
*      *         GOODSMVT_MATDOCITEM =        .
        IF goodsmvt_headret IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          WRITE : lv_po ,'GR reversal successful agianst', goodsmvt_headret-mat_doc ,goodsmvt_headret-mat_doc.
          NEW-LINE.
          process_flag = 'S'.
        ELSE.
          WRITE : 'GR reversal ERROR:',lv_po.
          LOOP AT it_return INTO ls_return WHERE type = 'E' OR type = 'A' .
            WRITE : ls_return-message.
          ENDLOOP.
          process_flag = 'E'.
        ENDIF.
        CLEAR: ls_ekbe,it_return,goodsmvt_headret.
      ENDIF.
    ENDIF. " end of reversal check of GR
    IF process_flag = 'S'. " proceed with deletion
      CALL FUNCTION 'ZPO_DELETE_ALLITEM'
        EXPORTING
          ponum = lv_po
        IMPORTING
*         FLAG  =
          msg   = msg.
      write : msg.
    ENDIF.
  ENDLOOP.
ENDIF.
