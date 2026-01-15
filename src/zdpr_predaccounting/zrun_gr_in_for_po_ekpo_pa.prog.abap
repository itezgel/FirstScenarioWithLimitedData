*&---------------------------------------------------------------------*
*& Report ZRUN_GR_IN_FOR_PO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrun_gr_in_for_po_ekpo_pa.


TABLES : ekko,ekbe.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_po FOR ekko-ebeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_logging TYPE TABLE OF ekko,
       ls_logging TYPE ekko,
       lv_po      TYPE ebeln.

DATA : gr_msg   TYPE char255,
       gr_flag  TYPE char1,
       mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
       mat_year TYPE bapi2017_gm_head_ret-doc_year,
       wa_gr    TYPE zgr_header,
       it_gr    TYPE TABLE OF zgr_header,
       inv_flag TYPE  char1,
       inv_msg  TYPE  char100,
       invoice  TYPE  bapi_incinv_fld-inv_doc_no,
       inv_year TYPE  bapi_incinv_fld-fisc_year.
DATA : ls_ekbe    TYPE ekbe,
       lv_gr      TYPE mblnr,
       lv_invoice TYPE mblnr.
DATA : lt_log_gr TYPE TABLE OF zpa_logging,
       lt_log_in TYPE TABLE OF zpa_logging.
DATA : ls_log TYPE zpa_logging.

SELECT * FROM ekko INTO TABLE lt_logging WHERE ebeln IN s_po.

IF  lt_logging[] IS NOT INITIAL.
  SORT lt_logging ASCENDING BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_logging COMPARING ebeln.
  LOOP AT lt_logging INTO ls_logging .
    CLEAR : lv_po, lv_gr, lv_invoice.
    lv_po = ls_logging-ebeln.
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE belnr INTO lv_gr FROM ekbe WHERE vgabe = 1 AND ebeln = lv_po .
    SELECT belnr FROM ekbe UP TO 1 ROWS INTO TABLE @DATA(lt_x002) WHERE vgabe = 1 AND ebeln = lv_po ORDER BY belnr.
    IF sy-subrc eq 0.
      READ TABLE lt_x002 INTO DATA(ls_x002) INDEX 1.
      lv_gr = ls_x002-belnr.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026) "VGABE = 1 ==> GR
    IF sy-subrc IS NOT INITIAL. " GR does not exist
      wa_gr-po_no =  lv_po.
      APPEND wa_gr TO it_gr.

      CALL FUNCTION 'ZCREATEGR_DATAGEN1_PA'
        EXPORTING
          posting_date = ls_logging-bedat
        IMPORTING
          msg          = gr_msg
          flag         = gr_flag
          ov_mat_doc   = mat_doc
          ov_mat_year  = mat_year
        TABLES
          po_gr        = it_gr.
      IF gr_flag EQ 'S'.
        WRITE:/ 'PO' ,lv_po , gr_msg.
***                    create Invoice*************
        CALL FUNCTION 'ZCREATEINVOICE_FROM_PO_PA'
          EXPORTING
            po_number    = lv_po "ls_logging-external_document
            posting_date = ls_logging-bedat
          IMPORTING
            flag         = inv_flag
            msg          = inv_msg
            invoice      = invoice
            inv_year     = inv_year.
        IF inv_flag = 'S'.
          WRITE:/ 'PO' ,lv_po , inv_msg.
        ELSEIF inv_flag = 'E'.
          WRITE:/ 'PO' ,lv_po , inv_msg.
          NEW-LINE.
        ENDIF.   "  ,End of Invoice.

      ELSE.
        WRITE:/ 'PO' ,lv_po , gr_msg.
        NEW-LINE.
      ENDIF.
      CLEAR: it_gr, ls_logging.
      NEW-LINE.
    ELSE. "GR completed **    check if INV completed
      WRITE:/ 'PO' ,lv_po, 'has the GR' , lv_gr ,'Completed'.
      SELECT * FROM zpa_logging INTO TABLE lt_log_gr WHERE external_document = ls_logging-ebeln
        AND result_flag = 'E' AND transaction_type = 'GR_PO'.
      IF sy-subrc IS INITIAL. "ls_logging-result_flag = 'E'.
*        update first record for 1 record as S.  " this logic is onhold as the Success entry is being added in the creation FM
        SORT lt_log_gr BY hseq_no ASCENDING.
*        read table lt_log_gr into ls_log INDEX 1.
*        ls_log-result_flag = 'S'.
*        modify zpa_logging from ls_log.
*        delete lt_log_gr index 1.
        DELETE zpa_logging FROM TABLE lt_log_gr.
      ENDIF.
    ENDIF.  """ End of GR Check.

*****    Check if Invoice Created.
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE belnr INTO lv_invoice FROM ekbe WHERE vgabe = 2 AND ebeln = lv_po .
    SELECT belnr FROM ekbe UP TO 1 ROWS INTO TABLE @DATA(lt_x001) WHERE vgabe = 2 AND ebeln = lv_po ORDER BY belnr.
    IF sy-subrc eq 0.
      READ TABLE lt_x001 INTO DATA(ls_x001) INDEX 1.
      lv_invoice = ls_x001-belnr.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026) "VGABE = 2 ==> Invoice
    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'ZCREATEINVOICE_FROM_PO_PA'
        EXPORTING
          po_number    = lv_po "ls_logging-external_document
          posting_date = ls_logging-bedat
        IMPORTING
          flag         = inv_flag
          msg          = inv_msg
          invoice      = invoice
          inv_year     = inv_year.

      IF inv_flag = 'S'.
        WRITE:/ 'PO' ,lv_po , inv_msg.
      ELSEIF inv_flag = 'E'.
        WRITE:/ 'PO' ,lv_po , inv_msg.
        NEW-LINE.
      ENDIF.   "  ,End of Invoice.
    ELSE.
      WRITE:/ 'PO' ,lv_po, 'has the invoice' , lv_invoice ,'Completed'.
*      DATA : ls_log TYPE zpa_logging.
      CLEAR : ls_log.
      SELECT * FROM zpa_logging INTO TABLE lt_log_in WHERE external_document = ls_logging-ebeln
        AND result_flag = 'E' AND transaction_type = 'IN_PO'.
      IF sy-subrc IS INITIAL. "ls_logging-result_flag = 'E'.
*        update first record for 1 record as S.  " this logic is onhold as the Success entry is being added in the creation FM
*        sort lt_log_in by HSEQ_NO ASCENDING.
*        read table lt_log_in into ls_log INDEX 1.
*        ls_log-result_flag = 'S'.
*        modify zpa_logging from ls_log.
*        delete lt_log_in index 1.
        DELETE zpa_logging FROM TABLE lt_log_in.
      ENDIF.
    ENDIF.
***     end of Invoice check
  ENDLOOP.
ENDIF.
