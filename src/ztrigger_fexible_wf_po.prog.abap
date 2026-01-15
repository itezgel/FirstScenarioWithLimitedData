*&---------------------------------------------------------------------*
*& Report ZRUN_GR_IN_FOR_PO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztrigger_fexible_wf_po.


TABLES : ekko,ekbe.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_po FOR ekko-ebeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_logging TYPE TABLE OF ekko,
       ls_logging TYPE ekko,
       lv_po      TYPE ebeln.

DATA : po_msg     TYPE  char255,
       gr_msg     TYPE char255,
       gr_flag    TYPE char1,
       mat_doc    TYPE bapi2017_gm_head_ret-mat_doc,
       mat_year   TYPE bapi2017_gm_head_ret-doc_year,
       it_pohdr   TYPE  zzpo_header_tt,
       wa_pohdr   LIKE LINE OF it_pohdr,
       it_poitems TYPE  zzpo_items_tt,
       wa_poitems LIKE LINE OF it_poitems,
       ot_log     TYPE TABLE OF  zpocreate_log,
       wa_log     TYPE zpocreate_log,
       wa_gr      TYPE zgr_header,
       it_gr      TYPE TABLE OF zgr_header,
       inv_flag   TYPE  char1,
       inv_msg    TYPE  char100,
       invoice    TYPE  bapi_incinv_fld-inv_doc_no,
       inv_year   TYPE  bapi_incinv_fld-fisc_year.
DATA : ls_ekbe    TYPE ekbe,
       lv_gr      TYPE mblnr,
       lv_invoice TYPE mblnr.

delete ADJACENT DUPLICATES FROM s_po.
SELECT * FROM ekko INTO TABLE lt_logging WHERE ebeln IN s_po.

IF  lt_logging[] IS NOT INITIAL.
*  DATA: lv_key    TYPE swr_struct-object_key.
*  DATA: lv_retcode    TYPE  sy-subrc,
*        lv_eventid    TYPE  swr_struct-event_id,
*        lt_in_cont    TYPE TABLE OF  swr_cont,
*        lt_msg_lines  TYPE TABLE OF  swr_messag,
*        lt_msg_struct TYPE TABLE OF   swr_mstruc.
*
*  LOOP AT lt_logging INTO ls_logging .
*    CLEAR lv_key.
**lv_key = MY_MBLNR.
*    CONCATENATE lv_key ls_logging-ebeln INTO lv_key.
*    IF  lv_key IS NOT INITIAL.
*      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
*        EXPORTING
*          object_type     = 'CL_MM_PUR_WF_OBJECT_PO'
*          object_key      = lv_key
*          event           = 'SUBMITTED_FOR_APPROVAL'
*          commit_work     = 'X'
**         event_language  = sy-langu
**         language        = sy-langu
**         user            = sy-uname
**         IFS_XML_CONTAINER       =
*        IMPORTING
*          return_code     = lv_retcode
*          event_id        = lv_eventid
*        TABLES
*          input_container = lt_in_cont
*          message_lines   = lt_msg_lines
*          message_struct  = lt_msg_struct.
*    ENDIF.
*  ENDLOOP.
*  WAIT UP TO 10 SECONDS.

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

      CALL FUNCTION 'ZCREATEGR_DATAGEN1'
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
        CALL FUNCTION 'ZCREATEINVOICE_FROM_PO'
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
*        IF ls_logging-result_flag = 'E'.
*          DELETE zdatagen_logging FROM ls_logging.
*        ENDIF.
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
      CALL FUNCTION 'ZCREATEINVOICE_FROM_PO'
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
*        IF ls_logging-result_flag = 'E'.
*          DELETE zdatagen_logging FROM ls_logging.
*        ENDIF.
    ENDIF.
***     end of Invoice check
  ENDLOOP.
ENDIF.
