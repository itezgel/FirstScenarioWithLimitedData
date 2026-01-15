FUNCTION ZCREATEGR_DATAGEN1_PA.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(POSTING_DATE) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR255
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(OV_MAT_DOC) TYPE  BAPI2017_GM_HEAD_RET-MAT_DOC
*"     VALUE(OV_MAT_YEAR) TYPE  BAPI2017_GM_HEAD_RET-DOC_YEAR
*"  TABLES
*"      PO_GR TYPE  ZGR_HEADER_TT
*"--------------------------------------------------------------------
  DATA: ls_header TYPE bapi2017_gm_head_01,
        ls_code   TYPE bapi2017_gm_code,
        mat_doc   TYPE bapi2017_gm_head_ret-mat_doc,
        mat_year  TYPE bapi2017_gm_head_ret-doc_year,
        lt_item   TYPE TABLE OF bapi2017_gm_item_create,
        ls_item   TYPE bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        wa_return TYPE bapiret2,
        PO_HEADER TYPE  BAPIEKKOL,
        po_itm    TYPE TABLE OF bapiekpo,
        po_itm_wa TYPE  bapiekpo,
        po_return TYPE TABLE OF bapireturn WITH HEADER LINE,
        lv_msg    TYPE char50.

  DATA : wa_po_gr       TYPE zgr_header,
         ls_datagen_log TYPE zpa_logging,
         lt_datagen_log TYPE TABLE OF zpa_logging,
         wa_log         TYPE zsocreate_log,
         it_success_log TYPE TABLE OF zsocreate_log,
         it_failure_log TYPE TABLE OF zsocreate_log,
         lv_rec         TYPE char5,
         lv_succ        TYPE char5,
         lv_fail        TYPE char5,
         hseqno         TYPE int4.


  IF po_gr[] IS NOT INITIAL .
    CLEAR: ls_header,ls_code.
    SELECT MAX( hseq_no ) FROM zpa_logging INTO hseqno .
    LOOP AT po_gr INTO wa_po_gr.
      REFRESH: lt_item[],lt_return[].
      hseqno = hseqno + 1.
      lv_rec = sy-index.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_po_gr-po_no
        IMPORTING
          output = wa_po_gr-po_no.
      ls_code-gm_code = '01'.
      ls_datagen_log-created_on = sy-datum. "ls_header-pstng_date = sy-datum.
*      ls_header-doc_date = sy-datum.
      ls_datagen_log-transaction_type = 'GR_PO'.
      ls_datagen_log-hseq_no = hseqno. " + 1.

*--Populate Item Details:
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder = wa_po_gr-po_no
          items         = 'X'
        IMPORTING
          PO_HEADER	    = PO_HEADER
        TABLES
          po_items      = po_itm
          return        = po_return.

***      Check if PO is released, else release.
IF PO_HEADER-REL_IND NE 'G'.
        data:porel_msg type char255,
           porel_flag type char1.

CALL FUNCTION 'ZPO_RELEASE'
  EXPORTING
    PONUM          =  wa_po_gr-po_no
    REL_CODE       = 'MA'
 IMPORTING
   RET_MSG        = porel_msg
   FLAG           = porel_flag.

ENDIF.

*******End of PO release
wait UP TO 1 SECONDS.
IF POSTING_DATE is not INITIAL.
  ls_header-pstng_date = POSTING_DATE.
else.
  ls_header-pstng_date = PO_HEADER-DOC_DATE.
ENDIF.
*      ls_header-pstng_date = PO_HEADER-DOC_DATE.
      ls_header-doc_date = PO_HEADER-DOC_DATE. "sy-datum.
*      READ TABLE po_itm INTO po_itm_wa WITH KEY po_item = wa_po_gr-po_item_no.
      READ TABLE po_return with key type = 'E'.
      IF sy-subrc is not INITIAL and po_itm[] is not INITIAL.
      LOOP AT po_itm into  po_itm_wa.
        ls_datagen_log-external_document = ls_item-po_number = wa_po_gr-po_no.
        ls_item-mvt_ind = 'B'.
        ls_item-move_type = '101'.
        ls_datagen_log-iseq_no = ls_item-po_item = sy-tabix. "wa_po_gr-po_item_no.
        ls_item-stge_loc = po_itm_wa-store_loc.
        ls_item-plant = po_itm_wa-plant.
        ls_datagen_log-material = ls_item-material = po_itm_wa-material.
        ls_datagen_log-qty = ls_item-entry_qnt = po_itm_wa-QUANTITY. "wa_po_gr-po_quantity.
        APPEND ls_item TO lt_item.
      ENDLOOP.
      ELSE.
        flag = 'E'.
        LOOP AT po_return.
          CONCATENATE msg po_return-type po_return-MESSAGE into msg SEPARATED BY space.
        ENDLOOP.
        return.
      ENDIF.
      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_header
          goodsmvt_code    = ls_code
        IMPORTING
          materialdocument = mat_doc
          matdocumentyear  = mat_year
        TABLES
          goodsmvt_item    = lt_item
          return           = lt_return.
      IF mat_doc IS INITIAL .
        lv_fail = lv_fail + 1.
        sort lt_return ASCENDING by NUMBER.
        delete ADJACENT DUPLICATES FROM lt_return COMPARING NUMBER.
        LOOP AT lt_return INTO wa_return.
          CALL FUNCTION 'FORMAT_MESSAGE'
            EXPORTING
              id        = wa_return-id
              lang      = '-D'
              no        = wa_return-number
              v1        = wa_return-message_v1
              v2        = wa_return-message_v2
              v3        = wa_return-message_v3
              v4        = wa_return-message_v4
            IMPORTING
              msg       = lv_msg
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          CONCATENATE 'FAILED' lv_msg INTO ls_datagen_log-msg SEPARATED BY space.
          CONCATENATE msg lv_msg into msg SEPARATED BY space.
          flag = 'E'.
          ls_datagen_log-result_flag = 'E'.
          ls_datagen_log-created_at = sy-uzeit.
          ls_datagen_log-date_posted = POSTING_DATE.
          APPEND ls_datagen_log TO lt_datagen_log.
        ENDLOOP.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
           WAIT          = 'X'.
*         IMPORTING
*           RETURN        =                  .

        flag = 'S'.
        ls_datagen_log-result_flag = 'S'.
        ls_datagen_log-documentno = mat_doc.
        ov_mat_doc = mat_doc.
        ov_mat_year = mat_year.
        CONCATENATE 'SUCCESS:' 'Goods Receipt' mat_doc 'Created ' INTO ls_datagen_log-msg SEPARATED BY space.
        ls_datagen_log-created_at = sy-uzeit.
        ls_datagen_log-date_posted = POSTING_DATE.
        APPEND ls_datagen_log TO lt_datagen_log.
        CONCATENATE msg ls_datagen_log-msg into msg SEPARATED BY space.
      ENDIF.
      clear wa_po_gr.
    ENDLOOP.
    IF lt_datagen_log[] IS NOT INITIAL .
      MODIFY zpa_logging FROM TABLE lt_datagen_log.
    ENDIF.
  ELSE.
    flag = 'E'.
    CONCATENATE 'No Records to process' '' INTO msg SEPARATED BY space.
  ENDIF.

ENDFUNCTION.
