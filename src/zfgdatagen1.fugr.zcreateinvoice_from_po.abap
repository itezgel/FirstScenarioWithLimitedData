FUNCTION zcreateinvoice_from_po.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PO_NUMBER) TYPE  EBELN
*"     VALUE(POSTING_DATE) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR100
*"     VALUE(INVOICE) TYPE  BAPI_INCINV_FLD-INV_DOC_NO
*"     VALUE(INV_YEAR) TYPE  BAPI_INCINV_FLD-FISC_YEAR
*"----------------------------------------------------------------------
  DATA: ls_header TYPE bapi2017_gm_head_01,
        ls_code   TYPE bapi2017_gm_code,
        mat_doc   TYPE bapi2017_gm_head_ret-mat_doc,
        mat_year  TYPE bapi2017_gm_head_ret-doc_year,
        lt_item   TYPE TABLE OF bapi2017_gm_item_create,
        ls_item   TYPE bapi2017_gm_item_create,
        lt_return TYPE TABLE OF bapiret2,
        wa_return TYPE bapiret2,
        po_header TYPE  bapiekkol,
        po_itm    TYPE TABLE OF bapiekpo,
        po_itm_wa TYPE  bapiekpo,
        po_return TYPE TABLE OF bapireturn WITH HEADER LINE,
        lv_msg    TYPE char50.


  DATA: i_header LIKE bapi_incinv_create_header OCCURS 0 WITH HEADER LINE.
  DATA: i_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
  DATA: i_item LIKE bapi_incinv_create_item OCCURS 0 WITH HEADER LINE.
  DATA: ws_belnr       LIKE bapi_incinv_fld-inv_doc_no,
        ws_gjahr       LIKE bapi_incinv_fld-fisc_year,
        ws_errflag     TYPE char1,
        ls_datagen_log TYPE zdatagen_logging,
        lt_datagen_log TYPE TABLE OF zdatagen_logging,
        hseqno         TYPE int4.

*--Populate Item Details:
  CALL FUNCTION 'BAPI_PO_GETDETAIL'
    EXPORTING
      purchaseorder = po_number
      items         = 'X'
    IMPORTING
      po_header     = po_header
    TABLES
      po_items      = po_itm
      return        = po_return.

  IF sy-subrc IS NOT INITIAL.
    flag = 'E'.
    CONCATENATE 'Failed:' 'No data found for PO' po_number INTO msg SEPARATED BY space.
    RETURN.
  ELSE.
    SELECT MAX( hseq_no ) FROM zdatagen_logging INTO hseqno . " for logging
    hseqno = hseqno + 1.
    ls_datagen_log-created_on = sy-datum.
    ls_datagen_log-transaction_type = 'IN_PO'.
    ls_datagen_log-hseq_no = hseqno.
    ls_datagen_log-external_document = po_number.

    i_header-invoice_ind = 'X'.
    i_header-ref_doc_no = po_number.
    i_header-doc_date  = sy-datum. "PO_HEADER-
    IF posting_date IS NOT INITIAL.
      i_header-pstng_date = posting_date .
    ELSE.
      i_header-pstng_date = sy-datum."PO_HEADER-sy
    ENDIF.

    i_header-comp_code = po_header-co_code.
*  i_header-GROSS_AMOUNT =  ls_ekko-dmbtr.
    i_header-currency = po_header-currency.
*    i_header-calc_tax_ind = 'X'.

    DATA: lv_wrbtr TYPE wrbtr.
    CLEAR lv_wrbtr.
    IF po_itm[] IS NOT INITIAL.
      LOOP AT po_itm INTO po_itm_wa.
        i_item-invoice_doc_item = sy-tabix.
        i_item-po_number = po_number.
        i_item-po_item = po_itm_wa-po_item.
*        IF I_header-comp_code = '1710'.
*        i_item-tax_code =  'I0'.
*        ELSEIF I_header-comp_code = 'AUC1'.
*          i_item-tax_code =  'P0'.
*        ELSEIF I_header-comp_code = 'USC1'.
*          i_item-tax_code =  'I0'.
*        ELSEIF I_header-comp_code = 'DEC1'.
*          i_item-tax_code =  'V0'.
*        ENDIF.
**        i_item-tax_code =  'P0'. "'I0'. "po_itm_wa-TAX_CODE.
        i_item-item_amount = po_itm_wa-net_value . "po_itm_wa-NET_PRICE.
        lv_wrbtr = lv_wrbtr + po_itm_wa-net_value.
        i_item-quantity = po_itm_wa-quantity.
        i_item-po_unit = po_itm_wa-unit.

        APPEND i_item.
      ENDLOOP.
    ENDIF.
    i_header-gross_amount =  lv_wrbtr. "ls_ekko-dmbtr.
  ENDIF.

  SORT i_item BY invoice_doc_item po_number po_item.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata       = i_header
    IMPORTING
      invoicedocnumber = ws_belnr
      fiscalyear       = ws_gjahr
    TABLES
      itemdata         = i_item
*     accountingdata   = i_accountingdata
*     TAXDATA          = I_TAX
      return           = i_return.

  ws_errflag = space.

  LOOP AT i_return .
    IF i_return-type = 'E'.
      ws_errflag = 'X'.
      flag = 'E'.
    ENDIF.
    CONCATENATE msg i_return-message INTO msg SEPARATED BY space.
    CONCATENATE 'FAILED' msg INTO ls_datagen_log-msg SEPARATED BY space.
    ls_datagen_log-result_flag = 'E'.
    ls_datagen_log-created_at = sy-uzeit.
    ls_datagen_log-date_posted = posting_date.
    APPEND ls_datagen_log TO lt_datagen_log.

  ENDLOOP.

  IF ws_errflag EQ space.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    flag = 'S'.
    invoice = ws_belnr.
    inv_year = ws_gjahr.
    CONCATENATE 'Success:' 'Invoice' ws_belnr ws_gjahr 'created successfully' INTO msg SEPARATED BY space.
    ls_datagen_log-result_flag = 'S'.
*    CONCATENATE ws_belnr  into ls_datagen_log-documentno. "ws_gjahr
    ls_datagen_log-documentno = ws_belnr .
    CONCATENATE 'SUCCESS:' msg INTO ls_datagen_log-msg SEPARATED BY space.
    ls_datagen_log-created_at = sy-uzeit.
    ls_datagen_log-date_posted = posting_date.
    APPEND ls_datagen_log TO lt_datagen_log.
*    FORMAT COLOR COL_HEADING INVERSE ON.
*    .         WRITE : / 'Document  Posted :' , ws_belnr , ws_gjahr.
*    FORMAT COLOR COL_HEADING INVERSE OFF.

  ENDIF.

  IF lt_datagen_log[] IS NOT INITIAL .
    MODIFY zdatagen_logging FROM TABLE lt_datagen_log.
  ENDIF.

ENDFUNCTION.
