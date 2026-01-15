FUNCTION ZCREATEPO_DATAGEN1_PA .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR255
*"  TABLES
*"      IT_POHDR TYPE  ZZPO_HEADER_TT
*"      IT_POITEMS TYPE  ZZPO_ITEMS_TT
*"      OT_LOG STRUCTURE  ZPOCREATE_LOG OPTIONAL
*"--------------------------------------------------------------------
*FUNCTION zcreatepo_datagen.

  SET BLANK LINES OFF.

  DATA:
    v_head   TYPE bapimepoheader,
    v_headx  TYPE bapimepoheaderx,
    i_item   TYPE bapimepoitem OCCURS 0 WITH HEADER LINE,
    i_itemx  TYPE bapimepoitemx OCCURS 0 WITH HEADER LINE,
    i_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
    i_sched  TYPE bapimeposchedule OCCURS 0 WITH HEADER LINE,
    i_schedx TYPE bapimeposchedulx OCCURS 0 WITH HEADER LINE,
    i_htext  TYPE bapimepotextheader OCCURS 0 WITH HEADER LINE,
    lv_count TYPE i VALUE 10.



  DATA : wa_poitem TYPE zzpo_items,
         wa_pohdr  TYPE zzpo_header,
         ponumber  TYPE bapimepoheader-po_number.

  DATA : wa_log         TYPE zpocreate_log,
         it_success_log TYPE TABLE OF zpocreate_log,
         it_failure_log TYPE TABLE OF zpocreate_log.

  DATA : ls_datagen_log TYPE zpa_logging,
         lt_datagen_log TYPE TABLE OF zpa_logging.
  DATA : lv_seq TYPE i VALUE 0.."POSNR_VA.
*  *********  logging details ****************
*  DATA : lv_log_seqno TYPE i.
  DATA: time_cnt TYPE n VALUE 1,
        log_time TYPE char10.
*  SELECT MAX( seq_no ) FROM zpa_logging INTO lv_log_seqno.

*****************************************
*********Header data
******************************************
  LOOP AT it_pohdr INTO wa_pohdr.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_pohdr-vendor
      IMPORTING
        output = wa_pohdr-vendor.
    v_head-vendor = wa_pohdr-vendor.
    v_headx-vendor = 'X'.


    IF wa_pohdr-doc_type IS NOT INITIAL.
      v_head-doc_type = wa_pohdr-doc_type. "'NB'.
    ELSE.
      v_head-doc_type = 'NB'.
    ENDIF.
    v_headx-doc_type = 'X'.


    IF wa_pohdr-pmtterms IS NOT INITIAL.
      v_head-pmnttrms = wa_pohdr-pmtterms. "'ZB01'.
      v_headx-pmnttrms = 'X'.
    ENDIF.

    v_head-purch_org = wa_pohdr-purch_org.
    v_headx-purch_org = 'X'.

    v_head-pur_group = wa_pohdr-purch_grp.
    v_headx-pur_group = 'X'.

    v_head-comp_code = wa_pohdr-comp_code.
    v_headx-comp_code = 'X'.

    v_head-item_intvl = 1. "LV_COUNT. "ITEM_INTVL."'10'.
    v_headx-item_intvl = 'X'.

    IF v_head-purch_org = '1710'.
      v_head-currency = 'USD'.
      v_headx-currency = 'X'.
      v_head-currency_iso = 'USD'.
      v_headx-currency_iso = 'X'.
    ELSEIF v_head-purch_org = '1010'.
      v_head-currency = 'EUR'.
      v_headx-currency = 'X'.
      v_head-currency_iso = 'EUR'.
      v_headx-currency_iso = 'X'.
    ENDIF.

****************added by i065658*******
*    v_head-PO_REL_IND = 'G'.
*    v_headx-PO_REL_IND = 'X'.
****************************************
**   add items.

    CLEAR lv_seq.
    LOOP AT it_poitems INTO wa_poitem  WHERE pohdr_seqno = wa_pohdr-pohdr_seqno.

      IF lv_seq = 0.
        lv_seq = lv_seq + 1.
      ENDIF.

      TRANSLATE wa_poitem-material TO UPPER CASE.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = wa_poitem-material
        IMPORTING
          output       = wa_poitem-material
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.


      i_item-po_item   = lv_seq * 10.
      i_itemx-po_item = lv_seq * 10.

      i_item-material  = wa_poitem-material.
      i_itemx-material = 'X'.

      i_item-plant     = wa_poitem-plant.
      i_itemx-plant  = 'X'.

      IF wa_poitem-stge_loc IS NOT INITIAL .
        i_item-stge_loc = wa_poitem-stge_loc.
        i_itemx-stge_loc = 'X'.
      ELSEIF wa_pohdr-stge_loc IS NOT INITIAL.
        i_item-stge_loc = wa_pohdr-stge_loc.
        i_itemx-stge_loc = 'X'.
      ENDIF.

      i_item-quantity  = wa_poitem-quantity.
      i_itemx-quantity = 'X'.

      IF wa_poitem-net_price IS NOT INITIAL.
        i_item-net_price = wa_poitem-net_price.
        i_item-po_price = '2'. " 1 -- gross , 2 as net
        i_itemx-net_price = 'X'.
        i_itemx-po_price = 'X'.
      ENDIF.

**********added by i065658********
      IF wa_poitem-plant EQ '1710'.
        i_item-tax_code = 'I0'. "V0 no tax.  V1 is the default maintained
        i_itemx-tax_code = 'X'.
      ELSEIF wa_poitem-plant EQ '1010'.
        i_item-tax_code = 'V0'. "V0 no tax.  V1 is the default maintained
        i_itemx-tax_code = 'X'.
      ENDIF.

****************************
*}   INSERT
      APPEND i_item.

      i_itemx-po_itemx = 'X'.
      i_itemx-matl_group = 'X'.
      APPEND i_itemx.


*}   REPLACE

      i_sched-po_item = lv_seq * 10.
      i_schedx-po_item = lv_seq * 10.

      i_sched-sched_line = lv_seq.
      i_schedx-sched_line = lv_seq.

      IF wa_poitem-delivery_date IS NOT INITIAL.
        i_sched-delivery_date = wa_poitem-delivery_date.
        i_schedx-delivery_date = 'X'.
      ENDIF.

      i_sched-quantity = wa_poitem-quantity.
      i_schedx-quantity = 'X'.

      i_schedx-po_itemx = 'X'.
      i_schedx-sched_linex = 'X'.

      APPEND i_sched.
      APPEND i_schedx.

      lv_seq = lv_seq + 1.

      CLEAR wa_poitem.
    ENDLOOP.  " End of item loop

    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader         = v_head
        poheaderx        = v_headx
      IMPORTING
        exppurchaseorder = ponumber
      TABLES
        return           = i_return
        poitem           = i_item
        poitemx          = i_itemx.
*        poschedule       = i_sched
*        poschedulex      = i_schedx.

*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*     IMPORTING
*       RETURN        =





    READ TABLE i_return WITH KEY type = 'S' id = '06' number = 017.
    IF sy-subrc EQ 0.
      wa_log-msg = i_return-message.
      wa_log-result_flag = 'S'.
      wa_log-ponumber = ponumber.
*      data:porel_msg type char255,
*           porel_flag type char1.
*      CALL FUNCTION 'ZPO_RELEASE'
*        EXPORTING
*          PONUM          = ponumber
*          REL_CODE       = 'MA'
*       IMPORTING
*         RET_MSG        = porel_msg
*         FLAG           = porel_flag.
*      CONCATENATE wa_log-msg 'and also released' into wa_log-msg SEPARATED BY space.


    ELSE.
      LOOP AT i_return WHERE number NE '001' AND number NE '000' AND type = 'E'.
        wa_log-msg = i_return-message.
      ENDLOOP.
      wa_log-result_flag = 'E'.
    ENDIF.


    CLEAR :  v_head, v_headx,i_return[],
          i_item[],i_itemx[],i_sched[],i_schedx[].

    DATA: lv_itm TYPE i VALUE 0.
    LOOP AT it_poitems INTO wa_poitem WHERE pohdr_seqno = wa_pohdr-pohdr_seqno.
      IF lv_itm EQ 0.
        lv_itm = lv_itm + 1.
      ENDIF.
      MOVE-CORRESPONDING wa_log TO ls_datagen_log.
      CONCATENATE sy-uzeit '[' time_cnt ']' INTO log_time.
      ADD 1 TO time_cnt.

      ls_datagen_log-iseq_no = lv_itm * 10.
      ls_datagen_log-material = wa_poitem-material.
      ls_datagen_log-qty = wa_poitem-quantity.
      ls_datagen_log-plant = wa_poitem-plant.
      ls_datagen_log-stge_loc = wa_poitem-stge_loc.
      ls_datagen_log-date_posted = wa_poitem-delivery_date.
      ls_datagen_log-net_price = wa_poitem-net_price.

      ls_datagen_log-mandt = sy-mandt.
      ls_datagen_log-transaction_type = 'PO'.
      ls_datagen_log-hseq_no = wa_pohdr-pohdr_seqno."lv_log_seqno.
      ls_datagen_log-created_on = sy-datum.
      ls_datagen_log-created_at = log_time.
      ls_datagen_log-created_by = sy-uname.
      ls_datagen_log-documentno = ponumber.

      ls_datagen_log-hseq_no = wa_pohdr-pohdr_seqno.
      ls_datagen_log-doc_type = wa_pohdr-doc_type.
      ls_datagen_log-prospect = wa_pohdr-vendor.
      ls_datagen_log-purch_org = wa_pohdr-purch_org.
      ls_datagen_log-comp_code = wa_pohdr-comp_code.
      ls_datagen_log-purch_grp = wa_pohdr-purch_grp.
      ls_datagen_log-pmtterms = wa_pohdr-pmtterms.
      ls_datagen_log-stge_loc = wa_pohdr-stge_loc.

      APPEND ls_datagen_log TO lt_datagen_log.
*      INSERT INTO zpa_logging VALUES ls_datagen_log.

      lv_itm = lv_itm + 1.

    ENDLOOP.
    CLEAR lv_itm.
    MOVE-CORRESPONDING ls_datagen_log TO wa_log.
    IF wa_log-result_flag = 'S'.
      APPEND wa_log TO it_success_log.
      APPEND wa_log TO ot_log.
    ELSEIF wa_log-result_flag = 'E'.
      APPEND wa_log TO it_failure_log.
      APPEND wa_log TO ot_log.
    ENDIF.

    CLEAR : ls_datagen_log,wa_pohdr, wa_log,ponumber.

  ENDLOOP.  " header loop

  IF lt_datagen_log[] IS NOT INITIAL .
****    added by I065658
    SORT lt_datagen_log DESCENDING BY iseq_no documentno.
    DELETE ADJACENT DUPLICATES FROM lt_datagen_log COMPARING hseq_no documentno. "we need to this to have only one entry into the logging table
***    *****end of add by i065658
    MODIFY zpa_logging FROM TABLE lt_datagen_log.
  ENDIF.


  DATA :count   TYPE i,
        lv_rec  TYPE char5,
        lv_succ TYPE char5,
        lv_fail TYPE char5.
  IF ot_log[] IS NOT INITIAL.
    DESCRIBE TABLE it_pohdr LINES count.
    WRITE : count TO lv_rec.
    CLEAR count.
    DESCRIBE TABLE it_success_log LINES count.
    WRITE : count TO lv_succ.
    CLEAR count.
    DESCRIBE TABLE it_failure_log LINES   count.
    WRITE : count TO lv_fail.

    CONCATENATE lv_rec 'records Processed' lv_succ 'Purchase orders created successfully' INTO msg SEPARATED BY space.

  ENDIF.

ENDFUNCTION.
