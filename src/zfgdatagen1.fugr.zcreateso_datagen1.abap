FUNCTION ZCREATESO_DATAGEN1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR255
*"  TABLES
*"      IT_SOHDR TYPE  ZSO_HEADER_TT
*"      IT_SOITEMS TYPE  ZSO_ITEMS_TT
*"      OT_LOG STRUCTURE  ZSOCREATE_LOG OPTIONAL
*"      OT_DBLOG STRUCTURE  ZDATAGEN_LOGGING OPTIONAL
*"--------------------------------------------------------------------

  SET BLANK LINES OFF.


  DATA: lv_soid    TYPE bapivbeln-vbeln,
        ls_sohdr   TYPE bapisdhd1,
        ls_soitem  TYPE bapisditm,
        lt_soitem  TYPE STANDARD TABLE OF bapisditm,
        ls_partner TYPE bapiparnr,
        lt_partner TYPE STANDARD TABLE OF bapiparnr,
        lt_return  TYPE bapirettab,
        wa_return  TYPE bapiret2,
        lv_msg     TYPE char50,
        lr_return  TYPE REF TO bapiret2.
*        lx_busi_exc   TYPE cx_mgw_busi_exception,
*        lo_meco       TYPE REF TO /iwbep/if_message_container.

  DATA: ls_schedules_in  TYPE  bapischdl,
        lt_schedules_in  TYPE  STANDARD TABLE OF bapischdl,
        ls_schedules_inx TYPE  bapischdlx,
        lt_schedules_inx TYPE  STANDARD TABLE OF bapischdlx.

  CONSTANTS: lc_soitems TYPE string VALUE 'SalesOrderItems'.

  DATA : wa_soitem TYPE zso_items,
         wa_sohdr  TYPE zso_header,
         sonumber  TYPE  bapivbeln-vbeln.

  DATA : wa_log         TYPE zsocreate_log,
         it_success_log TYPE TABLE OF zsocreate_log,
         it_failure_log TYPE TABLE OF zsocreate_log.

  DATA : ls_datagen_log TYPE zdatagen_logging,
         lt_datagen_log TYPE TABLE OF zdatagen_logging.

*********  logging details ****************
*  DATA : lv_log_seqno TYPE i.
  DATA: time_cnt TYPE n VALUE 1,
        log_time TYPE char10.
*  SELECT MAX( seq_no ) FROM zdatagen_logging INTO lv_log_seqno.

*****************************************
*********Header data
******************************************
  LOOP AT it_sohdr INTO wa_sohdr.

    CALL FUNCTION 'CONVERSION_EXIT_AUART_INPUT'
      EXPORTING
        input         = wa_sohdr-doc_type
     IMPORTING
       OUTPUT        = wa_sohdr-doc_type.

    ls_sohdr-doc_type = wa_sohdr-doc_type. "ls_salesorder-doc_type.
    ls_sohdr-sales_org = wa_sohdr-sales_org." ls_salesorder-sales_org.
    ls_sohdr-distr_chan = wa_sohdr-distrib_channel. " ls_salesorder-distr_chan.
    ls_sohdr-division = wa_sohdr-division . "ls_salesorder-division.
    ls_sohdr-purch_no_c = wa_sohdr-collect_no.

    IF wa_sohdr-date_posted IS NOT INITIAL.
      ls_sohdr-req_date_h =  wa_sohdr-date_posted.
      ls_sohdr-PRICE_DATE = wa_sohdr-date_posted.
      ls_sohdr-DOC_DATE = wa_sohdr-date_posted.
    ELSE.
      ls_sohdr-req_date_h = sy-datum.
      ls_sohdr-PRICE_DATE = sy-datum.
      wa_sohdr-date_posted = sy-datum.
    ENDIF.



    " Fill Partner table with one entry - Sold-to Party
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_sohdr-soldto
      IMPORTING
        output = wa_sohdr-soldto.

    ls_partner-partn_role = 'AG'.
    ls_partner-partn_numb = wa_sohdr-soldto. "ls_salesorder-sold_to.
    APPEND ls_partner TO lt_partner.

    DATA : lv_seq TYPE i VALUE 0.."POSNR_VA.
    " Item data
*    LOOP AT ls_salesorder-salesorderitems INTO ls_salesorderitem.
    CLEAR lv_seq.
    LOOP AT it_soitems INTO wa_soitem WHERE sohdr_seqno = wa_sohdr-sohdr_seqno.
      IF lv_seq = 0.
        lv_seq = lv_seq + 1.
      ENDIF.
      ls_soitem-itm_number = lv_seq * 10. "WA_SOITEM-itm_number.
      ls_soitem-material = wa_soitem-material.
*      ls_soitem-plant = WA_SOITEM-plant.
      ls_soitem-target_qty = wa_soitem-qty.
      APPEND ls_soitem TO lt_soitem.
*---Schedule line update
      ls_schedules_in-itm_number = lv_seq * 10 ."WA_SOITEM-itm_number.
      ls_schedules_in-sched_line = '0001'.
      ls_schedules_in-req_qty    = wa_soitem-qty.
*      ls_SCHEDULES_in-REQ_DATE = WA_SOITEM-req_date.
       ls_schedules_in-SCHED_TYPE = 'CN'.

*****      test code for trial***
*      ls_schedules_in-SCHED_TYPE = 'CN'.
*      ls_schedules_inx-SCHED_TYPE = 'CN'.
************************


      APPEND ls_schedules_in TO lt_schedules_in.

      ls_schedules_inx-itm_number = lv_seq * 10 . "WA_SOITEM-itm_number.
      ls_schedules_inx-sched_line = '0001'.
      ls_schedules_inx-req_qty    = 'X'.
      ls_schedules_inx-updateflag = 'I'.
*      ls_SCHEDULES_inx-REQ_DATE = 'X'.
      ls_schedules_inx-SCHED_TYPE = 'CN'.
      APPEND ls_schedules_inx TO lt_schedules_inx.



*      wa_log-item_no = lv_seq * 10.
*      wa_log-material = wa_soitem-material.
*      wa_log-qty = wa_soitem-qty.
*      add 1 to time_cnt.
*      lv_log_seqno = lv_log_seqno + 1. " RECORD NO OF LOGGING DB TABLE.
      lv_seq = lv_seq + 1.
    ENDLOOP.

            CALL FUNCTION 'SD_SALES_DOCUMENT_INIT'
         EXPORTING
           STATUS_BUFFER_REFRESH       = 'X'
           KEEP_LOCK_ENTRIES           = ' '
           SIMULATION_MODE_BAPI        = 'X'
           CALL_ACTIVE                 = ' '
                  .

    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
*       SALESDOCUMENTIN     =
        order_header_in     = ls_sohdr
*       ORDER_HEADER_INX    =
*       SENDER              =
*       BINARY_RELATIONSHIPTYPE       =
*       INT_NUMBER_ASSIGNMENT         =
*       BEHAVE_WHEN_ERROR   =
*       LOGIC_SWITCH        =
*       TESTRUN             =
*       CONVERT             = ' '
      IMPORTING
        salesdocument       = sonumber
      TABLES
        return              = lt_return
        order_items_in      = lt_soitem
*       ORDER_ITEMS_INX     =
        order_partners      = lt_partner
        order_schedules_in  = lt_schedules_in
        order_schedules_inx = lt_schedules_inx.

    IF  sonumber IS INITIAL.
      MOVE 'FAILED' TO lv_msg.
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

        CONCATENATE lv_msg wa_log-msg INTO wa_log-msg SEPARATED BY space.
        wa_log-sohdr_seqno = wa_sohdr-sohdr_seqno.
        wa_log-result_flag = 'E'.
      ENDLOOP.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*         IMPORTING
*           RETURN        =
                  .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
   EXPORTING
     WAIT          = 'X'
*   IMPORTING
*     RETURN        =
        .

      wa_log-sohdr_seqno = wa_sohdr-sohdr_seqno.
      wa_log-result_flag = 'S'.
      wa_log-sonumber = sonumber.

      CONCATENATE 'SUCCESS:' 'Sales Order ' sonumber 'Created ' INTO wa_log-msg SEPARATED BY space.
    ENDIF.


    DATA: lv_itm TYPE i VALUE 0.
    LOOP AT it_soitems INTO wa_soitem WHERE sohdr_seqno = wa_sohdr-sohdr_seqno.
      IF lv_itm EQ 0.
        lv_itm = lv_itm + 1.
      ENDIF.
      MOVE-CORRESPONDING wa_log TO ls_datagen_log.
      CONCATENATE sy-uzeit '[' time_cnt ']' INTO log_time.
      ADD 1 TO time_cnt.

      ls_datagen_log-iseq_no = lv_itm * 10.
      ls_datagen_log-material = wa_soitem-material.
      ls_datagen_log-qty = wa_soitem-qty.

      ls_datagen_log-mandt = sy-mandt.
      ls_datagen_log-TRANSACTION_TYPE = 'SO'.
      ls_datagen_log-hseq_no = wa_sohdr-sohdr_seqno."lv_log_seqno.
      ls_datagen_log-created_on = sy-datum.
      ls_datagen_log-created_at = log_time.
      ls_datagen_log-created_by = sy-uname.
      ls_datagen_log-DOCUMENTNO = sonumber.

*      ls_datagen_log-sohdr_seqno = wa_sohdr-sohdr_seqno.
      ls_datagen_log-date_posted = wa_sohdr-date_posted.
      ls_datagen_log-doc_type = wa_sohdr-doc_type.
      ls_datagen_log-collect_no = wa_sohdr-collect_no.
      ls_datagen_log-sales_org = wa_sohdr-sales_org.
      ls_datagen_log-distrib_channel = wa_sohdr-distrib_channel.
      ls_datagen_log-division = wa_sohdr-division.
      ls_datagen_log-soldto = wa_sohdr-soldto.

      APPEND ls_datagen_log TO lt_datagen_log.
*      INSERT INTO zdatagen_logging VALUES ls_datagen_log.

      lv_itm = lv_itm + 1.
    ENDLOOP.
    clear lv_itm.

    MOVE-CORRESPONDING ls_datagen_log to wa_log.
        IF wa_log-result_flag = 'S'.
      APPEND wa_log TO it_success_log.
      APPEND wa_log TO ot_log.
    ELSEIF wa_log-result_flag = 'E'.
      APPEND wa_log TO it_failure_log.
      APPEND wa_log TO ot_log.
    ENDIF.

*** added on 25/05/2018  after upgrade to 1709**
    MODIFY zdatagen_logging FROM ls_datagen_log.

    CLEAR : ls_datagen_log.
    CLEAR : wa_sohdr, wa_log, ls_sohdr,
            lt_return,lt_soitem,lt_partner,lt_schedules_in,lt_schedules_inx, sonumber, lv_msg.

  ENDLOOP.

  IF lt_datagen_log[] IS NOT INITIAL .
*      MODIFY zdatagen_logging FROM TABLE lt_datagen_log.
      OT_DBLOG[] = lt_datagen_log[].
    ENDIF.

  DATA :count   TYPE i,
        lv_rec  TYPE char5,
        lv_succ TYPE char5,
        lv_fail TYPE char5.
  IF ot_log[] IS NOT INITIAL.
    DESCRIBE TABLE it_sohdr LINES count.
    WRITE : count TO lv_rec.
    CLEAR count.
    DESCRIBE TABLE it_success_log LINES count.
    WRITE : count TO lv_succ.
    CLEAR count.
    DESCRIBE TABLE it_failure_log LINES   count.
    WRITE : count TO lv_fail.

    CONCATENATE lv_rec 'records Processed' lv_succ 'sales orders created successfully' INTO msg SEPARATED BY space.

  ENDIF.

ENDFUNCTION.
