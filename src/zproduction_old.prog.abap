*&---------------------------------------------------------------------*
*& Report ZPRODUCTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zproduction_old.

PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum,
             lv_date2 TYPE dats DEFAULT sy-datum.


* internal structure to load remote data
DATA: ls_sales_order TYPE     zdemo_data, "
      lt_sales_order TYPE TABLE OF zdemo_data,
      lt_dbupdate_so TYPE TABLE OF zdemo_data,
      ls_dbupdate_so TYPE zdemo_data.

DATA : ls_datagen_log TYPE zdatagen_logging,
       lt_datagen_log TYPE TABLE OF zdatagen_logging.
DATA : lv_seq TYPE i VALUE 0.."POSNR_VA.
*  *********  logging details ****************
*  DATA : lv_log_seqno TYPE i.
DATA: time_cnt TYPE n VALUE 1,
      log_time TYPE char10.

TYPES: BEGIN OF ty_productionlog,
         matnr     TYPE matnr,
         mtart     TYPE mtart,
         qty       TYPE dzmeng,
         prodorder TYPE aufnr,
         msg       TYPE  bapi_msg,
         flag      TYPE char1,
       END OF ty_productionlog.

DATA: ls_productionlog TYPE ty_productionlog,
      lt_productionlog TYPE TABLE OF ty_productionlog.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime ."sy-uzeit.
NEW-LINE.

DATA: lv_timestamp      TYPE timestamp,
      lv_timestamp_end  TYPE timestamp,
      lv_tzntimestp     TYPE tzntimestp, " timestamp.
      lv_tzntimestp_end TYPE tzntimestp.
DATA: temp_date TYPE dats.

CONVERT DATE lv_date1 TIME '000000'
                 INTO TIME STAMP lv_timestamp
                 TIME ZONE 'UTC'.
WRITE:lv_timestamp TO lv_tzntimestp.
CONVERT DATE lv_date2 TIME '235959'
                INTO TIME STAMP lv_timestamp_end
                TIME ZONE 'UTC'.
WRITE:lv_timestamp_end TO lv_tzntimestp_end.

DATA : lv_date TYPE tzntimestp .
IF lv_date1 IS NOT INITIAL.
  CONCATENATE lv_date1 '000000' INTO lv_date.
ELSE.
  CONCATENATE sy-datum '000000' INTO lv_date.
ENDIF.
SELECT * FROM zdemo_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .
*DELETE lt_sales_order where material NE 'MZ-FG-R200'.  " delete this.
DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E'.
IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
  WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
  EXIT.
ENDIF.

DELETE  lt_sales_order WHERE material CS 'MZ-TG-Y'. "delete Trading goods materials
DATA: lt_mat  TYPE   zso_items_tt_copy,
      ls_mat  LIKE LINE OF lt_mat,
      lt_mat2 TYPE   zso_items_tt_copy,
      ls_mat2 LIKE LINE OF lt_mat2,
      lt_mat3 TYPE   zso_items_tt_copy,
      ls_mat3 LIKE LINE OF lt_mat3.

LOOP AT lt_sales_order INTO ls_sales_order .
  ls_mat-sohdr_seqno = sy-tabix.
  ls_mat-material = ls_sales_order-material.
  ls_mat-qty = ls_sales_order-quantity.
  ls_mat-collect_no = ls_sales_order-collect_no.  " Collect for identiying individual rows
  ls_mat-record_date = ls_sales_order-dateposted.
  APPEND ls_mat TO lt_mat.
**********End of add items n materials.
  CLEAR:  ls_mat." ls_sohdr, ls_soitems,
ENDLOOP.

SORT lt_mat ASCENDING BY collect_no.
DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING collect_no.

*************<<<<<<<<<<<<<<Write material and qty*************
IF lt_mat[] IS NOT INITIAL.
  lt_mat2[] = lt_mat[].
  temp_date = lv_date1.
  WHILE temp_date <= lv_date2.
    LOOP AT lt_mat INTO ls_mat WHERE record_date = temp_date.
*  write: /
*      LS_MAT-SOHDR_SEQNO ,
*  LS_MAT-MATERIAL,
*  LS_MAT-QTY.
      LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material AND record_date = temp_date.
        ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
        ls_mat3-material = ls_mat2-material.
        ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
        ls_mat3-collect_no = ls_mat2-collect_no." will always fill the last records Collect_no.
        ls_mat3-record_date = ls_mat2-record_date.
        CLEAR ls_mat2.
      ENDLOOP.
      APPEND ls_mat3 TO lt_mat3.
      CLEAR ls_mat3.
      DELETE lt_mat WHERE material = ls_mat-material AND record_date = temp_date. " and SOHDR_SEQNO = ls_mat-SOHDR_SEQNO.
      CLEAR ls_mat.
    ENDLOOP.
    temp_date  = temp_date + 1.
  ENDWHILE.
ENDIF.

IF lt_mat3[] IS NOT INITIAL.
  LOOP AT lt_mat3 INTO ls_mat3.
    NEW-LINE.
    CONCATENATE sy-uzeit '[' time_cnt ']' INTO log_time.
    ADD 1 TO time_cnt.
    CLEAR ls_productionlog.
    MOVE-CORRESPONDING ls_mat3 TO ls_productionlog.
    ls_productionlog-matnr = ls_mat3-material.
    ls_productionlog-qty = ls_mat3-qty.
    MOVE-CORRESPONDING ls_mat3 TO ls_datagen_log.
    ls_datagen_log-iseq_no = sy-tabix.
    ls_datagen_log-mandt = sy-mandt.
    ls_datagen_log-transaction_type = 'PROD'.
    ls_datagen_log-created_on = sy-datum.
    ls_datagen_log-created_at = log_time.
    ls_datagen_log-created_by = sy-uname.

    ls_productionlog-flag = 'S'.
***   create prod order.
    DATA: is_prodorder TYPE  zprodorder_str,
          order_number TYPE  aufnr,
          prod_flag    TYPE  char1,
          prod_msg     TYPE  char100.
    CLEAR: is_prodorder.

    is_prodorder-matnr =  ls_mat3-material.
    is_prodorder-plant = '1710'.
    is_prodorder-order_type = 'YBM1'. " 'YBM1' . "'ZMTS'.
    is_prodorder-total_quantity = ls_mat3-qty.
    is_prodorder-schedule_type = '4'.
    is_prodorder-start_date = ls_mat3-record_date.
    is_prodorder-end_date = ls_mat3-record_date.

    CALL FUNCTION 'ZCREATE_PRODORDER'
      EXPORTING
        is_prodorder = is_prodorder
      IMPORTING
        order_number = order_number
        flag         = prod_flag
        msg          = prod_msg.

    IF order_number IS INITIAL.
      WRITE : 'failed:Production order not created for material',ls_mat3-material,prod_msg. NEW-LINE.
      ls_productionlog-msg = prod_msg.
      ls_productionlog-flag = 'E'.
      APPEND ls_productionlog TO lt_productionlog.

      ls_datagen_log-result_flag = ls_productionlog-flag.
      ls_datagen_log-msg = ls_productionlog-msg.
      ls_datagen_log-date_posted = ls_mat3-record_date.
      APPEND ls_datagen_log TO lt_datagen_log.
      CONTINUE.
    ELSE. " continue to release Production order.
      ls_productionlog-prodorder = order_number.
      DATA : rel_flag          TYPE  char1,
             rel_msg           TYPE  char100,
             es_return         TYPE  bapiret2,
             it_orders         TYPE  tb_bapi_order_key,
             wa_orders         LIKE LINE OF it_orders,
             et_return_details TYPE  bapirettab,
             wa_return_details LIKE LINE OF et_return_details.

      wa_orders-order_number = order_number.
      APPEND wa_orders TO it_orders.
      CLEAR wa_orders.
      CALL FUNCTION 'ZRELEASE_PRODORDER'
        IMPORTING
          flag              = rel_flag
          msg               = rel_msg
          es_return         = es_return
        TABLES
          it_orders         = it_orders
          et_return_details = et_return_details.
      rel_flag = 'S'.
      READ TABLE et_return_details INTO wa_return_details WITH KEY type = 'W' id = 'CO' number = '281'.
      IF sy-subrc IS INITIAL.  " Has some missing parts.
        CONCATENATE wa_return_details-message ls_productionlog-msg INTO ls_productionlog-msg SEPARATED BY space.
      ENDIF.
      LOOP AT et_return_details INTO wa_return_details WHERE type = 'E'.
        rel_flag = 'E'.
        ls_productionlog-flag = 'E'.
        CONCATENATE ls_productionlog-msg wa_return_details-message INTO ls_productionlog-msg SEPARATED BY space.
        APPEND ls_productionlog TO lt_productionlog.
        ls_datagen_log-result_flag = ls_productionlog-flag.
        ls_datagen_log-msg = ls_productionlog-msg.
        ls_datagen_log-date_posted = ls_mat3-record_date.
        APPEND ls_datagen_log TO lt_datagen_log.
        MODIFY zdatagen_logging FROM ls_datagen_log.
      ENDLOOP.
      CLEAR: et_return_details,it_orders,es_return.
      IF rel_flag = 'E'.
        CONTINUE.
      ELSEIF rel_flag = 'S'. "Confirm activities if PO released.
        DATA: confirm_flag TYPE  char1,
              confirm_msg  TYPE  char100.
        WAIT UP TO 1 SECONDS.
        CALL FUNCTION 'ZCONFIRM_PRODORDER'
          EXPORTING
            iv_order_number   = order_number
            iv_pstng_date     = ls_mat3-record_date
*           IV_UNIT           =
            iv_total_quantity = ls_mat3-qty
*           IV_YIELD1         =
*           IV_YIELD2         =
*           IV_YIELD3         =
*           IV_YIELD4         =
          IMPORTING
            flag              = confirm_flag
            msg               = confirm_msg.
        IF confirm_flag EQ 'E'.
          WRITE : confirm_msg.
          ls_productionlog-flag = 'E'.
          ls_productionlog-msg = confirm_msg.
          ls_datagen_log-date_posted = ls_mat3-record_date.
          APPEND ls_datagen_log TO lt_datagen_log.
          ls_datagen_log-result_flag = ls_productionlog-flag.
          ls_datagen_log-msg = ls_productionlog-msg.
          ls_datagen_log-date_posted = ls_mat3-record_date.
          APPEND ls_datagen_log TO lt_datagen_log.
          MODIFY zdatagen_logging FROM ls_datagen_log.
          ls_datagen_log-documentno = ls_productionlog-prodorder.
          ls_datagen_log-result_flag = ls_productionlog-flag.
          ls_datagen_log-msg = ls_productionlog-msg.
          ls_datagen_log-date_posted = ls_mat3-record_date.
          MODIFY zdatagen_logging FROM ls_datagen_log.
        ELSE. "IF confirm_flag EQ 'S'.
          WRITE :order_number,ls_mat3-qty, 'for' , ls_mat3-material,confirm_msg.
          NEW-LINE.
          ls_productionlog-msg = confirm_msg.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND ls_productionlog TO lt_productionlog.
    ls_datagen_log-documentno = ls_productionlog-prodorder.
    ls_datagen_log-result_flag = ls_productionlog-flag.
    ls_datagen_log-msg = ls_productionlog-msg.
    ls_datagen_log-date_posted = ls_mat3-record_date.
    MODIFY zdatagen_logging FROM ls_datagen_log.
    APPEND ls_datagen_log TO lt_datagen_log.
    CLEAR: ls_productionlog,  ls_datagen_log.
    CLEAR ls_mat3.
  ENDLOOP.
  NEW-LINE.
ENDIF.

*IF lt_datagen_log[] IS NOT INITIAL .
*  MODIFY zdatagen_logging FROM TABLE lt_datagen_log.
*ENDIF.

*********** Print Total Processing time and End Time*******
endtime = sy-uzeit.
NEW-LINE.
WRITE : 'END TIME' , endtime .
NEW-LINE.
DATA : totaltime TYPE sy-uzeit.
totaltime = endtime - starttime .
NEW-LINE.
WRITE : 'Total Time',totaltime.
***************************************End of time print******************
********************************Send Mail********************************
IF lt_productionlog[] IS NOT INITIAL.
  DATA: temp_productionlog TYPE TABLE OF ty_productionlog,  " temp table if any processing needed.
        wa_productionlog   TYPE ty_productionlog.
*** Header
  DATA : wa_header TYPE sodocchgi1.

*** Contents Data
  DATA : it_content   TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0,
         wa_content   TYPE solisti1,
*** Receivers Data
         it_receivers TYPE STANDARD TABLE OF somlreci1 INITIAL SIZE 0,
         wa_receivers TYPE somlreci1,
         it_para      TYPE STANDARD TABLE OF soparai1 INITIAL SIZE 0,
         wa_para      TYPE soparai1,
         lv_qty       TYPE c LENGTH 13,
         c_date1      TYPE c LENGTH 10,
         c_date2      TYPE c LENGTH 10.
  WRITE  lv_date1 TO c_date1 .
  WRITE  lv_date2 TO c_date2 .

*  wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
*  wa_receivers-rec_type = 'U'.
*  wa_receivers-com_type = 'INT'.
*  APPEND wa_receivers TO it_receivers.
*  CLEAR: wa_receivers.

********Adding recivers list on exceptional/Error cases.
  DATA: maillist_productionlog TYPE ty_productionlog.
  READ TABLE lt_productionlog INTO maillist_productionlog WITH KEY flag = 'E'.
  IF sy-subrc IS INITIAL.
*******        Adding Somendra for exception mail list.
*    wa_receivers-receiver = 'somendra.sahu@sap.com'.
*    wa_receivers-rec_type = 'U'.
*    wa_receivers-com_type = 'INT'.
*    APPEND wa_receivers TO it_receivers.
*    CLEAR: wa_receivers.
  ENDIF.

******* End of Exception receiverslist***

  wa_receivers-receiver = sy-uname.                         "'I065658'.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'.  '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'Production Cycle for ' c_date1 'to' c_date2  INTO wa_header-obj_descr SEPARATED BY space.

  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This automatic email serves to notify that' 'Production cycle for the records from ' c_date1 'to' c_date2 'has been completed.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
*  DATA : successful_count      TYPE i,
*         successful_count_char TYPE c LENGTH 10,
  DATA:      tot_rec_char          TYPE c LENGTH 10.
*         error_count           TYPE i,
*         error_count_char      TYPE c LENGTH 10.

  temp_productionlog[] =  lt_productionlog[].
*********** Total records to process******************
  DATA : gv_total_records TYPE i.
  DESCRIBE TABLE temp_productionlog LINES gv_total_records.
  DESCRIBE TABLE lt_mat3 LINES gv_total_records.
  WRITE gv_total_records TO tot_rec_char.

  READ TABLE temp_productionlog INTO wa_productionlog WITH KEY flag = 'E'.
  IF sy-subrc IS NOT INITIAL. "Everything executed Successfully
    CONCATENATE 'Hurray!!' 'Production Cycle executed successfully for'  tot_rec_char 'Materials/records ' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    APPEND '<br>' TO it_content.
  ELSE.
    LOOP AT temp_productionlog INTO wa_productionlog.
      lv_qty = ls_productionlog-qty.
      CONDENSE lv_qty.
      IF wa_productionlog-flag = 'E'.
        CONCATENATE 'Production cycle FAILED for ' ls_productionlog-matnr 'Quantity' lv_qty ':' ls_productionlog-msg INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
        APPEND '<br>' TO it_content.
      ELSEIF wa_productionlog-flag = 'S'.
        CONCATENATE 'Production cycle COMPLETED for -' ls_productionlog-matnr  'Quantity' lv_qty ':' ls_productionlog-msg INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
        APPEND '<br>' TO it_content.
      ENDIF.
      CLEAR ls_productionlog.
    ENDLOOP.
    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.

  ENDIF.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND 'Regards,' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND 'Workflow System.' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

*  sy-uname  = 'I065658'."'WF-BATCH' .

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data  = wa_header
      document_type  = 'HTM'
      commit_work    = 'X'
    TABLES
      object_content = it_content
      object_para    = it_para
      receivers      = it_receivers.

ENDIF.  " end of lt_productionlog[] initial check
****************End of mail notification************************
