*&---------------------------------------------------------------------*
*& Report ZPROCURETOPAY_PREDACCOUNTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprocuretopay_predaccounting.

PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum OBLIGATORY,
             lv_date2 TYPE dats DEFAULT sy-datum OBLIGATORY.


DATA: ls_sales_order TYPE   zpredacct_data, "
      lt_sales_order TYPE TABLE OF   zpredacct_data,
      lt_dbupdate_so TYPE TABLE OF  zpredacct_data,
      ls_dbupdate_so TYPE  zpredacct_data.

TYPES: BEGIN OF ty_p2plog,
         matnr    TYPE matnr,
         mtart    TYPE mtart,
         flag     TYPE char1,
         log_date TYPE sy-datum,
         objectid TYPE char40,
         msg      TYPE char255,
       END OF ty_p2plog.

DATA: ls_p2plog TYPE ty_p2plog,
      lt_p2plog TYPE TABLE OF ty_p2plog.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.

AT SELECTION-SCREEN.
  IF lv_date1 GT sy-datum.
    MESSAGE 'From date cannot be Future Date' Type 'E'.
  ELSEIF lv_date2 GT sy-datum.
    MESSAGE 'To date cannot be Future Date' Type 'E'.
  ELSEIF lv_date1 GT lv_date2.
    MESSAGE 'From date cannot be greater than To Date' Type 'E'.
    Return.
  ENDIF.

START-OF-SELECTION.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime.
NEW-LINE.

DATA: lv_timestamp      TYPE timestamp,
      lv_timestamp_end  TYPE timestamp,
      lv_tzntimestp     TYPE tzntimestp, " timestamp.
      lv_tzntimestp_end TYPE tzntimestp.
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
SELECT * FROM zpredacct_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .
*DELETE lt_sales_order where material NE 'MZ-FG-C900'.  " delete this.
*DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E'.
DELETE lt_sales_order WHERE status NE 'F'. " Select only records of Future posted status.
IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
  WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
  EXIT.
ENDIF.

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
  SELECT SINGLE mtart FROM mara INTO ls_mat-mtart WHERE matnr = ls_mat-material.
  APPEND ls_mat TO lt_mat.
**********End of add items n materials.
  CLEAR:  ls_mat." ls_sohdr, ls_soitems,
ENDLOOP.

SORT lt_mat ASCENDING BY collect_no.
DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING collect_no.
DATA: temp_lt_mat LIKE lt_mat.
temp_lt_mat[] = lt_mat[]. " for qty per material without date.
*************<<<<<<<<<<<<<<Concatenate Qty for each of the materials for each of the date*************
IF lt_mat[] IS NOT INITIAL.
  lt_mat2[] = lt_mat[].
  LOOP AT lt_mat INTO ls_mat.
    LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material AND record_date = ls_mat-record_date.
      ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
      ls_mat3-material = ls_mat2-material.
      ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
      ls_mat3-collect_no = ls_mat2-collect_no." will always fill the last records Collect_no.
      ls_mat3-record_date = ls_mat2-record_date.
      ls_mat3-mtart = ls_mat2-mtart.
      CLEAR ls_mat2.
    ENDLOOP.
    APPEND ls_mat3 TO lt_mat3.
    CLEAR ls_mat3.
    DELETE lt_mat WHERE material = ls_mat-material AND record_date = ls_mat-record_date.
    CLEAR ls_mat.
  ENDLOOP.
ENDIF.

DATA: lt_procure TYPE TABLE OF zdatagen_procure,
      ls_procure TYPE zdatagen_procure.

TYPES : BEGIN OF ty_p2p,
          matnr TYPE matnr,
          qty   TYPE dzmeng,
          po    TYPE bapimepoheader-po_number,
          gr    TYPE bapi2017_gm_head_ret-mat_doc,
          inv   TYPE re_belnr,
        END OF ty_p2p.

TYPES: BEGIN OF ty_mattype,
         mtart TYPE mtart,
       END OF ty_mattype.
DATA: lt_mattype TYPE TABLE OF ty_mattype,
      ls_mattype TYPE ty_mattype.
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
DATA : lv_lock TYPE sy-subrc." VALUE 1.

IF lt_mat3[] IS NOT INITIAL.
  LOOP AT lt_mat3 INTO ls_mat3.
    IF ls_mat3-mtart = 'HAWA'.
      CLEAR : ls_p2plog.
      ls_p2plog-mtart = ls_mat3-mtart.
      CLEAR :  lt_procure,  ls_procure,it_pohdr , it_poitems.
      SELECT  * FROM zdatagen_procure INTO TABLE lt_procure  WHERE mtart EQ ls_mat3-mtart AND matnr EQ ls_mat3-material.
      IF lt_procure[] IS NOT INITIAL.
        LOOP AT lt_procure INTO ls_procure.
          wa_poitems-pohdr_seqno = 1.
          wa_poitems-material = ls_procure-component. "MATNR
          wa_poitems-plant = ls_procure-plant .
          wa_poitems-stge_loc = ls_procure-stge_loc.
*          READ TABLE lt_mat3 INTO ls_mat3 WITH KEY material = ls_procure-component."MATERIAL.
          wa_poitems-quantity =  ls_procure-comp_qty_factor * ls_mat3-qty. "multiple factor as maintained in ZDATAGEN_PROCURE table
          wa_poitems-delivery_date = ls_mat3-record_date. "sy-datum.
          wa_poitems-net_price = ls_procure-net_price .
          APPEND wa_poitems TO it_poitems.
          ls_p2plog-matnr = ls_mat3-material.
        ENDLOOP.
        wa_pohdr-pohdr_seqno = 1. " check this.
        wa_pohdr-vendor = ls_procure-vendor.
        wa_pohdr-purch_org = ls_procure-purch_org.
        wa_pohdr-comp_code = ls_procure-comp_code.
        wa_pohdr-purch_grp = ls_procure-purch_grp.
        wa_pohdr-doc_type = ls_procure-doc_type.
        wa_pohdr-stge_loc = ls_procure-stge_loc.
        APPEND wa_pohdr TO it_pohdr.

        CALL FUNCTION 'ZCREATEPO_DATAGEN1_PA'
          IMPORTING
            msg        = po_msg
          TABLES
            it_pohdr   = it_pohdr
            it_poitems = it_poitems
            ot_log     = ot_log.
        DELETE ADJACENT DUPLICATES FROM ot_log COMPARING ponumber.
        LOOP AT ot_log INTO wa_log.
          IF wa_log-result_flag = 'S'.
*            WAIT UP TO 1 SECONDS.
*            WRITE: 'Success:', wa_log-ponumber, 'created for material', wa_log-material,'---',wa_log-msg.
*            NEW-LINE.
            ls_p2plog-flag = 'S'.
            ls_p2plog-objectid = wa_log-ponumber.
            ls_p2plog-msg = wa_log-msg.

            wa_gr-po_no = wa_log-ponumber. "PO_ITEM_NO PO_QUANTITY PO_UOM
            APPEND wa_gr TO it_gr.

            WAIT UP TO 3 SECONDS.
            lv_lock = 1.
            WHILE lv_lock IS NOT INITIAL.
              CALL FUNCTION 'ENQUEUE_EMEKKOE'
                EXPORTING
                  mode_ekko      = 'E'
                  mode_ekpo      = 'E'
                  mandt          = sy-mandt
                  ebeln          = wa_log-ponumber
*                 EBELP          =
                  x_ebeln        = ' '
                  x_ebelp        = ' '
                  _scope         = '2'
                  _wait          = ' '
                  _collect       = ' '
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.
              lv_lock = sy-subrc.
              IF lv_lock is INITIAL.
                CALL FUNCTION 'DEQUEUE_EMEKKOE'
                 EXPORTING
                   MODE_EKKO       = 'E'
                   MODE_EKPO       = 'E'
                   MANDT           = SY-MANDT
                   EBELN           = wa_log-ponumber
*                   EBELP           =
                   X_EBELN         = ' '
                   X_EBELP         = ' '
                   _SCOPE          = '3'
                   _SYNCHRON       = ' '
                   _COLLECT        = ' '
                          .
              ENDIF.
            ENDWHILE.



            CALL FUNCTION 'ZCREATEGR_DATAGEN1_PA'
              EXPORTING
                posting_date = ls_mat3-record_date
              IMPORTING
                msg          = gr_msg
                flag         = gr_flag
                ov_mat_doc   = mat_doc
                ov_mat_year  = mat_year
              TABLES
                po_gr        = it_gr.
            IF gr_flag EQ 'S'.
*              WRITE : gr_msg.
*              NEW-LINE.
              ls_p2plog-flag = 'S'.
*              ls_p2plog-objectid = wa_log-ponumber.
              CONCATENATE wa_log-ponumber mat_doc INTO ls_p2plog-objectid SEPARATED BY '-'.
              CLEAR ls_p2plog-msg.
              ls_p2plog-msg = gr_msg.
***                    create Invoice*************
              CALL FUNCTION 'ZCREATEINVOICE_FROM_PO_PA'
                EXPORTING
                  po_number    = wa_gr-po_no
                  posting_date = ls_mat3-record_date
                IMPORTING
                  flag         = inv_flag
                  msg          = inv_msg
                  invoice      = invoice
                  inv_year     = inv_year.

              IF inv_flag = 'S'.
*                WRITE : inv_msg.
*                NEW-LINE.
                ls_p2plog-flag = 'S'.
                CONCATENATE wa_log-ponumber mat_doc invoice INTO ls_p2plog-objectid SEPARATED BY '-'.
                CLEAR ls_p2plog-msg.
                ls_p2plog-msg = inv_msg.
                APPEND ls_p2plog TO lt_p2plog.
              ELSEIF inv_flag = 'E'.
                WRITE : inv_msg.
                NEW-LINE.
                ls_p2plog-flag = 'E'.
                CLEAR ls_p2plog-msg.
                ls_p2plog-msg = inv_msg.
                APPEND ls_p2plog TO lt_p2plog.
              ENDIF.   "  ,End of Invoice.

            ELSE.
              WRITE: gr_msg.
              NEW-LINE.
              ls_p2plog-flag = 'E'.
              CLEAR ls_p2plog-msg.
              ls_p2plog-msg = gr_msg.
              APPEND ls_p2plog TO lt_p2plog.
            ENDIF.
            CLEAR it_gr.
          ELSEIF wa_log-result_flag = 'E'.
            WRITE: 'Failed:', 'PO not created' ,'for material', wa_log-material,'---',wa_log-msg.
            NEW-LINE.
            ls_p2plog-flag = 'E'.
            CONCATENATE 'Failed:' wa_log-msg INTO ls_p2plog-msg SEPARATED BY space.
            APPEND ls_p2plog TO lt_p2plog.
          ENDIF.
        ENDLOOP.
        CLEAR :ot_log , wa_log.
        CLEAR :  it_pohdr , it_poitems,  lt_procure,  ls_procure.
      ENDIF.
*      CLEAR : ls_p2plog.

    ELSEIF ls_mat3-mtart = 'FERT'.  " Create PO fro components of these FERT type materials
      CLEAR : ls_p2plog.
      ls_p2plog-mtart = ls_mat3-mtart.
      CLEAR: lt_procure, ls_procure, it_pohdr , it_poitems.
      TYPES: BEGIN OF ty_distinct_mat,
               matnr TYPE matnr,
             END OF ty_distinct_mat.
      DATA: lt_distinct_fert_mat TYPE TABLE OF ty_distinct_mat, "ZDATAGEN_PROCURE,
            wa_distinct_fert_mat TYPE ty_distinct_mat, " ZDATAGEN_PROCURE,
            lv_hdrseq            TYPE i VALUE IS INITIAL.
      lv_hdrseq =  sy-tabix." lv_hdrseq + 1.
      SELECT * FROM zdatagen_procure INTO  TABLE lt_procure WHERE matnr = ls_mat3-material AND  mtart EQ ls_mat3-mtart.
      IF lt_procure[] IS NOT INITIAL.
        LOOP AT lt_procure INTO ls_procure.
          wa_poitems-pohdr_seqno = lv_hdrseq. "1.
          wa_poitems-material = ls_procure-component. "MATNR
          wa_poitems-plant = ls_procure-plant .
          wa_poitems-stge_loc = ls_procure-stge_loc.
          wa_poitems-quantity =  ls_procure-comp_qty_factor * ls_mat3-qty.
          wa_poitems-delivery_date = ls_mat3-record_date."sy-datum.
          wa_poitems-net_price = ls_procure-net_price .
          APPEND wa_poitems TO it_poitems.
          ls_p2plog-matnr = ls_mat3-material.
        ENDLOOP.
        wa_pohdr-pohdr_seqno = lv_hdrseq. "1. " check this.
        wa_pohdr-vendor = ls_procure-vendor.
        wa_pohdr-purch_org = ls_procure-purch_org.
        wa_pohdr-comp_code = ls_procure-comp_code.
        wa_pohdr-purch_grp = ls_procure-purch_grp.
        wa_pohdr-doc_type = ls_procure-doc_type.
        wa_pohdr-stge_loc = ls_procure-stge_loc.
        APPEND wa_pohdr TO it_pohdr.

        CALL FUNCTION 'ZCREATEPO_DATAGEN1_PA'
          IMPORTING
            msg        = po_msg
          TABLES
            it_pohdr   = it_pohdr
            it_poitems = it_poitems
            ot_log     = ot_log.
        LOOP AT ot_log INTO wa_log.
          IF wa_log-result_flag = 'S'.
*            WAIT UP TO 1 SECONDS.
*            WRITE: 'Success:', wa_log-ponumber, 'created for material', wa_log-material,'---',wa_log-msg.
*            NEW-LINE.
            ls_p2plog-flag = 'S'.
            ls_p2plog-objectid = wa_log-ponumber.
            ls_p2plog-msg = wa_log-msg.

            wa_gr-po_no = wa_log-ponumber. "PO_ITEM_NO PO_QUANTITY PO_UOM
            APPEND wa_gr TO it_gr.

            WAIT UP TO 3 SECONDS.
            lv_lock = 1.
            WHILE lv_lock IS NOT INITIAL.
              CALL FUNCTION 'ENQUEUE_EMEKKOE'
                EXPORTING
                  mode_ekko      = 'E'
                  mode_ekpo      = 'E'
                  mandt          = sy-mandt
                  ebeln          = wa_log-ponumber
*                 EBELP          =
                  x_ebeln        = ' '
                  x_ebelp        = ' '
                  _scope         = '2'
                  _wait          = ' '
                  _collect       = ' '
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.
              lv_lock = sy-subrc.
              IF lv_lock is INITIAL.
                CALL FUNCTION 'DEQUEUE_EMEKKOE'
                 EXPORTING
                   MODE_EKKO       = 'E'
                   MODE_EKPO       = 'E'
                   MANDT           = SY-MANDT
                   EBELN           = wa_log-ponumber
*                   EBELP           =
                   X_EBELN         = ' '
                   X_EBELP         = ' '
                   _SCOPE          = '3'
                   _SYNCHRON       = ' '
                   _COLLECT        = ' '
                          .
              ENDIF.
            ENDWHILE.

            CALL FUNCTION 'ZCREATEGR_DATAGEN1_PA'
              EXPORTING
                posting_date = ls_mat3-record_date
              IMPORTING
                msg          = gr_msg
                flag         = gr_flag
                ov_mat_doc   = mat_doc
                ov_mat_year  = mat_year
              TABLES
                po_gr        = it_gr.
            IF gr_flag EQ 'S'.
*              WRITE : gr_msg.
*              NEW-LINE.
              ls_p2plog-flag = 'S'.
*              ls_p2plog-objectid = wa_log-ponumber.
              CONCATENATE wa_log-ponumber mat_doc INTO ls_p2plog-objectid SEPARATED BY '-'.
              CLEAR ls_p2plog-msg.
              ls_p2plog-msg = gr_msg.
***                    create Invoice*************
              CALL FUNCTION 'ZCREATEINVOICE_FROM_PO_PA'
                EXPORTING
                  po_number    = wa_gr-po_no
                  posting_date = ls_mat3-record_date
                IMPORTING
                  flag         = inv_flag
                  msg          = inv_msg
                  invoice      = invoice
                  inv_year     = inv_year.

              IF inv_flag = 'S'.
*                WRITE : inv_msg.
*                NEW-LINE.
                ls_p2plog-flag = 'S'.
                CONCATENATE wa_log-ponumber mat_doc invoice INTO ls_p2plog-objectid SEPARATED BY '-'.
                CLEAR ls_p2plog-msg.
                ls_p2plog-msg = inv_msg.
                APPEND ls_p2plog TO lt_p2plog.
              ELSEIF inv_flag = 'E'.
                WRITE : inv_msg.
                NEW-LINE.
                ls_p2plog-flag = 'E'.
                CLEAR ls_p2plog-msg.
                ls_p2plog-msg = inv_msg.
                APPEND ls_p2plog TO lt_p2plog.
              ENDIF.   "  ,End of Invoice.

            ELSE.
              WRITE: gr_msg.
              NEW-LINE.
              ls_p2plog-flag = 'E'.
              CLEAR ls_p2plog-msg.
              ls_p2plog-msg = gr_msg.
              APPEND ls_p2plog TO lt_p2plog.
            ENDIF.
            CLEAR it_gr.
          ELSEIF wa_log-result_flag = 'E'.
            WRITE: 'Failed:', 'PO not created' ,'for material', wa_log-material,'---',wa_log-msg.
            NEW-LINE.
            ls_p2plog-flag = 'E'.
            CONCATENATE 'Failed:' wa_log-msg INTO ls_p2plog-msg SEPARATED BY space.
            APPEND ls_p2plog TO lt_p2plog.
          ENDIF.
        ENDLOOP.
        CLEAR : ot_log , wa_log.
        CLEAR :  lt_procure,  ls_procure.
        CLEAR :  it_pohdr , it_poitems.
*            CLEAR : ls_p2plog.
      ENDIF.
    ENDIF.
    CLEAR ls_mat3.
  ENDLOOP.
ENDIF.

*********** Print Total Processing time and End Time*******
endtime = sy-uzeit.
NEW-LINE.
WRITE : 'END TIME' , endtime ."sy-uzeit.
NEW-LINE.
DATA : totaltime TYPE sy-uzeit.
totaltime = endtime - starttime .
NEW-LINE.
WRITE : 'Total Time',totaltime.
***************************************End of time print******************
********************************Send Mail********************************
IF lt_p2plog[] IS NOT INITIAL.
  NEW-LINE.
  NEW-LINE.
  NEW-LINE.
  DATA: temp_p2plog TYPE TABLE OF ty_p2plog,  " temp table if any processing needed.
        wa_p2plog   TYPE ty_p2plog.

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
         c_date1      TYPE c LENGTH 10,
         c_date2      TYPE c LENGTH 10.
  WRITE  lv_date1 TO c_date1 .
  WRITE  lv_date2 TO c_date2 .

*  wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
*  wa_receivers-rec_type = 'U'.
*  wa_receivers-com_type = 'INT'.
*  APPEND wa_receivers TO it_receivers.
*  CLEAR: wa_receivers.

  wa_receivers-receiver = sy-uname.                         "'I065658'.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'.  '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

*********Adding recivers list on exceptional/Error cases.
*  DATA: maillist_p2plog TYPE ty_p2plog.
*  READ TABLE lt_p2plog INTO maillist_p2plog WITH KEY flag = 'E'.
*  IF sy-subrc IS INITIAL.
*******        Adding Somendra for exception mail list.
*    wa_receivers-receiver = 'somendra.sahu@sap.com'.
*    wa_receivers-rec_type = 'U'.
*    wa_receivers-com_type = 'INT'.
*    APPEND wa_receivers TO it_receivers.
*    CLEAR: wa_receivers.
*  ENDIF.
*
******** End of Exception receiverslist***

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'Procure-to-Pay Cycle for ' c_date1 'to' c_date2  INTO wa_header-obj_descr SEPARATED BY space.
  WRITE: wa_header-obj_descr.

  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This automatic email is to notify that' 'Procure-to-Pay cycle for records from' c_date1 'to' c_date2 'has been completed.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  WRITE: wa_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

*  DATA : successful_count      TYPE i,
*         successful_count_char TYPE c LENGTH 10,
  DATA:      tot_rec_char          TYPE c LENGTH 10.
*         error_count           TYPE i,
*         error_count_char      TYPE c LENGTH 10.

  temp_p2plog[] =  lt_p2plog[].
*********** Total records to process******************
  DATA : gv_total_records TYPE i.
  DESCRIBE TABLE temp_p2plog LINES gv_total_records.
  DESCRIBE TABLE lt_mat3 LINES gv_total_records.
  WRITE gv_total_records TO tot_rec_char.

  READ TABLE temp_p2plog INTO wa_p2plog WITH KEY flag = 'E'.
  IF sy-subrc IS NOT INITIAL. "Everything executed Successfully
    CONCATENATE 'Hurray!!' 'Procure-to-Pay Cycle executed successfully for'  tot_rec_char 'Materials/records ' INTO wa_content SEPARATED BY space.
    WRITE: wa_content.
    APPEND wa_content TO it_content.
  ELSE.
    LOOP AT temp_p2plog INTO wa_p2plog.
      IF wa_p2plog-flag = 'E'.
        CONCATENATE 'P2P cycle FAILED for material ' ls_p2plog-matnr ls_p2plog-objectid  ls_p2plog-msg INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
        WRITE: wa_content.
      ELSEIF wa_p2plog-flag = 'S'.
        CONCATENATE 'P2P cycle COMPLETED for material -' ls_p2plog-matnr ls_p2plog-objectid  ls_p2plog-msg INTO wa_content SEPARATED BY space.
        WRITE: wa_content.
        APPEND wa_content TO it_content.
      ENDIF.
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

*  sy-uname  = 'BPINST'."'WF-BATCH' .

IF it_receivers[] is not INITIAL.
CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
  EXPORTING
    document_data  = wa_header
    document_type  = 'HTM'
    commit_work    = 'X'
  TABLES
    object_content = it_content
    object_para    = it_para
    receivers      = it_receivers.
ENDIF.

ENDIF.  " end of lt_p2plog[] initial check
***************End of mail notification************************
