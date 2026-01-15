*&---------------------------------------------------------------------*
*& Report ZRUN_DELIVERY_BILLING_FOR_SO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrun_delivery_billing_for_so.

TABLES : vbak.
*PARAMETERS: p_vbeln TYPE vbak-vbeln .

*PARAMETERS : fromdate TYPE dats,
*             todate   TYPE dats,
*             createby TYPE sy-uname.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_so FOR vbak-vbeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
PARAMETERS : ordtype TYPE auart DEFAULT 'ZCM'.
SELECTION-SCREEN END OF BLOCK block.


DATA: lt_vbak TYPE TABLE OF vbak,
      ls_vbak TYPE vbak.
DATA : lv_sonum TYPE vbeln.
DATA:flag            TYPE  sy-subrc,
     deliverydoc_msg TYPE  string,
     deliveryno      TYPE  vbeln.

*PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum,
*             lv_date2 TYPE dats DEFAULT sy-datum.

* internal structure to load remote data
*DATA: ls_sales_order TYPE zdemo_data ,
*      lt_sales_order TYPE TABLE OF  zdemo_data ,
*      lt_dbupdate_so TYPE TABLE OF zdemo_data ,
*      ls_dbupdate_so TYPE zdemo_data.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime .
NEW-LINE.


*DATA: lv_timestamp      TYPE timestamp,
*      lv_timestamp_end  TYPE timestamp,
*      lv_tzntimestp     TYPE tzntimestp,
*      lv_tzntimestp_end TYPE tzntimestp.
*CONVERT DATE lv_date1 TIME '000000'
*                 INTO TIME STAMP lv_timestamp
*                 TIME ZONE 'UTC'.
*WRITE:lv_timestamp TO lv_tzntimestp.
*CONVERT DATE lv_date2 TIME '235959'
*                INTO TIME STAMP lv_timestamp_end
*                TIME ZONE 'UTC'.
*WRITE:lv_timestamp_end TO lv_tzntimestp_end.

********** Total records to process******************
DATA : gv_total_records TYPE i.

* HERE we have now all the data we need in the temporary stab table, now we need to loop through the line items and call the necessary programs
* here as a example i just display the data (2 fields)
DATA: it_sohdr        TYPE  zso_header_tt,
      ls_sohdr        LIKE LINE OF it_sohdr,
      it_soitems      TYPE  zso_items_tt,
      ls_soitems      LIKE LINE OF it_soitems,
      it_temp_soitems TYPE zso_items_tt,
      temp_soitems    LIKE LINE OF it_temp_soitems,
      it_temp_sohdr   TYPE  zso_header_tt,
      temp_sohdr      LIKE LINE OF it_temp_sohdr,
      ot_log          TYPE TABLE OF  zsocreate_log,
      ls_log          TYPE   zsocreate_log,
      lt_mat          TYPE   zso_items_tt_copy,
      ls_mat          LIKE LINE OF lt_mat,
      lt_mat2         TYPE   zso_items_tt_copy,
      ls_mat2         LIKE LINE OF lt_mat2,
      lt_mat3         TYPE   zso_items_tt_copy, " WITH HEADER LINE,
      ls_mat3         LIKE LINE OF lt_mat3,
      ot_log_temp     TYPE TABLE OF  zsocreate_log,
      gt_solog        TYPE TABLE OF  zsocreate_log,
      ot_dblog        TYPE TABLE OF zdatagen_logging,
      ls_dblog        TYPE zdatagen_logging,
      msg             TYPE char255.

DATA : lv_delvdoc TYPE vbeln,
       lv_billdoc TYPE vbeln.

*DATA : itab_error_records LIKE lt_sales_order,
*       lt_error_dbstruc   TYPE TABLE OF Zdemo_data.
DATA :  ret_falg  TYPE char1.


DATA : lv_date TYPE tzntimestp .

*IF p_vbeln IS NOT INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vbeln = p_vbeln.
*ELSEIF fromdate IS NOT INITIAL AND todate IS NOT INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat BETWEEN fromdate AND todate.
*ELSEIF fromdate IS NOT INITIAL AND todate IS INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat EQ fromdate ."and todate.
*ENDIF.

IF s_so IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE vbeln IN s_so.
ENDIF.

IF lt_vbak[] IS INITIAL.
  MESSAGE 'No records found' TYPE 'I'.
  RETURN.
ELSE.
  LOOP AT lt_vbak INTO ls_vbak.
    CLEAR: lv_billdoc, lv_delvdoc.
    lv_sonum = ls_vbak-vbeln .
*    *      CALL FUNCTION 'ZCREATEDELIVERYDOC_DATAGEN'
*    check if delv doc exists.

* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE vbeln INTO lv_delvdoc FROM vbfa WHERE vbelv = lv_sonum AND vbtyp_n = 'J'.
    SELECT vbeln FROM vbfa INTO TABLE @DATA(lt_x002) UP TO 1 ROWS WHERE vbelv = lv_sonum AND vbtyp_n = 'J' ORDER BY vbeln.
    IF sy-subrc eq 0.
      READ TABLE lt_x002 INTO DATA(ls_x002) INDEX 1.
      lv_delvdoc = ls_x002-vbeln.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
    IF  sy-subrc IS NOT INITIAL .
      CALL FUNCTION 'ZDELIVERY_PGI_DATAGEN'
        EXPORTING
          so_num         = lv_sonum
          actual_gidate  = ls_vbak-audat "ls_log-date_posted
        IMPORTING
          flag           = flag
          msg            = deliverydoc_msg
          deliverynumber = deliveryno.
      IF flag = 2 AND deliveryno IS INITIAL.
        NEW-LINE.
        WRITE : 'Delivery Doc  creation failed'.
        WRITE : deliverydoc_msg.
        NEW-LINE.
        CONCATENATE ls_dblog-msg deliverydoc_msg INTO ls_dblog-msg SEPARATED BY space.
*        ls_dbupdate_so-status = 'E'.
*        ls_dbupdate_so-objectid = lv_sonum.
*        ls_dbupdate_so-docstatus = '1'.
*        CONCATENATE lv_sonum  '-' INTO ls_dbupdate_so-objectid." SEPARATED BY '-'.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.

        MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.
*        CLEAR : ls_dblog,ls_dbupdate_so.
        CONTINUE. "EXIT.

      ELSEIF deliveryno IS NOT INITIAL. " delivery doc created with PGI
        NEW-LINE.
        ls_dblog-external_document = deliveryno.
*        WRITE :  deliverydoc_msg .
*        WRITE : 'Delivery Doc', deliveryno, 'Created successfully'.
        DATA :billingdocument TYPE  vbeln,
              billingdoc_msg  TYPE  string,
              messtab         TYPE TABLE OF bdcmsgcoll,
              ls_messtab      TYPE  bdcmsgcoll.
        WAIT UP TO 1 SECONDS.
        CALL FUNCTION 'ZCREATEBILLINGDOC_DATAGEN'
          EXPORTING
            deliverydoc     = deliveryno
          IMPORTING
            billingdocument = billingdocument
            msg             = billingdoc_msg
          TABLES
            messtab         = messtab.
        READ TABLE messtab INTO ls_messtab WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL OR billingdocument IS INITIAL.
*          ls_dbupdate_so-status = 'E'.
*          ls_dbupdate_so-docstatus = '2'.
*          CONCATENATE lv_sonum deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          WRITE : 'Delivery doc:', deliveryno, 'created.'.
          NEW-LINE.
          WRITE : 'Billing Doc:', billingdoc_msg.
*          CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ELSEIF billingdocument IS NOT INITIAL.
*          ls_dbupdate_so-status = 'S'.
*          ls_dbupdate_so-docstatus = '3'.
*          CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
          CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          NEW-LINE.
*          WRITE: ' Billing Document ', billingdocument, ' created successfully'.
          WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
        ENDIF.
      ENDIF.
      NEW-LINE.
    ELSE.
**      check if bill done.
* Start-Antigravity made this changes-(15.01.2026)
*       SELECT SINGLE vbeln INTO lv_billdoc FROM vbfa WHERE vbelv = lv_sonum AND vbtyp_n = 'M'.
      SELECT vbeln FROM vbfa INTO TABLE @DATA(lt_x001) UP TO 1 ROWS WHERE vbelv = lv_sonum AND vbtyp_n = 'M' ORDER BY vbeln.
      IF sy-subrc eq 0.
        READ TABLE lt_x001 INTO DATA(ls_x001) INDEX 1.
        lv_billdoc = ls_x001-vbeln.
      ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
      IF sy-subrc IS NOT INITIAL.
        CALL FUNCTION 'ZCREATEBILLINGDOC_DATAGEN'
          EXPORTING
            deliverydoc     = lv_delvdoc
          IMPORTING
            billingdocument = billingdocument
            msg             = billingdoc_msg
          TABLES
            messtab         = messtab.
        READ TABLE messtab INTO ls_messtab WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL OR billingdocument IS INITIAL.
*          ls_dbupdate_so-status = 'E'.
*          ls_dbupdate_so-docstatus = '2'.
*          CONCATENATE lv_sonum deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          WRITE : 'Delivery doc:', lv_delvdoc, 'created.'.
          NEW-LINE.
          WRITE : 'Billing Doc:', billingdoc_msg.
*          CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ELSEIF billingdocument IS NOT INITIAL.
*          ls_dbupdate_so-status = 'S'.
*          ls_dbupdate_so-docstatus = '3'.
*          CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
          CONCATENATE lv_delvdoc billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          NEW-LINE.
*          WRITE: ' Billing Document ', billingdocument, ' created successfully'.
          WRITE: 'SO-',lv_sonum,'DeiveryDoc-',lv_delvdoc,' Billing Document ', billingdocument, ' created successfully'.
        ENDIF.
       ELSE.
         WRITE: 'SO-',lv_sonum,'DeiveryDoc-',lv_delvdoc,' Billing Document ', lv_billdoc, ' created successfully'.
         NEW-LINE.
      ENDIF.
    ENDIF.
    NEW-LINE.
  ENDLOOP.
ENDIF.
