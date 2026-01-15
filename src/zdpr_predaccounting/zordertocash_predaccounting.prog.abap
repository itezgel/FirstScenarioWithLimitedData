*&---------------------------------------------------------------------*
*& Report ZORDERTOCASH_PREDACCOUNTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zordertocash_predaccounting.

PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum OBLIGATORY,
             lv_date2 TYPE dats DEFAULT sy-datum OBLIGATORY.

* internal structure to load remote data
DATA: ls_sales_order TYPE zpredacct_data,
      lt_sales_order TYPE TABLE OF  zpredacct_data,
      lt_dbupdate_so TYPE TABLE OF zpredacct_data.
*      ls_sales_order TYPE zpredacct_data.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime .
NEW-LINE.

AT SELECTION-SCREEN.
  IF lv_date1 GT sy-datum.
    MESSAGE 'From date cannot be Future Date' TYPE 'E'.
  ELSEIF lv_date2 GT sy-datum.
    MESSAGE 'To date cannot be Future Date' TYPE 'E'.
  ELSEIF lv_date1 GT lv_date2.
    MESSAGE 'From date cannot be greater than To Date' TYPE 'E'.
    RETURN.
  ENDIF.


START-OF-SELECTION.

  DATA: lv_timestamp      TYPE timestamp,
        lv_timestamp_end  TYPE timestamp,
        lv_tzntimestp     TYPE tzntimestp,
        lv_tzntimestp_end TYPE tzntimestp.
  CONVERT DATE lv_date1 TIME '000000'
                   INTO TIME STAMP lv_timestamp
                   TIME ZONE 'UTC'.
  WRITE:lv_timestamp TO lv_tzntimestp.
  CONVERT DATE lv_date2 TIME '235959'
                  INTO TIME STAMP lv_timestamp_end
                  TIME ZONE 'UTC'.
  WRITE:lv_timestamp_end TO lv_tzntimestp_end.

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
        ot_dblog        TYPE TABLE OF zpa_logging,
        ls_dblog        TYPE zpa_logging,
        msg             TYPE char255.


  DATA : itab_error_records LIKE lt_sales_order,
         lt_error_dbstruc   TYPE TABLE OF zpredacct_data.
  DATA :  ret_falg  TYPE char1.
  DATA : lv_date TYPE tzntimestp .

  IF lv_date1 IS NOT INITIAL.
    CONCATENATE lv_date1 '000000' INTO lv_date.
  ELSE.
    CONCATENATE sy-datum '000000' INTO lv_date.
  ENDIF.
  SELECT * FROM zpredacct_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .
*  DELETE lt_sales_order WHERE material NE 'MZ-FG-R300'.  " delete this.
  SORT lt_sales_order BY material.
  DESCRIBE TABLE lt_sales_order LINES gv_total_records.

*  itab_error_records[] = lt_sales_order[].
*  DELETE itab_error_records WHERE status NE 'E'." OR STATUS = 'E'.
*  MOVE-CORRESPONDING itab_error_records TO lt_error_dbstruc.
*  IF lt_error_dbstruc[] IS NOT INITIAL .
*    CALL FUNCTION 'ZPROCESS_INCOMPLETE_SOCYCLE'
*      IMPORTING
*        ret_flag       = ret_falg
*      TABLES
*        lt_dbupdate_so = lt_error_dbstruc.
*    MOVE-CORRESPONDING lt_error_dbstruc TO lt_dbupdate_so.
*  ENDIF.

*  DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E' OR status EQ 'P'.  " as we only want to run the delivery and billing.
  DELETE lt_sales_order WHERE status NE 'F'. " Select only records of Future posted status.
*delete lt_sales_order where material CS 'MZ-TG-Y'.
  IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
    WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
    EXIT.
  ENDIF.

  LOOP AT lt_sales_order INTO ls_sales_order .
***** Add materials and items.
    ls_mat-sohdr_seqno = sy-tabix.
    ls_mat-material = ls_sales_order-material.
    ls_mat-qty = ls_sales_order-quantity.
    ls_mat-collect_no = ls_sales_order-collect_no.  " Collect for identiying individual rows
    APPEND ls_mat TO lt_mat.
**********End of add items n materials.
    CLEAR:  ls_sohdr, ls_soitems, ls_mat.
  ENDLOOP.

*************<<<<<<<<<<<<<<Write material and qty*************
  IF lt_mat[] IS NOT INITIAL.
    lt_mat2[] = lt_mat[].
    LOOP AT lt_mat INTO ls_mat.
      LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material.
        ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
        ls_mat3-material = ls_mat2-material.
        ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
        ls_mat3-collect_no = ls_mat2-collect_no." will always fill the last records Collect_no.
        CLEAR ls_mat2.
      ENDLOOP.
      APPEND ls_mat3 TO lt_mat3.
      CLEAR ls_mat3.
      DELETE lt_mat WHERE material = ls_mat-material.
      CLEAR ls_mat.
    ENDLOOP.
  ENDIF.

  TYPES : BEGIN OF ty_material_stock,
            sohdr_seqno       TYPE  int4,
            material          TYPE  matnr,
            qty               TYPE  dzmeng,
            unrestricted_stck TYPE  labst,
            total_stck        TYPE  sum01,
            stck_available    TYPE char1,
          END OF ty_material_stock.
  DATA: ls_material_stock TYPE ty_material_stock,
        lt_material_stock TYPE TABLE OF ty_material_stock,
        stck_flag         TYPE  char1,
        stck_msg          TYPE  char200,
        unrestricted_stck TYPE  labst,
        total_stck        TYPE  sum01.


  NEW-LINE.
  NEW-LINE.
  NEW-LINE.
  IF lt_mat3[] IS NOT INITIAL.
    MOVE-CORRESPONDING lt_mat3 TO lt_material_stock.
    WRITE: / 'SlNo','Material','Req. Quantity','unrestricted_stck','total_stck','Stock Available'.
    LOOP AT lt_material_stock INTO ls_material_stock.
      ls_material_stock-sohdr_seqno = sy-tabix.
      CALL FUNCTION 'ZCHECK_MATERIAL_STOCK'
        EXPORTING
          material_long     = ls_material_stock-material
          plant             = '1710'
        IMPORTING
          flag              = stck_flag
          msg               = stck_msg
          unrestricted_stck = unrestricted_stck
          total_stck        = total_stck.

      IF stck_flag = 'S'.
        ls_material_stock-unrestricted_stck       = unrestricted_stck.
        ls_material_stock-total_stck              = total_stck.
        IF unrestricted_stck GE ls_material_stock-qty ."LE UNRESTRICTED_STCK. Stock available for processing
          ls_material_stock-stck_available = 'Y'.
        ELSE.
          ls_material_stock-stck_available = 'N'.
        ENDIF.
        MODIFY lt_material_stock FROM ls_material_stock TRANSPORTING sohdr_seqno unrestricted_stck total_stck stck_available.
      ENDIF.
      WRITE: /
            ls_material_stock-sohdr_seqno ,
        ls_material_stock-material,
        ls_material_stock-qty,
        ls_material_stock-unrestricted_stck ,
        ls_material_stock-total_stck ,
        ls_material_stock-stck_available.
      CLEAR: ls_material_stock,unrestricted_stck, total_stck.
    ENDLOOP.
  ENDIF.
  NEW-LINE.
*************** Delete of Entries from lt_sales_order**************
  DATA: gt_mail_stck_issues TYPE TABLE OF ty_material_stock,
        ls_mail_stck_issues TYPE  ty_material_stock.

  LOOP AT lt_material_stock INTO ls_mail_stck_issues WHERE stck_available = 'N'.
    DELETE lt_sales_order WHERE material = ls_mail_stck_issues-material.
    DELETE lt_mat3 WHERE material = ls_mail_stck_issues-material.
    APPEND ls_mail_stck_issues TO gt_mail_stck_issues.
  ENDLOOP.
*******************************************************************

**************** Execute each distinct Material records Once*************
  DATA : lt_dbstruc          TYPE TABLE OF zpredacct_data, "zdatagen_dbstructure
         wa_dbstuc           TYPE  zpredacct_data,
         temp_lt_sales_order TYPE TABLE OF  zpredacct_data,
         temp_lt_dbupdate_so TYPE TABLE OF zpredacct_data,
         temp_ls_sales_order TYPE zpredacct_data,
         lv_flag             TYPE char1,
         wa_mat3             LIKE LINE OF lt_mat3,
         wa_salesorder       LIKE LINE OF lt_sales_order.
  DATA : gt_dbupdate_so TYPE TABLE OF zpredacct_data .


  IF lt_sales_order[] IS NOT INITIAL.
    LOOP AT lt_sales_order INTO ls_sales_order.
**** DO delv-pgi
      MOVE-CORRESPONDING ls_sales_order TO ls_dblog.
      ls_dblog-date_posted = ls_sales_order-dateposted_str.
      DATA : lv_sonum TYPE vbeln.
      DATA:flag            TYPE  sy-subrc,
           deliverydoc_msg TYPE  string,
           deliveryno      TYPE  vbeln.

      lv_sonum = ls_sales_order-vbeln.
      ls_dblog-transaction_type = 'DLVSO'.
      CALL FUNCTION 'ZDELIVERY_PGI_DATAGEN_PA'
        EXPORTING
          so_num         = lv_sonum
          actual_gidate  = ls_sales_order-dateposted_str "ls_log-date_posted
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
        ls_sales_order-status = 'E'.
        ls_sales_order-objectid = lv_sonum.
        ls_sales_order-docstatus = '1'.
        CONCATENATE lv_sonum  '-' INTO ls_sales_order-objectid." SEPARATED BY '-'.
*        APPEND ls_sales_order TO lt_dbupdate_so.
        ls_dblog-documentno = deliveryno.
        ls_dblog-external_document = lv_sonum.
        ls_dblog-created_on = sy-datum.
        MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
        APPEND ls_sales_order TO lt_dbupdate_so.
        CLEAR : ls_dblog,ls_sales_order.
        CONTINUE. "EXIT.

      ELSEIF deliveryno IS NOT INITIAL. " delivery doc created with PGI
*        ls_dblog-transaction_type = 'BILSO'.
        CONCATENATE ls_dblog-msg deliverydoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ls_sales_order-status = 'S'.
        ls_dblog-documentno = deliveryno.
        ls_dblog-external_document = lv_sonum.
        ls_dblog-created_on = sy-datum.
        MODIFY zpa_logging FROM ls_dblog. " End of DLVPGI logging to zpa_logging.
        NEW-LINE.
*        MODIFY zpa_logging FROM ls_dblog. " this is needed so that we have 2 diff log entries for DLVPGI and billing further.
*        WRITE :  deliverydoc_msg .
*        WRITE : 'Delivery Doc', deliveryno, 'Created successfully'.
        DATA :billingdocument TYPE  vbeln,
              billingdoc_msg  TYPE  string,
              messtab         TYPE TABLE OF bdcmsgcoll,
              ls_messtab      TYPE  bdcmsgcoll.
        WAIT UP TO 1 SECONDS.
        ls_dblog-transaction_type = 'BILSO'.
        ls_dblog-external_document = deliveryno. "lv_sonum.
        ls_dblog-created_on = sy-datum.
        CALL FUNCTION 'ZCREATEBILLINGDOC_DATAGEN_PA'
          EXPORTING
            deliverydoc     = deliveryno
          IMPORTING
            billingdocument = billingdocument
            msg             = billingdoc_msg
          TABLES
            messtab         = messtab.
        READ TABLE messtab INTO ls_messtab WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL OR billingdocument IS INITIAL.
          ls_dblog-documentno = billingdocument.
          ls_sales_order-status = 'E'.
          ls_sales_order-delvierydoc = deliveryno.
          ls_sales_order-docstatus = '2'.
          CONCATENATE lv_sonum deliveryno INTO ls_sales_order-objectid SEPARATED BY '-'.
          APPEND ls_sales_order TO lt_dbupdate_so.
          WRITE : 'Delivery doc:', deliveryno, 'created.'.
          NEW-LINE.
          WRITE : 'Billing Doc:', billingdoc_msg.
          ls_dblog-external_document = lv_sonum.
          ls_dblog-created_on = sy-datum.
          CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ELSEIF billingdocument IS NOT INITIAL.
          ls_sales_order-status = 'S'.
          ls_sales_order-docstatus = '3'.
          ls_sales_order-delvierydoc = deliveryno.
          ls_sales_order-billingdoc = billingdocument.
          CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_sales_order-objectid SEPARATED BY '-'.
          CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
          ls_dblog-documentno = billingdocument.
          APPEND ls_sales_order TO lt_dbupdate_so.
          NEW-LINE.
*          WRITE: ' Billing Document ', billingdocument, ' created successfully'.
          WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
        ENDIF.
      ENDIF.

***Do Billing
****check nif below needed***
*      MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
      MODIFY zpa_logging FROM ls_dblog.
      MODIFY zpredacct_data FROM ls_sales_order.
      CLEAR : ls_dblog,ls_sales_order.
      NEW-LINE.
      NEW-LINE.
      NEW-LINE.
    ENDLOOP.

**************approach 2 end
*  Udpate Abap system log table
    IF ot_dblog[] IS NOT INITIAL.
*    MODIFY zpa_logging FROM TABLE ot_dblog.
      LOOP AT ot_dblog INTO ls_dblog.
        INSERT zpa_logging FROM ls_dblog.
      ENDLOOP.
    ENDIF.
***  End of ABAP system log table
  ENDIF.
****************************************End of remaining record processing of lt_sales_order*****************
*	***********Update db table as no HANADB exists******
  IF lt_dbupdate_so[] IS NOT INITIAL.
    IF sy-subrc IS NOT INITIAL. " sy-subrc is not cleared after last check. thus it gives error of db connect msg and then not being able to go to db update code lines.
      CLEAR sy-subrc.
    ENDIF.
*MODIFY zpredacct_data from table lt_dbupdate_so.
    DATA : ls_demodata TYPE zpredacct_data.
    LOOP AT lt_dbupdate_so INTO ls_demodata.
*      UPDATE zpredacct_data SET status = ls_demodata-status
*                                          objectid = ls_demodata-objectid
*                                          docstatus = ls_demodata-docstatus WHERE collect_no = ls_demodata-collect_no.
      MODIFY zpredacct_data FROM ls_demodata.
      CLEAR ls_demodata.
    ENDLOOP.
  ENDIF.
**************End of demodata table update************


*********** Print Total Processing time and End Time*******
  endtime = sy-uzeit.
  NEW-LINE.
  WRITE : 'END TIME' , endtime ."sy-uzeit.
  NEW-LINE.
  DATA : totaltime TYPE sy-uzeit.
  totaltime = endtime - starttime .
  NEW-LINE.
  WRITE : 'Total Time',totaltime.
***************************************End of time print********************
**************Do the mail Sending for the status update***********
  DATA: wa_solog            TYPE zsocreate_log , "GT_SOLOG  TYPE TABLE OF  zsocreate_log,
        wa_mail_stck_issues TYPE ty_material_stock, "gt_mail_stck_issues TYPE TABLE OF ty_material_stock.
        wa_dbupdate_so      TYPE zpredacct_data . "  salesorder_type. " gt_dbupdate_so TYPE TABLE OF salesorder_type.

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
         c_date2      TYPE c LENGTH 10,
         c_endtime    TYPE c LENGTH 8.

  DATA : mailflag TYPE char1 VALUE ''. " flag for exception mail list
  .
  WRITE  lv_date1 TO c_date1 .
  WRITE  lv_date2 TO c_date2 .

  WRITE: endtime TO c_endtime.

*wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
*wa_receivers-rec_type = 'U'.
*wa_receivers-com_type = 'INT'.
*APPEND wa_receivers TO it_receivers.
*CLEAR: wa_receivers.
*

  wa_receivers-receiver = sy-uname.                         "'I065658'.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'.  '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.


  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'SO-Billing Cycle for' c_date1 'to' c_date2  INTO wa_header-obj_descr SEPARATED BY space.
  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This automatic email serves to notify that' 'SO-Billing cycle for the records from' c_date1 'to' c_date2 'has been completed.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  IF gt_solog[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Issues relating to SO Creation' '.' INTO wa_content SEPARATED BY space. "lv_link
    APPEND wa_content TO it_content.
    LOOP AT gt_solog INTO wa_solog.
      CONCATENATE 'SO Creation failed for PO reference' wa_solog-collect_no 'for reasons: ' wa_solog-msg INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  IF gt_mail_stck_issues[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Stock issues:' '.' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    DATA: temp_qty       TYPE char10,
          available_stck TYPE char10.
    LOOP AT gt_mail_stck_issues INTO wa_mail_stck_issues.
      WRITE wa_mail_stck_issues-qty  TO temp_qty.
      WRITE wa_mail_stck_issues-unrestricted_stck TO available_stck.
      CONCATENATE 'Records with material' wa_mail_stck_issues-material 'requires stock' temp_qty "wa_mail_stck_issues-qty
                  'and the available stock is' available_stck  INTO wa_content ." wa_mail_stck_issues-unrestricted_stck total_stck
      APPEND wa_content TO it_content.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  IF gt_dbupdate_so[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Below are the Icomplete/Erroneous execution' '.' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    DATA: temp_collect TYPE char40.
    LOOP AT gt_dbupdate_so INTO wa_dbupdate_so.
      WRITE wa_dbupdate_so-collect_no TO temp_collect.
      CONDENSE temp_collect.
      IF wa_dbupdate_so-docstatus EQ '1'.
        CONCATENATE 'Record with PO:' temp_collect 'with material' wa_dbupdate_so-material
        'failed to create Delivery document for SO' wa_dbupdate_so-objectid INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
      ELSEIF wa_dbupdate_so-docstatus EQ '2'.
        CONCATENATE 'Record with PO:' temp_collect 'with material' wa_dbupdate_so-material
        'failed to create Billing document for SO-DeliveryDoc' wa_dbupdate_so-objectid INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
      ENDIF.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  DATA : mail_lt_dbupdate_so   LIKE lt_dbupdate_so,
         successful_count      TYPE i,
         successful_count_char TYPE c LENGTH 10,
         tot_rec_char          TYPE c LENGTH 10,
         error_count           TYPE i,
         error_count_char      TYPE c LENGTH 10.

  mail_lt_dbupdate_so[] = lt_dbupdate_so[].
  DELETE mail_lt_dbupdate_so WHERE status NE 'S'.
  DESCRIBE TABLE mail_lt_dbupdate_so LINES successful_count .
  WRITE successful_count TO  successful_count_char.

  error_count = gv_total_records - successful_count.
  WRITE gv_total_records TO tot_rec_char.
  WRITE error_count TO error_count_char.

  IF gt_solog[] IS INITIAL AND gt_mail_stck_issues[] IS INITIAL AND gt_dbupdate_so[] IS INITIAL. "Everything executed Successfully
    CONCATENATE 'Hurray!!'  tot_rec_char 'records executed successfully' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
  ELSE.
    mailflag = 'X'.
    IF error_count IS NOT INITIAL AND error_count GT 0.
      CONCATENATE 'Only' successful_count_char 'executed successfully out of total' tot_rec_char 'that were to be processed' INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ELSE.
      CONCATENATE 'Report could not process all the' 'records due to above listed issues.'
                     'Please correct them and re-run the code ZCORPMODEL' INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ENDIF.
  ENDIF.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE 'Corporate Model execution End time' c_endtime INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.


  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND 'Regards,' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND 'Workflow System.' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

********Adding recivers list on exceptional/Error cases.
  IF mailflag EQ 'X'.
*******        Adding Somendra for exception mail list.
*  wa_receivers-receiver = 'somendra.sahu@sap.com'.
*  wa_receivers-rec_type = 'U'.
*  wa_receivers-com_type = 'INT'.
*  APPEND wa_receivers TO it_receivers.
*  CLEAR: wa_receivers.
  ENDIF.

******* End of Exception receiverslist***
*sy-uname  = 'I065658'."'WF-BATCH' .
  IF it_receivers[] IS NOT INITIAL.
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
****************End of mail notification************************
