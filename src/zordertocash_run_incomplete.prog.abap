*&---------------------------------------------------------------------*
*& Report ZTEST_DIFFMATERIAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zordertocash_run_incomplete.


PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum OBLIGATORY,
             lv_date2 TYPE dats DEFAULT sy-datum OBLIGATORY,
             no_recs  TYPE i DEFAULT ''. "10.

* internal structure to load remote data
DATA: ls_sales_order TYPE  zdemo_data ,"
      lt_sales_order TYPE TABLE OF zdemo_data ,
      lt_dbupdate_so TYPE TABLE OF zdemo_data ,
      ls_dbupdate_so TYPE zdemo_data .
DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime ."sy-uzeit.
NEW-LINE.


DATA: lv_timestamp      TYPE timestamp,
      lv_timestamp_end  TYPE timestamp,
      lv_tzntimestp     TYPE tzntimestp, " timestamp.
      lv_tzntimestp_end TYPE tzntimestp.

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
DESCRIBE TABLE lt_sales_order LINES gv_total_records.

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

DATA : itab_error_records LIKE lt_sales_order,
       lt_error_dbstruc   TYPE TABLE OF zdatagen_dbstructure.
DATA :  ret_falg  TYPE char1.

DATA : LV_DATE TYPE TZNTIMESTP .
IF LV_DATE1 IS NOT INITIAL.
  CONCATENATE lv_date1 '000000' into lv_date.
else.
  CONCATENATE sy-datum '000000' into lv_date.
ENDIF.
select * from ZDEMO_DATA into table lt_sales_order where DATEPOSTED BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .

*****temp Code***
**select * from ZDEMO_DATA into table lt_sales_order.
*DELETE lt_sales_order WHERE MATERIAL NE 'MZ-TG-Y120' AND MATERIAL NE 'MZ-TG-Y200'  AND MATERIAL NE 'MZ-TG-Y240'.
**DELETE lt_sales_order WHERE MATERIAL NE 'MZ-TG-Y120' OR MATERIAL NE 'MZ-TG-Y200'  OR MATERIAL NE 'MZ-TG-Y240'.
**DELETE lt_sales_order WHERE MATERIAL NE 'MZ-FG-C950'.
*DELETE lt_sales_order WHERE MATERIAL EQ 'MZ-FG-C950' OR MATERIAL EQ 'MZ-FG-M525'.
*****temp Code***
*data : temp_it like lt_sales_order.
*temp_it[] = lt_sales_order[].
*sort temp_it by MATERIAL Quantity.
*clear lt_sales_order[].
*lt_sales_order[] = temp_it[].
**** End of temp code

itab_error_records[] = lt_sales_order[].
DELETE itab_error_records WHERE status NE 'E'." OR STATUS = 'E'.

MOVE-CORRESPONDING itab_error_records TO lt_error_dbstruc.
IF lt_error_dbstruc[] IS NOT INITIAL .
  CALL FUNCTION 'ZPROCESS_INCOMPLETE_SOCYCLE'
    IMPORTING
      ret_flag       = ret_falg
    TABLES
      lt_dbupdate_so = lt_error_dbstruc.
  MOVE-CORRESPONDING lt_error_dbstruc TO lt_dbupdate_so.
ENDIF.

DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E' OR status EQ 'P'. "P for predictive Accounting.
IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
  WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
  EXIT.
ENDIF.

*LOOP AT lt_sales_order INTO ls_sales_order ."where DATEPOSTED_STR BETWEEN '20150531' AND  '20150630' .
******* Add materials and items.
*  ls_mat-sohdr_seqno = sy-tabix.
*  ls_mat-material = ls_sales_order-material.
*  ls_mat-qty = ls_sales_order-quantity. "ls_mat-qty + ls_sales_order-quantity.
*  ls_mat-collect_no = ls_sales_order-collect_no.  " Collect for identiying individual rows
*  APPEND ls_mat TO lt_mat.
***********End of add items n materials.
*  CLEAR:  ls_sohdr, ls_soitems, ls_mat.
*ENDLOOP.


**************<<<<<<<<<<<<<<Write material and qty*************
*IF lt_mat[] IS NOT INITIAL.
*  lt_mat2[] = lt_mat[].
*  LOOP AT lt_mat INTO ls_mat.
**  write: /
**      LS_MAT-SOHDR_SEQNO ,
**  LS_MAT-MATERIAL,
**  LS_MAT-QTY.
*    LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material.
*      ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
*      ls_mat3-material = ls_mat2-material.
*      ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
*      ls_mat3-collect_no = ls_mat2-collect_no." will always fill the last records Collect_no.
*      CLEAR ls_mat2.
*    ENDLOOP.
*    APPEND ls_mat3 TO lt_mat3.
*    CLEAR ls_mat3.
*    DELETE lt_mat WHERE material = ls_mat-material." and SOHDR_SEQNO = ls_mat-SOHDR_SEQNO.
*    CLEAR ls_mat.
*  ENDLOOP.
*ENDIF.

TYPES : BEGIN OF ty_material_stock,
          sohdr_seqno       TYPE  int4,
          material          TYPE  matnr,
          qty               TYPE  dzmeng,
          unrestricted_stck TYPE  labst,
          total_stck        TYPE  sum01,
          stck_available    TYPE char1,
        END OF ty_material_stock.
DATA: ls_material_stock TYPE ty_material_stock,
      lt_material_stock TYPE TABLE OF ty_material_stock, " WITH HEADER LINE ,
      stck_flag         TYPE  char1,
      stck_msg          TYPE  char200,
      unrestricted_stck TYPE  labst,
      total_stck        TYPE  sum01.
*
*
*NEW-LINE.
*NEW-LINE.
*NEW-LINE.
*IF lt_mat3[] IS NOT INITIAL.
*  MOVE-CORRESPONDING lt_mat3 TO lt_material_stock.
*  WRITE: / 'SlNo','Material','Req. Quantity','unrestricted_stck','total_stck','Stock Available'.
*  LOOP AT lt_material_stock INTO ls_material_stock.
*    ls_material_stock-sohdr_seqno = sy-tabix.
*    CALL FUNCTION 'ZCHECK_MATERIAL_STOCK'
*      EXPORTING
*        material_long     = ls_material_stock-material
*        plant             = '1010'
*      IMPORTING
*        flag              = stck_flag
*        msg               = stck_msg
*        unrestricted_stck = unrestricted_stck
*        total_stck        = total_stck.
*
*    IF stck_flag = 'S'.
*      ls_material_stock-unrestricted_stck       = unrestricted_stck.
*      ls_material_stock-total_stck              = total_stck.
*      IF unrestricted_stck GE ls_material_stock-qty ."LE UNRESTRICTED_STCK. Stock available for processing
*        ls_material_stock-stck_available = 'Y'.
*      ELSE.
*        ls_material_stock-stck_available = 'N'.
*      ENDIF.
*      MODIFY lt_material_stock FROM ls_material_stock TRANSPORTING sohdr_seqno unrestricted_stck total_stck stck_available.
*    ENDIF.
*    WRITE: /
*          ls_material_stock-sohdr_seqno ,
*      ls_material_stock-material,
*      ls_material_stock-qty,
*      ls_material_stock-unrestricted_stck ,
*      ls_material_stock-total_stck ,
*      ls_material_stock-stck_available.
*    CLEAR: ls_material_stock,unrestricted_stck, total_stck.
*  ENDLOOP.
*ENDIF.
*NEW-LINE.
**************** Delete of Entries from lt_sales_order**************
DATA: gt_mail_stck_issues TYPE TABLE OF ty_material_stock,
      ls_mail_stck_issues TYPE  ty_material_stock.
*
*LOOP AT lt_material_stock INTO ls_mail_stck_issues WHERE stck_available = 'N'.
*  DELETE lt_sales_order WHERE material = ls_mail_stck_issues-material.
*  DELETE lt_mat3 WHERE material = ls_mail_stck_issues-material.
*  APPEND ls_mail_stck_issues TO gt_mail_stck_issues.
*ENDLOOP.
********************************************************************
*
***************** Execute each distinct Material records Once*************
*DATA : lt_dbstruc          TYPE TABLE OF zdatagen_dbstructure,
*       wa_dbstuc           TYPE  zdatagen_dbstructure,
*       temp_lt_sales_order TYPE TABLE OF  zdemo_data ,"  salesorder_type,
*       temp_lt_dbupdate_so TYPE TABLE OF zdatagen_dbstructure,
*       temp_ls_dbupdate_so TYPE zdatagen_dbstructure,
*       lv_flag             TYPE char1,
*       wa_mat3             LIKE LINE OF lt_mat3,
*       wa_salesorder       LIKE LINE OF lt_sales_order.
DATA : gt_dbupdate_so TYPE TABLE OF zdemo_data ."  salesorder_type.
*
*
**IF lt_mat3[] IS NOT INITIAL.  "" for always sending the distinct material records -1 each
**  LOOP AT lt_mat3 INTO wa_mat3.
**    READ TABLE lt_sales_order INTO wa_salesorder WITH KEY collect_no = wa_mat3-collect_no.
**    MOVE-CORRESPONDING wa_salesorder TO wa_dbstuc.
**    APPEND wa_dbstuc TO lt_dbstruc.
**    DELETE lt_sales_order WHERE collect_no = wa_mat3-collect_no.
**    CLEAR : wa_dbstuc,wa_salesorder.
**  ENDLOOP.
**ELSE.
**  MOVE-CORRESPONDING lt_sales_order TO  lt_dbstruc.
**ENDIF.
**
**CALL FUNCTION 'ZCHECK_SOCYCLE_DISTINCT_MAT'
**  IMPORTING
**    ret_flag        = lv_flag
**  TABLES
**    it_distinct_mat = lt_mat3
**    it_records      = lt_dbstruc
**    lt_dbupdate_so  = temp_lt_dbupdate_so.
**
**IF lv_flag = 'E'.
**  WRITE: 'Something went wrong, please check input records and the material stocks'.
**  EXIT.
**ELSE.
**  LOOP AT temp_lt_dbupdate_so INTO temp_ls_dbupdate_so." WHERE STATUS = 'E'.
**    IF temp_ls_dbupdate_so-status = 'E'.
**      DELETE lt_sales_order WHERE material = temp_ls_dbupdate_so-material.
**      MOVE-CORRESPONDING temp_ls_dbupdate_so TO ls_dbupdate_so.
**      APPEND ls_dbupdate_so TO gt_dbupdate_so.
**      APPEND ls_dbupdate_so TO lt_dbupdate_so.
**    ELSEIF temp_ls_dbupdate_so-status = 'S'.
**      MOVE-CORRESPONDING temp_ls_dbupdate_so TO ls_dbupdate_so.
**      APPEND ls_dbupdate_so TO lt_dbupdate_so.
**      DELETE lt_sales_order WHERE collect_no = temp_ls_dbupdate_so-collect_no.
**    ENDIF.
**  ENDLOOP.
**  MOVE-CORRESPONDING lt_dbstruc TO temp_lt_sales_order.
**ENDIF.
*************************************************End of distinct material SO-billing cycle
*
**********************Process remaining records from LT_Sales_order
*LOOP AT lt_sales_order INTO ls_sales_order." WHERE dateposted BETWEEN '20150531' AND  '20150630' .
*  ls_sohdr-sohdr_seqno = sy-tabix.
*  ls_sohdr-date_posted = ls_sales_order-dateposted.
*  ls_sohdr-doc_type = ls_sales_order-doc_type.
*  ls_sohdr-collect_no = ls_sales_order-collect_no.
*  ls_sohdr-sales_org = ls_sales_order-sales_org.
*  ls_sohdr-distrib_channel = ls_sales_order-distr_chan.
*  IF ls_sales_order-division EQ 0.
*    ls_sohdr-division = '00'.
*  ENDIF.
**  ls_sohdr-division = ls_sales_order-division.
*  ls_sohdr-soldto = ls_sales_order-sold_to.
*  APPEND ls_sohdr TO it_sohdr.
*
*  ls_soitems-sohdr_seqno = ls_sohdr-sohdr_seqno. "sy-tabix.
*  ls_soitems-material = ls_sales_order-material.
*  ls_soitems-qty = ls_sales_order-quantity.
*  APPEND ls_soitems TO it_soitems.
*
*  CLEAR:  ls_sohdr, ls_soitems.
*
*  IF no_recs IS NOT INITIAL.
*    IF sy-tabix = no_recs. "5.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*ENDLOOP.
*
*IF it_sohdr[] IS NOT INITIAL AND it_soitems IS NOT INITIAL.
*  CALL FUNCTION 'ZCREATESO_DATAGEN1'
*    IMPORTING
*      msg        = msg
*    TABLES
*      it_sohdr   = it_sohdr
*      it_soitems = it_soitems
*      ot_log     = ot_log
*      ot_dblog   = ot_dblog.
*
*  ot_log_temp[] = ot_log.
*
*******  capture records that failed to create SO
*
*  LOOP AT ot_log_temp INTO ls_log WHERE result_flag = 'E'.
*    APPEND ls_log TO gt_solog.
*    CLEAR ls_log.
*  ENDLOOP.
*  DELETE ot_log_temp WHERE result_flag = 'E'.

*****  ****End of So fail capture*****

*  DATA : lv_sonum TYPE vbeln.
*  DATA:flag            TYPE  sy-subrc,
*       deliverydoc_msg TYPE  string,
*       deliveryno      TYPE  vbeln.
*  IF ot_log_temp[] IS NOT INITIAL. " Some SO's created successfully
*    LOOP AT ot_log_temp INTO ls_log.
*      lv_sonum = ls_log-sonumber.
*      READ TABLE ot_dblog INTO ls_dblog WITH KEY documentno = lv_sonum.
*      READ TABLE lt_sales_order INTO ls_dbupdate_so WITH KEY collect_no = ls_dblog-collect_no.
*      ls_dbupdate_so-lastchangedate = sy-datum.
*      ls_dbupdate_so-lastchangeuser = sy-uname.
*
*      WRITE : 'Record',sy-tabix.
*      WRITE : 'Sales order ' , lv_sonum ,'Created successfully'.
**      CALL FUNCTION 'ZCREATEDELIVERYDOC_DATAGEN'
*      CALL FUNCTION 'ZDELIVERY_PGI_DATAGEN'
*        EXPORTING
*          so_num         = lv_sonum
*          actual_gidate  = ls_log-date_posted
*        IMPORTING
*          flag           = flag
*          msg            = deliverydoc_msg
*          deliverynumber = deliveryno.
*
*      IF flag = 2 AND deliveryno IS INITIAL.
*        NEW-LINE.
*        WRITE : 'Delivery Doc  creation failed'.
*        WRITE : deliverydoc_msg.
*        NEW-LINE.
*        CONCATENATE ls_dblog-msg deliverydoc_msg INTO ls_dblog-msg SEPARATED BY space.
*        ls_dbupdate_so-status = 'E'.
*        ls_dbupdate_so-objectid = lv_sonum.
*        ls_dbupdate_so-docstatus = '1'.
*        CONCATENATE lv_sonum  '-' INTO ls_dbupdate_so-objectid." SEPARATED BY '-'.
**        APPEND ls_dbupdate_so TO lt_dbupdate_so.
*
*        MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
**        ls_dbupdate_so-lastchangedate = sy-datum.
**        ls_dbupdate_so-lastchangeuser = sy-uname.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.
*        CLEAR : ls_dblog,ls_dbupdate_so.
*        CONTINUE. "EXIT.
*
*      ELSEIF deliveryno IS NOT INITIAL. " delivery doc created with PGI
*        NEW-LINE.
*        ls_dblog-external_document = deliveryno.
**        WRITE :  deliverydoc_msg .
**        WRITE : 'Delivery Doc', deliveryno, 'Created successfully'.
*        DATA :billingdocument TYPE  vbeln,
*              billingdoc_msg  TYPE  string,
*              messtab         TYPE TABLE OF bdcmsgcoll,
*              ls_messtab      TYPE  bdcmsgcoll.
*        WAIT UP TO 1 SECONDS.
*        CALL FUNCTION 'ZCREATEBILLINGDOC_DATAGEN'
*          EXPORTING
*            deliverydoc     = deliveryno
*          IMPORTING
*            billingdocument = billingdocument
*            msg             = billingdoc_msg
*          TABLES
*            messtab         = messtab.
*        READ TABLE messtab INTO ls_messtab WITH KEY msgtyp = 'E'.
*        IF sy-subrc IS INITIAL OR billingdocument IS INITIAL.
*          ls_dbupdate_so-status = 'E'.
*          ls_dbupdate_so-docstatus = '2'.
*          CONCATENATE lv_sonum deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
*          WRITE : billingdoc_msg.
*          NEW-LINE.
*          CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
*        ELSEIF billingdocument IS NOT INITIAL.
*          ls_dbupdate_so-status = 'S'.
*          ls_dbupdate_so-docstatus = '3'.
*          CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
*          NEW-LINE.
**          WRITE: ' Billing Document ', billingdocument, ' created successfully'.
*          WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
*        ENDIF.
*      ENDIF.
*      MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
*
**      ls_dbupdate_so-lastchangedate = sy-datum.
**      ls_dbupdate_so-lastchangeuser = sy-uname.
*
**      APPEND ls_dbupdate_so TO lt_dbupdate_so.
*      CLEAR : ls_dblog,ls_dbupdate_so.
*      NEW-LINE.
*      NEW-LINE.
*      NEW-LINE.
*    ENDLOOP.
*  ENDIF.
***************approach 2 end
**  Udpate Abap system log table
*  IF ot_dblog[] IS NOT INITIAL.
*    MODIFY zdatagen_logging FROM TABLE ot_dblog.
*  ENDIF.
****  End of ABAP system log table
*ENDIF.
*****************************************End of remaining record processing of lt_sales_order*****************

***********Update db table as no HANADB exists******
IF lt_dbupdate_so[] IS NOT INITIAL.
  IF sy-subrc IS NOT INITIAL. " sy-subrc is not cleared after last check. thus it gives error of db connect msg and then not being able to go to db update code lines.
    CLEAR sy-subrc.
  ENDIF.
MODIFY ZDEMO_DATA from table lt_dbupdate_so.
ENDIF.

**************End of demodata table update************

*************** udpate HANA DB****************
*IF lt_dbupdate_so[] IS NOT INITIAL.
*  IF sy-subrc IS NOT INITIAL. " sy-subrc is not cleared after last check. thus it gives error of db connect msg and then not being able to go to db update code lines.
*    CLEAR sy-subrc.
*  ENDIF.
*  EXEC SQL.
*    CONNECT TO 'DATAREPO'
*  ENDEXEC.
*  IF sy-subrc <> 0.
*    MESSAGE 'Unable to connect to Data Repository DB. Please check the connection in DB02' TYPE 'E' DISPLAY LIKE 'I'.
*    RETURN.
*  ENDIF.
*
*  LOOP AT LT_DBUPDATE_SO into LS_DBUPDATE_SO.
*    EXEC SQL.
*      OPEN dbcur1 FOR
*         UPDATE "DATA_PRODUCTION"."SALESORDER_HE4_V1" SET "STATUS" = :LS_DBUPDATE_SO-STATUS,
*                                                      "LASTCHANGEDATE" = :LS_DBUPDATE_SO-LASTCHANGEDATE,
*                                                      "LASTCHANGEUSER" = :LS_DBUPDATE_SO-LASTCHANGEUSER,
*                                                      "OBJECTID" = :LS_DBUPDATE_SO-OBJECTID,
*                                                      "DOCSTATUS" = :LS_DBUPDATE_SO-DOCSTATUS
*                                                      WHERE "COLLECT_NO" = :LS_DBUPDATE_SO-COLLECT_NO
*    ENDEXEC.
** close remote connection
*    EXEC SQL.
*      CLOSE dbcur1
*    ENDEXEC.
*  ENDLOOP.
*
** Reset to "default connection"
*  EXEC SQL.
*    SET CONNECTION DEFAULT
*  ENDEXEC.
*ENDIF.
******END of HANADB update


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
      wa_dbupdate_so      TYPE zdemo_data ."  salesorder_type. " gt_dbupdate_so TYPE TABLE OF salesorder_type.

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

wa_receivers-receiver = sy-uname.                           "'I065658'.
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

****************End of mail notification************************
