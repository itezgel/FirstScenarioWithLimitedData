FUNCTION zprocess_incomplete_socycle.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(RET_FLAG) TYPE  CHAR1
*"  TABLES
*"      LT_DBUPDATE_SO STRUCTURE  ZDEMO_DATA OPTIONAL
*"----------------------------------------------------------------------

  DATA:
    temp_it_records TYPE  TABLE OF Zdemo_data,
*        ls_sales_order  TYPE Zdemo_data,
    ls_dbupdate_so  TYPE Zdemo_data,
    ot_log          TYPE TABLE OF  zsocreate_log,
    ls_log          TYPE   zsocreate_log,
    ot_log_temp     TYPE TABLE OF  zsocreate_log,
    ot_dblog        TYPE TABLE OF zdatagen_logging,
    ls_dblog        TYPE zdatagen_logging,
    msg             TYPE char255.

  DATA : lv_sonum TYPE vbeln.
  DATA:flag            TYPE  sy-subrc,
       deliverydoc_msg TYPE  string,
       deliveryno      TYPE  vbeln.

  IF lt_dbupdate_so[] IS INITIAL.
    ret_flag = 'E'.
    RETURN.
  ELSE.
    temp_it_records[] = lt_dbupdate_so[].
  ENDIF.

  LOOP AT lt_dbupdate_so INTO ls_dbupdate_so.
    ls_dbupdate_so-lastchangedate = sy-datum.
    ls_dbupdate_so-lastchangeuser = sy-uname.
    IF ls_dbupdate_so-docstatus = '1'.
      lv_sonum = ls_dbupdate_so-objectid.
      REPLACE all OCCURRENCES OF '-' in lv_sonum with ''.
      SELECT * FROM zdatagen_logging INTO TABLE ot_dblog WHERE documentno = lv_sonum.
      READ TABLE ot_dblog INTO ls_dblog WITH KEY documentno = lv_sonum.

      data : temp_gidate type WADAT_IST.
      write : ls_dbupdate_so-dateposted to temp_gidate.
      CALL FUNCTION 'ZDELIVERY_PGI_DATAGEN'
        EXPORTING
          so_num         = lv_sonum
          actual_gidate  = temp_gidate "ls_dbupdate_so-dateposted
        IMPORTING
          flag           = flag
          msg            = deliverydoc_msg
          deliverynumber = deliveryno.

      IF flag = 2 AND deliveryno IS INITIAL.
        NEW-LINE.
*        WRITE : 'Delivery Doc  creation failed'.
*        WRITE : deliverydoc_msg.
        CONCATENATE ls_dblog-msg deliverydoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ls_dbupdate_so-status = 'E'.
        ls_dbupdate_so-objectid = lv_sonum.
        ls_dbupdate_so-docstatus = '1'.
        CONCATENATE lv_sonum  '-' INTO ls_dbupdate_so-objectid." SEPARATED BY '-'.
        MODIFY lt_dbupdate_so FROM ls_dbupdate_so TRANSPORTING status objectid docstatus lastchangedate lastchangeuser WHERE collect_no = ls_dbupdate_so-collect_no.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.
*        Write : 'material:',LS_DBUPDATE_SO-MATERIAL,'Has Erros'.
        NEW-LINE.
        CONTINUE."EXIT.
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
        IF sy-subrc IS INITIAL.
          ls_dbupdate_so-status = 'E'.
          ls_dbupdate_so-docstatus = '2'.
          CONCATENATE lv_sonum  deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          MODIFY lt_dbupdate_so FROM ls_dbupdate_so TRANSPORTING status objectid docstatus lastchangedate lastchangeuser WHERE collect_no = ls_dbupdate_so-collect_no.
*          LS_DBUPDATE_SO-OBJECTID = lv_sonum.
*          WRITE : billingdoc_msg.
          CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
        ELSEIF billingdocument IS NOT INITIAL.
          ls_dbupdate_so-status = 'S'.
          ls_dbupdate_so-docstatus = '3'.
          CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          MODIFY lt_dbupdate_so FROM ls_dbupdate_so TRANSPORTING  status objectid docstatus lastchangedate lastchangeuser WHERE collect_no = ls_dbupdate_so-collect_no.
          CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
          NEW-LINE.
          WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
        ENDIF.
      ENDIF.
      MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
      CLEAR : ls_dblog,ls_dbupdate_so.



    ELSEIF ls_dbupdate_so-docstatus = '2'.
      SPLIT ls_dbupdate_so-objectid AT '-' INTO lv_sonum deliveryno.
      SELECT * FROM zdatagen_logging INTO TABLE ot_dblog WHERE documentno = lv_sonum.
      READ TABLE ot_dblog INTO ls_dblog WITH KEY documentno = lv_sonum.
      ls_dblog-external_document = deliveryno.
*        WRITE :  deliverydoc_msg .
      CALL FUNCTION 'ZCREATEBILLINGDOC_DATAGEN'
        EXPORTING
          deliverydoc     = deliveryno
        IMPORTING
          billingdocument = billingdocument
          msg             = billingdoc_msg
        TABLES
          messtab         = messtab.
      READ TABLE messtab INTO ls_messtab WITH KEY msgtyp = 'E'.
      IF sy-subrc IS INITIAL.
        ls_dbupdate_so-status = 'E'.
        ls_dbupdate_so-docstatus = '2'.
        CONCATENATE lv_sonum  deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.
        MODIFY lt_dbupdate_so FROM ls_dbupdate_so TRANSPORTING  status objectid docstatus lastchangedate lastchangeuser WHERE collect_no = ls_dbupdate_so-collect_no.
*          LS_DBUPDATE_SO-OBJECTID = lv_sonum.
*          WRITE : billingdoc_msg.
        CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
      ELSEIF billingdocument IS NOT INITIAL.
        ls_dbupdate_so-status = 'S'.
        ls_dbupdate_so-docstatus = '3'.
        CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
*        APPEND ls_dbupdate_so TO lt_dbupdate_so.
        MODIFY lt_dbupdate_so FROM ls_dbupdate_so TRANSPORTING  status objectid docstatus lastchangedate lastchangeuser WHERE collect_no = ls_dbupdate_so-collect_no.
        CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
        NEW-LINE.
        WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
      ENDIF.
*          ENDIF.
      MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.

*      APPEND ls_dbupdate_so TO lt_dbupdate_so.
      CLEAR : ls_dblog,ls_dbupdate_so.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
