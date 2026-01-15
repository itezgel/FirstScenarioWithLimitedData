FUNCTION zcheck_socycle_distinct_mat.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(RET_FLAG) TYPE  CHAR1
*"  TABLES
*"      IT_DISTINCT_MAT TYPE  ZSO_ITEMS_TT_COPY
*"      IT_RECORDS STRUCTURE  ZDATAGEN_DBSTRUCTURE
*"      LT_DBUPDATE_SO STRUCTURE  ZDATAGEN_DBSTRUCTURE OPTIONAL
*"----------------------------------------------------------------------

  DATA: temp_it_records TYPE  TABLE OF zdatagen_dbstructure,
        ls_sales_order  TYPE zdatagen_dbstructure,
        ls_dbupdate_so  TYPE zdatagen_dbstructure,
        it_sohdr        TYPE  zso_header_tt,
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
        lt_mat3         TYPE   zso_items_tt_copy,
        ls_mat3         LIKE LINE OF lt_mat3,
        ot_log_temp     TYPE TABLE OF  zsocreate_log,
        ot_dblog        TYPE TABLE OF zdatagen_logging,
        ls_dblog        TYPE zdatagen_logging,
        msg             TYPE char255.
  IF it_records[] IS INITIAL.
    ret_flag = 'E'.
    RETURN.
  ELSE.
    temp_it_records[] = it_records[].
  ENDIF.


  LOOP AT temp_it_records INTO ls_sales_order ."where DATEPOSTED_STR BETWEEN '20150531' AND  '20150630' .

    ls_sohdr-sohdr_seqno = sy-tabix.
    ls_sohdr-date_posted = ls_sales_order-dateposted_str.
    ls_sohdr-doc_type = ls_sales_order-doc_type.
    ls_sohdr-collect_no = ls_sales_order-collect_no.
    ls_sohdr-sales_org = ls_sales_order-sales_org.
    ls_sohdr-distrib_channel = ls_sales_order-distr_chan.
    ls_sohdr-division = ls_sales_order-division.
    ls_sohdr-soldto = ls_sales_order-soldto.

    APPEND ls_sohdr TO it_sohdr.

    ls_soitems-sohdr_seqno = ls_sohdr-sohdr_seqno. "sy-tabix.
    ls_soitems-material = ls_sales_order-material.
    ls_soitems-qty = ls_sales_order-quantity.

    APPEND ls_soitems TO it_soitems.

******* Add materials and items.
*    AT FIRST.
*      ls_mat-sohdr_seqno = ls_sohdr-sohdr_seqno. "sy-tabix.
*      ls_mat-material = ls_sales_order-material.
*      ls_mat-qty = ls_mat-qty + ls_sales_order-quantity.
*    ENDAT.

    ls_mat-sohdr_seqno = ls_sohdr-sohdr_seqno. "sy-tabix.
    ls_mat-material = ls_sales_order-material.
    ls_mat-qty =  ls_sales_order-quantity."ls_mat-qty + ls_sales_order-quantity.
    ls_mat-collect_no = ls_sales_order-collect_no.  " Collect for identiying individual rows
    APPEND ls_mat TO lt_mat.

**********End of add items n materials.
    CLEAR:  ls_sohdr, ls_soitems, ls_mat.
  ENDLOOP.

  SORT lt_mat ASCENDING BY collect_no.
  DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING collect_no.

  IF it_distinct_mat[] IS NOT INITIAL.
    lt_mat3[] = it_distinct_mat[].
  ELSE. " Else fetch distinct materials from temp_it_records
* **************<<<<<<<<<<<<<<Write material and qty*************
    IF lt_mat[] IS NOT INITIAL.
      lt_mat2[] = lt_mat[].
      LOOP AT lt_mat INTO ls_mat.
        LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material.
          ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
          ls_mat3-material = ls_mat2-material.
          ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
          ls_mat3-collect_no = ls_mat2-collect_no. " will always fill the last records Collect_no.
          CLEAR ls_mat2.
        ENDLOOP.
        APPEND ls_mat3 TO lt_mat3.
        CLEAR ls_mat3.
        DELETE lt_mat WHERE material = ls_mat-material." and SOHDR_SEQNO = ls_mat-SOHDR_SEQNO.
        CLEAR ls_mat.
      ENDLOOP.
    ENDIF.
  ENDIF.
***********end of write************

  IF it_sohdr[] IS NOT INITIAL AND it_soitems IS NOT INITIAL.
    LOOP AT lt_mat3 INTO ls_mat3.
      READ TABLE it_soitems INTO temp_soitems WITH KEY material = ls_mat3-material.
*      TEMP_SOITEMS-SOHDR_SEQNO = sy-tabix.
*      temp_soitems-qty = 1." put for testing, use actual qty instead.
      APPEND temp_soitems TO it_temp_soitems .
      READ TABLE it_sohdr INTO temp_sohdr WITH KEY collect_no = ls_mat3-collect_no. "sohdr_seqno = temp_soitems-sohdr_seqno.
      IF  temp_sohdr-division EQ 0.
        temp_sohdr-division = '00'.
      ENDIF.
*      TEMP_SOHDR-SOHDR_SEQNO = sy-tabix.
      APPEND temp_sohdr TO it_temp_sohdr.
    ENDLOOP.

    SORT it_temp_sohdr ASCENDING BY sohdr_seqno.
    SORT it_temp_soitems ASCENDING BY sohdr_seqno.
    IF it_temp_sohdr[] IS NOT INITIAL AND it_temp_soitems IS NOT INITIAL.
      CALL FUNCTION 'ZCREATESO_DATAGEN1'
        IMPORTING
          msg        = msg
        TABLES
          it_sohdr   = it_temp_sohdr
          it_soitems = it_temp_soitems
          ot_log     = ot_log
          ot_dblog   = ot_dblog.

      ot_log_temp[] = ot_log.
      DELETE ot_log_temp WHERE result_flag = 'E'.

      DATA : lv_sonum TYPE vbeln.
      DATA:flag            TYPE  sy-subrc,
           deliverydoc_msg TYPE  string,
           deliveryno      TYPE  vbeln.
      IF ot_log_temp[] IS NOT INITIAL. " Some SO's created successfully
        LOOP AT ot_log_temp INTO ls_log.



          lv_sonum = ls_log-sonumber.
          READ TABLE ot_dblog INTO ls_dblog WITH KEY documentno = lv_sonum.
          READ TABLE temp_it_records INTO ls_dbupdate_so WITH KEY collect_no = ls_dblog-collect_no.
          ls_dbupdate_so-lastchangedate = sy-datum.
          ls_dbupdate_so-lastchangeuser = sy-uname.

          CALL FUNCTION 'ZDELIVERY_PGI_DATAGEN'
            EXPORTING
              so_num         = lv_sonum
              actual_gidate  = ls_log-date_posted
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
*            APPEND ls_dbupdate_so TO lt_dbupdate_so.
*        Write : 'material:',LS_DBUPDATE_SO-MATERIAL,'Has Erros'.
            MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.
*        ls_dbupdate_so-lastchangedate = sy-datum.
*        ls_dbupdate_so-lastchangeuser = sy-uname.
        APPEND ls_dbupdate_so TO lt_dbupdate_so.
        CLEAR : ls_dblog,ls_dbupdate_so.
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
            IF sy-subrc IS INITIAL or billingdocument is INITIAL.
              ls_dbupdate_so-status = 'E'.
              ls_dbupdate_so-docstatus = '2'.
              CONCATENATE lv_sonum  deliveryno INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
              APPEND ls_dbupdate_so TO lt_dbupdate_so.
*          LS_DBUPDATE_SO-OBJECTID = lv_sonum.
*          WRITE : billingdoc_msg.
              CONCATENATE ls_dblog-msg billingdoc_msg INTO ls_dblog-msg SEPARATED BY space.
            ELSEIF billingdocument IS NOT INITIAL.
              ls_dbupdate_so-status = 'S'.
              ls_dbupdate_so-docstatus = '3'.
              CONCATENATE lv_sonum  deliveryno billingdocument INTO ls_dbupdate_so-objectid SEPARATED BY '-'.
              APPEND ls_dbupdate_so TO lt_dbupdate_so.
              CONCATENATE deliveryno billingdocument INTO ls_dblog-external_document SEPARATED BY '-'.
              NEW-LINE.
              WRITE: 'SO-',lv_sonum,'DeiveryDoc-',deliveryno,' Billing Document ', billingdocument, ' created successfully'.
            ENDIF.
          ENDIF.
          MODIFY ot_dblog FROM ls_dblog TRANSPORTING msg external_document WHERE documentno = lv_sonum.

*          ls_dbupdate_so-lastchangedate = sy-datum.
*          ls_dbupdate_so-lastchangeuser = sy-uname.

*          APPEND ls_dbupdate_so TO lt_dbupdate_so.
          CLEAR : ls_dblog,ls_dbupdate_so.


          NEW-LINE.
          NEW-LINE.
          NEW-LINE.
        ENDLOOP.
      ENDIF.
***************approach 2 end

*  Udpate Abap system log table
      IF ot_dblog[] IS NOT INITIAL.
        MODIFY zdatagen_logging FROM TABLE ot_dblog.
      ENDIF.
***  End of ABAP system log table
    ENDIF.
  ENDIF.

ENDFUNCTION.
