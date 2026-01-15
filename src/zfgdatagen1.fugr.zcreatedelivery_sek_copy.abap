FUNCTION zcreatedelivery_sek_copy.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SALESORDERNUMBER) TYPE  VBAK-VBELN
*"  EXPORTING
*"     VALUE(CHECK) TYPE  SY-SUBRC
*"     VALUE(MESSAGE) TYPE  STRING
*"     VALUE(DELIVERYNUMBER) TYPE  VBELN
*"----------------------------------------------------------------------
  DATA: BEGIN OF t_vbap OCCURS 0,
          vbeln LIKE vbap-vbeln,
          posnr LIKE vbap-posnr,
          zmeng LIKE vbap-kwmeng,
          matnr LIKE vbap-matnr,
          werks LIKE vbap-werks,
        END OF t_vbap.

  DATA: t_request TYPE STANDARD TABLE OF bapideliciousrequest WITH HEADER LINE.

  DATA: t_created TYPE STANDARD TABLE OF bapideliciouscreateditems WITH HEADER LINE.

  DATA: t_return TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE.

  DATA : BEGIN OF goodsdate OCCURS 0,
           goodsmovementdate TYPE likp-wadat,
           docdate           TYPE likp-bldat,
         END OF goodsdate.

  SELECT vbeln posnr kwmeng matnr werks
  INTO TABLE t_vbap
  FROM vbap
  WHERE vbeln = salesordernumber.

*  select wadat bldat into table goodsdate from likp where vbeln = SALESORDERNUMBER.

*  sekhar

  DATA view TYPE order_view.
  DATA key TYPE TABLE OF sales_key WITH HEADER LINE.
  DATA it_sched TYPE TABLE OF bapisdhedu WITH HEADER LINE.
  DATA it_item TYPE TABLE OF bapisdit WITH HEADER LINE.

  view-sdschedule = 'X'.
  view-item = 'X'.
  APPEND salesordernumber TO key.

  CALL FUNCTION 'BAPISDORDER_GETDETAILEDLIST'
    EXPORTING
      i_bapi_view         = view
    TABLES
      sales_documents     = key
      order_items_out     = it_item
      order_schedules_out = it_sched.

* sekhar

  LOOP AT t_vbap.
    LOOP AT it_sched WHERE itm_number = t_vbap-posnr AND confir_qty IS NOT INITIAL.
      EXIT.
    ENDLOOP.

    t_request-document_numb = t_vbap-vbeln.
    t_request-document_item = t_vbap-posnr.
    t_request-quantity_sales_uom = t_vbap-zmeng.
    t_request-quantity_base__uom = t_vbap-zmeng.
    t_request-id = 1.
    t_request-document_type = 'A'.
*    t_request-delivery_date = sy-datum.
    t_request-delivery_date = it_sched-ms_date.
    t_request-material = t_vbap-matnr.
    t_request-plant = t_vbap-werks.
    t_request-date = sy-datum.
    t_request-goods_issue_date = it_sched-gi_date.
*    t_request-date = .
*    t_request-goods_issue_time = sy-uzeit.
    APPEND t_request.

  ENDLOOP.
  WAIT UP TO 1 SECONDS.
  CALL FUNCTION 'BAPI_DELIVERYPROCESSING_EXEC'
    TABLES
      request      = t_request
      createditems = t_created
      return       = t_return.


  READ TABLE t_return WITH KEY type = 'E'.

  IF sy-subrc = 0.
    message = t_return-message.
    EXIT.
  ELSE.
    CONCATENATE 'Delivery' t_created-document_numb  'has been saved.' INTO message SEPARATED BY space.
    deliverynumber = t_created-document_numb.
  ENDIF.


  COMMIT WORK.

* check locked ornot
  IF deliverynumber IS NOT INITIAL.

    DATA number LIKE sy-tabix VALUE '1'.
    DATA lock_org TYPE eqegraarg.
    DATA it_enq TYPE TABLE OF seqg3 .
    DATA subrc LIKE sy-subrc.

    CONCATENATE sy-mandt deliverynumber INTO lock_org.

    WHILE number = 1.
      CLEAR number.
      WAIT UP TO 1 SECONDS.
      CALL FUNCTION 'ENQUE_READ'
        EXPORTING
          gclient = sy-mandt
          gname   = 'LIKP'
          garg    = lock_org
          guname  = sy-uname
        IMPORTING
          number  = number
          subrc   = subrc
        TABLES
          enq     = it_enq.

    ENDWHILE.

  ENDIF.

* end locking chek

  SELECT wadat bldat INTO TABLE goodsdate FROM likp WHERE vbeln = deliverynumber.

  DATA : tempdate      TYPE bdcdata-fval,
         tempdocdate   TYPE bdcdata-fval,
*      tempPickQuan type c length 13,
         tmep          TYPE p LENGTH 13 DECIMALS 2,
         temppickquan1 TYPE bdcdata-fval.

  LOOP AT goodsdate.
    WRITE goodsdate-goodsmovementdate TO tempdate.
    WRITE goodsdate-docdate TO tempdocdate.
  ENDLOOP.

  DATA tempdeliverydocnum TYPE bdcdata-fval.
  WRITE t_created-document_numb TO tempdeliverydocnum.

*      write sy-datum to tempdate.
  tmep = t_request-quantity_sales_uom.
  WRITE tmep TO temppickquan1.
  CONDENSE temppickquan1.
*  condense tempPickQuan.
*  tmep = tempPickQuan.
*  move tempPickQuan to tempPickQuan1.


*   confirm picking

  DATA ret TYPE bdcmsgcoll OCCURS 0.
  WAIT UP TO 1 SECONDS.
  CALL FUNCTION 'ZFM_CONFIRMPICK_SEK_COPY'
    EXPORTING
      ctu          = 'X'
      mode         = 'N'
      update       = 'L'
*     GROUP        =
*     USER         =
*     KEEP         =
*     HOLDDATE     =
*     NODATA       = '/'
      vbeln_001    = tempdeliverydocnum
      pikmg_01_002 = temppickquan1
      bldat_003    = tempdocdate
      wadat_004    = tempdate
*     BTGEW_005    = '200'
*     GEWEI_006    = 'G'
      bldat_007    = tempdocdate
      wadat_008    = tempdate
*     BTGEW_009    = '200'
*     GEWEI_010    = 'G'
    IMPORTING
      subrc        = check
    TABLES
      item         = it_item
      messtab      = ret.

  IF check EQ 0.

    number = 1.
    WHILE number = 1.
      CLEAR number.
      WAIT UP TO 1 SECONDS.
      CALL FUNCTION 'ENQUE_READ'
        EXPORTING
          gclient = sy-mandt
          gname   = 'LIKP'
          garg    = lock_org
          guname  = sy-uname
        IMPORTING
          number  = number
          subrc   = subrc
        TABLES
          enq     = it_enq.

    ENDWHILE.
  ELSE.
    message = 'Goods picking not successful.'.
    check = '2'.
  ENDIF.
ENDFUNCTION.
