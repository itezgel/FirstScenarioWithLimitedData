FUNCTION zperform_pgi_of_deliverydoc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SO_NUM) TYPE  VBAK-VBELN
*"     VALUE(ACTUAL_GIDATE) TYPE  WADAT_IST
*"  EXPORTING
*"     VALUE(FLAG) TYPE  SY-SUBRC
*"     VALUE(MSG) TYPE  STRING
*"     VALUE(DELIVERYNUMBER) TYPE  VBELN
*"----------------------------------------------------------------------

  DATA: lf_vbeln  TYPE vbeln_vl,
        lf_num    TYPE vbnum,
        ls_deli   TYPE bapishpdelivnumb,
        lt_deli   TYPE TABLE OF bapishpdelivnumb,
        lt_order  TYPE TABLE OF bapidlvreftosalesorder,
        ls_order  TYPE bapidlvreftosalesorder,
        ls_itm    TYPE bapidlvitemcreated,
        lt_itm    TYPE TABLE OF bapidlvitemcreated,
        ls_ext    TYPE bapiparex,
        lt_extin  TYPE TABLE OF bapiparex,
        lt_extout TYPE TABLE OF bapiparex,
        ls_ret    TYPE bapiret2,
        lt_return TYPE TABLE OF bapiret2.

  DATA: lt_vbfa_h TYPE TABLE OF vbfa,
        lt_vbfa_r TYPE TABLE OF vbfa,
        r_count   TYPE i,
        h_count type i.

  CALL FUNCTION 'RV_DELIVERY_INIT'
*   EXPORTING
*     STATUS_BUFF_INIT       =
*     TEXT_MEMORY_INIT       = ' '
*     PP_DELIVERY            = ' '
*     I_CALL_ACTIVITY        = ' '
*     IF_NO_DEQUE            = ' '
    .

** SalesOrderItems (here: complete sales order)
*  ls_order-ref_doc = so_num.
*  APPEND ls_order TO lt_order.
*
** ExtensionIn
*  ls_ext = 'My additional input'.
*  APPEND ls_ext TO lt_extin.
  DATA: ls_vbfa TYPE vbfa.
  SELECT SINGLE * FROM vbfa INTO ls_vbfa WHERE vbelv = so_num AND vbtyp_n = 'J'.

  IF sy-subrc IS NOT INITIAL.
    CONCATENATE 'Error:Delivery doc does not exist for ' so_num  INTO msg SEPARATED BY space.
    RETURN.
  ELSE.
    lf_vbeln = ls_vbfa-vbeln.
    SELECT * FROM vbfa INTO table lt_vbfa_r WHERE vbelv = ls_vbfa-vbeln AND vbtyp_n = 'R'. " No of Goods issue
      DESCRIBE TABLE lt_vbfa_r LINES r_count.
    SELECT * FROM vbfa INTO table lt_vbfa_h WHERE vbelv = ls_vbfa-vbeln AND vbtyp_n = 'h'. " No of Goods issue reversal
      DESCRIBE TABLE lt_vbfa_h LINES h_count.
      IF r_count GT h_count. " Do PGI
          FLAG = 1.
          deliverynumber = lf_vbeln.
          return.

      ENDIF.
 ENDIF.




** Synchronous RFCova03
*  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
**   DESTINATION logsys
**   EXPORTING
**     SHIP_POINT               =
**     DUE_DATE                 =
**     DEBUG_FLG               =
*    IMPORTING
*      delivery          = lf_vbeln
*      num_deliveries    = lf_num
*    TABLES
*      sales_order_items = lt_order
*      extension_in      = lt_extin
*      deliveries        = lt_deli
*      created_items     = lt_itm
*      extension_out     = lt_extout
*      return            = lt_return.

      IF lf_vbeln IS NOT INITIAL.
        DATA: return TYPE bapiret2.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait   = 'X'
*      IMPORTING
*        return = return.

        deliverynumber = lf_vbeln.
************* Try picking and PGI together**************
        DATA: l_vbkok TYPE vbkok , "OCCURS 0,
              i_vbpok TYPE TABLE OF vbpok WITH HEADER LINE,
              l_verko TYPE TABLE OF verko WITH HEADER LINE.

        l_vbkok-vbtyp_vl = 'J'.
        l_vbkok-vbeln = l_vbkok-vbeln_vl = lf_vbeln. "do number
        l_vbkok-wabuc = 'X'. "automatically PGI selection
        IF actual_gidate IS NOT INITIAL.
          l_vbkok-wadat_ist = actual_gidate. "Actual Goods Movement Date
        ENDIF.

*APPEND l_vbkok.

        l_verko-object = '01'.
        l_verko-objkey = lf_vbeln.

        APPEND: l_verko.
        DATA : i_lips TYPE TABLE OF lips WITH HEADER LINE.
        REFRESH i_lips.
        SELECT * FROM lips
        INTO TABLE i_lips
        WHERE vbeln = lf_vbeln." AND charg NE ''.


          CLEAR i_vbpok.
*    LOOP AT i_lips..
**move-corresponding i_lips to i_vbpok.
*      i_vbpok-vbeln_vl = i_lips-vbeln."Delivery
*      i_vbpok-posnr_vl = i_lips-posnr."Delivery Item
*      i_vbpok-posnn = i_lips-posnr."Delivery Item
*      i_vbpok-vbeln = i_lips-vbeln."Pick Order
*      i_vbpok-vbtyp_n = 'Q'.
*
*      i_vbpok-pikmg = i_lips-lfimg.
*      i_vbpok-meins = i_lips-meins.
**
***i_VBPOK-NDIFM = 0.
**i_VBPOK-TAQUI = ' '.
*      i_vbpok-charg = i_lips-charg.
*      i_vbpok-matnr = i_lips-matnr.
*      i_vbpok-werks = i_lips-werks.
*      APPEND i_vbpok.
*    ENDLOOP.

          CALL FUNCTION 'WS_DELIVERY_UPDATE_2'
            EXPORTING
              vbkok_wa             = l_vbkok
              synchron             = ''
              no_messages_update_1 = ' '
*             commit               = 'X'
              delivery             = lf_vbeln "dono
              update_picking       = 'X'
              nicht_sperren_1      = 'X'
            TABLES
              vbpok_tab            = i_vbpok
              verko_tab            = l_verko.

          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' " call FM to
              EXPORTING
                wait = 'X'.
            flag = 1.
            CONCATENATE 'Delivery' lf_vbeln 'created, PGI Complete' INTO msg SEPARATED BY space.
          ELSE.
            ROLLBACK WORK.
            flag = 2.
            CONCATENATE 'Delivery' lf_vbeln 'created, PGI FAILED' INTO msg SEPARATED BY space.
          ENDIF.

        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
            .
          CALL FUNCTION 'RV_DELIVERY_INIT'
*   EXPORTING
*     STATUS_BUFF_INIT       =
*     TEXT_MEMORY_INIT       = ' '
*     PP_DELIVERY            = ' '
*     I_CALL_ACTIVITY        = ' '
*     IF_NO_DEQUE            = ' '
            .

          flag = 2.
          LOOP AT lt_return INTO ls_ret.
            CONCATENATE msg ls_ret-message INTO msg SEPARATED BY space.
          ENDLOOP.
        ENDIF.

*WRITE: / 'Delivery:', lf_vbeln,
*       / 'NumDeliveries:', lf_num,
*       / 'Deliveries:'.
*LOOP AT lt_deli INTO ls_deli.
*  WRITE ls_deli-deliv_numb.
*ENDLOOP.

*IF NOT lt_itm[] IS INITIAL.
*  WRITE: / 'CreatedItems:'.
*  LOOP AT lt_itm INTO ls_itm.
*    WRITE: / ls_itm-ref_doc,
*             ls_itm-ref_item,
*             ls_itm-deliv_numb,
*             ls_itm-deliv_item,
*             ls_itm-material,
*             ls_itm-dlv_qty,
*             ls_itm-sales_unit,
*             ls_itm-sales_unit_iso.
*  ENDLOOP.
*ENDIF.

*if not lt_return[] is initial.
*  write: / 'Return:'.
*  loop at lt_return into ls_ret.
*    write: / ls_ret-type, ls_ret-id, ls_ret-number,
*             ls_ret-message,
*           /.
*  endloop.
*endif.
*
*if not lt_extout[] is initial.
*  write: / 'ExtensionOut:'.
*  loop at lt_extout into ls_ext.
*    write: / ls_ext.
*  endloop.
*endif.

      ENDFUNCTION.
