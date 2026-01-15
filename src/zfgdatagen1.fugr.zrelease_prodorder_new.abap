FUNCTION zrelease_prodorder_new.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR100
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      IT_ORDERS TYPE  TB_BAPI_ORDER_KEY OPTIONAL
*"      ET_RETURN_DETAILS TYPE  BAPIRETTAB OPTIONAL
*"----------------------------------------------------------------------

  DATA: ls_return  TYPE bapiret2,
        lt_details TYPE TABLE OF bapi_order_return.
  FIELD-SYMBOLS: <ls_detail> TYPE bapi_order_return.

*DATA: ES_RETURN  TYPE BAPIRET2,
*ET_RETURN_DETAILS  TYPE BAPIRETTAB.

  DATA : ls_order LIKE LINE OF it_orders.
  DATA: lwa_aufnr  TYPE ord_pre,
        lwa_tca11  TYPE tca11,
        lwa_caufvd TYPE caufvd,
        lt_aufnr   TYPE STANDARD TABLE OF ord_pre.

  LOOP AT it_orders INTO ls_order .

    CLEAR : lt_aufnr,lwa_aufnr,lwa_caufvd,lwa_tca11.

    lwa_aufnr-aufnr = ls_order-order_number.
    APPEND lwa_aufnr TO lt_aufnr.

    lwa_tca11-flg_alt  =
    lwa_tca11-flg_seq  =
    lwa_tca11-flg_opr  =
    lwa_tca11-flg_sop  =
    lwa_tca11-flg_aob  =
    lwa_tca11-flg_fhm  =
    lwa_tca11-flg_mst  =
    lwa_tca11-flg_phas = 'X'.

    CALL FUNCTION 'CO_ZF_ORDER_READ'
      EXPORTING
        flg_dialog        = space
        flg_enqueue       = 'X'
        objects_imp       = lwa_tca11
        flg_prot_imp      = 'X'
      TABLES
        aufnr_tab_imp     = lt_aufnr
      EXCEPTIONS
        order_not_found   = 1
        release_no_change = 2
        OTHERS            = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION 'CO_BT_CAUFV_READ_WITH_KEY'
        EXPORTING
          aufnr_act      = ls_order-order_number
          no_dialog_info = 'X'
        IMPORTING
          caufvd_exp     = lwa_caufvd
        EXCEPTIONS
          not_found      = 1
          OTHERS         = 2.

      IF sy-subrc EQ 0.
        CALL FUNCTION 'CO_ZR_HEADER_RELEASE_NEW'
          EXPORTING
            caufvd_imp         = lwa_caufvd
            prot_init_imp      = 'X'
            write_prot_imp     = 'X'
            no_dialog_imp      = 'X'
            no_avail_check_imp = 'X'
          IMPORTING
            caufvd_exp         = lwa_caufvd
          EXCEPTIONS
            free_failed        = 1
            end_task           = 2
            OTHERS             = 3.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          ls_return-type = 'S'.
          CONCATENATE ls_order-order_number 'released scessfully' INTO ls_return-message SEPARATED BY space.
        ELSE.
          ls_return-type = 'E'.
          CONCATENATE ls_order-order_number 'Could not be released' INTO ls_return-message SEPARATED BY space.
        ENDIF.
      ENDIF.
    ELSEIF sy-subrc = 1.
      ls_return-type = 'E'.
      CONCATENATE ls_order-order_number 'Not Found' INTO ls_return-message SEPARATED BY space.

    ELSEIF sy-subrc = 2.
      ls_return-type = 'E'.
      CONCATENATE ls_order-order_number 'Release No change' INTO ls_return-message SEPARATED BY space.
    ELSE.
      ls_return-type = 'E'.
      CONCATENATE ls_order-order_number 'Could not be released' INTO ls_return-message SEPARATED BY space.
    ENDIF.

*    ls_return-type
*    ls_return-id
*    ls_return-number
*    ls_return-message.
    APPEND ls_return TO et_return_details.
    clear: ls_return,ls_order.
  ENDLOOP.

ENDFUNCTION.
