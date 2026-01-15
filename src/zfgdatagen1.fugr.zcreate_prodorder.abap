FUNCTION zcreate_prodorder.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_PRODORDER) TYPE  ZPRODORDER_STR
*"  EXPORTING
*"     VALUE(ORDER_NUMBER) TYPE  AUFNR
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR100
*"----------------------------------------------------------------------


*   This method creates a production order
  DATA: ls_orderdata      TYPE bapi_pp_order_create.
  DATA: lv_order_number   TYPE aufnr.

  DATA: lt_return         TYPE TABLE OF bapiret2.
  DATA: ls_return         TYPE bapiret2.
  DATA: lv_prod_version TYPE verid.


  ls_orderdata-material_long    = is_prodorder-matnr.
  TRANSLATE ls_orderdata-material_long TO UPPER CASE.
  ls_orderdata-plant            = is_prodorder-plant.
  ls_orderdata-order_type       = is_prodorder-order_type.
  ls_orderdata-quantity         = is_prodorder-total_quantity.
  ls_orderdata-quantity_uom     = is_prodorder-unit.
*  ls_orderdata-basic_start_date = sy-datum. "is_prodorder-start_date.
  IF is_prodorder-start_date IS INITIAL.
    ls_orderdata-basic_start_date  = sy-datum.
  ELSE .
    ls_orderdata-basic_start_date   = is_prodorder-start_date.
  ENDIF.

  IF is_prodorder-end_date IS INITIAL.
    ls_orderdata-basic_end_date  = sy-datum.
  ELSE .
    ls_orderdata-basic_end_date   = is_prodorder-end_date.
  ENDIF.

  IF is_prodorder-prod_version IS INITIAL.

    CALL FUNCTION 'CO_SD_GET_PROD_VERS'
      EXPORTING
        imp_werks        = ls_orderdata-plant
        imp_matnr        = ls_orderdata-material_long
        imp_gamng        = ls_orderdata-quantity
        imp_date         = ls_orderdata-basic_start_date "sy-datum
*       KZ_FAUF          =
*       KZ_F4_HELP       =
      IMPORTING
        exp_verid        = ls_orderdata-prod_version "lv_prod_version
*       EXP_VERTO        =
*       EXP_LGORT        =
*       EXP_CSPLT        =
      EXCEPTIONS
        no_version_found = 1
        OTHERS           = 2.
    IF sy-subrc = 1.
* Implement suitable error handling here
      flag = 'E'.
      CONCATENATE 'Failed:' 'Production version not found' INTO msg SEPARATED BY space.
      RETURN.
    ENDIF.

  ENDIF.
*  ls_orderdata-prod_version     = is_prodorder-prod_version.
  ls_orderdata-order_priority   = is_prodorder-order_priority.
  ls_orderdata-gr_proc_time     = is_prodorder-gr_proc_time.
  ls_orderdata-explosion_date   = sy-datum.

*   create production order
  CALL FUNCTION 'BAPI_PRODORD_CREATE'
    EXPORTING
      orderdata    = ls_orderdata
    IMPORTING
      return       = ls_return
      order_number = lv_order_number.

  APPEND ls_return TO lt_return.
  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY
    type = 'E'.

  IF sy-subrc <> 0.
    " no error.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'  " commit not needed as internal commit available
*     EXPORTING
*       WAIT          = 'X'.


    flag = 'S'.
    order_number = lv_order_number.
    CONCATENATE 'Sucess!! : Production Order' order_number 'created successfully' INTO msg SEPARATED BY space.

    IF is_prodorder-schedule_type IS NOT INITIAL.
      DATA : sched_type       TYPE bapi_order_func_cntrl-sched_type,
             orders           TYPE TABLE OF  bapi_order_key,
             wa_orders        LIKE LINE OF orders,
             detail_return    TYPE TABLE OF   bapi_order_return,
             wa_detail_return LIKE LINE OF detail_return,
             return           TYPE bapiret2.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' " calling commit as per the below bapi documentation
        EXPORTING
          wait = 'X'.

      wa_orders-order_number = order_number.
      APPEND wa_orders TO orders.
      WAIT UP TO 1 SECONDS.
      CALL FUNCTION 'BAPI_PRODORD_SCHEDULE'
        EXPORTING
          sched_type    = is_prodorder-schedule_type
*         WORK_PROCESS_GROUP       = 'COWORK_BAPI'
*         WORK_PROCESS_MAX         = 99
*     IMPORTING
*         RETURN        =
        TABLES
          orders        = orders
          detail_return = detail_return
*         APPLICATION_LOG          =
        .

      READ TABLE detail_return INTO wa_detail_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CONCATENATE msg '--Scheduling not updated ' INTO msg SEPARATED BY space.
*      else.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*             EXPORTING
*       WAIT          = 'X'.
      ENDIF.

    ENDIF.


  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    flag = 'E'.
    CONCATENATE 'Failed:' ls_return-message INTO msg SEPARATED BY space.
  ENDIF.

*    et_return       = lt_return.

ENDFUNCTION.
