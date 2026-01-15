FUNCTION ZRELEASE_PRODORDER_PA.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR100
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"  TABLES
*"      IT_ORDERS TYPE  TB_BAPI_ORDER_KEY OPTIONAL
*"      ET_RETURN_DETAILS TYPE  BAPIRETTAB OPTIONAL
*"--------------------------------------------------------------------

DATA: ls_return  TYPE bapiret2,
          lt_details TYPE TABLE OF bapi_order_return.
    FIELD-SYMBOLS: <ls_detail> TYPE bapi_order_return.

*DATA: ES_RETURN  TYPE BAPIRET2,
*ET_RETURN_DETAILS  TYPE BAPIRETTAB.


    CALL FUNCTION 'BAPI_PRODORD_RELEASE'
      EXPORTING
        release_control = '1' "1 single order 2 for collective
      IMPORTING
        return          = es_return
      TABLES
        orders          = it_orders
        detail_return   = lt_details.

    LOOP AT lt_details ASSIGNING <ls_detail>.
      MOVE-CORRESPONDING <ls_detail> TO ls_return.
      APPEND ls_return TO et_return_details.
    ENDLOOP.


    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = 'X'.

ENDFUNCTION.
