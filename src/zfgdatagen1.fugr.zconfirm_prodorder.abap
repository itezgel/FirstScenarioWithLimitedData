FUNCTION zconfirm_prodorder .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_ORDER_NUMBER) TYPE  AUFNR
*"     VALUE(IV_PSTNG_DATE) TYPE  DATS OPTIONAL
*"     VALUE(IV_UNIT) TYPE  CO_GMEIN OPTIONAL
*"     VALUE(IV_TOTAL_QUANTITY) TYPE  GAMNG OPTIONAL
*"     VALUE(IV_YIELD1) TYPE  RU_LMNGA OPTIONAL
*"     VALUE(IV_YIELD2) TYPE  RU_LMNGA OPTIONAL
*"     VALUE(IV_YIELD3) TYPE  RU_LMNGA OPTIONAL
*"     VALUE(IV_YIELD4) TYPE  RU_LMNGA OPTIONAL
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR100
*"----------------------------------------------------------------------

  DATA: ls_pp_order_objects TYPE bapi_pp_order_objects,
        lt_operations       TYPE TABLE OF bapi_order_operation1,
        lv_index            TYPE sy-tabix,
        ls_return           TYPE bapiret1,
        ls_return2          TYPE bapiret2,
        lt_detail_return    TYPE TABLE OF bapi_coru_return,
        ls_confirm          TYPE bapi_pp_timeticket,
        lt_confirms         TYPE TABLE OF bapi_pp_timeticket.
  FIELD-SYMBOLS: <ls_operation> TYPE bapi_order_operation1,
                 <ls_return>    TYPE bapi_coru_return.

  DATA : es_return         TYPE bapiret2,
         et_return_details TYPE bapirettab.


*  CHECK iv_yield1 IS NOT INITIAL.

  ls_pp_order_objects-operations = 'X'.

  CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
    EXPORTING
      number        = iv_order_number
      order_objects = ls_pp_order_objects
    TABLES
      operation     = lt_operations.

  DATA : propose            TYPE  bapi_pp_conf_prop,
         return             TYPE  bapiret1,
         timetickets        TYPE TABLE OF  bapi_pp_timeticket,
         ls_timetickets     LIKE LINE OF timetickets,
         goodsmovements     TYPE TABLE OF  bapi2017_gm_item_create,
         link_conf_goodsmov TYPE TABLE OF  bapi_link_conf_goodsmov,
         detail_return      TYPE TABLE OF   bapi_coru_return.




*  ls_confirm-orderid = iv_order_number.
*  ls_confirm-sequence = ''.
  LOOP AT lt_operations ASSIGNING <ls_operation>.
    lv_index = sy-tabix.
*    ls_confirm-operation = <ls_operation>-operation_number.
*    ls_confirm-sub_oper = <ls_operation>-suboperation.
*    ls_confirm-conf_quan_unit = iv_unit.

    CLEAR ls_timetickets.
    ls_timetickets-orderid = iv_order_number.
    ls_timetickets-sequence = ''.
    ls_timetickets-operation = <ls_operation>-operation_number.
    ls_timetickets-sub_oper = <ls_operation>-suboperation.
    APPEND ls_timetickets TO timetickets.

    CALL FUNCTION 'BAPI_PRODORDCONF_GET_TT_PROP'
* EXPORTING
*   PROPOSE                  = PROPOSE
      IMPORTING
        return             = return
      TABLES
        timetickets        = timetickets
        goodsmovements     = goodsmovements
        link_conf_goodsmov = link_conf_goodsmov
        detail_return      = detail_return.

    READ TABLE timetickets INTO ls_confirm INDEX 1.
    IF sy-subrc IS NOT INITIAL.
      flag = 'E'.
      LOOP AT detail_return ASSIGNING <ls_return>.
        MOVE-CORRESPONDING <ls_return> TO ls_return2.
        IF ls_return2-type = 'E'.
          flag = 'E'.
          CONCATENATE ls_return2-message msg INTO msg SEPARATED BY space.
        ELSE.
          flag = 'S'.
          CONCATENATE ls_return2-message msg INTO msg SEPARATED BY space.
          APPEND ls_return TO et_return_details.
        ENDIF.
      ENDLOOP.
      EXIT.
    ENDIF.

*    CASE lv_index.
*      WHEN 1.
*        ls_confirm-yield = iv_yield1.
*        WHEN 2.
*          ls_confirm-yield = iv_yield2.
*        WHEN 3.
*          ls_confirm-yield = iv_yield3.
*        WHEN 4.
*          ls_confirm-yield = iv_yield4.
*    ENDCASE.

    IF iv_pstng_date IS NOT INITIAL.
      ls_confirm-postg_date = iv_pstng_date.
    ENDIF.

    IF  iv_total_quantity IS INITIAL.
      flag = 'S'.
      CONCATENATE 'Please enter Quantiyt/Yield'  msg INTO msg SEPARATED BY space.
      EXIT.
    ELSE.
      ls_confirm-yield = iv_total_quantity.
    ENDIF.

*    AT last .
**    IF ls_confirm-yield = iv_total_quantity.
    ls_confirm-fin_conf = abap_true.
**    ENDIF.
*    ENDAT.
    APPEND ls_confirm TO lt_confirms.
    CLEAR  : ls_confirm, timetickets.
  ENDLOOP.

  CALL FUNCTION 'BAPI_PRODORDCONF_CREATE_TT'
    IMPORTING
      return             = ls_return
    TABLES
      timetickets        = lt_confirms
      goodsmovements     = goodsmovements
      link_conf_goodsmov = link_conf_goodsmov
      detail_return      = lt_detail_return.

  MOVE-CORRESPONDING ls_return TO es_return.
  IF ls_return-type = 'E'.
    flag = 'E'.
    CONCATENATE ls_return-message msg INTO msg SEPARATED BY space.
  ELSE.
    flag = 'S'.
    CONCATENATE ls_return-message msg INTO msg SEPARATED BY space.
  ENDIF.

  LOOP AT lt_detail_return ASSIGNING <ls_return>.
    MOVE-CORRESPONDING <ls_return> TO ls_return2.
    IF ls_return2-type = 'E'.
      flag = 'E'.
      CONCATENATE ls_return2-message msg INTO msg SEPARATED BY space.
    ELSE.
      flag = 'S'.
      CONCATENATE ls_return2-message msg INTO msg SEPARATED BY space.
      APPEND ls_return TO et_return_details.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDFUNCTION.
