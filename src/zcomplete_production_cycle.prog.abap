*&---------------------------------------------------------------------*
*& Report ZCOMPLETE_PRODUCTION_CYCLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcomplete_production_cycle.

TABLES: aufk,afko.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_order FOR aufk-aufnr." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_aufk      TYPE TABLE OF aufk,
       ls_aufk      TYPE aufk,
       order_number TYPE aufnr.
DATA : ls_afko TYPE afko,
       lt_afko TYPE TABLE OF afko.
DATA : ls_datagen_log TYPE zdatagen_logging,
       lt_datagen_log TYPE TABLE OF zdatagen_logging.

DATA : rel_flag          TYPE  char1,
       rel_msg           TYPE  char100,
       es_return         TYPE  bapiret2,
       it_orders         TYPE  tb_bapi_order_key,
       wa_orders         LIKE LINE OF it_orders,
       et_return_details TYPE  bapirettab,
       wa_return_details LIKE LINE OF et_return_details.

*TYPES: BEGIN OF ty_productionlog,
*         matnr     TYPE matnr,
*         mtart     TYPE mtart,
*         qty       TYPE dzmeng,
*         prodorder TYPE aufnr,
*         msg       TYPE  bapi_msg,
*         flag      TYPE char1,
*       END OF ty_productionlog.*
*DATA: ls_productionlog TYPE ty_productionlog,
*      lt_productionlog TYPE TABLE OF ty_productionlog.

SELECT * FROM aufk INTO TABLE lt_aufk WHERE aufnr IN s_order.

IF  lt_aufk[] IS NOT INITIAL.
  LOOP AT lt_aufk INTO ls_aufk.
    CLEAR: order_number, ls_datagen_log.
    order_number = ls_aufk-aufnr.
***to update log
    SELECT SINGLE * FROM zdatagen_logging INTO ls_datagen_log WHERE documentno =  order_number AND transaction_type = 'PROD'." AND result_flag = 'E'.

    wa_orders-order_number = ls_aufk-aufnr. "order_number.
    APPEND wa_orders TO it_orders.
    CLEAR wa_orders.
    CALL FUNCTION 'ZRELEASE_PRODORDER'
      IMPORTING
        flag              = rel_flag
        msg               = rel_msg
        es_return         = es_return
      TABLES
        it_orders         = it_orders
        et_return_details = et_return_details.
    rel_flag = 'S'.
    READ TABLE et_return_details INTO wa_return_details WITH KEY type = 'W' id = 'CO' number = '281'.
    IF sy-subrc IS INITIAL.  " Has some missing parts.
      CONCATENATE rel_msg wa_return_details-message INTO rel_msg SEPARATED BY space.
      rel_flag = 'E'.
    ENDIF.
    LOOP AT et_return_details INTO wa_return_details WHERE type = 'E'.
      rel_flag = 'E'.
      CONCATENATE rel_msg wa_return_details-message INTO rel_msg SEPARATED BY space.
    ENDLOOP.
    ls_datagen_log-result_flag = rel_flag.
    ls_datagen_log-msg = rel_msg.
    CLEAR: et_return_details,it_orders,es_return.
    IF rel_flag = 'E'.
      WRITE : order_number ,rel_msg. NEW-LINE.
      APPEND ls_datagen_log TO lt_datagen_log.
      CONTINUE.
    ELSEIF rel_flag = 'S'. "Confirm activities if PO released.
      DATA: confirm_flag TYPE  char1,
            confirm_msg  TYPE  char100.
      WAIT UP TO 1 SECONDS.
      SELECT SINGLE * FROM afko INTO ls_afko WHERE aufnr = order_number.
      IF sy-subrc IS NOT INITIAL.
        WRITE : ' Error FETCHING  Order header details from Table AFKO ', order_number.NEW-LINE.
        CONCATENATE ls_datagen_log-msg 'Error FETCHING  Order header details from Table AFKO' order_number INTO ls_datagen_log-msg SEPARATED BY space.
        APPEND ls_datagen_log TO lt_datagen_log.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'ZCONFIRM_PRODORDER' "'ZCONFIRM_PRODORDER_NEW'
        EXPORTING
          iv_order_number   = order_number
          iv_pstng_date     = ls_afko-gstrs " SCHEDULED START DATE
*         IV_UNIT           =
          iv_total_quantity = ls_afko-gamng
*         IV_YIELD1         =
*         IV_YIELD2         =
*         IV_YIELD3         =
*         IV_YIELD4         =
        IMPORTING
          flag              = confirm_flag
          msg               = confirm_msg.

      WRITE : confirm_msg.NEW-LINE.
      ls_datagen_log-result_flag = confirm_flag.
      ls_datagen_log-msg = confirm_msg.
      APPEND ls_datagen_log TO lt_datagen_log.

    ENDIF.

*    MOVE-CORRESPONDING ls_productionlog TO ls_datagen_log.
*    APPEND ls_datagen_log TO lt_datagen_log.
    CLEAR : ls_datagen_log , ls_afko."ls_productionlog,

  ENDLOOP.
ENDIF.

IF lt_datagen_log[] IS NOT INITIAL .
  MODIFY zdatagen_logging FROM TABLE lt_datagen_log.
ENDIF.
