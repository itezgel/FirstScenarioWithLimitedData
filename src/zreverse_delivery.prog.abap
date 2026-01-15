*&---------------------------------------------------------------------*
*& Report ZREVERSE_DELIVERY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zreverse_delivery.

TABLES : vbak,vbfa.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
PARAMETERS : ordtype TYPE auart OBLIGATORY DEFAULT 'ZCM'.
*PARAMETERS : ordno TYPE vbeln_va .
*PARAMETERS : vdatu TYPE edatu_vbak.
*PARAMETERS : fromdate TYPE dats,
*             todate   TYPE dats.
SELECT-OPTIONS : s_ordno for vbak-vbeln.
SELECT-OPTIONS : s_vdatu for vbak-vdatu.
SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.



DATA : lt_vbfa TYPE TABLE OF vbfa,
       ls_vbfa TYPE vbfa,
       lt_vbak TYPE TABLE OF vbak,
       ls_vbak TYPE vbak.
DATA : vbak_count    TYPE i,
             lv_seqno      TYPE i,
             lv_postingdate type dats.

*       success_count TYPE i VALUE IS INITIAL,
*       failure_count TYPE i VALUE IS INITIAL,
*       lv_fkdat      TYPE fkdat,
*       ls_datagenlog TYPE zdatagen_logging,

*DATA : return     TYPE TABLE OF  bapireturn1,
*       ls_return  TYPE bapireturn1,
*       success    TYPE TABLE OF bapivbrksuccess,
*       ls_success TYPE  bapivbrksuccess.
DATA: lv_argument TYPE seqg3-garg,
      lv_number   TYPE sy-tabix,
      lv_subrc    TYPE sy-subrc,
      lt_seqg3    TYPE TABLE OF seqg3.
DATA: lt_mesg       TYPE TABLE OF mesg,
      ls_mesg       TYPE mesg,
      it_likp       TYPE TABLE OF likp,
      lv_deliveryno LIKE likp-vbeln,
      lv_vbtyp      LIKE likp-vbtyp,
      ls_emkpf      TYPE  emkpf.
DATA: ls_header_inx      TYPE bapisdh1x,
      ls_header_in       TYPE bapisdh1,
      lt_bapireturn1     TYPE TABLE OF bapireturn1,
      ls_bapireturn1     TYPE  bapireturn1,
      it_bapivbrksuccess TYPE TABLE OF  bapivbrksuccess,
      lv_bill_doc        LIKE bapivbrksuccess-bill_doc,
      lt_message         TYPE TABLE OF string,
      ls_message         TYPE string,
      it_vbrp            TYPE TABLE OF vbrp,
      wa_vbrp            TYPE vbrp,
      lt_bapiret2        TYPE TABLE OF bapiret2,
      ls_bapiret2        TYPE  bapiret2.
DATA: ls_del_header  TYPE  bbp_inbd_l,
      lt_del_item    TYPE TABLE OF bbp_inbd_d,
      ls_del_item    TYPE bbp_inbd_d,
      lt_bapireturn  TYPE TABLE OF bapireturn,
      ls_bapireturn  TYPE bapireturn,
      ls_header      TYPE bapiobdlvhdrchg,
      ls_headercntrl TYPE bapiobdlvhdrctrlchg,
      lt_item        TYPE TABLE OF  bapiobdlvitemchg,
      ls_item        LIKE LINE OF lt_item,
      lt_itemcntrl   TYPE TABLE OF   bapiobdlvitemctrlchg,
      ls_itemcntrl   LIKE LINE OF lt_itemcntrl.

IF s_ordno IS NOT INITIAL AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vbeln in s_ordno.
ELSEIF s_vdatu IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vdatu in s_vdatu.
ELSEIF s_erdat is not initial AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat in s_erdat.
ENDIF.
*ELSEIF vdatu IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vdatu = vdatu.
*ELSEIF fromdate IS NOT INITIAL AND todate IS NOT INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat BETWEEN fromdate AND todate.
*ELSEIF fromdate IS NOT INITIAL AND todate IS INITIAL AND ordtype IS NOT INITIAL.
*  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat EQ fromdate ."and todate.
*ENDIF.

IF lt_vbak[] IS INITIAL.
  MESSAGE 'No records found' TYPE 'I'.
  RETURN.
ELSE.
  DESCRIBE TABLE lt_vbak LINES vbak_count.
  WRITE : vbak_count , 'records found.'.
  NEW-LINE.
  LOOP AT lt_vbak INTO ls_vbak.
    lv_seqno = lv_seqno + 1.
*---------------Cancelling Billing Document Number-------------------*

*--------------Reverting PGI against Delivery No---------------------*
*WAIT UP TO 10 SECONDS.
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE * FROM vbfa INTO ls_vbfa WHERE vbelv = ls_vbak-vbeln and vbtyp_n = 'J'.
    SELECT * FROM vbfa INTO TABLE @DATA(lt_x001) UP TO 1 ROWS WHERE vbelv = ls_vbak-vbeln AND vbtyp_n = 'J' ORDER BY vbeln.
    IF sy-subrc eq 0.
      READ TABLE lt_x001 INTO ls_vbfa INDEX 1.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
      lv_postingdate = ls_vbak-vdatu.
    IF sy-subrc = 0.
      lv_deliveryno = ls_vbfa-vbeln.
      lv_vbtyp = 'J'.

      CONCATENATE sy-mandt ls_vbfa-vbeln INTO lv_argument.
*  SET UPDATE TASK LOCAL.
      CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
        EXPORTING
          i_vbeln                   = lv_deliveryno
          i_budat                   = lv_postingdate "sy-datlo
          i_tcode                   = 'VL09'
          i_vbtyp                   = lv_vbtyp
        IMPORTING
          es_emkpf                  = ls_emkpf
        TABLES
          t_mesg                    = lt_mesg
        EXCEPTIONS
          error_reverse_goods_issue = 1
          OTHERS                    = 2.

      IF sy-subrc = 0 AND lt_mesg IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
        CONCATENATE 'Reversal of PGI completed:'
        ls_emkpf-mblnr '/' ls_emkpf-mjahr INTO ls_message.  .
        APPEND ls_message TO lt_message.
        CLEAR: ls_message.

        APPEND ls_message TO lt_message.
        CLEAR: ls_message.
      ELSE.
        ls_message = 'Error while reversing PGI'.
        APPEND ls_message TO lt_message.
        CLEAR: ls_message.

        LOOP AT lt_mesg INTO ls_mesg.
          ls_message = ls_mesg.
          APPEND ls_message TO lt_message.
          CLEAR: ls_message.
        ENDLOOP.

        APPEND ls_message TO lt_message.
        CLEAR: ls_message.
      ENDIF.

*-----------------------Reverting PGI against Delivery No------------*

*------------Deleting Delivery order no against sales order----------*

*--Get Delivery order details--*
*      WAIT UP TO 5 SECONDS.
*      CALL FUNCTION 'BBP_INB_DELIVERY_GETDETAIL'
*        EXPORTING
*          if_delivery            = lv_deliveryno
*        IMPORTING
*          es_inb_delivery_header = ls_del_header
*        TABLES
*          et_inb_delivery_detail = lt_del_item
*          return                 = lt_bapireturn.
*
**--Fill Header Details--*
*      ls_header-deliv_numb       = lv_deliveryno.
*
*      ls_headercntrl-deliv_numb  = lv_deliveryno.
*      ls_headercntrl-dlv_del     = 'X' .
*
**--Fill Item Details--*
*      LOOP AT lt_del_item INTO ls_del_item.
*
*        ls_item-deliv_numb       = ls_del_item-delivery.
*        ls_item-deliv_item       = ls_del_item-deliv_item.
*        ls_item-material         = ls_del_item-material.
*        ls_item-dlv_qty          = ls_del_item-deliv_qty.
*        ls_item-fact_unit_nom    = 1.
*        ls_item-fact_unit_denom  = 1.
*
*        ls_itemcntrl-deliv_numb   = ls_del_item-delivery..
*        ls_itemcntrl-deliv_item   = ls_del_item-deliv_item..
*        ls_itemcntrl-del_item     = 'X'.
*
*        APPEND: ls_itemcntrl TO lt_itemcntrl,
*                ls_item TO lt_item.
*        CLEAR: ls_item,ls_itemcntrl.
*
*      ENDLOOP.

*--Deleting Delivery order no against sales order--*
*      SET UPDATE TASK LOCAL.
*      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*        EXPORTING
*          header_data    = ls_header
*          header_control = ls_headercntrl
*          delivery       = lv_deliveryno
*        TABLES
*          item_data      = lt_item
*          item_control   = lt_itemcntrl
*          return         = lt_bapiret2.

*
*      IF lt_bapiret2 IS INITIAL.
*        COMMIT WORK AND WAIT.
**    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**      EXPORTING
**        wait = 'X'.
*
*        CONCATENATE 'Delivery Order :' lv_deliveryno 'deleted'
*        INTO ls_message SEPARATED BY ''.
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*
*      ELSE.
*
*        ls_message = 'Error while deleting Delivery Order'.
*        APPEND ls_message TO lt_message.
*        CLEAR:ls_message.
*        LOOP AT lt_bapiret2 INTO ls_bapiret2.
*          ls_message = ls_bapiret2-message.
*          APPEND ls_message TO lt_message.
*          CLEAR: ls_message.
*        ENDLOOP.
*      ENDIF.
*--Deleting Delivery order no against sales order--*

    ENDIF.
*-----------Deleting Delivery order no against sales order-----------*
*      IF sy-subrc IS INITIAL.
*            write :'Success',ls_return-message.
*        NEW-LINE.
*        ls_datagenlog-transaction_type = 'DELET'.
*        ls_datagenlog-hseq_no = lv_seqno.
*        ls_datagenlog-iseq_no = lv_seqno.
*        ls_datagenlog-documentno = ls_vbfa-vbeln.
**            ls_datagenlog-EXTERNAL_DOCUMENT
*        ls_datagenlog-msg = ls_return-message.
*        ls_datagenlog-created_on = sy-datum.
*        ls_datagenlog-created_by = sy-uname.
*        ls_datagenlog-result_flag = 'S'.
*        success_count = success_count + 1.
*        modify zdatagen_logging from ls_datagenlog.
*      ELSE.
*        LOOP AT return INTO ls_return WHERE type = 'E'.
*          WRITE : 'Failed:', ls_return-message.
**        ENDLOOP.
**        failure_count = failure_count + 1.
**      ENDIF.

    CLEAR : ls_vbak,ls_vbfa.
        IF lt_message IS NOT INITIAL.
      LOOP AT lt_message INTO ls_message.
        WRITE: / ls_message.
      ENDLOOP.
    ELSE.
      WRITE : / 'Data reset for SO', ls_vbak-vbeln  .
    ENDIF.
    CLEAR : lt_message.
    NEW-LINE.
  ENDLOOP.
ENDIF.
