*&---------------------------------------------------------------------*
*& Report ZSO_RESET_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zso_reset_data.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vbeln TYPE vbak-vbeln .
PARAMETERS : ordtype TYPE auart OBLIGATORY DEFAULT 'ZCM'.
PARAMETERS : vdatu TYPE edatu_vbak.
PARAMETERS : fromdate TYPE dats,
             todate   TYPE dats.
*             createby TYPE sy-uname.
SELECTION-SCREEN END OF BLOCK block.

TABLES : vbak,vbfa.

DATA: lt_vbak TYPE TABLE OF vbak,
      ls_vbak TYPE vbak.
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

DATA: lt_mesg       TYPE TABLE OF mesg,
      ls_mesg       TYPE mesg,
      it_likp       TYPE TABLE OF likp,
      lv_deliveryno LIKE likp-vbeln,
      lv_vbtyp      LIKE likp-vbtyp,
      ls_emkpf      TYPE  emkpf,
      ls_vbfa       TYPE vbfa.

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

DATA : vbak_count    TYPE i,
*       success_count TYPE i VALUE IS INITIAL,
*       failure_count TYPE i VALUE IS INITIAL,
*       lv_fkdat      TYPE fkdat,
       ls_datagenlog TYPE zdatagen_logging,
       lv_seqno      TYPE i.

IF p_vbeln IS NOT INITIAL AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vbeln = p_vbeln.
ELSEIF vdatu IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND vdatu = vdatu.
ELSEIF fromdate IS NOT INITIAL AND todate IS NOT INITIAL AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat BETWEEN fromdate AND todate.
ELSEIF fromdate IS NOT INITIAL AND todate IS INITIAL AND ordtype IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE auart EQ ordtype AND erdat EQ fromdate ."and todate.
ENDIF.

IF lt_vbak[] IS INITIAL.
  MESSAGE 'No records found' TYPE 'I'.
  RETURN.
ELSE.
*  SORT lt_vbak DESCENDING BY vbeln.
*  DESCRIBE TABLE lt_vbak LINES vbak_count.
*  WRITE : vbak_count , 'records found.'.
*  NEW-LINE.
*  LOOP AT lt_vbak INTO ls_vbak.
*    lv_seqno = lv_seqno + 1.
**    *WAIT UP TO 5 SECONDS.
**---------------Cancelling Billing Document Number-------------------*
*    SELECT * FROM vbrp INTO TABLE it_vbrp WHERE aubel = ls_vbak-vbeln .
*
*    SORT it_vbrp DESCENDING BY  vbeln.
*    READ TABLE it_vbrp INTO wa_vbrp INDEX 1.
*    CLEAR lv_bill_doc.
*
*    lv_bill_doc = wa_vbrp-vbeln.
*
*    DATA :xkomfk TYPE TABLE OF  komfk,
*          xkomv  TYPE TABLE OF  komv,
*          xthead TYPE TABLE OF  theadvb,
*          xvbfs  TYPE TABLE OF  vbfs,
*          xvbpa  TYPE TABLE OF  vbpavb,
*          xvbrk  TYPE TABLE OF  vbrkvb,
*          xvbrp  TYPE TABLE OF  vbrpvb,
*          xvbss  TYPE TABLE OF  vbss.
*
*    CALL FUNCTION 'RV_INVOICE_REFRESH'
**     EXPORTING
**       WITH_POSTING       = ' '
**       I_NO_NAST          = ' '
*      TABLES
*        xkomfk = xkomfk
*        xkomv  = xkomv
*        xthead = xthead
*        xvbfs  = xvbfs
*        xvbpa  = xvbpa
*        xvbrk  = xvbrk
*        xvbrp  = xvbrp
*        xvbss  = xvbss.
*
*
**--Cancel Billing doc (Tcode-VF11)--*
**    SET UPDATE TASK LOCAL.
*    CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
*      EXPORTING
*        billingdocument = lv_bill_doc
*      TABLES
*        return          = lt_bapireturn1
*        success         = it_bapivbrksuccess.
**EXIT.
*
*    READ TABLE lt_bapireturn1 TRANSPORTING NO FIELDS WITH KEY type = 'E'.
*    IF sy-subrc <> 0.
*      COMMIT WORK AND WAIT.
**
**  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**    EXPORTING
**      wait = 'X'.
*** exit.
*      CONCATENATE 'Billing document cancelled for the sales order:' ls_vbak-vbeln
*      INTO ls_message SEPARATED BY space.
*
*      APPEND ls_message TO lt_message.
*      CLEAR: ls_message.
*
*    ELSE.
*      CONCATENATE 'Error in canceling billing doc - ' lv_bill_doc ' of Sales order - ' ls_vbak-vbeln INTO ls_message.
*      APPEND ls_message TO lt_message.
*      CLEAR: ls_message.
*
*      LOOP AT lt_bapireturn1 INTO ls_bapireturn1.
*        ls_message = ls_bapireturn1-message.
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*      ENDLOOP.
*
*      APPEND ls_message TO lt_message.
*      CLEAR: ls_message.
*    ENDIF.
*
**---------------Cancelling Billing Document Number-------------------*
*
**--------------Reverting PGI against Delivery No---------------------*
**WAIT UP TO 10 SECONDS.
*    DATA: lv_argument TYPE seqg3-garg,
*          lv_number   TYPE sy-tabix,
*          lv_subrc    TYPE sy-subrc,
*          lt_seqg3    TYPE TABLE OF seqg3.
*
*
*
*    SELECT SINGLE * FROM vbfa INTO ls_vbfa WHERE vbelv = ls_vbak-vbeln AND vbtyp_n = 'J'.
*    IF sy-subrc = 0.
*      lv_deliveryno = ls_vbfa-vbeln.
*      lv_vbtyp = 'J'.
*
*      CONCATENATE sy-mandt ls_vbfa-vbeln INTO lv_argument.
**  SET UPDATE TASK LOCAL.
*      CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
*        EXPORTING
*          i_vbeln                   = lv_deliveryno
*          i_budat                   = sy-datlo
*          i_tcode                   = 'VL09'
*          i_vbtyp                   = lv_vbtyp
*        IMPORTING
*          es_emkpf                  = ls_emkpf
*        TABLES
*          t_mesg                    = lt_mesg
*        EXCEPTIONS
*          error_reverse_goods_issue = 1
*          OTHERS                    = 2.
*
*      IF sy-subrc = 0 AND lt_mesg IS INITIAL.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*        CONCATENATE 'Reversal of PGI completed:'
*        ls_emkpf-mblnr '/' ls_emkpf-mjahr INTO ls_message.  .
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*      ELSE.
*        ls_message = 'Error while reversing PGI'.
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*
*        LOOP AT lt_mesg INTO ls_mesg.
*          ls_message = ls_mesg.
*          APPEND ls_message TO lt_message.
*          CLEAR: ls_message.
*        ENDLOOP.
*
*        APPEND ls_message TO lt_message.
*        CLEAR: ls_message.
*      ENDIF.
*
**-----------------------Reverting PGI against Delivery No------------*
*
**------------Deleting Delivery order no against sales order----------*
*
**--Get Delivery order details--*
*      WAIT UP TO 3 SECONDS.
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
*
**--Deleting Delivery order no against sales order--*
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
**--Deleting Delivery order no against sales order--*
*      CALL FUNCTION 'RV_DELIVERY_INIT'
**   EXPORTING
**     STATUS_BUFF_INIT       =
**     TEXT_MEMORY_INIT       = ' '
**     PP_DELIVERY            = ' '
**     I_CALL_ACTIVITY        = ' '
**     IF_NO_DEQUE            = ' '
*        .
*
*    ENDIF.
**-----------Deleting Delivery order no against sales order-----------*
**------------------------ Display Messages---------------------------*
*    IF lt_message IS NOT INITIAL.
*      LOOP AT lt_message INTO ls_message.
*        WRITE: / ls_message.
*      ENDLOOP.
*    ELSE.
*      WRITE : / 'Data reset for SO', ls_vbak-vbeln  .
*    ENDIF.
*    CLEAR : lt_message.
*    NEW-LINE.
*  ENDLOOP.
ENDIF.
