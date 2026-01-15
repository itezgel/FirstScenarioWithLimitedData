FUNCTION zcreatebillingdoc_datagen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DELIVERYDOC) TYPE  VBELN
*"  EXPORTING
*"     VALUE(BILLINGDOCUMENT) TYPE  VBELN
*"     VALUE(MSG) TYPE  STRING
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------


  DATA: it_atg_billing LIKE bapivbrk OCCURS 1 WITH HEADER LINE,
        it_atg_return  LIKE bapiret1 OCCURS 1 WITH HEADER LINE,
        it_atg_success LIKE bapivbrksuccess OCCURS 1 WITH HEADER LINE,
        git_likp       TYPE TABLE OF likp WITH HEADER LINE.


  DATA : my_error TYPE TABLE OF  bapivbrkerrors.
*         bapi_return type bapiret1.

**  ***** remove billing block***
*  DATA : billflag TYPE char1,
*         billmsg  TYPE char255.
*  data: so_num type VBELN_VON.
*  select single VBELV into so_num from vbfa where VBELN = DELIVERYDOC and VBTYP_N = 'J'.
*    IF sy-subrc is NOT INITIAL.
*      CONCATENATE 'Error: ' deliverydoc 'does not exisst' into msg SEPARATED BY space.
*      return.
*    ENDIF.
*  CALL FUNCTION 'ZREMOVE_BILLINGBLOCK'
*    EXPORTING
*      sonum = so_num
*    IMPORTING
*      flag  = billflag
*      msg   = billmsg.
*  IF billflag = 'E'.
**    flag = 2.
*    msg = billmsg.
*    RETURN.
*  ENDIF.
****End of remove billing block****

data :XKOMFK type table of  KOMFK,
      XKOMV type table of  KOMV,
      XTHEAD type table of  THEADVB,
      XVBFS type table of  VBFS,
      XVBPA type table of  VBPAVB,
      XVBRK type table of  VBRKVB,
      XVBRP type table of  VBRPVB,
      XVBSS type table of  VBSS.


  IF deliverydoc IS NOT INITIAL.

    CALL FUNCTION 'RV_INVOICE_REFRESH'
*     EXPORTING
*       WITH_POSTING       = ' '
*       I_NO_NAST          = ' '
      TABLES
        xkomfk             = xkomfk
        xkomv              = xkomv
        xthead             = xthead
        xvbfs              = xvbfs
        xvbpa              = xvbpa
        xvbrk              = xvbrk
        xvbrp              = xvbrp
        xvbss              = xvbss
              .


*  wait UP TO 6 SECONDS.
    it_atg_billing-ref_doc = deliverydoc.
    it_atg_billing-ordbilltyp = 'F2' ."'ZGT2'.
    it_atg_billing-ref_doc_ca = 'J'.
    APPEND it_atg_billing.

    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
      TABLES
        billingdatain = it_atg_billing
        return        = it_atg_return
        errors        = my_error
        success       = it_atg_success.

    IF it_atg_success IS NOT INITIAL AND it_atg_return-type = 'S' .
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
     EXPORTING
       WAIT          = 'X'
*     IMPORTING
*       RETURN        =
              .
      DATA : wa_succ LIKE LINE OF it_atg_success.
      READ TABLE it_atg_success INTO wa_succ INDEX 1.
      billingdocument = wa_succ-bill_doc.

    ELSEIF it_atg_return-type NE 'S'.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id   = it_atg_return-id
          lang = '-D'
          no   = it_atg_return-number
          v1   = it_atg_return-message_v1
          v2   = it_atg_return-message_v2
          v3   = it_atg_return-message_v3
          v4   = it_atg_return-message_v4
        IMPORTING
          msg  = msg.
*       IF SY-SUBRC <> 0.
** Implement suitable error handling here
*       ENDIF.
    ENDIF.
  ELSE.
    CONCATENATE msg 'Please enter a Delivery Document number' INTO msg SEPARATED BY space.
  ENDIF.

ENDFUNCTION.
