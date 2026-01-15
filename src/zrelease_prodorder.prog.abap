*&---------------------------------------------------------------------*
*& Report ZRELEASE_PRODORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelease_prodorder.

PARAMETERS : g_aufnr type aufnr.
DATA: lwa_aufnr  TYPE ord_pre,

      lwa_tca11  TYPE tca11,

      lwa_caufvd TYPE caufvd,

      lt_aufnr   TYPE STANDARD TABLE OF ord_pre.

lwa_aufnr-aufnr = g_aufnr.

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
      aufnr_act      = g_aufnr
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

    ENDIF.



  ENDIF.



ENDIF.
