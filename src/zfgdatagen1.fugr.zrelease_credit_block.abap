FUNCTION zrelease_credit_block.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  VBAK-VBELN
*"  EXPORTING
*"     VALUE(SUBRC) TYPE  SUBRC
*"----------------------------------------------------------------------


  CALL FUNCTION 'SD_ORDER_CREDIT_STATUS_INFO'
    EXPORTING
      i_vbeln                 = vbeln
      i_credit_status         = 'X'
      i_requirements          = 'X'
    EXCEPTIONS
      no_exception            = 01
      credit_blocked          = 02
      requirements_reset      = 03
      credit_and_requirements = 04.

  IF sy-subrc IS INITIAL.
    subrc = sy-subrc.
  ELSE."IF sy-subrc = '02' OR sy-subrc = 2. " blocked

    CALL FUNCTION 'SD_SALES_DOCUMENT_INIT'
*     EXPORTING
*       STATUS_BUFFER_REFRESH       = 'X'
*       KEEP_LOCK_ENTRIES           = ' '
*       SIMULATION_MODE_BAPI        = ' '
*       CALL_ACTIVE                 = ' '
      .



    CALL FUNCTION 'SD_ORDER_CREDIT_RELEASE'
      EXPORTING
        vbeln = vbeln
*       IF_SYNCHRON       = 'X'
      .
    subrc = sy-subrc.

  ENDIF.



ENDFUNCTION.
