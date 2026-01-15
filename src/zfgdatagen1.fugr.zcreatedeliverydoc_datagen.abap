FUNCTION ZCREATEDELIVERYDOC_DATAGEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SO_NUM) TYPE  VBAK-VBELN
*"  EXPORTING
*"     VALUE(FLAG) TYPE  SY-SUBRC
*"     VALUE(MSG) TYPE  STRING
*"     VALUE(DELIVERYNUMBER) TYPE  VBELN
*"----------------------------------------------------------------------

DATA : my_msg type string,
        my_check type sy-subrc,
        my_vbeln type VBELN.

  IF SO_NUM is not INITIAL.
    CALL FUNCTION 'ZCREATEDELIVERY_SEK_COPY'
      EXPORTING
        SALESORDERNUMBER = SO_NUM
      IMPORTING
        CHECK            = my_check
        MESSAGE          = my_msg
        DELIVERYNUMBER   = my_vbeln.

    IF sy-subrc is INITIAL AND my_vbeln is not INITIAL.
      FLAG                  =  my_check.
      MSG                   =  my_msg.
      DELIVERYNUMBER        =  my_vbeln.
    elseif my_vbeln is INITIAL.
      FLAG                  =  my_check.
      MSG                   =  my_msg.
    ENDIF.

  else.
    concatenate MSG 'Please provide a Sales Order Number' into MSG SEPARATED BY SPACE.
  ENDIF.
ENDFUNCTION.
