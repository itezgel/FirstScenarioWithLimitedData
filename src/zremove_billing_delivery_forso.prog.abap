*&---------------------------------------------------------------------*
*& Report ZREMOVE_BILLING_DELIVERY_FORSO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zremove_billing_delivery_forso.

TABLES :  vbak.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
PARAMETERS : ordtype TYPE auart OBLIGATORY DEFAULT 'ZCM'.
*PARAMETERS : ordno TYPE vbeln_va .
*PARAMETERS : vdatu TYPE edatu_vbak.
*PARAMETERS : fromdate TYPE dats,
*             todate   TYPE dats.
SELECT-OPTIONS : s_ordno FOR vbak-vbeln.
SELECT-OPTIONS : s_vdatu FOR vbak-vdatu.
SELECT-OPTIONS : s_erdat FOR vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.


DATA : clearance_tab TYPE TABLE OF abaplist.
CLEAR clearance_tab.
SUBMIT zcancel_billing
              WITH ordtype   EQ ordtype SIGN 'I'
              WITH s_ordno     IN s_ordno " SIGN 'I'
              WITH s_vdatu     IN s_vdatu "SIGN 'I'
              WITH s_erdat  IN s_erdat "SIGN 'I'
*              WITH todate    EQ todate  SIGN 'I'
                   EXPORTING LIST TO MEMORY
               AND RETURN.

CALL FUNCTION 'LIST_FROM_MEMORY'
  TABLES
    listobject = clearance_tab
  EXCEPTIONS
    not_found  = 1
    OTHERS     = 2.

IF sy-subrc = 0.
  CALL FUNCTION 'WRITE_LIST'
    TABLES
      listobject = clearance_tab.
  CLEAR clearance_tab.
ENDIF.

SUBMIT  zreverse_delivery
              WITH ordtype   EQ ordtype SIGN 'I'
              WITH s_ordno     IN s_ordno " SIGN 'I'
              WITH s_vdatu     IN s_vdatu "SIGN 'I'
              WITH s_erdat  IN s_erdat "SIGN 'I'
*              WITH todate    EQ todate  SIGN 'I'
                   EXPORTING LIST TO MEMORY
               AND RETURN.

CALL FUNCTION 'LIST_FROM_MEMORY'
  TABLES
    listobject = clearance_tab
  EXCEPTIONS
    not_found  = 1
    OTHERS     = 2.

IF sy-subrc = 0.
  CALL FUNCTION 'WRITE_LIST'
    TABLES
      listobject = clearance_tab.
ENDIF.
