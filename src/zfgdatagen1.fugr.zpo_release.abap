FUNCTION ZPO_RELEASE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PONUM) TYPE  EBELN
*"     VALUE(REL_CODE) TYPE  FRGCO
*"  EXPORTING
*"     VALUE(RET_MSG) TYPE  CHAR255
*"     VALUE(FLAG) TYPE  CHAR1
*"----------------------------------------------------------------------

data : REL_STATUS_NEW TYPE  BAPIMMPARA-REL_STATUS,
        REL_INDICATOR_NEW TYPE  BAPIMMPARA-REL_IND,
        RETURN TYPE TABLE OF   BAPIRETURN ,
        WA_RETURN TYPE BAPIRETURN.

data : lv_FRGKE type FRGKE.

CALL FUNCTION 'MEPO_DOC_INITIALIZE'
          .

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    input         = PONUM
IMPORTING
   OUTPUT        = PONUM.

select single FRGKE into lv_FRGKE from EKKO where EBELN = PONUM.
IF lv_FRGKE = 'B'. " PO Blocked
    CALL FUNCTION 'BAPI_PO_RELEASE'
  EXPORTING
    purchaseorder                = PONUM
    po_rel_code                  = REL_CODE
   USE_EXCEPTIONS               = 'X'
*   NO_COMMIT                    = ' '
IMPORTING
   REL_STATUS_NEW               = REL_STATUS_NEW
   REL_INDICATOR_NEW            = REL_INDICATOR_NEW
TABLES
   RETURN                       = RETURN
EXCEPTIONS
   AUTHORITY_CHECK_FAIL         = 1
   REQUISITION_NOT_FOUND        = 2
   ENQUEUE_FAIL                 = 3
   PREREQUISITE_FAIL            = 4
   RELEASE_ALREADY_POSTED       = 5
   RESPONSIBILITY_FAIL          = 6
   OTHERS                       = 7.

IF sy-subrc is INITIAL AND REL_STATUS_NEW NE REL_CODE.
  CONCATENATE 'SUCCESS:Release. New Status Code' REL_STATUS_NEW  into RET_MSG SEPARATED BY SPACE.
  flag = 'S'.
ELSE.
   READ TABLE RETURN INTO WA_RETURN WITH KEY TYPE = 'E'.
   CONCATENATE WA_RETURN-TYPE WA_RETURN-CODE WA_RETURN-MESSAGE  into RET_MSG SEPARATED BY SPACE.
   flag = 'E'.
ENDIF.
else.
  CONCATENATE 'PO is NOT blocked, So no release carried' RET_MSG  into RET_MSG SEPARATED BY SPACE.
  flag = 'S'.
  ENDIF.






ENDFUNCTION.
