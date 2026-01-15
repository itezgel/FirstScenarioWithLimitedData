FUNCTION ZCHECK_MATERIAL_STOCK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(MATERIAL_LONG) TYPE  BAPI_MRP_MAT_PARAM-MATERIAL_LONG
*"     VALUE(PLANT) TYPE  BAPI_MRP_MAT_PARAM-PLANT DEFAULT '1710'
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR200
*"     VALUE(UNRESTRICTED_STCK) TYPE  LABST
*"     VALUE(TOTAL_STCK) TYPE  SUM01
*"----------------------------------------------------------------------

*PARAMETERS :MATERIAL TYPE  BAPI_MRP_MAT_PARAM-MATERIAL OBLIGATORY,
*            PLANT TYPE  BAPI_MRP_MAT_PARAM-PLANT OBLIGATORY DEFAULT '1710'.

DATA :MRP_STOCK_DETAIL  TYPE  BAPI_MRP_STOCK_DETAIL,
      RETURN  TYPE  BAPIRET2.

CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
 EXPORTING
*   MATERIAL                = MATERIAL
   PLANT                   = PLANT
*   MRP_AREA                =
*   PLAN_SCENARIO           =
*   SELECTION_RULE          =
*   DISPLAY_FILTER          =
*   PERIOD_INDICATOR        =
*   GET_ITEM_DETAILS        =
*   GET_IND_LINES           = 'X'
*   GET_TOTAL_LINES         =
*   IGNORE_BUFFER           =
*   MATERIAL_EVG            =
   MATERIAL_LONG           = MATERIAL_LONG
 IMPORTING
*   MRP_LIST                =
*   MRP_CONTROL_PARAM       =
   MRP_STOCK_DETAIL        = MRP_STOCK_DETAIL
   RETURN                  = RETURN
* TABLES
*   MRP_ITEMS               =
*   MRP_IND_LINES           =
*   MRP_TOTAL_LINES         =
*   EXTENSIONOUT            =
          .

IF return-type EQ 'S'.
   UNRESTRICTED_STCK = MRP_STOCK_DETAIL-UNRESTRICTED_STCK.
   TOTAL_STCK = MRP_STOCK_DETAIL-TOTAL_STCK.
*    Write : 'UNRESTRICTED_STCK:',MRP_STOCK_DETAIL-UNRESTRICTED_STCK.
*    Write :'TOTAL_STCK:',MRP_STOCK_DETAIL-TOTAL_STCK.
    NEW-LINE.
    CONCATENATE return-type return-MESSAGE  into msg.
*    Write:  return-type ,return-MESSAGE .
    FLAG = 'S'.
ELSEIF return-type = 'E'.
  FLAG = 'E'.
  CONCATENATE return-type return-MESSAGE  into msg.
*  Write:  return-type ,return-MESSAGE.
ENDIF.



ENDFUNCTION.
