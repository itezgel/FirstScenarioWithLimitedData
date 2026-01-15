*&---------------------------------------------------------------------*
*& Report ZR_OPEN_SO_DELETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_open_so_delete LINE-SIZE 100.




*---Local Type Declaration---*
TYPES: BEGIN OF ty_msg,
         vbeln   TYPE vbeln,
         type    TYPE char1,
         message TYPE bapi_msg,
       END OF ty_msg,

       BEGIN OF ty_salesorder,
         mark    TYPE char1,
         vbeln   TYPE vbeln,
         name1   TYPE name1,
         type    TYPE char1,
         message TYPE bapi_msg,
       END OF ty_salesorder.

*---Local Data Declaration---*
DATA: lv_auart          TYPE vbak-auart,
      lv_vkorg          TYPE vkorg,
      lv_vtweg          TYPE vtweg,
      lv_audat          TYPE audat,
      lv_vbeln          TYPE vbak-vbeln,
      lt_vbmtv          TYPE TABLE OF svbmtv_trvog,
      ls_vbmtv          TYPE  svbmtv_trvog,
      lt_lvbmtv         TYPE TABLE OF  vbmtv,
      lt_message        TYPE TABLE OF ty_msg,
      ls_message        TYPE ty_msg,
      ls_message1       TYPE ty_msg,
      lt_bapiret2       TYPE TABLE OF bapiret2,
      ls_bapiret2       TYPE  bapiret2,
      ls_header_inx     TYPE bapisdh1x,
      ls_header_in      TYPE bapisdh1,
      ls_salesorderhead TYPE bapisdhd,
      lt_salesorder     TYPE TABLE OF ty_salesorder,
      lt_salesorder1    TYPE TABLE OF ty_salesorder,
      ls_salesorder     TYPE ty_salesorder.

*---Selection Screen inputs---*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*PARAMETERS : p_vbeln TYPE vbap-vbeln.
SELECT-OPTIONS:  s_vbeln FOR lv_vbeln. "Sales Order.
*                 s_auart FOR lv_auart DEFAULT 'OR',"Document type
*                 s_vkorg FOR lv_vkorg, ","Sales Org
*                 s_vtweg FOR lv_vtweg,"Distribution Channel
*                 s_audat FOR lv_audat."Document Date
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  CLEAR:ls_salesorder,
      ls_vbmtv,ls_bapiret2,
        ls_header_inx,ls_header_in.

  REFRESH:lt_salesorder1,lt_salesorder1,
          lt_lvbmtv,lt_vbmtv,lt_bapiret2.
**---Retrive all open sales order---*
*  CALL FUNCTION 'SD_SELECT_SALES_DOCUMENTS'
*    EXPORTING
*      iv_trvog = '0'
*      iv_vboff = 'X'
*    TABLES
*      t_vbmtv  = lt_vbmtv
*      lvbmtv   = lt_lvbmtv.
*
*  IF lt_vbmtv IS NOT INITIAL.
**---Filter the data based on selection criteria---*
*        DELETE lt_vbmtv WHERE vbeln NOT IN s_vbeln."NE p_vbeln." NOT IN s_vbeln." OR
**                          auart NOT IN s_auart OR
**                          vkorg NOT IN s_vkorg OR
**                          vtweg NOT IN s_vtweg OR
**                          audat NOT IN s_audat.
*    IF lt_vbmtv IS INITIAL.
*      MESSAGE 'No entry found' TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
*    PERFORM collect_data.
*    PERFORM delete_salesorder TABLES lt_salesorder.
*    LOOP AT lt_salesorder1 into ls_salesorder.
*      write :/ ls_salesorder-vbeln ,ls_salesorder-name1, ls_salesorder-type,ls_salesorder-message.
*    ENDLOOP.
*  ENDIF.
  IF s_vbeln is not INITIAL.
    data : lt_vbak type TABLE OF vbak,
           ls_vbak type vbak.
    SELECT * from vbak into CORRESPONDING FIELDS OF table lt_salesorder where vbeln  in s_vbeln.
     PERFORM delete_salesorder TABLES lt_salesorder.
    LOOP AT lt_salesorder1 into ls_salesorder.
      write :/ ls_salesorder-vbeln ,ls_salesorder-name1, ls_salesorder-type,ls_salesorder-message.
    ENDLOOP.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  COLLECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_data .
  LOOP AT lt_vbmtv INTO ls_vbmtv.
*    ls_salesorder-mark = 'X'. " Marked for deletion
    ls_salesorder-vbeln = ls_vbmtv-vbeln.
    ls_salesorder-name1 = ls_vbmtv-name1.
    APPEND ls_salesorder TO lt_salesorder.
    CLEAR: ls_salesorder.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_SALESORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SALESORDER  text
*----------------------------------------------------------------------*
FORM delete_salesorder  TABLES lt_salesorder STRUCTURE ls_salesorder."
*---Delete Open Sales Order---*
  REFRESH:lt_salesorder1.
  LOOP AT lt_salesorder INTO ls_salesorder."  WHERE mark = 'X'.
    CALL FUNCTION 'BAPI_SALESORDER_GETDETAILBOS'
      EXPORTING
        salesdocument = ls_salesorder-vbeln
        internaluse   = ' '
      IMPORTING
        orderheader   = ls_salesorderhead
      TABLES
        return        = lt_bapiret2.

* Sales organization *
    ls_header_in-sales_org  = ls_salesorderhead-sales_org.
    ls_header_inx-sales_org = 'X'.
* Distribution channel *
    ls_header_in-distr_chan  = ls_salesorderhead-distr_chan.
    ls_header_inx-distr_chan = 'X'.
* Division *
    ls_header_in-division   = ls_salesorderhead-division.
    ls_header_inx-division  = 'X'.
* Setting Deletion flag *
    ls_header_inx-updateflag = 'D'.

*---Call the bapi to delete the sales order---*
    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' " DESTINATION 'NONE'
      EXPORTING
        salesdocument    = ls_salesorder-vbeln
        order_header_in  = ls_header_in
        order_header_inx = ls_header_inx
      TABLES
        return           = lt_bapiret2.
*---check for errors---*
    READ TABLE lt_bapiret2 TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' " DESTINATION 'NONE'
        EXPORTING
          wait = 'X'.
      ls_salesorder-vbeln   =  ls_salesorder-vbeln.
      ls_salesorder-type    =  'S'.
      ls_salesorder-message =  'Sales order Deleted'.
      APPEND ls_salesorder TO lt_salesorder1.
      CLEAR: ls_salesorder.

    ELSE.
      LOOP AT lt_bapiret2 INTO ls_bapiret2.
        ls_salesorder-vbeln   = ls_salesorder-vbeln.
        ls_salesorder-type    = ls_bapiret2-type.
        ls_salesorder-message = ls_bapiret2-message.
        APPEND ls_salesorder TO lt_salesorder1.
        CLEAR: ls_salesorder.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.
