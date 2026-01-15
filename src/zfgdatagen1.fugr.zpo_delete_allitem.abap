FUNCTION zpo_delete_allitem.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PONUM) TYPE  BAPIMEPOHEADER-PO_NUMBER
*"  EXPORTING
*"     VALUE(FLAG) TYPE  CHAR1
*"     VALUE(MSG) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA:
    lv_purchaseorder   TYPE bapimepoheader-po_number,
    ls_poitem          TYPE bapimepoitem,
    lt_poitem          TYPE TABLE OF bapimepoitem INITIAL SIZE 0,
    ls_poitemx         TYPE bapimepoitemx,
    lt_poitemx         TYPE TABLE OF bapimepoitemx INITIAL SIZE 0,
    lt_return          TYPE TABLE OF bapiret2 INITIAL SIZE 0,
    ls_return          TYPE bapiret2,
    flag_remove        TYPE xfeld,
    ls_poschedule      TYPE bapimeposchedule,
    ls_poschedulex     TYPE bapimeposchedulx,
    lt_poschedule      TYPE TABLE OF bapimeposchedule INITIAL SIZE 0,
    lt_poschedulex     TYPE TABLE OF bapimeposchedulx INITIAL SIZE 0,
    lv_lines           TYPE i,
    lt_eket            TYPE TABLE OF eket,
    ls_eket            TYPE eket,
    flag_schedule_line TYPE xfeld,
    lv_text(220)       TYPE c,
    ls_message         TYPE bal_s_msg.

  DATA : ok_flag TYPE xfeld.

  lv_purchaseorder = ponum.
* get number of schedule lines
  SELECT * FROM eket
           INTO TABLE lt_eket
          WHERE ebeln = lv_purchaseorder. "ls_eket-belnr
*  AND ebelp = itmno.
*  DESCRIBE TABLE lt_eket LINES lv_lines.
  IF lt_eket[] IS NOT INITIAL.
*  reset ok_flag and bapi return table
    CLEAR: ok_flag, lt_return.

*   set flag for schedule line
*  flag_schedule_line = 'X'.

*   set input parameters of bapi
    lv_purchaseorder       =  lv_purchaseorder."ls_eket-belnr.
    LOOP AT lt_eket INTO ls_eket.
      lv_purchaseorder       =  ls_eket-ebeln.
      ls_poitem-po_item      =  ls_eket-ebelp.
      ls_poitemx-po_item     =  ls_eket-ebelp.
      ls_poitemx-po_itemx    =  'X'.
      ls_poitem-delete_ind   =  'L'.
      ls_poitemx-delete_ind  =  'X'.
      ls_poitem-no_more_gr   =  'X'.
      ls_poitemx-no_more_gr  =  'X'.
      APPEND ls_poitem TO lt_poitem.
      APPEND ls_poitemx TO lt_poitemx.
    ENDLOOP.
*   call bapi to cancel purchase order schedule line
    SET UPDATE TASK LOCAL.
    CALL FUNCTION 'BAPI_PO_CHANGE'
      EXPORTING
        purchaseorder = lv_purchaseorder
      TABLES
        return        = lt_return
        poitem        = lt_poitem
        poitemx       = lt_poitemx
        poschedule    = lt_poschedule
        poschedulex   = lt_poschedulex.
    COMMIT WORK AND WAIT.
    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    CLEAR msg.
    IF sy-subrc IS INITIAL.
      flag = 'E'.
      LOOP AT lt_return INTO ls_return.
        CONCATENATE 'Error:' ls_return-message INTO msg SEPARATED BY space.
      ENDLOOP.
    ELSE.
      flag = 'S'.
      CONCATENATE  'Success:' ponum 'Set for Deletion'   INTO msg SEPARATED BY space.
    ENDIF.
* get bapi return status
    flag_remove = 'X'.
    LOOP AT lt_return INTO ls_return.
      IF ls_return-type = 'E'.
        CLEAR: flag_remove.
      ENDIF.
    ENDLOOP.
* clear local variables
    CLEAR: lt_poitem, lt_poitemx, lt_poschedule, lt_poschedulex.
    CLEAR: ls_poitem, ls_poitemx, ls_poschedule, ls_poschedulex.
    CLEAR: flag_schedule_line.
    CLEAR: ls_return, ls_message.
  ENDIF.
ENDFUNCTION.
