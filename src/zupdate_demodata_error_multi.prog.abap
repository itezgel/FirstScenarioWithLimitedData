*&---------------------------------------------------------------------*
*& Report ZUPDATE_DEMOSALES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zupdate_demodata_error_multi.

TABLES : vbak.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_so FOR vbak-vbeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
  PARAMETERS : ordtype TYPE auart DEFAULT 'ZCM'.
SELECTION-SCREEN END OF BLOCK block.

DATA : lt_tab   TYPE TABLE OF zdemo_data,
       ls_tab   TYPE zdemo_data,
       lv_count TYPE i VALUE IS INITIAL.
DATA: lt_vbak  TYPE TABLE OF vbak WITH HEADER LINE,
      ls_vbak  TYPE vbak,
      lv_subrc TYPE subrc.

IF s_so IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE vbeln IN s_so.
ENDIF.

***log table was not updated due to n/w issue with gloria, when ran for 25th Nov to 2nd dec.. updating log table.
* select * from zdemo_data into table lt_tab for ALL ENTRIES IN lt_vbak where COLLECT_NO = lt_vbak-BSTNK.
*  DESCRIBE TABLE lt_tab lines lv_count.
*  write : lv_count.
*  NEW-LINE.
LOOP AT lt_vbak INTO ls_vbak.
* Start-Antigravity made this changes-(15.01.2026)
*   SELECT SINGLE * FROM zdemo_data INTO ls_tab WHERE collect_no = ls_vbak-bstnk.
    SELECT * FROM zdemo_data INTO TABLE @DATA(lt_x001) UP TO 1 ROWS WHERE collect_no = ls_vbak-bstnk ORDER BY dateposted_str.
    IF sy-subrc eq 0.
      READ TABLE lt_x001 INTO ls_tab INDEX 1.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
  IF sy-subrc IS INITIAL.
    IF ls_tab-status NE 'S'.
      UPDATE zdemo_data SET status = 'S' docstatus = '3' objectid = 'Processed Successfully-I065658'
            WHERE collect_no = ls_vbak-bstnk AND status NE 'S' AND docstatus NE '3'.
      lv_subrc = sy-subrc.
      lv_count = lv_count + 1.
      WRITE : sy-tabix, 'Order-',ls_vbak-vbeln ,'CustReference-', ls_vbak-bstnk, 'Subrc-', lv_Subrc.
      CLEAR lv_Subrc.
      NEW-LINE.
    ENDIF.
  ELSE.
    WRITE : 'Error:' , ls_vbak-bstnk,' not found in ZDEMO_DATA'.NEW-LINE.
  ENDIF.
ENDLOOP.

NEW-LINE.
WRITE : 'Total records',lv_count.
