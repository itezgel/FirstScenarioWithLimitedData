*&---------------------------------------------------------------------*
*& Report ZUPDATE_DEMOSALES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUPDATE_PREDDATA_ERROR_MULTI.

TABLES : vbak.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_so FOR vbak-vbeln." OBLIGATORY.
SELECT-OPTIONS : s_bstnk FOR vbak-bstnk.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
PARAMETERS : ordtype TYPE auart DEFAULT 'ZCM'.
SELECTION-SCREEN END OF BLOCK block.

data : lt_tab type table of zpredacct_data,
       ls_tab type zpredacct_data,
       lv_count type i VALUE IS INITIAL.
DATA: lt_vbak TYPE TABLE OF vbak WITH HEADER LINE,
      ls_vbak TYPE vbak,
      lv_subrc type subrc.

IF s_so IS NOT INITIAL.  " update based on Sales order
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE vbeln IN s_so.
***log table was not updated due to n/w issue with gloria, when ran for 25th Nov to 2nd dec.. updating log table.
* select * from zpredacct_data into table lt_tab for ALL ENTRIES IN lt_vbak where COLLECT_NO = lt_vbak-BSTNK.
*  DESCRIBE TABLE lt_tab lines lv_count.
*  write : lv_count.
*  NEW-LINE.
LOOP AT lt_vbak into ls_vbak.
  UPDATE zpredacct_data SET STATUS = 'S' DOCSTATUS = '3' OBJECTID = 'Processed Successfully-I065658'
        where COLLECT_NO = ls_vbak-BSTNK ." and ( STATUS NE 'S'  OR STATUS NE 'E') ."and DOCSTATUS NE '3'.
  lv_subrc = sy-subrc.
  lv_count = lv_count + 1.
  write : sy-tabix, 'Order-',ls_vbak-vbeln ,'CustReference-', ls_vbak-BSTNK, 'Subrc-', lv_Subrc.
  clear lv_Subrc.
  NEW-LINE.
ENDLOOP.

NEW-LINE.
write : 'Total records',lv_count.

ELSEIF s_bstnk is not INITIAL.  " Update based on Collect_no
 select * from zpredacct_data into table lt_tab where COLLECT_NO in s_bstnk.
  DESCRIBE TABLE lt_tab lines lv_count.
  write : lv_count. clear : lv_count.
  NEW-LINE.
  LOOP AT lt_tab into ls_tab.
    UPDATE zpredacct_data SET status = 'S' lastchangedate = '' lastchangeuser = ''
    DOCSTATUS = '' OBJECTID = 'Processed Successfully-I065658' VBELN = ''
          WHERE collect_no = ls_tab-collect_no." and status = 'F'." AND ( status NE 'S'  OR status NE 'E') ."and DOCSTATUS NE '3'.
    lv_subrc = sy-subrc.
    lv_count = lv_count + 1.
    WRITE : sy-tabix, 'Order-',ls_vbak-vbeln ,'CustReference-', ls_vbak-bstnk, 'Subrc-', lv_Subrc.
    CLEAR lv_Subrc.
    NEW-LINE.
  ENDLOOP.
  NEW-LINE.
write : 'Total records',lv_count.
ENDIF.
