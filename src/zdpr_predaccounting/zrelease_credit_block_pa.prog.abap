*&---------------------------------------------------------------------*
*& Report ZRELEASE_CREDIT_BLOCK_PA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRELEASE_CREDIT_BLOCK_PA.

TABLES : vbak.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_so FOR vbak-vbeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
PARAMETERS : ordtype TYPE auart DEFAULT 'ZCM'.
SELECTION-SCREEN END OF BLOCK block.

DATA: lt_vbak TYPE TABLE OF vbak,
      ls_vbak TYPE vbak.
DATA : lv_sonum TYPE vbeln.
DATA : release_subrc TYPE subrc.

IF s_so IS NOT INITIAL.
  SELECT * FROM vbak INTO TABLE lt_vbak WHERE vbeln IN s_so.
ENDIF.

IF lt_vbak[] IS INITIAL.
  MESSAGE 'No records found' TYPE 'I'.
  RETURN.
ELSE.
  LOOP AT lt_vbak INTO ls_vbak.
    lv_sonum = ls_vbak-vbeln .
    CALL FUNCTION 'ZRELEASE_CREDIT_BLOCK_PA' DESTINATION 'NONE'
      EXPORTING
        vbeln = lv_sonum
      IMPORTING
        subrc = release_subrc.
    IF release_subrc IS INITIAL.
      WRITE : lv_sonum ,'Customer Credit Block RELEASED'."
    ELSE.
      WRITE : lv_sonum ,'has Customer Credit Block'." INTO msg SEPARATED BY space.
    ENDIF.
    NEW-LINE.
  ENDLOOP.
ENDIF.
