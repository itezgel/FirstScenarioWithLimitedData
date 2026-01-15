*&---------------------------------------------------------------------*
*& Report ZFLC_UPLOAD_PRGM_LIST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zflc_upload_prgm_list.


CONSTANTS:gc_x TYPE char1 VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_mdl RADIOBUTTON GROUP rb1  DEFAULT 'X',
               p_mlc RADIOBUTTON GROUP rb1,
               p_ctd RADIOBUTTON GROUP rb1,
               p_gl  RADIOBUTTON GROUP rb1,
               p_sp  RADIOBUTTON GROUP rb1,
               p_cop RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b.


AT SELECTION-SCREEN.

  IF p_mdl = gc_x.
    SUBMIT z_data_load
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_mlc = gc_x.
    SUBMIT zmlc_upload_textfiles
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_ctd = gc_x.
    SUBMIT zdemodata_excel_upload_cons
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_gl = gc_x.
    SUBMIT zglpostings_excel_upload
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_sp = gc_x.
    SUBMIT zserviceprocure_excel_upload
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_cop = gc_x.
    SUBMIT zopex_excel_upload_cons
       VIA SELECTION-SCREEN
       AND RETURN.


  ENDIF.
