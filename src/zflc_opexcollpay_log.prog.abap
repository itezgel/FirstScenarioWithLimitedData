*&---------------------------------------------------------------------*
*& Report ZFLC_OPEXCOLLPAY_LOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zflc_opexcollpay_log.


CONSTANTS:gc_x TYPE char1 VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_gl   RADIOBUTTON GROUP rb1  DEFAULT 'X',
               p_sp   RADIOBUTTON GROUP rb1,
               p_coll RADIOBUTTON GROUP rb1,
               p_vp   RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b.


AT SELECTION-SCREEN.

  IF p_gl = gc_x.
    SUBMIT zrun_glpostings
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_sp = gc_x.
    SUBMIT zrun_serviceprocurement
       VIA SELECTION-SCREEN
       AND RETURN.
  ELSEIF p_coll = gc_x.
    SUBMIT zcollections_cloud
       VIA SELECTION-SCREEN
       AND RETURN.

  ELSEIF p_vp = gc_x.
    SUBMIT zpayments
       VIA SELECTION-SCREEN
       AND RETURN.

  ENDIF.
