*&---------------------------------------------------------------------*
*& Report ZCHECK_AND_FIX_DAILY_FLC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCHECK_AND_FIX_DAILY_FLC.

PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum OBLIGATORY,
             lv_date2 TYPE dats DEFAULT sy-datum OBLIGATORY.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime .
NEW-LINE.

AT SELECTION-SCREEN.
  IF lv_date1 GT sy-datum.
    MESSAGE 'From date cannot be Future Date' TYPE 'E'.
  ELSEIF lv_date2 GT sy-datum.
    MESSAGE 'To date cannot be Future Date' TYPE 'E'.
  ELSEIF lv_date1 GT lv_date2.
    MESSAGE 'From date cannot be greater than To Date' TYPE 'E'.
    RETURN.
  ENDIF.

START-OF-SELECTION.

  DATA: lv_timestamp      TYPE timestamp,
        lv_timestamp_end  TYPE timestamp,
        lv_tzntimestp     TYPE tzntimestp,
        lv_tzntimestp_end TYPE tzntimestp.
  CONVERT DATE lv_date1 TIME '000000'
                   INTO TIME STAMP lv_timestamp
                   TIME ZONE 'UTC'.
  WRITE:lv_timestamp TO lv_tzntimestp.
  CONVERT DATE lv_date2 TIME '235959'
                  INTO TIME STAMP lv_timestamp_end
                  TIME ZONE 'UTC'.
  WRITE:lv_timestamp_end TO lv_tzntimestp_end.

  DATA : lt_log TYPE TABLE OF zdatagen_logging,
         ls_log TYPE zdatagen_logging.
  DATA : lv_count TYPE i.

  TYPES: BEGIN OF s_po ,
           ebeln TYPE ekko-ebeln,
         END OF s_po.
  DATA : lt_po TYPE TABLE OF s_po,
         ls_po TYPE s_po.

*  DATA: lt_seltab TYPE TABLE OF rsparams,
*        ls_seltab LIKE LINE OF lt_seltab.
  DATA: range_tab  LIKE RANGE OF ekko-ebeln,
        range_line LIKE LINE OF range_tab.

**** check PO errors*************
  SELECT * FROM zdatagen_logging INTO TABLE lt_log WHERE result_flag = 'E' AND transaction_type LIKE '%PO%' AND
                                            ( date_posted BETWEEN lv_date1 AND lv_date2 )  .
  IF sy-subrc IS INITIAL.
    DESCRIBE TABLE lt_log LINES lv_count.
    WRITE : lv_count,'Error records in PO Cycle'.

    LOOP AT lt_log INTO ls_log.
      ls_po-ebeln = ls_log-external_document.
      APPEND ls_po TO lt_po.

      range_line-sign   = 'I'.
      range_line-option = 'EQ'.
      range_line-low    = ls_log-external_document.
      APPEND range_line TO range_tab.

*        ls_seltab-selname = 's_po'.          " Name of parameter on submitted program
*        ls_seltab-kind    = 'S'.
*        ls_seltab-sign    = 'I'.
*        ls_seltab-option  = 'EQ'.
*        ls_seltab-low     = ls_log-external_document.
**        ls_seltab-high    = so_date-high.
*        APPEND ls_seltab TO lt_seltab.
    ENDLOOP.
    SORT range_tab BY low.
    DELETE ADJACENT DUPLICATES FROM range_tab COMPARING low.
    IF lt_po[] IS NOT INITIAL.
      SUBMIT ztrigger_po_workflow "VIA SELECTION-SCREEN
*                 WITH SELECTION-TABLE lt_seltab
                  WITH s_po IN range_tab
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
      WAIT UP TO 10 SECONDS.

      SUBMIT  zrun_gr_in_for_po_ekpo  "VIA SELECTION-SCREEN
*                 WITH SELECTION-TABLE lt_seltab
                  WITH s_po IN range_tab
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
    ENDIF.
  ENDIF.
  CLEAR : range_tab[], range_line, lt_log[], ls_log,lv_count.
******  *****End of PO records

************* Prod Errors************
  TYPES : BEGIN OF s_prod,
            aufnr TYPE aufk-aufnr,
          END OF s_prod.
  DATA: lt_prod TYPE TABLE OF  s_prod,
        ls_prod TYPE s_prod.
  DATA: prod_range_tab  LIKE RANGE OF  aufk-aufnr,
        prod_range_line LIKE LINE OF range_tab.

  SELECT * FROM zdatagen_logging INTO TABLE lt_log WHERE result_flag = 'E' AND transaction_type LIKE 'PROD' AND
                                            ( date_posted BETWEEN lv_date1 AND lv_date2 )  .

  IF sy-subrc IS INITIAL.
    DESCRIBE TABLE lt_log LINES lv_count.
    WRITE : lv_count,'Error records in SO Cycle'.
    LOOP AT lt_log INTO ls_log.
      ls_prod-aufnr = ls_log-documentno.
      APPEND ls_prod TO lt_prod.

      prod_range_line-sign   = 'I'.
      prod_range_line-option = 'EQ'.
      prod_range_line-low    = ls_log-documentno.
      APPEND prod_range_line TO prod_range_tab.
    ENDLOOP.
    SORT prod_range_tab BY low.
    DELETE ADJACENT DUPLICATES FROM prod_range_tab COMPARING low.
    IF lt_prod[] IS NOT INITIAL.
      SUBMIT  zcomplete_production_cycle  "VIA SELECTION-SCREEN
*                 WITH SELECTION-TABLE lt_seltab
                  WITH s_order IN prod_range_tab
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
    ENDIF.
  ENDIF.
******  *****End of Prod records

************* SO Errors************
  TYPES : BEGIN OF s_so,
            vbeln TYPE vbak-vbeln,
          END OF s_so.
  DATA: lt_so TYPE TABLE OF s_so,
        ls_so TYPE s_so.
  DATA: so_range_tab  LIKE RANGE OF  vbak-vbeln,
        so_range_line LIKE LINE OF range_tab.

  SELECT * FROM zdatagen_logging INTO TABLE lt_log WHERE result_flag = 'E' AND transaction_type LIKE 'SO' AND
                                            ( date_posted BETWEEN lv_date1 AND lv_date2 )  .
  IF sy-subrc IS INITIAL.
    DESCRIBE TABLE lt_log LINES lv_count.
    WRITE : lv_count,'Error records in SO Cycle'.
    LOOP AT lt_log INTO ls_log.
      ls_so-vbeln = ls_log-documentno.
      APPEND ls_so TO lt_so.

      so_range_line-sign   = 'I'.
      so_range_line-option = 'EQ'.
      so_range_line-low    = ls_log-documentno.
      APPEND so_range_line TO so_range_tab.
    ENDLOOP.
    SORT so_range_tab BY low.
    DELETE ADJACENT DUPLICATES FROM so_range_tab COMPARING low.
    IF lt_so[] IS NOT INITIAL.
      SUBMIT  zrun_delivery_billing_for_so  "VIA SELECTION-SCREEN
*                 WITH SELECTION-TABLE lt_seltab
                  WITH s_so IN so_range_tab
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
    ENDIF.
  ENDIF.
****  ******  *****End of SO records
