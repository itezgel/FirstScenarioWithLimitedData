*&---------------------------------------------------------------------*
*& Report ZPREDACCT_DATAGENLOG_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpredacct_datagenlog_report.
DATA : lv_posdate TYPE zpa_logging-date_posted,
       lv_lines   TYPE i,
       lv_po      TYPE zpa_logging-transaction_type. " VALUE '%PO%'.
DATA : it_fcat TYPE slis_t_fieldcat_alv .
DATA : wa_fcat LIKE LINE OF it_fcat .
DATA : lt_po    TYPE RANGE OF ekko-ebeln,
       ls_po    LIKE LINE OF lt_po,
       lt_prod  TYPE RANGE OF aufk-aufnr,
       lt_so    TYPE RANGE OF vbak-vbeln,
       ls_so    LIKE LINE OF lt_so,
       ls_prod  LIKE LINE OF lt_prod,
       lt_poval TYPE TABLE OF zpa_logging.

DATA lt_fun TYPE TABLE OF rseul_fun.
DATA : it_exclude TYPE slis_t_extab,
       wa_exclude TYPE slis_extab.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_date FOR lv_posdate OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
  PARAMETERS : p_po   RADIOBUTTON GROUP rb1  DEFAULT 'X',
               p_prod RADIOBUTTON GROUP rb1,
               p_so   RADIOBUTTON GROUP rb1,
               p_all  RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN .
  IF p_po EQ 'X'.
    lv_po = '%PO%'.
  ELSEIF p_prod EQ 'X'.
    lv_po = 'PROD'.
  ELSEIF p_so EQ 'X'.
    lv_po = 'SO'.
  ELSEIF p_all EQ 'X'.
*    lv_po = ' '.
  ENDIF.


START-OF-SELECTION.
  IF p_all EQ 'X'.
    SELECT * FROM zpa_logging INTO TABLE @DATA(lt_log)
      WHERE date_posted IN @s_date
      AND result_flag EQ 'S'.
    SORT lt_log BY transaction_type documentno external_document ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_log COMPARING transaction_type documentno external_document.
    DESCRIBE TABLE lt_log LINES lv_lines.
  ELSE.
    SELECT * FROM zpa_logging INTO TABLE @lt_log
   WHERE transaction_type LIKE @lv_po
  AND date_posted IN @s_date
  AND result_flag EQ 'E'.
    SORT lt_log BY transaction_type documentno external_document ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_log COMPARING transaction_type documentno external_document.
    DESCRIBE TABLE lt_log LINES lv_lines.

  ENDIF.
  IF lt_log[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name     = sy-repid
        i_internal_tabname = 'LT_LOG'
        i_structure_name   = 'zpa_logging'
      CHANGING
        ct_fieldcat        = it_fcat.

    LOOP AT it_fcat INTO DATA(ls_fcat).
      CASE ls_fcat-fieldname.
        WHEN 'HSEQ_NO' OR 'ISEQ_NO' OR 'CREATED_ON' OR 'CREATED_AT' OR 'CREATED_BY'.
          ls_fcat-no_out = 'X'.
          MODIFY it_fcat FROM ls_fcat.
      ENDCASE.
    ENDLOOP.

    CALL FUNCTION 'RS_CUA_GET_STATUS_FUNCTIONS'
      EXPORTING
        language  = 'E'
        program   = 'ZPREDACCT_DATAGENLOG_REPORT'
        status    = 'STANDARD'
      TABLES
        functions = lt_fun.
    IF p_po EQ 'X'.
      wa_exclude-fcode = 'PROD'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'SO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.
    ELSEIF p_prod EQ 'X'.
      wa_exclude-fcode = 'PO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'SO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'WFPO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

    ELSEIF p_so EQ 'X'.
      wa_exclude-fcode = 'PROD'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'PO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'WFPO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.
    ELSEIF p_all EQ 'X'.
      wa_exclude-fcode = 'PROD'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'PO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'SO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.

      wa_exclude-fcode = 'WFPO'.
      APPEND wa_exclude TO it_exclude.
      CLEAR wa_exclude.
    ENDIF.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'PF_STATUS'
        i_callback_top_of_page   = 'TOP-OF-PAGE' "see FORM
        i_callback_user_command  = 'USER_COMMAND'
        it_fieldcat              = it_fcat
        it_excluding             = it_exclude
      TABLES
        t_outtab                 = lt_log.
  ELSE.
    IF p_all EQ 'X'.
      MESSAGE 'Success Records not Found' TYPE 'S'.
    ELSE.
      MESSAGE 'Hurray!! There are no Errors' TYPE 'S'.
    ENDIF.
  ENDIF.
*  *&---------------------------------------------------------------------*
*&      Form  sub_pf_status
*&---------------------------------------------------------------------*
*  Sub-Routine to Set the PF status
*----------------------------------------------------------------------*

FORM pf_status USING it_exclude TYPE slis_t_extab..
  SET PF-STATUS 'STANDARD' EXCLUDING it_exclude.
ENDFORM.                    "sub_pf_status
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*   Sub-Routine to handle the click on the ALV aoutput
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm    TYPE sy-ucomm
                       rs_selfield TYPE slis_selfield.
  IF r_ucomm EQ 'WFPO'.
    APPEND LINES OF lt_log TO lt_poval.
    DELETE lt_poval WHERE result_flag NE 'E'.
    SORT lt_poval BY external_document ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_poval COMPARING external_document.
    DESCRIBE TABLE lt_poval LINES lv_lines.
    LOOP AT lt_poval INTO DATA(ls_poval).
      ls_po-sign = 'I'.
      ls_po-option = 'EQ'.
      ls_po-low = ls_poval-external_document.
      APPEND ls_po TO lt_po.
      CLEAR ls_po.
    ENDLOOP.
    IF lt_po IS NOT INITIAL.
      SUBMIT ztrigger_po_workflow WITH s_po IN lt_po AND RETURN.
    ENDIF.
  ENDIF.


  IF r_ucomm EQ 'PO'.
    APPEND LINES OF lt_log TO lt_poval.
    DELETE lt_poval WHERE result_flag NE 'E'.
    SORT lt_poval BY external_document ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_poval COMPARING external_document.
    DESCRIBE TABLE lt_poval LINES lv_lines.
    LOOP AT lt_poval INTO ls_poval.
      ls_po-sign = 'I'.
      ls_po-option = 'EQ'.
      ls_po-low = ls_poval-external_document.
      APPEND ls_po TO lt_po.
      CLEAR ls_po.
    ENDLOOP.
    IF lt_po IS NOT INITIAL.
      SUBMIT zrun_gr_in_for_po_ekpo_pa WITH s_po IN lt_po AND RETURN.
    ENDIF.

  ENDIF.

  IF r_ucomm EQ 'PROD'.
    APPEND LINES OF lt_log TO lt_poval.
    DELETE lt_poval WHERE result_flag NE 'E'.
    SORT lt_poval BY documentno ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_poval COMPARING documentno.
    DESCRIBE TABLE lt_poval LINES lv_lines.
    LOOP AT lt_poval INTO ls_poval.
      ls_prod-sign = 'I'.
      ls_prod-option = 'EQ'.
      ls_prod-low = ls_poval-documentno.
      APPEND ls_prod TO lt_prod.
      CLEAR ls_prod.
    ENDLOOP.
    IF lt_prod IS NOT INITIAL.
      SUBMIT zcomplete_production_cycle_pa WITH s_order  IN lt_prod AND RETURN.
    ENDIF.
  ENDIF.

  IF r_ucomm EQ 'SO'.
    APPEND LINES OF lt_log TO lt_poval.
    DELETE lt_poval WHERE result_flag NE 'E'.
    SORT lt_poval BY documentno ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_poval COMPARING documentno.
    DESCRIBE TABLE lt_poval LINES lv_lines.
    LOOP AT lt_poval INTO ls_poval.
      ls_so-sign = 'I'.
      ls_so-option = 'EQ'.
      ls_so-low = ls_poval-documentno.
      APPEND ls_so TO lt_so.
      CLEAR ls_so.
    ENDLOOP.
    IF lt_so IS NOT INITIAL.
      SUBMIT zrun_delivery_billing_for_sopa WITH s_so IN lt_so AND RETURN.
    ENDIF.
  ENDIF.
*  CLEAR : lt_poval.

ENDFORM.  "User_command
FORM top-of-page.

  DATA: t_header      TYPE slis_t_listheader,
        wa_header     TYPE slis_listheader,
        t_line        LIKE wa_header-info,
        ld_linesc(10) TYPE c.

*  *TITLE
  wa_header-typ = 'H'.
  wa_header-info = 'DataGeneration Log Report '.
  APPEND wa_header TO t_header.
  CLEAR wa_header.


  ld_linesc = lv_lines.
  CONCATENATE 'Total No. of Records Selected: ' ld_linesc
  INTO t_line SEPARATED BY space.
  wa_header-typ = 'A'.
  wa_header-info = t_line.
  APPEND wa_header TO t_header.
  CLEAR: wa_header, t_line.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.
