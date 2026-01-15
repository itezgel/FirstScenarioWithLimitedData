*&---------------------------------------------------------------------*
*& Report ZDEMODATA_EXCEL_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDEMODATA_EXCEL_UPLOAD.
*Report     : ZDEMODATA_EXCEL_UPLOAD
*Objective  : Upload demo data from Excel into for S4Model Data Generation
*Created On : 04.08.2016
*Developer  : Hifzulla.A (I065658)
*--------------------------------------------------------------------*

*-------------------Types Declerations---------------------------------*
TYPES: BEGIN OF ty_excel,   " structure with field in sequence as in excle
         dateposted_str TYPE c LENGTH 8,
         doc_type    TYPE c LENGTH 4,
         collect_no  TYPE c LENGTH 35,
         sales_org   TYPE  c LENGTH 4,
         distr_chan  TYPE  c LENGTH 2,
         division    TYPE  c LENGTH 2,
         REQ_DATE_H  TYPE c LENGTH 20,
         material    TYPE  c LENGTH 40,
         quantity    TYPE  c LENGTH 13,
         sold_to     TYPE  c LENGTH 10,
         ship_to     TYPE  c LENGTH 10,
         dateposted TYPE c LENGTH 14,
       END OF ty_excel.

*-------------------Field Symbol Declartion----------------------------*
FIELD-SYMBOLS <fs> TYPE any.

*--------------Local Variables and Internal Table declerations-----------*
DATA: l_message TYPE char30,
      ls_excel  TYPE ty_excel,
      lt_excel  TYPE TABLE OF ty_excel,
      lt_data   TYPE STANDARD TABLE OF alsmex_tabline,
      ls_data   TYPE alsmex_tabline.

DATA: ls_demodata           TYPE zdemo_data,
      wa_demodata           TYPE zdemo_data,
      lt_demodata           TYPE TABLE OF zdemo_data,
      lt_demodata_saved     TYPE TABLE OF zdemo_data,
      rec_count             TYPE i VALUE 0,
      rec_count_char        TYPE c LENGTH 5,
      upload_rec_count      TYPE i VALUE 0,
      upload_rec_count_char TYPE c LENGTH 5,
      lv_sl_no              TYPE int4.

*--------------------Selection Screen Declartion ----------------------*
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
PARAMETERS :  p_fname TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK block.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM f4_help.

START-OF-SELECTION.
  PERFORM get_data.

  IF lt_excel[] IS NOT INITIAL.
    PERFORM save_to_db.
  ENDIF.

  PERFORM send_mail.

FORM f4_help . " Get f4 help to select file
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_fname.
ENDFORM.

FORM get_data .   " Read excel data

  DATA:   gv_index        TYPE i.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = '0001'
      i_begin_row             = 2 "2 with 1st row being column names.
      i_end_col               = '0012'
      i_end_row               = 99999
    TABLES
      intern                  = lt_data
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_data INTO ls_data.
    MOVE ls_data-col TO gv_index.
*--- Assigning the each record to an internal table row
    ASSIGN COMPONENT gv_index OF STRUCTURE ls_excel TO <fs>.
*--- Asigning the field value to a field symbol
    MOVE ls_data-value TO <fs>.
    AT END OF row.
      APPEND ls_excel TO lt_excel.
      CLEAR:ls_excel,
            ls_data.
    ENDAT.
  ENDLOOP.
* Validating Blank excel file
  IF lt_excel[] IS INITIAL.
    MESSAGE TEXT-011 TYPE 'E'.
    RETURN.
  ENDIF.
*
ENDFORM.                    " GET_DATA


FORM save_to_db.
  DATA : lv_collectno TYPE numc10,
         rc           TYPE  inri-returncode.
    DATA: lv_timestamp  TYPE timestamp,
        lv_tzntimestp TYPE tzntimestp.

  DESCRIBE TABLE lt_excel LINES upload_rec_count.

  LOOP AT lt_excel INTO ls_excel.
    ls_demodata-mandt = sy-mandt.
    ls_demodata-collect_no = ls_excel-collect_no.
    ls_demodata-dateposted_str = ls_excel-dateposted_str.
    ls_demodata-doc_type = ls_excel-doc_type.

    ls_demodata-sales_org = ls_excel-sales_org.
    ls_demodata-distr_chan = ls_excel-distr_chan.
    ls_demodata-division = ls_excel-division.
    IF ls_excel-division EQ 0 OR ls_excel-division EQ '00'.
      ls_demodata-division = '00'.
    ENDIF.

      CONVERT DATE ls_demodata-dateposted_str TIME sy-uzeit
                       INTO TIME STAMP lv_timestamp
                       TIME ZONE 'UTC'.
      WRITE:lv_timestamp TO lv_tzntimestp.

    ls_demodata-REQ_DATE_H = lv_timestamp.
    ls_demodata-material = ls_excel-material.
    ls_demodata-quantity = ls_excel-quantity.
    ls_demodata-sold_to = ls_excel-sold_to.
    ls_demodata-ship_to = ls_excel-ship_to.
    ls_demodata-DATEPOSTED = lv_tzntimestp.

      APPEND ls_demodata TO lt_demodata.
      rec_count = rec_count + 1.
   CLEAR ls_demodata.
  ENDLOOP.

 IF lt_demodata[] IS NOT INITIAL.
   sort lt_demodata by collect_no.
   delete ADJACENT DUPLICATES FROM  lt_demodata  COMPARING collect_no.
   sort lt_demoData by dateposted_str.
    INSERT zdemo_data FROM TABLE lt_demodata.
    IF sy-subrc is INITIAL.
      write : rec_count,'records uploaded successfully'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM send_mail.
*  ********send a mail about activity start***********
*** Header
  DATA : wa_header TYPE sodocchgi1.

*** Contents Data
  DATA : it_content   TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0,
         wa_content   TYPE solisti1,
*** Receivers Data
         it_receivers TYPE STANDARD TABLE OF somlreci1 INITIAL SIZE 0,
         wa_receivers TYPE somlreci1,
         it_para      TYPE STANDARD TABLE OF soparai1 INITIAL SIZE 0,
         wa_para      TYPE soparai1,
         lv_date      TYPE c LENGTH 10.
  WRITE sy-datum TO lv_date .

  WRITE : rec_count TO rec_count_char.
  WRITE : upload_rec_count TO upload_rec_count_char.
*  WRITE : vendor_rec_count TO vendor_rec_count_char.

  wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
  wa_receivers-rec_type = 'U'.
  wa_receivers-com_type = 'INT'.
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_receivers-receiver = 'gloria.marcinko@sap.com'.
  wa_receivers-rec_type = 'U'.
  wa_receivers-com_type = 'INT'.
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_receivers-receiver = 'm.gadamsetty@sap.com'.
  wa_receivers-rec_type = 'U'.
  wa_receivers-com_type = 'INT'.
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.


  wa_receivers-receiver = sy-uname.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'. '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'DemoData Uploading on' lv_date ''  INTO wa_header-obj_descr SEPARATED BY space.
  WRITE: wa_header-obj_descr.

  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This email serves to notify that' 'Demo data uploading was done on ' lv_date '.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  WRITE: wa_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE:  upload_rec_count_char 'recods found in excel and' rec_count_char 'records uploaded to db table successfully.' INTO wa_content SEPARATED BY space.
*  vendor_rec_count_char 'records skipped as Vendor service records.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  WRITE: wa_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND 'Regards,' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND 'Workflow System.' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

*  sy-uname  = 'S4MODEL'."'WF-BATCH' .

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data  = wa_header
      document_type  = 'HTM'
      commit_work    = 'X'
    TABLES
      object_content = it_content
      object_para    = it_para
      receivers      = it_receivers.
**********************End of mail sending************
ENDFORM.
