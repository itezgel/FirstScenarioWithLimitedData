*&---------------------------------------------------------------------*
*& Report ZPROCURE_EXCEL_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZPROCURE_EXCEL_UPLOAD.
*Report     : ZPROCURE_EXCEL_UPLOAD
*Objective  : Upload Procurement data relating to procurement cycle from Excel into for S4Model Data Generation
*Created On : 04.08.2016
*Developer  : Hifzulla.A (I065658)
*--------------------------------------------------------------------*

*-------------------Types Declerations---------------------------------*
TYPES: BEGIN OF ty_excel,   " structure with field in sequence as in excle
MATNR       TYPE C LENGTH 40,
MTART       TYPE C LENGTH 4,
COMPONENT   TYPE C LENGTH 40,
COMP_QTY_FACTOR  type i,
NET_PRICE   TYPE C LENGTH 30,
VENDOR      TYPE C LENGTH 10,
PLANT       TYPE C LENGTH 4,
STGE_LOC    TYPE C LENGTH 4,
PURCH_ORG   TYPE C LENGTH 4,
COMP_CODE   TYPE C LENGTH 4,
PURCH_GRP   TYPE C LENGTH 3,
DOC_TYPE    TYPE C LENGTH 4,
PMTTERMS    TYPE C LENGTH 4,
       END OF ty_excel.

*-------------------Field Symbol Declartion----------------------------*
FIELD-SYMBOLS <fs> TYPE any.

*--------------Local Variables and Internal Table declerations-----------*
DATA: l_message TYPE char30,
      ls_excel  TYPE ty_excel,
      lt_excel  TYPE TABLE OF ty_excel,
      lt_data   TYPE STANDARD TABLE OF alsmex_tabline,
      ls_data   TYPE alsmex_tabline.

DATA: ls_procure           TYPE ZDATAGEN_PROCURE,
      wa_procure           TYPE ZDATAGEN_PROCURE,
      lt_procure           TYPE TABLE OF ZDATAGEN_PROCURE,
      lt_procure_saved     TYPE TABLE OF ZDATAGEN_PROCURE,
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
      i_begin_row             = 2
      i_end_col               = '0013'
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
  DATA : lv_p type p DECIMALS 9.


  DESCRIBE TABLE lt_excel LINES upload_rec_count.

  LOOP AT lt_excel INTO ls_excel.
ls_procure-mandt            = sy-mandt.
ls_procure-MATNR            = ls_excel-MATNR.
ls_procure-MTART            = ls_excel-MTART .
ls_procure-COMPONENT        = ls_excel-COMPONENT.
ls_procure-COMP_QTY_FACTOR  = ls_excel-COMP_QTY_FACTOR.

replace all OCCURRENCES OF ',' in ls_excel-NET_PRICE WITH '.'.
lv_p = ls_excel-NET_PRICE.

ls_procure-NET_PRICE        = lv_p. "ls_excel-NET_PRICE
ls_procure-VENDOR           = ls_excel-VENDOR         .
ls_procure-PLANT            = ls_excel-PLANT          .
ls_procure-STGE_LOC         = ls_excel-STGE_LOC       .
ls_procure-PURCH_ORG        = ls_excel-PURCH_ORG      .
ls_procure-COMP_CODE        = ls_excel-COMP_CODE      .
ls_procure-PURCH_GRP        = ls_excel-PURCH_GRP      .
ls_procure-DOC_TYPE         = ls_excel-DOC_TYPE       .
ls_procure-PMTTERMS         = ls_excel-PMTTERMS       .

      APPEND ls_procure TO lt_procure.
      rec_count = rec_count + 1.
   CLEAR ls_procure.
  ENDLOOP.

 IF lt_procure[] IS NOT INITIAL.
   sort lt_procure by MATNR COMPONENT ASCENDING.
   INSERT ZDATAGEN_PROCURE FROM TABLE lt_procure.
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

*  wa_receivers-receiver = 'testmail@gmail.com'.
*  wa_receivers-rec_type = 'U'.
*  wa_receivers-com_type = 'INT'.
*  APPEND wa_receivers TO it_receivers.
*  CLEAR: wa_receivers.

  wa_receivers-receiver = sy-uname.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'. '' = sap office internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'Procurent data Uploading on' lv_date ''  INTO wa_header-obj_descr SEPARATED BY space.
  WRITE: wa_header-obj_descr.

  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This email serves to notify that' 'Procurement data uploading was done on ' lv_date '.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  WRITE: wa_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE:  upload_rec_count_char 'recods found in excel and' rec_count_char 'records uploaded to db table successfully.' INTO wa_content SEPARATED BY space.
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

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data  = wa_header
      document_type  = 'HTM'
      commit_work    = 'X'
    TABLES
      object_content = it_content
      object_para    = it_para
      receivers      = it_receivers.
ENDFORM. "**********************End of mail sending************
