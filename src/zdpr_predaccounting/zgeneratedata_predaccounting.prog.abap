*&---------------------------------------------------------------------*
*& Report ZGENERATEDATA_PREDACCOUNTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgeneratedata_predaccounting.

PARAMETERS : frm_date TYPE dats DEFAULT sy-datum OBLIGATORY,
             to_date  TYPE dats DEFAULT sy-datum OBLIGATORY.

DATA list_tab TYPE TABLE OF abaplist.
DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.

* internal structure to load remote data
DATA: ls_sales_order TYPE  zpredacct_data,
      lt_sales_order TYPE TABLE OF zpredacct_data,
      lt_dbupdate_so TYPE TABLE OF zpredacct_data,
      ls_dbupdate_so TYPE zpredacct_data.
DATA : norecords_flag   TYPE c LENGTH 1 VALUE '',
       dbconnect_failed TYPE c LENGTH 1 VALUE ''.



AT SELECTION-SCREEN.
  IF frm_date GT sy-datum.
    MESSAGE 'From date cannot be Future Date' TYPE 'E'.
  ELSEIF to_date GT sy-datum.
    MESSAGE 'To date cannot be Future Date' TYPE 'E'.
  ELSEIF frm_date GT to_date.
    MESSAGE 'From date cannot be greater than To Date' TYPE 'E'.
    RETURN.
  ENDIF.


START-OF-SELECTION.

  IF frm_date IS INITIAL.
    frm_date = sy-datum.
  ENDIF.
  IF to_date IS INITIAL.
    to_date = sy-datum.
  ENDIF.






******************** Check Material List******************
  DATA: lv_timestamp      TYPE timestamp,
        lv_timestamp_end  TYPE timestamp,
        lv_tzntimestp     TYPE tzntimestp, " timestamp.
        lv_tzntimestp_end TYPE tzntimestp.
  CONVERT DATE frm_date TIME '000000'
                   INTO TIME STAMP lv_timestamp
                   TIME ZONE 'UTC'.
  WRITE:lv_timestamp TO lv_tzntimestp.
  CONVERT DATE to_date TIME '235959'
                  INTO TIME STAMP lv_timestamp_end
                  TIME ZONE 'UTC'.
  WRITE:lv_timestamp_end TO lv_tzntimestp_end.


*AT SELECTION-SCREEN.
  starttime = sy-uzeit.
  WRITE : 'START TIME' , starttime ."sy-uzeit.
  NEW-LINE.
  DATA : c_starttime TYPE c LENGTH 10.
  DATA : c_endtime TYPE c LENGTH 10.
  WRITE : starttime TO c_starttime.


  WRITE: 'Data generation Execution started.'. NEW-LINE.

  DATA : lv_date2 TYPE tzntimestp .
  IF frm_date IS NOT INITIAL.
    CONCATENATE frm_date '000000' INTO lv_date2.
  ELSE.
    CONCATENATE sy-datum '000000' INTO lv_date2.
  ENDIF.
  SELECT * FROM zpredacct_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .

  DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E'.
  IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
    IF sy-batch NE 'X'.
      WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
    ENDIF.
    norecords_flag = 'X'.
*  EXIT.
  ENDIF.

  TYPES: BEGIN OF ty_cust_mat_list,
           sohdr_seqno TYPE  int4,
           material    TYPE  matnr,
           qty         TYPE  dzmeng,
           collect_no  TYPE  bstkd,
           record_date TYPE  dats,
           mtart       TYPE  mtart,
           soldto      TYPE kunnr,
         END OF ty_cust_mat_list.

  DATA: lt_mat  TYPE TABLE OF ty_cust_mat_list,
        ls_mat  LIKE LINE OF lt_mat,
        lt_mat2 TYPE TABLE OF ty_cust_mat_list,
        ls_mat2 LIKE LINE OF lt_mat2,
        lt_mat3 TYPE TABLE OF ty_cust_mat_list,
        ls_mat3 LIKE LINE OF lt_mat3.

  LOOP AT lt_sales_order INTO ls_sales_order .
    ls_mat-sohdr_seqno = sy-tabix.
    ls_mat-material = ls_sales_order-material.
    ls_mat-qty = ls_sales_order-quantity.
    ls_mat-collect_no = ls_sales_order-collect_no.
    ls_mat-record_date = ls_sales_order-dateposted.
    ls_mat-soldto = ls_sales_order-sold_to.
    APPEND ls_mat TO lt_mat.
**********End of add items n materials.
    CLEAR:  ls_mat." ls_sohdr, ls_soitems,

  ENDLOOP.

  SORT lt_mat ASCENDING BY collect_no.
  DELETE ADJACENT DUPLICATES FROM lt_mat COMPARING collect_no.
  SORT lt_mat ASCENDING BY soldto.
  DATA: temp_lt_mat LIKE lt_mat.
  temp_lt_mat[] = lt_mat[]. " for qty per material without date.
*************<<<<<<<<<<<<<<Concatenate Qty for each of the materials for each of the date*************
  IF lt_mat[] IS NOT INITIAL.
    lt_mat2[] = lt_mat[].
    LOOP AT lt_mat INTO ls_mat.
      LOOP AT lt_mat2 INTO ls_mat2 WHERE material = ls_mat-material AND record_date = ls_mat-record_date.
        ls_mat3-sohdr_seqno = ls_mat2-sohdr_seqno.
        ls_mat3-material = ls_mat2-material.
        ls_mat3-qty = ls_mat3-qty + ls_mat2-qty.
        ls_mat3-collect_no = ls_mat2-collect_no." will always fill the last records Collect_no.
        ls_mat3-record_date = ls_mat2-record_date.
        ls_mat3-mtart = ls_mat2-mtart.
        ls_mat3-soldto = ls_mat2-soldto . "ls_sales_order-soldto.
        CLEAR ls_mat2.
      ENDLOOP.
      APPEND ls_mat3 TO lt_mat3.
      CLEAR ls_mat3.
      DELETE lt_mat WHERE material = ls_mat-material AND record_date = ls_mat-record_date." and SOHDR_SEQNO = ls_mat-SOHDR_SEQNO.
      CLEAR ls_mat.
    ENDLOOP.
  ENDIF.

********send a mail about activity start***********
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

*wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
*wa_receivers-rec_type = 'U'.
*wa_receivers-com_type = 'INT'.
*APPEND wa_receivers TO it_receivers.
*CLEAR: wa_receivers.
*

  wa_receivers-receiver = sy-uname.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'. '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'Corporate Data Model Execution for' lv_date ''  INTO wa_header-obj_descr SEPARATED BY space.
  IF sy-batch NE 'X'.
    WRITE: wa_header-obj_descr.
  ENDIF.

  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This email serves to notify that' 'Corporate Data Model Execution is started for the date ' lv_date 'for the following Materials and Quantity' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  IF sy-batch NE 'X'.
    WRITE: wa_content.
  ENDIF.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'Execution Start time' c_starttime INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  IF norecords_flag = 'X'.
    CONCATENATE: 'There are no records left to be processed for ' lv_date INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    endtime = sy-uzeit.
    WRITE endtime TO c_endtime.
    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
    CONCATENATE: 'Execution End time' c_endtime INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
  ENDIF.

  IF dbconnect_failed = 'X'.
    CONCATENATE:  'Unable to connect to Data Repository DB.' 'Please check the connection in DB02 and run the tcode ZCORPMODEL' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    endtime = sy-uzeit.
    WRITE endtime TO c_endtime.
    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
    CONCATENATE: 'Execution End time' c_endtime INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
  ENDIF.

*  DATA : successful_count      TYPE i,
*         successful_count_char TYPE c LENGTH 10,
  DATA:      tot_rec_char          TYPE c LENGTH 10.
*         error_count           TYPE i,
*         error_count_char      TYPE c LENGTH 10.

  DATA: lv_qty TYPE c LENGTH 13,
        lv_row TYPE c LENGTH 3.

  SORT lt_mat3 BY material.
  LOOP AT lt_mat3 INTO ls_mat3.
    lv_qty = ls_mat3-qty.
    lv_row = sy-tabix.
    CONCATENATE  lv_row ' ' ls_mat3-material ' '  lv_qty INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    APPEND '<br>' TO it_content.
  ENDLOOP.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND 'Regards,' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND 'Workflow System.' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

**sy-uname  = 'I065658'."'WF-BATCH' .
*  IF it_receivers[] IS NOT INITIAL.
*    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
*      EXPORTING
*        document_data  = wa_header
*        document_type  = 'HTM'
*        commit_work    = 'X'
*      TABLES
*        object_content = it_content
*        object_para    = it_para
*        receivers      = it_receivers.
*  ENDIF.
**********************End of mail sending************
  IF dbconnect_failed NE 'X' AND norecords_flag NE 'X'.
******Call report one after the others************
    IF frm_date IS NOT INITIAL AND to_date IS NOT INITIAL.
      SUBMIT zprocuretopay_predaccounting  "VIA SELECTION-SCREEN
                 WITH lv_date1 EQ frm_date SIGN 'I'
                 WITH lv_date2 EQ to_date SIGN 'I'
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
      PERFORM clear_memory.

      SUBMIT zproduction_predaccounting  "VIA SELECTION-SCREEN
                   WITH lv_date1 EQ frm_date SIGN 'I'
                   WITH lv_date2 EQ to_date SIGN 'I'
                   EXPORTING LIST TO MEMORY
                   AND RETURN.
      PERFORM clear_memory.

      WAIT UP TO 5 SECONDS. " Sometimes the production is not complete for few materials. Thus waiting
      SUBMIT zordertocash_predaccounting  "VIA SELECTION-SCREEN
                   WITH lv_date1 EQ frm_date SIGN 'I'
                   WITH lv_date2 EQ to_date SIGN 'I'
                   EXPORTING LIST TO MEMORY
                   AND RETURN.
      PERFORM clear_memory.
******      Addded 21feb2019
*      SUBMIT zcheck_and_fix_daily_flc "VIA SELECTION-SCREEN
*                   WITH lv_date1 EQ frm_date SIGN 'I'
*                   WITH lv_date2 EQ to_date SIGN 'I'
*                   EXPORTING LIST TO MEMORY
*                   AND RETURN.
*      PERFORM clear_memory.

    ELSE.
      SUBMIT zprocuretopay_predaccounting  "VIA SELECTION-SCREEN
                 WITH lv_date1 EQ sy-datum SIGN 'I'
                 WITH lv_date2 EQ sy-datum SIGN 'I'
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
      PERFORM clear_memory.

      SUBMIT zproduction_predaccounting " VIA SELECTION-SCREEN
                   WITH lv_date1 EQ sy-datum SIGN 'I'
                   WITH lv_date2 EQ sy-datum SIGN 'I'
                   EXPORTING LIST TO MEMORY
                   AND RETURN.
      PERFORM clear_memory.
      WAIT UP TO 5 SECONDS. " Sometimes the production is not complete for few materials. Thus waiting

      SUBMIT zordertocash_predaccounting  "VIA SELECTION-SCREEN
                   WITH lv_date1 EQ sy-datum SIGN 'I'
                   WITH lv_date2 EQ sy-datum SIGN 'I'
                   EXPORTING LIST TO MEMORY
                   AND RETURN.
      PERFORM clear_memory.
******      Addded 21feb2019
*      SUBMIT zcheck_and_fix_daily_flc "VIA SELECTION-SCREEN
*                   WITH lv_date1 EQ sy-datum SIGN 'I'
*                   WITH lv_date2 EQ sy-datum SIGN 'I'
*                   EXPORTING LIST TO MEMORY
*                   AND RETURN.
*      PERFORM clear_memory.
    ENDIF.

*CALL FUNCTION 'LIST_FROM_MEMORY'
*  TABLES
*    listobject = list_tab
*  EXCEPTIONS
*    not_found  = 1
*    OTHERS     = 2.
*
*IF sy-subrc = 0.
*  CALL FUNCTION 'WRITE_LIST'
*    TABLES
*      listobject = list_tab.
*ENDIF.

  ELSE.
    EXIT.
  ENDIF.

********** Print Total Processing time and End Time*******
  endtime = sy-uzeit.
  NEW-LINE.
  WRITE : 'END TIME' , endtime ."sy-uzeit.
  NEW-LINE.
  DATA : totaltime TYPE sy-uzeit.
  totaltime = endtime - starttime .
  NEW-LINE.
  WRITE : 'Total Time',totaltime. NEW-LINE.
***************************************End of time print********************
  WRITE: 'Data generation Execution Complete.'. NEW-LINE.

******************end of submit reports.
FORM clear_memory.
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = list_tab
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
*  CALL FUNCTION 'WRITE_LIST'
*    TABLES
*      listobject = list_tab.

    CALL FUNCTION 'LIST_FREE_MEMORY'
      TABLES
        listobject = list_tab.

  ENDIF.
ENDFORM.
