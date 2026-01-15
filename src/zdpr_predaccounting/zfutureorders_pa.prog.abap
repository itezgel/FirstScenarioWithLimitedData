*&---------------------------------------------------------------------*
*& Report ZFUTUREORDERS_PA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfutureorders_pa.
PARAMETERS : lv_date1 TYPE dats DEFAULT sy-datum OBLIGATORY,
             lv_date2 TYPE dats DEFAULT sy-datum OBLIGATORY,
             pa_post  TYPE boolean . "OBLIGATORY.

* internal structure to load remote data
DATA: ls_sales_order TYPE zpredacct_data,
      lt_sales_order TYPE TABLE OF  zpredacct_data,
      lt_dbupdate_so TYPE TABLE OF zpredacct_data,
      ls_dbupdate_so TYPE zpredacct_data.

DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.
starttime = sy-uzeit.
WRITE : 'START TIME' , starttime .
NEW-LINE.

AT SELECTION-SCREEN.
*  IF lv_date1 LT sy-datum. "GT sy-datum.
*   MESSAGE 'From date cannot be past Date' TYPE 'E'. "Future Date' TYPE 'E'.
*  ELSEIF lv_date2 LT sy-datum. "GT sy-datum.
*IF lv_date2 LT sy-datum. "GT sy-datum.
*    MESSAGE 'To date cannot be past Date' TYPE 'E'. "Future Date' TYPE 'E'.
*  ELSEIF lv_date1 GT lv_date2.
*    MESSAGE 'From date cannot be greater than To Date' TYPE 'E'.
*    RETURN.
*  ENDIF.

START-OF-SELECTION.

********** Total records to process******************
* HERE we have now all the data we need in the temporary stab table, now we need to loop through the line items and call the necessary programs
* here as a example i just display the data (2 fields)
  DATA: it_sohdr        TYPE  zso_header_tt,
        ls_sohdr        LIKE LINE OF it_sohdr,
        it_soitems      TYPE  zso_items_tt,
        ls_soitems      LIKE LINE OF it_soitems,
        it_temp_soitems TYPE zso_items_tt,
        temp_soitems    LIKE LINE OF it_temp_soitems,
        it_temp_sohdr   TYPE  zso_header_tt,
        temp_sohdr      LIKE LINE OF it_temp_sohdr,
        ot_log          TYPE TABLE OF  zsocreate_log,
        ls_log          TYPE   zsocreate_log,
        lt_mat          TYPE   zso_items_tt_copy,
        ls_mat          LIKE LINE OF lt_mat,
        lt_mat2         TYPE   zso_items_tt_copy,
        ls_mat2         LIKE LINE OF lt_mat2,
        lt_mat3         TYPE   zso_items_tt_copy, " WITH HEADER LINE,
        ls_mat3         LIKE LINE OF lt_mat3,
        ot_log_temp     TYPE TABLE OF  zsocreate_log,
        gt_solog        TYPE TABLE OF  zsocreate_log,
        ot_dblog        TYPE TABLE OF zpa_logging,
        ls_dblog        TYPE zpa_logging,
        msg             TYPE char255.
  DATA : itab_error_records LIKE lt_sales_order,
         lt_error_dbstruc   TYPE TABLE OF zpredacct_data.
  DATA :  ret_falg  TYPE char1.
  DATA : lv_date TYPE tzntimestp .
  DATA : gv_total_records TYPE i.

  TYPES : BEGIN OF ty_material_stock,
            sohdr_seqno       TYPE  int4,
            material          TYPE  matnr,
            qty               TYPE  dzmeng,
            unrestricted_stck TYPE  labst,
            total_stck        TYPE  sum01,
            stck_available    TYPE char1,
          END OF ty_material_stock.

  DATA: gt_mail_stck_issues TYPE TABLE OF ty_material_stock,
        ls_mail_stck_issues TYPE  ty_material_stock.
  DATA : gt_dbupdate_so TYPE TABLE OF zpredacct_data .




*******New logic for future posting only.
  IF pa_post NE 'X'.
    IF sy-batch NE 'X'.
      MESSAGE 'This program is only for future posting for Predaccting Scenario not for regular posting' TYPE 'E'.
      EXIT.
    ELSE.
      WRITE : 'This program is only for future posting for Predaccting Scenario not for regular posting'.
    ENDIF.
  ELSE. " it is for PA Future post
    DATA : ls_zpa_percentage TYPE zpa_percentage.
    DATA : lv_pa_switch TYPE zpaswitch.

    SELECT SINGLE switch INTO lv_pa_switch FROM zpa_switch.
    IF lv_pa_switch EQ 'X'. "if PA scenario enabled continue.
***calculate the entries to be moved.
*      DATA : lv_currmonth   TYPE monat, "numc. monat - fiscal period.
*             lv_currmonth_1 TYPE monat, "zcurrmonth_1,
*             lv_currmonth_2 TYPE monat, "zcurrmonth_2,
*             lv_currmonth_3 TYPE monat, "zcurrmonth_3,
*             lv_currmonth_4 TYPE monat, "zcurrmonth_4.
*             lv_currmonth_5 TYPE monat, "zcurrmonth_5.
*             lv_currmonth_6 TYPE monat. "zcurrmonth_6.

      DATA : lv_futuremonths TYPE i VALUE 6.
      DATA : currperiod TYPE c LENGTH 6.
      DATA : nextperiod TYPE c LENGTH 6.
      DATA : nextmonthstartdate TYPE dats.
      DATA : nextmonthenddate TYPE dats.
      DATA : lv_month_loop   TYPE i,
             lv_month_loop_c TYPE c LENGTH 3.
      DATA : lv_Firsttime_exec TYPE i VALUE 0. " to check if the Future posting for first time.


*****temporary code as workaround *** i065658  check if any unprocessed record in current month.
      DATA : lt_temp_padata TYPE TABLE OF zpredacct_data.
*      DATA : prevperiod TYPE c LENGTH 6.
      DATA: lv_curr_period_start TYPE dats,
            lv_curr_period_end   TYPE dats.
      DATA : lv_flag_currmonth_rec TYPE char01. " to set current month value
      currperiod = sy-datum+0(6).
      CONCATENATE currperiod lv_date1+6(2) INTO lv_curr_period_start.
      CONCATENATE currperiod lv_date2+6(2) INTO lv_curr_period_end.
      SELECT * FROM zpredacct_data INTO TABLE lt_temp_padata WHERE
          dateposted_str BETWEEN lv_curr_period_start AND lv_curr_period_end.
      DELETE lt_temp_padata WHERE status EQ 'F' OR status EQ 'S' OR status EQ 'E'.

      IF lt_temp_padata[] IS NOT INITIAL.
        lv_futuremonths = 7.
        lv_date1 = lv_curr_period_start.
        lv_date2 = lv_curr_period_end.
        currperiod = sy-datum+0(6) - 1.
        lv_flag_currmonth_rec = 'X'.
        WRITE: 'Current Month ', currperiod,'Bcoz open records in current month'.
      ELSE.
        currperiod = sy-datum+0(6).
        WRITE: 'Current Month ', currperiod.
        lv_flag_currmonth_rec = ''.
      ENDIF.
**End of temp code to check if any open records.

***** fetch current month and next months.
**** below line to be ucommented if above lines are deleted**
*      currperiod = sy-datum+0(6)." processingdate+0(6). " since we will always run it for next months only
*      WRITE: 'Current Month ', currperiod.
      NEW-LINE.
      lv_month_loop = 1.
      WHILE lv_month_loop LE lv_futuremonths.
        PERFORM get_next_period USING currperiod CHANGING nextperiod nextmonthstartdate nextmonthenddate.
*      WRITE:/ 'Current period', currperiod, 'Next period is', nextperiod , 'Start Date', nextmonthstartdate,'End Date',nextmonthenddate.
        lv_month_loop_c = lv_month_loop.
        WRITE  'Current +' . WRITE: lv_month_loop_c,'Month' . WRITE : '---' ,nextperiod.
        WRITE : '---','Start Date', nextmonthstartdate.
        WRITE : '---','End Date', nextmonthenddate.
        currperiod = nextperiod.
        lv_month_loop = lv_month_loop + 1.
        NEW-LINE.
      ENDWHILE.
***    ****end of changes
****      check the dates to be posted and get the future records.
      DATA : lv_month                TYPE monat,
             lv_month_next5          TYPE monat,
             lv_monthpercent_counter TYPE i.
      DATA : frmdate TYPE dats,
             todate  TYPE dats.
      DATA : lt_padata_temp TYPE TABLE OF zpredacct_data.
      DATA : lv_total_records TYPE i.
      DATA : lv_monthly_records TYPE i.
      DATA : lv_flag_lastday_frmdate TYPE boolean.
      DATA : lv_flag_lastday_todate TYPE boolean.
      DATA : lv_temp_firstdate TYPE dats,
             lv_temp_lastdate  TYPE dats.

****** check if firsttime execution, so that it picks from Day 1 for Future SO posting
      DATA : ls_first_palogging TYPE zpa_logging,
             ls_first_preddata  TYPE zpredacct_data.
      SELECT SINGLE * FROM zpa_logging INTO ls_first_palogging WHERE transaction_type = 'SO'.
      SELECT SINGLE * FROM zpredacct_data INTO ls_first_preddata WHERE Status = 'F'.
      IF ls_first_palogging IS NOT INITIAL OR ls_first_preddata IS NOT INITIAL. " meaning not first execution
        lv_Firsttime_exec = 0.
      ELSE.
        lv_Firsttime_exec = 1.  " true so include from 1st date.
      ENDIF.
**** end of first time exec check

*****      Check if last day of the month execution.
      CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
        EXPORTING
          iv_date             = lv_date2
        IMPORTING
          ev_month_begin_date = lv_temp_firstdate "tempdate
          ev_month_end_date   = lv_temp_firstdate. " todate. "lv_lastdate.
      IF frmdate EQ lv_temp_firstdate .
        lv_flag_lastday_todate = 'X'.
      ENDIF.
*****      end of check if last day of the month

      DATA : lv_firstdate TYPE d,
             lv_lastdate  TYPE d.
      DATA : processingdate TYPE datum ."VALUE '20190601'." =  12.06.2019
*    frm_date = '20190601'."'20190501'.
      processingdate = lv_date1."sy-datum.
      CLEAR : nextperiod, nextmonthstartdate ,nextmonthenddate.
      lv_month_loop = 1. " resetting
      IF lv_flag_currmonth_rec = 'X'.
        currperiod = processingdate+0(6) - 1.
      ELSE.
        currperiod = processingdate+0(6).
      ENDIF.
*      currperiod = processingdate+0(6).
      WHILE lv_month_loop LE lv_futuremonths.
        PERFORM get_next_period USING currperiod CHANGING nextperiod nextmonthstartdate nextmonthenddate.
        NEW-LINE. WRITE : 'For month' ,nextperiod.
        CONCATENATE nextperiod lv_date1+6(2) INTO frmdate.
        CONCATENATE nextperiod lv_date2+6(2) INTO todate.
        IF lv_Firsttime_exec = 1. " if first time execution, run from date 1
          CONCATENATE frmdate+0(6) '01' INTO frmdate.
*          move nextmonthenddate to todate.
        ENDIF.
        IF lv_flag_lastday_todate EQ 'X'. " INNVALId DATE OR if LT 31 days month.
***get late last day of the month**
          DATA : tempdate TYPE dats.
*          DATA : lv_firstdate TYPE d,
*                 lv_lastdate  TYPE d.
          CONCATENATE todate+0(4) todate+4(2) '01' INTO tempdate . "First day of the month.
          CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
            EXPORTING
              iv_date             = tempdate
            IMPORTING
              ev_month_begin_date = lv_firstdate "tempdate
              ev_month_end_date   = lv_lastdate. " todate. "lv_lastdate.

          IF frmdate = todate.
            frmdate = todate = lv_lastdate.
          ELSE.
            todate = lv_lastdate.
          ENDIF.
        ENDIF.
        SELECT * FROM zpredacct_data INTO TABLE lt_padata_temp WHERE dateposted_str BETWEEN frmdate AND todate.
        IF lt_padata_temp[] IS NOT INITIAL.
*          READ TABLE lt_padata_temp INTO ls_sales_order INDEX 1.
*          APPEND ls_sales_order TO lt_sales_order. " temporary for testing. delete this
          APPEND LINES OF lt_padata_temp TO lt_sales_order. "uncomment this
        ENDIF.
        DESCRIBE TABLE lt_padata_temp LINES lv_monthly_records.
        WRITE : 'Records from', frmdate ,'to', todate, lv_monthly_records.
        CLEAR : lt_padata_temp,frmdate, todate,lv_monthly_records.


        currperiod = nextperiod.
        lv_month_loop = lv_month_loop + 1.
        NEW-LINE.
        CLEAR : nextperiod, nextmonthstartdate ,nextmonthenddate.
      ENDWHILE.  " End of record selection

      IF lt_sales_order[] IS NOT INITIAL.  " Commenting from here
        DELETE lt_sales_order WHERE status EQ 'F' OR status EQ 'S' OR status EQ 'E'.
        SORT lt_sales_order BY dateposted_str ASCENDING.
        DESCRIBE TABLE lt_sales_order LINES lv_total_records.
        NEW-LINE.WRITE : 'Total no. of records to be future posted', lv_total_records.
*********************process remaining records from lt_sales_order
        LOOP AT lt_sales_order INTO ls_sales_order." WHERE dateposted BETWEEN '20150531' AND  '20150630' .
          ls_sohdr-sohdr_seqno = sy-tabix.
          ls_sohdr-date_posted = ls_sales_order-dateposted.
          ls_sohdr-doc_type = ls_sales_order-doc_type.
          ls_sohdr-collect_no = ls_sales_order-collect_no.
          CONDENSE ls_sohdr-collect_no NO-GAPS.
          ls_sohdr-sales_org = ls_sales_order-sales_org.
          ls_sohdr-distrib_channel = ls_sales_order-distr_chan.
          IF ls_sales_order-division EQ 0.
            ls_sohdr-division = '00'.
          ENDIF.

          ls_sohdr-soldto = ls_sales_order-sold_to.
          APPEND ls_sohdr TO it_sohdr.

          ls_soitems-sohdr_seqno = ls_sohdr-sohdr_seqno. "sy-tabix.
          ls_soitems-material = ls_sales_order-material.
          ls_soitems-qty = ls_sales_order-quantity.
          APPEND ls_soitems TO it_soitems.
          CLEAR:  ls_sohdr, ls_soitems.
        ENDLOOP.

        IF it_sohdr[] IS NOT INITIAL AND it_soitems IS NOT INITIAL.
          CALL FUNCTION 'ZCREATESO_DATAGEN1_PA'   "Note that entry to log table already happening inside FM
            IMPORTING
              msg        = msg
            TABLES
              it_sohdr   = it_sohdr
              it_soitems = it_soitems
              ot_log     = ot_log
              ot_dblog   = ot_dblog.
          ot_log_temp[] = ot_log.

*******  capture records that failed to create SO  "" Commented temporariy
*    LOOP AT ot_log_temp INTO ls_log WHERE result_flag = 'E'.
*      APPEND ls_log TO gt_solog.
*      CLEAR ls_log.
*    ENDLOOP.
*    DELETE ot_log_temp WHERE result_flag = 'E'.
*****  ****End of So fail capture*****

**************approach 2 end
*  Udpate Abap system log table
          IF ot_dblog[] IS NOT INITIAL.  " currently only update zpredacct_data table as SO creation being updated in SO FM
*    MODIFY zpa_logging FROM TABLE ot_dblog.
            LOOP AT ot_dblog INTO ls_dblog.
*        INSERT zpa_logging FROM ls_dblog.
              READ TABLE lt_sales_order INTO ls_dbupdate_so WITH KEY collect_no = ls_dblog-collect_no.
*        ls_dbupdate_so-lastchangedate = sy-datum.
*        ls_dbupdate_so-lastchangeuser = sy-uname.
              ls_dbupdate_so-status = 'F'. "Forward/future posting
              ls_dbupdate_so-lastchangedate = sy-datum.
              ls_dbupdate_so-lastchangeuser = sy-uname.
              ls_dbupdate_so-objectid = ls_dblog-documentno.
              ls_dbupdate_so-docstatus = '1'.
              ls_dbupdate_so-vbeln = ls_dblog-documentno.
              MODIFY zpredacct_data FROM ls_dbupdate_so.
            ENDLOOP.
          ENDIF.
***  End of ABAP system log table
        ENDIF.
**************************************End of remaining record processing of lt_sales_order*****************
      ENDIF.  " end of COmment test
    ENDIF. "End of PA Switch Check
  ENDIF. " End of Future post check
****end of date correction for future posting.

*********** Print Total Processing time and End Time*******
  endtime = sy-uzeit.
  NEW-LINE.
  WRITE : 'END TIME' , endtime ."sy-uzeit.
  NEW-LINE.
  DATA : totaltime TYPE sy-uzeit.
  totaltime = endtime - starttime .
  NEW-LINE.
  WRITE : 'Total Time',totaltime.
***************************************End of time print********************
**************Do the mail Sending for the status update***********
  DATA: wa_solog            TYPE zsocreate_log , "GT_SOLOG  TYPE TABLE OF  zsocreate_log,
        wa_mail_stck_issues TYPE ty_material_stock, "gt_mail_stck_issues TYPE TABLE OF ty_material_stock.
        wa_dbupdate_so      TYPE zpredacct_data . "  salesorder_type. " gt_dbupdate_so TYPE TABLE OF salesorder_type.

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
         c_date1      TYPE c LENGTH 10,
         c_date2      TYPE c LENGTH 10,
         c_endtime    TYPE c LENGTH 8.

  DATA : mailflag TYPE char1 VALUE ''. " flag for exception mail list
  .
  WRITE  lv_date1 TO c_date1 .
  WRITE  lv_date2 TO c_date2 .

  WRITE: endtime TO c_endtime.

*wa_receivers-receiver = 'h.a@sap.com'. "'COMM_MAN@mail.cl1.sap.biz'.
*wa_receivers-rec_type = 'U'.
*wa_receivers-com_type = 'INT'.
*APPEND wa_receivers TO it_receivers.
*CLEAR: wa_receivers.
*

  wa_receivers-receiver = sy-uname.                         "'I065658'.
  wa_receivers-rec_type = 'B'.
*  wa_receivers-com_type = 'INT'.  '' = sap pffice internal
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.


  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  CONCATENATE 'SO-Billing Cycle for' c_date1 'to' c_date2  INTO wa_header-obj_descr SEPARATED BY space.
  APPEND 'Hello Team,' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE: 'This automatic email serves to notify that' 'SO-Billing cycle for the records from' c_date1 'to' c_date2 'has been completed.' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  IF gt_solog[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Issues relating to SO Creation' '.' INTO wa_content SEPARATED BY space. "lv_link
    APPEND wa_content TO it_content.
    LOOP AT gt_solog INTO wa_solog.
      CONCATENATE 'SO Creation failed for PO reference' wa_solog-collect_no 'for reasons: ' wa_solog-msg INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  IF gt_mail_stck_issues[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Stock issues:' '.' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    DATA: temp_qty       TYPE char10,
          available_stck TYPE char10.
    LOOP AT gt_mail_stck_issues INTO wa_mail_stck_issues.
      WRITE wa_mail_stck_issues-qty  TO temp_qty.
      WRITE wa_mail_stck_issues-unrestricted_stck TO available_stck.
      CONCATENATE 'Records with material' wa_mail_stck_issues-material 'requires stock' temp_qty "wa_mail_stck_issues-qty
                  'and the available stock is' available_stck  INTO wa_content ." wa_mail_stck_issues-unrestricted_stck total_stck
      APPEND wa_content TO it_content.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  IF gt_dbupdate_so[] IS NOT INITIAL.
    mailflag = 'X'.
    CONCATENATE 'Below are the Icomplete/Erroneous execution' '.' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
    DATA: temp_collect TYPE char40.
    LOOP AT gt_dbupdate_so INTO wa_dbupdate_so.
      WRITE wa_dbupdate_so-collect_no TO temp_collect.
      CONDENSE temp_collect.
      IF wa_dbupdate_so-docstatus EQ '1'.
        CONCATENATE 'Record with PO:' temp_collect 'with material' wa_dbupdate_so-material
        'failed to create Delivery document for SO' wa_dbupdate_so-objectid INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
      ELSEIF wa_dbupdate_so-docstatus EQ '2'.
        CONCATENATE 'Record with PO:' temp_collect 'with material' wa_dbupdate_so-material
        'failed to create Billing document for SO-DeliveryDoc' wa_dbupdate_so-objectid INTO wa_content SEPARATED BY space.
        APPEND wa_content TO it_content.
      ENDIF.
    ENDLOOP.

    APPEND '<br>' TO it_content.
    APPEND '<br>' TO it_content.
  ENDIF.

  DATA : mail_lt_dbupdate_so   LIKE lt_dbupdate_so,
         successful_count      TYPE i,
         successful_count_char TYPE c LENGTH 10,
         tot_rec_char          TYPE c LENGTH 10,
         error_count           TYPE i,
         error_count_char      TYPE c LENGTH 10.

  mail_lt_dbupdate_so[] = lt_dbupdate_so[].
  DELETE mail_lt_dbupdate_so WHERE status NE 'S'.
  DESCRIBE TABLE mail_lt_dbupdate_so LINES successful_count .
  WRITE successful_count TO  successful_count_char.

  error_count = gv_total_records - successful_count.
  WRITE gv_total_records TO tot_rec_char.
  WRITE error_count TO error_count_char.

  IF gt_solog[] IS INITIAL AND gt_mail_stck_issues[] IS INITIAL AND gt_dbupdate_so[] IS INITIAL. "Everything executed Successfully
    CONCATENATE 'Hurray!!'  tot_rec_char 'records executed successfully' INTO wa_content SEPARATED BY space.
    APPEND wa_content TO it_content.
  ELSE.
    mailflag = 'X'.
    IF error_count IS NOT INITIAL AND error_count GT 0.
      CONCATENATE 'Only' successful_count_char 'executed successfully out of total' tot_rec_char 'that were to be processed' INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ELSE.
      CONCATENATE 'Report could not process all the' 'records due to above listed issues.'
                     'Please correct them and re-run the code ZCORPMODEL' INTO wa_content SEPARATED BY space.
      APPEND wa_content TO it_content.
    ENDIF.
  ENDIF.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  CONCATENATE 'Corporate Model execution End time' c_endtime INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.


  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  APPEND 'Regards,' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND 'Workflow System.' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

********Adding recivers list on exceptional/Error cases.
  IF mailflag EQ 'X'.
*******        Adding Somendra for exception mail list.
*  wa_receivers-receiver = 'somendra.sahu@sap.com'.
*  wa_receivers-rec_type = 'U'.
*  wa_receivers-com_type = 'INT'.
*  APPEND wa_receivers TO it_receivers.
*  CLEAR: wa_receivers.
  ENDIF.

******* End of Exception receiverslist***
*sy-uname  = 'I065658'."'WF-BATCH' .
  IF it_receivers[] IS NOT INITIAL.
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data  = wa_header
        document_type  = 'HTM'
        commit_work    = 'X'
      TABLES
        object_content = it_content
        object_para    = it_para
        receivers      = it_receivers.
  ENDIF.

****************End of mail notification************************

FORM get_next_period USING
    VALUE(l_currperiod) TYPE char6
*                        VALUE(newperiod) TYPE char5
    CHANGING VALUE(l_newperiod) TYPE char6
             VALUE(l_nextperiodstartdate) TYPE  dats
             VALUE(l_nextperiodenddate) TYPE  dats.

  DATA : lv_nextmonth TYPE monat , "LENGTH 2,
         lv_nextyear  TYPE c LENGTH 4.
  lv_nextmonth = l_currperiod+4(2) + 1.
  IF lv_nextmonth GT 12.
    lv_nextyear = l_currperiod+0(4) + 1.
    lv_nextmonth = '01'.
  ELSE.
    lv_nextyear = l_currperiod+0(4).
  ENDIF.
  CONCATENATE lv_nextyear lv_nextmonth INTO l_newperiod.
  CONCATENATE l_newperiod '01' INTO l_nextperiodstartdate.

  CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = l_nextperiodstartdate
    IMPORTING
      ev_month_begin_date = l_nextperiodstartdate
      ev_month_end_date   = l_nextperiodenddate.

ENDFORM.
