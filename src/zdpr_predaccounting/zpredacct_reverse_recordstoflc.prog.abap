*&---------------------------------------------------------------------*
*& Report ZPREDACCT_MOVE_RECORDS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpredacct_reverse_recordstoflc.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
  PARAMETERS : frm_date TYPE dats DEFAULT sy-datum  OBLIGATORY,
               to_date  TYPE dats DEFAULT sy-datum OBLIGATORY.

  PARAMETERS : addedon TYPE dats.
  SELECTION-SCREEN SKIP.
  PARAMETERS  : flcstatu TYPE char01 DEFAULT ''.
  SELECTION-SCREEN SKIP.
  PARAMETERS  :deletepa TYPE boolean.
  SELECTION-SCREEN COMMENT /10(2) or.
  PARAMETERS : pastatus TYPE char01 DEFAULT 'S'.
SELECTION-SCREEN END OF BLOCK block.

AT SELECTION-SCREEN OUTPUT.
  or = 'OR'.

 START-OF-SELECTION.

  DATA : lt_pa      TYPE TABLE OF zpredacct_data,
         lt_pa_temp TYPE TABLE OF zpredacct_data,
         ls_pa      TYPE zpredacct_data,
         lt_flc     TYPE TABLE OF zdemo_data,
         ls_flc     TYPE zdemo_data,
         lv_count   TYPE i.
  DATA : lv_flcflag TYPE char01,
         lv_paflag  TYPE char01.
  IF addedon IS NOT INITIAL.
    SELECT * FROM zpredacct_data INTO TABLE lt_pa WHERE dateposted_str BETWEEN frm_date AND to_date.
  ELSE.
    SELECT * FROM zpredacct_data INTO TABLE lt_pa WHERE addedon = addedon AND  dateposted_str BETWEEN frm_date AND to_date.
  ENDIF.

*  IF sy-subrc is INITIAL.
  IF  lt_pa[] IS NOT INITIAL.
    DELETE lt_pa WHERE status EQ 'F' OR status EQ 'S' OR status EQ 'E'.
    IF lt_pa[] IS NOT INITIAL. " move these to FLC back.
      lt_pa_temp[] = lt_pa[].
      IF deletepa EQ 'X'. ""-- update FLC and delete from predacct
        SELECT * FROM zdemo_data INTO TABLE lt_flc FOR ALL ENTRIES IN lt_pa WHERE collect_no EQ lt_pa-collect_no.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_flc INTO ls_flc.
            ls_flc-status = flcstatu. "''.
            MODIFY zdemo_data FROM ls_flc.
            IF sy-subrc IS NOT INITIAL.
              lv_flcflag = 'E'.
            ENDIF.
            CLEAR :ls_pa, ls_flc.
          ENDLOOP.
        ENDIF.
        DELETE zpredacct_data FROM TABLE lt_pa_temp.
        IF sy-subrc IS NOT INITIAL.
          lv_paflag = 'E'.
        ENDIF.
      ELSE.
        SELECT * FROM zdemo_data INTO TABLE lt_flc FOR ALL ENTRIES IN lt_pa WHERE collect_no EQ lt_pa-collect_no.
        IF sy-subrc IS INITIAL.
          LOOP AT lt_flc INTO ls_flc.
            ls_flc-status = flcstatu. "''.
            MODIFY zdemo_data FROM ls_flc.
            IF sy-subrc IS NOT INITIAL.
              lv_flcflag = 'E'.
            ENDIF.
            IF sy-subrc IS INITIAL.
              READ TABLE lt_pa INTO ls_pa WITH KEY collect_no = ls_flc-collect_no.
              IF sy-subrc IS INITIAL.
                ls_pa-status = pastatus. "'S' . " set as reveresed - to success for now.
                ls_pa-objectid = 'Reversed to FLC'.
                MODIFY zpredacct_data FROM ls_pa.
                IF sy-subrc IS NOT INITIAL.
                  lv_paflag = 'E'.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR :ls_pa, ls_flc.
            WRITE : 'Updation and Reversal Complete'.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lv_paflag = 'E'.
        WRITE : 'PredAcct updation might not complete'.
      ELSE.
        WRITE : 'PredAcct updation Complete'.
      ENDIF.
      NEW-LINE.
      IF lv_flcflag = 'E'.
        WRITE : 'FLC reversal might not complete'.
      ELSE.
        WRITE : 'FLC reversal Complete'.
      ENDIF.
    ELSE.
      WRITE : 'No records to be moved'.
    ENDIF.
  ELSE.
    WRITE : 'No records found in PredAcct table for give inputs'.
  ENDIF.

******************* below corrections to be taken care in next refresh dev
*PARAMETERS : frm_date TYPE dats DEFAULT sy-datum  OBLIGATORY,
*             to_date  TYPE dats DEFAULT sy-datum OBLIGATORY.
*PARAMETERS : testmode AS CHECKBOX DEFAULT 'X'.
*
*DATA list_tab TYPE TABLE OF abaplist.
*DATA: starttime TYPE sy-uzeit,
*      endtime   TYPE sy-uzeit.
*
** internal structure to load remote data
*DATA: ls_sales_order TYPE           zdemo_data,
*      lt_sales_order TYPE TABLE OF  zdemo_data,
*      lt_dbupdate_so TYPE TABLE OF zdemo_data,
*      ls_dbupdate_so TYPE zdemo_data.
*
*DATA : norecords_flag   TYPE c LENGTH 1 VALUE '',
*       dbconnect_failed TYPE c LENGTH 1 VALUE ''.
*
*
*AT SELECTION-SCREEN.
*
*  IF frm_date GE sy-datum.
*    MESSAGE 'From date cannot be Current or Future Date' TYPE 'E'.
*  ELSEIF to_date GE sy-datum.
*    MESSAGE 'To date cannot be Current or Future Date' TYPE 'E'.
*  ELSEIF frm_date GT to_date.
*    MESSAGE 'From date cannot be greater than To Date' TYPE 'E'.
*    RETURN.
*  ENDIF.
*
*START-OF-SELECTION.
*
*  IF frm_date IS INITIAL.
*    PERFORM get_period USING currperiod CHANGING prevperiod prevmonthstartdate prevmonthenddate.
*    frm_date = sy-datum.
*  ENDIF.
*  IF to_date IS INITIAL.
*    to_date = sy-datum - 1.
*  ENDIF.
*
********************* Check Material List******************
*  DATA: lv_timestamp      TYPE timestamp,
*        lv_timestamp_end  TYPE timestamp,
*        lv_tzntimestp     TYPE tzntimestp, " timestamp.
*        lv_tzntimestp_end TYPE tzntimestp.
*  CONVERT DATE frm_date TIME '000000'
*                   INTO TIME STAMP lv_timestamp
*                   TIME ZONE 'UTC'.
*  WRITE:lv_timestamp TO lv_tzntimestp.
*  CONVERT DATE to_date TIME '235959'
*                  INTO TIME STAMP lv_timestamp_end
*                  TIME ZONE 'UTC'.
*  WRITE:lv_timestamp_end TO lv_tzntimestp_end.
*
*
**AT SELECTION-SCREEN.
*  DATA : c_starttime TYPE c LENGTH 10.
*  DATA : c_endtime TYPE c LENGTH 10.
*
*  starttime = sy-uzeit.
*  WRITE : starttime TO c_starttime.
*  IF sy-batch NE 'X'.
*    WRITE : 'START TIME' , starttime ."sy-uzeit.
*    NEW-LINE.
*    WRITE: 'Predictive Accounting Data Select and shift started.'. NEW-LINE.
*  ENDIF.
*
**  DATA : lv_date2 TYPE tzntimestp .
**  IF frm_date IS NOT INITIAL.
**    CONCATENATE frm_date '000000' INTO lv_date2.
**  ELSE.
**    CONCATENATE sy-datum '000000' INTO lv_date2.
**  ENDIF.
**
**  SELECT * FROM zdemo_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .
*
*
********* check for predictive accounting scenario ****
**  DATA : lt_zpredacct_data TYPE TABLE OF zpredacct_data,
**         ls_zpredacct_data TYPE zpredacct_data.
*
*  DATA : ls_zpa_percentage TYPE zpa_percentage.
*  DATA : lv_pa_switch TYPE zpaswitch.
**data : ls_zpa_switch type zpa_switch.
*
*  SELECT SINGLE switch INTO lv_pa_switch FROM zpa_switch.
*  IF lv_pa_switch EQ 'X'. "if PA scenario enabled continue.
***calculate the entries to be moved.
**    DATA : lv_currmonth   TYPE monat, "numc. monat - fiscal period.
**           lv_currmonth_1 TYPE monat, "zcurrmonth_1,
**           lv_currmonth_2 TYPE monat, "zcurrmonth_2,
**           lv_currmonth_3 TYPE monat, "zcurrmonth_3,
**           lv_currmonth_4 TYPE monat, "zcurrmonth_4.
**           lv_currmonth_5 TYPE monat, "zcurrmonth_5.
**           lv_currmonth_6 TYPE monat. "zcurrmonth_6.
*****    **** calculate the percentage/no of records to be moved.
*    SELECT SINGLE * FROM zpa_percentage INTO ls_zpa_percentage.
*    IF sy-subrc IS NOT INITIAL.
*      IF sy-batch NE 'X'.
*        MESSAGE 'PredAcccounting Percentage not maintainec in ZPA_PERCENTAGE table' TYPE 'E'.
*        EXIT.
*      ELSE.
*        WRITE : 'PredAcccounting Percentage not maintainec in ZPA_PERCENTAGE table'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
****** fetch current month and prev months. --- (not needed as we are doing weekly logic))) ****
*    DATA : processingdate TYPE datum ."VALUE '20190601'." =  12.06.2019
**    frm_date = '20190601'."'20190501'.
*    processingdate = frm_date. "sy-datum.
*    NEW-LINE.
****    New changes Aug23.
*    DATA : lv_futuremonths TYPE i VALUE 6.
*    DATA : currperiod TYPE c LENGTH 6.
*    DATA : prevperiod TYPE c LENGTH 6.
*    DATA : prevmonthstartdate TYPE dats.
*    DATA : prevmonthenddate TYPE dats.
*    DATA : lv_month_loop   TYPE i,
*           lv_month_loop_c TYPE c LENGTH 3.
*
*    currperiod = processingdate+0(6). "currdate+0(6).
*    WRITE: 'Current Month ', currperiod.
*    NEW-LINE.
*    lv_month_loop = 1.
*    WHILE lv_month_loop LE lv_futuremonths.
*      PERFORM get_prev_period USING currperiod CHANGING prevperiod prevmonthstartdate prevmonthenddate.
**      WRITE:/ 'Current period', currperiod, 'prev period is', prevperiod , 'Start Date', prevmonthstartdate,'End Date',prevmonthenddate.
*      lv_month_loop_c = lv_month_loop.
*      WRITE  'Current +' . WRITE: lv_month_loop_c,'Month' . WRITE : '---' ,prevperiod.
*      WRITE : '---','Start Date', prevmonthstartdate.
*      WRITE : '---','End Date', prevmonthenddate.
*      currperiod = prevperiod.
*      lv_month_loop = lv_month_loop + 1.
*      NEW-LINE.
*    ENDWHILE.
****    ****end of changes
**  ENDIF.  " end of PA switch check
*
******* fetch the total Number of records by calculating first and last day.
*    DATA : lt_zdemodata TYPE TABLE OF zdemo_data,
*           ls_zdemodata TYPE zdemo_data.
*    DATA : lt_zpredacct_data TYPE TABLE OF zpredacct_data,
*           ls_zpredacct_data TYPE zpredacct_data.
*    DATA : lv_total_records_curmonth TYPE i.
*    DATA : lv_records_to_pa TYPE i.
*    DATA : lv_firstdate TYPE d,
*           lv_lastdate  TYPE d.
**    DATA : processingdate TYPE datum ."VALUE '20190601'." =  12.06.2019
*    DATA : lv_month                TYPE monat,
*           lv_month_prev5          TYPE monat,
*           lv_monthpercent_counter TYPE i.
**    frm_date = '20190501'.
**    processingdate = frm_date. "sy-datum.
***** check if entries already exist for movement in current month --- Check for addenON for any day of current month.
*    DATA : lt_zpreddata TYPE TABLE OF zpredacct_data,
*           ls_zpreddata TYPE zpredacct_data,
*           lv_firstday  TYPE d,
*           lv_lastday   TYPE d.
*
*    CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
*      EXPORTING
*        iv_date             = processingdate "sy-datum
*      IMPORTING
*        ev_month_begin_date = lv_firstday
*        ev_month_end_date   = lv_lastday.
*    processingdate = lv_firstday.
*    NEW-LINE.
*    WRITE : 'Processing Date is', processingdate.
*
*    SELECT * FROM zpredacct_data INTO TABLE lt_zpreddata WHERE  addedon BETWEEN lv_firstday AND lv_lastday .
**                and ( dateposted_str BETWEEN lv_firstday AND lv_lastday ).
*    IF sy-subrc IS NOT INITIAL. " records movements NOT DONE for current month
***      CONTINUE.
***    ELSE.
****************  monthly logic***********
*
*******      changes:
*      DATA : forpadate TYPE datum.
*      lv_month_loop = 1. " resetting
*      currperiod = processingdate+0(6).
*      WHILE lv_month_loop LE lv_futuremonths.
*        PERFORM get_prev_period USING currperiod CHANGING prevperiod prevmonthstartdate prevmonthenddate.
*        NEW-LINE. WRITE : 'For month' ,prevperiod.
*        lv_firstdate = prevmonthstartdate.
*        lv_lastdate = prevmonthenddate.
*        forpadate = prevmonthstartdate.
**        CONCATENATE currperiod sy-datum+6(2) INTO forpadate. "processingdate.
*        SELECT * FROM zdemo_data INTO TABLE lt_zdemodata WHERE ( dateposted_str BETWEEN lv_firstdate AND lv_lastdate ).
**        " and  status NE 'P'.
*        DESCRIBE TABLE lt_zdemodata LINES lv_total_records_curmonth.
**    NEW-LINE.
*        WRITE : lv_total_records_curmonth, 'records found' , 'for' , forpadate ,'month'.
***    lv_month = lv_month + 1.
***    WRITE : 'Percetage variable', lv_monthpercent_counter .
*        DATA : lv_percentage TYPE int1.
*        CLEAR : lv_percentage.
*        CASE  lv_month_loop.
*          WHEN 1.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus1.
*          WHEN 2.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus2.
*          WHEN 3.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus3.
*          WHEN 4.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus4.
*          WHEN 5.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus5.
*          WHEN 6.
*            lv_percentage =  ls_zpa_percentage-currmonth_plus6.
*          WHEN OTHERS.
*        ENDCASE.
*        WRITE : 'Percetage variable', lv_month_loop,'has' ,lv_percentage,'%'.
**        lv_monthpercent_counter  = lv_monthpercent_counter  + 1.
*        CLEAR : lv_records_to_pa.
*        lv_records_to_pa  = ( lv_total_records_curmonth * lv_percentage ) / 100.
*        WRITE : lv_records_to_pa , 'to be moved'.
*
********        check if already records exist for current month.
*        DATA : lt_existrecord_pa              TYPE TABLE OF zpredacct_data,
*               lv_existrecord_count           TYPE i,
*               lv_remaining_recordcount_to_pa TYPE i.
*        DATA : lv_total_new_recs_curmonth TYPE i.
*
*        SELECT * FROM zpredacct_data INTO TABLE lt_existrecord_pa WHERE ( dateposted_str BETWEEN lv_firstdate AND lv_lastdate ).
*        IF sy-subrc IS INITIAL.
*          DESCRIBE TABLE lt_existrecord_pa LINES lv_existrecord_count.
*          NEW-LINE.
*          WRITE : 'Existing records between',lv_firstdate, 'and', lv_lastdate, lv_existrecord_count.
*          lv_remaining_recordcount_to_pa = lv_records_to_pa - lv_existrecord_count.
*        ELSE.
*          lv_remaining_recordcount_to_pa = lv_records_to_pa.
*        ENDIF.
*
**        DATA : lv_moverec_count TYPE i VALUE 0,
*        DATA : lv_index         TYPE sy-index.
*        DELETE lt_zdemodata WHERE status EQ 'S' OR status EQ 'E' OR status EQ 'P'. " to select only unprocessed records.
*        DESCRIBE TABLE lt_zdemodata LINES lv_total_new_recs_curmonth.
*        NEW-LINE.
*        WRITE : lv_remaining_recordcount_to_pa, 'records to be moved'.
******        *****        exception case***
*        IF lv_total_new_recs_curmonth LT lv_remaining_recordcount_to_pa.
*          lv_remaining_recordcount_to_pa = lv_total_new_recs_curmonth.
*        ENDIF.
****        ****end of exception case
*
**        IF lv_remaining_recordcount_to_pa = lv_total_new_recs_curmonth.
**          MOVE-CORRESPONDING lt_zdemodata TO lt_zpredacct_data.
**          ls_zpredacct_data-addedon = frm_date."sy-datum.
**          MODIFY lt_zpredacct_data FROM ls_zpredacct_data TRANSPORTING addedon.
**        ELSE.
*        DO lv_remaining_recordcount_to_pa TIMES. "lv_records_to_pa TIMES.
*          CALL FUNCTION 'RANDOM_I4'
*            EXPORTING
*              rnd_min   = 1
*              rnd_max   = lv_total_new_recs_curmonth "lv_total_records_curmonth
*            IMPORTING
*              rnd_value = lv_index.
**          CALL FUNCTION 'QF05_RANDOM_INTEGER'
**            EXPORTING
**              ran_int_max   = lv_total_new_recs_curmonth
**              ran_int_min   = 1
**            IMPORTING
**              ran_int       = lv_index                " Here you get a random integer
**            EXCEPTIONS
**              invalid_input = 1
**              OTHERS        = 2.
*          READ TABLE lt_zdemodata  INTO ls_zdemodata INDEX lv_index.
*          MOVE-CORRESPONDING ls_zdemodata TO ls_zpredacct_data.
*          ls_zpredacct_data-addedon = frm_date."sy-datum.
*          MODIFY zpredacct_data FROM ls_zpredacct_data.
*          IF sy-subrc IS INITIAL.
*            ls_zdemodata-status = 'P'.
*            MODIFY zdemo_data FROM ls_zdemodata.
*            DELETE lt_zdemodata WHERE collect_no = ls_zdemodata-collect_no.
*            lv_total_new_recs_curmonth = lv_total_new_recs_curmonth - 1.
*          ENDIF.
**      NEW-LINE.WRITE : sy-tabix,lv_index, 'index added'.
*          APPEND ls_zpredacct_data TO lt_zpredacct_data.
*          CLEAR : lv_index, ls_zdemodata, ls_zpredacct_data,forpadate.
*        ENDDO.
**        ENDIF.
*        NEW-LINE.
*        CLEAR: lv_records_to_pa,lv_remaining_recordcount_to_pa,lv_total_new_recs_curmonth.
*        SORT lt_zpredacct_data ASCENDING BY collect_no.
*        DELETE ADJACENT DUPLICATES FROM lt_zpredacct_data  COMPARING collect_no.
*        currperiod = prevperiod.
*        lv_month_loop = lv_month_loop + 1.
*        NEW-LINE.
*        CLEAR : prevperiod, prevmonthstartdate ,prevmonthenddate.
*      ENDWHILE.
*
******      ***end of new changes Aug23
**  ************** END monthly logic***********
*      IF lt_zpredacct_data[] IS NOT INITIAL.
*        ls_zpredacct_data-addedon = frm_date."sy-datum.
*        MODIFY lt_zpredacct_data FROM ls_zpredacct_data TRANSPORTING addedon WHERE addedon IS INITIAL.
*        SORT lt_zpredacct_data ASCENDING BY collect_no.
*        DELETE ADJACENT DUPLICATES FROM lt_zpredacct_data  COMPARING collect_no.
*        MODIFY zpredacct_data FROM TABLE lt_zpredacct_data.
*        IF sy-subrc IS INITIAL.
*          NEW-LINE.WRITE:  'Update to zpredacct_data table successfull'.
*          DATA : lt_update_zdemodata TYPE TABLE OF zdemo_data,
*                 ls_update_zdemodata TYPE zdemo_data.
*          MOVE-CORRESPONDING lt_zpredacct_data TO lt_update_zdemodata.
*          ls_update_zdemodata-status  = 'P'. "set P as sent for predictive records
*          MODIFY lt_update_zdemodata FROM ls_update_zdemodata TRANSPORTING status WHERE status NE 'P'.
*          MODIFY zdemo_data FROM TABLE lt_update_zdemodata.
*          IF sy-subrc IS INITIAL.
*            NEW-LINE.WRITE:  'Update to zdemo_data table successfull'.
*          ENDIF.
*        ENDIF.
*      ENDIF. " insert to zpredacct_data.
*    ELSE.
*      NEW-LINE.
*      WRITE : 'Records already moved for the period from' ,lv_firstday,'to',lv_lastday.
*    ENDIF. " End of check if already moved.
*  ELSE.
*    IF sy-batch NE 'X'.
*      MESSAGE 'PredAcccounting Switch is not ON in ZPA_SWITCH table' TYPE 'E'.
*      EXIT.
*    ELSE.
*      WRITE : 'PredAcccounting Switch is not ON in ZPA_SWITCH table'.
*      EXIT.
*    ENDIF.
*  ENDIF.  " end of PA switch check
***** end of Predivtive acct scenario********
*
*
**  DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E' OR status = 'P'.
**  IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
**    IF sy-batch NE 'X'.
**      WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
**    ENDIF.
**    norecords_flag = 'X'.
***  EXIT.
**  ENDIF.
*
*FORM get_prev_period USING
*      VALUE(l_currperiod) TYPE char6
**                        VALUE(newperiod) TYPE char5
*      CHANGING VALUE(l_newperiod) TYPE char6
*               VALUE(l_prevperiodstartdate) TYPE  dats
*               VALUE(l_prevperiodenddate) TYPE  dats.
*
*  DATA : lv_prevmonth TYPE monat , "LENGTH 2,
*         lv_prevyear  TYPE c LENGTH 4.
*  lv_prevmonth = l_currperiod+4(2) + 1.
*  IF lv_prevmonth GT 12.
*    lv_prevyear = l_currperiod+0(4) + 1.
*    lv_prevmonth = '01'.
*  ELSE.
*    lv_prevyear = l_currperiod+0(4).
*  ENDIF.
*  CONCATENATE lv_prevyear lv_prevmonth INTO l_newperiod.
*  CONCATENATE l_newperiod '01' INTO l_prevperiodstartdate.
*
*  CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
*    EXPORTING
*      iv_date             = l_prevperiodstartdate
*    IMPORTING
*      ev_month_begin_date = l_prevperiodstartdate
*      ev_month_end_date   = l_prevperiodenddate.
*
*ENDFORM.
