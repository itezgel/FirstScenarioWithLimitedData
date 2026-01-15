*&---------------------------------------------------------------------*
*& Report ZPREDACCT_MOVE_RECORDS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpredacct_move_records.


PARAMETERS : for_date TYPE dats DEFAULT sy-datum  OBLIGATORY.
*             to_date  TYPE dats DEFAULT sy-datum OBLIGATORY.

DATA list_tab TYPE TABLE OF abaplist.
DATA: starttime TYPE sy-uzeit,
      endtime   TYPE sy-uzeit.

* internal structure to load remote data
DATA: ls_sales_order TYPE           zdemo_data,
      lt_sales_order TYPE TABLE OF  zdemo_data,
      lt_dbupdate_so TYPE TABLE OF zdemo_data,
      ls_dbupdate_so TYPE zdemo_data.

DATA : norecords_flag   TYPE c LENGTH 1 VALUE '',
       dbconnect_failed TYPE c LENGTH 1 VALUE ''.

AT SELECTION-SCREEN.
  IF for_date GT sy-datum.
    MESSAGE 'You cannot run for Future Date' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.

  IF for_date IS INITIAL.
    for_date = sy-datum.
  ENDIF.

******************** Check Material List******************
  DATA: lv_timestamp      TYPE timestamp,
        lv_timestamp_end  TYPE timestamp,
        lv_tzntimestp     TYPE tzntimestp, " timestamp.
        lv_tzntimestp_end TYPE tzntimestp.
  CONVERT DATE for_date TIME '000000'
                   INTO TIME STAMP lv_timestamp
                   TIME ZONE 'UTC'.
  WRITE:lv_timestamp TO lv_tzntimestp.

*AT SELECTION-SCREEN.
  DATA : c_starttime TYPE c LENGTH 10.
  DATA : c_endtime TYPE c LENGTH 10.

  starttime = sy-uzeit.
  WRITE : starttime TO c_starttime.
  IF sy-batch NE 'X'.
    WRITE : 'START TIME' , starttime ."sy-uzeit.
    NEW-LINE.
    WRITE: 'Predictive Accounting Data Select and shift started.For execution date'. WRITE for_date. NEW-LINE.
  ENDIF.

******** check for predictive accounting scenario ****
*  DATA : lt_zpredacct_data TYPE TABLE OF zpredacct_data,
*         ls_zpredacct_data TYPE zpredacct_data.

  DATA : ls_zpa_percentage TYPE zpa_percentage.
  DATA : lv_pa_switch TYPE zpaswitch.
  DATA : processingdate TYPE datum .
*data : ls_zpa_switch type zpa_switch.

  DATA : lv_futuremonths TYPE i VALUE 6.
  DATA : currperiod TYPE c LENGTH 6.
  DATA : nextperiod TYPE c LENGTH 6.
  DATA : nextmonthstartdate TYPE dats.
  DATA : nextmonthenddate TYPE dats.
  DATA : lv_month_loop   TYPE i,
         lv_month_loop_c TYPE c LENGTH 3.

  SELECT SINGLE switch INTO lv_pa_switch FROM zpa_switch.
  IF lv_pa_switch EQ 'X'. "if PA scenario enabled continue.
**calculate the entries to be moved.
*    DATA : lv_currmonth   TYPE monat, "numc. monat - fiscal period.
*           lv_currmonth_1 TYPE monat, "zcurrmonth_1,
****    **** calculate the percentage/no of records to be moved.
    SELECT SINGLE * FROM zpa_percentage INTO ls_zpa_percentage.
    IF sy-subrc IS NOT INITIAL.
      IF sy-batch NE 'X'.
        MESSAGE 'PredAcccounting Percentage not maintained in ZPA_PERCENTAGE table' TYPE 'E'.
        EXIT.
      ELSE.
        WRITE : 'PredAcccounting Percentage not maintained in ZPA_PERCENTAGE table'.
        EXIT.
      ENDIF.
    ENDIF.

********  check for day of the month to decide the next month ie., currmonth+1 or currmonth+2
    DATA: lv_flagmovementday TYPE char01.
    IF for_date+6(2) LE 15.
      lv_flagmovementday = ''. " means the movement is with current month +1 as next month(start month)
      currperiod = for_date+0(6) .
      CONCATENATE currperiod for_date+6(2) INTO processingdate.
    ELSEIF for_date+6(2) GT 15 AND for_date+6(2) LE 31.
      lv_flagmovementday = 'X'. " means the movement is with current month +2 as next month(start month)
      currperiod = for_date+0(6) + 1.
      CONCATENATE currperiod for_date+6(2) INTO processingdate.
    ENDIF.
*****************

*check for lv_flagmovementday flag
*    IF lv_flagmovementday = 'X'.
*      currperiod = processingdate+0(6) + 1.
*      CONCATENATE currperiod processingdate+6(2) INTO processingdate.
*    ELSE.
*      currperiod = processingdate+0(6). "currdate+0(6).
*      CONCATENATE currperiod processingdate+6(2) INTO processingdate.
*    ENDIF.
*    currperiod = processingdate+0(6). "currdate+0(6).
    WRITE: 'Current Month ', currperiod.
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
*  ENDIF.  " end of PA switch check

****** fetch the total Number of records by calculating first and last day.
    DATA : lt_zdemodata TYPE TABLE OF zdemo_data,
           ls_zdemodata TYPE zdemo_data.
    DATA : lt_zpredacct_data TYPE TABLE OF zpredacct_data,
           ls_zpredacct_data TYPE zpredacct_data.
    DATA : lv_total_records_curmonth TYPE i.
    DATA : lv_records_to_pa TYPE i.
    DATA : lv_firstdate TYPE d,
           lv_lastdate  TYPE d.
*    DATA : processingdate TYPE datum ."VALUE '20190601'." =  12.06.2019
    DATA : lv_month                TYPE monat,
           lv_month_next5          TYPE monat,
           lv_monthpercent_counter TYPE i.

**** check if entries already exist for movement in current month --- Check for addenON for any day of current month.
    DATA : lt_zpreddata TYPE TABLE OF zpredacct_data,
           ls_zpreddata TYPE zpredacct_data,
           lv_firstday  TYPE d,
           lv_lastday   TYPE d.

    CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = processingdate "sy-datum
      IMPORTING
        ev_month_begin_date = lv_firstday
        ev_month_end_date   = lv_lastday.
    processingdate = lv_firstday.
    NEW-LINE.
    WRITE : 'Processing Date is', processingdate.

    SELECT * FROM zpredacct_data INTO TABLE lt_zpreddata WHERE  addedon BETWEEN lv_firstday AND lv_lastday .
*                and ( dateposted_str BETWEEN lv_firstday AND lv_lastday ).
    IF sy-subrc IS NOT INITIAL. " records movements NOT DONE for current month
**      CONTINUE.
**    ELSE.
***************  monthly logic***********

******      changes:
      DATA : forpadate TYPE datum.
      lv_month_loop = 1. " resetting
      currperiod = processingdate+0(6).
      WHILE lv_month_loop LE lv_futuremonths.
        PERFORM get_next_period USING currperiod CHANGING nextperiod nextmonthstartdate nextmonthenddate.
        NEW-LINE. WRITE : 'For month' ,nextperiod.
        lv_firstdate = nextmonthstartdate.
        lv_lastdate = nextmonthenddate.
        forpadate = nextmonthstartdate.
*        CONCATENATE currperiod sy-datum+6(2) INTO forpadate. "processingdate.
        SELECT * FROM zdemo_data INTO TABLE lt_zdemodata WHERE ( dateposted_str BETWEEN lv_firstdate AND lv_lastdate ).
*        " and  status NE 'P'.
        DESCRIBE TABLE lt_zdemodata LINES lv_total_records_curmonth.
*    NEW-LINE.
        WRITE : lv_total_records_curmonth, 'records found' , 'for' , forpadate ,'month'.
**    lv_month = lv_month + 1.
**    WRITE : 'Percetage variable', lv_monthpercent_counter .
        DATA : lv_percentage TYPE int1.
        CLEAR : lv_percentage.
        CASE  lv_month_loop.
          WHEN 1.
            lv_percentage =  ls_zpa_percentage-currmonth_plus1.
          WHEN 2.
            lv_percentage =  ls_zpa_percentage-currmonth_plus2.
          WHEN 3.
            lv_percentage =  ls_zpa_percentage-currmonth_plus3.
          WHEN 4.
            lv_percentage =  ls_zpa_percentage-currmonth_plus4.
          WHEN 5.
            lv_percentage =  ls_zpa_percentage-currmonth_plus5.
          WHEN 6.
            lv_percentage =  ls_zpa_percentage-currmonth_plus6.
          WHEN OTHERS.
        ENDCASE.
        WRITE : 'Percetage variable', lv_month_loop,'has' ,lv_percentage,'%'.
*        lv_monthpercent_counter  = lv_monthpercent_counter  + 1.
        CLEAR : lv_records_to_pa.
        lv_records_to_pa  = ( lv_total_records_curmonth * lv_percentage ) / 100.
        WRITE : lv_records_to_pa , 'to be moved'.

*******        check if already records exist for current month.
        DATA : lt_existrecord_pa              TYPE TABLE OF zpredacct_data,
               lv_existrecord_count           TYPE i,
               lv_remaining_recordcount_to_pa TYPE i.
        DATA : lv_total_new_recs_curmonth TYPE i.

        SELECT * FROM zpredacct_data INTO TABLE lt_existrecord_pa WHERE ( dateposted_str BETWEEN lv_firstdate AND lv_lastdate ).
        IF sy-subrc IS INITIAL.
          DESCRIBE TABLE lt_existrecord_pa LINES lv_existrecord_count.
          NEW-LINE.
          WRITE : 'Existing records between',lv_firstdate, 'and', lv_lastdate, lv_existrecord_count.
          lv_remaining_recordcount_to_pa = lv_records_to_pa - lv_existrecord_count.
        ELSE.
          lv_remaining_recordcount_to_pa = lv_records_to_pa.
        ENDIF.

*        DATA : lv_moverec_count TYPE i VALUE 0,
        DATA : lv_index         TYPE sy-index.
        DELETE lt_zdemodata WHERE status EQ 'S' OR status EQ 'E' OR status EQ 'P'. " to select only unprocessed records.
        DESCRIBE TABLE lt_zdemodata LINES lv_total_new_recs_curmonth.
        NEW-LINE.
        WRITE : lv_remaining_recordcount_to_pa, 'records to be moved'.
*****        *****        exception case***
        IF lv_total_new_recs_curmonth LT lv_remaining_recordcount_to_pa.
          lv_remaining_recordcount_to_pa = lv_total_new_recs_curmonth.
        ENDIF.
***        ****end of exception case

*        IF lv_remaining_recordcount_to_pa = lv_total_new_recs_curmonth.
*          MOVE-CORRESPONDING lt_zdemodata TO lt_zpredacct_data.
*          ls_zpredacct_data-addedon = for_date."sy-datum.
*          MODIFY lt_zpredacct_data FROM ls_zpredacct_data TRANSPORTING addedon.
*        ELSE.
        DO lv_remaining_recordcount_to_pa TIMES. "lv_records_to_pa TIMES.
          CALL FUNCTION 'RANDOM_I4'
            EXPORTING
              rnd_min   = 1
              rnd_max   = lv_total_new_recs_curmonth "lv_total_records_curmonth
            IMPORTING
              rnd_value = lv_index.
*          CALL FUNCTION 'QF05_RANDOM_INTEGER'
*            EXPORTING
*              ran_int_max   = lv_total_new_recs_curmonth
*              ran_int_min   = 1
*            IMPORTING
*              ran_int       = lv_index                " Here you get a random integer
*            EXCEPTIONS
*              invalid_input = 1
*              OTHERS        = 2.
          READ TABLE lt_zdemodata  INTO ls_zdemodata INDEX lv_index.
          MOVE-CORRESPONDING ls_zdemodata TO ls_zpredacct_data.
          ls_zpredacct_data-addedon = for_date."sy-datum.
****          MODIFY zpredacct_data FROM ls_zpredacct_data. " commented for testing 8th April 2020
          IF sy-subrc IS INITIAL.
            ls_zdemodata-status = 'P'.
*****            MODIFY zdemo_data FROM ls_zdemodata. " commented for testing 8th April 2020
            DELETE lt_zdemodata WHERE collect_no = ls_zdemodata-collect_no.
            lv_total_new_recs_curmonth = lv_total_new_recs_curmonth - 1.
          ENDIF.
*      NEW-LINE.WRITE : sy-tabix,lv_index, 'index added'.
          APPEND ls_zpredacct_data TO lt_zpredacct_data.
          CLEAR : lv_index, ls_zdemodata, ls_zpredacct_data,forpadate.
        ENDDO.
*        ENDIF.
        NEW-LINE.
        CLEAR: lv_records_to_pa,lv_remaining_recordcount_to_pa,lv_total_new_recs_curmonth.
        SORT lt_zpredacct_data ASCENDING BY collect_no.
        DELETE ADJACENT DUPLICATES FROM lt_zpredacct_data  COMPARING collect_no.
        currperiod = nextperiod.
        lv_month_loop = lv_month_loop + 1.
        NEW-LINE.
        CLEAR : nextperiod, nextmonthstartdate ,nextmonthenddate.
      ENDWHILE.

*****      ***end of new changes Aug23
**************  ************** END monthly logic***********  "" commented for testing 8th April 2020
      IF lt_zpredacct_data[] IS NOT INITIAL.
        ls_zpredacct_data-addedon = for_date."sy-datum.
        MODIFY lt_zpredacct_data FROM ls_zpredacct_data TRANSPORTING addedon WHERE addedon IS INITIAL.
        SORT lt_zpredacct_data ASCENDING BY collect_no.
        DELETE ADJACENT DUPLICATES FROM lt_zpredacct_data  COMPARING collect_no.
        MODIFY zpredacct_data FROM TABLE lt_zpredacct_data.
        IF sy-subrc IS INITIAL.
          NEW-LINE.WRITE:  'Update to zpredacct_data table successfull'.
          DATA : lt_update_zdemodata TYPE TABLE OF zdemo_data,
                 ls_update_zdemodata TYPE zdemo_data.
          MOVE-CORRESPONDING lt_zpredacct_data TO lt_update_zdemodata.
          ls_update_zdemodata-status  = 'P'. "set P as sent for predictive records
          MODIFY lt_update_zdemodata FROM ls_update_zdemodata TRANSPORTING status WHERE status NE 'P'.
          MODIFY zdemo_data FROM TABLE lt_update_zdemodata.
          IF sy-subrc IS INITIAL.
            NEW-LINE.WRITE:  'Update to zdemo_data table successfull'.
          ENDIF.
        ENDIF.
      ENDIF. " insert to zpredacct_data.
    ELSE.
      NEW-LINE.
      WRITE : 'Records already moved for the period from' ,lv_firstday,'to',lv_lastday.
    ENDIF. " End of check if already moved.
  ELSE.
    IF sy-batch NE 'X'.
      MESSAGE 'PredAcccounting Switch is not ON in ZPA_SWITCH table' TYPE 'E'.
      EXIT.
    ELSE.
      WRITE : 'PredAcccounting Switch is not ON in ZPA_SWITCH table'.
      EXIT.
    ENDIF.
  ENDIF.  " end of PA switch check
**** end of Predivtive acct scenario********

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
