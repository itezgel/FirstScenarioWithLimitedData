*&---------------------------------------------------------------------*
*& Report ZGENERATEDATA_MODELCOMPANY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpredacct_data_date_logic.

PARAMETERS : frm_date TYPE dats DEFAULT sy-datum  OBLIGATORY,
             to_date  TYPE dats DEFAULT sy-datum OBLIGATORY.

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
  DATA : c_starttime TYPE c LENGTH 10.
  DATA : c_endtime TYPE c LENGTH 10.

  starttime = sy-uzeit.
  WRITE : starttime TO c_starttime.
  IF sy-batch NE 'X'.
    WRITE : 'START TIME' , starttime ."sy-uzeit.
    NEW-LINE.
    WRITE: 'Data generation Execution started.'. NEW-LINE.
  ENDIF.

  DATA : lv_date2 TYPE tzntimestp .
  IF frm_date IS NOT INITIAL.
    CONCATENATE frm_date '000000' INTO lv_date2.
  ELSE.
    CONCATENATE sy-datum '000000' INTO lv_date2.
  ENDIF.
  SELECT * FROM zdemo_data INTO TABLE lt_sales_order WHERE dateposted BETWEEN lv_tzntimestp AND lv_tzntimestp_end." EQ lv_date .


******** check for predictive accounting scenario ****
  DATA : lt_zpredacct_data TYPE TABLE OF zpredacct_data,
         ls_zpredacct_data TYPE zpredacct_data.
  DATA : ls_zpa_percentage TYPE zpa_percentage.
  DATA : lv_pa_switch TYPE zpaswitch.
*data : ls_zpa_switch type zpa_switch.

  SELECT SINGLE switch INTO lv_pa_switch FROM zpa_switch.
  IF lv_pa_switch EQ 'X'. "if PA scenario enabled continue.
**calculate the entries to be moved.
    DATA : lv_currmonth   TYPE monat, "numc. monat - fiscal period.
           lv_currmonth_1 TYPE monat, "zcurrmonth_1,
           lv_currmonth_2 TYPE monat, "zcurrmonth_2,
           lv_currmonth_3 TYPE monat, "zcurrmonth_3,
           lv_currmonth_4 TYPE monat, "zcurrmonth_4.
           lv_currmonth_5 TYPE monat. "zcurrmonth_5.

***** fetch current month and next months. --- not needed as we are doing weekly logic****
    NEW-LINE.
    lv_currmonth = sy-datum+4(2).
    WRITE : 'Current Month', lv_currmonth.
    NEW-LINE.
    lv_currmonth_1 = lv_currmonth + 1.
    WRITE : 'Current Month +1', lv_currmonth_1.
    NEW-LINE.
    lv_currmonth_2 = lv_currmonth + 2.
    WRITE : 'Current Month +2', lv_currmonth_2.
    NEW-LINE.
    lv_currmonth_3 = lv_currmonth + 3.
    WRITE : 'Current Month +3', lv_currmonth_3.
    NEW-LINE.
    lv_currmonth_4 = lv_currmonth + 4.
    WRITE : 'Current Month +4', lv_currmonth_4.
    NEW-LINE.
    lv_currmonth_5 = lv_currmonth + 5.
    WRITE : 'Current Month +5', lv_currmonth_5.
  ENDIF.

****** fetch the total Number of records by calculating first and last day.
  DATA : lt_zdemodata_pa  TYPE TABLE OF zdemo_data.
  DATA : lv_total_records_curmonth TYPE i.
  DATA : lv_firstdate TYPE d,
         lv_lastdate  TYPE d.

  DATA : processingdate TYPE datum.  "20190612 =  12.06.2019
  DATA : lv_month       TYPE monat,
         lv_month_next4 TYPE monat.
**************  monthly logic***********
  lv_month = sy-datum+4(2).
  lv_month_next4 = lv_month + 4.
  WHILE lv_month <= lv_month_next4 ."( lv_month + next 5 months).
    lv_month = lv_month + 1.  " adding in beggining as i dont need to anything for current month
    NEW-LINE.
    WRITE : lv_month.
    CONCATENATE sy-datum+0(4) lv_month sy-datum+6(2) INTO processingdate.
    CALL FUNCTION 'ZGET_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = processingdate "sy-datum
      IMPORTING
        ev_month_begin_date = lv_firstdate
        ev_month_end_date   = lv_lastdate.

    SELECT * FROM zdemo_data INTO TABLE lt_zdemodata_pa WHERE dateposted_str BETWEEN lv_firstdate AND lv_lastdate.
    DESCRIBE TABLE lt_zdemodata_pa LINES lv_total_records_curmonth.
    NEW-LINE.
    WRITE : lv_total_records_curmonth, 'records found' , 'for' , processingdate.
*    lv_month = lv_month + 1.
  ENDWHILE.
*  ************** END monthly logic***********

****try weekly logic***********
*Get current week.
  DATA : curr_week TYPE scal-week.
*  CALL FUNCTION 'DATE_GET_WEEK'
*    EXPORTING
*      date         = sy-datum
*    IMPORTING
*      week         = curr_week
*    EXCEPTIONS
*      date_invalid = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
*    MESSAGE 'Inccorect Date' TYPE 'E'.
*    RETURN.
*  ENDIF.
*  NEW-LINE. WRITE : 'Current Week', curr_week.
** get first day and last day of the week***
  DATA :currweek_firstdate TYPE scal-date,
        currweek_lastdate  TYPE scal-date.
  DATA : lv_total_records_curweek TYPE i.
*  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
*    EXPORTING
*      week         = curr_week
*    IMPORTING
*      date         = currweek_firstdate
*    EXCEPTIONS
*      week_invalid = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
*    MESSAGE 'invalid week' TYPE 'E'.
*    RETURN.
*  ENDIF.
*  currweek_lastdate = currweek_firstdate + 6. " 6 as days in a week

  CALL FUNCTION 'ZGET_WEEK_INFO_BASED_ON_DATE'
    EXPORTING
      date   = frm_date " sy-datum
    IMPORTING
      week   = curr_week
      monday = currweek_firstdate
      sunday = currweek_lastdate.
  NEW-LINE. WRITE : 'Current Week', curr_week, '-Firstdate', currweek_firstdate, '-Lastdate',currweek_lastdate.
  SELECT * FROM zdemo_data INTO TABLE lt_zdemodata_pa WHERE dateposted_str BETWEEN currweek_firstdate AND currweek_lastdate.
  DESCRIBE TABLE lt_zdemodata_pa LINES lv_total_records_curweek.
*  NEW-LINE.
  WRITE : lv_total_records_curweek, 'records found' , 'between' ,currweek_firstdate, 'AND', currweek_lastdate.

***try for 4 weeks as a month and for next 5 months.
  DATA : i_month_counter TYPE i.
  DATA : nextmonth_week   LIKE curr_week,
         lv_week_multiply TYPE i. " VALUE 1.
*  i_month_counter = 1. "5. for months.
*  WHILE i_month_counter < 5. "for 5 months.
*    lv_week_multiply = 1.
  DO 6 TIMES.  " weeks as a month (current month + 5 month)
    nextmonth_week = curr_week+4(2) + ( 4 * lv_week_multiply ). "sy-tabix ).
*    nextmonth_week =  curr_week+4(2) +  ( i_month_counter * 4 ) + lv_week_multiply . "sy-tabix ).
    CONCATENATE '2019' nextmonth_week+4(2) INTO nextmonth_week.
    NEW-LINE.
    WRITE : nextmonth_week.
*    CLEAR : nextmonth_week.
    lv_week_multiply = lv_week_multiply + 1.
**      get week dates****
    DATA :nextweek_firstdate TYPE scal-date,
          nextweek_lastdate  TYPE scal-date.
    DATA : lv_total_records_nextweek TYPE i.
    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = nextmonth_week
      IMPORTING
        date         = nextweek_firstdate
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE 'invalid week' TYPE 'E'.
      RETURN.
    ENDIF.
    nextweek_lastdate = nextweek_firstdate + 6. " 6 as days in a week
    NEW-LINE. WRITE : 'next Week', nextmonth_week, '-Firstdate', nextweek_firstdate, '-Lastdate',nextweek_lastdate.
    SELECT * FROM zdemo_data INTO TABLE lt_zdemodata_pa WHERE dateposted_str BETWEEN nextweek_firstdate AND nextweek_lastdate.
    DESCRIBE TABLE lt_zdemodata_pa LINES lv_total_records_nextweek.
*  NEW-LINE.
    WRITE : lv_total_records_nextweek, 'records found' , 'between' ,nextweek_firstdate, 'AND', nextweek_lastdate.
    CLEAR : nextmonth_week,nextweek_firstdate, nextweek_lastdate,lv_total_records_curweek.
  ENDDO.
*    i_month_counter = i_month_counter + 1.
*    CLEAR lv_week_multiply.
*  ENDWHILE.



*  NEW-LINE. WRITE : 'Current Week', curr_week, '-Firstdate', currweek_firstdate, '-Lastdate',currweek_lastdate.
*  SELECT * FROM zdemo_data INTO TABLE lt_zdemodata_pa WHERE dateposted_str BETWEEN currweek_firstdate AND currweek_lastdate.
*  DESCRIBE TABLE lt_zdemodata_pa LINES lv_total_records_curweek.
**  NEW-LINE.
*  WRITE : lv_total_records_curweek, 'records found' , 'between' ,currweek_firstdate, 'AND', currweek_lastdate.

*  DATA : weekofmonth TYPE monat.
*  weekofmonth = ( curr_week+4(2) MOD 4 ) .
*  NEW-LINE. WRITE: 'Current week of the Month', weekofmonth.

****End of weekly logic.

**** end of Predivtive acct scenario********


  DELETE lt_sales_order WHERE status EQ 'S' OR status EQ 'E' OR status = 'P'.
  IF lt_sales_order[] IS INITIAL AND lt_dbupdate_so[] IS INITIAL.
    IF sy-batch NE 'X'.
      WRITE: 'No records found to be processed ,for the specified Selection conditions,Thank you for using the report.'.
    ENDIF.
    norecords_flag = 'X'.
*  EXIT.
  ENDIF.
