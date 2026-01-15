FUNCTION zget_week_info_based_on_date.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DATE) LIKE  SY-DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(WEEK) LIKE  SCAL-WEEK
*"     REFERENCE(MONDAY) LIKE  SY-DATUM
*"     REFERENCE(SUNDAY) LIKE  SY-DATUM
*"--------------------------------------------------------------------

  DATA:    im_date LIKE scal-date.
  DATA:    ex_week LIKE scal-week.
  DATA:    ex_monday LIKE scal-date.
  DATA:    ex_sunday LIKE scal-date.

* Get the week including the given day
  im_date = date.
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = im_date
    IMPORTING
      week         = ex_week
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Get the monday

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = ex_week
    IMPORTING
      date         = ex_monday
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

  ex_sunday = ex_monday.
  ADD 6 TO ex_sunday.

  MOVE ex_week TO week.
  MOVE ex_monday TO monday.
  MOVE ex_sunday TO sunday.

ENDFUNCTION.
