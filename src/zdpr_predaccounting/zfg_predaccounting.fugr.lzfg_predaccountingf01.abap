*----------------------------------------------------------------------*
***INCLUDE LZFG_PREDACCOUNTINGF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_begda_of_month
*&---------------------------------------------------------------------*
FORM get_begda_of_month
       USING
         iv_date TYPE d
       CHANGING
         cv_month_begin_date TYPE d.

  cv_month_begin_date(6) = iv_date(6).
  cv_month_begin_date+6(2) = '01'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_endda_of_month
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM get_endda_of_month
       USING
         iv_date TYPE d
       CHANGING
         cv_month_end_date TYPE d.

  IF iv_date+4(2) = '12'.
    cv_month_end_date(6) = iv_date(6).
    cv_month_end_date+6(2) = '31'.
  ELSE.
    PERFORM get_begda_of_month
      USING
        iv_date
      CHANGING
        cv_month_end_date.

    cv_month_end_date+4(2) = cv_month_end_date+4(2) + 1.
    cv_month_end_date = cv_month_end_date - 1.
  ENDIF.
ENDFORM.
