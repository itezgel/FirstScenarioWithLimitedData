FUNCTION ZGET_MONTH_BEGIN_END_DATE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DATE) TYPE  D
*"  EXPORTING
*"     VALUE(EV_MONTH_BEGIN_DATE) TYPE  D
*"     VALUE(EV_MONTH_END_DATE) TYPE  D
*"--------------------------------------------------------------------

  if ev_month_begin_date is requested.
    perform get_begda_of_month
      using
        iv_date
      changing
        ev_month_begin_date.
  endif.

  if ev_month_end_date is requested.
    perform get_endda_of_month
      using
        iv_date
      changing
        ev_month_end_date.
  endif.

ENDFUNCTION.
