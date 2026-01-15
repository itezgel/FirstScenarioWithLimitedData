*&---------------------------------------------------------------------*
*& Report ZFETCH_SODELIVERYBILLING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfetch_sodeliverybilling.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN COMMENT /2(30) TEXT-003.
SELECTION-SCREEN COMMENT /2(30) TEXT-004.
SELECTION-SCREEN COMMENT /2(30) TEXT-005.
SELECTION-SCREEN COMMENT /2(30) TEXT-006.
SELECTION-SCREEN COMMENT /2(30) TEXT-007.
SELECTION-SCREEN COMMENT /2(30) TEXT-008.
SELECTION-SCREEN COMMENT /2(30) TEXT-009.
SELECTION-SCREEN COMMENT /2(30) TEXT-010.
SELECTION-SCREEN COMMENT /2(30) TEXT-011.
SELECTION-SCREEN COMMENT /2(30) TEXT-012.
SELECTION-SCREEN COMMENT /2(30) TEXT-013.
SELECTION-SCREEN COMMENT /2(30) TEXT-014.
*SELECTION-SCREEN COMMENT /2(30) text-015.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS : lv_matnr TYPE matnr,
             lv_date  TYPE dats DEFAULT sy-datum,
             lv_user  TYPE sy-uname DEFAULT sy-uname,
             sort_so  TYPE char1 DEFAULT 'Y',
             sort_po  TYPE char1 DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK b2.

TABLES  : vbap,  lips, vbrp .

DATA : lt_vbap TYPE TABLE OF vbap,
       ls_vbap TYPE vbap,
       lt_lips TYPE TABLE OF lips,
       lt_vbrp TYPE TABLE OF vbrp,
       ls_vbrp TYPE vbrp.

TYPES: BEGIN OF ty_matlist,
         matnr TYPE matnr,
       END OF ty_matlist.

DATA : ls_matlist TYPE ty_matlist,
       lt_matlist TYPE TABLE OF ty_matlist WITH HEADER LINE.

IF lv_matnr IS NOT INITIAL.
  ls_matlist-matnr = lv_matnr.
  APPEND ls_matlist TO lt_matlist.
ELSE.
  ls_matlist-matnr = 'MZ-FG-R300'. APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-R200'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-R100'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-M550'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-M525'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-M500'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-C990'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-C950'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-FG-C900'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-TG-Y120'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-TG-Y200'.APPEND ls_matlist TO lt_matlist.
  ls_matlist-matnr = 'MZ-TG-Y240'.APPEND ls_matlist TO lt_matlist.
ENDIF.

IF lv_date IS INITIAL.
  lv_date = sy-datum.
ENDIF.

IF lv_user IS INITIAL.
  lv_user = sy-uname.
ENDIF.

SORT lt_matlist ASCENDING BY matnr.

TYPES : BEGIN OF ty_struc,
          matnr    TYPE matnr,
          qty      TYPE kwmeng,
*          tot_qty  TYPE i,
          ref_po   TYPE bstkd,
          audat     TYPE audat,
          KUNNR	TYPE  KUNAG,
          so       TYPE	vbeln_va,
          delivery TYPE vbeln_vl,
          billing  TYPE vbeln_vf,
        END OF ty_struc.

DATA : lt_docs TYPE TABLE OF ty_struc,
       ls_docs LIKE LINE OF lt_docs,
       qty     TYPE i.

*SELECT * FROM vbap INTO CORRESPONDING FIELDS OF TABLE lt_vbap WHERE matnr EQ lt_matlist-matnr.

LOOP AT  lt_matlist INTO ls_matlist .
  ls_docs-matnr = ls_matlist-matnr.
  SELECT * FROM vbap INTO TABLE lt_vbap WHERE matnr = ls_matlist-matnr AND erdat = lv_date AND ernam = lv_user.
  SORT lt_vbap ASCENDING BY vbeln.
  LOOP AT lt_vbap INTO ls_vbap.
    ls_docs-so = ls_vbap-vbeln.
    ls_docs-matnr = ls_vbap-matnr.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT         = ls_vbap-vbeln
*     IMPORTING
*       OUTPUT        = ls_docs-so.

    ls_docs-qty = ls_vbap-kwmeng.
    qty = qty +  ls_docs-qty.
    select single audat kunnr from vbak into  ( ls_docs-audat, ls_docs-kunnr ) where VBELN = ls_docs-so.
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE bstkd INTO ls_docs-ref_po FROM vbkd WHERE vbeln = ls_docs-so .
    SELECT bstkd FROM vbkd INTO TABLE @DATA(lt_x003) UP TO 1 ROWS WHERE vbeln = ls_docs-so ORDER BY posnr.
    IF sy-subrc eq 0.
      READ TABLE lt_x003 INTO DATA(ls_x003) INDEX 1.
      ls_docs-ref_po = ls_x003-bstkd.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE vbeln INTO ls_docs-delivery FROM lips WHERE vgbel = ls_docs-so .
    SELECT vbeln FROM lips INTO TABLE @DATA(lt_x002) UP TO 1 ROWS WHERE vgbel = ls_docs-so ORDER BY vbeln posnr.
    IF sy-subrc eq 0.
      READ TABLE lt_x002 INTO DATA(ls_x002) INDEX 1.
      ls_docs-delivery = ls_x002-vbeln.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
* Start-Antigravity made this changes-(15.01.2026)
*     SELECT SINGLE vbeln INTO ls_docs-billing FROM vbrp WHERE posnr = '000010' AND aubel = ls_docs-so AND vgbel = ls_docs-delivery.
    SELECT vbeln FROM vbrp INTO TABLE @DATA(lt_x001) UP TO 1 ROWS WHERE posnr = '000010' AND aubel = ls_docs-so AND vgbel = ls_docs-delivery ORDER BY vbeln.
    IF sy-subrc eq 0.
      READ TABLE lt_x001 INTO DATA(ls_x001) INDEX 1.
      ls_docs-billing = ls_x001-vbeln.
    ENDIF.
* Finish-Antigravity made this changes-(15.01.2026)
    APPEND ls_docs TO lt_docs.
    clear: ls_vbap.
    clear ls_docs.
  ENDLOOP.
*  clear : ls_docs.
ENDLOOP.

IF sort_po EQ 'Y' OR sort_po EQ 'y'.
  SORT lt_docs ASCENDING BY ref_po.
ENDIF.

IF sort_so EQ 'Y' OR sort_so EQ 'y'.
  SORT lt_docs ASCENDING BY so.
ENDIF.

*SORT lt_docs ASCENDING BY matnr.
IF lt_docs[] IS NOT INITIAL.
  DATA: count TYPE i.
  DESCRIBE TABLE lt_docs LINES count.
  IF lv_matnr IS NOT INITIAL.
    WRITE : count , 'records found','Quantity=',qty.
    NEW-LINE.
  ELSE.
    WRITE : count , 'records found'.
    NEW-LINE.
  ENDIF.

  LOOP AT lt_docs INTO ls_docs.
    WRITE : ls_docs-ref_po,'-', ls_docs-matnr,'-',ls_docs-qty,'-',ls_docs-audat,'-',ls_docs-kunnr,
            '-',ls_docs-so,'-',ls_docs-delivery,'-',ls_docs-billing.
    NEW-LINE.
  ENDLOOP.

ELSE.
  WRITE : 'No records found for the inputs provided'.
ENDIF.
