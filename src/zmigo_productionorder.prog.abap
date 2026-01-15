*&---------------------------------------------------------------------*
*& Report ZMIGO_PRODUCTIONORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmigo_productionorder.

TABLES : aufk.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_aufnr FOR aufk-aufnr." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.



DATA: ls_goodsmvt_header  TYPE bapi2017_gm_head_01,
      lt_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
      ls_goodsmvt_item    TYPE bapi2017_gm_item_create,
      lt_return           TYPE TABLE OF  bapiret2,
      ls_goodsmvt_headret TYPE  bapi2017_gm_head_ret,
      ls_goodsmvt_code    TYPE bapi2017_gm_code,
*      ls_ekko             TYPE ekko,
*      lt_ekko             TYPE TABLE OF ekko,
*      ls_ekpo             TYPE ekpo,
*      lt_ekpo             TYPE TABLE OF ekpo,
      testrun             TYPE c VALUE 'X',
      lv_ebeln            TYPE xblnr,
      l_return            LIKE bapiret2,
      e_pgino             TYPE mblnr.

START-OF-SELECTION.

  DATA : lt_aufk TYPE TABLE OF aufk,
         ls_aufk TYPE aufk.

DATA : ORDER_OBJECTS TYPE BAPI_PP_ORDER_OBJECTS,
      it_cOMPONENT TYPE TABLE OF  BAPI_ORDER_COMPONENT,
      LS_COMPONENT TYPE BAPI_ORDER_COMPONENT,
      ls_return type BAPIRET2.


  SELECT * FROM aufk INTO TABLE lt_aufk WHERE aufnr IN s_aufnr.

  IF lt_aufk[] IS NOT INITIAL.
    LOOP AT lt_aufk INTO ls_aufk.
      CLEAR: ls_goodsmvt_item,ls_goodsmvt_header.



*---Filing Header Data-------*
      ls_goodsmvt_header-pstng_date = '20180131'. "sy-datum.
      ls_goodsmvt_header-doc_date   =  sy-datum.
      ls_goodsmvt_header-ref_doc_no = ls_aufk-aufnr.
*      ls_goodsmvt_header-ver_gr_gi_slip = '1'.
*      ls_goodsmvt_header-ref_doc_no_long = '123456'.

      ls_goodsmvt_code-gm_code      = '03'.
*ls_goodsmvt_code-gm_code      = '03'.
*---Filing Header Data-------*
ORDER_OBJECTS-COMPONENTS = 'X'.

CALL FUNCTION 'BAPI_PRODORD_GET_DETAIL'
  EXPORTING
    number                 = ls_aufk-aufnr
*   COLLECTIVE_ORDER       =
    order_objects          = ORDER_OBJECTS
 IMPORTING
   RETURN                 = ls_return
 TABLES
*   HEADER                 =
*   POSITION               =
*   SEQUENCE               =
*   OPERATION              =
*   TRIGGER_POINT          =
   COMPONENT              = IT_COMPONENT
*   PROD_REL_TOOL          =
*   FSH_BUNDLES            =
          .

IF ls_return-type = 'E'.
  Write : 'Error:',ls_aufk-aufnr, ls_return-message.NEW-LINE.
ENDIF.

LOOP AT it_component into ls_component.
*  MOVE-CORRESPONDING ls_component to ls_goodsmvt_item.
*  ls_goodsmvt_item-move_type      = '261'.
*  APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
*---Filing Item Data-------*
      ls_goodsmvt_item-material       = ls_component-material.
      ls_goodsmvt_item-plant          = ls_component-PROD_PLANT.
      ls_goodsmvt_item-stge_loc       = ls_component-storage_location.
      ls_goodsmvt_item-move_type      = '261'.
      ls_goodsmvt_item-entry_qnt      = ls_component-ENTRY_QUANTITY. "'1'.
      ls_goodsmvt_item-entry_uom      = ls_component-ENTRY_UOM."'ST'.
      ls_goodsmvt_item-reserv_no      = ls_component-RESERVATION_NUMBER.
      ls_goodsmvt_item-RES_ITEM      = ls_component-RESERVATION_ITEM.

      APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
ENDLOOP.
*---Filing Item Data-------*

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          goodsmvt_header  = ls_goodsmvt_header
          goodsmvt_code    = '03' "ls_goodsmvt_code
          testrun          = ' '
        IMPORTING
          goodsmvt_headret = ls_goodsmvt_headret
        TABLES
          goodsmvt_item    = lt_goodsmvt_item
*         GOODSMVT_SERIALNUMBER =
          return           = lt_return.

      IF lt_return[] IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'
          IMPORTING
            return = l_return.

        WRITE : 'Success:prodorder', ls_goodsmvt_headret ,' has Goods Issue', ls_goodsmvt_headret-mat_Doc.
        NEW-LINE.
      ELSE.
        Write : 'Error:',ls_aufk-aufnr, 'Could not be processed'.NEW-LINE.
        LOOP AT lt_return into ls_return WHERE type = 'E'.
          WRITE : 'Error:',ls_aufk-aufnr,ls_return-message. new-LINE.
          delete lt_return where ID = ls_return-ID and NUMBER = ls_return-NUMBER.
        ENDLOOP.
        NEW-LINE.
      ENDIF.

*      e_pgino = ls_goodsmvt_headret-mat_doc.

CLEAR: ls_aufk,ls_goodsmvt_header,lt_goodsmvt_item,lt_return,l_return.
NEW-LINE.
    ENDLOOP.
  ENDIF.
