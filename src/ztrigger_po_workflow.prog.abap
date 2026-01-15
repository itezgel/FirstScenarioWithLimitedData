*&---------------------------------------------------------------------*
*& Report ZTRIGGER_PO_WORKFLOW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztrigger_po_workflow.


TABLES : ekko.

SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_po FOR ekko-ebeln." OBLIGATORY.
*  SELECT-OPTIONS : s_erdat for vbak-erdat.
SELECTION-SCREEN END OF BLOCK block.

TYPES : BEGIN OF struc,
          ebeln TYPE ebeln,
        END OF struc.

DATA : ls_po TYPE struc,
       lt_po TYPE TABLE OF struc.

IF s_po IS INITIAL.
  MESSAGE 'Provide purchasing document' TYPE 'I'.
ELSE.
  clear lt_po[].
  SELECT * FROM ekko INTO CORRESPONDING FIELDS OF TABLE lt_po WHERE ebeln IN s_po.
  IF lt_po[] IS INITIAL.
    MESSAGE 'Error: No purchasing Documents found' TYPE 'E'.
  ELSE.
    sort lt_po ASCENDING by ebeln .
    DELETE ADJACENT DUPLICATES FROM lt_po COMPARING ebeln.
    LOOP AT lt_po INTO ls_po.
*      DATA declarations
      DATA: lv_objtype          TYPE sibftypeid,
            lv_event            TYPE sibfevent,
            lv_objkey           TYPE sibfinstid,
            lr_event_parameters TYPE REF TO if_swf_ifs_parameter_container,
            lv_param_name       TYPE swfdname,
            lv_id               TYPE char10.

* Setting values of Event Name
      lv_objtype = 'CL_MM_PUR_WF_OBJECT_PO'. " your class name
      lv_event   = 'SUBMITTED_FOR_APPROVAL'.  " event name.

      lv_objkey = ls_po-ebeln.

* Instantiate an empty event container
      CALL METHOD cl_swf_evt_event=>get_event_container
        EXPORTING
          im_objcateg  = cl_swf_evt_event=>mc_objcateg_cl
          im_objtype   = lv_objtype
          im_event     = lv_event
        RECEIVING
          re_reference = lr_event_parameters.

* Set up the name/value pair to be added to the container
      lv_param_name  = 'IV_OBJECT_ID'.  " parameter name of the event
      lv_id          = ls_po-ebeln.

* Add the name/value pair to the event conainer
      TRY.
          CALL METHOD lr_event_parameters->set
            EXPORTING
              name  = lv_param_name
              value = lv_id.

        CATCH cx_swf_cnt_cont_access_denied .
        CATCH cx_swf_cnt_elem_access_denied .
        CATCH cx_swf_cnt_elem_not_found .
        CATCH cx_swf_cnt_elem_type_conflict .
        CATCH cx_swf_cnt_unit_type_conflict .
        CATCH cx_swf_cnt_elem_def_invalid .
        CATCH cx_swf_cnt_container .
      ENDTRY.

* Raise the event passing the prepared event container
      TRY.
          CALL METHOD cl_swf_evt_event=>raise
            EXPORTING
              im_objcateg        = cl_swf_evt_event=>mc_objcateg_cl
              im_objtype         = lv_objtype
              im_event           = lv_event
              im_objkey          = lv_objkey
              im_event_container = lr_event_parameters.
        CATCH cx_swf_evt_invalid_objtype .
        CATCH cx_swf_evt_invalid_event .
      ENDTRY.

      COMMIT WORK.
      WRITE : 'Workflow triggered for ' , ls_po-ebeln.
      NEW-LINE.
      clear :ls_po, lv_id.
    ENDLOOP.
  ENDIF.
ENDIF.
