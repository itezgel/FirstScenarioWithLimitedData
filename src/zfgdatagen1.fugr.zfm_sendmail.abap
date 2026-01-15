FUNCTION ZFM_SENDMAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_USER) TYPE  BNAME OPTIONAL
*"     VALUE(I_WORKITEMID) TYPE  SWWWIHEAD-WI_ID OPTIONAL
*"     VALUE(I_MAILID) TYPE  SO_RECNAME OPTIONAL
*"     VALUE(I_COMPANYNAME) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CUSTOMERADDR) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CUSTOMERTYPE) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CUSTOMERASSO) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CUSTOMERATTR) TYPE  MAKTX OPTIONAL
*"     VALUE(I_PAYMENTTERMS) TYPE  MAKTX OPTIONAL
*"     VALUE(I_INVOICINGMETHOD) TYPE  MAKTX OPTIONAL
*"     VALUE(I_REFDOCNO) TYPE  EBELN OPTIONAL
*"     VALUE(I_ZIP) TYPE  MAKTX OPTIONAL
*"     VALUE(I_STATE) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CITY) TYPE  MAKTX OPTIONAL
*"     VALUE(I_COUNTRY) TYPE  MAKTX OPTIONAL
*"     VALUE(I_CUSTOMER) TYPE  MAKTX OPTIONAL
*"     VALUE(I_HOUSENO) TYPE  MAKTX OPTIONAL
*"  EXPORTING
*"     VALUE(E_REASON) TYPE  CHAR200
*"----------------------------------------------------------------------


  DATA: l_user TYPE bname.


  DATA:   t_simple_cont     TYPE TABLE OF swr_cont,
          t_message_lines   TYPE TABLE OF swr_messag,
          t_message_struct  TYPE TABLE OF swr_mstruc,
          t_subcont_all_obj TYPE TABLE OF swr_cont,
          w_reason          TYPE          swr_cont,
          t_subcont_bor_obj TYPE TABLE OF swr_cont,
          t_object_content  TYPE TABLE OF solisti1,
          w_object_content  TYPE          solisti1,
          t_prop_data       TYPE REF TO   data,
          w_return          TYPE          bapiret2.

  DATA:   l_document_id     TYPE sofolenti1-doc_id,
          l_reason_txt      TYPE swcont-value,
          l_reasonfull      TYPE string,
          l_return_code     TYPE sy-subrc,
          l_xml_container   TYPE xstring,
          l_xml_cont_schema TYPE xstring,
          l_no_att          TYPE sy-index.

  DATA : wa_header     TYPE sodocchgi1,
         it_content    TYPE STANDARD TABLE OF solisti1,
         wa_content    TYPE solisti1,
         it_receivers  TYPE STANDARD TABLE OF somlreci1,
         wa_receivers  TYPE somlreci1,
         it_para       TYPE STANDARD TABLE OF soparai1,
         t_related_wis TYPE TABLE OF swwwihead,
         w_related_wis TYPE  swwwihead.

  CALL FUNCTION 'SWI_GET_RELATED_WORKITEMS'
    EXPORTING
      wi_id       = i_workitemid
    TABLES
      related_wis = t_related_wis.

  READ TABLE t_related_wis INTO w_related_wis WITH KEY wi_rhtext = 'Generic decision task'.
  IF sy-subrc = 0.
* Read the work item container from the work item ID
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id              = w_related_wis-wi_id
        language                 = sy-langu
        user                     = sy-uname
      IMPORTING
        return_code              = l_return_code
        ifs_xml_container        = l_xml_container
        ifs_xml_container_schema = l_xml_cont_schema
      TABLES
        simple_container         = t_simple_cont
        message_lines            = t_message_lines
        message_struct           = t_message_struct
        subcontainer_bor_objects = t_subcont_bor_obj
        subcontainer_all_objects = t_subcont_all_obj.

* Initialize
    l_no_att = 0.
* Read the _ATTACH_OBJECTS element
    LOOP AT t_subcont_all_obj INTO w_reason
                              WHERE element = '_ATTACH_OBJECTS'.
      l_no_att = l_no_att + 1.
      l_document_id = w_reason-value.
    ENDLOOP.
* Read the SOFM Document
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id    = l_document_id
      TABLES
        object_content = t_object_content.
* Pass the text to the exporting parameter
    IF sy-subrc = 0.
*        READ TABLE t_object_content INTO l_reason_txt INDEX 1.
      LOOP AT t_object_content INTO w_object_content.
        l_reason_txt = w_object_content-line.
        SHIFT l_reason_txt BY 5 PLACES LEFT.
        CONCATENATE l_reasonfull l_reason_txt  INTO l_reasonfull
                                               SEPARATED BY space.
      ENDLOOP.
    ENDIF.
    e_reason = l_reasonfull.
  ENDIF.

*-------------Send mail----------------*
  DATA: l_userid     TYPE bapibname-bapibname,
        w_address    TYPE bapiaddr3,
        t_return     TYPE TABLE OF bapiret2,
        t_bapiadsmtp TYPE TABLE OF bapiadsmtp,
        w_bapiadsmtp TYPE  bapiadsmtp.

  l_userid = i_user.

  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = l_userid
    IMPORTING
      address  = w_address
    TABLES
      return   = t_return
      addsmtp  = t_bapiadsmtp.

  READ TABLE t_bapiadsmtp INTO w_bapiadsmtp INDEX 1.
  i_mailid = w_bapiadsmtp-e_mail.

  wa_receivers-receiver = i_mailid."'rhaas@mail.cl1.sap.biz'.
*  wa_receivers-receiver = 'ayyappan.venugopal@sap.com'."i_mailid'."'rhaas@mail.cl1.sap.biz'.
  wa_receivers-rec_type = 'U'.
  wa_receivers-com_type = 'INT'.
  APPEND wa_receivers TO it_receivers.
  CLEAR: wa_receivers.

  wa_header-obj_prio = 1.
  wa_header-priority = 1.
  wa_header-obj_langu = sy-langu.

  SHIFT i_refdocno LEFT DELETING LEADING '0'.

  CONCATENATE 'Customer' i_refdocno 'Created' INTO
  wa_header-obj_descr SEPARATED BY space.

  CONCATENATE 'Hi' w_address-fullname ',' wa_content INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  CLEAR: wa_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE 'Find the Customer details' wa_content INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Estes Company Name :' '</B>' i_companyname INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Customer No :' '</B>' i_refdocno INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Customer Name:' '</B>' i_customer INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Street :'  '</B>' i_customeraddr INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'House No :'  '</B>' i_houseno INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Zip :'  '</B>' i_zip INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'State :'  '</B>' i_state INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'City :'  '</B>' i_city INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Country :'  '</B>' i_country INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Customer Type :' '</B>' i_customertype INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Customer Association :' '</B>' i_customerasso INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Customer Attribute :' '</B>' i_customerattr INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Payment Terms :' '</B>' i_paymentterms INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Invoicing Method :' '</B>' i_invoicingmethod INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Approval Comment :' '</B>' e_reason  INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  CLEAR: wa_content.
  APPEND '<br>' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE  'Regards,' wa_content  INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  APPEND '<br>' TO it_content.

  CONCATENATE '<B>' 'Workflow System :' '</B>' INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  CLEAR: wa_content.
  APPEND '<br>' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.


  CONCATENATE '<B>' '<font color="red">' 'Don`t reply,system generated !!!!! '  '</font>''</B>'
  INTO wa_content SEPARATED BY space.
  APPEND wa_content TO it_content.
  CLEAR: wa_content.
  APPEND '<br>' TO it_content.

  APPEND '<br>' TO it_content.
  APPEND '<br>' TO it_content.


  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data  = wa_header
      document_type  = 'HTM'
      commit_work    = 'X'
    TABLES
      object_content = it_content
      object_para    = it_para
      receivers      = it_receivers.

  WAIT UP TO 2 SECONDS.
  SUBMIT rsconn01 WITH mode = 'INT'
  AND RETURN.

ENDFUNCTION.
