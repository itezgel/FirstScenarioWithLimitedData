FUNCTION ZSEND_MAIL_RECEIPIENT .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(FORMOUTPUT) TYPE  FPFORMOUTPUT OPTIONAL
*"     VALUE(SENDERUNAME) TYPE  SY-UNAME
*"     VALUE(RECEIPIENTUNAME) TYPE  SY-UNAME
*"     VALUE(MAILSUBJECT) TYPE  CHAR50
*"     VALUE(I_TEXT1) TYPE  SOLI_TAB OPTIONAL
*"  EXPORTING
*"     VALUE(MESSAGE) TYPE  SOLI
*"----------------------------------------------------------------------

      DATA : I_TEXT TYPE SOLI_TAB,
             V_TEXT TYPE SOLI.

             I_text = I_text1.

       DATA:
        SEND_REQUEST    TYPE REF TO CL_BCS,
        DOCUMENT        TYPE REF TO CL_DOCUMENT_BCS,
        DOCUMENT1        TYPE REF TO CL_DOCUMENT_BCS,
        RECIPIENT       TYPE REF TO IF_RECIPIENT_BCS,
        sender       TYPE REF TO IF_sender_BCS,
        BCS_EXCEPTION   TYPE REF TO CX_BCS,
        LV_SENT_TO_ALL  TYPE OS_BOOLEAN,
        LP_PDF_SIZE     TYPE SO_OBJ_LEN,
        PDF_CONTENT        TYPE SOLIX_TAB.

              data temp_syb_dest type AD_SYMBDST.
              write sy-sysid to temp_syb_dest.

              data temp_client type AD_UMAND.
              write sy-mandt to temp_client.

              data temp_uname type AD_UNAME.
              write sy-uname to temp_uname.

*      V_TEXT = 'Hi,' .
*      APPEND V_TEXT TO I_TEXT.
*      CLEAR V_TEXT.
*      APPEND V_TEXT TO I_TEXT.
*
*
*      V_TEXT =  'Please see the attachement for further processing from your end.'  .
*      APPEND V_TEXT TO I_TEXT.
*      CLEAR V_TEXT.
*
*
*      V_TEXT = 'Best Regards,'.
*      APPEND V_TEXT TO I_TEXT.
*      CLEAR V_TEXT.
*
*      V_TEXT = 'Workflow-system'.
*      APPEND V_TEXT TO I_TEXT.
*      CLEAR V_TEXT.

*      * ------------ Call BCS interface ----------------------------------

      TRY.
*   ---------- create persistent send request ----------------------


          SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).

*   ---------- add document ----------------------------------------
*   get PDF xstring and convert it to BCS format

if not formoutput is initial.

          LP_PDF_SIZE = XSTRLEN( FORMOUTPUT-PDF ).

          PERFORM XSTRING_TO_SOLIX
                         USING
                         FORMOUTPUT-PDF.
          IMPORT PDF_CONTENT FROM MEMORY ID 'PDF_NEW'.

          DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                I_TYPE    = 'PDF' " cf. RAW, DOC
                I_HEX     = PDF_CONTENT
                I_LENGTH  = LP_PDF_SIZE
                I_TEXT    = I_TEXT
                I_SUBJECT = mailSUBJECT ).                      "#EC NOTEXT

*    Concatenate 'Material' MATERIAL '- Create Sales View.' into SUBJECT separated by space.
          DOCUMENT1 = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                I_TYPE    = 'RAW'
                I_TEXT    = I_TEXT
                I_SUBJECT = mailSUBJECT ).                      "#EC NOTEXT

          DOCUMENT1->ADD_DOCUMENT_AS_ATTACHMENT( DOCUMENT ).

*   add document to send request
          SEND_REQUEST->SET_DOCUMENT( DOCUMENT1 ).
endif.


*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.
*
          sender = cl_sapuser_bcs=>create( SENDERUNAME ).
          CALL METHOD send_request->set_sender
            EXPORTING
              i_sender = sender.


*   ---------- add recipient (e-mail address) ----------------------
*    RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*        I_ADDRESS_STRING = 'prasanna.rangaraju@sap.com' ).
*      RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*          I_ADDRESS_STRING = 'mithun.seshadri@sap.com' ).
*      RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*     I_ADDRESS_STRING = 'abc@sap.com' ).
*
      RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
     I_ADDRESS_STRING = 'h.a@sap.com'). "'nivedita.patil@bcone.com' ).

*          call method CL_CAM_ADDRESS_BCS=>CREATE_RML_ADDRESS
*            EXPORTING
*              I_USERNAME = RECEIPIENTUNAME
*              I_SYST     = temp_syb_dest
*              I_CLIENT   = temp_client
*            RECEIVING
*              result     = recipient.
*

*   add recipient to send request
          SEND_REQUEST->ADD_RECIPIENT( I_RECIPIENT = RECIPIENT ).

*   ---------- send document ---------------------------------------

If not formoutput is INITIAL.

          LV_SENT_TO_ALL = SEND_REQUEST->SEND(
              I_WITH_ERROR_SCREEN = 'X' ).

          IF LV_SENT_TO_ALL = 'X'.
*        MESSAGE I022(SO).
            Concatenate 'Workflow mail sent to user' RECEIPIENTUNAME 'for processing.'  into message separated by space.
          ENDIF.
*        If there is not any attachment to be sent, just send the mail
else.
DATA: OBJPACK LIKE SOPCKLSTI1 OCCURS  2 WITH HEADER LINE.

DATA: OBJHEAD LIKE SOLISTI1   OCCURS  1 WITH HEADER LINE.

DATA: OBJBIN  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.

DATA: OBJTXT  LIKE SOLISTI1   OCCURS 10 WITH HEADER LINE.

DATA: RECLIST LIKE SOMLRECI1  OCCURS  5 WITH HEADER LINE.

DATA: DOC_CHNG LIKE SODOCCHGI1.

DATA: TAB_LINES LIKE SY-TABIX.


* Creating the document to be sent

DOC_CHNG-OBJ_NAME = 'OFFER'.

DOC_CHNG-OBJ_DESCR = mailSUBJECT.

data wa_itext like  line of i_text1.

loop at i_text1 into wa_itext.
  objtxt = wa_itext.
  append objtxt.
endloop.

*OBJTXT = mailsubject.

*APPEND OBJTXT.

DESCRIBE TABLE OBJTXT LINES TAB_LINES.

READ TABLE OBJTXT INDEX TAB_LINES.

DOC_CHNG-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( OBJTXT ).

* Creating the entry for the compressed document

CLEAR OBJPACK-TRANSF_BIN.

OBJPACK-HEAD_START = 1.

OBJPACK-HEAD_NUM   = 0.

OBJPACK-BODY_START = 1.

OBJPACK-BODY_NUM   = TAB_LINES.

OBJPACK-DOC_TYPE   = 'RAW'.

APPEND OBJPACK.

* Entering names in the distribution list

RECLIST-RECEIVER = RECEIPIENTUNAME.

RECLIST-REC_TYPE = 'B'.

APPEND RECLIST.

****Remove this later

if RECEIPIENTUNAME eq 'GLANDER'.

*if RECEIPIENTUNAME eq 'I036765'.
clear reclist.
refresh reclist.

*RECLIST-RECEIVER = 'suneetha.y@sap.com'.

RECLIST-RECEIVER = 'h.a@sap.com'. "'christopher.glander@sap.com'.

RECLIST-REC_TYPE = 'U'.

APPEND RECLIST.
endif.

***Remove this later
* Sending the document

CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'

     EXPORTING

          DOCUMENT_DATA = DOC_CHNG

          PUT_IN_OUTBOX = 'X'

          COMMIT_WORK   = 'X'

     TABLES

          PACKING_LIST  = OBJPACK

          CONTENTS_TXT  = OBJTXT

          RECEIVERS     = RECLIST

     EXCEPTIONS

          TOO_MANY_RECEIVERS = 1

          DOCUMENT_NOT_SENT  = 2

          OPERATION_NO_AUTHORIZATION = 4

          OTHERS = 99.

CASE SY-SUBRC.

  WHEN 0.

    WRITE: / 'Result of the send process:'.

    LOOP AT RECLIST.

      WRITE: / RECLIST-RECEIVER(48), ':'.

      IF RECLIST-RETRN_CODE = 0.

        Concatenate 'E-mail Sent to user' RECEIPIENTUNAME '.' INTO Message SEPARATED BY space.

      ELSE.

        MESSAGE = 'Mail not sent'.

      ENDIF.

    ENDLOOP.

  WHEN 1.

    MESSAGE = 'no authorization to send to the specified number of recipients!'.

  WHEN 2.

    MESSAGE =  'document could not be sent to any of the recipients!'.

  WHEN 4.

    MESSAGE = 'no authorization to send !'.

  WHEN OTHERS.

    MESSAGE =  'error occurred during sending !'.

  ENDCASE.

     endif.


*   ---------- explicit 'commit work' is mandatory! ----------------
          COMMIT WORK.

          WAIT UP TO 2 SECONDS.

*Force running the job to send the mail immediately
          SUBMIT RSCONN01 USING SELECTION-SET 'SAP&CONNECTRML' EXPORTING LIST TO MEMORY AND RETURN.

* ------------------------------------------------------------------
* *            exception handling
* ------------------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* ------------------------------------------------------------------
        CATCH CX_BCS INTO BCS_EXCEPTION.
*      MESSAGE e019 WITH bcs_exception->error_type.
*     Sending fax/mail failed
          EXIT.

      ENDTRY.




ENDFUNCTION.


*&---------------------------------------------------------------------*
*&      Form  XSTRING_TO_SOLIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IP_XSTRING text
*----------------------------------------------------------------------*
FORM  XSTRING_TO_SOLIX
    USING IP_XSTRING TYPE XSTRING.

  DATA:
    LP_OFFSET          TYPE I,
    LT_SOLIX           TYPE SOLIX_TAB,
    LS_SOLIX_LINE      TYPE SOLIX,
    LP_PDF_STRING_LEN  TYPE I,
    LP_SOLIX_ROWS      TYPE I,
    LP_LAST_ROW_LENGTH TYPE I,
    LP_ROW_LENGTH      TYPE I.
  DATA : PDF_CONTENT TYPE SOLIX_TAB.
* transform xstring to SOLIX
  DESCRIBE TABLE LT_SOLIX.
  LP_ROW_LENGTH = SY-TLENG.
  LP_OFFSET = 0.

  LP_PDF_STRING_LEN = XSTRLEN( IP_XSTRING ).

  LP_SOLIX_ROWS = LP_PDF_STRING_LEN DIV LP_ROW_LENGTH.
  LP_LAST_ROW_LENGTH = LP_PDF_STRING_LEN MOD LP_ROW_LENGTH.
  DO LP_SOLIX_ROWS TIMES.
    LS_SOLIX_LINE-LINE =
           IP_XSTRING+LP_OFFSET(LP_ROW_LENGTH).
    APPEND LS_SOLIX_LINE TO PDF_CONTENT.
    ADD LP_ROW_LENGTH TO LP_OFFSET.
  ENDDO.
  IF LP_LAST_ROW_LENGTH > 0.
    CLEAR LS_SOLIX_LINE-LINE.
    LS_SOLIX_LINE-LINE = IP_XSTRING+LP_OFFSET(LP_LAST_ROW_LENGTH).
    APPEND LS_SOLIX_LINE TO PDF_CONTENT.
  ENDIF.

  EXPORT PDF_CONTENT TO MEMORY ID 'PDF_NEW'.
ENDFORM.
                "XSTRING_TO_SOLI
