*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPA_PERCENTAGE..................................*
DATA:  BEGIN OF STATUS_ZPA_PERCENTAGE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPA_PERCENTAGE                .
CONTROLS: TCTRL_ZPA_PERCENTAGE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPA_PERCENTAGE                .
TABLES: ZPA_PERCENTAGE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
