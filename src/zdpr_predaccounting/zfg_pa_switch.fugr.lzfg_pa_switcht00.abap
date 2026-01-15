*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPA_SWITCH......................................*
DATA:  BEGIN OF STATUS_ZPA_SWITCH                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPA_SWITCH                    .
CONTROLS: TCTRL_ZPA_SWITCH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPA_SWITCH                    .
TABLES: ZPA_SWITCH                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
