*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDEMO_DATA......................................*
DATA:  BEGIN OF STATUS_ZDEMO_DATA                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDEMO_DATA                    .
CONTROLS: TCTRL_ZDEMO_DATA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDEMO_DATA                    .
TABLES: ZDEMO_DATA                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
