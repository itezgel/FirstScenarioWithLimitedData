*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZDATAGEN_PROCURE................................*
DATA:  BEGIN OF STATUS_ZDATAGEN_PROCURE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZDATAGEN_PROCURE              .
CONTROLS: TCTRL_ZDATAGEN_PROCURE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZDATAGEN_PROCURE              .
TABLES: ZDATAGEN_PROCURE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
