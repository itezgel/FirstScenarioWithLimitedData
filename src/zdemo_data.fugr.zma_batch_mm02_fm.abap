FUNCTION ZMA_BATCH_MM02_FM.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(MATNR_001) LIKE  BDCDATA-FVAL DEFAULT '1722'
*"     VALUE(KZSEL_01_002) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(KZSEL_02_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MAKTX_004) LIKE  BDCDATA-FVAL
*"         DEFAULT 'Additional Lighting'
*"     VALUE(MEINS_005) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(MATKL_006) LIKE  BDCDATA-FVAL DEFAULT 'FL80'
*"     VALUE(MTPOS_MARA_007) LIKE  BDCDATA-FVAL DEFAULT 'NORM'
*"     VALUE(BRGEW_008) LIKE  BDCDATA-FVAL DEFAULT '10'
*"     VALUE(GEWEI_009) LIKE  BDCDATA-FVAL DEFAULT 'KG'
*"     VALUE(NTGEW_010) LIKE  BDCDATA-FVAL DEFAULT '8'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"--------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLMGMM' '0060'.
perform bdc_field       using 'BDC_CURSOR'
                              'RMMG1-MATNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'RMMG1-MATNR'
                              MATNR_001.
perform bdc_dynpro      using 'SAPLMGMM' '0070'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSICHTAUSW-DYTXT(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'MSICHTAUSW-KZSEL(01)'
                              KZSEL_01_002.
perform bdc_field       using 'MSICHTAUSW-KZSEL(02)'
                              KZSEL_02_003.
perform bdc_dynpro      using 'SAPLMGMM' '4004'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'MAKT-MAKTX'
                              MAKTX_004.
perform bdc_field       using 'MARA-MEINS'
                              MEINS_005.
perform bdc_field       using 'MARA-MATKL'
                              MATKL_006.
perform bdc_field       using 'MARA-MTPOS_MARA'
                              MTPOS_MARA_007.
perform bdc_field       using 'BDC_CURSOR'
                              'MARA-NTGEW'.
perform bdc_field       using 'MARA-BRGEW'
                              BRGEW_008.
perform bdc_field       using 'MARA-GEWEI'
                              GEWEI_009.
perform bdc_field       using 'MARA-NTGEW'
                              NTGEW_010.
perform bdc_transaction tables messtab
using                         'MM02'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
INCLUDE BDCRECXY .
