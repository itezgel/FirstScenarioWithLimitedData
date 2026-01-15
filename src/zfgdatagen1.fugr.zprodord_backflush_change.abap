FUNCTION ZPRODORD_BACKFLUSH_CHANGE.
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
*"     VALUE(AUFNR_001) LIKE  BDCDATA-FVAL DEFAULT '1005031'
*"     VALUE(FILTER_BOX_002) LIKE  BDCDATA-FVAL DEFAULT 'NO_FIL'
*"     VALUE(SORT_BOX_003) LIKE  BDCDATA-FVAL DEFAULT 'ST_STA'
*"     VALUE(RGEKZ_01_004) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_02_005) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_03_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_04_007) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_05_008) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_06_009) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_07_010) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_08_011) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(RGEKZ_09_012) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(MATNR_013) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-01'
*"     VALUE(POSNR_014) LIKE  BDCDATA-FVAL DEFAULT '0010'
*"     VALUE(LGORT_015) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_016) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_017) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_018) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_019) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_020) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-02'
*"     VALUE(POSNR_021) LIKE  BDCDATA-FVAL DEFAULT '0020'
*"     VALUE(LGORT_022) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_023) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_024) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_025) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_026) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_027) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-03'
*"     VALUE(POSNR_028) LIKE  BDCDATA-FVAL DEFAULT '0030'
*"     VALUE(LGORT_029) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_030) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_031) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_032) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_033) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_034) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-04'
*"     VALUE(POSNR_035) LIKE  BDCDATA-FVAL DEFAULT '0040'
*"     VALUE(LGORT_036) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_037) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_038) LIKE  BDCDATA-FVAL DEFAULT '72'
*"     VALUE(EINHEIT_039) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_040) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_041) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-05'
*"     VALUE(POSNR_042) LIKE  BDCDATA-FVAL DEFAULT '0050'
*"     VALUE(LGORT_043) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_044) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_045) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_046) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_047) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_048) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-06'
*"     VALUE(POSNR_049) LIKE  BDCDATA-FVAL DEFAULT '0060'
*"     VALUE(LGORT_050) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_051) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_052) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_053) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_054) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_055) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-07'
*"     VALUE(POSNR_056) LIKE  BDCDATA-FVAL DEFAULT '0070'
*"     VALUE(LGORT_057) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_058) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_059) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_060) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_061) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_062) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-08'
*"     VALUE(POSNR_063) LIKE  BDCDATA-FVAL DEFAULT '0080'
*"     VALUE(LGORT_064) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_065) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_066) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_067) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_068) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_069) LIKE  BDCDATA-FVAL DEFAULT 'MZ-RM-R300-09'
*"     VALUE(POSNR_070) LIKE  BDCDATA-FVAL DEFAULT '0090'
*"     VALUE(LGORT_071) LIKE  BDCDATA-FVAL DEFAULT '171C'
*"     VALUE(WERKS_072) LIKE  BDCDATA-FVAL DEFAULT '1710'
*"     VALUE(MENGE_073) LIKE  BDCDATA-FVAL DEFAULT '36'
*"     VALUE(EINHEIT_074) LIKE  BDCDATA-FVAL DEFAULT 'PC'
*"     VALUE(SANKA_075) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"--------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLCOKO1' '0110'.
perform bdc_field       using 'BDC_CURSOR'
                              'CAUFVD-AUFNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=KPU2'.
perform bdc_field       using 'CAUFVD-AUFNR'
                              AUFNR_001.
perform bdc_dynpro      using 'SAPLCOMK' '0120'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-RGEKZ(09)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'FILTER_BOX'
                              FILTER_BOX_002.
perform bdc_field       using 'SORT_BOX'
                              SORT_BOX_003.
perform bdc_field       using 'RESBD-RGEKZ(01)'
                              RGEKZ_01_004.
perform bdc_field       using 'RESBD-RGEKZ(02)'
                              RGEKZ_02_005.
perform bdc_field       using 'RESBD-RGEKZ(03)'
                              RGEKZ_03_006.
perform bdc_field       using 'RESBD-RGEKZ(04)'
                              RGEKZ_04_007.
perform bdc_field       using 'RESBD-RGEKZ(05)'
                              RGEKZ_05_008.
perform bdc_field       using 'RESBD-RGEKZ(06)'
                              RGEKZ_06_009.
perform bdc_field       using 'RESBD-RGEKZ(07)'
                              RGEKZ_07_010.
perform bdc_field       using 'RESBD-RGEKZ(08)'
                              RGEKZ_08_011.
perform bdc_field       using 'RESBD-RGEKZ(09)'
                              RGEKZ_09_012.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_013.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_014.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_015.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_016.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_017.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_018.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_019.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_020.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_021.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_022.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_023.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_024.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_025.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_026.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_027.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_028.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_029.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_030.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_031.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_032.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_033.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_034.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_035.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_036.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_037.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_038.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_039.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_040.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_041.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_042.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_043.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_044.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_045.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_046.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_047.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_048.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_049.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_050.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_051.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_052.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_053.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_054.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_055.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_056.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_057.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_058.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_059.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_060.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_061.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_062.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_063.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_064.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_065.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_066.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_067.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_068.
perform bdc_dynpro      using 'SAPLCOMD' '0110'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'BDC_CURSOR'
                              'RESBD-MATNR'.
perform bdc_field       using 'RESBD-MATNR'
                              MATNR_069.
perform bdc_field       using 'RESBD-POSNR'
                              POSNR_070.
perform bdc_field       using 'RESBD-LGORT'
                              LGORT_071.
perform bdc_field       using 'RESBD-WERKS'
                              WERKS_072.
perform bdc_field       using 'RESBD-MENGE'
                              MENGE_073.
perform bdc_field       using 'RESBD-EINHEIT'
                              EINHEIT_074.
perform bdc_field       using 'RESBD-SANKA'
                              SANKA_075.
perform bdc_transaction tables messtab
using                         'CO02'
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
