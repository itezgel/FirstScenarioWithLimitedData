FUNCTION ZFM_CONFIRMPICK_SEK_COPY.
*"----------------------------------------------------------------------
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
*"     VALUE(VBELN_001) LIKE  BDCDATA-FVAL DEFAULT '80017768'
*"     VALUE(PIKMG_01_002) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(BLDAT_003) LIKE  BDCDATA-FVAL DEFAULT '30.04.2007'
*"     VALUE(WADAT_004) LIKE  BDCDATA-FVAL DEFAULT '02.05.2007'
*"     VALUE(BTGEW_005) LIKE  BDCDATA-FVAL DEFAULT '200'
*"     VALUE(GEWEI_006) LIKE  BDCDATA-FVAL DEFAULT 'G'
*"     VALUE(BLDAT_007) LIKE  BDCDATA-FVAL DEFAULT '30.04.2007'
*"     VALUE(WADAT_008) LIKE  BDCDATA-FVAL DEFAULT '02.05.2007'
*"     VALUE(BTGEW_009) LIKE  BDCDATA-FVAL DEFAULT '200'
*"     VALUE(GEWEI_010) LIKE  BDCDATA-FVAL DEFAULT 'G'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      ITEM STRUCTURE  BAPISDIT OPTIONAL
*"----------------------------------------------------------------------

data temp_pikmg type string.
data temp_line(2) type n.
data temp_quan(19).

subrc = 0.

perform bdc_nodata      using nodata.

perform open_group      using group user keep holddate ctu.

perform bdc_dynpro      using 'SAPMV50A' '4004'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'LIKP-VBELN'
                              vbeln_001.

perform bdc_dynpro      using 'SAPMV50A' '1000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=T\02'.
perform bdc_dynpro      using 'SAPMV50A' '1000'.
perform bdc_field       using 'BDC_OKCODE'
                              '=WABU_T'.

loop at item.
write item-req_qty to temp_quan.
condense temp_quan.
temp_line = sy-tabix.
concatenate 'LIPSD-PIKMG(' temp_line ')' into temp_pikmg.
perform bdc_field       using temp_pikmg
                              temp_quan.
endloop.

perform bdc_transaction tables messtab
using                         'VL02N'
                              ctu
                              mode
                              update.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     ctu.

ENDFUNCTION.
