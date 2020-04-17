;BLOCK 1 ############ INITIALIZE VARIABLES ################################################################
device,decompose=0   ;for proper color handling in hiphop on-screen plots

PRONAME='04.4_HEALTH_IMPACTS_FROM_HIRES_FASST_GRIDMAPS_GBD2017_CMIP6.PRO'

;fixed directories with data
ROOTDIR='D:\0 MY WORK_ONGOING\0 ACTION RESEARCH\2019\FASST\FASST_IDL_PACK_v4\'
ANCILDIR=rootdir+'ANCILLARY\'
GBD_DIR=ANCILDIR+'\MORTALITY\BASEMORT2018\'
RR_DIR='D:\0 MY WORK_ONGOING\0 ACTION RESEARCH\2019\FASST\FASST_IDL_PACK_V3\ANCILLARY\MORTALITY\RRs2018\FIT\'
IDLDIR='D:\0 MY WORK_ONGOING\0 ACTION RESEARCH\2019\FASST\FASST_IDL_PACK_V3\CODE\TEMPLATES\'
SSP_DIR=ANCILDIR+'POPULATION_SSP\NETCDF\'

;PROJECT-DEPENDENT IN/OUT FOLDERS
PROJECT='SSP-CMIP6'    ;give name for proper file labeling.
IERMODEL='IER2018'     ; IERs as downloaded from Aaron Cohen's links provided on 26/2/2019
ver='TEST'
OUTDIR=rootDIR+'FASST_OUTPUT\'+PROJECT+'\'  ;mainfolder for output
indir=outdir
tabdir=OUTDIR+'TABLES\'
if(~file_test(tabdir)) then file_mkdir,tabdir
ncdir=INDIR+'NCDF\'

;PROJECT-DEPENDENT SCENARIOS
SCEN=['SSP1_26']
SCENLAB=SCEN+'_'
YEAR=  ['2015']  ; Scenario years to be analyzed
SSP=   ['SSP1']  ;Defines population file - SSP array has same dimension as SCEN, each SCEN corresponds to a matching SSP

nSCEN=n_elements(SCEN)
nyr=n_elements(year)
nsp=n_elements(SSP)

if(nsp ne nscen) then STOP, 'SSP array not matching Scenario array dimension'

;DO NOT MODIFY FOLLOWING BLOCK:
      SSP_YRS=['2000','2010','2020','2030','2040','2050','2060','2070','2080','2090','2100'] ;AVAILABLE YEARS WITH SSP POPULATATION. IF NEEDED FOR SCENARIOS, INTERPOLATE BETWEEN THOSE YEARS
      SDM8THR=29.1  ;counterfactual level for SDMA8h for O3 health impact - GBD2017 uses between 29.1 and 35.7 for M3M. Malley/Turner use between 26.7 and 31.1
      ADM8THR=26.7 ;counterfactual level for ADMA8h for O3 health impact - Turner uses both 26.7 and 31.1

      ;risk function parameters
      RRFUNC='RRATE'
      RR_PARAM_FILE='RR_ALL_GBD_2017_FITTINGS_ANALYT.csv'

      ;AGE CLASSES TO CONSIDER:
      AGES=INDGEN(21)*5

      AGEFRAC_COPD=[0,100] & C1=WHERE(AGES EQ AGEFRAC_COPD[0]) &  C2=WHERE(AGES EQ AGEFRAC_COPD[1])  ;ALL AGES FROM MIN TO MAX    (MAX 100)
      AGEFRAC_LC=[0,100]   & L1=WHERE(AGES EQ AGEFRAC_LC[0]) &  L2=WHERE(AGES EQ AGEFRAC_LC[1])      ;ALL AGES FROM MIN TO MAX       (MAX 100)
      AGEFRAC_LRI=[0,100]   & LR1=WHERE(AGES EQ AGEFRAC_LRI[0]) &  LR2=WHERE(AGES EQ AGEFRAC_LRI[1])  ;ALL AGES FROM MIN TO MAX  (MAX 100)
      AGEFRAC_DMT2=[0,100]   & DM1=WHERE(AGES EQ AGEFRAC_DMT2[0]) &  DM2=WHERE(AGES EQ AGEFRAC_DMT2[1])  ;ALL AGES FROM MIN TO MAX  (MAX 100)
      AGEFRAC_IHD=[25,95]  & IH1=WHERE(AGES EQ AGEFRAC_IHD[0]) &  IH2=WHERE(AGES EQ AGEFRAC_IHD[1])  ;AGE SPECIFIC FROM MIN TO MAX (MAX 95, BECAUSE AGE-SPECIFIC RR AVAILABLE TILL 95)

      ;OZONE MORTALITIES - NOTE that mortality rates in GBD are expressed as number divided by total population. COPD mortalitiy rates below 25 are zero.
      AGEFRAC_O3=[0,100] & O1=WHERE(AGES EQ AGEFRAC_O3[0]) &  O2=WHERE(AGES EQ AGEFRAC_O3[1]) ;ALL AGES FROM MIN TO MAX (MAX 100)

      restore,IDLdir+'mort_templ_V2.sav'   ;template for mortality rate files
      restore,IDLdir+'RR2017TEMPL.SAV'     ;template for fitted params for risk rates (LC, COPD, LRI all ages aggregated)
      restore,IDLdir+'pop_templ.sav'       ;template for population file (total and 20 agegroups)

      COD1=['COPD','LC','LRI','DMT2']  ;CODs for which RR is aggregated over all age groups
      COD2=['IHD','STROKE']            ;CODs for which RR is age group sepcific

      AGE_GRP=strtrim(STRING(indgen(15)*5+25),2)       ; Do not change; generates string array of AGE GROUP NAMES (25 TO 95 IN 5 YEAR BINS) 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95
      nage=n_elements(age_grp)

      MORT_YEARS=strtrim(STRING(indgen(11)*5+1990),2)  ;DO NOT CHANGE;  AVAILABLE YEARS OF STATS FOR MORTALITIES: 1990 - 2040.
      POP_YEARS=strtrim(STRING(indgen(23)*5+1990),2)   ;DO NOT CHANGE;  AVAILABLE YEARS OF POP AGE STRUCTURE DATA: 1990 - 2100 (UN 2017 REVISION, MEDIUM VARIANT FOR PROJECTED).

; END OF BLOCK NOT TO BE MODIFIED

;BLOCK 2 ##############  load population, base mortality data and risk rate function parameters #####################################################
; note: available population totals from SSP: 2000, 2010, 2020, 2030, ..., 2100
; available base mortalities and fraction of pop <5yr and >30yr: 2005 2010 2015 2030 2050.
; If other years are needed, an additional interpolation is carried out on the latter. For years > 2040 mortality stats for 2040 are used. FOr years < 2005, mortality stats for 2005 are used.

;Population country totals
POPFILE=GBD_DIR+'POP_1990-2100_UN2017_AGEGRP.csv'; POP data UN 2017 revision, medium variant projections till 2100 (per 1000)
POP=READ_ASCII(POPFILE,TEMPLATE=POP_TEMPL)

;generate indices of list with unique country codes and names from POP file
clist=uniq(pop.cntr_id)
ncntr=n_elements(clist)
cntr_id=pop.cntr_id[clist]
cntr_nm=pop.cntr_name[clist]
cntr_iso=pop.cntr_iso3[clist]

;Read country identification gridmap (Ciesin GPW v4)
restore,ancildir+'CIESIN_COUNTRY_MASK\cntr_id_grid_templ.sav'
CNTRGRID=read_ascii(ancildir+'CIESIN_COUNTRY_MASK\CIESIN_V4\15minx15min\gpw_v4_national_identifier_grid_rev10_15_min.asc',template=cntr_id_grid_templ)
CGRID=reverse(cntrgrid.code,2)
latmin=-60
latmax=latmin+580./4
CNTRCODE=intarr(1440,720)

LATFUL=(indgen(720)-360)/4.
LONFUL=(indgen(1440)-720)/4.
indmin=where(latful eq latmin)
indmax=where(latful eq latmax)

CNTRCODE[0:1439,indmin:indmax-1]=CGRID   ;UN country codes
HRCNTRCODE=CONGRID(CNTRCODE,2880,1440)

COPD_FILE=GBD_DIR+'COPD_MORT_RATE_GBD2016.csv'   ;files with base mortality rates (per 100k population)
LC_FILE=GBD_DIR+'LC_MORT_RATE_GBD2016.csv'
DMT2_FILE=GBD_DIR+'DMT2_MORT_RATE_GBD2016.csv'
LRI_FILE=GBD_DIR+'LRI_MORT_RATE_GBD2016.csv'
IHD_FILE=GBD_DIR+'IHD_MORT_RATE_GBD2016.csv'
STROKE_FILE=GBD_DIR+'STROKE_MORT_RATE_GBD2016.csv'

COPD=READ_ASCII(COPD_FILE,TEMPLATE=MORT_TEMPL)
LC=READ_ASCII(LC_FILE,TEMPLATE=MORT_TEMPL)
LRI=READ_ASCII(LRI_FILE,TEMPLATE=MORT_TEMPL)
IHD=READ_ASCII(IHD_FILE,TEMPLATE=MORT_TEMPL)
STROKE=READ_ASCII(STROKE_FILE,TEMPLATE=MORT_TEMPL)
DMT2=READ_ASCII(DMT2_FILE,TEMPLATE=MORT_TEMPL)

;Read the fitting parameters for the Burnett IER functions for all CODs and age classes
RR=read_ascii(RR_DIR+RR_PARAM_FILE,TEMPLATE=RRTEMPL)
RR_TAGS=tag_names(RR)

COPD_MED=RR.RRMED(WHERE((RR.COD EQ 'COPD') AND (RR.AGE EQ 99)))
COPD_LO=RR.RRLO(WHERE((RR.COD EQ 'COPD') AND (RR.AGE EQ 99)))
COPD_HI=RR.RRHI(WHERE((RR.COD EQ 'COPD') AND (RR.AGE EQ 99)))

LRI_MED=RR.RRMED(WHERE((RR.COD EQ 'LRI') AND (RR.AGE EQ 99)))
LRI_LO=RR.RRLO(WHERE((RR.COD EQ 'LRI') AND (RR.AGE EQ 99)))
LRI_HI=RR.RRHI(WHERE((RR.COD EQ 'LRI') AND (RR.AGE EQ 99)))

LC_MED=RR.RRMED(WHERE((RR.COD EQ 'LC') AND (RR.AGE EQ 99)))
LC_LO=RR.RRLO(WHERE((RR.COD EQ 'LC') AND (RR.AGE EQ 99)))
LC_HI=RR.RRHI(WHERE((RR.COD EQ 'LC') AND (RR.AGE EQ 99)))

DT2_MED=RR.RRMED(WHERE((RR.COD EQ 'DT2') AND (RR.AGE EQ 99)))
DT2_LO=RR.RRLO(WHERE((RR.COD EQ 'DT2') AND (RR.AGE EQ 99)))
DT2_HI=RR.RRHI(WHERE((RR.COD EQ 'DT2') AND (RR.AGE EQ 99)))

IHD_MED=FLTARR(4,nage)
IHD_LO=FLTARR(4,nage)
IHD_HI=FLTARR(4,nage)
STROKE_MED=FLTARR(4,nage)
STROKE_LO=FLTARR(4,nage)
STROKE_HI=FLTARR(4,nage)

;extract the appropriate RR parameters for each COD and assign to each variable - for easier tracking.
for iage=0,nage-1 do begin
  IHD_MED[*,iage]=RR.RRMED(WHERE((RR.COD EQ 'IHD') AND (RR.AGE EQ age_grp[iage])))
  IHD_LO[*,iage]=RR.RRLO(WHERE((RR.COD EQ 'IHD') AND (RR.AGE EQ age_grp[iage])))
  IHD_HI[*,iage]=RR.RRHI(WHERE((RR.COD EQ 'IHD') AND (RR.AGE EQ age_grp[iage])))
  STROKE_MED[*,iage]=RR.RRMED(WHERE((RR.COD EQ 'STROKE') AND (RR.AGE EQ age_grp[iage])))
  STROKE_LO[*,iage]=RR.RRLO(WHERE((RR.COD EQ 'STROKE') AND (RR.AGE EQ age_grp[iage])))
  STROKE_HI[*,iage]=RR.RRHI(WHERE((RR.COD EQ 'STROKE') AND (RR.AGE EQ age_grp[iage])))
endfor

RR_COPD_TU=FLTARR(3)
RR_COPD_GBD=FLTARR(3)

;Ozone RRs with log-lin ER function
RR_COPD_TU[0]=1.14    ;new TURNER!, for new exposure metric annual mean of daily 8h max!
RR_COPD_TU[1]=1.08    ;new TURNER
RR_COPD_TU[2]=1.21    ;new TURNER

RR_COPD_GBD[0]=1.06   ;new GBD2017!, for new exposure metric 6-month mean of daily 8h max!
RR_COPD_GBD[1]=1.05   ;new GBD
RR_COPD_GBD[2]=1.10   ;new GBD

;###BETA = LN(RR)/10
BETA_COPD_TU=ALOG(RR_COPD_TU)/10.
BETA_COPD_GBD=ALOG(RR_COPD_GBD)/10.

;OPEN TXT output file and write headers
openw,6,tabdir+'ALLCNTRIES_'+project+'_'+IERMODEL+'_'+VER+'.TXT'
printf,6,'NEW GBD2017 IER'
printf,6,'programme: '+PRONAME
printf,6,'date of run: ',systime()
printf,6,format='(A18,F5.2)','GBD 6MDMA8H threshold = ',sDM8THR
printf,6,format='(A18,F5.2)','TURNER ADMA8H threshold = ',ADM8THR
printf,6,format='(2(A18))','IER MODEL:', IERMODEL
printf,6,format='(a150)','AGE CLASSES:'
printf,6,format='(3(A18))','COPD: ',STRING(AGEFRAC_COPD)
printf,6,format='(3(A18))','LC: ',STRING(AGEFRAC_LC)
printf,6,format='(3(A18))','LRI: ',STRING(AGEFRAC_LRI)
printf,6,format='(3(A18))','IHD + STROKE: ',STRING(AGEFRAC_IHD)
printf,6,format='(3(A18))','COPD O3: ',STRING(AGEFRAC_O3)
Printf,6, format='(2(A15),A6,A5,33(A20))','SCENARIO','SSP','YEAR','ISO','COUNTRY','POP_CNT', 'POPW_PM2.5_TOT_35%','POPW_NAT_PM25_35%', 'POPW_ADM8h','POPW_SDMA8h', $
                                     'MORT_PM_6COD_M','MORT_PM_6COD_L','MORT_PM_6COD_H', $
                                     'MORT_PM_COPD_M','MORT_PM_LC_M','MORT_PM_LRI_M','MORT_PM_DMT2_M','MORT_PM_IHD_M','MORT_PM_STROKE_M',$
                                     'MORT_PM_COPD_L','MORT_PM_LC_L','MORT_PM_LRI_L','MORT_PM_DMT2_L','MORT_PM_IHD_L','MORT_PM_STROKE_L',$
                                     'MORT_PM_COPD_H','MORT_PM_LC_H','MORT_PM_LRI_H','MORT_PM_DMT2_H','MORT_PM_IHD_H','MORT_PM_STROKE_H',$
                                     'MORT_O3_COPD_GBD_M','MORT_O3_COPD_GBD_L','MORT_O3_COPD_GBD_H',$
                                     'MORT_O3_COPD_TUR_M','MORT_O3_COPD_TUR_L','MORT_O3_COPD_TUR_H'
;load FASST countries masks
restore, ancildir+'FASST_REGION_MASK\country_mask_0.5x0.5_v3.sav'

;high resolution lon lat dimensions
img=2880
jmg=1440
SCALE=FLOAT(IMG)/360.
hr_lats=double((findgen(180*SCALE)+0.5)/SCALE-90.)
hr_lons=double((findgen(360*SCALE)+0.5)/SCALE-180.)

;med resolution lon lat dimensions
imed=720
jmed=360
SCALE=FLOAT(imed)/360.
mr_lats=double((findgen(180*SCALE)+0.5)/SCALE-90.)
mr_lons=double((findgen(360*SCALE)+0.5)/SCALE-180.)

; lon-lat arrays
hr_grid_tot=fltarr(IMG,JMG)*0.
hr_grid_tot1=fltarr(IMG,JMG)*0.    ;intermediate placeholders needed when interpolating
hr_grid_tot2=fltarr(IMG,JMG)*0.    ;intermediate placeholders needed when interpolating
scenpopmask=hr_grid_tot*0.
scenpop=hr_grid_tot*0.
iminlat=273   ;index where hr_lats eq min lat in SSP hr grid
imaxlat=1389  ;index where hr_lats eq max lat in SSP hr grid

;layers containing the AFs, depends on PM fields
AF_COPD_GRID=fltarr(3,2880,1440)       ;ALL AGES >25
AF_LRI_GRID=fltarr(3,2880,1440)        ;ALL AGES
AF_LC_GRID=fltarr(3,2880,1440)         ;ALL AGES >25
AF_DMT2_GRID=fltarr(3,2880,1440)       ;ALL AGES >25
AF_IHD_GRID=fltarr(3,15,2880,1440)     ;15 AGE CLASSES >25
AF_STROKE_GRID=fltarr(3,15,2880,1440)  ;15 AGE CLASSES >25

;BLOCK 3 ############ START LOOP WITH SCENARIO ANALYSIS  - EACH LOOP = 1 SCENARIO #################

for isc=0,nSCEN-1 do begin   ; loop scenarios
  for iyr=0,nyr-1 do begin   ; loop years

        ;CHECK IF SSP POPULATION YEAR IS AVAILABLE, IF NOT:INTERPOLATE BETWEEN AVAILABLE YEARS
        INTPOL=0
        NPOP=MAX(WHERE(FIX(SSP_YRS) LE YEAR[IYR]))
        JPOP=MIN(WHERE(FIX(SSP_YRS) GE YEAR[IYR]))
        IF NPOP NE JPOP THEN BEGIN
          INTPOL=1    ;Set interpolation flag
          FYR=(FLOAT(YEAR[IYR])-FLOAT(SSP_YRS[NPOP]))/(FLOAT(SSP_YRS[JPOP])-FLOAT(SSP_YRS[NPOP]))
        ENDIF

       ;restore HIGH RESOLUTION (HIRES) population map(s) and interpolate if needed
             dir=SSP_DIR+SSP[ISC]+'_NETCDF\'
             IF NOT(INTPOL) THEN BEGIN
                   totfil=dir+'total\netcdf\'+ssp[ISC]+'_'+YEAR[iyr]+'.nc'
                   if (YEAR[iyr] eq '2000') then begin              ;the SSP files for year 2000 have tagname for gridmap that starts with a number (2000) which is not allowed in ncdf_read -> use different procedure
                     ncdf_get,totfil,['2000total','lat','lon'],TOT,/struct
                     hr_grid_tot[*,iminlat:imaxlat]=reverse(tot._2000total.value,2)
                   endif else begin
                     ncdf_read,tot,file=totfil,/all
                     hr_grid_tot[*,iminlat:imaxlat]=reverse(TOT.(2),2)
                   endelse
             ENDIF ELSE BEGIN ; interpolation
                   totfil1=dir+'total\netcdf\'+ssp[ISC]+'_'+SSP_YRS[NPOP]+'.nc'
                   if (SSP_YRS[NPOP] eq '2000') then begin     ;the SSP files for year 2000 have tagname for gridmap that starts with a number (2000) which is not allowed in ncdf_read -> use different procedure
                     ncdf_get,totfIL1,['2000total','lat','lon'],TOT1,/struct
                     hr_grid_tot1[*,iminlat:imaxlat]=reverse(tot1._2000total.value,2)
                   endif else begin
                     ncdf_read,tot1,file=totfil1,/all
                     hr_grid_tot1[*,iminlat:imaxlat]=reverse(TOT1.(2),2)
                   endelse
                   totfil2=dir+'total\netcdf\'+ssp[ISC]+'_'+SSP_YRS[JPOP]+'.nc'
                   if (SSP_YRS[JPOP] eq '2000') then begin     ;the SSP files for year 2000 have tagname for gridmap that starts with a number (2000) which is not allowed in ncdf_read -> use different procedure
                     ncdf_get,totfil2,['2000total','lat','lon'],TOT2,/struct
                     hr_grid_tot2[*,iminlat:imaxlat]=reverse(tot2._2000total.value,2)
                   endif else begin
                     ncdf_read,tot2,file=totfil2,/all
                     hr_grid_tot2[*,iminlat:imaxlat]=reverse(TOT2.(2),2)
                   endelse
                hr_grid_tot= hr_grid_tot1+FYR*(hr_grid_tot2-hr_grid_tot1)

              ENDELSE

         no_data = 2147483647.0
         ipop = where (((hr_grid_tot[*,*] gt 0) and (double(hr_grid_tot[*,*]) lt double(no_data))),count)  ;identify grids with valid population data
         scenpop[ipop]=hr_grid_tot[ipop]
         scenpopmask[ipop]=1.


; Read the scenario input file (high resolution grid map)
         scendir=    SCEN[isc]+'\'
         scenario =  scen[isc]+'_'+YEAR[IYR]
         infile=ncDIR+scendir+'FASST_75x75_'+scen[isc]+'_'+YEAR[IYR]+'.nc'

          if not(file_test(infile)) then begin
                  print, 'file not found: '+infile
                  stop
          endif

          ncdf_read, SC,/all,filename=infile
          print, 'processing...'+infile

         	SC_HIRES=sc.tot_pm_35      ;extract total pm from SC structure and load into SC_HIRES variable
         	SC_ANTH_HIRES=sc.ant_pm_35 ;extract anthropogenic pm from SC structure and load into SC_HIRES variable

          print, 'calculate AFs @'+systime()

 ; Calculate attributable fractions AF = 1-1/RR for central values, low and high confidence interval bound
          AF_COPD_GRID[0,*,*]= 1.-1/RRATE(COPD_MED,SC_HIRES)
          AF_COPD_GRID[1,*,*]= 1.-1/RRATE(COPD_LO,SC_HIRES)
          AF_COPD_GRID[2,*,*]= 1.-1/RRATE(COPD_HI,SC_HIRES)

          SIG_MIN_AF_COPD=AF_COPD_GRID[0,*,*]*sqrt(((RRATE(COPD_MED,SC_HIRES)-RRATE(COPD_LO,SC_HIRES))/RRATE(COPD_Med,SC_HIRES))^2)
          SIG_MAX_AF_COPD=AF_COPD_GRID[0,*,*]*sqrt(((RRATE(COPD_MED,SC_HIRES)-RRATE(COPD_HI,SC_HIRES))/RRATE(COPD_Med,SC_HIRES))^2)

          AF_LC_GRID[0,*,*]= 1.-1/RRATE(LC_MED,SC_HIRES)
          AF_LC_GRID[1,*,*]= 1.-1/RRATE(LC_LO,SC_HIRES)
          AF_LC_GRID[2,*,*]= 1.-1/RRATE(LC_HI,SC_HIRES)

          SIG_MIN_AF_LC=AF_LC_GRID[0,*,*]*sqrt(((RRATE(LC_MED,SC_HIRES)-RRATE(LC_LO,SC_HIRES))/RRATE(LC_Med,SC_HIRES))^2)
          SIG_MAX_AF_LC=AF_LC_GRID[0,*,*]*sqrt(((RRATE(LC_MED,SC_HIRES)-RRATE(LC_HI,SC_HIRES))/RRATE(LC_Med,SC_HIRES))^2)

          AF_LRI_GRID[0,*,*]= 1.-1/RRATE(LRI_MED,SC_HIRES)
          AF_LRI_GRID[1,*,*]= 1.-1/RRATE(LRI_LO,SC_HIRES)
          AF_LRI_GRID[2,*,*]= 1.-1/RRATE(LRI_HI,SC_HIRES)

          SIG_MIN_AF_LRI=AF_LRI_GRID[0,*,*]*sqrt(((RRATE(LRI_MED,SC_HIRES)-RRATE(LRI_LO,SC_HIRES))/RRATE(LRI_Med,SC_HIRES))^2)
          SIG_MAX_AF_LRI=AF_LRI_GRID[0,*,*]*sqrt(((RRATE(LRI_MED,SC_HIRES)-RRATE(LRI_HI,SC_HIRES))/RRATE(LRI_Med,SC_HIRES))^2)

          AF_DMT2_GRID[0,*,*]= 1.-1/RRATE(DT2_MED,SC_HIRES)
          AF_DMT2_GRID[1,*,*]= 1.-1/RRATE(DT2_LO,SC_HIRES)
          AF_DMT2_GRID[2,*,*]= 1.-1/RRATE(DT2_HI,SC_HIRES)

          SIG_MIN_AF_DMT2=AF_DMT2_GRID[0,*,*]*sqrt(((RRATE(DT2_MED,SC_HIRES)-RRATE(DT2_LO,SC_HIRES))/RRATE(DT2_MED,SC_HIRES))^2)
          SIG_MAX_AF_DMT2=AF_DMT2_GRID[0,*,*]*sqrt(((RRATE(DT2_MED,SC_HIRES)-RRATE(DT2_HI,SC_HIRES))/RRATE(DT2_MED,SC_HIRES))^2)

          SIG_MIN_AF_IHD=FLTARR(2880,1440)
          SIG_MAX_AF_IHD=FLTARR(2880,1440)
          SIG_MIN_AF_STROKE=FLTARR(2880,1440)
          SIG_MAX_AF_STROKE=FLTARR(2880,1440)

          for iage=0,nage-1 do begin
            AF_IHD_GRID[0,iage,*,*]=1.-1./RRATE(IHD_MED[*,iage],SC_HIRES)
            AF_IHD_GRID[1,iage,*,*]=1.-1./RRATE(IHD_LO[*,iage],SC_HIRES)
            AF_IHD_GRID[2,iage,*,*]=1.-1./RRATE(IHD_HI[*,iage],SC_HIRES)
            AF_STROKE_GRID[0,iage,*,*]=1.-1./RRATE(STROKE_MED[*,iage],SC_HIRES)
            AF_STROKE_GRID[1,iage,*,*]=1.-1./RRATE(STROKE_LO[*,iage],SC_HIRES)
            AF_STROKE_GRID[2,iage,*,*]=1.-1./RRATE(STROKE_HI[*,iage],SC_HIRES)

            SIG_MIN_AF_IHD=SIG_MIN_AF_IHD+((RRATE(IHD_MED[*,IAGE],SC_HIRES)-RRATE(IHD_LO[*,IAGE],SC_HIRES))/RRATE(IHD_MED[*,IAGE],SC_HIRES))^2
            SIG_MAX_AF_IHD=SIG_MAX_AF_IHD+((RRATE(IHD_MED[*,IAGE],SC_HIRES)-RRATE(IHD_HI[*,IAGE],SC_HIRES))/RRATE(IHD_MED[*,IAGE],SC_HIRES))^2
            SIG_MIN_AF_STROKE=SIG_MIN_AF_STROKE+((RRATE(STROKE_MED[*,IAGE],SC_HIRES)-RRATE(STROKE_LO[*,IAGE],SC_HIRES))/RRATE(STROKE_MED[*,IAGE],SC_HIRES))^2
            SIG_MAX_AF_STROKE=SIG_MAX_AF_STROKE+((RRATE(STROKE_MED[*,IAGE],SC_HIRES)-RRATE(STROKE_HI[*,IAGE],SC_HIRES))/RRATE(STROKE_MED[*,IAGE],SC_HIRES))^2
          endfor
            SIG_MIN_AF_IHD=SQRT(SIG_MIN_AF_IHD)
            SIG_MAX_AF_IHD=SQRT(SIG_MAX_AF_IHD)
            SIG_MIN_AF_stroke=SQRT(SIG_MIN_AF_STROKE)
            SIG_MAX_AF_STROKE=SQRT(SIG_MIN_AF_STROKE)

; BLOCK 3a ########### BASE INCIDENCES (MED,LO,UP). Will be calculated and stored if gridmap was not previously stored (for particular year). Otherwise retrieve stored files.
        mordir=ANCILDIR+'\MORTALITY\GBD_2017_BASEMORT_GRIDMAPS\'

        if (file_test(mordir+'MRATE_COPD_GBD2017_'+year[iyr]+'.sav')) then begin
            restore, mordir+'MRATE_COPD_GBD2017_'+year[iyr]+'.sav'
            restore, mordir+'MRATE_LC_GBD2017_'+year[iyr]+'.sav'
            restore, mordir+'MRATE_LRI_GBD2017_'+year[iyr]+'.sav'
            restore, mordir+'MRATE_DMT2_GBD2017_'+year[iyr]+'.sav'
            restore, mordir+'MRATE_IHD_GBD2017_'+year[iyr]+'.sav'
            restore, mordir+'MRATE_STROKE_GBD2017_'+year[iyr]+'.sav'
        endif else begin

            MRATE_COPD0=FLTARR(2880,1440)
            MRATE_COPD1=FLTARR(2880,1440)
            MRATE_COPD2=FLTARR(2880,1440)

            MRATE_LC0=FLTARR(2880,1440)
            MRATE_LC1=FLTARR(2880,1440)
            MRATE_LC2=FLTARR(2880,1440)

            MRATE_LRI0=FLTARR(2880,1440)
            MRATE_LRI1=FLTARR(2880,1440)
            MRATE_LRI2=FLTARR(2880,1440)

            MRATE_DMT20=FLTARR(2880,1440)
            MRATE_DMT21=FLTARR(2880,1440)
            MRATE_DMT22=FLTARR(2880,1440)

            MRATE_IHD0=FLTARR(2880,1440)
            MRATE_IHD1=FLTARR(2880,1440)
            MRATE_IHD2=FLTARR(2880,1440)

            MRATE_STROKE0=FLTARR(2880,1440)
            MRATE_STROKE1=FLTARR(2880,1440)
            MRATE_STROKE2=FLTARR(2880,1440)

            ;lOOP THROUGH COUNTRIES; RETRIEVE BASE MORTALITY RATES AND MAP TO EACH OF THE 3 GRID LAYERS (MED,LO,UP)

             for icntr=0,ncntr-1 do begin
                  ;COUNTRY IDENTIFICATION
                  imask=where(hrcntrcode eq  cntr_id[icntr])
                  if(cntr_id[icntr] eq 736) then imask=where(hrcntrcode eq 729)    ;new code for Sudan since splitting South Sudan

                  ;BASE INCIDENCE RATE FOR EACH COD IN CURRENT COUNTRY AND YEAR, available till 2040
                  if year[iyr] gt max(COPD.year) then begin
                    maxyear=max(copd.year)
                    iCOPD=where((COPD.year eq maxyear)*(strupcase(COPD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                    iLC=where((LC.year eq maxyear)*(strupcase(LC.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                    iLRI=where((LRI.year eq maxyear)*(strupcase(LRI.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                    iDM=where((DMt2.year eq maxyear)*(strupcase(dmt2.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                    MRATE_COPD0[imask]=COPD.val[iCOPD]
                    MRATE_COPD1[imask]=copd.lo[iCOPD]
                    MRATE_COPD2[imask]=COPD.HI[iCOPD]
                    MRATE_LC0[imask]=LC.val[iLC]
                    MRATE_LC1[imask]=LC.lo[iLC]
                    MRATE_LC2[imask]=LC.HI[iLC]
                    MRATE_LRI0[imask]=LRI.val[iLRI]
                    MRATE_LRI1[imask]=LRI.lo[iLRI]
                    MRATE_LRI2[imask]=LRI.HI[iLRI]
                    MRATE_DMT20[imask]=DMT2.val[idm]
                    MRATE_DMT21[imask]=DMT2.lo[idm]
                    MRATE_DMT22[imask]=DMT2.HI[idm]

                  endif else begin
                      if year[iyr] lt min(COPD.year) then begin
                          minyear=min(copd.year)
                          iCOPD=where((COPD.year eq minyear)*(strupcase(COPD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                          iLC=where((LC.year eq minyear)*(strupcase(LC.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                          iLRI=where((LRI.year eq minyear)*(strupcase(LRI.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                          iDM=where((DMt2.year eq minyear)*(strupcase(dmt2.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                          MRATE_COPD0[imask]=COPD.val[iCOPD]
                          MRATE_COPD1[imask]=copd.lo[iCOPD]
                          MRATE_COPD2[imask]=COPD.HI[iCOPD]
                          MRATE_LC0[imask]=LC.val[iLC]
                          MRATE_LC1[imask]=LC.lo[iLC]
                          MRATE_LC2[imask]=LC.HI[iLC]
                          MRATE_LRI0[imask]=LRI.val[iLRI]
                          MRATE_LRI1[imask]=LRI.lo[iLRI]
                          MRATE_LRI2[imask]=LRI.HI[iLRI]
                          MRATE_DMT20[imask]=DMT2.val[idm]
                          MRATE_DMT21[imask]=DMT2.lo[idm]
                          MRATE_DMT22[imask]=DMT2.HI[idm]

                      endif else begin
                        iCOPD=where(COPD.year eq year[IYR])
                        if (icopd eq -1) then begin  ;if current year not available interpolate between available years
                            ILO=MAX(WHERE(COPD.YEAR LE YEAR[IYR])) & YEARLO=COPD.YEAR[ILO]
                            IHI=MIN(WHERE(COPD.YEAR GE YEAR[IYR])) & YEARHI=COPD.YEAR[IHI]
                            iCOPD1=where((COPD.year eq yearlo)*(strupcase(COPD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                            iLC1=where((LC.year eq yearlo)*(strupcase(LC.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            iLRI1=where((LRI.year eq yearlo)*(strupcase(LRI.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            idm1=where((DMT2.year eq yearlo)*(strupcase(DMT2.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            iCOPD2=where((COPD.year eq yearhi)*(strupcase(COPD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                            iLC2=where((LC.year eq yearhi)*(strupcase(LC.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            iLRI2=where((LRI.year eq yearhi)*(strupcase(LRI.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            idm2=where((DMT2.year eq yearhi)*(strupcase(DMT2.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            MRATE_COPD0[imask]=COPD.val[iCOPD1]+(COPD.val[iCOPD2]-COPD.val[iCOPD1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_COPD1[imask]=COPD.LO[iCOPD1]+(COPD.LO[iCOPD2]-COPD.LO[iCOPD1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_COPD2[imask]=COPD.HI[iCOPD1]+(COPD.HI[iCOPD2]-COPD.HI[iCOPD1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LC0[imask]=LC.val[iLC1]+(LC.val[iLC2]-LC.val[iLC1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LC1[imask]=LC.LO[iLC1]+(LC.LO[iLC2]-LC.LO[iLC1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LC2[imask]=LC.HI[iLC1]+(LC.HI[iLC2]-LC.HI[iLC1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LRI0[imask]=LRI.val[iLRI1]+(LRI.val[iLRI2]-LRI.val[iLRI1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LRI1[imask]=LRI.LO[iLRI1]+(LRI.LO[iLRI2]-LRI.LO[iLRI1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_LRI2[imask]=LRI.HI[iLRI1]+(LRI.HI[iLRI2]-LRI.HI[iLRI1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_DMT20[imask]=DMT2.val[IDM1]+(DMT2.val[IDM2]-DMT2.val[IDM1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_DMT21[imask]=DMT2.LO[IDM1]+(DMT2.LO[IDM2]-DMT2.LO[IDM1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_DMT22[imask]=DMT2.HI[IDM1]+(DMT2.HI[IDM2]-DMT2.HI[IDM1])/(yearhi-yearlo)*(year[iyr]-yearlo)
                         ENDIF ELSE BEGIN ;no interpolation needed
                            iCOPD=where((COPD.year eq year[IYR])*(strupcase(COPD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                            iLC=where((LC.year eq year[IYR])*(strupcase(LC.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            iLRI=where((LRI.year eq year[IYR])*(strupcase(LRI.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            idm=where((DMT2.year eq year[IYR])*(strupcase(DMT2.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            MRATE_COPD0[imask]=COPD.val[iCOPD]
                            MRATE_COPD1[imask]=copd.lo[iCOPD]
                            MRATE_COPD2[imask]=COPD.HI[iCOPD]
                            MRATE_LC0[imask]=LC.val[iLC]
                            MRATE_LC1[imask]=LC.lo[iLC]
                            MRATE_LC2[imask]=LC.HI[iLC]
                            MRATE_LRI0[imask]=LRI.val[iLRI]
                            MRATE_LRI1[imask]=LRI.lo[iLRI]
                            MRATE_LRI2[imask]=LRI.HI[iLRI]
                            MRATE_DMT20[imask]=DMT2.val[idm]
                            MRATE_DMT21[imask]=DMT2.lo[idm]
                            MRATE_DMT22[imask]=DMT2.HI[idm]
                         endelse
                      ENDELSE
                 ENDELSE

             next_COUNTRY:
             ENDFOR   ;cntry list

                  MRATE_COPD=FLTARR(3,2880,1440)
                  MRATE_COPD[0,*,*]=MRATE_COPD0
                  MRATE_COPD[1,*,*]=MRATE_COPD1
                  MRATE_COPD[2,*,*]=MRATE_COPD2

                  MRATE_LC=FLTARR(3,2880,1440)
                  MRATE_LC[0,*,*]=MRATE_LC0
                  MRATE_LC[1,*,*]=MRATE_LC1
                  MRATE_LC[2,*,*]=MRATE_LC2

                  MRATE_LRI=FLTARR(3,2880,1440)
                  MRATE_LRI[0,*,*]=MRATE_LRI0
                  MRATE_LRI[1,*,*]=MRATE_LRI1
                  MRATE_LRI[2,*,*]=MRATE_LRI2

                  MRATE_DMT2=FLTARR(3,2880,1440)
                  MRATE_DMT2[0,*,*]=MRATE_DMT20
                  MRATE_DMT2[1,*,*]=MRATE_DMT21
                  MRATE_DMT2[2,*,*]=MRATE_DMT22

                  MRATE_IHD=FLTARR(3,15,2880,1440)*0.
                  MRATE_STROKE=FLTARR(3,15,2880,1440)*0.

              FOR ICL=0,14 DO BEGIN         ;15 AGE CLASSES FOR IHD AND STROKE ONLY
                PRINT, ICL
                FOR icntr=0,ncntr-1 do begin
                      ;COUNTRY IDENTIFICATION
                      imask=where(hrcntrcode eq  cntr_id[icntr])
                      if(cntr_id[icntr] eq 736) then imask=where(hrcntrcode eq 729)

                      ;BASE INCIDENCE RATE FOR EACH COD IN CURRENT COUNTRY AND YEAR
                      if year[iyr] gt max(IHD.year) then begin
                        maxyear=max(IHD.year)
                        iIHD=where((IHD.year eq maxyear)*(strupcase(IHD.cntr_NAME) eq strupcase(cntr_nm[icntr])))           ;15 VALUES FOR AGE CLASSES
                        iSTROKE=where((STROKE.year eq maxyear)*(strupcase(STROKE.cntr_NAME) eq strupcase(cntr_nm[icntr])))  ;15 VALUES FOR AGE CLASSES
                        IF ((MIN(IIHD) EQ -1) OR (MIN(ISTROKE) EQ -1)) THEN GOTO,next_COUNTRY2
                        MRATE_IHD0[imask]=IHD.val[iIHD[ICL]]
                        MRATE_IHD1[imask]=IHD.LO[iIHD[ICL]]
                        MRATE_IHD2[imask]=IHD.HI[iIHD[ICL]]

                        MRATE_STROKE0[imask]=STROKE.val[iSTROKE[ICL]]
                        MRATE_STROKE1[imask]=STROKE.LO[iSTROKE[ICL]]
                        MRATE_STROKE2[imask]=STROKE.HI[iSTROKE[ICL]]

                      endif else begin
                        if year[iyr] Lt MIN(IHD.year) then begin
                          MINyear=mIN(IHD.year)
                          iIHD=where((IHD.year eq MINyear)*(strupcase(IHD.cntr_NAME) eq strupcase(cntr_nm[icntr])))           ;15 VALUES FOR AGE CLASSES
                          iSTROKE=where((STROKE.year eq MINyear)*(strupcase(STROKE.cntr_NAME) eq strupcase(cntr_nm[icntr])))  ;15 VALUES FOR AGE CLASSES
                          IF ((MIN(IIHD) EQ -1) OR (MIN(ISTROKE) EQ -1)) THEN GOTO,next_COUNTRY2
                          MRATE_IHD0[imask]=IHD.val[iIHD[ICL]]
                          MRATE_IHD1[imask]=IHD.LO[iIHD[ICL]]
                          MRATE_IHD2[imask]=IHD.HI[iIHD[ICL]]

                          MRATE_STROKE0[imask]=STROKE.val[iSTROKE[ICL]]
                          MRATE_STROKE1[imask]=STROKE.LO[iSTROKE[ICL]]
                          MRATE_STROKE2[imask]=STROKE.HI[iSTROKE[ICL]]

                        endif else begin
                          iIHD=max(where(IHD.year eq year[iyr]))
                          if (iIHD eq -1)then begin  ;interpolate
                            ;interpolate
                            ILO=MAX(WHERE(IHD.YEAR LE YEAR[IYR])) & YEARLO=IHD.YEAR[ILO]
                            IHI=MIN(WHERE(IHD.YEAR GE YEAR[IYR])) & YEARHI=IHD.YEAR[IHI]
                            iiHD1=where((IHD.year eq yearlo)*(strupcase(IHD.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                            iIHD2=where((IHD.year eq yearHI)*(strupcase(IHD.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            ISTROKE1=where((STROKE.year eq yearlo)*(strupcase(STROKE.cntr_NAME) eq strupcase(cntr_nm[icntr])))        ;1 VALUE FOR ALL AGES
                            ISTROKE2=where((STROKE.year eq yearHI)*(strupcase(STROKE.cntr_NAME) eq strupcase(cntr_nm[icntr])))
                            IF ((MIN(IIHD1) EQ -1) OR (MIN(ISTROKE1) EQ -1)) THEN GOTO,next_COUNTRY2

                            MRATE_IHD0[imask]=IHD.val[iIHD1[ICL]]+(IHD.val[iIHD2[ICL]]-IHD.val[iIHD1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_IHD1[imask]=IHD.LO[iIHD1[ICL]]+(IHD.LO[iIHD2[ICL]]-IHD.LO[iIHD1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_IHD2[imask]=IHD.HI[iIHD1[ICL]]+(IHD.HI[iIHD2[ICL]]-IHD.HI[iIHD1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_STROKE0[imask]=STROKE.val[iSTROKE1[ICL]]+(STROKE.val[iSTROKE2[ICL]]-STROKE.val[iSTROKE1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_STROKE1[imask]=STROKE.LO[iSTROKE1[ICL]]+(STROKE.LO[iSTROKE2[ICL]]-STROKE.LO[iSTROKE1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)
                            MRATE_STROKE2[imask]=STROKE.HI[iSTROKE1[ICL]]+(STROKE.HI[iSTROKE2[ICL]]-STROKE.HI[iSTROKE1[ICL]])/(yearhi-yearlo)*(year[iyr]-yearlo)

                          endif else begin
                              iIHD=where((IHD.year eq year[IYR])*(strupcase(IHD.cntr_NAME) eq strupcase(cntr_nm[icntr])))         ;15 VALUES FOR AGE CLASSES
                              iSTROKE=where((STROKE.year eq year[IYR])*(strupcase(STROKE.cntr_NAME) eq strupcase(cntr_nm[icntr])));15 VALUES FOR AGE CLASSES (25 TO 95)
                              IF ((MIN(IIHD) EQ -1) OR (MIN(ISTROKE) EQ -1)) THEN GOTO,next_COUNTRY2

                              MRATE_IHD0[imask]=IHD.val[iIHD[ICL]]
                              MRATE_IHD1[imask]=IHD.LO[iIHD[ICL]]
                              MRATE_IHD2[imask]=IHD.HI[iIHD[ICL]]

                              MRATE_STROKE0[imask]=STROKE.val[iSTROKE[ICL]]
                              MRATE_STROKE1[imask]=STROKE.LO[iSTROKE[ICL]]
                              MRATE_STROKE2[imask]=STROKE.HI[iSTROKE[ICL]]

                          endelse
                        ENDELSE
                      ENDELSE

                next_COUNTRY2:
                ENDFOR
                MRATE_IHD[0,ICL,*,*]=MRATE_IHD0
                MRATE_IHD[1,ICL,*,*]=MRATE_IHD1
                MRATE_IHD[2,ICL,*,*]=MRATE_IHD2

                MRATE_STROKE[0,ICL,*,*]=MRATE_STROKE0
                MRATE_STROKE[1,ICL,*,*]=MRATE_STROKE1
                MRATE_STROKE[2,ICL,*,*]=MRATE_STROKE2

            ENDFOR  ;age classes

           save, mrate_copd, filename= mordir+'MRATE_COPD_GBD2017_'+year[iyr]+'.sav'
           save, mrate_lc, filename= mordir+'MRATE_LC_GBD2017_'+year[iyr]+'.sav'
           save, mrate_lri, filename= mordir+'MRATE_LRI_GBD2017_'+year[iyr]+'.sav'
           save, mrate_DMT2, filename= mordir+'MRATE_DMT2_GBD2017_'+year[iyr]+'.sav'
           save, mrate_ihd, filename= mordir+'MRATE_IHD_GBD2017_'+year[iyr]+'.sav'
           save, mrate_stroke, filename= mordir+'MRATE_STROKE_GBD2017_'+year[iyr]+'.sav'
     endelse  ;end calculation of incidence rate maps in case they were not available
; END OF BLOCK 3a ########################################

; BLOCK 3b ####### RETRIEVE AGE STRUCTURE PER COUNTRY FROM UN2017 REVISION. ############################################################################
; CALCULATION IS CPU EXPENSIVE. IF FILE IS NOT PRESENT, CALCULATE AND STORE FOR FUTURE RESTORING INSTEAD OF CALCULATION
           POP_AGE_FIL=mordir+'POP_AGE_CLASS_FRACTIONS_UN2017_'+year[iyr]+'.sav'

           IF NOT(file_test(POP_AGE_FIL)) then begin
                 ;CALCULATE AGE GROUP FRACTIONS IN EACH AGE CLASS
                 POP_AGE_FR=FLTARR(21,2880,1440)    ;POP FRACTION PER AGE CLASS FOR ALL CLASSES 0 TO 100 (21 CLASSES)
                print, 'calculating population fraction per age class'
                  FOR ICL=0,20 DO BEGIN         ;21 AGE CLASSES
                    DUMMY=FLTARR(2880,1440)*0.
                    print, ICL
                    FOR icntr=0,ncntr-1 do begin
                         imask=where(hrcntrcode eq  cntr_id[icntr])
                         if(cntr_id[icntr] eq 736) then imask=where(hrcntrcode eq 729)
                         ;define index of current year
                         IAGEgrp=WHERE((POP.YEAR EQ YEAR[IYR])*(POP.CNTR_ID EQ CNTR_ID[ICNTR]))
                      IF (MIN(iagegrp) lt 0) THEN begin
                          ;interpolate
                          ;define bounding years
                          IAGEMIN = max(where(pop.year LE YEAR[iyr])) & YEARLO=POP.YEAR[IAGEMIN]
                          IAGEMAX = min(where(pop.year GT YEAR[iyr])) & YEARHI=POP.YEAR[IAGEMAX]
                          IAGE_1 =where((POP.YEAR EQ YEARLO) * (POP.CNTR_ID EQ CNTR_ID[ICNTR]))
                          IAGE_2 =where((POP.YEAR EQ YEARHI)* (POP.CNTR_ID EQ CNTR_ID[ICNTR]))
                          poptot=total(pop.age_pop[iage_1])+(total(pop.age_pop[iage_2])-total(pop.age_pop[iage_1]))/(yearhi-yearlo)*(year[iyr]-yearlo)
                          pop_agecl=pop.age_pop[iage_1[ICL]]+((pop.age_pop[iage_2[ICL]])-(pop.age_pop[iage_1[ICL]]))/(yearhi-yearlo)*(year[iyr]-yearlo)
                          popfrac=pop_agecl/poptot
                          dummy[imask]=popfrac
                       ENDIF ELSE BEGIN
                         poptot=total(pop.age_pop[iagegrp])   ;tabulated value
                         popfrac=pop.age_pop[iagegrp[ICL]]/poptot
                         dummy[imask]=popfrac  ;fill map with population fraction in present age class
                       ENDELSE
                       NEXTCOUNTRY3:
                    ENDFOR ;countries
                  POP_AGE_FR[ICL,*,*]=dummy
                  endfor ;age classes
                  SAVE,POP_AGE_FR,FILENAME=mordir+'POP_AGE_CLASS_FRACTIONS_UN2017_'+year[iyr]+'.sav'
            ENDIF ELSE BEGIN
                  RESTORE, POP_AGE_FIL
            ENDELSE


            ;CALCULATE APPROPRIATE AGE FRACTIONS
            FRAC_COPD=TOTAL(POP_AGE_FR[C1:C2,*,*],1)
            FRAC_LC=TOTAL(POP_AGE_FR[L1:L2,*,*],1)
            FRAC_LRI=TOTAL(POP_AGE_FR[LR1:LR2,*,*],1)
            FRAC_DMT2=TOTAL(POP_AGE_FR[DM1:DM2,*,*],1)

            FRAC_O3=TOTAL(POP_AGE_FR[O1:O2,*,*],1)

            ;NUMBER OF SPECIFIC 5YR AGE CLASSES FOR IHD AND STROKE
            NCL_IHD=IH2[0]-IH1[0]+1        ;SAME FOR STROKE

           ;At this point we have global grid maps of AF, POP and MORTALITY RATES, both for total as for age classes
           ;LRI: <5YR, COPD AND LC:>30 YR IHD AND STROKE: AGE CLASSES FOR 25+
            print, 'End of age fractions. CALCULATE DMORT @'+systime()
 ; END OF BLOCK 3b ########################################
 ; END OF BLOCK 3 #############################################################################

 ; BLOCK 4 ##############  CALCULATE DMORT = AF*MRATE*POP*AGEFRAC/100K ############################################
           DMORT_COPD=FLTARR(3,2880,1440)
           DMORT_LC=FLTARR(3,2880,1440)
           DMORT_LRI=FLTARR(3,2880,1440)
           DMORT_DMT2=FLTARR(3,2880,1440)
           DMORT_IHD=FLTARR(3,15,2880,1440)
           DMORT_STROKE=FLTARR(3,15,2880,1440)
           DMORT_O3_TU=FLTARR(3,2880,1440)
           DMORT_O3_GBD=FLTARR(3,2880,1440)

           ;Confidence interval bounds
           sig_min_copd=fltarr(2880,1440)*0.
           sig_max_copd=sig_min_copd
           sig_min_lc=sig_min_copd
           sig_max_lc=sig_min_copd
           sig_min_lri=sig_min_copd
           sig_max_lri=sig_min_copd
           sig_min_dmt2=sig_min_copd
           sig_max_dmt2=sig_min_copd
           sig_min_ihd=sig_min_copd
           sig_max_ihd=sig_min_copd
           sig_min_stroke=sig_min_copd
           sig_max_stroke=sig_min_copd
           sig_min_ihd_a=fltarr(15,2880,1440)*0.
           sig_max_ihd_a=fltarr(15,2880,1440)*0.
           sig_min_stroke_a=fltarr(15,2880,1440)*0.
           sig_max_stroke_a=fltarr(15,2880,1440)*0.

         ;calculate central values of total mortalities
          FOR ILAYER=0,0 DO BEGIN    ;ONLY CENTRAL VALUE. lOWER AND UPPER BOUNDARIES  BY ERROR PROPAGATION
           DMORT_COPD[ILAYER,*,*]=AF_COPD_GRID[ILAYER,*,*]*MRATE_COPD[ilayer,*,*]*FRAC_COPD*SCENPOP/1.E5     ;GBD2016: from class 15 to 75-79  GBD2017: all age
           DMORT_LC[ILAYER,*,*]=AF_LC_GRID[ILAYER,*,*]*MRATE_LC[ilayer,*,*]*FRAC_LC*SCENPOP/1.E5             ;GBD2016: from class 15-19 to 75-79  GBD2017: all age
           DMORT_LRI[ILAYER,*,*]=AF_LRI_GRID[ILAYER,*,*]*MRATE_LRI[ilayer,*,*]*FRAC_LRI*SCENPOP/1.E5         ;GBD2016: from class 0-4 to 75-79  GBD2017: all age
           DMORT_DMT2[ILAYER,*,*]=AF_DMT2_GRID[ILAYER,*,*]*MRATE_DMT2[ilayer,*,*]*FRAC_DMT2*SCENPOP/1.E5     ;GBD2016: from class 15-19 to 75-79  GBD2017: all age
           FOR ICL=0,NCL_IHD-1 DO BEGIN    ;GBD2016: ONLY 10 CLASSES; GBD2017:15 CLASSES
              DMORT_IHD[ilayer,ICL,*,*]=AF_IHD_GRID[ilayer,ICL,*,*]*MRATE_IHD[ilayer,ICL,*,*]*SCENPOP*POP_AGE_FR[ICL+5,*,*]/1.E5          ;GBD2016: from class 25-29 to 75-79  GBD2017: all > 25
              DMORT_STROKE[ilayer,ICL,*,*]=AF_STROKE_GRID[ilayer,ICL,*,*]*MRATE_STROKE[ilayer,ICL,*,*]*SCENPOP*POP_AGE_FR[ICL+5,*,*]/1.E5 ;GBD2016: from class 25-29 to 75-79  GBD2017: all > 25
           ENDFOR
         ENDFOR

         ; error propagation at grid cell level from uncertainty on AF and mrate: sig_dmort/mort=sqrt((sig_AF/AF)^2+(sig_mrate/mrate)^2)
         sig_min_copd=reform(dmort_copd[0,*,*]*sqrt(SIG_MIN_AF_COPD^2+((mrate_copd[0,*,*]-mrate_copd[1,*,*])/mrate_copd[0,*,*])^2))
         sig_max_copd=reform(dmort_copd[0,*,*]*sqrt(SIG_MAX_AF_COPD^2+((mrate_copd[2,*,*]-mrate_copd[0,*,*])/mrate_copd[0,*,*])^2))
         sig_min_lc=reform(dmort_lc[0,*,*]*sqrt(SIG_MIN_AF_LC^2+((mrate_lc[0,*,*]-mrate_lc[1,*,*])/mrate_lc[0,*,*])^2))
         sig_max_lc=reform(dmort_lc[0,*,*]*sqrt(SIG_MAX_AF_LC^2+((mrate_lc[2,*,*]-mrate_lc[0,*,*])/mrate_lc[0,*,*])^2))
         sig_min_lri=reform(dmort_lri[0,*,*]*sqrt(SIG_MIN_AF_LRI^2+((mrate_lri[0,*,*]-mrate_lri[1,*,*])/mrate_lri[0,*,*])^2))
         sig_max_lri=reform(dmort_lri[0,*,*]*sqrt(SIG_MAX_AF_LC^2+((mrate_lri[2,*,*]-mrate_lri[0,*,*])/mrate_lri[0,*,*])^2))
         sig_min_dmt2=reform(dmort_dmt2[0,*,*]*sqrt(SIG_MIN_AF_DMT2^2+((mrate_dmt2[0,*,*]-mrate_dmt2[1,*,*])/mrate_dmt2[0,*,*])^2))
         sig_max_dmt2=reform(dmort_dmt2[0,*,*]*sqrt(SIG_MAX_AF_DMT2^2+((mrate_dmt2[2,*,*]-mrate_dmt2[0,*,*])/mrate_dmt2[0,*,*])^2))

         ;error for each age class
         FOR ICL=0,NCL_IHD-1 DO BEGIN
           sig_min_ihd_a[icl,*,*]=reform(dmort_ihd[0,icl,*,*]*sqrt(((af_ihd_GRID[0,icl,*,*]-af_ihd_GRID[1,icl,*,*])/af_ihd_GRID[0,icl,*,*])^2+((mrate_ihd[0,icl,*,*]-mrate_ihd[1,icl,*,*])/mrate_ihd[0,icl,*,*])^2))
           sig_max_ihd_a[icl,*,*]=reform(dmort_ihd[0,icl,*,*]*sqrt(((af_ihd_GRID[2,icl,*,*]-af_ihd_GRID[0,icl,*,*])/af_ihd_GRID[0,icl,*,*])^2+((mrate_ihd[2,icl,*,*]-mrate_ihd[0,icl,*,*])/mrate_ihd[0,icl,*,*])^2))
           sig_min_stroke_a[icl,*,*]=reform(dmort_stroke[0,icl,*,*]*sqrt(((af_stroke_GRID[0,icl,*,*]-af_stroke_GRID[1,icl,*,*])/af_stroke_GRID[0,icl,*,*])^2+((mrate_stroke[0,icl,*,*]-mrate_stroke[1,icl,*,*])/mrate_stroke[0,icl,*,*])^2))
           sig_max_stroke_a[icl,*,*]=reform(dmort_stroke[0,icl,*,*]*sqrt(((af_stroke_GRID[2,icl,*,*]-af_stroke_GRID[0,icl,*,*])/af_stroke_GRID[0,icl,*,*])^2+((mrate_stroke[2,icl,*,*]-mrate_stroke[0,icl,*,*])/mrate_stroke[0,icl,*,*])^2))
         ENDFOR
         ;error on sum of age classes (for each grid cell)
         sig_min_ihd=sqrt(total((sig_min_ihd_a^2),1))
         sig_max_ihd=sqrt(total((sig_max_ihd_a^2),1))
         sig_min_stroke=sqrt(total((sig_min_stroke_a^2),1))
         sig_max_stroke=sqrt(total((sig_max_stroke_a^2),1))

         sig_min_copd(where(~finite(sig_min_copd)))=0.
         sig_max_copd(where(~finite(sig_max_copd)))=0.
         sig_min_lc(where(~finite(sig_min_lc)))=0.
         sig_max_lc(where(~finite(sig_max_lc)))=0.
         sig_min_lri(where(~finite(sig_min_lri)))=0.
         sig_max_lri(where(~finite(sig_max_lri)))=0.
         sig_min_dmt2(where(~finite(sig_min_dmt2)))=0.
         sig_max_dmt2(where(~finite(sig_max_dmt2)))=0.
         sig_min_ihd(where(~finite(sig_min_ihd)))=0.
         sig_max_ihd(where(~finite(sig_max_ihd)))=0.
         sig_min_stroke(where(~finite(sig_min_stroke)))=0.
         sig_max_stroke(where(~finite(sig_max_stroke)))=0.

         sig_min_all=sig_min_copd+sig_min_lc+sig_min_lri+sig_min_dmt2+sig_min_ihd+sig_min_stroke
         sig_max_all=sig_max_copd+sig_max_lc+sig_max_lri+sig_max_dmt2+sig_max_ihd+sig_max_stroke

         DMORT_IHD_ALL=reform(TOTAL(DMORT_IHD[0,*,*,*],2))
         DMORT_stroke_ALL=reform(TOTAL(DMORT_stroke[0,*,*,*],2))
         ALL_MORT=DMORT_LC[0,*,*]+DMORT_LRI[0,*,*]+DMORT_COPD[0,*,*]+DMORT_DMT2[0,*,*]+ DMORT_IHD_ALL+DMORT_stroke_ALL

         ;CONSTRUCT LAYERS WITH LOWER AND UPPER BOUNDARIES
         DMORT_COPD[1,*,*] = DMORT_COPD[0,*,*]-SIG_MIN_COPD
         DMORT_COPD[2,*,*] = DMORT_COPD[0,*,*]+SIG_MAX_COPD
         DMORT_LC[1,*,*] = DMORT_LC[0,*,*]-SIG_MIN_LC
         DMORT_LC[2,*,*] = DMORT_LC[0,*,*]+SIG_MAX_LC
         DMORT_LRI[1,*,*] = DMORT_LRI[0,*,*]-SIG_MIN_LRI
         DMORT_LRI[2,*,*] = DMORT_LRI[0,*,*]+SIG_MAX_LRI
         DMORT_DMT2[1,*,*] = DMORT_DMT2[0,*,*]-SIG_MIN_DMT2
         DMORT_DMT2[2,*,*] = DMORT_DMT2[0,*,*]+SIG_MAX_DMT2
         DMORT_IHD_LO = DMORT_IHD_ALL-SIG_MIN_IHD
         DMORT_IHD_HI = DMORT_IHD_ALL+SIG_MAX_IHD
         DMORT_STROKE_LO = DMORT_stroke_ALL-SIG_MIN_STROKE
         DMORT_STROKE_HI = DMORT_stroke_ALL+SIG_MAX_STROKE

         PRINT, 'TOTAL MORTALITIES AMBIENT PM PER COD:'
         PRINT,'COPD ',TOTAL(DMORT_COPD[0,*,*]),'(', TOTAL(DMORT_COPD[1,*,*]),',', TOTAL(DMORT_COPD[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'LC ',TOTAL(DMORT_LC[0,*,*]),'(',     TOTAL(DMORT_lc[1,*,*]),',',   TOTAL(DMORT_lc[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'LRI ',TOTAL(DMORT_LRI[0,*,*]),'(',   TOTAL(DMORT_lri[1,*,*]),',',  TOTAL(DMORT_lri[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'DMT2 ',TOTAL(DMORT_DMT2[0,*,*]),'(', TOTAL(DMORT_dmt2[1,*,*]),',', TOTAL(DMORT_dmt2[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'IHD ',TOTAL(DMORT_IHD_ALL),'(',      TOTAL(DMORT_ihd_LO),',',      TOTAL(DMORT_ihd_HI),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'STROKE ',TOTAL(DMORT_STROKE_ALL),'(',      TOTAL(DMORT_stroke_LO),',', TOTAL(DMORT_stroke_HI),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
         PRINT,'TOTAL PM ',total(all_mort),'(',      total(all_mort-sig_min_all),',', total(all_mort+sig_max_all),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'


         DMORT_O3_TU[0,*,*]=REFORM(MRATE_COPD[0,*,*]*FRAC_O3*scenPOP*scenpopmask*(1-EXP(-BETA_COPD_TU[0]*((SC.adm8h-ADM8THR)>0)))/1.e5)
         DMORT_O3_GBD[0,*,*]=REFORM(MRATE_COPD[0,*,*]*FRAC_O3*scenPOP*scenpopmask*(1-EXP(-BETA_COPD_GBD[0]*((SC.sdm8h-sDM8THR)>0)))/1.e5)

         SIG_COPD_MIN=(MRATE_COPD[0,*,*]-MRATE_COPD[1,*,*])/MRATE_COPD[0,*,*]
         SIG_COPD_MAX=(MRATE_COPD[2,*,*]-MRATE_COPD[0,*,*])/MRATE_COPD[0,*,*]

         SIG_AF_TU_MIN=((1-EXP(-BETA_COPD_TU[0]*((SC.adm8h-ADM8THR)>0))) - (1-EXP(-BETA_COPD_TU[1]*((SC.adm8h-ADM8THR)>0))))/(1-EXP(-BETA_COPD_TU[0]*((SC.adm8h-ADM8THR)>0)))
         SIG_AF_TU_MAX=((1-EXP(-BETA_COPD_TU[2]*((SC.adm8h-ADM8THR)>0))) - (1-EXP(-BETA_COPD_TU[0]*((SC.adm8h-ADM8THR)>0))))/(1-EXP(-BETA_COPD_TU[0]*((SC.adm8h-ADM8THR)>0)))

         SIG_AF_GBD_MIN=((1-EXP(-BETA_COPD_GBD[0]*((SC.adm8h-ADM8THR)>0))) - (1-EXP(-BETA_COPD_GBD[1]*((SC.adm8h-ADM8THR)>0))))/(1-EXP(-BETA_COPD_GBD[0]*((SC.adm8h-ADM8THR)>0)))
         SIG_AF_GBD_MAX=((1-EXP(-BETA_COPD_GBD[2]*((SC.adm8h-ADM8THR)>0))) - (1-EXP(-BETA_COPD_GBD[0]*((SC.adm8h-ADM8THR)>0))))/(1-EXP(-BETA_COPD_GBD[0]*((SC.adm8h-ADM8THR)>0)))

         SIG_O3_TU_MIN=DMORT_O3_TU[0,*,*]*SQRT(SIG_COPD_MIN^2+SIG_AF_TU_MIN^2)
         SIG_O3_TU_MIN(WHERE(~FINITE(SIG_O3_TU_MIN)))=0.
         SIG_O3_TU_MAX=DMORT_O3_TU[0,*,*]*SQRT(SIG_COPD_MAX^2+SIG_AF_TU_MAX^2)
         SIG_O3_TU_MAX(WHERE(~FINITE(SIG_O3_TU_MAX)))=0.
         SIG_O3_GBD_MIN=DMORT_O3_GBD[0,*,*]*SQRT(SIG_COPD_MIN^2+SIG_AF_GBD_MIN^2)
         SIG_O3_GBD_MIN(WHERE(~FINITE(SIG_O3_GBD_MIN)))=0.
         SIG_O3_GBD_MAX=DMORT_O3_GBD[0,*,*]*SQRT(SIG_COPD_MAX^2+SIG_AF_GBD_MAX^2)
         SIG_O3_GBD_MAX(WHERE(~FINITE(SIG_O3_GBD_MAX)))=0.

         DMORT_O3_GBD[1,*,*]=DMORT_O3_GBD[0,*,*]-SIG_O3_GBD_MIN
         DMORT_O3_GBD[2,*,*]=DMORT_O3_GBD[0,*,*]+SIG_O3_GBD_MAX
         DMORT_O3_TU[1,*,*]=DMORT_O3_TU[0,*,*]-SIG_O3_TU_MIN
         DMORT_O3_TU[2,*,*]=DMORT_O3_TU[0,*,*]+SIG_O3_TU_MAX

         PRINT, 'TOTAL MORTALITIES O3:'
         PRINT,FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)','COPD GBD ',TOTAL(DMORT_O3_GBD[0,*,*]), ' (',TOTAL(DMORT_O3_GBD[1,*,*]), ',',TOTAL(DMORT_O3_GBD[2,*,*]),')'
         PRINT,FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)','COPD TUR ',TOTAL(DMORT_O3_TU[0,*,*]), ' (',TOTAL(DMORT_O3_TU[1,*,*]), ',',TOTAL(DMORT_O3_TU[2,*,*]),')'


; BLOCK 5 #############  Pass to 0.5x0.5deg resolution for aggregation #################################################################

        ;make sum of 4x4 subgrids to pass from 7.5'x7.5' (=0.125x0.125deg) to 0.5x0.5deg resolution
        MRES_DMORT_COPD = FLTARR(3,720,360)
        MRES_DMORT_LC = FLTARR(3,720,360)
        MRES_DMORT_LRI = FLTARR(3,720,360)
        MRES_DMORT_DMT2 = FLTARR(3,720,360)
        MRES_DMORT_IHD = FLTARR(3,720,360)
        MRES_DMORT_STROKE = FLTARR(3,720,360)
        MRES_DMORT_O3_TU = FLTARR(3,720,360)
        MRES_DMORT_O3_GBD = FLTARR(3,720,360)

        POPMED=fltarr(720,360)

       ; aggregate subgrid cells
            for im = 0,719 do begin
              for jm = 0,359 do begin
                  IMIN=IM*4
                  IMAX=(IM+1)*4-1
                  JMIN=JM*4
                  JMAX=(JM+1)*4-1
                  popmed[im,jm]=total(scenpop[IMIN:IMAX,JMIN:JMAX])
                  ;central values
                       MRES_DMORT_COPD[0,IM,JM]=max([0,TOTAL(DMORT_COPD[0,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LC[0,IM,JM]=max([0,TOTAL(DMORT_LC[0,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LRI[0,IM,JM]=max([0,TOTAL(DMORT_LRI[0,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_DMT2[0,IM,JM]=max([0,TOTAL(DMORT_DMT2[0,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_IHD[0,IM,JM]=max([0,TOTAL(DMORT_IHD_ALL[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_STROKE[0,IM,JM]=max([0,TOTAL(DMORT_STROKE_ALL[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_TU[0,IM,JM]=max([0,TOTAL(DMORT_O3_TU[0,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_GBD[0,IM,JM]=max([0,TOTAL(DMORT_O3_GBD[0,IMIN:IMAX,JMIN:JMAX])])
                  ;lower bounds
                       MRES_DMORT_COPD[1,IM,JM]=max([0,TOTAL(DMORT_COPD[1,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LC[1,IM,JM]=max([0,TOTAL(DMORT_LC[1,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LRI[1,IM,JM]=max([0,TOTAL(DMORT_LRI[1,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_DMT2[1,IM,JM]=max([0,TOTAL(DMORT_DMT2[1,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_IHD[1,IM,JM]=max([0,TOTAL(DMORT_IHD_LO[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_STROKE[1,IM,JM]=max([0,TOTAL(DMORT_STROKE_LO[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_TU[1,IM,JM]=max([0,TOTAL(DMORT_O3_TU[1,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_GBD[1,IM,JM]=max([0,TOTAL(DMORT_O3_GBD[1,IMIN:IMAX,JMIN:JMAX])])
                   ;upper bounds
                       MRES_DMORT_COPD[2,IM,JM]=max([0,TOTAL(DMORT_COPD[2,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LC[2,IM,JM]=max([0,TOTAL(DMORT_LC[2,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_LRI[2,IM,JM]=max([0,TOTAL(DMORT_LRI[2,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_DMT2[2,IM,JM]=max([0,TOTAL(DMORT_DMT2[2,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_IHD[2,IM,JM]=max([0,TOTAL(DMORT_IHD_HI[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_STROKE[2,IM,JM]=max([0,TOTAL(DMORT_STROKE_HI[IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_TU[2,IM,JM]=max([0,TOTAL(DMORT_O3_TU[2,IMIN:IMAX,JMIN:JMAX])])
                       MRES_DMORT_O3_GBD[2,IM,JM]=max([0,TOTAL(DMORT_O3_GBD[2,IMIN:IMAX,JMIN:JMAX])])
               endfor
            endfor

        MORT_SC_MED=reform(MRES_DMORT_COPD[0,*,*]+MRES_DMORT_LC[0,*,*]+MRES_DMORT_LRI[0,*,*]+MRES_DMORT_DMT2[0,*,*]+MRES_DMORT_IHD[0,*,*]+MRES_DMORT_STROKE[0,*,*])
        MORT_SC_lo=reform(MRES_DMORT_COPD[1,*,*]+MRES_DMORT_LC[1,*,*]+MRES_DMORT_LRI[1,*,*]+MRES_DMORT_DMT2[1,*,*]+MRES_DMORT_IHD[1,*,*]+MRES_DMORT_STROKE[1,*,*])
        MORT_SC_HI=reform(MRES_DMORT_COPD[2,*,*]+MRES_DMORT_LC[2,*,*]+MRES_DMORT_LRI[2,*,*]+MRES_DMORT_DMT2[2,*,*]+MRES_DMORT_IHD[2,*,*]+MRES_DMORT_STROKE[2,*,*])
        TOT_MORT_MED=TOTAL(MORT_SC_MED)

            PRINT, 'TOTAL 0.5x0.5 GRID MORTALITIES AMBIENT PM PER COD:'
            PRINT,'COPD ',TOTAL(mres_dmort_copd[0,*,*]), '(', TOTAL(MRES_DMORT_COPD[1,*,*]), ',', TOTAL(MRES_DMORT_COPD[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
            PRINT,'LC ',TOTAL(mres_dmort_LC[0,*,*]), '(', TOTAL(MRES_DMORT_LC[1,*,*]), ',', TOTAL(MRES_DMORT_LC[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
            PRINT,'LRI ',TOTAL(mres_dmort_LRI[0,*,*]), '(', TOTAL(MRES_DMORT_LRI[1,*,*]), ',', TOTAL(MRES_DMORT_LRI[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
            PRINT,'DMT2 ',TOTAL(mres_dmort_DMT2[0,*,*]), '(', TOTAL(MRES_DMORT_DMT2[1,*,*]), ',', TOTAL(MRES_DMORT_DMT2[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
            PRINT,'IHD ',TOTAL(mres_dmort_IHD[0,*,*]), '(', TOTAL(MRES_DMORT_IHD[1,*,*]), ',', TOTAL(MRES_DMORT_IHD[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)'
            PRINT,'STROKE ',TOTAL(mres_dmort_STROKE[0,*,*]), '(', TOTAL(MRES_DMORT_STROKE[1,*,*]), ',', TOTAL(MRES_DMORT_STROKE[2,*,*]),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)

            PRINT, 'TOTAL 0.5x0.5 GRID MORTALITIES O3:',TOT_MORT_MED,'(',TOTAL(MORT_SC_LO),',',TOTAL(MORT_SC_HI),')',FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)
            PRINT,FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)','COPD GBD ',TOTAL(MRES_DMORT_O3_GBD[0,*,*]), ' (',TOTAL(MRES_DMORT_O3_GBD[1,*,*]), ',',TOTAL(MRES_DMORT_O3_GBD[2,*,*]),')'
            PRINT,FORMAT='(A15,E10.2,A3,E10.2,A2,E10.2,A3)','COPD TUR ',TOTAL( MRES_DMORT_O3_TU[0,*,*]), ' (',TOTAL( MRES_DMORT_O3_TU[1,*,*]), ',',TOTAL( MRES_DMORT_O3_TU[2,*,*]),')'

        ; 0.5x0.5deg resolution of pollutants
        med_PMTOT_ANT_35=congrid(sc.ant_pm_35,720,360)
        med_PMTOT_35=congrid(sc.tot_pm_35,720,360)
        med_PMNAT_DRY=congrid(sc.NAT_PM_DRY,720,360)
        med_NAT_H2O35=congrid(sc.H2O35_ss,720,360)
        med_ADMA8=congrid(sc.adm8h,720,360)
        med_SDMA8=congrid(sc.sdm8h,720,360)

;BLOCK 6 #################### aggregate individual countries' mortalities using country masks - make pop-weighted mean of PM and O3 ################################################################

      	print, 'Aggregate countries @'+systime()
        restore, ancildir+'FASST_REGION_MASK\0.5x0.5_INDIV_COUNTRY_MASK.SAV'    ;restore structure CNTRMASK_MEDRES with 0.5x0.5 resolution country masks and names and ISO3

        FOR icntr=0,ncntr-1 do begin

         	    cmask=reform(CNTRMASK_MEDRES.CNTRYMASKMED[icntr,*,*])  ;remove empty dimensions: generates [720,360] array in stead of [1,720,360]

        	    ;med
        	    CTOT_COPD_MED=TOTAL(cmask*MRES_DMORT_COPD[0,*,*])
        	    CTOT_LC_MED=TOTAL(cmask*MRES_DMORT_LC[0,*,*])
        	    CTOT_LRI_MED=TOTAL(cmask*MRES_DMORT_LRI[0,*,*])
        	    CTOT_DMT2_MED=TOTAL(cmask*MRES_DMORT_DMT2[0,*,*])
        	    CTOT_ihd_MED=TOTAL(cmask*MRES_DMORT_ihd[0,*,*])
        	    CTOT_stroke_MED=TOTAL(cmask*MRES_DMORT_stroke[0,*,*])

        	    ;low
        	    CTOT_COPD_LO=TOTAL(cmask*MRES_DMORT_COPD[1,*,*])
        	    CTOT_LC_LO=TOTAL(cmask*MRES_DMORT_LC[1,*,*])
        	    CTOT_LRI_LO=TOTAL(cmask*MRES_DMORT_LRI[1,*,*])
        	    CTOT_DMT2_LO=TOTAL(cmask*MRES_DMORT_DMT2[1,*,*])
        	    CTOT_IHD_LO=TOTAL(cmask*MRES_DMORT_ihd[1,*,*])
        	    CTOT_STROKE_LO=TOTAL(cmask*MRES_DMORT_stroke[1,*,*])

        	    ;high
        	    CTOT_COPD_HI=TOTAL(cmask*MRES_DMORT_COPD[2,*,*])
              CTOT_LC_HI=TOTAL(cmask*MRES_DMORT_LC[2,*,*])
              CTOT_LRI_HI=TOTAL(cmask*MRES_DMORT_LRI[2,*,*])
              CTOT_DMT2_HI=TOTAL(cmask*MRES_DMORT_DMT2[2,*,*])
              CTOT_IHD_HI=TOTAL(cmask*MRES_DMORT_ihd[2,*,*])
              CTOT_STROKE_HI=TOTAL(cmask*MRES_DMORT_stroke[2,*,*])

        	    CTOT_MORT_MED_SC=TOTAL(cmask*MORT_SC_MED)

        	    CTOT_MORT_LO_SC=CTOT_MORT_MED_SC-sqrt((ctot_copd_med-ctot_copd_lo)^2+(ctot_lc_med-ctot_lc_lo)^2+(ctot_lri_med-ctot_lri_lo)^2+(ctot_dmt2_med-ctot_dmt2_lo)^2 $
        	      +(ctot_ihd_med-ctot_ihd_lo)^2+(ctot_stroke_med-ctot_stroke_lo)^2)
        	    CTOT_MORT_HI_SC=CTOT_MORT_MED_SC+sqrt((ctot_copd_med-ctot_copd_hi)^2+(ctot_lc_med-ctot_lc_hi)^2+(ctot_lri_med-ctot_lri_hi)^2+(ctot_dmt2_med-ctot_dmt2_hi)^2 $
        	      +(ctot_ihd_med-ctot_ihd_hi)^2+(ctot_stroke_med-ctot_stroke_hi)^2)

        	    CTOT_O3MORT_MED_SC_TU=TOTAL(cmask*MRES_DMORT_O3_TU[0,*,*])
        	    CTOT_O3MORT_LO_SC_TU=TOTAL(cmask*MRES_DMORT_O3_TU[1,*,*])
        	    CTOT_O3MORT_HI_SC_TU=TOTAL(cmask*MRES_DMORT_O3_TU[2,*,*])

        	    CTOT_O3MORT_MED_SC_GBD=TOTAL(cmask*MRES_DMORT_O3_GBD[0,*,*])
        	    CTOT_O3MORT_LO_SC_GBD=TOTAL(cmask*MRES_DMORT_O3_GBD[1,*,*])
        	    CTOT_O3MORT_HI_SC_GBD=TOTAL(cmask*MRES_DMORT_O3_GBD[2,*,*])

        	    popcn=Total(cmask*POPmed)
         	    POP_NAT_DRY=Total(med_PMNAT_DRY*cmask*popmed)/popcn
        	    POP_SS_H2O35=Total(med_NAT_H2O35*cmask*popmed)/popcn
        	    POP_ANT_35=Total(med_PMTOT_ANT_35*cMASK*popmed)/popcn
        	    POP_PMTOT_35=Total(med_PMTOT_35*cMASK*popmed)/popcn
        	    POP_ADMA8h=Total(med_adma8*cMASK*popmed)/popcn
        	    POP_SDMA8h=Total(med_sdma8*cMASK*popmed)/popcn
        	    POP_NAT_35=pop_nat_dry+pop_ss_h2o35

        ; TXT table 1 line output for current scenario, year, country
        	    printf,6,format='(2(A15),A6,A5,A20,5(f20.2),27(E20.4))',SCENLAB[ISC],SSP[ISC],YEAR[iyr],cntr_ISO[icntr],CNTR_NM[icntr],popcn, POP_PMTOT_35,POP_NAT_35,POP_ADMA8h, POP_SDMA8h,$
        	      CTOT_MORT_MED_SC,CTOT_MORT_LO_SC,CTOT_MORT_HI_SC,$
        	      CTOT_COPD_MED,CTOT_LC_MED,CTOT_LRI_MED,CTOT_DMT2_MED,CTOT_IHD_MED,CTOT_STROKE_MED,$
        	      CTOT_COPD_LO,CTOT_LC_LO,CTOT_LRI_LO,CTOT_DMT2_LO,CTOT_IHD_LO,CTOT_STROKE_LO, $
        	      CTOT_COPD_HI,CTOT_LC_HI,CTOT_LRI_HI,CTOT_DMT2_HI,CTOT_IHD_HI,CTOT_STROKE_HI,$
        	      CTOT_O3MORT_MED_SC_GBD,CTOT_O3MORT_LO_SC_GBD,CTOT_O3MORT_HI_SC_GBD,$
        	      CTOT_O3MORT_MED_SC_TU,CTOT_O3MORT_LO_SC_TU,CTOT_O3MORT_HI_SC_TU

        	  endfor
        	  print, 'end countries loop @'+systime()

        	  skipcountries:

;BLOCK 7 ################## store mortalities in gridded ncdf file #########################################################################################

        	pathnc=ncdir
        	file_mkdir,pathnc

        ;store as ncdf file
        filenc2=ncdir+'\'+scen[isc]+'\FASST_05x05_MORTALITIES_'+PROJECT+'_'+YEAR[iyr]+'_'+SCEN[isc]+'.nc'
        print,'creating ',filenc2

        id = NCDF_CREATE(filenc2,/CLOBBER)

        NCDF_CONTROL, id, /FILL ; Fill the file with default values.
        ; Define variables.

        xid = NCDF_DIMDEF(id, 'lon', imed) ; Make dimensions.
        yid = NCDF_DIMDEF(id, 'lat', jmed) ; Make dimensions.

        xid = NCDF_VARDEF(id, 'lon', [xid], /DOUBLE)
        yid = NCDF_VARDEF(id, 'lat', [yid], /DOUBLE)

        prtid1 = NCDF_VARDEF(id, 'PM', [xid,yid], /FLOAT)
        prtid2 = NCDF_VARDEF(id, 'ANTH_PM', [xid,yid], /FLOAT)
        mrtid1 = NCDF_VARDEF(id, 'GBD_PM_MORT', [xid,yid], /FLOAT)
        mrtid2 = NCDF_VARDEF(id, 'GBD_O3_MORT', [xid,yid], /FLOAT)
        mrtid3 = NCDF_VARDEF(id, 'TUR_O3_MORT', [xid,yid], /FLOAT)

        ; Describe variables:
        NCDF_ATTPUT, id, yid, 'standard_name', 'latitude'
        NCDF_ATTPUT, id, yid, 'long_name', 'latitude'
        NCDF_ATTPUT, id, yid, 'units', 'degrees_north'
        NCDF_ATTPUT, id, yid, 'axis', 'Y'
        NCDF_ATTPUT, id, yid, 'bounds', 'lat_bnds'

        NCDF_ATTPUT, id, xid, 'standard_name', 'longtitude'
        NCDF_ATTPUT, id, xid, 'long_name', 'longtitude'
        NCDF_ATTPUT, id, xid, 'units', 'degrees_east'
        NCDF_ATTPUT, id, xid, 'axis', 'X'
        NCDF_ATTPUT, id, xid, 'bounds', 'lon_bnds'

        NCDF_ATTPUT, id, prtid1, 'units', 'ug/m3'
        NCDF_ATTPUT, id, prtid1, 'standard_name', 'PM'
        NCDF_ATTPUT, id, prtid1, 'long_name', 'scenario total PM2.5 AT 35%RH incl dust and SS'

        NCDF_ATTPUT, id, prtid2, 'units', 'ug/m3'
        NCDF_ATTPUT, id, prtid2, 'standard_name', 'ANTH_PM'
        NCDF_ATTPUT, id, prtid2, 'long_name', 'scenario anthropogenic PM2.5 AT 35% RH'

        NCDF_ATTPUT, id, mrtid1, 'units', '#'
        NCDF_ATTPUT, id, mrtid1, 'standard_name', 'MORT_TOT_PM'
        NCDF_ATTPUT, id, mrtid1, 'long_name', 'Burnett Mortalities from total anthropogenic + natural PM2.5 AT 35% RH'

        NCDF_ATTPUT, id, mrtid2, 'units', '#'
        NCDF_ATTPUT, id, mrtid2, 'standard_name', 'MORT_O3'
        NCDF_ATTPUT, id, mrtid2, 'long_name', 'GBD2017 Mortalities from O3'

        NCDF_ATTPUT, id, mrtid3, 'units', '#'
        NCDF_ATTPUT, id, mrtid3, 'standard_name', 'MORT_O3'
        NCDF_ATTPUT, id, mrtid3, 'long_name', 'Turner/Malley Mortalities from O3'

        NCDF_ATTPUT, id, /GLOBAL, 'source', PRONAME
        NCDF_ATTPUT, id, /GLOBAL, 'contact', 'rita.van-dingenen@ec.europa.eu)'
        NCDF_ATTPUT, id, /GLOBAL, 'scenario', SCEN[isc]
        NCDF_ATTPUT, id, /GLOBAL, 'year', year[iyr]

        NCDF_ATTPUT, id, /GLOBAL, 'GBD O3_threshold ppb', string(SDM8THR)
        NCDF_ATTPUT, id, /GLOBAL, 'TURNER O3_threshold ppb', string(ADM8THR)

        NCDF_CONTROL, id, /ENDEF ; Put file in data mode.

        ; write variables
        NCDF_VARPUT, id, yid, mR_latS ; Input data.
        NCDF_VARPUT, id, xid, mR_lonS ; Input data.
        NCDF_VARPUT, id, PRTID1, med_PMTOT_35 ; Input data.
        NCDF_VARPUT, id, PRTID2, med_PMTOT_ANT_35 ; Input data.
        NCDF_VARPUT, id, mRTID1, MORT_SC_MED ; Input data.
        NCDF_VARPUT, id, mRTID2,  reform(MRES_DMORT_O3_GBD[0,*,*]) ; Input data.
        NCDF_VARPUT, id, mRTID3, reform(MRES_DMORT_O3_TU[0,*,*])  ; Input data.

        NCDF_CLOSE, id ; Close the NetCDF file.
        print,'written ',filenc2

        next_scen:

     endfor ;years
  endfor ;scenarios

	close,6     ;close text file
end
