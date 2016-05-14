; Transit competitiveness
READ FILE = '.\ClassModels\Models\Base2009\1ControlCenter.block'
READ FILE = '.\ClassModels\Models\Base2009\0GeneralParameters.block'

/* ==========================================================================================================
   PURPOSE:	Create Best WALK TO Transit transit skims
   ========================================================================================================= */
/*
RUN PGM=MATRIX
  FILEI MATI[1] =E:\ClassModels\Models\Base2009\Temp\4_TmpMC\A\Best_Walk_Skims_temp.mtx 
  FILEO MATO    =E:\ClassModels\Models\Base2009\Temp\4_TmpMC\A\Best_Walk_Skims.mtx  MO=1-6, NAME=WALKTIME, INITWAIT, XFERWAIT, T45678, DRIVETIME, GENCOST
  
  MW[1] = mi.1.1/100
  MW[2] = mi.1.2/100
  MW[3] = mi.1.3/100
  MW[4] = mi.1.4/100
  MW[5] = mi.1.5/100
  MW[6] = MW[4] + MW[2] + MW[3] + MW[1] + MW[5] 
  ; note: this counts time equally. Perhaps should have weight on wait time and transfer time.
  
  ZONEMSG = @ZoneMsgRate@
ENDRUN
*/
/* ==========================================================================================================
   PURPOSE:	Identify paths where transit is competitive, and find how many people are available.
   ========================================================================================================= */

   
RUN PGM=MATRIX
  FILEI MATI[1]=ClassModels\Models\Base2009\Temp\4_TmpMC\A\Best_Walk_Skims.mtx 
  FILEI MATI[2]=ClassModels\Models\Base2009\5_AssignHwy\Ao\1Skims\skm_autotime.mtx 
  FILEI MATI[3]=ClassModels\Models\Base2009\3_Distribute\Do\PA_AllPurp_GRAVITY.mtx
  
  FILEO MATO = CompetitiveTrips.mtx  MO=1-6, NAME=LOWER, OneHalf, Thirty, V_LOWER, V_OneHalf, V_Thirty
  ;FILEO MATO = CompetitiveTrips.txt  DEC=9*0, FORMAT=TXT, PATTERN=IJM:V, MO=1-6

  
  ;Find OD pairs where:
  ; Transit has lower time than auto
  JLOOP


  IF(mi.1.GENCOST < mi.2.TIME_PM) 
    mw[1]=1
  ENDIF
  
  ; Transit within 150% of auto
  IF (1.5*mi.1.GENCOST < mi.2.TIME_PM) 
    mw[2]=1
  ENDIF
  
  ; Transit within 30 minutes of auto
  IF(mi.1.GENCOST - 30 < mi.2.TIME_PM) 
    mw[3]=1
  ENDIF
  
  ; take out cases where there is no skim
  IF(mi.1.GENCOST == 0)
    mw[1]=0
    mw[2]=0
    mw[3]=0
  ENDIF
  
  ; Multiply by All Trips
  MW[4] = MW[1] * mi.3.TOT
  MW[5] = MW[2] * mi.3.TOT
  MW[6] = MW[3] * mi.3.TOT
  
  ENDJLOOP
  
 ENDRUN
   
   
