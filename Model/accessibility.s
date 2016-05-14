/* ************************************************************
 ACCESSIBILITY CALCULATOR

 This script calculates the accessibility of each zone, for
  research purposes.
  1. Pull in SOV and Transit skims
  2. Identify zones within
     a. 10 minutes
		 b. 20 minutes
     c. 30 minutes
     d. 45 minutes
     e. 60 minutes
  3. Pull in TAZ data


  Author: Greg Macfarlane, Georgia Tech
 *************************************************************
*/
/*
; ************************************************************
; Accessibility Matrix
RUN PGM = MATRIX

FILEI MATI[1] = M:\TDModels\Atlanta\2010\AMPK10HWY.SKM 			;SOV congested highway skim file
			MATI[2] = M:\TDModels\Atlanta\2010\WLKPRE_ALL.SKM			;Walk to premium transit skim
			MATI[3] = M:\TDModels\Atlanta\2010\AUTOPRE_ALL.SKM 		;Drive to premium transit skim
			
FILEO MATO[1] = AccessMatrix.mtx, MO = 11-15,21-25,31-35,
	NAME = d10, d20, d30, d45, d60, w10,w20,w30,w45,w60, p10,p20,p30,p45,p60

	ZONEMSG = 50
	
			mw[1] = mi.1.TIME 					;SOV drive time (minutes)
			mw[2] = mi.2.tottime /100 	;walk premium time (hundredths of minutes)
			mw[3] = mi.3.tottime /100 	;drive premium time (hundredths of minutes)
		
		;Drive timev
		JLOOP
			IF ( mw[1] <=10 & mw[2] > 0) ;less than 10
					mw[11] = 1
			ENDIF			
			IF ( mw[1] <= 20 & mw[1] > 10 ) ;10-20
					mw[12] = 1
			ENDIF			
			IF ( mw[1] <= 30 & mw[1] > 20 ) ;20-30
					mw[13] = 1
			ENDIF			
			IF ( mw[1] <= 45 & mw[1] > 30 ) ;30-45
					mw[14] = 1
			ENDIF			
			IF ( mw[1] <= 60 & mw[1] > 45 ) ;45-60
					mw[15] = 1
			ENDIF
		ENDJLOOP
		
		;Walk to Transit
		JLOOP
			IF ( mw[2] <=10 & mw[2] > 0) ;less than 10, 0 means inaccessible
					mw[21] = 1
			ENDIF			
			IF ( mw[2] <= 20 & mw[2] > 10 ) ;10-20
					mw[22] = 1
			ENDIF			
			IF ( mw[2] <= 30 & mw[2] > 20 ) ;20-30
					mw[23] = 1
			ENDIF			
			IF ( mw[2] <= 45 & mw[2] > 30 ) ;30-45
					mw[24] = 1
			ENDIF
			IF ( mw[2] <= 60 & mw[2] > 45 ) ;45-60
					mw[25] = 1
			ENDIF
		ENDJLOOP
		
		;Drive to Transit
		JLOOP
			IF ( mw[3] <=10 & mw[2] > 0)   ;less than 10
					mw[31] = 1
			ENDIF			
			IF ( mw[3] <= 20 & mw[3] > 10 ) ;10-20
					mw[32] = 1
			ENDIF			
			IF ( mw[3] <= 30 & mw[3] > 20 ) ;20-30
					mw[33] = 1
			ENDIF			
			IF ( mw[3] <= 45 & mw[3] > 30 ) ;30-45
					mw[34] = 1
			ENDIF			
			IF ( mw[3] <= 60 & mw[3] > 45 ) ;45-60
					mw[35] = 1
			ENDIF
		ENDJLOOP
	
ENDRUN
*/


; *******************************************************************
; Calculate Employment Accessibilty
; *******************************************************************
RUN PGM MATRIX
	FILEI ZDATI = 'M:\TDModels\Atlanta\2010\ZoneData.dbf', Z = ZONE	;SEdata file
				MATI[1] = AccessMatrix.mtx		
		ZONEMSG = 50
		ZONES = 2027			
				
				; need to multiply the accessibility matrix by the transpose
				; of the employment vector, and sum along the rows

		ARRAY	D10 = ZONES,
					D20 = ZONES,
					D30 = ZONES,
					D45 = ZONES,
					D60 = ZONES,
					
					W10 = ZONES,
					W20 = ZONES,
					W30 = ZONES,
					W45 = ZONES,
					W60 = ZONES,
					
					P10 = ZONES,
					P20 = ZONES,
					P30 = ZONES,
					P45 = ZONES,
					P60 = ZONES


		JLOOP
			D10 = D10 + mi.1.d10 * zi.1.EMP[j]
			D20 = D20 + mi.1.d20 * zi.1.EMP[j]
			D30 = D30 + mi.1.d30 * zi.1.EMP[j]
			D45 = D45 + mi.1.d45 * zi.1.EMP[j]
			D60 = D60 + mi.1.d60 * zi.1.EMP[j]
			
			
			W10 = W10 + mi.1.w10 * zi.1.EMP[j]
			W20 = W20 + mi.1.w20 * zi.1.EMP[j]
			W30 = W30 + mi.1.w30 * zi.1.EMP[j]
			W45 = W45 + mi.1.w45 * zi.1.EMP[j]
			W60 = W60 + mi.1.w60 * zi.1.EMP[j]
			
			P10 = P10 + mi.1.p10 * zi.1.EMP[j]
			P20 = P20 + mi.1.p20 * zi.1.EMP[j]
			P30 = P30 + mi.1.p30 * zi.1.EMP[j]
			P45 = P45 + mi.1.p45 * zi.1.EMP[j]
			P60 = P60 + mi.1.p60 * zi.1.EMP[j]
		ENDJLOOP
				
		IF (I=1)
				PRINT CSV=T, FORM=12.2,
				FILE='EMPAccess.csv',
				LIST='TAZ','d10', 'd20', 'd30', 'd45', 'd60', 'w10', 'w20', 'w30',
							'w45', 'w60', 'p10', 'p20', 'p30', 'p45', 'p60'
		ENDIF
		
		;print matrix data to a linear csv file
		PRINT CSV=T, FORM=9, 
			FILE='EMPAccess.csv',
  		LIST=I(5.0),	D10, D20, D30, D45, D60,
										W10, W20, W30, W45, W60,
										P10, P20, P30, P45, P60
ENDRUN

; *******************************************************************
; Calculate Retail Accessibilty
; *******************************************************************
RUN PGM MATRIX
	FILEI ZDATI = 'M:\TDModels\Atlanta\2010\ZoneData.dbf', Z = ZONE	;SEdata file
				MATI[1] = AccessMatrix.mtx		

		ZONEMSG = 50
		ZONES = 2027		
		
		
		; create array to store each sum as J calculates
		ARRAY	D10 = ZONES,
					D20 = ZONES,
					D30 = ZONES,
					D45 = ZONES,
					D60 = ZONES,
					
					W10 = ZONES,
					W20 = ZONES,
					W30 = ZONES,
					W45 = ZONES,
					W60 = ZONES,
					
					P10 = ZONES,
					P20 = ZONES,
					P30 = ZONES,
					P45 = ZONES,
					P60 = ZONES


		; loop through all zones, and sum the opportunities accessibile
		JLOOP
			D10 = D10 + mi.1.d10 * zi.1.RETAIL[j]
			D20 = D20 + mi.1.d20 * zi.1.RETAIL[j]
			D30 = D30 + mi.1.d30 * zi.1.RETAIL[j]
			D45 = D45 + mi.1.d45 * zi.1.RETAIL[j]
			D60 = D60 + mi.1.d60 * zi.1.RETAIL[j]
			
			W10 = W10 + mi.1.w10 * zi.1.RETAIL[j]
			W20 = W20 + mi.1.w20 * zi.1.RETAIL[j]
			W30 = W30 + mi.1.w30 * zi.1.RETAIL[j]
			W45 = W45 + mi.1.w45 * zi.1.RETAIL[j]
			W60 = W60 + mi.1.w60 * zi.1.RETAIL[j]
			
			P10 = P10 + mi.1.p10 * zi.1.RETAIL[j]
			P20 = P20 + mi.1.p20 * zi.1.RETAIL[j]
			P30 = P30 + mi.1.p30 * zi.1.RETAIL[j]
			P45 = P45 + mi.1.p45 * zi.1.RETAIL[j]
			P60 = P60 + mi.1.p60 * zi.1.RETAIL[j]
		ENDJLOOP
				

		; Print header
		IF (I=1)
				PRINT CSV=T, FORM=12.2,
				FILE='RETAccess.csv',
				LIST='TAZ','d10', 'd20', 'd30', 'd45', 'd60', 'w10', 'w20', 'w30',
							'w45', 'w60', 'p10', 'p20', 'p30', 'p45', 'p60'
		ENDIF
		
		;print matrix data to a linear csv file
		PRINT CSV=T, FORM=9, 
			FILE='RETAccess.csv',
  		LIST=I(5.0),	D10, D20, D30, D45, D60,
										W10, W20, W30, W45, W60,
										P10, P20, P30, P45, P60
ENDRUN

