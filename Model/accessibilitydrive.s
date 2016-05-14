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
			
FILEO MATO[1] = AccessMatrix.mtx, MO = 11-16,
	NAME = d10, d20, d30, d45, d60, d60p

	ZONEMSG = 50
	
			mw[1] = mi.1.TIME 					;SOV drive time (minutes)
	
		;Drive timev
		JLOOP
			IF ( mw[1] <=10 & mw[1] > 0) ;less than 10
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
			IF ( mw[1] > 60 ) 							;+60
					mw[16] = 1
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
					D60P= ZONES
					
					D10 = 0
					D20 = 0
					D30 = 0
					D45 = 0
					D60 = 0
					D60P= 0
		JLOOP
			D10 = D10 + mi.1.d10 * zi.1.EMP[j]
			D20 = D20 + mi.1.d20 * zi.1.EMP[j]
			D30 = D30 + mi.1.d30 * zi.1.EMP[j]
			D45 = D45 + mi.1.d45 * zi.1.EMP[j]
			D60 = D60 + mi.1.d60 * zi.1.EMP[j]
			D60P= D60P+ mi.1.d60p* zi.1.EMP[j]
		ENDJLOOP
				
		IF (I=1)
				PRINT CSV=T, FORM=12.2,
				FILE='EMPAccess.csv',
				LIST='TAZ','EMPd10', 'EMPd20', 'EMPd30', 'EMPd45', 'EMPd60', 'EMPd60p'
		ENDIF
		
		;print matrix data to a linear csv file
		PRINT CSV=T, FORM=9, 
			FILE='EMPAccess.csv',
  		LIST=I(5.0),	D10, D20, D30, D45, D60, D60P
		
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
					D60 = ZONES
					D60P= ZONES
					
					D10 = 0
					D20 = 0
					D30 = 0
					D45 = 0
					D60 = 0
					D60P= 0
		; loop through all zones, and sum the opportunities accessibile
		JLOOP
			D10 = D10 + mi.1.d10 * zi.1.RETAIL[j]
			D20 = D20 + mi.1.d20 * zi.1.RETAIL[j]
			D30 = D30 + mi.1.d30 * zi.1.RETAIL[j]
			D45 = D45 + mi.1.d45 * zi.1.RETAIL[j]
			D60 = D60 + mi.1.d60 * zi.1.RETAIL[j]
			D60P= D60P+ mi.1.d60p* zi.1.RETAIL[j]
		ENDJLOOP
				
		IF (I=1)
				PRINT CSV=T, FORM=12.2,
				FILE='RETAccess.csv',
				LIST='TAZ','RETd10', 'RETd20', 'RETd30', 'RETd45', 'RETd60', 'RETd60p'
		ENDIF
		
		;print matrix data to a linear csv file
		PRINT CSV=T, FORM=9, 
			FILE='RETAccess.csv',
  		LIST=I(5.0),	D10, D20, D30, D45, D60, D60P

ENDRUN

