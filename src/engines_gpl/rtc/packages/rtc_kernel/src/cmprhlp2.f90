!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

      Double Precision FUNCTION CMPRHLP2 (NAME, INDEXA, VARHIS, SHIFT, COEF, &
                                RESULTS, IDEBUG, ITIM, NLOC, NDECV, &
                                NSPAR, NPARS, NTIMS, IPARA, IVAL)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  RTC  version 1.0.                   Date: June 1997
! *********************************************************************
! *** Last update: June   1997       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Compute value to be added to decision variable
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NAME    = name/indicatie soort variabelen (Sobek/3B/etc)
! ***  INDEXA  = geeft bij beslisparameter IPARA, variabele lokatie IVAL: het lokatienummer ILOC in de HIS file
! ***  VARHIS  = geeft bij beslisparameter IPARA, variabele lokatie IVAL: het serienummer IPAR in de HIS file
! ***  SHIFT   = geeft bij beslisparameter IPARA, variabele lokatie IVAL: de gebruikte timeshift (0=geen time shift, -1=1 tijdstap terug etc.)
! ***  COEF    = geeft bij beslisparameter IPARA, variabele lokatie IVAL: de coefficienten voor vermenigvuldiging en optelling
! ***  RESULTS = array met resultaten van huidige en vorige tijdstappen
! ***  IDEBUG  = file unit number of debug file
! ***  ITIM    = actuele tijdstap
! ***  NLOC    = max. aantal lokaties
! ***  NDECV   = max. aantal beslisparameters
! ***  NSPAR   = max. aantal variabelen per type (Sobek/3B/etc) per beslisparameter
! ***  NPARS   = max. aantal series (parameters) in het RESULTS array
! ***  NTIMS   = max. aantal tijdstappen
! ***  IPARA   = actuele beslisparameter
! ***  IVAL    = actuele variabele/lokatie van de beslisparameter
! *********************************************************************
!
      INTEGER ILOC, IPAR, IT, IPARA, IVAL
      Double Precision Rhlp, CFMULT, CFADD
      CHARACTER*6   NAME
!
      INTEGER NDECV, NSPAR, NLOC, NPARS, NTIMS, IDEBUG, ITIM
      INTEGER INDEXA(NDECV,NSPAR), VARHIS(NDECV,NSPAR), SHIFT(NDECV,NSPAR)
      Double Precision COEF(NDECV,NSPAR,2), RESULTS (NLOC,NPARS+NTIMS)
!
!
      IF (IDEBUG .GT. 0) WRITE (IDEBUG,1)
    1 FORMAT (' Cmprhlp2')
!
! *********************************************************************
! *** Bepaal waarden beslisparameters
! *********************************************************************
! *** NB time shift in array TISSBK is 0 of negatief;
! ***    tijdindex NTIMS   = huidige tijdstap
! ***    tijdindex NTIMS-1 = een tijdstap terug
! ***    tijdindex NTIMS-2 = twee tijdstappen terug etc.
! ***      in eerste tijdstap nog geen vorige tijdstap waarde bekend;
! ***      neem dan waarde van huidige tijdstap!
! *********************************************************************
!
      ILOC   = INDEXA (IPARA,IVAL)
      IPAR   = VARHIS (IPARA,IVAL)
      IT     = MAX (NTIMS + SHIFT (IPARA,IVAL), NTIMS-ITIM+1)
      IF (SHIFT(IPARA,IVAL) .GT. 0) IT = MAX (NTIMS+ SHIFT(IPARA,IVAL)-1, NTIMS-ITIM+1)
      CFMULT = COEF (IPARA,IVAL,1)
      CFADD  = COEF (IPARA,IVAL,2)
      RHLP   = CFMULT * RESULTS (ILOC,IPAR+IT-1) + CFADD
      IF (IDEBUG .GT. 0) THEN
        WRITE(IDEBUG,*)  ' Add var        ', NAME
        WRITE(IDEBUG,*)  '  index         ', IVAL
        WRITE(IDEBUG,*)  '  lokatie in HIS', ILOC
        WRITE(IDEBUG,*)  '  parameter     ', IPAR
        WRITE(IDEBUG,*)  '  tijdindex     ', IT
        WRITE(IDEBUG,*)  '  coefficienten ', CFMULT, CFADD
        WRITE(IDEBUG,*)  '  uit HIS file  ', RESULTS(ILOC,IPAR+IT-1)
        WRITE(IDEBUG,*)  '  RHLP       =  ', RHLP
      ENDIF

      CMPRHLP2 = RHLP

      RETURN
      END
