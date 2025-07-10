!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 28-08-97 6:00p   $
!
! current revision: $Revision:: 7               $


      SUBROUTINE INIT2 (IDAYWK, ITMSTP, IFlagRRRunoff, CallFromInit1)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialisatie per tijdstap in een event
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = debug file unit
! ***  IHOUR  = uur
! ***  IDAYWK = dag van de week
! *********************************************************************


    USE CONF_FIL
    USE CONF_ARR
    use Boundary
    use Paved
    use NWRW
    use Unpaved
    use Openwater
    use Structures
    use Greenhouse
    use RWZI
    use Industry
    use Messages
    use Output
    use Sacramento
    use RRRunoff

    Implicit none

      Real    RValue(2)
      INTEGER IDAYWK
      Integer iMap, NR, IMFL
      Integer iTmStp
      Integer iDebug, Iout1, IHour
      Integer IFlag, iFlagRRRunoff, CallFromInit1


      iFlag = 1   ! Call Capsim2Storagecoefficient om storage coefficient te bepalen
      iOut1  = ConfFil_get_iOut1()
      iDebug = ConfFil_get_iDebug()

      iHour = ConfArr_get_iHour()


      IF (iDebug .ne. 0) WRITE (IDEBUG,1)
    1 FORMAT (' INIT2')

! *********************************************************************
! *** Initialiseer: flooding in huidige tijdstap=FALSE
! *********************************************************************

      call ConfArr_set_FLDOVH(.FALSE.)
      call ConfArr_set_fldVhg(.FALSE.)
!
! *********************************************************************
! *** Initialiseer:
! ***  initiele berging nieuwe tijdstap = berging eind huidige tijdstap
! *********************************************************************
! *** bereken droogweerafvoer
! ***   optie 1: aantal inwoners * constante dwa per capita per uur
! ***   optie 2: aantal inwoners * variabele dwa per capita per uur
! ***   optie 3:   1  *            constante dwa per uur
! ***   optie 4:   1  *            variabele dwa per uur
! *** input in liter per hour, liter per day, 24 percentages.
! *********************************************************************

      IF (iDebug .ne. 0)  WRITE(IDEBUG,*) ' INIT2: IHOUR=', ConfArr_get_IHOUR()

! Verhard gebied
      Call Init2Paved (IHour, NrSHr)

! Onverhard gebied
      Call Init2Ovh (Itmstp, iflag)

!Kasgebied
      Call Init2Kas

!Open water
      Call Init2OpenWater

!structures
      Call INIT2str (IDAYWK, ITMSTP)

!Boundaries
      Call Init2Bound

!NWRW inloopknopen; bereken DWA
      Call Init2NWRW (Idebug, IOut1, IHour, NRSHR, Itmstp)

! Sacramento
      Call Sacramento_Init2

! RWZI
      Call Init2RWZI

! Industry
      Call Init2Industry(Idebug, IOut1, IHour)

! RRConnectionnodes and Bifurcationsnodes
      Call Init2RRConnection
      Call Init2RRBifurcation

! Cel
!     Call Init2Cel (Idebug, IOut1, IHour, NRSHR, Itmstp)

! RRRunoff
      Call RRRunoffNode_Init2 (iflagRRRunoff, CallFromInit1, Itmstp)

!Inititalisatie detail uitvoer RSLMAP

      RValue(1) = 0.0
      RValue(2) = 0.0

      DO IMAP=1,NKAART
        NR = NFLMAP(IMAP) / 2
        DO IMFL = NR+1, NFLMAP(IMAP)
              select case (imap)
                case (1)
                  Call InitResultArray  (IMAP, Rslmap1_vhg, MaxSeriesPerMap(imap), Nvhg  , 2, RValue, 1)
                case (2)
                  Call InitResultArray  (IMAP, Rslmap2_ovh, MaxSeriesPerMap(imap), Novh  , 2, RValue, 1)
                case (3)
                  Call InitResultArray  (IMAP, Rslmap3_kas, MaxSeriesPerMap(imap), Nkas  , 2, RValue, 1)
                case (4)
                  Call InitResultArray  (IMAP, Rslmap4_ow , MaxSeriesPerMap(imap), Now   , 2, RValue, 1)
                case (5)
                  Call InitResultArray  (IMAP, Rslmap5_str, MaxSeriesPerMap(imap), Nstr  , 2, RValue, 1)
                case (6)
                  Call InitResultArray  (IMAP, Rslmap6_bnd, MaxSeriesPerMap(imap), Nbnd  , 2, RValue, 1)
                case (7)
                  Call InitResultArray  (IMAP, Rslmap7_plv, MaxSeriesPerMap(imap), Nplv  , 2, RValue, 1)
                case (8)
! uitzetten ivm ARS 1648.
! test toch weer aanzetten voor t=1
                  if (itmstp .eq. 1) &
                   Call InitResultArray  (IMAP, Rslmap8_bal, MaxSeriesPerMap(imap), NNod, 2, RValue, 2)
! end test
                case (9)
                  if (islcmp .eq. 0) then
                    Call InitResultArray  (IMAP, Rslmap9_slt, MaxSeriesPerMap(imap), 1     , 2, RValue, 1)
                  else
                    Call InitResultArray  (IMAP, Rslmap9_slt, MaxSeriesPerMap(imap), NNod  , 2, RValue, 1)
                  endif
                case (10)
                  Call InitResultArray  (IMAP, Rslmap14_rwzi, MaxSeriesPerMap(imap), Nrwzi , 2, RValue, 1)
                case (11)
                  Call InitResultArray  (IMAP, Rslmap15_ind , MaxSeriesPerMap(imap), Nindus, 2, RValue, 1)
                case (12)
                  Call InitResultArray  (IMAP, Rslmap17_Sacr, MaxSeriesPerMap(imap), NcSacr, 2, RValue, 1)
                case (13)
                  Call InitResultArray  (IMAP, Rslmap16_flows, MaxSeriesPerMap(imap), Nclink, 2, RValue, 1)
                case (14)
!                 Call InitResultArray  (IMAP, Rslmap18_cel, MaxSeriesPerMap(imap), NcCell, 2, RValue, 1)
                case (15)
                  Call InitResultArray  (IMAP, Rslmap19_RRRunoff, MaxSeriesPerMap(imap), NcRRRunoff, 2, RValue, 1)
              end select
        END DO
      END DO


      RETURN
      END
