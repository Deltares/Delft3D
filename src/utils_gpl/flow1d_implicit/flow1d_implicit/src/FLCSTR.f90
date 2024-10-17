subroutine FLCSTR(time   ,dt1    ,istep  ,g      ,rho    ,ncontr ,&
&contrl ,strtyp ,strpar ,ngrid  ,h      ,q      ,&
&af     ,maxtab ,ntabm  ,ntab   ,table  ,ncsrel ,&
&cnstrl ,ntrigr ,triger ,ntcrel ,trcnrl ,nstru  ,&
&strhis ,conhis ,cnpflg ,lagstm ,nlags  ,buflag ,&
&nqlat  ,qltpar ,juer   ,lrest  ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCSTR (FLow Controlled STRuctures)
!
! Module description: This routine determines for each controlled
!                     structure the new value of the controlled
!                     variables.
!
!                     At the start of a new time step each controlled
!                     structure will be processed by this routine. This
!                     will be done in the following steps:
!                     - First, routine FLTRIG is called to check each
!                       trigger. A trigger must return the value true
!                       or false.
!                     - Secondly, routine FLTRCO is called to determine
!                       the status of each controller (active or
!                       not-active). The status depends on the value of
!                       the related triggers.
!
!                     A trigger is related to one or more controllers.
!                     A controller is related to zero, one or more
!                     triggers.
!
!                     - Copy "current control values" to "previous
!                       values". (c(tn-1) = c(tn))
!                     - Thirdly, routine FLCONT is called to calculate
!                       the values of the structure parameters
!                       controlled by all active controllers. These
!                       values are stored in array conhis.
!                     - Finally, routine FLCOST copies the actual values
!                       of the controlled parameters to the arrays of
!                       the related structures (strpar and strhis).
!
!                     A controller is related to one or more structures.
!                     A structure is related to zero, one or more
!                     controllers.
!
!
!
!                     In routine FLTRIG all triggers will be evaluated.
!                     The values are ON=true or OFF=false.
!                     Dependend of the value of a trigger (ON or OFF)
!                     related controllers will be triggered.
!
!                     In routine FLTRIG the actual trigger (ON/OFF) is
!                     computed. Three options are possible:
!
!                     - time trigger;
!                     - hydraulic trigger;
!                     - combination of time and hydraulic trigger.
!
!                     It is possible to perform a logical AND or
!                     logical OR function on the time- and hydraulic
!                     trigger.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 af                P  -
! 19 cnstrl            P  -
! 26 conhis            P  -
!  7 contrl            P  -
!  2 dt1               P  -
!  4 g                 P  -
! 11 h                 P  -
!  3 istep             P  -
! 14 maxtab            I  Maximum number of defined tables.
!  6 ncontr            I  Number of controlled structures.
! 18 ncsrel            P  -
! 10 ngrid             I  Number of grid points in network.
! 24 nstru             I  Number of structures.
! 16 ntab              P  -
! 15 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 22 ntcrel            P  -
! 20 ntrigr            P  -
! 12 q                 P  -
!  5 rho               P  -
! 25 strhis(10,nstru)  IO For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
!  9 strpar            P  -
!  8 strtyp            P  -
! 17 table             P  -
!  1 time              P  -
! 23 trcnrl            P  -
! 21 triger            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flcont  FLow CONTrollers
! flcost  FLow COntroller STructure relations
! fltrco  FLow TRigger COntroller relations
! fltrig  FLow TRIGger
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcstr.pf,v $
! Revision 1.11  1999/03/15  15:49:44  kuipe_j
! tabs removed
!
! Revision 1.10  1998/12/11  13:05:05  kuipe_j
! avoid test dupl contr in autostart
!
! Revision 1.9  1998/06/08  12:29:42  kuipe_j
! time lag hydr controller
!
! Revision 1.8  1996/09/03  14:51:53  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.7  1996/05/30  09:56:34  kuipe_j
! general structure dlim, controllers
!
! Revision 1.6  1996/01/17  14:38:17  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:01:12  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:29  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:54:55  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:52  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:37  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:23  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:46  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer istep , ncontr, maxtab, ntabm, ngrid, ncsrel, ntrigr
   integer ntcrel, nstru , juer  , ker, lagstm ,nlags, nqlat
   integer strtyp(10,nstru), ntab(4,maxtab), cnstrl(2,*)
   integer triger(10,*), trcnrl(5,*) , cnpflg(dmcopr,*)
   real    g, table(ntabm), strpar(dmstrpar,nstru)
   real    af(ngrid), contrl(17,*) ,rho(ngrid)
   real    conhis(5,nstru), strhis(dmstrh,*), buflag(lagstm,nlags)
   real    qltpar(9,nqlat)
   logical lrest
   double  precision  time ,dt1, h(ngrid), q(ngrid)
!
!     Declaration of local variables:
!
   integer i, j
   logical lauto
!
   lauto = istep .eq. 0
   if (.not. lauto) then
      call FLTRIG(time   ,strtyp ,ntrigr ,triger ,maxtab ,ntabm  ,&
      &ntab   ,table  ,ngrid  ,h      ,q      ,strhis ,&
      &nqlat  ,qltpar ,strpar ,g      ,rho    )
!
      call FLTRCO(ntcrel ,trcnrl ,triger, contrl )
!
!     Copy "current control values" to "previous values"
!
      do 10 i = 1, nstru
         do 10 j = 1, 3
            strhis(j+4,i) = strhis(j,i)
10    continue
!
!     Collect discharges on locations with time lag.
!
      call FLTLAG(3   ,ncontr ,contrl ,lagstm ,nlags  ,buflag ,&
      &dt1 ,ngrid  ,q      )
   endif
!
   call FLCONT(lauto  ,time   ,dt1    ,istep  ,g      ,rho    ,&
   &strtyp ,ngrid  ,h      ,q      ,af     ,maxtab ,&
   &ntabm  ,ntab   ,table  ,ncontr ,contrl ,ncsrel ,&
   &cnstrl ,strhis ,conhis ,strpar ,nstru  ,lrest  )
!
   call FLCOST(ncsrel ,nstru  ,strtyp ,strpar ,strhis ,cnstrl ,&
   &conhis ,contrl ,cnpflg ,juer   ,ker    )
!
   if (lauto) then
      do 20 i=1,ncontr
         contrl(2,i) = 1.
20    continue
   endif
end
