subroutine SODEIN ( nqlat, ngrid,&
&juer,  ker&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEIN (SObek DEclare INterface variables)
!
! Module description: Declare variables for aggregation of water flow
!                     results to water quality results.
!
!                     Declare variables for aggregation of water flow
!                     results to a water quality interface file. This
!                     routine is called
!
!                     1. if the aggragation is switched on in combinati-
!                     on with the flow module;
!                     2. if the water quality interface module is swit-
!                     ched on with or without the flow module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 juer              P  -
!  4 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 ngrid             I  Number of grid points in network.
!  1 nqlat             I  Number of lateral discharge stations.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtrpnt  GeT Real PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mkrpnt  MaKe Real PoiNTer
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sodein.pf,v $
! Revision 1.4  1995/09/22  10:03:58  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:46  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:39  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:04  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:36  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer          nqlat, ngrid,&
   &juer,  ker
!
!     Variables
!
   integer          errcod, size  , errno
   character*16     name
   character*50     txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer          mkrpnt, mkipnt, gtrpnt
   external         mkrpnt, mkipnt, gtrpnt
!
!     Delwaq time step counter
!
   size   = 1
   name   = 'DLWQTS'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Flow widths, areas and flows for main and sub section 1
!
   name   = 'AFWFQS'
   if (gtrpnt ( name ) .lt. 0) then
      size   = ngrid * 8
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif
!
!     Chezy coefficients (Whole, Main, Sub 1, Sub 2)
!
   name   = 'CPACK'
   if (gtrpnt ( name ) .lt. 0) then
      size   = ngrid * 4
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif
!
!     Aggregated flows
!
   size   = ngrid * 3
   name   = 'QAGGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Aggregated lateral discharges
!
   size   = nqlat
   name   = 'QLAGGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Hydraulic radius (Whole, Main, Sub 1, Sub 2)
!
   name   = 'RPACK'
   if (gtrpnt ( name ) .lt. 0) then
      size   = ngrid * 4
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif
!
!     Flow widths and areas
!
   name   = 'WAOFT'
   if (gtrpnt ( name ) .lt. 0) then
      size   = ngrid * 6
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif

   return
!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODEIN Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
