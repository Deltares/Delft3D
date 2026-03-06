subroutine SODEFK ( juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEFK (SObek DEclare in Flow mod Kalman vars)
!
! Module description: Declare variables for usage in the flow module,
!                     if the Kalman module is not present.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 juer              P  -
!  2 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
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
! $Log: sodefk.pf,v $
! Revision 1.3  1996/12/02  10:03:43  kuipe_j
! avoid negative pointers
!
! Revision 1.2  1996/04/12  13:05:58  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Parameters
!
   integer          juer   ,ker
!
!     Variables
!
   integer          errcod, size  , errno
   integer          nnc   , nnm   , nns   , nnn   ,nnf   ,nnmu,&
   &nosdim
   character*16     name
   character*80     txt
   parameter        ( nnc  = 1,&
   &nnm  = 1,&
   &nns  = 1,&
   &nnn  = 1,&
   &nnf  = 1,&
   &nnmu = 1&
   &)
!
!     External functions
!
   integer          mkipnt, mkrpnt, gtipnt
   external         mkipnt, mkrpnt, gtipnt
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     correction parameters for the bottom friction.
!
   size   = nnf
   name   = 'PFA'
   errcod = mkrpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     correction parameters for contraction at free gate flow.
!
   size   = nnmu
   name   = 'PMUA'
   errcod = mkrpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     correction parameter for the wind stress.
!
   size   = 1
   name   = 'PW'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SClceq
!
   size   = nnc+1
   name   = 'SCLCEQ'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCceq
!
   size   = 1
   name   = 'SCCEQ'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SClmeq
!
   size   = nnm+1
   name   = 'SCLMEQ'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCmeq
!
   size   = 1
   name   = 'SCMEQ'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCifri
!
   size   = 1
   name   = 'SCIFRI'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCimu
!
   size   = 1
   name   = 'SCIMU'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SClqhs
!
   size   = nns+1
   name   = 'SCLQHS'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCqhs
!
   size   = 1
   name   = 'SCQHS'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SClnod
!
   size   = nnn+1
   name   = 'SCLNOD'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     SCnode
!
   size   = 1
   name   = 'SCNODE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Uncorr. random noise proc. for the continuity equation
!
   size   = 1
   name   = 'NNC'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nnc
!
   ip(gtipnt(name)) = nnc
!
!     Uncorr. random noise proc. for the bott. fr. corr.  equation
!
   size   = 1
   name   = 'NNF'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nnf
!
   ip(gtipnt(name)) = nnf
!
!     Uncorr. random noise proc. for the momentum equation
!
   size   = 1
   name   = 'NNM'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nnm
!
   ip(gtipnt(name)) = nnm
!
!     Uncorr. random noise proc. for the contr. coeff. corr.  equation
!
   size   = 1
   name   = 'NNMU'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nnmu
!
   ip(gtipnt(name)) = nnmu
!
!     Uncorr. random noise proc. for the nodal eq. (boundary conditions)
!
   size   = 1
   name   = 'NNN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nnn
!
   ip(gtipnt(name)) = nnn
!
!     Uncorr. random noise proc. for the (Q,h) relation
!
   size   = 1
   name   = 'NNS'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nns
!
   ip(gtipnt(name)) = nns
!
!     Set and store noise dimension
!
   size   = 1
   name   = 'NOSDIM'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
   nosdim     = 2
   ip(errcod) = nosdim
!

!
!     Dummy arrays for system noise
!
   size   = 1
   name = 'SNCEQ'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNFRIC'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNMEQ'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNMU'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNNODE'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNQHS'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
   name = 'SNWIND'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
   return
!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODEFK Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
