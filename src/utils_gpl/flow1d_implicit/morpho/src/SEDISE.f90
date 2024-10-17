subroutine sedise (sedbr  ,maxtab ,ntabm  ,stot  ,qtot  ,&
&sdrdbf ,ntab   ,table  ,dissdb )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDISE (SEdiment DIstribute SEdredge)
!
! Module description: Distribute sediment for a Sedredge nodal
!                     gridpoint.
!
!                     This routine distributes the total incoming sedi-
!                     ment transport of a sedredge branch over the left
!                     and right channel.
!
! Precondition:       The total incoming transport en discharge in the
!                     branch have signs corresponding to the branch
!                     direction. Coefficient ALPHA-s is positive.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 dissdb(2)         O  (1) distributed sediment in left channel
!                         (2) distributed sediment in right channel
!  2 maxtab            I  Maximum number of defined tables.
!  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  3 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  5 qtot              I  Total discharge flowing towards node.
!  6 sdrdbf(2,nsedrd)  I  Defines for each sedredge branch the pointer
!                         to the sediment distribution function and the
!                         flow distribution function.
!                         (1,j) = Table pointer Q distribution table for
!                                 sedredge branch in case the sedredge
!                                 branch starts at an internal node. If
!                                 the sedredge branch starts at a boun-
!                                 dary the table pointer points to a
!                                 table with boundary condition for the
!                                 right channel.
!                         (2,j) = Table pointer S distribution table for
!                                 sedredge branch j.
!  1 sedbr             I  Sedredge branch number
!  4 stot              I  Total sediment transport going towards node.
!  8 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sedise.pf,v $
! Revision 1.3  1995/10/18  09:00:38  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:07:19  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:21  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:34  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:45  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:20  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    sedbr ,maxtab,ntabm
   integer    sdrdbf(2,*)      ,ntab(4,maxtab)
   real       stot
   real       table (ntabm)    ,dissdb(2)
   double precision qtot
!
!     Declaration of local parameters
!
   integer    itab
   real       alphas
!
   itab = sdrdbf(2,sedbr)
!
!     Interpolate in table and get alpha-s = f(Q)
!
   call inttab (ntab (1,itab)      ,ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &dble(qtot)  ,alphas    )
!
   dissdb(1) = stot * alphas
   dissdb(2) = stot * (1.-alphas)
!
end
