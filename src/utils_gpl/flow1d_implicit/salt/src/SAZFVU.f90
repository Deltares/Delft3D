subroutine sazfvu (im     ,nnode ,nhstat ,maxtab ,ntabm ,time   ,&
&dt     ,tp    ,lslack ,mouth  ,node  ,hbdpar ,&
&ntab   ,table ,emppar ,mouqpu )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAZFVU (SAlt Zwendl Flood Vol. and max. vel.(U))
!
! Module description: Calculate flood volume and maximum flood velocity
!                     for next tide.
!
!                     At the start of a tidal period a new flood volume
!                     and maximum velocity have to be calculated. This
!                     is done by reading the hydrodynamic boundary con-
!                     ditions and determination of the highest and lo-
!                     west water level over the next tidal period. Note
!                     that for the Zwendl formulation the boundary con-
!                     dition for the mouth must be a water level as a
!                     function of time.
!
!                     The obtained flood volume and the maximum flood
!                     velocity are stored for the last tide and the
!                     values of the last tide are stored for the one but
!                     last tide.
!
! Precondition:       It is already determined if it is the start of a
!                     new tidal period.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  7 dt                I  Computational time step dt [sec].
! 15 emppar(4,nmouth)  I  Constants for each sea mouth for the empirical
!                         formulation:
!                         (1,i) = Constant u0 at sea mouth i.
!                         (2,i) = Constant u1 at sea mouth i.
!                         (3,i) = Constant P0 at sea mouth i.
!                         (4,i) = Constant P1 at sea mouth i.
! 12 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
!                         (1,i) = Location [grid point] for H-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : h = f(t)
!                                 cbfqoh (2) : h = h(Q)
!                                 cbfour (3) : h = fourier
!                                 cbtidl (4) : h = tidal components
!                         (3,i) = Table number for f(t), h(Q), fourier
!                                 or tidal components table.
!  1 im                I  Mouth number.
!  9 lslack            I  = .true.   :  Slack water before flood encoun-
!                                       tered
!                         = .false.  :  Otherwise.
!  4 maxtab            I  Maximum number of defined tables.
! 16 mouqpu(3,0:2,     IO Contains auxilliary data for the Thatcher
!        nmouth)          Harleman or ZWENDL dispersion formulation.
!                         - First index:
!                         (1,,) = Fresh water discharge.
!                         (2,,) = Flood volume.
!                         (3,,) = Maximum flood velocity.
!                         - Second index:
!                         (,0,) = For current tide. Mouqpu(i,0,j) con-
!                                 tains the actual sum or maximum on the
!                                 current time.
!                         (,1,) = For the last tide.
!                         (,2,) = For the tide before the last tide.
!                         - Third index:
!                         (,,i) = Mouth number.
! 10 mouth(2,nmouth)   I  Node numbers which are mouths:
!                         (1,i) = Node number j which is a mouth.
!                         (2,i) = Number of the branch that contains the
!                                 mouth.
!  3 nhstat            I  Number of H-boundary stations.
!  2 nnode             I  Number of nodes.
! 11 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
! 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 14 table             P  -
!  6 time              I  Actual time level tn+1. in sec.
!  8 tp                I  Tidal period   (salt module).
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
! series  SERIES of fourier or tidal component
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sazfvu.pf,v $
! Revision 1.4  1995/10/18  09:00:34  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:21  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:26  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:07  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:31  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:34:17  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:17  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer   im    ,nnode ,nhstat ,maxtab  ,ntabm
   integer   mouth (2,*)      ,node(4,nnode) ,hbdpar(3,nhstat),&
   &ntab  (4,maxtab)
   real      table (ntabm) ,emppar(4,*)      ,mouqpu(3,0:2,*)
   double precision time   ,dt    ,tp
   logical   lslack
!
!     Declaration of local parameters
!
   integer   inod ,ihstat ,iopt ,itab  ,nt  ,it
   real      h    ,hmax   ,hmin
   double    precision     tcur
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   if (lslack) then
!
!        Change from outflow to inflow (slack water before flood).
!        At first, the minimum and maximum water levels at the
!        mouth are calculated in the next tidal period.
!
      inod   = mouth(1,im)
      ihstat = node (3,inod)
      iopt   = int(hbdpar(2,ihstat))
!
!        itab       : TABLE number h=f(t)
!
      itab   = int(hbdpar(3,ihstat))
!
      hmax   = -1.e+10
      hmin   =  1.e+10
      nt     = int(tp / dt)
      do 10 it = 1,nt
         tcur = dble(it-1)*dt + time
         if (iopt .eq. cbftim) then

            call inttab (ntab (1,itab)      ,ntab(4,itab),&
            &table(ntab(2,itab)),&
            &table(ntab(3,itab)),&
            &tcur               ,h           )
         else if (iopt .eq. cbfour .or. iopt .eq. cbtidl) then
            call series (tcur  ,iopt  ,table(ntab(2,itab)),&
            &ntab(1,itab) ,h                  )
         endif
!
         hmax = max(hmax,h)
         hmin = min(hmin,h)
10    continue
!
!        Secondly the flood volume and maximum velocity are calculated.
!        [ Doc. S-FO-001.5KV / Eq 19-10(b,c) ]
!
      mouqpu(2,0,im) = emppar(3,im) + emppar(4,im)*(hmax-hmin)
      mouqpu(3,0,im) = emppar(1,im) + emppar(2,im)*(hmax-hmin)
!
!        Shift flood volume.
      mouqpu(2,2,im) = mouqpu(2,1,im)
      mouqpu(2,1,im) = mouqpu(2,0,im)
!
!        Shift maximum velocity.
      mouqpu(3,2,im) = mouqpu(3,1,im)
      mouqpu(3,1,im) = mouqpu(3,0,im)
!
   endif
!
end
