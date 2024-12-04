subroutine seini (nbran  ,ngrid  ,nsedrd ,g      ,sedpar ,branch,&
&sedinf ,nucoef ,uscoef ,trform ,grsize ,forcon,&
&sedtr  ,celer  ,dissed ,sedini ,juer   ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEINI (SEdiment INItialisation)
!
! Module description: Calculate all coefficients in the sediment trans-
!                     port formulas that are constant in time. The coef-
!                     ficients are calculated in every grid point in the
!                     relevant sections.
!
!                     Initialize the sediment transport result arrays
!                     and the celerity array.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 13 celer(ngrid,*)    O  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 14 dissed(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
! 11 forcon(4,ngrid,*) O  Constants in transport formulas that do not
!               1|3       vary in time.
!                         Index 1:      A transport formula may have 1
!                                       till 4 constants.
!                         Index 2:      Grid point number.
!                         Index 3:      Section number:
!                         - Normal branches:
!                         (i,j,1) =     Constant in main section.
!                         - Sedredge branches:
!                         (i,j,1) =     Constant in left channel.
!                         (i,j,2) =     Constant in right channel.
!                         (i,j,3) =     Constant in exchange region.
!  4 g                 P  -
! 10 grsize(4,ngrid,*) I  Grain sizes for each gridpoint and section.
!                  1|2    Depending on the transport formula chosen one
!                         or more of the grain sizes will be defined.
!                         When a branch has been marked as a sedredge
!                         branch the D50 value should always be defined,
!                         no matter what transport formula has been
!                         chosen. For normal branches only section 1
!                         will be used being the D size in the main and
!                         sub sections. For sedredge branches section 1
!                         will be the D size in the left channel and
!                         section 2 will be the D size in the right
!                         channel.
!                         (1,i,j) =     D35 value for section j, grid-
!                                       point i.
!                         (2,i,j) =     D50 value for section j, grid-
!                                       point i.
!                         (3,i,j) =     D90 value for section j, grid-
!                                       point i.
!                         (4,i,j) =     Dmedium value for section j,
!                                       gridpoint i.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  3 nsedrd            I  Number of defined sedredge branches.
!  7 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  5 sedpar(10)        I  (1) = kinematic viscosity
!                         (2) = packing factor (porosity)
!                         (3) = relative density
!                         (4) = bed form height ratio
!                         (5) = reduction factor for sed. transp. width
! 12 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!  9 trform(3,nbran)   I  Defines for each branch a transport formula:
!                         (1,j) = Transport formula number:
!                                 ctrfeh (1) : Engelund & Hansen
!                                 ctrfmm (2) : Meyer-Peter & Muller
!                                 ctrfaw (3) : Ackers & White
!                                 ctrfvr (4) : Van Rijn
!                                 ctrfpk (5) : Parker & Klingeman
!                                 ctrfud (6) : User Defined Formula
!                         (2,j) = Starting index of the user coeffi-
!                                 cients for this branch.
!                         (3,j) = Calibration factor for transport for-
!                                 mula.
!  8 uscoef            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! setrfo  SEdiment TRansport FOrmula
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: seini.pf,v $
! Revision 1.7  1999/06/01  13:42:42  kuipe_j
! names in messages substituted + message template
!
! Revision 1.6  1999/03/15  15:53:37  kuipe_j
! tabs removed
!
! Revision 1.5  1998/02/25  12:48:56  kuipe_j
! Check on grain size added
!
! Revision 1.4  1998/02/13  12:09:57  kuipe_j
! Avoid pointer toempty array
!
! Revision 1.3  1997/06/17  11:27:15  kuipe_j
! output in history format
!
! Revision 1.2  1995/05/30  07:07:24  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:25  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:53  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:21  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer    nbran ,ngrid   ,nsedrd, juer, ker, nucoef
   integer    branch(4,nbran), sedinf(2,nbran)
   integer    sedini(*)
   real       g
   real       uscoef(nrcoefs,nucoef) ,trform(3,nbran)  ,&
   &dissed(4,nbran)        ,sedpar(*)        ,&
   &grsize(4,ngrid,*)      ,forcon(4,ngrid,*),&
   &sedtr (ngrid,*)        ,celer (ngrid,*)
!
!     Declaration of local parameters
!
   integer    ibr   ,igr   ,ind   ,isec ,nsec  ,ised  ,i  ,j  ,k  ,&
   &n1    ,n2    ,ifil, nmess ,lbrnam,ibrn
   real       velo  ,chezy ,depth ,hrad ,sedtra
   real       pacfac,relden,kinvis, xc
   real       grn(4)
   logical    incall
   character*40    branam
   character*11    xtxt
!
   kinvis = sedpar(1)
   relden = sedpar(2)
   pacfac = sedpar(3)
!
!     Set initial flags for HIS files
!
   do 5 ifil=1,2
      sedini(ifil)=0
5  continue
!
!     Set some array's initialy to zero. This avoids undefined values
!     when some of the branches are sedredge branches.
!
   if (nsedrd .eq. 0) then
      n1 = 1
      n2 = 1
   else
      n1 = 3
      n2 = 2
   endif
!
   do 40 j=1,ngrid
      do 20 k=1,n1
         do 10 i=1,4
            forcon(i,j,k) = 0.
10       continue
         sedtr(j,k) = 0.
20    continue
      do 30 k=1,n2
         celer(j,k) = 0.
30    continue
40 continue
!
   do 60 j=1,nbran
      do 50 i=1,4
         dissed(i,j) = 0.
50    continue
60 continue
!
!     Initialize parameters that are not used when the transport
!     formulas are called in initial mode.
!
   velo   = 0.
   chezy  = 0.
   depth  = 0.
   hrad   = 0.
   sedtra = 0.
!
   nmess = 0
   do 100 ibr=1,nbran
!
      ind  = max(int(trform(2,ibr)),1)
      ised = sedinf(1,ibr)
!
!        IND  = Index of coefficients for user defined formula of
!               current branch.
!        ISED = Sedredge branch number.
!
      if (ised.eq.0) then
         nsec = 1
      else
         nsec = 2
      endif
!
      incall = .true.
!
      do 90 igr = branch(3,ibr),branch(4,ibr)
!
!           Calculate constants through channels 1 (and 2).
!
         do 70 isec = 1,nsec
!
            call setrfo&
            &(incall  ,g        ,pacfac       ,relden,kinvis     ,&
            &grsize(1,igr,isec),chezy ,velo  ,depth ,hrad       ,&
            &uscoef(1,ind)     ,trform(1,ibr),forcon(1,igr,isec),&
            &sedtra)
!
!
!              Check if grainsize is specified
!              Forcon(1,,) is set to -1001 if grain size was not
!              specified
!
            if (nmess .lt. 11) then
               if (forcon(1,igr,isec) .lt. -1000.) then
                  nmess = nmess + 1
                  ker   = fatal
                  call getloc (igr,ibrn,xc)
                  write (xtxt,'(f10.2)') xc
                  call getbrn (ibrn,branam,lbrnam)
                  call error (juer ,&
                  &'SEINI grain sizes too small'//&
                  &' at branch @'//branam(:lbrnam)//'@ X= @' //&
                  &xtxt//'@', esegrn , ker )
               endif
            endif
70       continue

         if (nsec .eq. 2) then
!
!              Calculate constants in exchange region (Sedredge)
!              At first calculate averaged grain sizes.
!
            do 80 i = 1, 4
               grn(i) = (grsize(i,igr,1) + grsize(i,igr,2)) *.5
80          continue
!
            call setrfo&
            &(incall  ,g    ,pacfac,relden  ,kinvis,grn     ,&
            &chezy         ,velo  ,depth   ,hrad  ,&
            &uscoef(1,ind) ,trform(1,ibr)  ,forcon(1,igr,3),&
            &sedtra        )
!
         endif
!
90    continue
100 continue
!
end
