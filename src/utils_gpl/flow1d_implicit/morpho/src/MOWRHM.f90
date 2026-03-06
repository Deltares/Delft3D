subroutine mowrhm( nmoman, nmomap, nmotim, ngrid , nbran , itim  ,&
&istep , nstep , mormap, mortim, maxlev, hlev  ,&
&sedtr , dissed, branch, typcr , mocpre, dt    ,&
&morini, buffer, sectc , sectv , wft   , sumda ,&
&hlev0 , gridnm)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         A.W.J. Koster
!
! Module:             MOWRHI(MOrphology WRite Morphology Results to
!                            HIs files)
!
! Module description: Subroutine MOWRHI writes the user selected morholo-
!                     gy results to the HIS file.
!
!                     In subroutine MOWRHI the user selected morp. results
!                     at user supplied locations and time levels (System
!                     Specifications for 1D Modelling System Front End)
!                     will be stored on the result file. The stored data
!                     can be processed further by the User Interface.
!                     The user can select functions of place and of
!                     time.
!
!                     Notice that the option for 'adapted cross sections'
!                     is NOT implemented for HIS files.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 20 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 25 buffer(nvar,ngrid)O  Buffer for results to be written to HIS file
! 19 dissed(4,nbran)   I  Redistributed sediment transport at begin and
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
! 24 gridnm            I  Identifiers for grid points
! 17 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  9 istep             I  Current time step number (t(n+1)).
!  8 itim              P  -
! 16 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 23 mocpre            I  Reference table for main codes off output
!                         system
! 14 mormap(nmomap)    I  parameter list for MAP block with morphology
!                         results:
!                         (1)      = Report begin time
!                         (2)      = Report end time
!                         (3)      = Report time step
!                         (4)      = Report parameter 1 main code
!                         (5)      = Report parameter 1 sub code
!                         (i)      = Report parameter 1 main code
!                         (i+1)    = Report parameter 1 sub code
!                         (nmomap) = Report parameter n sub code
! 15 mortim(nmotim)    I  Parameter list for HIST block with morphology
!                         results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report parameter code 1
!                         (i+2)    = Report parameter sub code 1
!                         (nmotim) = Report parameter n sub code
!  7 nbran             I  Number of branches.
!  6 ngrid             I  Number of grid points in network.
!  3 nmoman            I  Number of main codes morphology module
!  4 nmomap            I  Number of entries in sedmap.
!  5 nmotim            I  Number of entries in sedtim.
! 18 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
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
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! mofdbr  MOrphology FinD BRanch number for a gridpoint
! parsdt  Parse SOBEK date for year, etc.
! resadm  RESults; make ADMinistration for element selection
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mowrhi.pf,v $
! Revision 1.6  1998/11/13  08:59:24  kuipe_j
! output error 2d morphology
!
! Revision 1.5  1998/02/13  12:12:51  kuipe_j
! Adapt to CMT
!
! Revision 1.4  1997/11/26  14:55:42  kuipe_j
! index error while writing cross-section headers
!
! Revision 1.3  1997/11/04  14:10:21  kuipe_j
! write cross-sections in His format
!
! Revision 1.2  1997/06/17  11:56:15  kuipe_j
! Add CVS keys
!
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
   include '..\include\sobcon.i'
   include '..\include\sobdim.i'
!
!     Parameters
!
   integer ,parameter ::   nentri = 6 ,mincros = 5
   integer ,parameter ::   maxpar = nentri + 2*mincros - 1
   integer       nmoman, nmomap ,nmotim, nbran, ngrid, istep,nstep,&
   &maxlev
   integer       itim   (2),&
   &mormap (nmomap),&
   &mortim (nmotim),&
   &mocpre (nmoman),&
   &branch (4,nbran),    typcr(ngrid) ,&
   &morini (2)
   double precision dt, hlev (ngrid,maxlev)
   real          wft(ngrid,maxlev),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &buffer (maxpar,ngrid),&
   &sectc  (ngrid,3),sectv(ngrid,dmsecv),sumda (ngrid),&
   &hlev0  (ngrid)
!
!     Local variables
!
   integer       i, j, k, ie, nlc
   integer       ifil , lun , nsk
   character*40  gridnm(*)
   double precision    scudt
   character*20  parnam(nentri+2*mincros-1)
   character*40  idmap(4) , idhis(4)
   integer       ibr   , iscudt
   integer       ijaar ,imaand ,idag  ,iuur ,imin   ,isec  ,iscu
   integer       nvar  ,igrid ,ivar ,istepf ,istphf

   logical       new   , newuit
   save          istepf, istphf, scudt

   integer                                  :: mvar = 0

!
!     External functions
!
   logical       yesmap
   external      yesmap

!     Clear array buffer:

   do i = 1, maxpar
      do j = 1, ngrid
         buffer( i, j ) = 0
      end do
   end do
!
!     Initialise
!
   idmap(1)  = 'SOBEK                                   '
   idmap(2)  = 'Maps results at gridpoints              '
   idmap(3)  = '                                        '
   idmap(4)  = '                                        '
   parnam(1) = 'Bed level           '
   parnam(2) = 'Mean bed level Main '
   parnam(3) = 'Increase of X-sect. '
   parnam(4) = 'Bed level increase  '
   parnam(5) = 'Integr. sed. transp.'
   parnam(6) = 'X-section level 1   '
   parnam(7) = 'X-section level 2   '
   parnam(8) = 'X-section level 3   '
   parnam(9) = 'X-section level 4   '
   parnam(10)= 'X-section level 5   '
   parnam(11)= 'X-section level 6   '
   parnam(12)= 'X-section level 7   '
   parnam(13)= 'X-section level 8   '
   parnam(14)= 'X-section level 9   '
   parnam(15)= 'X-section level 10  '

   idhis(1)  = 'SOBEK                                   '
   idhis(2)  = 'History results at gridpoints           '
   idhis(3)  = '                                        '
   idhis(4)  = '                                        '
!
!     Write MAP results
!
   if (nmomap .gt. 3) then
      if (yesmap(mormap(1), mormap(2), mormap(3), istep )) then
         nvar = min(nint(0.5*(nmomap-3)),maxpar)
         ifil = 1
         lun  = 58

         if ( morini(ifil) .gt. 0 ) then
!           File exists already hence open again:
            open ( unit = lun, file = mrpmap, form = 'binary',&
            &access = 'APPEND' )
         end if


         if ( morini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = mrpmap , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = mrpmap , form = 'unformatted')
#else
            open(lun , file = mrpmap , form = 'unformatted')
#endif
#endif
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            iscu  = nstep*dt/1.0d9+1.d0
            scudt = dt/dble(iscu)
            write ( idmap(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idmap(i) , i = 1,4)

            ivar = 0
            do 33 i = 4, nmomap, 2

               ie = mocpre(mormap(i)) + mormap(i+1)
               if ( ie .ge. 1 .and. ie .lt. 16 ) ivar = ivar + 1

33          continue

            mvar = ivar
            write(lun) mvar,ngrid
            do 34 i = 4, nmomap, 2

               ie = mocpre(mormap(i)) + mormap(i+1)

               if ( ie .ge. 1 .and. ie .lt. 16 ) then
                  write(lun) ( parnam(ie) )
               end if
34          continue
            write(lun) (igrid , gridnm(igrid)(:20) ,&
            &igrid = 1 , ngrid )
            morini(ifil) = 1
            istepf       = istep
         endif
!
         ivar = 0
         do 50 i = 4, nmomap, 2

            ie = mocpre(mormap(i)) + mormap(i+1)
!
            if (ie .lt. 16) ivar = ivar + 1

!              Find quantities to be written:
            if (ie .eq. 1) then
!
!                 Write bottoms (= min of two first levels)
!
!                 Bottom is min of two lowest levels
!
               do 10 igrid=1,ngrid
                  buffer(ivar,igrid) =&
                  &real( min(hlev(igrid,1), hlev(igrid,2)) )
10             continue
!
            else if (ie .eq. 2) then
!
!                 Write Mean Bed level
!
               do ibr = 1,nbran
                  if (typcr(ibr).ne.ccrsed) then
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buffer(ivar,igrid) =&
                        &sectv(igrid,2)-sectv(igrid,4) /&
                        &sectc(igrid,2)
                     enddo
                  else
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buffer(ivar,igrid) =&
                        &real( hlev(igrid,2) - wft(igrid,1) *&
                        &(hlev(igrid,2) - hlev(igrid,1)) /&
                        &(wft(igrid,1) + wft(igrid,2)) )
                     enddo
                  endif
               enddo

            else if (ie .eq. 3) then
!
!                 Increase in Area
!
               do 14 igrid=1,ngrid
                  buffer(ivar,igrid) = sumda(igrid)
14             continue

            else if (ie .eq. 4) then
!
!                 Increase of bed level
!
               do igrid=1,ngrid
                  buffer(ivar,igrid) = real (min(hlev(igrid,1),&
                  &hlev(igrid,2)) -&
                  &hlev0(igrid) )
               enddo
            else if (ie .eq. 5) then
!
!              Write integrated sediment transports
!
               do 30 ibr = 1, nbran
!
!                 S_int on first and last point from dissed
!
                  buffer(ivar,branch(3,ibr)) = dissed(1,ibr)
                  buffer(ivar,branch(4,ibr)) = dissed(3,ibr)
!
                  do 20 igrid = branch(3,ibr)+1, branch(4,ibr)-1
                     buffer(ivar,igrid) = sedtr(igrid,1)
20                continue
30             continue
!
            else if (ie .ge. 6 .and. ie .lt. 16 ) then
!
!                 Write cross sections
!
               k = mormap(i+1) + 1
               do 40 igrid=1,ngrid
                  buffer(ivar,igrid) = real( hlev(igrid,k) )
40             continue
!
            endif
50       continue
!
!           Compute integer timestep for maps:

         iscudt = nint( (istep-istepf) * scudt )

         write(lun) iscudt
         write(lun) ((buffer(ivar,igrid), ivar  = 1 , mvar),&
         &igrid = 1 , ngrid)
         close ( lun )

      endif
   endif
!
!     Write HIStory results
!
   nlc = mortim(1)
   new = mod(nmotim-nlc,2) .eq. 0
   if (new) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nmotim .gt. 1+nsk .and. 0>1 ) then
      if (new) then
         newuit = yesmap(mortim(nlc+2),mortim(nlc+3),mortim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (mortim(1).gt.0 .and. .not. new) .or. newuit ) then
         nvar = min(nint(.5*(nmotim-mortim(1)-1-nsk)),maxpar)
         ifil = 2
         lun  = 59

         if ( morini(ifil) .gt. 0 ) then
!           File exists already hence open again:
            open ( unit = lun, file = mrphis, form = 'binary',&
            &access = 'APPEND' )
         end if

         if ( morini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = mrphis , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = mrphis , form = 'unformatted')
#else
            open(lun , file = mrphis , form = 'unformatted')
#endif
#endif
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            iscu  = nstep*dt/1.0d9+1.d0
            scudt = dt/dble(iscu)
            write ( idhis(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idhis(i) , i = 1,4)

            ivar = 0
            do 133 i = mortim(1)+2+nsk, nmotim, 2

               ie = mocpre(mortim(i)) + mortim(i+1)

               if ( ie .ge. 1 .and. ie .lt. 16 ) ivar = ivar + 1

133         continue

            mvar = ivar
            write(lun) mvar, mortim(1)

            do 134 i = mortim(1)+2+nsk, nmotim, 2

               ie = mocpre(mortim(i)) + mortim(i+1)
               if ( ie .ge. 1 .and. ie .lt. 16 ) then
                  write(lun) ( parnam(ie) )
               end if
134         continue
!
            write(lun) (mortim(j),gridnm(mortim(j))(:20),&
            &j = 2 , mortim(1)+1)
            morini(ifil) = 1
            istphf       = istep
         endif
!
         ivar = 0
         do 130 i = mortim(1)+2+nsk, nmotim, 2

            ie = mocpre(mortim(i))+mortim(i+1)

            if ( ie .lt. 16 ) ivar = ivar + 1

            if ( ie .eq. 1) then
!
!                 Bottom for selected grid points
!
               do 100 j = 1, mortim(1)
                  buffer(ivar,j) = real(&
                  &min ( hlev(mortim(j+1),1),hlev(mortim(j+1),2)) )
100            continue
!
            else if (ie .eq. 2) then
!
!                 Write Mean Bed level
!
               do j=1,mortim(1)
                  igrid = mortim(j+1)
                  call mofdbr ( nbran ,branch ,igrid ,ibr)
                  if (typcr(ibr).ne.ccrsed) then
                     buffer(ivar,j) =&
                     &sectv(igrid,2)-sectv(igrid,4) /&
                     &sectc(igrid,2)
                  else
                     buffer(ivar,j) = real(&
                     &hlev(igrid,2) - wft(igrid,1) *&
                     &(hlev(igrid,2) - hlev(igrid,1)) /&
                     &(wft(igrid,1) + wft(igrid,2))  )
                  endif
               enddo
!
            elseif (ie .eq. 3) then
!
!                 Increase in Area
!
               do 102 j = 1, mortim(1)
                  buffer(ivar,j) = sumda(mortim(j+1))
102            continue
!
            else if (ie .eq. 4) then
!
!                Increase of bed level
!
               do  j = 1, mortim(1)
                  k = mortim(j+1)
                  buffer(ivar,j) = real( min(hlev(k,1),&
                  &hlev(k,2)) -&
                  &hlev0(k)  )
               enddo
            else if (ie .eq. 5) then
!
!                 S_int for selected grid points
!
               do 110 j = 1, mortim(1)
!
!                    Find branch number of this gridpoint
!
                  call mofdbr ( nbran, branch, mortim(j+1), ibr )
!
!                    Test for first or last point
!
                  if      (mortim(j+1) .eq. branch(3,ibr)) then
                     buffer(ivar,j)= dissed(1,ibr)
                  else if (mortim(j+1) .eq. branch(4,ibr)) then
                     buffer(ivar,j)= dissed(3,ibr)
                  else
                     buffer(ivar,j)= sedtr(mortim(j+1),1)
                  endif
!
110            continue
!
            elseif (ie .ge. 6 .and. ie .lt. 16 ) then
!
!                 Cross sections for selected grid points
!
               k = mortim(i+1) + 1
               do 120 j = 1, mortim(1)
!
                  call mofdbr ( nbran, branch, mortim(j+1), ibr )
!
                  buffer(ivar,j) = real( hlev(mortim(j+1),k)  )
120            continue

            endif
130      continue
!
!           Compute integer timestep for histories:

         iscudt = nint( (istep-istphf) * scudt )

         write(lun) iscudt
         write(lun) ((buffer(ivar,j),&
         &ivar = 1,mvar),&
         &j    = 1,mortim(1))
         close ( lun )

      endif
   endif
!
   return
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
end
