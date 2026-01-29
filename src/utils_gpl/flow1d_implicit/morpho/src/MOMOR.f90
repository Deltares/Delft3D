subroutine momor (fd_nefis_res, nmoman, nmomap, nmotim, ngrid ,&
&nbran , itim  , istep , nstep , first , writim,&
&juer  , mormap, mortim, maxlev, hlev  , sedtr ,&
&dissed, branch, typcr , ncelm , ncelh , mocpre,&
&sectc , sectv , wft   , sumda , hlev0 , buf   ,&
&ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOMOR (MOrphology MOrphology Results)
!
! Module description: Subroutine MOMOR writes the user selected morholo-
!                     gy results to the result file. The result file is
!                     processed by the User Interface.
!
!                     In subroutine MOMOR the user selected salt results
!                     at user supplied locations and time levels (System
!                     Specifications for 1D Modelling System Front End)
!                     will be stored on the result file. The stored data
!                     can be processed further by the User Interface.
!                     The user can select functions of place and of
!                     time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 20 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 24 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
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
! 11 first             I  True in case of first call.
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
! 13 juer              P  -
! 25 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
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
! 22 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 21 ncelm             I  Actual cell number of a map block of result
!                         file.
!  6 ngrid             I  Number of grid points in network.
!  3 nmoman            I  Number of main codes morphology module
!  4 nmomap            I  Number of entries in sedmap.
!  5 nmotim            I  Number of entries in sedtim.
! 10 nstep             I  Last time step number in simulation.
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
! 12 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! mofdbr  MOrphology FinD BRanch number for a gridpoint
! putrel  PUT Real ELement to a nefis file
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
! $Log: momor.pf,v $
! Revision 1.8  1999/03/15  15:52:58  kuipe_j
! tabs removed
!
! Revision 1.7  1997/06/17  11:26:58  kuipe_j
! output in history format
!
! Revision 1.6  1997/02/17  10:23:16  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.5  1997/01/23  08:29:49  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/09/03  14:48:49  kuipe_j
! frequency time hist,Improved sed distribution at nodes
!
! Revision 1.3  1995/05/30  09:55:55  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:53  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:21  hoeks_a
! Initial check-in
!
! Revision 1.3  1995/03/08  09:21:01  kuipe_j
! Improvement message Momor
! Improvement shock celerity
!
! Revision 1.2  1993/11/26  15:32:52  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\sobcon.i'
   include '..\include\sobdim.i'
!
!     Parameters
!
   integer       nmoman, nmomap, nmotim, nbran, ngrid, istep ,&
   &nstep , maxlev, juer  , ncelm , ncelh, ker
   logical       first , writim
   integer       fd_nefis_res,&
   &itim   (2),&
   &mormap (nmomap),&
   &mortim (nmotim),&
   &mocpre (nmoman),&
   &branch (4,nbran) , typcr (ngrid)

   real          wft(ngrid,maxlev),&
   &sedtr  (ngrid,*),&
   &dissed (4,nbran),&
   &buf    (ngrid)  ,sumda (ngrid) ,hlev0(ngrid),&
   &sectc  (ngrid,3),sectv (ngrid,dmsecv)

   double precision hlev   (ngrid,maxlev)

!
!     Local variables
!
   integer       nentri
   parameter    (nentri=6)
   integer       neferr, i, j, k, l, ie, ie1, ie2, nrerr, nsk, nlc,&
   &lastcod, igrid
   integer       usrord(1), uindex(3)
   character*16  grnamm, grnamh, grnamd, name, tmpnam
   character*16  nameel(nentri), quanel(nentri), unitel(nentri),&
!
!     For this module nameac must be as large as possible because
!     HIS_Z0 will be extended to 'maxlev' plus HIS_Zb
!
   &nameac(100)
   character*64  descel(nentri)
   character*2   txt
   character*8   txt2
   logical       llog, new   ,newuit
   integer       ibr
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer       putrel, flsdat
   logical       yesmap
   external      putrel, flsdat, yesmap
!
   data          usrord /1/
!
!     Definition of elements
!
   data  (descel(i),  nameel(i),  quanel(i),&
   &unitel(i), i=1,nentri) /&
!
!      1
   &'Bed level'                       ,'HIS_Zb' ,'Zb' ,'m'   ,&
!      2
   &'Mean Bed level of Main channel'  ,'HIS_Za' ,'Za' ,'m'   ,&
!      3
   &'Increase of Cross-sectional Area','HIS_dA' ,'dA' ,'m2'  ,&
!      4
   &'Increase of Bed level           ','HIS_dZ' ,'dZ' ,'m'   ,&
!      5
   &'Integrated S'                    ,'HIS_Si' ,'Si' ,'m2/s',&
!      6
   &'H-levels'                        ,'HIS_Z0' ,'XS' ,'m'   /
!
   data grnamm, grnamh, grnamd /&
   &'MORP-MAP-GROUP' ,&
   &'MORP-HIS-GROUP' ,&
   &'MORP-DES-GROUP' /
!
   lastcod = 1000000
!
!     Initialise
!
   if (first) then
      nrerr = emoboo
!
      call resini ( fd_nefis_res, grnamd, grnamm , grnamh, 'MORP',&
      &nentri, nmomap, nmotim, ngrid  , itim  , nameel,&
      &quanel, unitel, descel, mocpre , mormap, mortim,&
      &ncelm , ncelh , nameac, lastcod, neferr)
      if (neferr .ne. 0) goto 1000
   endif
!
!     Write MAP results
!
   if (nmomap .gt. 3) then
      if (yesmap(mormap(1), mormap(2), mormap(3), istep )) then
         nrerr = emomap
!
         call restim ( fd_nefis_res, grnamm, 1, itim, ncelm,&
         &nameac, neferr )
         if (neferr .ne. 0) goto 1000
!
         call resdes ( 'MORP', fd_nefis_res, grnamd, 1, writim,&
         &mortim, ncelm , ncelh , neferr )
         if (neferr .ne. 0) goto 1000
!
         uindex (1) = ncelm
         uindex (2) = ncelm
         uindex (3) = 1
!
         do 70 i = 4, nmomap, 2
            ie = mocpre(mormap(i)) + mormap(i+1)
!
            if (ie .eq. 1) then
!
!                 Write bottoms (= min of two first levels)
!
               do 20 ibr = 1, nbran
                  do 10 j = branch(3,ibr), branch(4,ibr)
!
!                       Bottom is min of two lowest levels
!
                     buf(j) = min(hlev(j,1), hlev(j,2))
10                continue
20             continue
!
               name = 'MAP' // nameel(ie)(4:)
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000
!
            else if (ie .eq. 2) then
!
!                 Write Mean Bed level
!
               do ibr = 1,nbran
                  if (typcr(ibr).ne.ccrsed) then
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buf(igrid) = sectv(igrid,2)-sectv(igrid,4) /&
                        &sectc(igrid,2)
                     enddo
                  else
                     do igrid = branch(3,ibr),branch(4,ibr)
                        buf(igrid) = hlev(igrid,2) - wft(igrid,1) *&
                        &(hlev(igrid,2) - hlev(igrid,1)) /&
                        &(wft(igrid,1) + wft(igrid,2))
                     enddo
                  endif
               enddo

               name = 'MAP' // nameel(ie)(4:)
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000

            else if (ie .eq. 3) then
!
!                 Increase in Area
!
               name = 'MAP' // nameel(ie)(4:)
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, sumda )
!
               if (neferr .ne. 0) goto 1000

            else if (ie .eq. 4) then
!
!                 Increase of bed level
!
               do j=1,ngrid
                  buf(j) = min(hlev(j,1),hlev(j,2)) - hlev0(j)
               enddo
!
               name = 'MAP' // nameel(ie)(4:)
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, buf   )
!
            else if (ie .eq. 5) then
!
!                 Write integrated sediment transports
!
               do 40 ibr = 1, nbran
!
!                    S_int on first and last point from dissed
!
                  buf(branch(3,ibr)) = dissed(1,ibr)
                  buf(branch(4,ibr)) = dissed(3,ibr)
!
                  do 30 j = branch(3,ibr)+1, branch(4,ibr)-1
                     buf(j) = sedtr(j,1)
30                continue
40             continue
!
               name = 'MAP' // nameel(ie)(4:)
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000
!
            else if (ie .ge. 6) then
!
!                 Write cross sections
!
               k = mormap(i+1) + 1
               do 60 ibr = 1, nbran
                  do 50 j = branch(3,ibr), branch(4,ibr)
                     buf(j) = hlev(j,k)
50                continue
60             continue
!
!                 Create name for this cross section level
!
               ie1 = min(ie,nentri)
               ie2 = ie-nentri
!
               if (ie2 .gt. 0) then
                  if (ie2 .gt. 9) then
                     write(txt,'(i2)') ie2
                  else
                     write(txt,'(i1,a)') ie2, ' '
                  endif
                  l      = index (nameel(nentri),' ') - 2
                  tmpnam = 'MAP' // nameel(nentri)(4:l)
                  l      = index (tmpnam,' ') - 1
                  name   = tmpnam (1:l) // txt
               else
                  name = 'MAP' // nameel(ie1)(4:)
               endif
!
               neferr = putrel ( fd_nefis_res, grnamm, name,&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000
!
            endif
70       continue
      endif
   endif
!
!     Write HIStory results
!
   nlc = mortim(1)
   new = mod(nmotim-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nmotim .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(mortim(nlc+2),mortim(nlc+3),mortim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (mortim(1).gt.0 .and. .not. new) .or. newuit ) then
         nrerr = emohis
!
         call restim ( fd_nefis_res, grnamh, 1, itim, ncelh,&
         &nameac, neferr )
         if (neferr .ne. 0) goto 1000
!
         call resdes ( 'MORP', fd_nefis_res, grnamd, 2, writim,&
         &mortim, ncelm , ncelh , neferr )
         if (neferr .ne. 0) goto 1000
!
         uindex (1) = ncelh
         uindex (2) = ncelh
         uindex (3) = 1
!
         do 110 i = mortim(1)+2+nsk, nmotim, 2
            ie = mocpre(mortim(i))+mortim(i+1)
            if (ie .eq. 1) then
!
!                 Bottom for selected grid points
!
               do 80 j = 1, mortim(1)
!
                  buf(j) = min ( hlev(mortim(j+1),1),&
                  &hlev(mortim(j+1),2)&
                  &)
!
80             continue
!
               neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),&
               &uindex, usrord, buf  )
!
               if (neferr .ne. 0) goto 1000

            else if (ie .eq. 2) then
!
!                 Write Mean Bed level
!
               do j=1,mortim(1)
                  igrid = mortim(j+1)
                  call mofdbr ( nbran ,branch ,igrid ,ibr)
                  if (typcr(ibr).ne.ccrsed) then
                     buf(j) = sectv(igrid,2)-sectv(igrid,4) /&
                     &sectc(igrid,2)
                  else
                     buf(j) = hlev(igrid,2) - wft(igrid,1) *&
                     &(hlev(igrid,2) - hlev(igrid,1)) /&
                     &(wft(igrid,1) + wft(igrid,2))
                  endif
               enddo
!
               neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000

            else if (ie .eq. 3) then
!
!                 Increase in Area
!
               do 82 j = 1, mortim(1)
                  buf(j) = sumda(mortim(j+1))
82             continue

               neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),&
               &uindex, usrord, buf   )
!
               if (neferr .ne. 0) goto 1000
!
            elseif (ie .eq. 4) then
!
!                 Increase of bed level
!
               do j = 1, mortim(1)
                  buf(j) = min (hlev(mortim(j+1),1),&
                  &hlev(mortim(j+1),2)) -&
                  &hlev0(mortim(j+1))
               enddo
!
               neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),&
               &uindex, usrord, buf  )
!
               if (neferr .ne. 0) goto 1000
!
            elseif (ie .eq. 5) then
!
!                 S_int for selected grid points
!
               do 90 j = 1, mortim(1)
!
!                    Find branch number of this gridpoint
!
                  call mofdbr ( nbran, branch, mortim(j+1), ibr )
!
!                    Test for first or last point
!
                  if (mortim(j+1) .eq. branch(3,ibr)) then
                     buf(j) = dissed(1,ibr)
                  else if (mortim(j+1) .eq. branch(4,ibr)) then
                     buf(j) = dissed(3,ibr)
                  else
                     buf(j) = sedtr(mortim(j+1),1)
                  endif
!
90             continue
!
               neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),&
               &uindex, usrord, buf  )
!
               if (neferr .ne. 0) goto 1000
!
            elseif (ie .ge. 6) then
!
!                 Cross sections for selected grid points
!
               k = mortim(i+1) + 1
               do 100 j = 1, mortim(1)
!
!                    Find branch number of this gridpoint
!
                  call mofdbr ( nbran, branch, mortim(j+1), ibr )
!
                  buf(j) = hlev(mortim(j+1),k)
100            continue
!
!                 Create name for this cross section level
!
               ie1 = min(ie,nentri)
               ie2 = ie-nentri
               if (ie2 .gt. 0) then
                  if (ie2 .gt. 9) then
                     write(txt,'(i2)') ie2
                  else
                     write(txt,'(i1,a)') ie2, ' '
                  endif
                  l    = index (nameel(nentri),' ') - 2
                  name = nameel(nentri)(1:l)//txt
               else
                  name = nameel(ie1)
               endif
!
               neferr = putrel ( fd_nefis_res, grnamh, name,&
               &uindex, usrord, buf  )
!

               if (neferr .ne. 0) goto 1000
            endif
110      continue
      endif
   endif
!
!     Be sure that at the last time step the number of cells has been
!     written correctly
!
   if (istep .ge. nstep) then
      nrerr = emoeoo
      llog  = .true.
!
      call resdes ( 'MORP', fd_nefis_res, grnamd, 3, llog ,&
      &mortim, ncelm , ncelh , neferr )
      if (neferr .ne. 0) goto 1000
!
      neferr = flsdat (fd_nefis_res)
      if (neferr .ne. 0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write(txt2,'(i8)') neferr
   call error (juer, 'MOMOR @'//txt2//'@', nrerr, ker)
!
end
