!detnst
      subroutine detnst (x     , y     , icom  , xbnd  , ybnd  ,
     *                   mcbsp , ncbsp , mcnes , ncnes , weight,
     *                   mmax  , nmax  , mc    , nc    , maxbnd,
     *                   nobnd , spher                         )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  
!  
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : detnst
! version            : v1.0
! date               : June 1997
! programmer         : Theo van der kaaij
!
! function           : determines coordinates nest stations and
!                      belonging weight factors
! error messages     :
! subroutines called : findnm, nearmn
!
!***********************************************************************
!
      integer       icom  ( mmax  , * ),
     *              mcbsp ( nobnd , 2     ), ncbsp ( nobnd , 2     )

      integer       mcnes ( maxbnd, 2     , 4     )       ,
     *              ncnes ( maxbnd, 2     , 4     )

      real          x     ( mmax  , nmax  )       ,
     *              y     ( mmax  , nmax  )       ,
     *              xbnd  ( maxbnd, 2     )       ,
     *              ybnd  ( maxbnd, 2     )       ,
     *              weight( maxbnd, 2     , 4     )
!
      logical       inside ,spher
!
      do 10 ibnd = 1, nobnd
!
         do 10 isize = 1, 2
!
            xbsp = xbnd  (ibnd, isize)
            ybsp = ybnd  (ibnd, isize)
!
!-----------find surrounding depth points overall model
!
            call findmn (xbsp  , ybsp  , x     , y     , mmax  , nmax  ,
     *                   mc    , nc    , mnst  , nnst  , rmnst , rnnst ,
     *                   inside, spher                                 )
     )

            if (inside) then
!
!-----------from depth points to zeta points
!
               rmnst = rmnst + 0.5
               rnnst = rnnst + 0.5

               if (rmnst .gt. 1.) then
                  mnst  = mnst + 1
                  rmnst = rmnst - 1.0
               endif

               if (rnnst .gt. 1.) then
                  nnst  = nnst + 1
                  rnnst = rnnst - 1.0
               endif

!
!--------------fill mcnes and ncnes and compute weights
!
               mcnes (ibnd,isize,1) = mnst
               ncnes (ibnd,isize,1) = nnst
               weight(ibnd,isize,1) = (1.- rmnst)*(1. - rnnst)

               mcnes (ibnd,isize,2) = mcnes (ibnd,isize,1) + 1
               ncnes (ibnd,isize,2) = ncnes (ibnd,isize,1)
               weight(ibnd,isize,2) = rmnst*(1. - rnnst)

               mcnes (ibnd,isize,3) = mcnes (ibnd,isize,1)
               ncnes (ibnd,isize,3) = ncnes (ibnd,isize,1) + 1
               weight(ibnd,isize,3) = (1.- rmnst)*rnnst

               mcnes (ibnd,isize,4) = mcnes (ibnd,isize,1) + 1
               ncnes (ibnd,isize,4) = ncnes (ibnd,isize,1) + 1
               weight(ibnd,isize,4) = rmnst*rnnst
            endif
!
!--------------not inside active computational grid point
!
!
!              call nearmn (xbsp  , ybsp  , x     , y     ,
!    *                      mmax  , nmax  , mc    , nc    ,
!    *                      mnst  , nnst  , icom          )
!
!              mcnes (ibnd,isize,1) = mnst
!              ncnes (ibnd,isize,1) = nnst
!              weight(ibnd,isize,1) = 1.0
!
!           endif
   10 continue
!
!-----delete inactive points from mcnes and ncnes arrays
!
      do 20 ibnd = 1, nobnd
         do 20 isize = 1, 2
            noin = 0
            do 30 inst = 1, 4

               mnst = mcnes(ibnd,isize,inst)
               nnst = ncnes(ibnd,isize,inst)

               if (mnst /= 0) then
                  if (icom(mnst,nnst) /= 1) then
                     noin = noin + 1
                     mcnes (ibnd,isize,inst) = 0
                     ncnes (ibnd,isize,inst) = 0
                     weight(ibnd,isize,inst) = 0.
                  endif
               else
                  noin = noin + 1
               endif
   30       continue
            if (noin .eq. 4) then
!
!--------------no active surrounding overall model points found
!              search nearest active point (not for diagonal vel bnd.)
!
               xbsp = xbnd  (ibnd, isize)
               ybsp = ybnd  (ibnd, isize)

               if (xbsp .lt. 1.0e19) then
                  write (*,'('' *** Warning :   No surrounding '',
     *                       ''overall model point for support '',
     *                       ''point ('',i5,'','',i5,'')'')')
     *                       mcbsp (ibnd,isize), ncbsp (ibnd,isize)
                  write (*,'(''                 Nearest active point'',
     *                       '' used'')')


                  call nearmn (xbsp  , ybsp  , x     , y     ,
     *                         mmax  , nmax  , mc    , nc    ,
     *                         mnst  , nnst  , icom          )

                  mcnes (ibnd,isize,1) = mnst
                  ncnes (ibnd,isize,1) = nnst
               endif
               weight(ibnd,isize,1) = 1.0
            endif
   20 continue
!
!-----finally normalize weights
!
      do 50 ibnd = 1, nobnd
         do 50 isize = 1, 2
            wtot = 0.
            do 60 inst = 1, 4
               if (weight(ibnd,isize,inst) .lt. 0.) then
                  weight (ibnd,isize,inst) = 1.0e-6
                endif
                wtot = wtot + weight(ibnd,isize,inst)
   60       continue

            do 70 inst = 1, 4
               weight (ibnd,isize,inst) = weight (ibnd,isize,inst)/
     *                                    wtot
   70       continue
   50 continue
!
!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------
!
      return
      end
