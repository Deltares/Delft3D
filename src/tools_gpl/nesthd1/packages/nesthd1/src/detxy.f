      subroutine detxy  (mcbsp , ncbsp , x     , y     , icom  ,
     *                   xbnd  , ybnd  , mmax  , nmax  , maxbnd,
     *                   nobnd , bndtype                          )
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
! subroutine         : detxy
! version            : v1.4
! date               : January 1998
! programmer         : Theo van der Kaaij
!
! function           : Determine x and y (world) coordinates of boundary
!                      support points
!                      For velocity boundaries (bndtype = 'UV') simply
!                      average between depth points
!                      For water level boundaries (bndtype = 'WL') mirror
!                      the first active water level point
!                      For diagonal water level boundaries average
!                      between depth points
! limitations        :
! subroutines called :
!***********************************************************************
!
      integer       icom  ( mmax  , *  )       ,
     *              mcbsp ( nobnd , 2     )       ,
     *              ncbsp ( nobnd , 2     )
!
      real          xbnd  ( maxbnd, 2     )       ,
     *              ybnd  ( maxbnd, 2     )       ,
     *              x     ( mmax  , nmax  )       ,
     *              y     ( mmax  , nmax  )
!
      character *2  bndtype
!
      logical       left  , lower

!-----------------------------------------------------------------------
!---- 1. determine X,Y co-ordinates of boundary support points
!        in WL and UV points
!-----------------------------------------------------------------------
!
      do 20 ibnd = 1, nobnd
         write (*,'(''Boundary section: '',i5,1x,a)') ibnd, bndtype    
!
         ma = mcbsp (ibnd,1)
         na = ncbsp (ibnd,1)
         mb = mcbsp (ibnd,2)
         nb = ncbsp (ibnd,2)
!
!---- 1.1 determine the orientation of the boundary section
!
!....... boundary section along a vertical grid line
!
         if( ma == mb .and. na /= nb )then
!
!.......... check left or right side active grid
            left  = .true.
            m     = ma
            mstep = 1
            if( ma .gt. 1 )then
               n = (na + nb) / 2
               if( icom(ma-1,n) == 1 )then
                  left  = .false.
                  m     = ma - 1
                  mstep = -1
               endif
            endif
!
!.......... then determine x and y coordinates boundary support points
!
            do 30 iside = 1, 2
               n    = ncbsp(ibnd,iside)
               xmid = 0.5 * ( x(m,n) + x(m,n-1) )
               ymid = 0.5 * ( y(m,n) + y(m,n-1) )
               dx   = 0.5 * ( x(m,n  ) - x(m+mstep,n  )
     *                      + x(m,n-1) - x(m+mstep,n-1) )
               dy   = 0.5 * ( y(m,n  ) - y(m+mstep,n  )
     *                      + y(m,n-1) - y(m+mstep,n-1) )
               if (bndtype == 'UV') then
                  xbnd   (ibnd,iside) = xmid
                  ybnd   (ibnd,iside) = ymid
               elseif (bndtype == 'WL') then
                  xbnd (ibnd,iside) = xmid + 0.5 * dx
                  ybnd (ibnd,iside) = ymid + 0.5 * dy
               endif
   30       continue
!
         endif
!
!....... same story however this time a boundary section
!        along a horizontal grid line
!
         if( na == nb .and. ma /= mb )then
!
!.......... check lower or upper side active grid
            lower = .true.
            n     = na
            nstep = 1
            if( na .gt. 1 )then
               m = (ma + mb) / 2
               if( icom(m,na-1) == 1 )then
                  lower = .false.
                  n     = na - 1
                  nstep = -1
               endif
            endif
!
!.......... then determine x and y coordinates boundary support points
!
            do 40 iside = 1, 2
               m    = mcbsp(ibnd,iside)
               xmid = 0.5 * ( x(m,n) + x(m-1,n) )
               ymid = 0.5 * ( y(m,n) + y(m-1,n) )
               dx   = 0.5 * ( x(m  ,n) - x(m  ,n+nstep)
     *                      + x(m-1,n) - x(m-1,n+nstep) )
               dy   = 0.5 * ( y(m  ,n) - y(m  ,n+nstep)
     *                      + y(m-1,n) - y(m-1,n+nstep) )
               if (bndtype == 'UV') then
                  xbnd   (ibnd,iside) = xmid
                  ybnd   (ibnd,iside) = ymid
               elseif (bndtype == 'WL') then
                  xbnd   (ibnd,iside) = xmid + 0.5 * dx
                  ybnd   (ibnd,iside) = ymid + 0.5 * dy
               endif
   40       continue
         endif
!
!....... same story however this time a boundary section
!        along a diagonal grid line (only allowed for water
!        level boundaries
!
         if (na /= nb .and. ma /= mb) then
            if (bndtype == 'WL') then

               if (abs(na - nb) /= abs (ma - mb)) then
                  write (*,'('' *** Error : Water level '',
     *                       ''boundary not diagonal'')')
                  stop
               endif

               do 50 isize = 1, 2
                  m = mcbsp (ibnd,isize)
                  n = ncbsp (ibnd,isize)
                  if (icom (min(m+1,mmax),n              ) == 1 .and.
     *                icom (m              ,max(n-1,1   )) == 1 ) then
                     xbnd (ibnd,isize) = 0.5*(x (m  ,n  ) + x (m-1,n-1))
                     ybnd (ibnd,isize) = 0.5*(y (m  ,n  ) + y (m-1,n-1))
                  elseif (icom (max(m-1,1), n           ) == 1 .and.
     *                    icom (m           , max(n-1,1)) == 1 ) then
                     xbnd (ibnd,isize) = 0.5*(x (m-1,n  ) + x (m  ,n-1))
                     ybnd (ibnd,isize) = 0.5*(y (m-1,n  ) + y (m  ,n-1))
                  elseif (icom (m         , min(n+1,nmax)) == 1 .and.
     *                    icom (max(m-1,1), n            ) == 1 ) then
                     xbnd (ibnd,isize) = 0.5*(x (m  ,n  ) + x (m-1,n-1))
                     ybnd (ibnd,isize) = 0.5*(y (m  ,n  ) + y (m-1,n-1))
                  elseif (icom (min(m+1,mmax), n            ).eq.1.and.
     *                    icom (m            , min(n+1,nmax)).eq.1) then
                     xbnd (ibnd,isize) = 0.5*(x (m  ,n-1) + x (m-1,n  ))
                     ybnd (ibnd,isize) = 0.5*(y (m  ,n-1) + y (m-1,n  ))
                  elseif (icom (m         ,max(n-1,1   )) /= 1 .and.
     *                    icom (m         ,min(n+1,nmax)) /= 1 .and.
     *                    icom (max(m-1,1),n            ) /= 1) then
!
!-----------------left point diagonal boundary
!
                     xmid = 0.5*(x(m  ,n  ) + x(m  ,n-1))
                     ymid = 0.5*(y(m  ,n  ) + y(m  ,n-1))
                     mstep = 1
                     dx   = 0.5 * ( x(m,n  ) - x(m+mstep,n  )
     *                            + x(m,n-1) - x(m+mstep,n-1) )
                     dy   = 0.5 * ( y(m,n  ) - y(m+mstep,n  )
     *                            + y(m,n-1) - y(m+mstep,n-1) )
                     xbnd (ibnd,isize) = xmid + 0.5 * dx
                     ybnd (ibnd,isize) = ymid + 0.5 * dy
                  elseif (icom (m            ,max(n-1,1   )) /= 1 .and.
     *                    icom (m            ,min(n+1,nmax)) /= 1 .and.
     *                    icom (min(m+1,mmax),n            ) /= 1) then
!
!-----------------right point diagonal boundary
!
                     m = m - 1
                     mstep = -1
                     xmid = 0.5*(x(m  ,n  ) + x(m  ,n-1))
                     ymid = 0.5*(y(m  ,n  ) + y(m  ,n-1))
                     dx   = 0.5 * ( x(m,n  ) - x(m+mstep,n  )
     *                            + x(m,n-1) - x(m+mstep,n-1) )
                     dy   = 0.5 * ( y(m,n  ) - y(m+mstep,n  )
     *                            + y(m,n-1) - y(m+mstep,n-1) )
                     xbnd (ibnd,isize) = xmid + 0.5 * dx
                     ybnd (ibnd,isize) = ymid + 0.5 * dy
                  elseif (icom (max(m-1,1)   ,n            ) /= 1.and.
     *                    icom (min(m+1,mmax),n            ) /= 1.and.
     *                    icom (m            ,max(n-1,1   )) /= 1)then
!
!-----------------lower point diagonal boundary
!
                     nstep = 1

                     xmid = 0.5 * ( x(m,n) + x(m-1,n) )
                     ymid = 0.5 * ( y(m,n) + y(m-1,n) )
                     dx   = 0.5 * ( x(m  ,n) - x(m  ,n+nstep)
     *                            + x(m-1,n) - x(m-1,n+nstep) )
                     dy   = 0.5 * ( y(m  ,n) - y(m  ,n+nstep)
     *                            + y(m-1,n) - y(m-1,n+nstep) )
                     xbnd (ibnd,isize) = xmid + 0.5 * dx
                     ybnd (ibnd,isize) = ymid + 0.5 * dy
                  elseif (icom (max(m-1,1)   ,n            ) /= 1.and.
     *                    icom (min(m+1,mmax),n            ) /= 1.and.
     *                    icom (m            ,min(n+1,nmax)) /= 1)then
!
!-----------------upper point diagonal boundary
!
                     n = n - 1
                     nstep = -1

                     xmid = 0.5 * ( x(m,n) + x(m-1,n) )
                     ymid = 0.5 * ( y(m,n) + y(m-1,n) )
                     dx   = 0.5 * ( x(m  ,n) - x(m  ,n+nstep)
     *                            + x(m-1,n) - x(m-1,n+nstep) )
                     dy   = 0.5 * ( y(m  ,n) - y(m  ,n+nstep)
     *                            + y(m-1,n) - y(m-1,n+nstep) )
                     xbnd (ibnd,isize) = xmid + 0.5 * dx
                     ybnd (ibnd,isize) = ymid + 0.5 * dy
                  endif
   50          continue
            else
!
!--------------set default value in case of diagonal velocity boundaries
!
               do 60 isize = 1, 2
                  xbnd (ibnd,isize) = 1.0e20
                  ybnd (ibnd,isize) = 1.0e20
   60          continue
            endif
         endif
         if (na == nb .and. ma == mb) then
            write (*,'(''    No support for this boundary section, '')')
            write (*,'(''    because begin and end point are te same'',
     *                    4(1x,i6))') ma, mb, na, nb     
         endif
   20 continue

!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------

      return

      end
