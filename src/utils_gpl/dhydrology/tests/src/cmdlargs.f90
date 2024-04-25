!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------

!============================================================================
! retrieving flags or parameters from the command-line:
! argint, argreal, arglogical
! R. Leander
!============================================================================
module m_cmdlargs
implicit none

private

public  ::  argint
public  ::  argreal
public  ::  argstring
public  ::  arglogical

    contains

        function argint(prefix, default) result (i)
        implicit none
        integer         :: i
        integer         :: lastchar
        integer         :: jarg
        integer         :: iarg
        integer         :: default, result
        character*(*)   :: prefix
        character*50    :: sarg

        i=default
        do jarg=1,iarg()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
              read(sarg,*,end=233,err=233) i
           endif
 233       continue
        enddo
        end function argint

        function argreal(prefix, default) result (r)
        implicit none
        real            :: r
        integer         :: jarg
        integer         :: iarg
        integer         :: lastchar
        real            :: default
        character*(*)   :: prefix
        character*50    :: sarg

        r=default
        do jarg=1,iarg()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
              read(sarg,*,end=323,err=323) r
           endif
 323       continue
        enddo
        end function argreal

        function arglogical(prefix) result (l)
!       returns .True. if the prefix is found in ARGV[]
        implicit none
        logical         :: l
        integer         :: jarg
        integer         :: iarg
        character*(*)   :: prefix
        character*50    :: sarg

        l=.False.
        jarg=1
        do jarg=1,iarg()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) l=.True.
        enddo
        end function arglogical

        function argstring(prefix, default, s) result (success)
        implicit none
        logical         :: success
        integer         :: jarg
        integer         :: iarg
        character*(*)   :: default
        character*(*)   :: s            !MAX 20 characters!
        character*(*)   :: prefix
        character*50    :: sarg

        s=default
        success=.False.
        do jarg=1,iarg()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
!             read(sarg,*,end=233,err=233) s
              s=sarg                                       !2012-02-19
              success=.True.
           endif
 233       continue
        enddo
        end function argstring

end module m_cmdlargs