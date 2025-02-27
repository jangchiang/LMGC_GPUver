!===========================================================================
!
! Copyright 2000-2024 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program 
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use, 
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info". 
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability. 
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or 
! data to be ensured and,  more generally, to use and operate it in the 
! same conditions as regards security. 
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================
MODULE wrap_PT2DL

  USE ISO_C_BINDING

  USE PT2DL,ONLY: &
       read_bodies_PT2DL, &
       set_precon_node_PT2DL, &
       compute_convection_matrix_PT2DL, &
       compute_convection_RHS_PT2DL, &
       add_convection2KT_PT2DL, &
       add_convection2RHS_PT2DL, &
 !! > B.o.B.o.R >
       get_nb_PT2TL, &
       set_hconv_PT2TL, &
       set_temp_PT2TL, &
       get_body_PT2TL, &
       get_nb_PT2DL, &
       get_nb_PT2TL, &
       set_Tconv_PT2TL, &
       clean_memory_PT2DL


CONTAINS

    SUBROUTINE LoadTactors() bind(c, name='PT2DL_LoadTactors')
      IMPLICIT NONE
       !! PURPOSE
       !!  read MODELS.DAT file
       !!  Initializes existing_entities variable for PT2DL contactors

       CALL read_bodies_PT2DL

    END SUBROUTINE

    SUBROUTINE PushPreconNodes() bind(c, name='PT2DL_PushPreconNodes')
      IMPLICIT NONE
       !! PURPOSE
       !!  Initializes existing_entities variable for PT2DL contactors

       CALL set_precon_node_PT2DL

    END SUBROUTINE 

    function GetNbPT2DL() bind(c, name='PT2DL_GetNbPT2DL')
      implicit none 
      integer(c_int) :: GetNbPT2DL

       GetNBPT2DL = get_nb_PT2DL()

    end function

!   function GetNbPT2TL() bind(c, name='PT2DL_GetNbPT2TL')
!     implicit none 
!    integer(c_int) :: GetNbPT2TL
!
!     GetNBPT2TL = get_nb_PT2TL()
!
! end function

!    subroutine SetHConv(itacty,value) bind(c, name='PT2DL_SetHConv')
!      implicit none
!      integer(c_int), intent(in), value :: itacty
!      real(c_double), intent(in), value :: value
!
!      call set_hconv_PT2TL(itacty, value)
!
!    end subroutine

!    subroutine SetTConv(itacty,value) bind(c, name='PT2DL_SetTConv')
!      implicit none
!      integer(c_int), intent(in), value :: itacty
!      real(c_double), intent(in), value :: value
!
!      call set_Tconv_PT2TL(itacty, value)
!
!    end subroutine

    SUBROUTINE ComputeConvectiveFlux() bind(c, name='PT2DL_ComputeConvectiveFlux')
      IMPLICIT NONE

       CALL compute_convection_matrix_PT2DL
       CALL compute_convection_RHS_PT2DL

    END SUBROUTINE

    SUBROUTINE AssembThermKT() bind(c, name='PT2DL_AssembThermKT')
      IMPLICIT NONE

       CALL add_convection2KT_PT2DL

    END SUBROUTINE

    SUBROUTINE AssembThermRHS() bind(c, name='PT2DL_AssembThermRHS')
      IMPLICIT NONE

       CALL add_convection2RHS_PT2DL

    END SUBROUTINE


 !! > B.o.B.o.R >
     function  GetNbPT2TL(ibdyty) bind(c,name='PT2DL_GetNbPT2TL')
      IMPLICIT NONE
      integer(c_int),value :: ibdyty
      integer(c_int)::GetNbPT2TL
      GetNbPT2TL = get_nb_PT2TL(ibdyty)
     END FUNCTION GetNbPT2TL
     function  GetBodyPT2TL(itacty) bind(c,name='PT2DL_GetBody')
      IMPLICIT NONE
      integer(c_int),value :: itacty
      integer(c_int)::GetBodyPT2TL
      GetBodyPT2TL = get_body_PT2TL(itacty)
     END FUNCTION GetBodyPT2TL
     SUBROUTINE SetHconv(itacty,hconv) bind(c,name='PT2TL_SetHconv')
       IMPLICIT NONE
       integer(c_int),value :: itacty
       real(c_double),value :: hconv
       call set_hconv_PT2TL(itacty,hconv)
     END SUBROUTINE SetHconv
     SUBROUTINE SetTconv(itacty,tconv) bind(c,name='PT2TL_SetTconv')
      IMPLICIT NONE
       integer(c_int),value :: itacty
       real(c_double),value :: tconv
       call set_temp_PT2TL(itacty,tconv)
     END SUBROUTINE SetTconv
!! < B.o.B.o.R <

    subroutine CleanMemory() bind(c, name='PT2DL_CleanMemory')
      implicit none
  
      call clean_memory_PT2DL

    end subroutine


END MODULE wrap_PT2DL
