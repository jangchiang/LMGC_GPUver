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
MODULE wrap_PRPRx

  USE ISO_C_BINDING

  USE overall, only: &
       io_last_Vloc_Rloc, &
       io_out_Vloc_Rloc

  USE PRPRx,ONLY:&
       point_surf_PRPRx     ,&
       line_surf_PRPRx      ,&
       surf_surf_PRPRx      ,&
       coor_prediction_PRPRx,&
       CHECK_PRPRx,&
       RUN_PRPRx, &
       get_write_Vloc_Rloc_PRPRx, &
       read_ini_Vloc_Rloc_PRPRx,&
       write_xxx_Vloc_Rloc_PRPRx,&
       compute_box_PRPRx, &
       creation_tab_visu_PRPRx, &
       !fd compute_contact_PRPRx, &
       display_prox_tactors_PRPRx,&
       get_nb_PRPRx, &
       set_cundall_iteration_PRPRx, &
       set_cundall_neighbor_PRPRx, &
       set_clipper_parameters, &
       use_old_ccpm_PRPRx, &
       set_shrink_polyr_faces_PRPRx, &
       set_size_factor_polyr_PRPRx, &
       !fd obso compute_explicit_contact_PRPRx, &
       creation_tab_visu_to_file_PRPRx, &
       creation_tab_visu_from_file_PRPRx, &
       wcp_compute_contact_PRPRx,&
       !fd wed_compute_contact_PRPRx,&
       wti_compute_contact_PRPRx,&
       set_xperiodic_data_PRPRx, &
       set_yperiodic_data_PRPRx, &
       set_f2f_tol_PRPRx, &
       set_f2f_tol_small_surface_PRPRx, &
       set_max_nb_pt_select_PRPRx, &
       nc_compute_contact_PRPRx, &
       f2f4all_compute_contact_PRPRx, &
       verbose_f2f_PRPRx, &
       get_f2f2inters_PRPRx, &
       Set_Reaction_Tracking_Length_PRPRx, &
       set_tol_recup_rloc_PRPRx, &
       get_interaction_vector_PRPRx, &
       get_interaction_internal_PRPRx, &
       set_interaction_internal_PRPRx, &
       get_interaction_internal_comment_PRPRx, &
       with_nodal_contact_PRPRx, &
       clean_memory_PRPRx

  use utilities, only : faterr, logmes

  LOGICAL :: is_first_time = .TRUE.
  LOGICAL :: save_PRPRx_to_file=.FALSE.,load_PRPRx_from_file=.FALSE.

  integer :: Detection_method = 0
  real(kind=8) :: global_alert_distance

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(C, name='PRPRx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between POLYR tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       INTEGER :: RESTART = 0
       logical :: is_initialized = .FALSE.
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       if( .not. check_PRPRx() ) return

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PRPRx] select      ')
       call start_itimer(timer_id)

       if (.not. is_initialized) then 
          call compute_box_PRPRx
          is_initialized = .TRUE.
       endif

       CALL coor_prediction_PRPRx
       
       IF (RUN_PRPRx()) then
         IF (load_PRPRx_from_file .and. RESTART == 0) THEN
           CALL creation_tab_visu_from_file_PRPRx
           RESTART=1
         ELSE
           CALL creation_tab_visu_PRPRx
         END IF

         IF (save_PRPRx_to_file) CALL creation_tab_visu_to_file_PRPRx
       END IF

       select case(detection_method)
       case(0)
          call FATERR('PRPRx_SelectProxTactors','You must specify a detection method')
       case(1)
         call FATERR('PRPRx_SelectProxTactors','Obsolete contact detection method')
         !CALL compute_contact_PRPRx
       case(2)
         CALL wcp_compute_contact_PRPRx
       case(3)
         CALL nc_compute_contact_PRPRx(global_alert_distance)
       case(4)
         CALL f2f4all_compute_contact_PRPRx(global_alert_distance)
       case(5)
         call FATERR('PRPRx_SelectProxTactors','External detection not implemented')
         !CALL wed_compute_contact_PRPRx
       case(6)
         call wti_compute_contact_PRPRx
       case default
          call FATERR('PRPRx_SelectProxTactors','Detection method unknown')
       end select

       call stop_itimer(timer_id)

  END SUBROUTINE SelectProxTactors

  !> common plane iterative method 
  SUBROUTINE Use_Cp_Cundall_Detection(iter,cd_shrink,an_shrink,delta) bind(C, name='PRPRx_UseCpCundallDetection')
    implicit none
    !> max iteration number
    integer(c_int), intent(in), value :: iter
    !> clipper shrinking value on candidate when doing face/face detection
    real(c_double), intent(in), value :: cd_shrink
    !> clipper shrinking value on antagonist when doing face/face detection
    real(c_double), intent(in), value :: an_shrink
    !> clipper simplification value of face/face intersection polygon of clipper
    real(c_double), intent(in), value :: delta
    integer :: it

    detection_method = 2
    it=iter
    
    CALL set_cundall_iteration_PRPRx(it)
    call set_clipper_parameters(cd_shrink, an_shrink, delta)

    ! in case of big polyr nc detection is used instead of cundall
    call with_nodal_contact_PRPRx()
    

  END SUBROUTINE

  !> common plane combinatory method 
  SUBROUTINE Use_Cp_f2f_Explicit_Detection(tol,cd_shrink,an_shrink,delta) bind(C, name='PRPRx_UseCpF2fExplicitDetection')
    implicit none
    !> tolerance on normals orientation
    real(c_double), intent(in), value :: tol
    !> clipper shrinking value on candidate when doing face/face detection
    real(c_double), intent(in), value :: cd_shrink
    !> clipper shrinking value on antagonist when doing face/face detection
    real(c_double), intent(in), value :: an_shrink
    !> clipper simplification value of face/face intersection polygon of clipper
    real(c_double), intent(in), value :: delta

    detection_method = 2

    call set_f2f_tol_PRPRx(tol)
    call set_clipper_parameters(cd_shrink, an_shrink, delta)

  END SUBROUTINE Use_Cp_f2f_Explicit_Detection

  !> compute contact point method
  SUBROUTINE Cp_Use_old_ccpm() bind(C, name='PRPRx_CpUseOldCcpm')
    implicit none

    call use_old_ccpm_PRPRx()

  END SUBROUTINE

  !> set a minimum surface size for f2f detection
  SUBROUTINE Set_f2f_tol_small_surface(tol) bind(C, name='PRPRx_SetF2fMinimalSurfaceSize')
    implicit none
    !> tolerance on normals orientation
    real(c_double), intent(in), value :: tol

    call set_f2f_tol_small_surface_PRPRx(tol)

  END SUBROUTINE


  !> common plane combinatory method 
  SUBROUTINE Use_Cp_f2f_Detection(tol,iter,cd_shrink,an_shrink,delta) bind(C, name='PRPRx_UseCpF2fDetection')
    implicit none
    !> tolerance on normals orientation
    real(c_double), intent(in), value :: tol
    !> max iteration number
    integer(c_int), intent(in), value :: iter
    !> clipper shrinking value on candidate when doing face/face detection
    real(c_double), intent(in), value :: cd_shrink
    !> clipper shrinking value on antagonist when doing face/face detection
    real(c_double), intent(in), value :: an_shrink
    !> clipper simplification value of face/face intersection polygon of clipper
    real(c_double), intent(in), value :: delta
    !
    integer :: it

    detection_method = 2

    call set_f2f_tol_PRPRx(tol)
    it=iter
    CALL set_cundall_iteration_PRPRx(it)
    call set_clipper_parameters(cd_shrink, an_shrink, delta)


  END SUBROUTINE

  SUBROUTINE Use_Nc_Detection(gdist) bind(C, name='PRPRx_UseNcDetection')
    implicit none
    real(c_double), intent(in), value :: gdist

    detection_method = 3
    global_alert_distance = gdist

  END SUBROUTINE

  SUBROUTINE Use_Nc_f2f_Detection(gdist,tol) bind(C, name='PRPRx_UseNcF2fDetection')
    implicit none
    !> global alert distance
    real(c_double), intent(in), value :: gdist
    !> tolerance on normals orientation
    real(c_double), intent(in), value :: tol

    detection_method = 3
    global_alert_distance = gdist

    call set_f2f_tol_PRPRx(tol)

  END SUBROUTINE

  SUBROUTINE Use_Nc_f2f_Explicit_Detection(gdist,tol) bind(C, name='PRPRx_UseNcF2fExplicitDetection')
    implicit none
    !> global alert distance
    real(c_double), intent(in), value :: gdist
    !> tolerance on normals orientation
    real(c_double), intent(in), value :: tol

    detection_method = 4
    global_alert_distance = gdist

    call set_f2f_tol_PRPRx(tol)

  END SUBROUTINE

  SUBROUTINE Use_External_Detection() bind(C, name='PRPRx_UseExternalDetection')
    implicit none

    detection_method = 5

  END SUBROUTINE

  SUBROUTINE Use_Triangles_Intersection_Detection(nb_max_pt) bind(C, name='PRPRx_UseTrianglesIntersectionDetection')
    implicit none
    !> maximum number of test points
    integer(c_int), intent(in), value :: nb_max_pt

    detection_method = 6
    call set_max_nb_pt_select_PRPRx(nb_max_pt)

  END SUBROUTINE Use_Triangles_Intersection_Detection

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() bind(c, name='PRPRx_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       IMPLICIT NONE

       if( .not. check_PRPRx() ) return

       CALL write_xxx_Vloc_Rloc_PRPRx(2)

  END SUBROUTINE WriteLastVlocRloc
   
!!!----------------------------------------------------

  SUBROUTINE WriteOutVlocRloc() bind(C, name = 'PRPRx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       if( .not. check_PRPRx() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_PRPRx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_PRPRx(1)

  END SUBROUTINE  

  SUBROUTINE DisplayOutVlocRloc() bind(C, name='PRPRx_DisplayOutVlocRloc')
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PRPRx contacts
       IMPLICIT NONE

       if( .not. check_PRPRx() ) return

       CALL write_xxx_Vloc_Rloc_PRPRx(6)

  END SUBROUTINE DisplayOutVlocRloc

  SUBROUTINE DisplayProxTactors() bind(C, name='PRPRx_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       if( .not. check_PRPRx() ) return

       CALL display_prox_tactors_PRPRx

  END SUBROUTINE DisplayProxTactors


  subroutine ReadIniVlocRloc(step) bind(c, name='PRPRx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_PRPRx() ) return

     call read_ini_Vloc_Rloc_PRPRx(step)

  end subroutine

  SUBROUTINE ShrinkPolyrFaces(shrink) bind(C, name='PRPRx_ShrinkPolyrFaces')
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: shrink

       ! pas utile if( .not. check_PRPRx() ) return

       ! rm: tested in set_shrink_polyr_faces
       !IF (SHRINK.LT.0.D0 .OR. SHRINK .GT. 1.D0) THEN
       !   PRINT *, 'You should specify a shrink factor in [0,1]'
       !   STOP
       !END IF

       CALL set_shrink_polyr_faces_PRPRx(SHRINK)

  END SUBROUTINE ShrinkPolyrFaces

  SUBROUTINE LowSizeArrayPolyr(sfactor) bind(C, name='PRPRx_LowSizeArrayPolyr')
       IMPLICIT NONE
       integer(C_int), INTENT(IN), VALUE :: sfactor

       !pas utile if( .not. check_PRPRx() ) return

       CALL set_size_factor_polyr_PRPRx(sfactor)

  END SUBROUTINE LowSizeArrayPolyr

  SUBROUTINE SaveProxTactorsToFile() bind(C, name='PRPRx_SaveProxTactorsToFile')
       IMPLICIT NONE

       if( .not. check_PRPRx() ) return

       save_PRPRx_to_file=.TRUE.

  END SUBROUTINE SaveProxTactorsToFile
    
  SUBROUTINE LoadProxTactorsFromFile() bind(C, name='PRPRx_LoadProxTactorsFromFile')
       IMPLICIT NONE

       if( .not. check_PRPRx() ) return

       load_PRPRx_from_file=.TRUE.

  END SUBROUTINE LoadProxTactorsFromFile

  SUBROUTINE SetXPeriodicCondition(xperiode) bind(C, name='PRPRx_SetXPeriodicCondition')
       !! PURPOSE
       !!  initialize data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: xperiode

       !fd dbg
       !if( .not. check_PRPRx() ) return

       CALL set_xperiodic_data_PRPRx(xperiode,.TRUE.)

  END SUBROUTINE SetXPeriodicCondition

  SUBROUTINE SetYPeriodicCondition(yperiode) bind(C, name='PRPRx_SetYPeriodicCondition')
       !! PURPOSE
       !!  initialize data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: yperiode

       !fd dbg debile pour un set de parametres
       !if( .not. check_PRPRx() ) return

       CALL set_yperiodic_data_PRPRx(yperiode,.TRUE.)

  END SUBROUTINE SetYPeriodicCondition
!!!---------------------------------------------------------------------

  subroutine VerboseF2F(cd,an) bind(C, name='PRPRx_VerboseF2F')
    implicit none
    integer(c_int), intent(in), value :: cd,an

    call verbose_f2f_PRPRx(cd,an)

  end subroutine

  subroutine getF2f2Inters(ivect, ivalue) bind(C, name='PRPRx_GetF2f2Inters')
    implicit none
    integer(c_int)                      :: ivalue
    type(c_ptr)                         :: ivect
    !
    integer(c_int), dimension(:), pointer :: vector

    vector => null()

    call get_f2f2inters_PRPRx( vector )

    ivalue = size(vector)
    ivect = c_loc(vector(1))

  end subroutine getF2f2Inters

  SUBROUTINE SetCundallNeighbor(neighbor) bind(C, name='PRPRx_SetCundallNeighbor')
       IMPLICIT NONE
       real(C_double), INTENT(IN), VALUE :: neighbor

       call set_cundall_neighbor_PRPRx(neighbor)

  END SUBROUTINE SetCundallNeighbor

  subroutine SetReactionTrackingLength(length) bind(C, name='PRPRx_SetReactionTrackingLength')
     implicit none
     real(c_double), intent(in), value :: length

     call set_reaction_tracking_length_PRPRx(length)  

  end subroutine SetReactionTrackingLength

  subroutine SetTolRecupRloc(tol) bind(c, name='PRPRx_SetTolRecupRloc')
    implicit none
    real(c_double), intent(in), value :: tol

    call set_tol_recup_rloc_PRPRx(tol)
  end subroutine

  subroutine GetInteractionVector(cvalue1_c,ivalue1,rvect,ivalue2) bind(C, name='PRPRx_GetInteractionVector')
    implicit none
    character(c_char), dimension(5),intent(in) :: cvalue1_c
    integer(c_int),intent(in), value :: ivalue1
    integer(c_int)                   :: ivalue2
    type(c_ptr)                      :: rvect
    !
    real(c_double), dimension(:), pointer :: vector
    character(len=5) :: cvalue1
    integer :: i

    cvalue1 = ''
    do i=1,5
       cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do

    ivalue2 = 3
    allocate(vector(ivalue2))
    call get_interaction_vector_PRPRx(cvalue1,ivalue1,vector,ivalue2)

    rvect = c_loc(vector(1))

  end subroutine

  function GetInteractionInternal(ivalue1,ivalue2) bind(C, name='PRPRx_GetInteractionInternal')
    implicit none
    INTEGER(c_int),INTENT(in), value :: ivalue1,ivalue2
    REAL(c_double)                   :: GetInteractionInternal

    real(kind=8) :: rvalue


    call get_interaction_internal_PRPRx(ivalue1,ivalue2,rvalue)

    GetInteractionInternal= rvalue

  end function

  subroutine SetInteractionInternal(ivalue1,ivalue2,rvalue) bind(C, name='PRPRx_SetInteractionInternal')
    implicit none
    INTEGER(c_int),INTENT(in), value :: ivalue1,ivalue2
    REAL(c_double),INTENT(in), value :: rvalue
    !

    call set_interaction_internal_PRPRx(ivalue1,ivalue2,rvalue)

  end subroutine

  subroutine GetInteractionInternalComment(ivalue, string_out, string_size, real_size) bind(C, name='PRPRx_GetInteractionInternalComment')
    implicit none
    integer(c_int), intent(in), value :: ivalue
    type(c_ptr) :: string_out
    integer(c_int), intent(out) :: string_size, real_size
    !
    character(len=100), pointer :: cvect

    allocate(cvect)
    call get_interaction_internal_comment_PRPRx(ivalue,cvect)    

    string_size = len(trim(cvect))
    real_size   = len(cvect)
    string_out  = c_loc(cvect(1:1))

  end subroutine

  !> 
  SUBROUTINE WithNodalContact() bind(C, name='PRPRx_WithNodalContact')
    implicit none

    CALL With_Nodal_Contact_PRPRx()

  END SUBROUTINE

  subroutine SetInternalSurface(ivalue, rvalue) bind(C, name='PRPRx_SetInternalSurface')
    implicit none
    integer(c_int), intent(in), value :: ivalue
    real(c_double), intent(in), value :: rvalue

    if( ivalue == 1 ) then
      point_surf_PRPRx = rvalue
    else if( ivalue == 2 ) then
      line_surf_PRPRx = rvalue
    else if( ivalue == 3 ) then
      surf_surf_PRPRx = rvalue
    else
      call faterr( 'PRPRx_SetInternalSurface', 'integer parameter must 1 for point, 2 for line or 3 for surface')
    end if
  end subroutine SetInternalSurface


  subroutine CleanMemory() bind(c, name='PRPRx_CleanMemory')
    implicit none

    call clean_memory_PRPRx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_PRPRx
