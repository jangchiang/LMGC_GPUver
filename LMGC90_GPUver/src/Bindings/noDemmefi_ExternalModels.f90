!===========================================================================
!
! Copyright 2000-2023 CNRS-UM.
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

module DemmefiExternalModels

  use utilities, only : logmes, faterr

  implicit none

  private

  public push_model, push_behaviour, check_ppset
  public set_nb_ppsets, clean_memory
  public compute_external_gp


contains

! wrapper
!------------------------------------------------------------------------
  subroutine push_model(imodel,itchatche)
    implicit none
    integer :: imodel
    logical :: itchatche

                              !123456789012345678901234567890123456
    character(len=36)  :: IAM='noDemmefi_ExternalModels::push_model'

    if (itchatche) call logmes('Entering : '//IAM)
    call faterr(iam,'Error: no Demmefi external models available')
  
  end subroutine push_model
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine set_nb_ppsets(nb,itchatche)
    implicit none
    integer :: nb
    logical :: itchatche
                              !123456789012345678901234567890123456789
    character(len=39)  :: IAM='noDemmefi_ExternalModels::set_nb_ppsets'

    if (itchatche) call logmes('Entering : '//IAM)
    call faterr(iam,'Error: no Demmefi external models available')

  end subroutine set_nb_ppsets
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine push_behaviour(ppsnb,itchatche,rho,filename)
    implicit none
    integer           :: ppsnb
    logical           :: itchatche
    real(kind=8)      :: rho
    character(len=80) :: filename

                              !1234567890123456789012345678901234567890
    character(len=40)  :: IAM='noDemmefi_ExternalModels::push_behaviour'

    if (itchatche) call logmes('Entering : '//IAM)
    call faterr(iam,'Error: no Demmefi external models available')

  end subroutine push_behaviour
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine check_ppset(ppsnb,imodel,itchatche)
    implicit none
    integer :: ppsnb,imodel
    logical :: itchatche
                              !1234567890123456789012345678901234567
    character(len=37)  :: IAM='noDemmefi_ExternalModels::check_ppset'

    if (itchatche) call logmes('Entering : '//IAM)
    call faterr(iam,'Error: no Demmefi external models available')

  end subroutine
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine compute_external_gp(ppsnb,imodel,extP_lbl,extP_len,ivalue, &
                                 extP_val,extP_nb, &
                                 GRAD0,FLUX0,INTERNAL0, &
                                 GRAD1,FLUX1,INTERNAL1, &
                                 D,H,calcD,xe3d)
    implicit none
    ! zone de stockage: gradient,flux,internal,operateur tangent
    real(kind=8),dimension(:)             :: GRAD0,FLUX0,INTERNAL0
    real(kind=8),dimension(:)             :: GRAD1,FLUX1,INTERNAL1
    real(kind=8),dimension(:,:),pointer   :: D
    real(kind=8)                          :: H

    ! parametres externes
    character(len=30),dimension(:) :: extP_lbl
    integer(kind=4)  ,dimension(:) :: extP_len
    real(kind=8)     ,dimension(:) :: extP_val
    integer(kind=4)                :: extP_nb, calcD

    !fd
    integer                        :: ppsnb,imodel,ibehav,ivalue,ierr
    real(kind=8),dimension(:,:)    :: xe3d

                              !123456789012345678901234567890123456789012345
    character(len=45)  :: IAM='noDemmefi_ExternalModels::compute_external_gp'

    call faterr(iam,'Error: no Demmefi external models available')

  end subroutine compute_external_gp
!------------------------------------------------------------------------

!------------------------------------------------------------------------
  subroutine clean_memory
    implicit none

                              !12345678901234567890123456789012345678
    character(len=38)  :: IAM='noDemmefi_ExternalModels::clean_memory'

    !nothing to clean here...
    !call faterr(iam,'Error: no Demmefi external models available')

  end subroutine clean_memory
!------------------------------------------------------------------------

end module DemmefiExternalModels
