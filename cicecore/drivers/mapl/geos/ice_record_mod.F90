
      module ice_record_mod

      use ice_kinds_mod
      use ice_constants,   only: field_loc_center, field_type_scalar, c0
      use ice_domain_size, only: max_blocks, ncat
      use ice_communicate, only: my_task, master_task
      use ice_blocks,      only: nx_block, ny_block
      use ice_state,       only: aicen, vicen, vsnon, trcrn
      use ice_exit,        only: abort_ice
      use ice_fileunits,   only: nu_diag
      use icepack_intfc,   only: icepack_query_tracer_sizes
      use icepack_intfc,   only: icepack_warnings_flush, icepack_warnings_aborted

      implicit none
      private
      public :: alloc_record_state, save_record_state, restore_record_state

      real (kind=dbl_kind), &
         dimension (:,:,:,:), allocatable :: &
         aicen_save , & ! concentration of ice
         vicen_save , & ! volume per unit area of ice          (m)
         vsnon_save     ! volume per unit area of snow         (m)


      real (kind=dbl_kind), &
         dimension (:,:,:,:,:), allocatable :: &
         trcrn_save     ! tracers
                   ! 1: surface temperature of ice/snow (C)

      contains 

      subroutine alloc_record_state
        integer (int_kind) :: ntrcr, ierr
        character(len=*),parameter :: subname='(alloc_record_state)'

        call icepack_query_tracer_sizes(ntrcr_out=ntrcr)
        call icepack_warnings_flush(nu_diag)
        if (icepack_warnings_aborted()) call abort_ice(error_message=subname, &
          file=__FILE__, line=__LINE__)

        allocate ( aicen_save(nx_block,ny_block,ncat,max_blocks) , & ! concentration of ice
                   vicen_save(nx_block,ny_block,ncat,max_blocks) , & ! volume per unit area of ice (m)
                   vsnon_save(nx_block,ny_block,ncat,max_blocks) , & ! volume per unit area of snow (m)
                   trcrn_save(nx_block,ny_block,ntrcr,ncat,max_blocks) , & ! tracers: 1: surface temperature of ice/snow (C)
                stat=ierr)
        if (ierr/=0) call abort_ice('(alloc_record_state): Out of memory1')


        trcrn_save = c0

      end subroutine alloc_record_state

      subroutine save_record_state

         character(len=*),parameter :: subname='(save_record_state)'

         if (.not. allocated(trcrn_save)) &
            call abort_ice(error_message=subname//': trcrn_save not allocated', &
            file=__FILE__, line=__LINE__)

            
         aicen_save(:,:,:,:) = aicen(:,:,:,:)
         vicen_save(:,:,:,:) = vicen(:,:,:,:)
         vsnon_save(:,:,:,:) = vsnon(:,:,:,:)
         trcrn_save(:,:,:,:,:) = trcrn(:,:,:,:,:)

         if(my_task == master_task) then
            write(*,*), 'CICE6 thermo state saved'
         endif 

      end subroutine save_record_state

      subroutine restore_record_state

         character(len=*),parameter :: subname='(restore_record_state)'
         if (.not. allocated(trcrn_save)) &
            call abort_ice(error_message=subname//': trcrn_save not allocated', &
            file=__FILE__, line=__LINE__)

         aicen(:,:,:,:) = aicen_save(:,:,:,:)
         vicen(:,:,:,:) = vicen_save(:,:,:,:)
         vsnon(:,:,:,:) = vsnon_save(:,:,:,:)
         trcrn(:,:,:,:,:) = trcrn_save(:,:,:,:,:)

         if(my_task == master_task) then
            write(*,*), 'CICE6 thermo state restored'
         endif 

      end subroutine restore_record_state

      end module ice_record_mod
