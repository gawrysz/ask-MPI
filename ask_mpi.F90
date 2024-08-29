! This little program tells the MPI library version and may provide some additional info.
!
! To compile and run, do:
!
!     mpif90 ask_mpi.F90 && ./a.out && rm a.out
!
! Use `mpif90 -fc=ifx` for compiling with Intel OneAPI.
!
! Try to add -D MPI41 for things available in newest MPI standard.

program ask_mpi

   use mpi_f08
#if defined(__INTEL_COMPILER)
   use ifport, only: getpid, getcwd!, hostnm
#endif /* __INTEL_COMPILER */
   ! use iso_fortran_env, only: output_unit

   implicit none

   character(len = MPI_MAX_LIBRARY_VERSION_STRING) :: version_string
   character(len = MPI_MAX_PROCESSOR_NAME) :: myname
   integer(kind = MPI_ADDRESS_KIND) :: tag_ub
   integer(kind = MPI_INTEGER_KIND) :: ierr, version, subversion, provided, resultlen, comm_size, my_rank, mynamelen, &
        &                              pid_proc, cwd_status, slen !, host_status
   integer, parameter :: buflen = 64, cwdlen = 512
   character(len = buflen) :: buf
   integer :: i
   logical :: flag
   character(len = cwdlen) :: cwd_proc
   ! character(len = buflen) :: host_proc
#ifdef MPI41
   type(MPI_Info) :: hw_info
#endif

   call MPI_Init(ierr)
   if (ierr /= MPI_SUCCESS) then
      call MPI_Error_string(ierr, cwd_proc, slen)
      write(*, '(A)')'MPI_Init failed: ' // cwd_proc
      stop
   endif

   call MPI_Comm_size(MPI_COMM_WORLD, comm_size)
   call MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

   if (my_rank == 0) write(*, '(A,I0,A)') 'Program ask_mpi started on ', comm_size, ' ranks.'

   pid_proc    = getpid()
   cwd_status  = getcwd(cwd_proc)
   call MPI_Get_processor_name(myname, mynamelen)
   !host_status = hostnm(host_proc)  ! hostnm returns the same thing as MPI_Get_processor_name

   do i = 0, comm_size
      call MPI_Barrier(MPI_COMM_WORLD)
      if (my_rank == i) then
         write(*, '(A,I11,A,I0,A)')'MPI rank: ', my_rank, ' hostname = "' // trim(myname) // '", PID = ', pid_proc, &
              &                    ' CWD = "' // trim(cwd_proc) // '"'
         ! call flush(output_unit)
      end if
   end do
   call MPI_Barrier(MPI_COMM_WORLD)
   call sleep(1)

   if (my_rank == 0) then
      call MPI_Get_version(version, subversion)
      write(*, '(A,I0,A,I0)')'MPI version:          ', version, '.', subversion

      call MPI_Query_thread(provided)
      select case (provided)
      case (MPI_THREAD_SINGLE)
         buf = 'Single-threaded'
      case (MPI_THREAD_FUNNELED)
         buf = 'Funneled'
      case (MPI_THREAD_SERIALIZED)
         buf = 'Serialized'
      case (MPI_THREAD_MULTIPLE)
         buf = 'Multiple'
      case default
         buf = 'Unknown'
      end select
      write(*, '(A)')'MPI_Query_thread:     ' // trim(buf)

      call MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_TAG_UB, tag_ub, flag)
      if (flag) then
         write(*, '(A,I0)')'Upper bound for tags: ', tag_ub
         ! This may vary a lot between MPI implementations and even
         ! compilations:
         !
         ! 2**19 - 1 = 524287 :
         !     * Intel(R) MPI 2021.13 (MPI 3.1)
         !
         ! 2**23 - 1 = 8388607 :
         !     * Open MPI v4.0.5 (MPI 3.1)
         !
         ! 2**28 - 1 = 268435455 :
         !     * MPICH 4.0.2 (MPI 4.0)
         !     * MPICH 4.2.2 (MPI 4.1)
         !
         ! 2**29 - 1 = 536870911 :
         !     * MPICH 3.1.4 (MPI 3.0)
         !
         ! 2**31 - 1 = 2147483647 :
         !     * Open MPI v5.0.4, v4.1.4, v2.1.1 (MPI 3.1)
      else
         write(*, '(A)')'Error: unknown attribute'
      end if

      call printinfo(MPI_INFO_ENV, "MPI_INFO_ENV")

#ifdef MPI41
      call MPI_Info_create(hw_info)
      call MPI_Get_hw_resource_info(hw_info)
      call printinfo(hw_info, "MPI_Get_hw_resource_info")
      call MPI_Info_free(hw_info)
#endif

      call MPI_Get_library_version(version_string, resultlen)
      write(*, '(2A)')'MPI build:            ', trim(version_string)

   end if

   call MPI_Finalize()

contains

   subroutine printinfo(mpi_info_object, descr)

      implicit none

      type(MPI_Info), intent(in) :: mpi_info_object
      character(len = *), intent(in) :: descr

      character(len = MPI_MAX_INFO_VAL) :: key, value
      integer(kind = MPI_INTEGER_KIND) :: nkeys
!!$#ifdef MPI41
!!$      integer, parameter :: buflen = MPI_MAX_INFO_VAL
!!$#endif

      call MPI_Info_get_nkeys(mpi_info_object, nkeys)
      if (nkeys > 0) then
         write(*, '(A,I0,A)')"Read ", nkeys, " keys from " // trim(descr)
         do i = 0, nkeys - 1
            call MPI_Info_get_nthkey(mpi_info_object, i, key)

            ! MPI_Info_get is deprecated since MPI 4.0
!!$#ifdef MPI41
!!$            call MPI_Info_get_string(mpi_info_object, key, slen, value, flag)
!!$#else
            call MPI_Info_get(mpi_info_object, key, MPI_MAX_INFO_VAL, value, flag)
!!$#endif
            if (flag) write(*, '(A)')"  Key:" // trim(key) // " = " // trim(value)
         end do
      else
         write(*, '(A)')"No keys were read from " // trim(descr)
      end if

   end subroutine printinfo

end program ask_mpi
