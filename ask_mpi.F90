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

   use mpi
#if defined(__INTEL_COMPILER)
   use ifport, only: getpid, getcwd!, hostnm
#endif /* __INTEL_COMPILER */
   ! use iso_fortran_env, only: output_unit

   implicit none

   character(len = MPI_MAX_LIBRARY_VERSION_STRING) :: version_string
   character(len = MPI_MAX_PROCESSOR_NAME) :: myname
   integer(kind = MPI_ADDRESS_KIND) :: tag_ub
   integer(kind = MPI_INTEGER_KIND) :: ierr, version, subversion, provided, resultlen, comm_size, my_rank, mynamelen, &
        &                              pid_proc, cwd_status !, host_status
   integer, parameter :: buflen = 64, cwdlen = 512
   character(len = buflen) :: buf, sbuf
   integer :: i
   logical :: flag
   character(len = cwdlen) :: cwd_proc
   ! character(len = buflen) :: host_proc
#ifdef MPI41
   type(MPI_Info) :: hw_info
#endif

   call MPI_Init(ierr)

   call MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

   if (my_rank == 0) then
      write(buf, *) comm_size
      write(*,*)'Program ask_mpi started on ' // trim(adjustl(buf)) // ' ranks.'
   end if

   pid_proc    = getpid()
   cwd_status  = getcwd(cwd_proc)
   call MPI_Get_processor_name(myname, mynamelen, ierr)
   !host_status = hostnm(host_proc)  ! hostnm returns the same thing as MPI_Get_processor_name

   do i = 0, comm_size
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      if (my_rank == i) then
         write(*,*)'MPI rank: ', my_rank, ' hostname: "' // trim(myname) // '", PID = ', pid_proc, &
              &    ' CWD = "' // trim(cwd_proc) // '"'
         ! call sleep(1)
         ! call flush(output_unit)
      end if
   end do
   call MPI_Barrier(MPI_COMM_WORLD, ierr)

   if (my_rank == 0) then
      call MPI_Get_version(version, subversion, ierr)
      write(buf, *) version
      write(sbuf, *) subversion
      write(*,*)'MPI version:          ' // trim(adjustl(buf)) // '.' // trim(adjustl(sbuf))

      call MPI_Query_thread(provided, ierr)
      write(buf, *) provided
      write(*,*)'MPI_Query_thread:     ' // trim(adjustl(buf))

      call MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_TAG_UB, tag_ub, flag, ierr)
      if (flag) then
         write(buf, *) tag_ub
         write(*,*)'Upper bound for tags: ' // trim(adjustl(buf))
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
         write(*,*)'Error: unknown attribute'
      end if

      call printinfo(MPI_INFO_ENV, "MPI_INFO_ENV")

      call MPI_Get_library_version(version_string, resultlen, ierr)
      write(*,*)'MPI build:            ' // trim(version_string)

   end if

   call MPI_Finalize(ierr)

contains

   subroutine printinfo(mpi_info_object, descr)

      implicit none

      integer(kind = MPI_INTEGER_KIND), intent(in) :: mpi_info_object
      character(len = *), intent(in) :: descr

      character(len = MPI_MAX_INFO_VAL) :: key, value
      integer(kind = MPI_INTEGER_KIND) :: nkeys

      call MPI_Info_get_nkeys(mpi_info_object, nkeys, ierr)
      if (nkeys > 0) then
         write(*,*)"Read ", nkeys, " keys from " // trim(descr)
         do i = 0, nkeys - 1
            call MPI_Info_get_nthkey(mpi_info_object, i, key, ierr)
            call MPI_Info_get(mpi_info_object, key, MPI_MAX_INFO_VAL, value, flag, ierr)
            if (flag) write(*,*)"  Key: " // trim(key) // " = " // trim(value)
         end do
      else
         write(*,*)"No keys were read from " // trim(descr)
      end if

   end subroutine printinfo

end program ask_mpi
