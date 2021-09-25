program hello
   use,intrinsic :: ISO_Fortran_env, only: REAL64
   use harmonic

   implicit none

   integer, parameter :: max_n = 200
   integer, parameter :: imax_n = 50

   real(kind=REAL64), parameter :: mu = 30.d0
   real(kind=REAL64), parameter :: eps = 30.d0+0.89864
   real(kind=REAL64), parameter :: prec_goal = 1.d-3

   character(100) :: out_file
   integer, parameter :: out_unit=20

   real(kind=REAL64) :: min_p, max_p
   real(kind=REAL64), dimension(:), allocatable :: input, output
   integer :: point_number, ierr, i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Check binom, table_d and table_c
!   call set_mu_eps(mu, eps)
!
!   call new_tables(max_n)
!   call add_values(imax_n)
!
!   call get_table()
!
!   call remove_tables()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Get a list
   print *, " ===> MAIN PROGRAM: OBTAINING Y(P) LIST <=== "
   print *, "Output file: "
   read (*,*) out_file

   open(unit=out_unit, file=out_file, action="write", status="replace", IOSTAT=ierr)
   if (ierr/=0) stop "*** CANNOT CREATE FILE*** "

   print *, "min(p): "
   read (*,*) min_p

   print *, "max(p): "
   read (*,*) max_p

   print *, "Number of points: "
   read (*,*) point_number

   allocate(input(point_number), STAT=ierr)
   if (ierr/=0) stop "*** OUT OF MEMORY*** "

   allocate(output(point_number), STAT=ierr)
   if (ierr/=0) stop "*** OUT OF MEMORY*** "

   do i=1, point_number
      input(i) = min_p + max_p*i/point_number
   end do


   call set_prec_goal(prec_goal)
   call set_mu_eps(mu, eps)

   call new_tables(max_n)
   call add_values(imax_n)

   call harmon(input, output)
   call remove_tables()

   do i=1, point_number
      write(out_unit,*) input(i), output(i)
   end do

   close(out_unit)

   deallocate(input)
   deallocate(output)
end
