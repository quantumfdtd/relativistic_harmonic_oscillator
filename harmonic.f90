module harmonic

use,intrinsic :: ISO_Fortran_env, only: REAL64

save

   public :: new_tables, remove_tables, add_values, &
           set_mu_eps, get_table, set_prec_goal, harmon

   private :: table_d, table_c, table_binom, &
           mu, eps, prec_goal, table_n, max_n

   ! table_d[i] = d_{2i}
   ! table_c[i] = c_{2i+1}
   real(kind=REAL64), dimension(:), allocatable :: table_c, table_d, table_binom

   real(kind=REAL64) :: mu, eps, prec_goal
   integer :: table_n, max_n

contains

   subroutine set_mu_eps(imu, ieps)
        real(kind=REAL64), intent(in) :: imu, ieps

        mu = imu
        eps = ieps
   end subroutine

   subroutine set_prec_goal(iprec_goal)
        real(kind=REAL64), intent(in) :: iprec_goal

        prec_goal = iprec_goal
   end subroutine

   subroutine harmon(p_array, ret_array)
       real(kind=REAL64), dimension(:), intent(in) :: p_array
       real(kind=REAL64), dimension(:), intent(out) :: ret_array

       real(kind=REAL64) :: p, ret, term, pot, fact
       integer :: n, i

       if (size(p_array) /= size(ret_array)) stop &
               "*** ERROR: ON harmon(p, ret), p AND ret MUST HAVE COMPATIBLE DIMENSIONS ***"

       do i=1, size(ret_array)
          p=p_array(i)

          n=0
          pot=p
          fact=1.d0
          term=table_c(0)*pot/fact
          ret=term

          do while (abs(term) > prec_goal)
             n=n+1
             pot=pot*p*p
             fact=fact*(2*n+1)*(2*n)
             term=table_c(n)*pot/fact
             ret=ret+term
          end do

          ret_array(i)=ret
       end do
   end subroutine

   subroutine get_table()
        integer :: i

        do i=0, table_n
            print *, i, " ---> d(", 2*i ,")=", table_d(i), &
            "---> c(", 2*i+1, ")=", table_c(i)
        end do

        print *, NEW_LINE('A')
        print *, "BINOM."
        print *, "------"
        print *, NEW_LINE('A')

        do i=0, max_n*2+1
           print *, "C[", max_n, ",", i, "]=", table_binom(i)
        end do
   end subroutine

   subroutine new_tables(imax_n)
        integer, intent(in) :: imax_n
        integer :: ierr

        max_n = imax_n
        if (max_n .lt. 5) then
            stop "*** Error: module(harmonic), subroutine(new_tables): minimum value of max_n is 3 ***"
        end if

        if (allocated(table_d)) then
                deallocate(table_d)
        end if

        if (allocated(table_c)) then
                deallocate(table_c)
        end if

        if (allocated(table_binom)) then
                deallocate(table_binom)
        end if

        allocate(table_d(0:max_n), STAT=ierr)
        if (ierr/=0) stop "*** OUT OF MEMORY ***"

        allocate(table_c(0:max_n), STAT=ierr)
        if (ierr/=0) stop "*** OUT OF MEMORY ***"

        allocate(table_binom(0:max_n*2+1), STAT=ierr)
        if (ierr/=0) stop "*** OUT OF MEMORY ***"

        table_n = 0

        !c_1 = (mu-eps)
        !c_3 = (mu-eps)
        table_c(0) = 1.d0
        table_c(1) = (mu-eps)

        !d_{2*0} = 0
        !d_{2*1} = 1
        table_d(0) = 0.d0
        table_d(1) = 1.d0

        ! binom(1,i)
        table_binom(0)=1.d0
        table_binom(1)=1.d0
        table_binom(2)=0.d0

   end subroutine new_tables

   subroutine remove_tables()
        deallocate(table_d)
        deallocate(table_c)
        deallocate(table_binom)
   end subroutine remove_tables

   subroutine add_binom()
        implicit none
        real(kind=REAL64) :: tmp1, tmp2
        integer :: k

        k = 1
        tmp1 = 1
        tmp2 = 1

        do while (tmp1 .gt. .1d0)
           tmp1 = table_binom(k)
           table_binom(k) = tmp2 + table_binom(k)
           k = k+1

           tmp2 = tmp1
        end do

        table_binom(k)=0
   end subroutine add_binom

   subroutine add_values(imax_n)
        integer, intent(in) :: imax_n
        real(kind=REAL64) :: mu_min_eps, tmp
        integer :: k, j

        if (imax_n .ge. max_n) stop "*** ERROR: TOO LOW VALUE FOR max_n ***"

        do k = table_n+1, imax_n-1
           table_d(k+1) = (1-4*k*k)*table_d(k)
        end do

        mu_min_eps = mu-eps

        do k = table_n+1, imax_n-1
           table_c(k+1) = mu_min_eps*table_c(k)
           tmp = mu
           call add_binom()
           call add_binom()
           do j = 1, k
             tmp = tmp/(mu*mu)
             table_c(k+1) = table_c(k+1) + &
                     tmp*table_binom(2*j)*table_d(j)*table_c(k-j)
           end do
        end do

        table_n = imax_n

   end subroutine add_values

end module harmonic
