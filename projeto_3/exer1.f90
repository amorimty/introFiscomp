program exer1

    implicit none
    real(16) :: m = 80.0_16, P = 400.0_16, tzao, delta_t, v_curr
    integer :: i = 0

    read(*,*) tzao
    read(*,*) delta_t
    read(*,*) v_curr

    open(1, file='vel1_out.dat', status='replace')

    write(1, *)  i*delta_t, v_curr

    do i = 1, ceiling(tzao/delta_t), 1

        v_curr = v_curr + delta_t*P/(m*v_curr)

        ! write(1, *)  i*delta_t, v_curr
        write(1,'(F12.6,1X,E26.16)') i*delta_t, v_curr

    end do
    
    ! write(*,*) 1 + integer(tzao/delta_t)
    close(1)
    

end program exer1
