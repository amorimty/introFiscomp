program exer2

    implicit none
    real(8) :: angulo, v0, vx, vy, delta_t, g, x, y
    real(8) :: x_prev, y_prev, vy_prev, frac
    integer :: i

    v0 = 500.0_8
    g = 9.8_8
    delta_t = 0.1_8
    x = 0.0_8
    y = 0.0_8
    i = 0

    read(*,*) angulo
    vx = v0*cosd(angulo)
    vy = v0*sind(angulo)
    
    open(1, file='output2.txt', status='replace')

    write(1,'(E26.16,1X,E26.16)') x, y  


    do
        x_prev = x
        y_prev = y
        vy_prev = vy

        x = x + vx*delta_t
        y = y + vy*delta_t
        vy = vy - g*delta_t

        if (y < 0.0_8) exit  
        write(1,'(E26.16,1X,E26.16)') x, y
    end do

    frac = y_prev / (y_prev - y)
    x = x_prev + frac*(x - x_prev)
    y = 0.0_8
    write(1,'(E26.16,1X,E26.16)') x, y

    close(1)

end program exer2
