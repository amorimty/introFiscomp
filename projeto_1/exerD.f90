program exerD

    implicit none
    
    real(4) :: l = 2.244
    integer :: i, rows, FIXED_DIMENSION = 2, circleCount, squareCount, count1 = 0, count2 = 0
    real(4), allocatable :: pontos(:, :)


    read(*,*) rows

    allocate(pontos(rows,FIXED_DIMENSION))

    call random_number(pontos)

    pontos = pontos*l - l/2
    
    count2 = rows
    do i = 1, size(pontos)/2

        if ( ((pontos(i,1))**2 + (pontos(i,2))**2) <= (l/2)**2 ) then
            count1 = count1 + 1
        end if

    end do

    print '(F0.4)', (real(count1)/real(count2))*4


end program exerD
