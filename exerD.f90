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
        ! if ( pontos(i,1) <= l .and. pontos(i,1) >= -l .and. pontos(i,2) <= l .and. pontos(i,2) >= -l) then
        !     count2 = count2 + 1
        ! end if
    end do

    ! circleCount = isInCircle(pontos, l)
    ! squareCount = isInSquare(pontos, l)


    ! write(*,'(F4.3)') (real(circleCount)/real(squareCount))*4

    ! write(*,*) (real(circleCount)/real(squareCount))*4

    ! print'(F0.4)', (real(circleCount)/real(squareCount))*4

    write(*,*) count1, count2
    print '(F0.4)', (real(count1)/real(count2))*4




contains
    integer function isInCircle(ptos, r)

        real(4), intent(in), dimension(:, :) :: ptos
        real(4), intent(in) :: r
        integer :: i, count = 0

        do i = 1, size(ptos)/2
            if ( ((ptos(i,1))**2 + (ptos(i,2))**2) <= r**2 ) then
                count = count + 1
            end if
        end do

        isInCircle = count

    end function isInCircle

    integer function isInSquare(ptos, l)

        real(4), intent(in), dimension(:, :) :: ptos
        real(4), intent(in) :: l
        integer :: i, j, count = 0

        do i = 1, size(ptos)/2
            if ( ptos(i,1) < l .and. ptos(i,1) > -l .and. ptos(i,2) < l .and. ptos(i,2) > -l) then
                count = count + 1
            end if
        end do

        isInSquare = count

    end function isInSquare


end program exerD
