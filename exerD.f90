program exerD

    implicit none
    
    real(4) :: l = 5.4565
    integer :: rows, FIXED_DIMENSION = 2, circleCount, squareCount
    real(4), allocatable :: pontos(:, :)


    read(*,*) rows

    allocate(pontos(rows,FIXED_DIMENSION))

    call random_number(pontos)

    pontos = pontos*(l + 0.5)
    

    circleCount = isInCircle(pontos, l)
    squareCount = isInSquare(pontos, l)


    ! write(*,'(F4.3)') (real(circleCount)/real(squareCount))*4

    ! write(*,*) (real(circleCount)/real(squareCount))*4

    print'(F0.4)', (real(circleCount)/real(squareCount))*4


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
