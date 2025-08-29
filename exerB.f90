program exerB

    implicit none

    expansaoTaylor(0.1_8)

contains
    real(8) function expansaoTaylor(a)
        implicit none
        real(8), intent(in) :: a
        integer :: i
        real(8) :: S = 0.0_8, c = 0.0_8, term, y, temp, eps 

        eps = epsilon(1.0_8)


        ! implementando a soma de kahan para reduzir a propagação de erro na série
        do i = 1, 100, 1
            term = ((-1.0_8)**(i+1))*(a**i)/real(i, 8)

            y = term - c 
            
            temp = S + y
            c = (temp - S) - y

            if ((temp == S) .or. (abs(term) <= eps)) exit

            S = temp
            
        end do

        write(*,*) S, i
        expansaoTaylor = S

    end function expansaoTaylor

end program exerB

