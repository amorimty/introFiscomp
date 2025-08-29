program exerB

    implicit none

    write(*,*) expansaoTaylorErro(0.1_8)

contains
    real(8) function expansaoTaylorErro(a)
        implicit none
        real(8), intent(in) :: a
        integer :: i
        real(8) :: S = 0.0_8, c = 0.0_8, term, y, temp, eps 

        eps = epsilon(1.0_8)


        ! implementando a soma de kahan para reduzir a propagação de erro na série
        do i = 1, 100, 1
            term = ((-1.0_8)**(i+1))*(a**i)/real(i, 8)

            if (abs(term) <= eps*max(1.0_8, abs(S))) exit

            y = term - c 
            temp = S + y
            c = (temp - S) - y

            
            S = temp
            
        end do

        expansaoTaylorErro = term/S

    end function expansaoTaylorErro

end program exerB

