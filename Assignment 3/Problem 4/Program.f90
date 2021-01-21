program Program
    
    implicit none

    real :: T, W, Lc, Lp, d, T_min, d_optimal
    W = 200.0
    Lc = 3.0
    Lp = 3.0
    d = 0.5
    T_min = huge(0.0)
    d_optimal = 0.5
    open(unit=2 , file="Output.txt")
        write (2, *) "Distance (m)  Tension (N)"
        write (2, *)

        do while ( d <= 2.8)
            T = ( W * Lc * Lp) / ( d * sqrt ((Lp ** 2) - (d ** 2)))
            write (2, 3) d, T
            3 format ( f5.1, f20.5)
            if( T_min >= T) then
                T_min = T
                d_optimal = d
            end if
            d = d + 0.1
        end do
        write (2, *)
        write (2, 4) "The minimum tension is", T_min, "N at", d_optimal, "metres from wall."
        4 format (a22, f10.5, a5, f5.1, a18)
    close ( unit = 2 ) !Closing the file after printing in it

end program Program