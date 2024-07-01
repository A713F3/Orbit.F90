module simulation
use :: sdl2
use :: planet_mod
use :: circle_mod
implicit none

    type(planet), allocatable :: planets(:) 

contains

    subroutine init_simulation()
        allocate(planets(0))
    end subroutine init_simulation

    subroutine add_planet(x, y)
        real, intent(in) :: x, y  
        type(planet), allocatable :: temp(:)
        type(planet) :: new_planet

        new_planet%object = sdl_circle(x=x, y=y, r=20)
        new_planet%mass = 50.0

        allocate(temp(size(planets) + 1))

        temp(1:size(planets)) = planets(1:size(planets))

        temp(size(planets) + 1) = new_planet

        call move_alloc(temp, planets)

    end subroutine add_planet

    subroutine simulate(EULER_COEFF)
        real, intent(in) :: EULER_COEFF
        integer :: i, j
        real :: f, fx, fy, cosa, sina, r
        type(planet) :: planet1, planet2
        real :: x1, x2, y1, y2

        do i = 1, size(planets)
            do j = 1, size(planets)
                if (i .eq. j) cycle

                planet1 = planets(i)
                planet2 = planets(j)

                x1 = planet1%object%x
                y1 = planet1%object%y

                x2 = planet2%object%x
                y2 = planet2%object%y

                r = sqrt(real( (x1 - x2)**2 +  (y1 - y2)**2 ))

                cosa = (x1 - x2) / r
                sina = (y1 - y2) / r

                f = planet1%attract(planet2) * EULER_COEFF

                fx = cosa * f
                fy = sina * f

                planets(j)%object%x = planets(j)%object%x + fx
                planets(j)%object%y = planets(j)%object%y + fy

                if ( planets(i)%colliding(planets(j)) ) then
                    planets(j)%object%x = planets(j)%object%x - fx
                    planets(j)%object%y = planets(j)%object%y - fy
                end if

                ! check out of bounds

            end do
        end do
    end subroutine

    function render_simulation(renderer) result(rc)
        type(c_ptr), intent(out) :: renderer
        integer :: i, rc

        do i = 1, size(planets)
            rc = planets(i)%render(renderer)

            !write(*,*) "x: ", planets(i)%object%x, "y: ", planets(i)%object%y 
        end do
    end function

end module simulation