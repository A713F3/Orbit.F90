module simulation
use :: sdl2
use :: planet_mod
use :: circle_mod
implicit none

    type(planet), allocatable, target :: planets(:) 

contains

    subroutine init_simulation()
        allocate(planets(0))
    end subroutine init_simulation

    subroutine add_planet(x, y)
        real, intent(in) :: x, y  
        type(planet), allocatable :: temp(:)
        type(planet) :: new_planet

        new_planet%object = sdl_circle(pos=vector2d(x=x, y=y), r=20)
        new_planet%mass = 50.0

        allocate(temp(size(planets) + 1))

        temp(1:size(planets)) = planets(1:size(planets))

        temp(size(planets) + 1) = new_planet

        call move_alloc(temp, planets)

    end subroutine add_planet

    subroutine simulate(EULER_COEFF)
        real, intent(in) :: EULER_COEFF
        integer :: i, j
        type(planet), pointer :: planet1, planet2

        do i = 1, size(planets)
            do j = 1, size(planets)
                if (i .eq. j) cycle

                planet1 => planets(i)
                planet2 => planets(j)

                call planet1%attract(planet2, EULER_COEFF)

                ! check out of bounds

            end do
        end do

        do i = 1, size(planets)
            call planets(i)%move(EULER_COEFF)
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