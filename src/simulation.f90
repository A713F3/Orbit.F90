! simulation.f90
!
! Simulation definitions.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

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

    subroutine add_planet(pos, velo)
        type(vector2d), intent(in) :: pos, velo
        type(planet), allocatable :: temp(:)
        type(planet) :: new_planet

        new_planet%object = sdl_circle(pos=pos, r=20)
        new_planet%mass = 10.0
        new_planet%velo = velo

        allocate(temp(size(planets) + 1))

        temp(1:size(planets)) = planets(1:size(planets))

        temp(size(planets) + 1) = new_planet

        call move_alloc(temp, planets)

    end subroutine add_planet

    subroutine simulate(EULER_COEFF)
        real, intent(in) :: EULER_COEFF
        integer :: i, j

        do i = 1, size(planets)
            do j = 1, size(planets)
                if (i .eq. j) cycle

                call planets(i)%attract(planets(j), EULER_COEFF)

                ! check out of bounds

            end do
        end do

        do i = 1, size(planets) - 1
            do j = i + 1, size(planets)
                call planets(i)%collide(planets(j))
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
        end do
    end function

end module simulation