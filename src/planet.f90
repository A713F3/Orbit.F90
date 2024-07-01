! planet.f90
!
! Planet definitions.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

module planet_mod
use :: sdl2
use :: circle_mod
implicit none

    type :: planet
        real :: mass
        type(sdl_circle) :: object
    contains
        procedure :: render
        procedure :: attract
        procedure :: colliding
    end type

    real :: G = 10.0

contains

    function render(self, renderer) result(rc)
        class(planet), intent(in) :: self
        type(c_ptr), intent(out)  :: renderer

        integer :: rc

        rc = self%object%render_fill(renderer)
    end function

    function attract(self, other_planet) result(force)
        class(planet), intent(in) :: self
        class(planet), intent(in) :: other_planet
        real :: force, r
        real :: x1, x2, y1, y2

        x1 = self%object%x
        y1 = self%object%y

        x2 = other_planet%object%x
        y2 = other_planet%object%y

        r = sqrt( (x1 - x2)**2 +  (y1 - y2)**2 )

        force = G * (self%mass * other_planet%mass) / (r*r)

    end function attract

    function colliding(self, other_planet) result(is_collided)
        class(planet), intent(in)  :: self
        class(planet), intent(in)  :: other_planet
        logical                    :: is_collided

        real :: r1, r2, x1, x2, y1, y2, distance

        x1 = self%object%x
        y1 = self%object%y
        r1 = self%object%r

        x2 = other_planet%object%x
        y2 = other_planet%object%y
        r2 = other_planet%object%r

        distance = sqrt( (x1 - x2)**2 +  (y1 - y2)**2 )

        is_collided = distance .le. (r1 + r2)

    end function colliding


end module planet_mod