! planet.f90
!
! Planet definitions.
!
! Author:  Ali Efe Karagul
! GitHub:  https://github.com/A713F3/Orbit.F90

module planet_mod
use :: sdl2
use :: circle_mod
use :: vector2d_mod
implicit none

    type :: planet
        real :: mass
        type(sdl_circle) :: object
        type(vector2d) :: velo
    contains
        procedure :: get_pos
        procedure :: get_velo
        procedure :: get_r
        procedure :: render
        procedure :: move
        procedure :: attract
        procedure :: colliding
    end type

    real :: G = 10.0

contains

    function get_pos(self) result(pos)
        class(planet), target, intent(in) :: self
        type(vector2d), pointer           :: pos
    
        pos => self%object%pos
    end function get_pos

    function get_velo(self) result(velo)
        class(planet), target, intent(in) :: self
        type(vector2d), pointer           :: velo
    
        velo => self%velo
    end function get_velo

    function get_r(self) result(r)
        class(planet), intent(in) :: self
        real                      :: r
    
        r = self%object%r
    end function get_r

    function render(self, renderer) result(rc)
        class(planet), intent(in) :: self
        type(c_ptr), intent(out)  :: renderer

        integer :: rc

        rc = self%object%render_fill(renderer)
    end function

    subroutine move(self, EULER_COEFF)
        class(planet), intent(in) :: self
        real, intent(in)          :: EULER_COEFF

        type(vector2d), pointer :: pos, velo
        type(vector2d) :: tmp_velo

        pos => self%get_pos()
        velo => self%get_velo()

        tmp_velo = velo

        call velo%mul(EULER_COEFF)
        call pos%add(velo)

        velo = tmp_velo

    end subroutine

    subroutine attract(self, other_planet, EULER_COEFF)
        class(planet), intent(in) :: self
        class(planet), intent(in) :: other_planet
        real, intent(in)          :: EULER_COEFF

        type(vector2d), pointer :: pos1, pos2, velo1, velo2
        real :: force, r, cosa, sina, fx, fy

        pos1 => self%get_pos()
        pos2 => other_planet%get_pos()

        velo1 => self%get_velo()
        velo2 => other_planet%get_velo()

        r = pos1%distance(pos2)

        force = EULER_COEFF * G * (self%mass * other_planet%mass) / (r*r)

        cosa = (pos1%x - pos2%x) / r
        sina = (pos1%y - pos2%y) / r

        fx = cosa * force
        fy = sina * force

        velo2%x = velo2%x + fx
        velo2%y = velo2%y + fy

    end subroutine attract

    function colliding(self, other_planet) result(is_collided)
        class(planet), intent(in)  :: self
        class(planet), intent(in)  :: other_planet
        logical                    :: is_collided

        type(vector2d), pointer :: pos1, pos2
        real :: r

        pos1 => self%get_pos()
        pos2 => other_planet%get_pos()

        r = pos1%distance(pos2)

        is_collided = r .le. (self%get_r() + other_planet%get_r())

    end function colliding


end module planet_mod