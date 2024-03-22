!#######################################################################

     module HF_init
     use Hf_Geom,  only: HFcond,block
     use input
     !use Rodtab,     only: nrrod          
implicit none

     type(HFCond    ), allocatable, public, target :: HFrods(:) !没有实例化

    
     public :: material_props,blk_allocate

     type :: rods_type
          integer :: jmax,kmax
          real,allocatable,dimension(:) :: linear_power
          real,allocatable,dimension (:) :: x
          type(surf_type),allocatable,dimension(:) :: surf
     end type

     type :: surf_type
          real,allocatable,dimension(:) :: htcl
     end type

     type(rods_type) :: rods(1)

contains
     subroutine material_props(imat,T,k,cp)
     implicit none
          real,intent(out) :: k,cp
          real,intent(in) :: T
          integer,intent(in) :: imat
          integer::i
          if (T<=material_temp(1) ) then
               i=2
               k=material_kc(i-1) + (material_kc(i)-material_kc(i-1))/(material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
               cp=material_cp(i-1) + (material_cp(i)-material_cp(i-1))/(material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
          elseif (T>=material_temp(size(material_temp)) ) then
               i=10
               k=material_kc(i-1) + (material_kc(i)-material_kc(i-1))/(material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
               cp=material_cp(i-1) + (material_cp(i)-material_cp(i-1))/(material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
          else 
               do i=2,size(material_temp)
                    if((T<material_temp(i)).and.(T>=material_temp(i-1))) then
                         k=material_kc(i-1) + (material_kc(i)-material_kc(i-1))/ &
                         (material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
                         cp=material_cp(i-1) + (material_cp(i)-material_cp(i-1))/ &
                         (material_temp(i)-material_temp(i-1))*(T-material_temp(i-1))
                    end if
               enddo
          end if
          return
     end subroutine

     subroutine blk_allocate(blk,Nx,Ny)
     implicit none
          type(block),intent(in out):: blk
          integer,intent(in):: Nx,Ny
          allocate(blk%wn_x(Nx,Ny),blk%wn_y(Nx,Ny),blk%ws_x(Nx,Ny),blk%ws_y(Nx,Ny))
          allocate(blk%en_x(Nx,Ny),blk%en_y(Nx,Ny),blk%es_x(Nx,Ny),blk%es_y(Nx,Ny))
          allocate(blk%c_x(Nx,Ny),blk%c_y(Nx,Ny),blk%n_x(Nx,Ny),blk%n_y(Nx,Ny))
          allocate(blk%s_x(Nx,Ny),blk%s_y(Nx,Ny),blk%w_x(Nx,Ny),blk%w_y(Nx,Ny),blk%e_x(Nx,Ny),blk%e_y(Nx,Ny))

          allocate(blk%lsN(Nx,Ny),blk%lsS(Nx,Ny),blk%lsW(Nx,Ny),blk%lsE(Nx,Ny))
          allocate(blk%DsN(Nx,Ny),blk%DsS(Nx,Ny),blk%DsW(Nx,Ny),blk%DsE(Nx,Ny))
          allocate(blk%Ds2N(Nx,Ny),blk%Ds2S(Nx,Ny),blk%Ds2W(Nx,Ny),blk%Ds2E(Nx,Ny))
          allocate(blk%deltaX(Nx,Ny),blk%deltaY(Nx,Ny),blk%in_ang_sin(Nx,Ny),blk%area(Nx,Ny),blk%mesh_num(Nx,Ny))
          allocate(blk%neighb_n(Nx,Ny),blk%neighb_s(Nx,Ny),blk%neighb_w(Nx,Ny),blk%neighb_e(Nx,Ny))
     end subroutine
     subroutine HFcond_allocate(hfrod,Nx,Ny,z)
     implicit none
          integer,intent(in):: Nx,Ny,z
          type(HFcond),intent(in out):: hfrod
          allocate(hfrod%fblk1_T(Nx,Ny,z),hfrod%fblk2_T(Nx,Ny,z),hfrod%fblk3_T(Nx,Ny,z),&
          hfrod%fblk4_T(Nx,Ny,z),hfrod%fblk5_T(Nx,Ny,z),hfrod%fblk6_T(Nx,Ny,z))
          allocate(hfrod%fblk1_2T(Nx,Ny,z),hfrod%fblk2_2T(Nx,Ny,z),hfrod%fblk3_2T(Nx,Ny,z),&
          hfrod%fblk4_2T(Nx,Ny,z),hfrod%fblk5_2T(Nx,Ny,z),hfrod%fblk6_2T(Nx,Ny,z))
          allocate(hfrod%fblk1_Tn(Nx,Ny,z),hfrod%fblk2_Tn(Nx,Ny,z),hfrod%fblk3_Tn(Nx,Ny,z),&
          hfrod%fblk4_Tn(Nx,Ny,z),hfrod%fblk5_Tn(Nx,Ny,z),hfrod%fblk6_Tn(Nx,Ny,z))
          allocate(hfrod%fblk1_2Tn(Nx,Ny,z),hfrod%fblk2_2Tn(Nx,Ny,z),hfrod%fblk3_2Tn(Nx,Ny,z),&
          hfrod%fblk4_2Tn(Nx,Ny,z),hfrod%fblk5_2Tn(Nx,Ny,z),hfrod%fblk6_2Tn(Nx,Ny,z))

          allocate(hfrod%cblk1_T(Nx,Ny,z),hfrod%cblk2_T(Nx,Ny,z),hfrod%cblk3_T(Nx,Ny,z),&
          hfrod%cblk4_T(Nx,Ny,z))
          allocate(hfrod%cblk1_2T(Nx,Ny,z),hfrod%cblk2_2T(Nx,Ny,z),hfrod%cblk3_2T(Nx,Ny,z),&
          hfrod%cblk4_2T(Nx,Ny,z))
          allocate(hfrod%cblk1_Tn(Nx,Ny,z),hfrod%cblk2_Tn(Nx,Ny,z),hfrod%cblk3_Tn(Nx,Ny,z),&
          hfrod%cblk4_Tn(Nx,Ny,z))
          allocate(hfrod%cblk1_2Tn(Nx,Ny,z),hfrod%cblk2_2Tn(Nx,Ny,z),hfrod%cblk3_2Tn(Nx,Ny,z),&
          hfrod%cblk4_2Tn(Nx,Ny,z))

          allocate(hfrod%twall(Nx,Ny),hfrod%heatFlux(Nx,Ny))
     end subroutine
     end module HF_init
