program main
!use temp2physi_conduction
use hf_heat_conduction
implicit none

    call mesh_genera()
    call solve_HF_heat_conduction()
    print *,'Heat Conduction finished'
end program main

! mesh_blk
 !type(block) :: blk1_fuel,blk2_fuel,blk3_fuel,blk4_fuel,blk5_fuel,blk6_fuel
 !type(block) :: blk1_clad,blk2_clad,blk3_clad,blk4_clad  
 !type(block) :: blk1_2fuel,blk2_2fuel,blk3_2fuel,blk4_2fuel,blk5_2fuel,blk6_2fuel
 !type(block) :: blk1_2clad,blk2_2clad,blk3_2clad,blk4_2clad
 !type :: block
 !       !>控制体左上角坐标
 !       real,allocatable :: wn_x(:,:),wn_y(:,:)
 !       !>控制体左下角坐标
 !       real,allocatable :: ws_x(:,:),ws_y(:,:)            
 !       !>控制体右上角坐标
 !       real,allocatable :: en_x(:,:),en_y(:,:)             
 !       !>控制体右下角坐标
 !       real,allocatable :: es_x(:,:),es_y(:,:)
 !       !>控制体中心坐标
 !       real,allocatable :: c_x(:,:),c_y(:,:)
 !       !>控制体上面坐标
 !       real,allocatable :: n_x(:,:),n_y(:,:)
 !       !>控制体下面坐标
 !       real,allocatable :: s_x(:,:),s_y(:,:)
 !       !>控制体左面坐标
 !       real,allocatable :: w_x(:,:),w_y(:,:)
 !       !>控制体右面坐标
 !       real,allocatable :: e_x(:,:),e_y(:,:)
 !       !>控制体上、下、左、右边长度
 !       real,allocatable :: LsN(:,:),LsS(:,:),LsW(:,:),LsE(:,:)
 !       !> 控制体中心到上、下、左、右边距离
 !       real,allocatable :: DsN(:,:),DsS(:,:),DsW(:,:),DsE(:,:)
 !       !> 控制体中心到相邻控制体中心距离
 !       real,allocatable :: Ds2N(:,:),Ds2S(:,:),Ds2W(:,:),Ds2E(:,:)
 !       !> 控制体dx,dy
 !       real,allocatable :: deltaX(:,:),deltaY(:,:)
 !    !> Angle of opposite side center line
 !       real,allocatable :: in_ang_sin(:,:)
 !       !>Area of control volume
 !       real,allocatable :: area(:,:)
 !       ! >控制体编号
 !       integer,allocatable :: mesh_num(:,:)
 !       !>控制体相邻控制体编号
 !       integer,allocatable :: neighb_n(:,:),neighb_s(:,:),neighb_w(:,:),neighb_e(:,:)        
 !   end type

