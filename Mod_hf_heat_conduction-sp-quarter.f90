Module hf_heat_conduction
  use Hf_Geom           !����
  !use Card_group_9        !����
  !use Rodtab,    only: nrrod  !rod number
  !use Heat_structs, only: rods !type
  !use unitf 
  use HF_init    
  use input
  
  implicit none
  public :: blk_Para,node_Para,meshblock,mesh_genera,solve_HF_heat_conduction,&
         get_coef_matrix,get_HF_fuel_props,get_HF_clad_props,equation_solution,&
         result_hf_conduction  
  
  !local variable
  !> r1_clad, radial of blade
  !> r2_clad, radial of elbow
  real :: r1_clad,r2_clad
  real :: r1_fuel,r2_fuel
  real :: a_clad,a_fuel
  real :: b_clad,b_fuel
  real :: dx_fuel,dx_clad,dy_fuel,dy_clad  
  real :: area_fuel
  real :: a0, a1, a2, a3      !conductivity of Zr-4,kW/cm C
  real :: b0,b1            !specfic heat capacity of Zr-4, J/kg C 
  logical :: debuging = .true.
  !type(block) :: blk1_fuel,blk2_fuel,blk3_fuel,blk4_fuel,blk5_fuel,blk6_fuel
  !type(block) :: blk1_clad,blk2_clad,blk3_clad,blk4_clad
  
contains
  !!!! in:N2,N3,N4,N5(NodeNum),rhc,dhc,thick,lhc,
  !!!! in,out:x1_clad,y1_clad,x1_2clad,y1_2clad...x8_2clad,y8_2clad;
  !!!! in,out:fuel,y1_fuel,x1_2fuel,y1_2fuel...x10_2fuel,y10_2fuel;
  subroutine blk_Para()  !Geometry
     use Hf_Geom
    implicit none
    integer :: i
    real :: theta1,ans
    real :: theta2,theta
      
    !write(*,*) "Start blk_Para"
    if (debuging) then
     print *,'  -blk para started'
    end if
    allocate(x1_clad(N2+1),y1_clad(N2+1),x1_2clad(N2+1),y1_2clad(N2+1))
    allocate(x2_clad(N2+1),y2_clad(N2+1),x2_2clad(N2+1),y2_2clad(N2+1))
    allocate(x3_clad(N3+1),y3_clad(N3+1),x3_2clad(N3+1),y3_2clad(N3+1))
    allocate(x4_clad(N3+1),y4_clad(N3+1),x4_2clad(N3+1),y4_2clad(N3+1))
    allocate(x5_clad(N4+1),y5_clad(N4+1),x5_2clad(N4+1),y5_2clad(N4+1))
    allocate(x6_clad(N4+1),y6_clad(N4+1),x6_2clad(N4+1),y6_2clad(N4+1))
    allocate(x7_clad(N5+1),y7_clad(N5+1),x7_2clad(N5+1),y7_2clad(N5+1))
    allocate(x8_clad(N5+1),y8_clad(N5+1),x8_2clad(N5+1),y8_2clad(N5+1))
    allocate(x1_fuel(N2+1),y1_fuel(N2+1),x1_2fuel(N2+1),y1_2fuel(N2+1))
    allocate(x2_fuel(N2+1),y2_fuel(N2+1),x2_2fuel(N2+1),y2_2fuel(N2+1))
    allocate(x3_fuel(N3+1),y3_fuel(N3+1),x3_2fuel(N3+1),y3_2fuel(N3+1))
    allocate(x4_fuel(N3+1),y4_fuel(N3+1),x4_2fuel(N3+1),y4_2fuel(N3+1))
    allocate(x5_fuel(N3+1),y5_fuel(N3+1),x5_2fuel(N3+1),y5_2fuel(N3+1))
    allocate(x6_fuel(N4+1),y6_fuel(N4+1),x6_2fuel(N4+1),y6_2fuel(N4+1))
    allocate(x7_fuel(N4+1),y7_fuel(N4+1),x7_2fuel(N4+1),y7_2fuel(N4+1))
    allocate(x8_fuel(N4+1),y8_fuel(N4+1),x8_2fuel(N4+1),y8_2fuel(N4+1))
    allocate(x9_fuel(N5+1),y9_fuel(N5+1),x9_2fuel(N5+1),y9_2fuel(N5+1))
    allocate(x10_fuel(N5+1),y10_fuel(N5+1),x10_2fuel(N5+1),y10_2fuel(N5+1))
    ans = 45.0/N2
    do i = 1,N2+1
      !! Region 1, block1-cladding
       !line1       
       theta1 = (225.0+ans*(i-1.0))*2.0*3.1415926/360.0
       x1_clad(i) = rhc*cos(theta1)+(rhc+dhc/2.0)
       y1_clad(i) = rhc*sin(theta1)+(rhc+dhc/2.0)       
      !line2
       x2_clad(i) = (rhc+thick)*cos(theta1)+(rhc+dhc/2.0)
       y2_clad(i) = (rhc+thick)*sin(theta1)+(rhc+dhc/2.0)
      !! Region 2, block1-2cladding
       theta2 = (180.0+ans*(i-1.0))*2.0*3.1415926/360.0
       x1_2clad(i) = rhc*cos(theta2)+(rhc+dhc/2.0)
       y1_2clad(i) = rhc*sin(theta2)+(rhc+dhc/2.0)       
      !line2
       x2_2clad(i) = (rhc+thick)*cos(theta2)+(rhc+dhc/2.0)
       y2_2clad(i) = (rhc+thick)*sin(theta2)+(rhc+dhc/2.0)          
    end do
    

    ans = lhc/N3
    do i =1,N3+1
      !!!Region 1, block2-cladding
      !line3       
       x3_clad(i) = rhc+dhc/2.0+ans*(i-1.0)
       y3_clad(i) = dhc/2.0
      !line4
       x4_clad(i) = rhc+dhc/2.0+ans*(i-1.0)
       y4_clad(i) = dhc/2.0-thick
      !!!Region 2, block2-2cladding
      !line3       
       x3_2clad(i) = dhc/2.0
       y3_2clad(i) = rhc+dhc/2.0+ans*(N3+1.0-i)
      !line4
       x4_2clad(i) = dhc/2.0-thick
       y4_2clad(i) = rhc+dhc/2.0+ans*(N3+1.0-i)       
    end do 
    

    ans = 45.0/N4    
    do i = 1,N4+1
      !!Region 1, block3-cladding
      !line5       
       theta = (90.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x5_clad(i) = (dhc/2.0)*cos(theta)+(rhc+dhc/2.0+lhc)
       y5_clad(i) = (dhc/2.0)*sin(theta)       
      !line6
       x6_clad(i) = (dhc/2.0-thick)*cos(theta)+(rhc+dhc/2.0+lhc)
       y6_clad(i) = (dhc/2.0-thick)*sin(theta)
      !!Region 2, block3-2cladding
      !line5       
       theta = (45.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x5_2clad(i) = (dhc/2.0)*cos(theta)
       y5_2clad(i) = (dhc/2.0)*sin(theta)+(rhc+dhc/2.0+lhc)      
      !line6
       x6_2clad(i) = (dhc/2.0-thick)*cos(theta)
       y6_2clad(i) = (dhc/2.0-thick)*sin(theta)+(rhc+dhc/2.0+lhc)       
       
    end do
    
    ans = 45.0/N5    
    do i = 1,N5+1
      !!Region 1, block4-cladding
      !line7       
       theta = (45.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x7_clad(i) = (dhc/2.0)*cos(theta)+(rhc+dhc/2.0+lhc)
       y7_clad(i) = (dhc/2.0)*sin(theta)           
      !line8    
       x8_clad(i) = (dhc/2.0-thick)*cos(theta)+(rhc+dhc/2.0+lhc)
       y8_clad(i) = (dhc/2.0-thick)*sin(theta)
        !!Region 2, block4-2cladding
        !line7       
       theta = (90.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x7_2clad(i) = (dhc/2.0)*cos(theta)
       y7_2clad(i) = (dhc/2.0)*sin(theta)+(rhc+dhc/2.0+lhc)           
      !line8    
       x8_2clad(i) = (dhc/2.0-thick)*cos(theta)
       y8_2clad(i) = (dhc/2.0-thick)*sin(theta)+(rhc+dhc/2.0+lhc)       
    end do
   

    ans = 45.0/N2    
    do i = 1,N2+1
      !!!Region 1, block1-fuel
      !line1       
       theta = (225.0+ans*(i-1.0))*2.0*3.1415926/360.0
       x1_fuel(i) = (rhc+thick)*cos(theta)+(rhc+dhc/2.0)
       y1_fuel(i) = (rhc+thick)*sin(theta)+(rhc+dhc/2.0) 
      !line2
       x2_fuel(i) = (rhc+dhc/2.0)/N2*(i-1.0)
       y2_fuel(i) = 0.0
      !!!Region 2, block1-2fuel
      !line1       
       theta = (180.0+ans*(i-1.0))*2.0*3.1415926/360.0
       x1_2fuel(i) = (rhc+thick)*cos(theta)+(rhc+dhc/2.0)
       y1_2fuel(i) = (rhc+thick)*sin(theta)+(rhc+dhc/2.0) 
      !line2
       x2_2fuel(i) = 0.0
       y2_2fuel(i) = (rhc+dhc/2.0)/N2*(N2+1.0-i)      
    end do
    

    ans = lhc/N3
    do i =1,N3+1
      !!!Region 1,block2-fuel
      !line3       
       x3_fuel(i) = rhc+dhc/2.0+ans*(i-1.0)
       y3_fuel(i) = dhc/2.0-thick
      !line4
       x4_fuel(i) = rhc+dhc/2.0+ans*(i-1.0)
       y4_fuel(i) = (dhc/2.0-thick)/2.0
      !!!Region 2,block2-2fuel
      !line3       
       x3_2fuel(i) = dhc/2.0-thick
       y3_2fuel(i) = rhc+dhc/2.0+ans*(N3+1.0-i)
      !line4
       x4_2fuel(i) = (dhc/2.0-thick)/2.0
       y4_2fuel(i) = rhc+dhc/2.0+ans*(N3+1.0-i)      
    end do
    
    do i =1,N3+1
      !!!Region 1, block3-fuel
      !line5 
      x5_fuel(i) = rhc+dhc/2.0+ans*(i-1.0)
      y5_fuel(i) = 0.0
      !!!Region 2, block3-2fuel
      !line5
      x5_2fuel(i) = 0.0
      y5_2fuel(i) = rhc+dhc/2.0+ans*(N3+1.0-i)    
    end do


    ans = 45.0/N4
    do i = 1,N4+1
      !!!Region 1, block4-fuel
      !line6       
      theta = (90.0-ans*(i-1))*2.0*3.1415926/360.0
      x6_fuel(i) = (dhc/2.0-thick)*cos(theta)+(rhc+dhc/2.0+lhc)
      y6_fuel(i) = (dhc/2.0-thick)*sin(theta)       
      !line7
       x7_fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0/N4*(i-1.0)
       y7_fuel(i) = (dhc/2.0-thick)/2.0
      !!!Region 2, block4-2fuel
      !line6       
       theta = (45.0-ans*(i-1))*2.0*3.1415926/360.0
       x6_2fuel(i) = (dhc/2.0-thick)*cos(theta)
       y6_2fuel(i) = (dhc/2.0-thick)*sin(theta)+(rhc+dhc/2.0+lhc)       
      !line7
       x7_2fuel(i) = (dhc/2.0-thick)/2.0 
       y7_2fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0/N4*(N4+1.0-i)    
    end do
    

    !line7
    do i =1,N4+1
      !!!Region1,block5-fuel 
      !line8
       x8_fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0/N4*(i-1.0)
       y8_fuel(i) = 0.0
      !!!Region2,block5-2fuel 
      !line8
       x8_2fuel(i) = 0.0
       y8_2fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0/N4*(N4+1.0-i)             
    end do
    
    ans = 45.0/N5
    do i = 1,N5+1
      !!!Region1,block6-fuel
      !line9       
       theta = (45.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x9_fuel(i) = (dhc/2.0-thick)*cos(theta)+(rhc+dhc/2.0+lhc)
       y9_fuel(i) = (dhc/2.0-thick)*sin(theta)       
      !line10
       x10_fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0
       y10_fuel(i) = (dhc/2.0-thick)/2.0/N5*(N5+1.0-i)
      !!!Region2,block6-2fuel
      !line9       
       theta = (90.0-ans*(i-1.0))*2.0*3.1415926/360.0
       x9_2fuel(i) = (dhc/2.0-thick)*cos(theta)
       y9_2fuel(i) = (dhc/2.0-thick)*sin(theta)+(rhc+dhc/2.0+lhc)       
      !line10
       x10_2fuel(i) = (dhc/2.0-thick)/2.0/N5*(i-1.0)
       y10_2fuel(i) = rhc+dhc/2.0+lhc+(dhc/2.0-thick)/2.0
    end do

  end subroutine blk_Para
  
  !!!! in,out:nodeCoordX,nodeCoordY
  subroutine node_Para(line1x,line1y,line2x,line2y,nodeCoordX,nodeCoordY,nodex,nodey)  
   use input
    implicit none
   
    real, intent(in) :: line1x(:),line1y(:),line2x(:),line2y(:)
    integer, intent(in) :: nodex,nodey!
    real, intent(in out) :: nodeCoordX(:,:),nodeCoordY(:,:)
    
    integer :: i,j
    real :: node_space
    integer :: row,col
    
    !row = size(nodeCoordX,1)    
    !write(*,*) "Start node_Para",row
        
    !node_spaceX = 1/nodex
    node_space = 1.0/nodex

    do col = 1, nodey+1
       do row = 1,nodex+1
         nodeCoordX(row,col) = line2x(col)+(line1x(col)-line2x(col))*(row-1)*node_space
         nodeCoordY(row,col) = line2y(col)+(line1y(col)-line2y(col))*(row-1)*node_space
         !write(*,*) nodex,nodey,row,col,line1x(col),line1y(col),line2x(col),line2y(col),nodeCoordX(row,col),&
         !nodeCoordY(row,col),node_space
       end do 
    end do
    return  
  end subroutine node_Para
  
  !> blk:block�Զ�����������
  !> nodeCoordX:�ڵ�x����
  !> nodeCoordY:�ڵ�y����
  !> M:����
  !> N:����
  !> meshN: ��block�п���������
  subroutine meshblock(blk,nodeCoordX,nodeCoordY,M,N,meshN)

    implicit none
    type(block), intent(in out) :: blk
    integer, intent(in out) :: meshN
    real, intent(in) :: nodeCoordX(:,:),nodeCoordY(:,:)
    integer, intent(in) :: M,N
    integer :: i,j
    real :: area1,area2
    call blk_allocate(blk,M,N)
    !write(*,*) "Start meshblock"
    
    do i = 1,M
       do j = 1, N
          !! �ڵ�����
          blk%wn_x(i,j) = nodeCoordX(i+1,j)  !��������
          blk%wn_y(i,j) = nodeCoordY(i+1,j)            
          blk%ws_x(i,j) = nodeCoordX(i,j)  !��������
          blk%ws_y(i,j) = nodeCoordY(i,j)         
          blk%en_x(i,j) = nodeCoordX(i+1,j+1)  !��������
          blk%en_y(i,j) = nodeCoordY(i+1,j+1)          
          blk%es_x(i,j) = nodeCoordX(i,j+1)  !��������
          blk%es_y(i,j) = nodeCoordY(i,j+1)          
          blk%c_x(i,j) = (nodeCoordX(i,j)+nodeCoordX(i,j+1)+nodeCoordX(i+1,j)+nodeCoordX(i+1,j+1))/4.0 !��������
          blk%c_y(i,j) = (nodeCoordY(i,j)+nodeCoordY(i,j+1)+nodeCoordY(i+1,j)+nodeCoordY(i+1,j+1))/4.0
          !! Area of control volume
          area1 = 0.5*abs(blk%es_x(i,j)*blk%en_y(i,j)+blk%ws_x(i,j)*blk%es_y(i,j)+blk%en_x(i,j)*blk%ws_y(i,j)-&
                    blk%es_x(i,j)*blk%ws_y(i,j)-blk%ws_x(i,j)*blk%en_y(i,j)-blk%en_x(i,j)*blk%es_y(i,j)) 
          area2 = 0.5*abs(blk%wn_x(i,j)*blk%en_y(i,j)+blk%ws_x(i,j)*blk%wn_y(i,j)+blk%en_x(i,j)*blk%ws_y(i,j)-&
                    blk%wn_x(i,j)*blk%ws_y(i,j)-blk%ws_x(i,j)*blk%en_y(i,j)-blk%en_x(i,j)*blk%wn_y(i,j)) 
          blk%area(i,j) = area1 + area2
          !! ���������������
          blk%n_x(i,j) = (nodeCoordX(i+1,j)+nodeCoordX(i+1,j+1))/2.0  !�ϱ���������
          blk%n_y(i,j) = (nodeCoordY(i+1,j)+nodeCoordY(i+1,j+1))/2.0
          blk%s_x(i,j) = (nodeCoordX(i,j)+nodeCoordX(i,j+1))/2.0  !�±���������
          blk%s_y(i,j) = (nodeCoordY(i,j)+nodeCoordY(i,j+1))/2.0 
          blk%w_x(i,j) = (nodeCoordX(i+1,j)+nodeCoordX(i,j))/2.0  !�����������
          blk%w_y(i,j) = (nodeCoordY(i+1,j)+nodeCoordY(i,j))/2.0
          blk%e_x(i,j) = (nodeCoordX(i+1,j+1)+nodeCoordX(i,j+1))/2.0  !�ұ���������
          blk%e_y(i,j) = (nodeCoordY(i+1,j+1)+nodeCoordY(i,j+1))/2.0
          !! ������߳�
          blk%LsN(i,j) = ((blk%wn_x(i,j)-blk%en_x(i,j))**2+(blk%wn_y(i,j)-blk%en_y(i,j))**2)**0.5  ! �ϱ߳�
          blk%LsS(i,j) = ((blk%ws_x(i,j)-blk%es_x(i,j))**2+(blk%ws_y(i,j)-blk%es_y(i,j))**2)**0.5  ! �±߳�
          blk%LsW(i,j) = ((blk%wn_x(i,j)-blk%ws_x(i,j))**2+(blk%wn_y(i,j)-blk%ws_y(i,j))**2)**0.5  ! ��߳�
          blk%LsE(i,j) = ((blk%en_x(i,j)-blk%es_x(i,j))**2+(blk%en_y(i,j)-blk%es_y(i,j))**2)**0.5  ! �ұ߳�
          !! �����������ľ���
          blk%DsN(i,j) = ((blk%c_x(i,j)-blk%n_x(i,j))**2+(blk%c_y(i,j)-blk%n_y(i,j))**2)**0.5  !�ϱ�
          blk%DsS(i,j) = ((blk%c_x(i,j)-blk%s_x(i,j))**2+(blk%c_y(i,j)-blk%s_y(i,j))**2)**0.5  !�±� 
          blk%DsW(i,j) = ((blk%c_x(i,j)-blk%w_x(i,j))**2+(blk%c_y(i,j)-blk%w_y(i,j))**2)**0.5  !���
          blk%DsE(i,j) = ((blk%c_x(i,j)-blk%e_x(i,j))**2+(blk%c_y(i,j)-blk%e_y(i,j))**2)**0.5  !�ұ�
          !! ������dx,dy
          blk%deltaX(i,j) = blk%DsW(i,j)+blk%DsE(i,j)
          blk%deltaY(i,j) = blk%DsN(i,j)+blk%DsS(i,j)
          
          blk%mesh_num(i,j) = (i-1)*N+j + meshN
          
          !write(*,*) M,N,i,j,nodeCoordX(i,j),nodeCoordY(i,j)              
       end do
    end do
  
    blk%neighb_n(:,:) = 0
    blk%neighb_s(:,:) = 0
    blk%neighb_w(:,:) = 0
    blk%neighb_e(:,:) = 0
    blk%Ds2N(:,:) = 0.0
    blk%Ds2S(:,:) = 0.0
    blk%Ds2W(:,:) = 0.0
    blk%Ds2E(:,:) = 0.0    
    
    i = M
    do j = 1,N
       blk%Ds2N(i,j) = ((blk%c_x(i,j)-blk%n_x(i,j))**2+(blk%c_y(i,j)-blk%n_y(i,j))**2)**0.5
    enddo 
    i = 1
    do j = 1,N
       blk%Ds2S(i,j) = ((blk%c_x(i,j)-blk%s_x(i,j))**2+(blk%c_y(i,j)-blk%s_y(i,j))**2)**0.5
    enddo
    j = 1
    do i = 1,M
       blk%Ds2W(i,j) = ((blk%c_x(i,j)-blk%w_x(i,j))**2+(blk%c_y(i,j)-blk%w_y(i,j))**2)**0.5
    enddo
    j = N
    do i = 1,M
       blk%Ds2E(i,j) = ((blk%c_x(i,j)-blk%e_x(i,j))**2+(blk%c_y(i,j)-blk%e_y(i,j))**2)**0.5
    enddo          
    
    do i = 1,M-1
       do j = 1,N
         blk%neighb_n(i,j) = blk%mesh_num(i+1,j)
         blk%Ds2N(i,j) = ((blk%c_x(i,j)-blk%c_x(i+1,j))**2+(blk%c_y(i,j)-blk%c_y(i+1,j))**2)**0.5
       end do 
    end do
    
    do i = 2,M
       do j = 1,N
         blk%neighb_s(i,j) = blk%mesh_num(i-1,j)
         blk%Ds2S(i,j) = ((blk%c_x(i,j)-blk%c_x(i-1,j))**2+(blk%c_y(i,j)-blk%c_y(i-1,j))**2)**0.5
       end do 
    end do    
 
    do i = 1,M
       do j =2,N
         blk%neighb_w(i,j) = blk%mesh_num(i,j-1)
         blk%Ds2W(i,j) = ((blk%c_x(i,j)-blk%c_x(i,j-1))**2+(blk%c_y(i,j)-blk%c_y(i,j-1))**2)**0.5
       end do 
    end do

    do i = 1,M
       do j =1,N-1
         blk%neighb_e(i,j) = blk%mesh_num(i,j+1)
         blk%Ds2E(i,j) = ((blk%c_x(i,j)-blk%c_x(i,j+1))**2+(blk%c_y(i,j)-blk%c_y(i,j+1))**2)**0.5
       end do 
    end do
    
    meshN = meshN + M*N
    return
  
  end subroutine meshblock
  
  subroutine mesh_genera()
   use input
    implicit none    
    integer :: meshN
    integer :: M,N,row,col,num,i,j
    !real,allocatable :: nodeCoordX1_clad(:,:),nodeCoordY1_clad(:,:),nodeCoordX2_clad(:,:),nodeCoordY2_clad(:,:)
    !real,allocatable :: nodeCoordX3_clad(:,:),nodeCoordY3_clad(:,:),nodeCoordX4_clad(:,:),nodeCoordY4_clad(:,:)
    !real,allocatable :: nodeCoordX1_fuel(:,:),nodeCoordY1_fuel(:,:),nodeCoordX2_fuel(:,:),nodeCoordY2_fuel(:,:)
    !real,allocatable :: nodeCoordX3_fuel(:,:),nodeCoordY3_fuel(:,:),nodeCoordX4_fuel(:,:),nodeCoordY4_fuel(:,:)
    !real,allocatable :: nodeCoordX5_fuel(:,:),nodeCoordY5_fuel(:,:),nodeCoordX6_fuel(:,:),nodeCoordY6_fuel(:,:)    
    if (debuging) then
     print *,'msh genera started'
    end if
    meshN = 0
    !write(*,*) "Start mesh_genera"

    call blk_Para
    if (debuging) then
     print *,'  -blk_Para finished'
    end if
    !node parameter of cladding
    !write(*,*) nodeCoordX1_clad
    !! Region 1
    allocate(nodeCoordX1_clad(N1+1,N2+1),nodeCoordX1_2clad(N1+1,N2+1),nodeCoordY1_clad(N1+1,N2+1),nodeCoordY1_2clad(N1+1,N2+1))
    allocate(nodeCoordX2_clad(N1+1,N3+1),nodeCoordX2_2clad(N1+1,N3+1),nodeCoordY2_clad(N1+1,N3+1),nodeCoordY2_2clad(N1+1,N3+1))
    allocate(nodeCoordX3_clad(N1+1,N4+1),nodeCoordX3_2clad(N1+1,N4+1),nodeCoordY3_clad(N1+1,N4+1),nodeCoordY3_2clad(N1+1,N4+1))
    allocate(nodeCoordX4_clad(N1+1,N5+1),nodeCoordX4_2clad(N1+1,N5+1),nodeCoordY4_clad(N1+1,N5+1),nodeCoordY4_2clad(N1+1,N5+1))

    allocate(nodeCoordX1_fuel(N5+N6+1,N2+1),nodeCoordY1_fuel(N5+N6+1,N2+1),&
    nodeCoordX1_2fuel(N5+N6+1,N2+1),nodeCoordY1_2fuel(N5+N6+1,N2+1))
    allocate(nodeCoordX2_fuel(N6+1,N3+1),nodeCoordY2_fuel(N6+1,N3+1),nodeCoordX2_2fuel(N6+1,N3+1),nodeCoordY2_2fuel(N6+1,N3+1))
    allocate(nodeCoordX3_fuel(N5+1,N3+1),nodeCoordY3_fuel(N5+1,N3+1),nodeCoordX3_2fuel(N5+1,N3+1),nodeCoordY3_2fuel(N5+1,N3+1))
    allocate(nodeCoordX4_fuel(N6+1,N4+1),nodeCoordY4_fuel(N6+1,N4+1),nodeCoordX4_2fuel(N6+1,N4+1),nodeCoordY4_2fuel(N6+1,N4+1))
    allocate(nodeCoordX5_fuel(N5+1,N4+1),nodeCoordY5_fuel(N5+1,N4+1),nodeCoordX5_2fuel(N5+1,N4+1),nodeCoordY5_2fuel(N5+1,N4+1))
    allocate(nodeCoordX6_fuel(N6+1,N5+1),nodeCoordY6_fuel(N6+1,N5+1),nodeCoordX6_2fuel(N6+1,N5+1),nodeCoordY6_2fuel(N6+1,N5+1))

    call node_Para(x1_clad,y1_clad,x2_clad,y2_clad,nodeCoordX1_clad,nodeCoordY1_clad,N1,N2)   !block 1
    call node_Para(x3_clad,y3_clad,x4_clad,y4_clad,nodeCoordX2_clad,nodeCoordY2_clad,N1,N3)   !block 2
    call node_Para(x5_clad,y5_clad,x6_clad,y6_clad,nodeCoordX3_clad,nodeCoordY3_clad,N1,N4)   !block 3
    call node_Para(x7_clad,y7_clad,x8_clad,y8_clad,nodeCoordX4_clad,nodeCoordY4_clad,N1,N5)   !block 4
    !! Region 2
    call node_Para(x1_2clad,y1_2clad,x2_2clad,y2_2clad,nodeCoordX1_2clad,nodeCoordY1_2clad,N1,N2)   !block 1-2
    call node_Para(x3_2clad,y3_2clad,x4_2clad,y4_2clad,nodeCoordX2_2clad,nodeCoordY2_2clad,N1,N3)   !block 2-2
    call node_Para(x5_2clad,y5_2clad,x6_2clad,y6_2clad,nodeCoordX3_2clad,nodeCoordY3_2clad,N1,N4)   !block 3-2
    call node_Para(x7_2clad,y7_2clad,x8_2clad,y8_2clad,nodeCoordX4_2clad,nodeCoordY4_2clad,N1,N5)   !block 4-2
    !node parameter of fuel
    !! Region 1
    call node_Para(x1_fuel,y1_fuel,x2_fuel,y2_fuel,nodeCoordX1_fuel,nodeCoordY1_fuel,N5+N6,N2)   !block 1
    call node_Para(x3_fuel,y3_fuel,x4_fuel,y4_fuel,nodeCoordX2_fuel,nodeCoordY2_fuel,N6,N3)   !block 2
    call node_Para(x4_fuel,y4_fuel,x5_fuel,y5_fuel,nodeCoordX3_fuel,nodeCoordY3_fuel,N5,N3)   !block 3
    call node_Para(x6_fuel,y6_fuel,x7_fuel,y7_fuel,nodeCoordX4_fuel,nodeCoordY4_fuel,N6,N4)   !block 4    
    call node_Para(x7_fuel,y7_fuel,x8_fuel,y8_fuel,nodeCoordX5_fuel,nodeCoordY5_fuel,N5,N4)   !block 5    
    call node_Para(x9_fuel,y9_fuel,x10_fuel,y10_fuel,nodeCoordX6_fuel,nodeCoordY6_fuel,N6,N5)  !block 6
    !! Region 1
    call node_Para(x1_2fuel,y1_2fuel,x2_2fuel,y2_2fuel,nodeCoordX1_2fuel,nodeCoordY1_2fuel,N5+N6,N2)   !block 1-2
    call node_Para(x3_2fuel,y3_2fuel,x4_2fuel,y4_2fuel,nodeCoordX2_2fuel,nodeCoordY2_2fuel,N6,N3)   !block 2-2
    call node_Para(x4_2fuel,y4_2fuel,x5_2fuel,y5_2fuel,nodeCoordX3_2fuel,nodeCoordY3_2fuel,N5,N3)   !block 3-2
    call node_Para(x6_2fuel,y6_2fuel,x7_2fuel,y7_2fuel,nodeCoordX4_2fuel,nodeCoordY4_2fuel,N6,N4)   !block 4-2    
    call node_Para(x7_2fuel,y7_2fuel,x8_2fuel,y8_2fuel,nodeCoordX5_2fuel,nodeCoordY5_2fuel,N5,N4)   !block 5-2    
    call node_Para(x9_2fuel,y9_2fuel,x10_2fuel,y10_2fuel,nodeCoordX6_2fuel,nodeCoordY6_2fuel,N6,N5)  !block 6-2    
    
    M = N5+N6
    N = N2
    if (debuging) then
     print *,'  -node_Para finished'
    end if
    
    !!=====================����Ϣ========================================
    !> о������
    !! Region 1
    call meshblock(blk1_fuel,nodeCoordX1_fuel,nodeCoordY1_fuel,M,N,meshN)
    call meshblock(blk2_fuel,nodeCoordX2_fuel,nodeCoordY2_fuel,N6,N3,meshN)
    call meshblock(blk3_fuel,nodeCoordX3_fuel,nodeCoordY3_fuel,N5,N3,meshN)
    call meshblock(blk4_fuel,nodeCoordX4_fuel,nodeCoordY4_fuel,N6,N4,meshN)
    call meshblock(blk5_fuel,nodeCoordX5_fuel,nodeCoordY5_fuel,N5,N4,meshN)
    call meshblock(blk6_fuel,nodeCoordX6_fuel,nodeCoordY6_fuel,N6,N5,meshN)
    !! Region 2
    call meshblock(blk1_2fuel,nodeCoordX1_2fuel,nodeCoordY1_2fuel,M,N,meshN)
    call meshblock(blk2_2fuel,nodeCoordX2_2fuel,nodeCoordY2_2fuel,N6,N3,meshN)
    call meshblock(blk3_2fuel,nodeCoordX3_2fuel,nodeCoordY3_2fuel,N5,N3,meshN)
    call meshblock(blk4_2fuel,nodeCoordX4_2fuel,nodeCoordY4_2fuel,N6,N4,meshN)
    call meshblock(blk5_2fuel,nodeCoordX5_2fuel,nodeCoordY5_2fuel,N5,N4,meshN)
    call meshblock(blk6_2fuel,nodeCoordX6_2fuel,nodeCoordY6_2fuel,N6,N5,meshN)    
    
    !>��������
    !! Region 1
    call meshblock(blk1_clad,nodeCoordX1_clad,nodeCoordY1_clad,N1,N2,meshN)
    call meshblock(blk2_clad,nodeCoordX2_clad,nodeCoordY2_clad,N1,N3,meshN)
    call meshblock(blk3_clad,nodeCoordX3_clad,nodeCoordY3_clad,N1,N4,meshN)
    call meshblock(blk4_clad,nodeCoordX4_clad,nodeCoordY4_clad,N1,N5,meshN)
    !! Region 2
    call meshblock(blk1_2clad,nodeCoordX1_2clad,nodeCoordY1_2clad,N1,N2,meshN)
    call meshblock(blk2_2clad,nodeCoordX2_2clad,nodeCoordY2_2clad,N1,N3,meshN)
    call meshblock(blk3_2clad,nodeCoordX3_2clad,nodeCoordY3_2clad,N1,N4,meshN)
    call meshblock(blk4_2clad,nodeCoordX4_2clad,nodeCoordY4_2clad,N1,N5,meshN)   
    if (debuging) then
     print *,'  -mshblocking finished'
    end if 
    !!!! ���紦��ֵ�趨
    i = M
    do j = 1,N
      blk1_fuel%Ds2N(i,j) = ((blk1_fuel%c_x(i,j)-blk1_clad%c_x(1,j))**2+(blk1_fuel%c_y(i,j)-blk1_clad%c_y(1,j))**2)**0.5
      blk1_clad%Ds2S(1,j) = blk1_fuel%Ds2N(i,j)
      
      blk1_2fuel%Ds2N(i,j) = ((blk1_2fuel%c_x(i,j)-blk1_2clad%c_x(1,j))**2+(blk1_2fuel%c_y(i,j)-blk1_2clad%c_y(1,j))**2)**0.5
      blk1_2clad%Ds2S(1,j) = blk1_2fuel%Ds2N(i,j)      
    enddo
    
    j = N
    do i = 1,N5
      blk1_fuel%Ds2E(i,j) = ((blk1_fuel%c_x(i,j)-blk3_fuel%c_x(i,1))**2+(blk1_fuel%c_y(i,j)-blk3_fuel%c_y(i,1))**2)**0.5
      blk3_fuel%Ds2W(i,1) = blk1_fuel%Ds2E(i,j)
      blk1_fuel%Ds2W(i,j) = ((blk1_fuel%c_x(i,j)-blk1_2fuel%c_x(i,N2))**2+(blk1_fuel%c_y(i,j)-blk1_2fuel%c_y(i,N2))**2)**0.5
      
      blk1_2fuel%Ds2E(i,N2) = blk1_fuel%Ds2W(i,j)
      blk1_2fuel%Ds2W(i,1) = ((blk1_2fuel%c_x(i,1)-blk3_2fuel%c_x(i,N3))**2+(blk1_2fuel%c_y(i,1)-blk3_2fuel%c_y(i,N3))**2)**0.5
      blk3_2fuel%Ds2E(i,N3) = blk1_2fuel%Ds2E(i,1)      
    enddo
    do i = N5+1,M
      blk1_fuel%Ds2E(i,j) = ((blk1_fuel%c_x(i,j)-blk2_fuel%c_x(i-N5,1))**2+(blk1_fuel%c_y(i,j)-blk2_fuel%c_y(i-N5,1))**2)**0.5
      blk2_fuel%Ds2W(i-N5,1) = blk1_fuel%Ds2E(i,j)
      blk1_fuel%Ds2W(i,j) = ((blk1_fuel%c_x(i,j)-blk1_2fuel%c_x(i,N2))**2+(blk1_fuel%c_y(i,j)-blk1_2fuel%c_y(i,N2))**2)**0.5
      
      blk1_2fuel%Ds2E(i,N2) = blk1_fuel%Ds2W(i,j)
      blk1_2fuel%Ds2W(i,1) = blk1_fuel%Ds2E(i,j)
      blk2_2fuel%Ds2E(i,N3) = blk1_2fuel%Ds2E(i,1)            
    enddo
        
    do j = 1,N3
      blk2_fuel%Ds2N(N6,j) = ((blk2_fuel%c_x(N6,j)-blk2_clad%c_x(1,j))**2+(blk2_fuel%c_y(N6,j)-blk2_clad%c_y(1,j))**2)**0.5
      blk2_clad%Ds2S(1,j) = blk2_fuel%Ds2N(N6,j)
      blk2_fuel%Ds2S(1,j) = ((blk2_fuel%c_x(1,j)-blk3_fuel%c_x(N5,j))**2+(blk2_fuel%c_y(1,j)-blk3_fuel%c_y(N5,j))**2)**0.5
      blk3_fuel%Ds2N(N5,j) = blk2_fuel%Ds2S(1,j)
      
      blk2_2fuel%Ds2N(N6,j) = ((blk2_2fuel%c_x(N6,j)-blk2_2clad%c_x(1,j))**2+(blk2_2fuel%c_y(N6,j)-blk2_2clad%c_y(1,j))**2)**0.5
      blk2_2clad%Ds2S(1,j) = blk2_2fuel%Ds2N(N6,j)
      blk2_2fuel%Ds2S(1,j) = ((blk2_2fuel%c_x(1,j)-blk3_2fuel%c_x(N5,j))**2+(blk2_2fuel%c_y(1,j)-blk3_2fuel%c_y(N5,j))**2)**0.5
      blk3_2fuel%Ds2N(N5,j) = blk2_2fuel%Ds2S(1,j)      
    enddo
    
    do i = 1,N6
      blk2_fuel%Ds2E(i,N3) = ((blk2_fuel%c_x(i,N3)-blk4_fuel%c_x(i,1))**2+(blk2_fuel%c_y(i,N3)-blk4_fuel%c_y(i,1))**2)**0.5
      blk4_fuel%Ds2W(i,1) =  blk2_fuel%Ds2E(i,N3)  
      blk4_fuel%Ds2E(i,N4) = ((blk4_fuel%c_x(i,N4)-blk6_fuel%c_x(i,1))**2+(blk4_fuel%c_y(i,N4)-blk6_fuel%c_y(i,1))**2)**0.5
      blk6_fuel%Ds2W(i,1) = blk4_fuel%Ds2E(i,N4)
      
      blk2_2fuel%Ds2W(i,1) = ((blk2_2fuel%c_x(i,1)-blk4_2fuel%c_x(i,N4))**2+(blk2_2fuel%c_y(i,1)-blk4_2fuel%c_y(i,N4))**2)**0.5
      blk4_2fuel%Ds2E(i,N4) =  blk2_2fuel%Ds2W(i,1)  
      blk4_2fuel%Ds2W(i,1) = blk4_fuel%Ds2E(i,N4)
      blk6_2fuel%Ds2E(i,N5) = blk4_2fuel%Ds2W(i,1) 
    enddo
    
    do i = 1,N5
      blk3_fuel%Ds2E(i,N3) = ((blk3_fuel%c_x(i,N3)-blk5_fuel%c_x(i,1))**2+(blk3_fuel%c_y(i,N3)-blk5_fuel%c_y(i,1))**2)**0.5
      blk5_fuel%Ds2W(i,1) = blk3_fuel%Ds2E(i,N3)
      
      blk3_2fuel%Ds2W(i,1) = blk3_fuel%Ds2E(i,N3)
      blk5_2fuel%Ds2E(i,N4) = blk3_2fuel%Ds2W(i,1)      
    end do
    
    do j = 1,N4
      blk4_fuel%Ds2N(N6,j) = ((blk4_fuel%c_x(N6,j)-blk3_clad%c_x(1,j))**2+(blk4_fuel%c_y(N6,j)-blk3_clad%c_y(1,j))**2)**0.5
      blk3_clad%Ds2S(1,j) = blk4_fuel%Ds2N(N6,j)
      blk4_fuel%Ds2S(1,j) = ((blk4_fuel%c_x(1,j)-blk5_fuel%c_x(N5,j))**2+(blk4_fuel%c_y(1,j)-blk5_fuel%c_y(N5,j))**2)**0.5
      blk5_fuel%Ds2N(N5,j) = blk4_fuel%Ds2S(1,j)
      
      blk4_2fuel%Ds2N(N6,j) = ((blk4_2fuel%c_x(N6,j)-blk3_2clad%c_x(1,j))**2+(blk4_2fuel%c_y(N6,j)-blk3_2clad%c_y(1,j))**2)**0.5
      blk3_2clad%Ds2S(1,j) = blk4_2fuel%Ds2N(N6,j)
      blk4_2fuel%Ds2S(1,j) = ((blk4_2fuel%c_x(1,j)-blk5_2fuel%c_x(N5,j))**2+(blk4_2fuel%c_y(1,j)-blk5_2fuel%c_y(N5,j))**2)**0.5
      blk5_2fuel%Ds2N(N5,j) = blk4_2fuel%Ds2S(1,j)      
    enddo
    
    do i = 1,N5
      blk5_fuel%Ds2E(i,N4)=((blk5_fuel%c_x(i,N4)-blk6_fuel%c_x(1,N5+1-i))**2+&
                      (blk5_fuel%c_y(i,N4)-blk6_fuel%c_y(1,N5+1-i))**2)**0.5
      blk6_fuel%Ds2S(1,N5+1-i) = blk5_fuel%Ds2E(i,N4)
      blk6_fuel%Ds2N(N6,N5+1-i) = ((blk6_fuel%c_x(N6,N5+1-i)-blk4_clad%c_x(1,N5+1-i))**2+&
                      (blk6_fuel%c_y(N6,N5+1-i)-blk4_clad%c_y(1,N5+1-i))**2)**0.5
      blk4_clad%Ds2S(1,N5+1-i) = blk6_fuel%Ds2N(N6,N5+1-i)
      
      blk5_2fuel%Ds2W(i,1)= blk5_fuel%Ds2E(i,N4)
      blk6_2fuel%Ds2S(1,i) = blk5_2fuel%Ds2W(i,1)
      blk6_2fuel%Ds2N(N6,i) = ((blk6_2fuel%c_x(N6,i)-blk4_2clad%c_x(1,i))**2+&
                      (blk6_2fuel%c_y(N6,i)-blk4_2clad%c_y(1,i))**2)**0.5
      blk4_2clad%Ds2S(1,i) = blk6_2fuel%Ds2N(N6,i)      
    enddo
    
    do i = 1,N1
      blk1_clad%Ds2E(i,N2) = ((blk1_clad%c_x(i,N2)-blk2_clad%c_x(i,1))**2+(blk1_clad%c_y(i,N2)-blk2_clad%c_y(i,1))**2)**0.5
      blk2_clad%Ds2W(i,1) = blk1_clad%Ds2E(i,N2)
      blk2_clad%Ds2E(i,N3) = ((blk2_clad%c_x(i,N3)-blk3_clad%c_x(i,1))**2+(blk1_clad%c_y(i,N3)-blk3_clad%c_y(i,1))**2)**0.5
      blk3_clad%Ds2W(i,1) = blk2_clad%Ds2E(i,N3)
      blk3_clad%Ds2E(i,N4) = ((blk3_clad%c_x(i,N4)-blk4_clad%c_x(i,1))**2+(blk3_clad%c_y(i,N4)-blk4_clad%c_y(i,1))**2)**0.5
      blk4_clad%Ds2W(i,1) = blk3_clad%Ds2E(i,N4)
      blk1_clad%Ds2W(i,1) = ((blk1_clad%c_x(i,1)-blk1_2clad%c_x(i,N2))**2+(blk1_clad%c_y(i,1)-blk1_2clad%c_y(i,N2))**2)**0.5
      
      blk1_2clad%Ds2E(i,N2) = blk1_clad%Ds2W(i,1)
      blk1_2clad%Ds2W(i,1) = blk1_clad%Ds2E(i,N2)
      blk2_2clad%Ds2E(i,1) = blk1_clad%Ds2E(i,N2)
      blk2_2clad%Ds2W(i,1) = blk2_clad%Ds2E(i,N3)
      blk3_2clad%Ds2E(i,1) = blk2_clad%Ds2E(i,N3)
      blk3_2clad%Ds2W(i,1) = blk3_clad%Ds2E(i,N4)
      blk4_2clad%Ds2E(i,N5) = blk3_clad%Ds2E(i,N4)      
    enddo
    
    if(debuging) then
      print *,'mesh genera finished'
   end if       
  end subroutine mesh_genera
  
  subroutine solve_HF_heat_conduction
  
    !use Xtradat,     only:dt    !ʱ�䲽��
    !use Timestep_mod,  only: Get_time_data  !ʱ����Ϣ
    !use Rodtab,      only: nrrod,nsrod  !������
    !use powermod,    only: rodpowers    !����
    !use FuelRod_type,  only: FuelRod    !�Զ������
    !use Rodtab,      only: pin_sc_conn  !������ͨ���Ĺ�ϵ
    !use Spltdat,     only: flmesh     !��������
    !use Sol_dom,     only: ch        !��ͨ���������
    !use Unitf,      only: Psfrel2psia  !ѹ��ת��
    !use fluidprops,   only: liquid_props
    !use Solidprops,   only: Rcold      !�ܶ�
    
    implicit none
    !�ֲ�����
    type(HFcond), pointer :: HFrod
    !type(FuelRod), pointer :: rod    
    integer :: n,i,j,z,node,row,col,k
    integer :: zmax,kmax,chnum,isec
    integer :: M,Nfblk1
    integer :: iter
    real :: dt_heat,rtwfp,timet
    real :: htc,htcavg
    real :: tfluid
    real :: hl,havg_ch,favg_ch
    real :: pij
    real :: linear_power, volume_power
    real :: kep, kwp, knp, ksp
    real :: damp,damp1,damp2
    real :: resmax1,maxresi
    real :: fblk1_resmax,fblk2_resmax,fblk3_resmax,fblk4_resmax,fblk5_resmax,fblk6_resmax
    real :: cblk1_resmax,cblk2_resmax,cblk3_resmax,cblk4_resmax
    real :: fblk1_2resmax,fblk2_2resmax,fblk3_2resmax,fblk4_2resmax,fblk5_2resmax,fblk6_2resmax
    real :: cblk1_2resmax,cblk2_2resmax,cblk3_2resmax,cblk4_2resmax    
    real :: ap1,ap2,ae,aw,as,an
    real :: RFuel,RCladding
    real :: CoordX,CoordY,Angle,NonUniF
    real :: tcd,sumtcd,nodenum             !thermal conduction distance
    real :: a1,a2,a3,a4
    real :: b1,b2,b3,b4
    real :: c1,c2,c3,c4
    !!region 1    
    real,allocatable :: fblk1_ap1(:,:),fblk1_ae1(:,:),fblk1_aw1(:,:),fblk1_an1(:,:),fblk1_as1(:,:),fblk1_ap0(:,:),fblk1_bp(:,:)
    real,allocatable :: fblk2_ap1(:,:),fblk2_ae1(:,:),fblk2_aw1(:,:),fblk2_an1(:,:),fblk2_as1(:,:),fblk2_ap0(:,:),fblk2_bp(:,:)
    real,allocatable :: fblk3_ap1(:,:),fblk3_ae1(:,:),fblk3_aw1(:,:),fblk3_an1(:,:),fblk3_as1(:,:),fblk3_ap0(:,:),fblk3_bp(:,:)
    real,allocatable :: fblk4_ap1(:,:),fblk4_ae1(:,:),fblk4_aw1(:,:),fblk4_an1(:,:),fblk4_as1(:,:),fblk4_ap0(:,:),fblk4_bp(:,:)
    real,allocatable :: fblk5_ap1(:,:),fblk5_ae1(:,:),fblk5_aw1(:,:),fblk5_an1(:,:),fblk5_as1(:,:),fblk5_ap0(:,:),fblk5_bp(:,:)
    real,allocatable :: fblk6_ap1(:,:),fblk6_ae1(:,:),fblk6_aw1(:,:),fblk6_an1(:,:),fblk6_as1(:,:),fblk6_ap0(:,:),fblk6_bp(:,:)
    real,allocatable :: cblk1_ap1(:,:),cblk1_ae1(:,:),cblk1_aw1(:,:),cblk1_an1(:,:),cblk1_as1(:,:),cblk1_ap0(:,:),cblk1_bp(:,:)
    real,allocatable :: cblk2_ap1(:,:),cblk2_ae1(:,:),cblk2_aw1(:,:),cblk2_an1(:,:),cblk2_as1(:,:),cblk2_ap0(:,:),cblk2_bp(:,:)
    real,allocatable :: cblk3_ap1(:,:),cblk3_ae1(:,:),cblk3_aw1(:,:),cblk3_an1(:,:),cblk3_as1(:,:),cblk3_ap0(:,:),cblk3_bp(:,:)
    real,allocatable :: cblk4_ap1(:,:),cblk4_ae1(:,:),cblk4_aw1(:,:),cblk4_an1(:,:),cblk4_as1(:,:),cblk4_ap0(:,:),cblk4_bp(:,:)    
    real,allocatable,dimension (:,:) :: fblk1_kc_HF,fblk2_kc_HF,fblk3_kc_HF,fblk4_kc_HF,fblk5_kc_HF,fblk6_kc_HF
    real,allocatable,dimension (:,:) :: cblk1_kc_HF,cblk2_kc_HF,cblk3_kc_HF,cblk4_kc_HF
    real,allocatable,dimension (:,:) :: fblk1_cp_HF,fblk2_cp_HF,fblk3_cp_HF,fblk4_cp_HF,fblk5_cp_HF,fblk6_cp_HF
    real,allocatable,dimension (:,:) :: cblk1_cp_HF,cblk2_cp_HF,cblk3_cp_HF,cblk4_cp_HF    
    real,allocatable,dimension (:,:) :: fblk1_tsolid,fblk2_tsolid,fblk3_tsolid,fblk4_tsolid,fblk5_tsolid,fblk6_tsolid
    real,allocatable,dimension (:,:) :: fblk1_tnsolid,fblk2_tnsolid,fblk3_tnsolid,fblk4_tnsolid,fblk5_tnsolid,fblk6_tnsolid
    real,allocatable,dimension (:,:) :: fblk1_tsolid0,fblk2_tsolid0,fblk3_tsolid0,fblk4_tsolid0,fblk5_tsolid0,fblk6_tsolid0
    real,allocatable,dimension (:,:) :: cblk1_tsolid,cblk2_tsolid,cblk3_tsolid,cblk4_tsolid
    real,allocatable,dimension (:,:) :: cblk1_tnsolid,cblk2_tnsolid,cblk3_tnsolid,cblk4_tnsolid
    real,allocatable,dimension (:,:) :: cblk1_tsolid0,cblk2_tsolid0,cblk3_tsolid0,cblk4_tsolid0
    real,allocatable,dimension (:) :: fblk1_te,fblk1_tw,fblk1_tn,fblk1_ts
    real,allocatable,dimension (:) :: fblk2_te,fblk2_tw,fblk2_tn,fblk2_ts
    real,allocatable,dimension (:) :: fblk3_te,fblk3_tw,fblk3_tn,fblk3_ts
    real,allocatable,dimension (:) :: fblk4_te,fblk4_tw,fblk4_tn,fblk4_ts
    real,allocatable,dimension (:) :: fblk5_te,fblk5_tw,fblk5_tn,fblk5_ts
    real,allocatable,dimension (:) :: fblk6_te,fblk6_tw,fblk6_tn,fblk6_ts
    real,allocatable,dimension (:) :: cblk1_te,cblk1_tw,cblk1_tn,cblk1_ts
    real,allocatable,dimension (:) :: cblk2_te,cblk2_tw,cblk2_tn,cblk2_ts
    real,allocatable,dimension (:) :: cblk3_te,cblk3_tw,cblk3_tn,cblk3_ts
    real,allocatable,dimension (:) :: cblk4_te,cblk4_tw,cblk4_tn,cblk4_ts
    real,allocatable,dimension (:) :: cblk1_twal,cblk2_twal,cblk3_twal,cblk4_twal
    real,allocatable,dimension (:) :: cblk1_heatflux,cblk2_heatflux,cblk3_heatflux,cblk4_heatflux
    !!region 2
    real,allocatable :: fblk1_2ap1(:,:),fblk1_2ae1(:,:),fblk1_2aw1(:,:),fblk1_2an1(:,:)
    real,allocatable :: fblk1_2as1(:,:),fblk1_2ap0(:,:),fblk1_2bp(:,:)
    real,allocatable :: fblk2_2ap1(:,:),fblk2_2ae1(:,:),fblk2_2aw1(:,:)
    real,allocatable :: fblk2_2an1(:,:),fblk2_2as1(:,:),fblk2_2ap0(:,:),fblk2_2bp(:,:)
    real,allocatable :: fblk3_2ap1(:,:),fblk3_2ae1(:,:),fblk3_2aw1(:,:),fblk3_2an1(:,:)
    real,allocatable :: fblk3_2as1(:,:),fblk3_2ap0(:,:),fblk3_2bp(:,:)
    real,allocatable :: fblk4_2ap1(:,:),fblk4_2ae1(:,:),fblk4_2aw1(:,:),fblk4_2an1(:,:)
    real,allocatable :: fblk4_2as1(:,:),fblk4_2ap0(:,:),fblk4_2bp(:,:)
    real,allocatable :: fblk5_2ap1(:,:),fblk5_2ae1(:,:),fblk5_2aw1(:,:),fblk5_2an1(:,:)
    real,allocatable :: fblk5_2as1(:,:),fblk5_2ap0(:,:),fblk5_2bp(:,:)
    real,allocatable :: fblk6_2ap1(:,:),fblk6_2ae1(:,:),fblk6_2aw1(:,:),fblk6_2an1(:,:)
    real,allocatable :: fblk6_2as1(:,:),fblk6_2ap0(:,:),fblk6_2bp(:,:)
    real,allocatable :: cblk1_2ap1(:,:),cblk1_2ae1(:,:),cblk1_2aw1(:,:),cblk1_2an1(:,:)
    real,allocatable :: cblk1_2as1(:,:),cblk1_2ap0(:,:),cblk1_2bp(:,:)
    real,allocatable :: cblk2_2ap1(:,:),cblk2_2ae1(:,:),cblk2_2aw1(:,:),cblk2_2an1(:,:)
    real,allocatable :: cblk2_2as1(:,:),cblk2_2ap0(:,:),cblk2_2bp(:,:)
    real,allocatable :: cblk3_2ap1(:,:),cblk3_2ae1(:,:),cblk3_2aw1(:,:),cblk3_2an1(:,:)
    real,allocatable :: cblk3_2as1(:,:),cblk3_2ap0(:,:),cblk3_2bp(:,:)
    real,allocatable :: cblk4_2ap1(:,:),cblk4_2ae1(:,:),cblk4_2aw1(:,:),cblk4_2an1(:,:)
    real,allocatable :: cblk4_2as1(:,:),cblk4_2ap0(:,:),cblk4_2bp(:,:)    
    real,allocatable,dimension (:,:) :: fblk1_2kc_HF,fblk2_2kc_HF,fblk3_2kc_HF,fblk4_2kc_HF,fblk5_2kc_HF,fblk6_2kc_HF
    real,allocatable,dimension (:,:) :: cblk1_2kc_HF,cblk2_2kc_HF,cblk3_2kc_HF,cblk4_2kc_HF
    real,allocatable,dimension (:,:) :: fblk1_2cp_HF,fblk2_2cp_HF,fblk3_2cp_HF,fblk4_2cp_HF,fblk5_2cp_HF,fblk6_2cp_HF
    real,allocatable,dimension (:,:) :: cblk1_2cp_HF,cblk2_2cp_HF,cblk3_2cp_HF,cblk4_2cp_HF    
    real,allocatable,dimension (:,:) :: fblk1_2tsolid,fblk2_2tsolid,fblk3_2tsolid,fblk4_2tsolid,fblk5_2tsolid,fblk6_2tsolid
    real,allocatable,dimension (:,:) :: fblk1_2tnsolid,fblk2_2tnsolid,fblk3_2tnsolid,fblk4_2tnsolid,fblk5_2tnsolid,fblk6_2tnsolid
    real,allocatable,dimension (:,:) :: fblk1_2tsolid0,fblk2_2tsolid0,fblk3_2tsolid0,fblk4_2tsolid0,fblk5_2tsolid0,fblk6_2tsolid0
    real,allocatable,dimension (:,:) :: cblk1_2tsolid,cblk2_2tsolid,cblk3_2tsolid,cblk4_2tsolid
    real,allocatable,dimension (:,:) :: cblk1_2tnsolid,cblk2_2tnsolid,cblk3_2tnsolid,cblk4_2tnsolid
    real,allocatable,dimension (:,:) :: cblk1_2tsolid0,cblk2_2tsolid0,cblk3_2tsolid0,cblk4_2tsolid0
    real,allocatable,dimension (:) :: fblk1_2te,fblk1_2tw,fblk1_2tn,fblk1_2ts
    real,allocatable,dimension (:) :: fblk2_2te,fblk2_2tw,fblk2_2tn,fblk2_2ts
    real,allocatable,dimension (:) :: fblk3_2te,fblk3_2tw,fblk3_2tn,fblk3_2ts
    real,allocatable,dimension (:) :: fblk4_2te,fblk4_2tw,fblk4_2tn,fblk4_2ts
    real,allocatable,dimension (:) :: fblk5_2te,fblk5_2tw,fblk5_2tn,fblk5_2ts
    real,allocatable,dimension (:) :: fblk6_2te,fblk6_2tw,fblk6_2tn,fblk6_2ts
    real,allocatable,dimension (:) :: cblk1_2te,cblk1_2tw,cblk1_2tn,cblk1_2ts
    real,allocatable,dimension (:) :: cblk2_2te,cblk2_2tw,cblk2_2tn,cblk2_2ts
    real,allocatable,dimension (:) :: cblk3_2te,cblk3_2tw,cblk3_2tn,cblk3_2ts
    real,allocatable,dimension (:) :: cblk4_2te,cblk4_2tw,cblk4_2tn,cblk4_2ts
    real,allocatable,dimension (:) :: cblk1_2twal,cblk2_2twal,cblk3_2twal,cblk4_2twal
    real,allocatable,dimension (:) :: cblk1_2heatflux,cblk2_2heatflux,cblk3_2heatflux,cblk4_2heatflux 
    
    real,allocatable,dimension (:) :: HTC_factor
    
    integer :: debug
    debug = 1 ! �������ó���������
    
    maxresi = 0.001
    a1 = -3.483; a2=-4.658;a3=0.5866;a4=1.483
    b1 = -0.1963; b2=0.4692;b3=-0.507;b4=1.112
    c1 = 13.75; c2=-63.73;c3=99.21;c4=-50.4
    
    Nfblk1 = N5+N6
    nodenum = N2+N3+N4+N5
    area_fuel = 3.14159*(dhc-2*thick)**2/2+lhc*(dhc-2*thick)*4+(2*(rhc+thick)+(dhc-2*thick))**2-3.14159*(rhc+thick)**2
    
    !! Sum thermal conduction distance
    sumtcd = 0.0
    !Blk1-Cladding
    do col = 1,N2
         CoordX = blk1_clad%n_x(N1,col);CoordY = blk1_clad%n_y(N1,col)
         tcd = sqrt(CoordX**2+CoordY**2)
         sumtcd = sumtcd + tcd**ntcp
    enddo    

    
    !Blk2-Cladding
    do col = 1,N3
         CoordX = blk2_clad%n_x(row,col);CoordY = blk2_clad%n_y(row,col)
         tcd = sqrt(CoordX**2+CoordY**2)
         sumtcd = sumtcd + tcd**ntcp
    enddo    

    !Blk3-Cladding
    do col = 1,N4
         CoordX = blk3_clad%n_x(row,col);CoordY = blk3_clad%n_y(row,col)
         tcd = sqrt(CoordX**2+CoordY**2)
         sumtcd = sumtcd + tcd**ntcp
    enddo           
 
    !Blk4-Cladding
    do col = 1,N5
         CoordX = blk4_clad%n_x(row,col);CoordY = blk4_clad%n_y(row,col)
         tcd = sqrt(CoordX**2+CoordY**2)
         sumtcd = sumtcd + tcd**ntcp
    enddo         
    
    dt_heat = 1e8    
    !Time step
    !if (hf_c == 1) then
    !  call Get_time_data(rtwfp=rtwfp)
    !  dt_heat = rtwfp * dt / 3600. ! [h]
    !    !��ǰʱ�̹���
    !  call rodpowers
    !end if
    
    write(*,*) "Start runing solve_HF_heat_conduction"
      !!! region 1
      allocate(fblk1_ap1(Nfblk1,N2),fblk1_ae1(Nfblk1,N2),fblk1_aw1(Nfblk1,N2),fblk1_an1(Nfblk1,N2),&
            fblk1_as1(Nfblk1,N2),fblk1_ap0(Nfblk1,N2),fblk1_bp(Nfblk1,N2))
      allocate(fblk2_ap1(N6,N3),fblk2_ae1(N6,N3),fblk2_aw1(N6,N3),fblk2_an1(N6,N3),fblk2_as1(N6,N3),&
            fblk2_ap0(N6,N3),fblk2_bp(N6,N3))
      allocate(fblk3_ap1(N5,N3),fblk3_ae1(N5,N3),fblk3_aw1(N5,N3),fblk3_an1(N5,N3),fblk3_as1(N5,N3),&
            fblk3_ap0(N5,N3),fblk3_bp(N5,N3))
      allocate(fblk4_ap1(N6,N4),fblk4_ae1(N6,N4),fblk4_aw1(N6,N4),fblk4_an1(N6,N4),fblk4_as1(N6,N4),&
            fblk4_ap0(N6,N4),fblk4_bp(N6,N4))
      allocate(fblk5_ap1(N5,N4),fblk5_ae1(N5,N4),fblk5_aw1(N5,N4),fblk5_an1(N5,N4),fblk5_as1(N5,N4),&
            fblk5_ap0(N5,N4),fblk5_bp(N5,N4))
      allocate(fblk6_ap1(N6,N5),fblk6_ae1(N6,N5),fblk6_aw1(N6,N5),fblk6_an1(N6,N5),fblk6_as1(N6,N5),&
            fblk6_ap0(N6,N5),fblk6_bp(N6,N5))
    
      allocate(cblk1_ap1(N1,N2),cblk1_ae1(N1,N2),cblk1_aw1(N1,N2),cblk1_an1(N1,N2),cblk1_as1(N1,N2),&
            cblk1_ap0(N1,N2),cblk1_bp(N1,N2))
      allocate(cblk2_ap1(N1,N3),cblk2_ae1(N1,N3),cblk2_aw1(N1,N3),cblk2_an1(N1,N3),cblk2_as1(N1,N3),&
            cblk2_ap0(N1,N3),cblk2_bp(N1,N3))
      allocate(cblk3_ap1(N1,N4),cblk3_ae1(N1,N4),cblk3_aw1(N1,N4),cblk3_an1(N1,N4),cblk3_as1(N1,N4),&
            cblk3_ap0(N1,N4),cblk3_bp(N1,N4))
      allocate(cblk4_ap1(N1,N5),cblk4_ae1(N1,N5),cblk4_aw1(N1,N5),cblk4_an1(N1,N5),cblk4_as1(N1,N5),&
            cblk4_ap0(N1,N5),cblk4_bp(N1,N5))
    
      allocate(fblk1_kc_HF(Nfblk1,N2),fblk2_kc_HF(N6,N3),fblk3_kc_HF(N5,N3),fblk4_kc_HF(N6,N4),&
            fblk5_kc_HF(N5,N4),fblk6_kc_HF(N6,N5))
      allocate(cblk1_kc_HF(N1,N2),cblk2_kc_HF(N1,N3),cblk3_kc_HF(N1,N4),cblk4_kc_HF(N1,N5))
      allocate(fblk1_cp_HF(Nfblk1,N2),fblk2_cp_HF(N6,N3),fblk3_cp_HF(N5,N3),fblk4_cp_HF(N6,N4),&
            fblk5_cp_HF(N5,N4),fblk6_cp_HF(N6,N5))
      allocate(cblk1_cp_HF(N1,N2),cblk2_cp_HF(N1,N3),cblk3_cp_HF(N1,N4),cblk4_cp_HF(N1,N5))      
      
      allocate(fblk1_tsolid(Nfblk1,N2),fblk2_tsolid(N6,N3),fblk3_tsolid(N5,N3),fblk4_tsolid(N6,N4),&
            fblk5_tsolid(N5,N4),fblk6_tsolid(N6,N5))
      allocate(fblk1_tnsolid(Nfblk1,N2),fblk2_tnsolid(N6,N3),fblk3_tnsolid(N5,N3),fblk4_tnsolid(N6,N4),&
            fblk5_tnsolid(N5,N4),fblk6_tnsolid(N6,N5))
      allocate(fblk1_tsolid0(Nfblk1,N2),fblk2_tsolid0(N6,N3),fblk3_tsolid0(N5,N3),fblk4_tsolid0(N6,N4),&
            fblk5_tsolid0(N5,N4),fblk6_tsolid0(N6,N5))
      
      allocate(cblk1_tsolid(N1+1,N2),cblk2_tsolid(N1,N3),cblk3_tsolid(N1,N4),cblk4_tsolid(N1,N5))
      allocate(cblk1_tnsolid(N1+1,N2),cblk2_tnsolid(N1,N3),cblk3_tnsolid(N1,N4),cblk4_tnsolid(N1,N5))
      allocate(cblk1_tsolid0(N1+1,N2),cblk2_tsolid0(N1,N3),cblk3_tsolid0(N1,N4),cblk4_tsolid0(N1,N5))
      
      allocate(fblk1_te(Nfblk1),fblk1_tw(Nfblk1),fblk1_tn(N2),fblk1_ts(N2))
      allocate(fblk2_te(N6),fblk2_tw(N6),fblk2_tn(N3),fblk2_ts(N2))
      allocate(fblk3_te(N5),fblk3_tw(N5),fblk3_tn(N3),fblk3_ts(N3))
      allocate(fblk4_te(N6),fblk4_tw(N6),fblk4_tn(N4),fblk4_ts(N4))
      allocate(fblk5_te(N5),fblk5_tw(N5),fblk5_tn(N4),fblk5_ts(N4))
      allocate(fblk6_te(N6),fblk6_tw(N6),fblk6_tn(N5),fblk6_ts(N5))
      
      allocate(cblk1_te(N1),cblk1_tw(N1),cblk1_tn(N2),cblk1_ts(N2),cblk1_twal(N2),cblk1_heatflux(N2))
      allocate(cblk2_te(N1),cblk2_tw(N1),cblk2_tn(N3),cblk2_ts(N3),cblk2_twal(N3),cblk2_heatflux(N3))
      allocate(cblk3_te(N1),cblk3_tw(N1),cblk3_tn(N4),cblk3_ts(N4),cblk3_twal(N4),cblk3_heatflux(N4))
      allocate(cblk4_te(N1),cblk4_tw(N1),cblk4_tn(N5),cblk4_ts(N5),cblk4_twal(N5),cblk4_heatflux(N5))
      !!================================================================================================!!
      !!!region 2
      allocate(fblk1_2ap1(Nfblk1,N2),fblk1_2ae1(Nfblk1,N2),fblk1_2aw1(Nfblk1,N2),fblk1_2an1(Nfblk1,N2),&
            fblk1_2as1(Nfblk1,N2),fblk1_2ap0(Nfblk1,N2),fblk1_2bp(Nfblk1,N2))
      allocate(fblk2_2ap1(N6,N3),fblk2_2ae1(N6,N3),fblk2_2aw1(N6,N3),fblk2_2an1(N6,N3),fblk2_2as1(N6,N3),&
            fblk2_2ap0(N6,N3),fblk2_2bp(N6,N3))
      allocate(fblk3_2ap1(N5,N3),fblk3_2ae1(N5,N3),fblk3_2aw1(N5,N3),fblk3_2an1(N5,N3),fblk3_2as1(N5,N3),&
            fblk3_2ap0(N5,N3),fblk3_2bp(N5,N3))
      allocate(fblk4_2ap1(N6,N4),fblk4_2ae1(N6,N4),fblk4_2aw1(N6,N4),fblk4_2an1(N6,N4),fblk4_2as1(N6,N4),&
            fblk4_2ap0(N6,N4),fblk4_2bp(N6,N4))
      allocate(fblk5_2ap1(N5,N4),fblk5_2ae1(N5,N4),fblk5_2aw1(N5,N4),fblk5_2an1(N5,N4),fblk5_2as1(N5,N4),&
            fblk5_2ap0(N5,N4),fblk5_2bp(N5,N4))
      allocate(fblk6_2ap1(N6,N5),fblk6_2ae1(N6,N5),fblk6_2aw1(N6,N5),fblk6_2an1(N6,N5),fblk6_2as1(N6,N5),&
            fblk6_2ap0(N6,N5),fblk6_2bp(N6,N5))
    
      allocate(cblk1_2ap1(N1,N2),cblk1_2ae1(N1,N2),cblk1_2aw1(N1,N2),cblk1_2an1(N1,N2),cblk1_2as1(N1,N2),&
            cblk1_2ap0(N1,N2),cblk1_2bp(N1,N2))
      allocate(cblk2_2ap1(N1,N3),cblk2_2ae1(N1,N3),cblk2_2aw1(N1,N3),cblk2_2an1(N1,N3),cblk2_2as1(N1,N3),&
            cblk2_2ap0(N1,N3),cblk2_2bp(N1,N3))
      allocate(cblk3_2ap1(N1,N4),cblk3_2ae1(N1,N4),cblk3_2aw1(N1,N4),cblk3_2an1(N1,N4),cblk3_2as1(N1,N4),&
            cblk3_2ap0(N1,N4),cblk3_2bp(N1,N4))
      allocate(cblk4_2ap1(N1,N5),cblk4_2ae1(N1,N5),cblk4_2aw1(N1,N5),cblk4_2an1(N1,N5),cblk4_2as1(N1,N5),&
            cblk4_2ap0(N1,N5),cblk4_2bp(N1,N5))
    
      allocate(fblk1_2kc_HF(Nfblk1,N2),fblk2_2kc_HF(N6,N3),fblk3_2kc_HF(N5,N3),fblk4_2kc_HF(N6,N4),&
            fblk5_2kc_HF(N5,N4),fblk6_2kc_HF(N6,N5))
      allocate(cblk1_2kc_HF(N1,N2),cblk2_2kc_HF(N1,N3),cblk3_2kc_HF(N1,N4),cblk4_2kc_HF(N1,N5))
      allocate(fblk1_2cp_HF(Nfblk1,N2),fblk2_2cp_HF(N6,N3),fblk3_2cp_HF(N5,N3),fblk4_2cp_HF(N6,N4),&
            fblk5_2cp_HF(N5,N4),fblk6_2cp_HF(N6,N5))
      allocate(cblk1_2cp_HF(N1,N2),cblk2_2cp_HF(N1,N3),cblk3_2cp_HF(N1,N4),cblk4_2cp_HF(N1,N5))      
      
      allocate(fblk1_2tsolid(Nfblk1,N2),fblk2_2tsolid(N6,N3),fblk3_2tsolid(N5,N3),fblk4_2tsolid(N6,N4),&
            fblk5_2tsolid(N5,N4),fblk6_2tsolid(N6,N5))
      allocate(fblk1_2tnsolid(Nfblk1,N2),fblk2_2tnsolid(N6,N3),fblk3_2tnsolid(N5,N3),fblk4_2tnsolid(N6,N4),&
            fblk5_2tnsolid(N5,N4),fblk6_2tnsolid(N6,N5))
      allocate(fblk1_2tsolid0(Nfblk1,N2),fblk2_2tsolid0(N6,N3),fblk3_2tsolid0(N5,N3),fblk4_2tsolid0(N6,N4),&
            fblk5_2tsolid0(N5,N4),fblk6_2tsolid0(N6,N5))
      
      allocate(cblk1_2tsolid(N1,N2),cblk2_2tsolid(N1,N3),cblk3_2tsolid(N1,N4),cblk4_2tsolid(N1,N5))
      allocate(cblk1_2tnsolid(N1,N2),cblk2_2tnsolid(N1,N3),cblk3_2tnsolid(N1,N4),cblk4_2tnsolid(N1,N5))
      allocate(cblk1_2tsolid0(N1,N2),cblk2_2tsolid0(N1,N3),cblk3_2tsolid0(N1,N4),cblk4_2tsolid0(N1,N5))
      
      allocate(fblk1_2te(Nfblk1),fblk1_2tw(Nfblk1),fblk1_2tn(N2),fblk1_2ts(N2))
      allocate(fblk2_2te(N6),fblk2_2tw(N6),fblk2_2tn(N3),fblk2_2ts(N3))
      allocate(fblk3_2te(N5),fblk3_2tw(N5),fblk3_2tn(N3),fblk3_2ts(N3))
      allocate(fblk4_2te(N6),fblk4_2tw(N6),fblk4_2tn(N4),fblk4_2ts(N4))
      allocate(fblk5_2te(N5),fblk5_2tw(N5),fblk5_2tn(N4),fblk5_2ts(N4))
      allocate(fblk6_2te(N6),fblk6_2tw(N6),fblk6_2tn(N5),fblk6_2ts(N5))
      
      allocate(cblk1_2te(N1),cblk1_2tw(N1),cblk1_2tn(N2),cblk1_2ts(N2),cblk1_2twal(N2),cblk1_2heatflux(N2))
      allocate(cblk2_2te(N1),cblk2_2tw(N1),cblk2_2tn(N3),cblk2_2ts(N3),cblk2_2twal(N3),cblk2_2heatflux(N3))
      allocate(cblk3_2te(N1),cblk3_2tw(N1),cblk3_2tn(N4),cblk3_2ts(N4),cblk3_2twal(N4),cblk3_2heatflux(N4))
      allocate(cblk4_2te(N1),cblk4_2tw(N1),cblk4_2tn(N5),cblk4_2ts(N5),cblk4_2twal(N5),cblk4_2heatflux(N5))      
      
      allocate(HTC_factor(N2+N3+N4+N5))

      rods(1)%jmax=jmax
       rods(1)%kmax=kmax
       zmax = rods(1)%jmax
       kmax = rods(1)%kmax
       allocate(rods(1)%linear_power(zmax),rods(1)%x(zmax),rods(1)%surf(zmax))
       rods(1)%linear_power=rod_linear_power

      allocate(HFrods(1))
      allocate(HFrods(1)%fblk1_T(Nfblk1,N2,zmax),HFrods(1)%fblk2_T(N6,N3,zmax),HFrods(1)%fblk3_T(N5,N3,zmax),&
      HFrods(1)%fblk4_T(N6,N4,zmax),HFrods(1)%fblk5_T(N5,N4,zmax),HFrods(1)%fblk6_T(N6,N5,zmax))
      allocate(HFrods(1)%fblk1_2T(Nfblk1,N2,zmax),HFrods(1)%fblk2_2T(N6,N3,zmax),HFrods(1)%fblk3_2T(N5,N3,zmax),&
      HFrods(1)%fblk4_2T(N6,N4,zmax),HFrods(1)%fblk5_2T(N5,N4,zmax),HFrods(1)%fblk6_2T(N6,N5,zmax))

      allocate(HFrods(1)%fblk1_Tn(Nfblk1,N2,zmax),HFrods(1)%fblk2_Tn(N6,N3,zmax),HFrods(1)%fblk3_Tn(N5,N3,zmax),&
      HFrods(1)%fblk4_Tn(N6,N4,zmax),HFrods(1)%fblk5_Tn(N5,N4,zmax),HFrods(1)%fblk6_Tn(N6,N5,zmax))
      allocate(HFrods(1)%fblk1_2Tn(Nfblk1,N2,zmax),HFrods(1)%fblk2_2Tn(N6,N3,zmax),HFrods(1)%fblk3_2Tn(N5,N3,zmax),&
      HFrods(1)%fblk4_2Tn(N6,N4,zmax),HFrods(1)%fblk5_2Tn(N5,N4,zmax),HFrods(1)%fblk6_2Tn(N6,N5,zmax))

      allocate(HFrods(1)%cblk1_T(N1+1,N2,zmax),HFrods(1)%cblk2_T(N1,N3,zmax),&
      HFrods(1)%cblk3_T(N1,N4,zmax),HFrods(1)%cblk4_T(N1,N5,zmax))
      allocate(HFrods(1)%cblk1_2T(N1+1,N2,zmax),HFrods(1)%cblk2_2T(N1,N3,zmax),&
      HFrods(1)%cblk3_2T(N1,N4,zmax),HFrods(1)%cblk4_2T(N1,N5,zmax))

      
      allocate(HFrods(1)%cblk1_Tn(N1+1,N2,zmax),HFrods(1)%cblk2_Tn(N1,N3,zmax),&
      HFrods(1)%cblk3_Tn(N1,N4,zmax),HFrods(1)%cblk4_Tn(N1,N5,zmax))
      allocate(HFrods(1)%cblk1_2Tn(N1+1,N2,zmax),HFrods(1)%cblk2_2Tn(N1,N3,zmax),&
      HFrods(1)%cblk3_2Tn(N1,N4,zmax),HFrods(1)%cblk4_2Tn(N1,N5,zmax))
      allocate(HFrods(1)%twall(2*(N2+N3+N4+N5),zmax),HFrods(1)%heatflux(2*(N2+N3+N4+N5),zmax))

      HTC_factor =[0.9087,0.9169,0.9256,0.9348,0.9446,0.9532,0.9585,0.9623,0.9677,0.9791,1.0006,1.0318,&
      1.0420,1.0726,1.0995,1.1208,1.1596,1.2178,1.2701,1.3159,1.3604,1.4014,1.4357,1.4620,1.4814,1.4939,1.4986,1.4958]  
      if (debuging) then
         print *,"  -allocate finished"
        end if 
    do n =1,nrrod
      !!region 1
      fblk1_ap1=0.0; fblk1_ae1=0.0; fblk1_aw1=0.0; fblk1_an1=0.0; fblk1_as1=0.0; fblk1_ap0=0.0; fblk1_bp=0.0
      fblk2_ap1=0.0; fblk2_ae1=0.0; fblk2_aw1=0.0; fblk2_an1=0.0; fblk2_as1=0.0; fblk2_ap0=0.0; fblk2_bp=0.0
      fblk3_ap1=0.0; fblk3_ae1=0.0; fblk3_aw1=0.0; fblk3_an1=0.0; fblk3_as1=0.0; fblk3_ap0=0.0; fblk3_bp=0.0
      fblk4_ap1=0.0; fblk4_ae1=0.0; fblk4_aw1=0.0; fblk4_an1=0.0; fblk4_as1=0.0; fblk4_ap0=0.0; fblk4_bp=0.0
      fblk5_ap1=0.0; fblk5_ae1=0.0; fblk5_aw1=0.0; fblk5_an1=0.0; fblk5_as1=0.0; fblk5_ap0=0.0; fblk5_bp=0.0
      fblk6_ap1=0.0; fblk6_ae1=0.0; fblk6_aw1=0.0; fblk6_an1=0.0; fblk6_as1=0.0; fblk6_ap0=0.0; fblk6_bp=0.0

      cblk1_ap1=0.0; cblk1_ae1=0.0; cblk1_aw1=0.0; cblk1_an1=0.0; cblk1_as1=0.0; cblk1_ap0=0.0; cblk1_bp=0.0
      cblk2_ap1=0.0; cblk2_ae1=0.0; cblk2_aw1=0.0; cblk2_an1=0.0; cblk2_as1=0.0; cblk2_ap0=0.0; cblk2_bp=0.0
      cblk3_ap1=0.0; cblk3_ae1=0.0; cblk3_aw1=0.0; cblk3_an1=0.0; cblk3_as1=0.0; cblk3_ap0=0.0; cblk3_bp=0.0
      cblk4_ap1=0.0; cblk4_ae1=0.0; cblk4_aw1=0.0; cblk4_an1=0.0; cblk4_as1=0.0; cblk4_ap0=0.0; cblk4_bp=0.0
      
      fblk1_te=0.0; fblk1_tw=0.0; fblk1_tn=0.0; fblk1_ts=0.0
      fblk2_te=0.0; fblk2_tw=0.0; fblk2_tn=0.0; fblk2_ts=0.0
      fblk3_te=0.0; fblk3_tw=0.0; fblk3_tn=0.0; fblk3_ts=0.0
      fblk4_te=0.0; fblk4_tw=0.0; fblk4_tn=0.0; fblk4_ts=0.0
      fblk5_te=0.0; fblk5_tw=0.0; fblk5_tn=0.0; fblk5_ts=0.0
      fblk6_te=0.0; fblk6_tw=0.0; fblk6_tn=0.0; fblk6_ts=0.0
      
      cblk1_te=0.0; cblk1_tw=0.0; cblk1_tn=0.0; cblk1_ts=0.0;cblk1_twal=0.0;cblk1_heatflux = 0.0
      cblk2_te=0.0; cblk2_tw=0.0; cblk2_tn=0.0; cblk2_ts=0.0;cblk2_twal=0.0;cblk2_heatflux = 0.0
      cblk3_te=0.0; cblk3_tw=0.0; cblk3_tn=0.0; cblk3_ts=0.0;cblk3_twal=0.0;cblk3_heatflux = 0.0
      cblk4_te=0.0; cblk4_tw=0.0; cblk4_tn=0.0; cblk4_ts=0.0;cblk4_twal=0.0;cblk4_heatflux = 0.0
      
      !! region 2
      fblk1_2ap1=0.0; fblk1_2ae1=0.0; fblk1_2aw1=0.0; fblk1_2an1=0.0; fblk1_2as1=0.0; fblk1_2ap0=0.0; fblk1_2bp=0.0
      fblk2_2ap1=0.0; fblk2_2ae1=0.0; fblk2_2aw1=0.0; fblk2_2an1=0.0; fblk2_2as1=0.0; fblk2_2ap0=0.0; fblk2_2bp=0.0
      fblk3_2ap1=0.0; fblk3_2ae1=0.0; fblk3_2aw1=0.0; fblk3_2an1=0.0; fblk3_2as1=0.0; fblk3_2ap0=0.0; fblk3_2bp=0.0
      fblk4_2ap1=0.0; fblk4_2ae1=0.0; fblk4_2aw1=0.0; fblk4_2an1=0.0; fblk4_2as1=0.0; fblk4_2ap0=0.0; fblk4_2bp=0.0
      fblk5_2ap1=0.0; fblk5_2ae1=0.0; fblk5_2aw1=0.0; fblk5_2an1=0.0; fblk5_2as1=0.0; fblk5_2ap0=0.0; fblk5_2bp=0.0
      fblk6_2ap1=0.0; fblk6_2ae1=0.0; fblk6_2aw1=0.0; fblk6_2an1=0.0; fblk6_2as1=0.0; fblk6_2ap0=0.0; fblk6_2bp=0.0

      cblk1_2ap1=0.0; cblk1_2ae1=0.0; cblk1_2aw1=0.0; cblk1_2an1=0.0; cblk1_2as1=0.0; cblk1_2ap0=0.0; cblk1_2bp=0.0
      cblk2_2ap1=0.0; cblk2_2ae1=0.0; cblk2_2aw1=0.0; cblk2_2an1=0.0; cblk2_2as1=0.0; cblk2_2ap0=0.0; cblk2_2bp=0.0
      cblk3_2ap1=0.0; cblk3_2ae1=0.0; cblk3_2aw1=0.0; cblk3_2an1=0.0; cblk3_2as1=0.0; cblk3_2ap0=0.0; cblk3_2bp=0.0
      cblk4_2ap1=0.0; cblk4_2ae1=0.0; cblk4_2aw1=0.0; cblk4_2an1=0.0; cblk4_2as1=0.0; cblk4_2ap0=0.0; cblk4_2bp=0.0
      
      fblk1_2te=0.0; fblk1_2tw=0.0; fblk1_2tn=0.0; fblk1_2ts=0.0
      fblk2_2te=0.0; fblk2_2tw=0.0; fblk2_2tn=0.0; fblk2_2ts=0.0
      fblk3_2te=0.0; fblk3_2tw=0.0; fblk3_2tn=0.0; fblk3_2ts=0.0
      fblk4_2te=0.0; fblk4_2tw=0.0; fblk4_2tn=0.0; fblk4_2ts=0.0
      fblk5_2te=0.0; fblk5_2tw=0.0; fblk5_2tn=0.0; fblk5_2ts=0.0
      fblk6_2te=0.0; fblk6_2tw=0.0; fblk6_2tn=0.0; fblk6_2ts=0.0
      
      cblk1_2te=0.0; cblk1_2tw=0.0; cblk1_2tn=0.0; cblk1_2ts=0.0;cblk1_2twal=0.0;cblk1_2heatflux = 0.0
      cblk2_2te=0.0; cblk2_2tw=0.0; cblk2_2tn=0.0; cblk2_2ts=0.0;cblk2_2twal=0.0;cblk2_2heatflux = 0.0
      cblk3_2te=0.0; cblk3_2tw=0.0; cblk3_2tn=0.0; cblk3_2ts=0.0;cblk3_2twal=0.0;cblk3_2heatflux = 0.0
      cblk4_2te=0.0; cblk4_2tw=0.0; cblk4_2tn=0.0; cblk4_2ts=0.0;cblk4_2twal=0.0;cblk4_2heatflux = 0.0       
       HFrod => HFrods(n)
       

       !isec = flmesh%Get_jloc_section(jabs = zmax-1)
       !������һʱ�䲽�¶�
       !! Region 1
       HFrod%fblk1_Tn(:,:,:)=HFrod%fblk1_T(:,:,:);HFrod%fblk2_Tn(:,:,:)=HFrod%fblk2_T(:,:,:);
       HFrod%fblk3_Tn(:,:,:)=HFrod%fblk3_T(:,:,:);HFrod%fblk4_Tn(:,:,:)=HFrod%fblk4_T(:,:,:);
       HFrod%fblk5_Tn(:,:,:)=HFrod%fblk5_T(:,:,:);HFrod%fblk6_T(:,:,:)=HFrod%fblk6_T(:,:,:)

       HFrod%cblk1_Tn(:,:,:)=HFrod%cblk1_T(:,:,:);HFrod%cblk2_Tn(:,:,:)=HFrod%cblk2_T(:,:,:);
       HFrod%cblk3_Tn(:,:,:)=HFrod%cblk3_T(:,:,:)
       HFrod%cblk4_Tn(:,:,:)=HFrod%cblk4_T(:,:,:)
       !! Region 2
       HFrod%fblk1_2Tn(:,:,:)=HFrod%fblk1_2T(:,:,:);HFrod%fblk2_2Tn(:,:,:)=HFrod%fblk2_2T(:,:,:);
       HFrod%fblk3_2Tn(:,:,:)=HFrod%fblk3_2T(:,:,:);HFrod%fblk4_2Tn(:,:,:)=HFrod%fblk4_2T(:,:,:);
       HFrod%fblk5_2Tn(:,:,:)=HFrod%fblk5_2T(:,:,:);HFrod%fblk6_2T(:,:,:)=HFrod%fblk6_2T(:,:,:)

       HFrod%cblk1_2Tn(:,:,:)=HFrod%cblk1_2T(:,:,:);HFrod%cblk2_2Tn(:,:,:)=HFrod%cblk2_2T(:,:,:);
       HFrod%cblk3_2Tn(:,:,:)=HFrod%cblk3_2T(:,:,:)
       HFrod%cblk4_2Tn(:,:,:)=HFrod%cblk4_2T(:,:,:)       
       
       
       do z = 1,zmax+1
         if (z>zmax) then
            cycle
         endif

        ! get average fluid temperature connected to this rod
        havg_ch = 0
        favg_ch = 0
        htcavg = 0
        pij = 0
        
        !RFuel = Rcold(matfuel)
        RFuel = 409.8
        RCladding = 409.8
        !if(matclad .ne. 0) RCladding = Rcold(matclad)
        
         !do k = 1, kmax                   
         !  chnum = pin_sc_conn(isec,n,k)
         !  hl = ch(chnum)%hl(z)
         !  favg_ch = favg_ch + ch(chnum)%flm(z)
         !  havg_ch = havg_ch + ch(chnum)%flm(z)*hl
         !  
         !  htc = rods(n)%surf(k)%htcl(z)
         !  htcavg = htcavg + htc
         !  
         !  pij = pij+ch(chnum)%p(z)          
         !end do
         !
         !havg_ch = havg_ch/favg_ch
         !pij = Psfrel2psia(pij/kmax)
         !htcavg = htcavg / kmax          
         ! call liquid_props(h = havg_ch,p=pij,T=tfluid)
        tfluid=330
        !write(*,*) z,htcavg
        
        ! source term
        linear_power = rods(n)%linear_power(z)
        volume_power = linear_power/area_fuel
        !write(*,*) volume_power
               
        HFrod%fuel_source = volume_power
        HFrod%cladding_source = 0.0        
        
        !��ʼ�������¶�
        !! Region 1
        fblk1_tsolid(:,:) = HFrod%fblk1_T(:,:,z);fblk2_tsolid(:,:) = HFrod%fblk2_T(:,:,z)
        fblk3_tsolid(:,:) = HFrod%fblk3_T(:,:,z);fblk4_tsolid(:,:) = HFrod%fblk4_T(:,:,z)
        fblk5_tsolid(:,:) = HFrod%fblk5_T(:,:,z);fblk6_tsolid(:,:) = HFrod%fblk6_T(:,:,z)
        cblk1_tsolid(:,:) = HFrod%cblk1_T(:,:,z);cblk2_tsolid(:,:) = HFrod%cblk2_T(:,:,z)
        cblk3_tsolid(:,:) = HFrod%cblk3_T(:,:,z);cblk4_tsolid(:,:) = HFrod%cblk4_T(:,:,z)

        fblk1_tsolid0(:,:) = HFrod%fblk1_T(:,:,z);fblk2_tsolid0(:,:) = HFrod%fblk2_T(:,:,z)
        fblk3_tsolid0(:,:) = HFrod%fblk3_T(:,:,z);fblk4_tsolid0(:,:) = HFrod%fblk4_T(:,:,z)
        fblk5_tsolid0(:,:) = HFrod%fblk5_T(:,:,z);fblk6_tsolid0(:,:) = HFrod%fblk6_T(:,:,z)
        cblk1_tsolid0(:,:) = HFrod%cblk1_T(:,:,z);cblk2_tsolid0(:,:) = HFrod%cblk2_T(:,:,z)
        cblk3_tsolid0(:,:) = HFrod%cblk3_T(:,:,z);cblk4_tsolid0(:,:) = HFrod%cblk4_T(:,:,z)
        
        !! Region 2
        fblk1_2tsolid(:,:) = HFrod%fblk1_2T(:,:,z);fblk2_2tsolid(:,:) = HFrod%fblk2_2T(:,:,z)
        fblk3_2tsolid(:,:) = HFrod%fblk3_2T(:,:,z);fblk4_2tsolid(:,:) = HFrod%fblk4_2T(:,:,z)
        fblk5_2tsolid(:,:) = HFrod%fblk5_2T(:,:,z);fblk6_2tsolid(:,:) = HFrod%fblk6_2T(:,:,z)
        cblk1_2tsolid(:,:) = HFrod%cblk1_2T(:,:,z);cblk2_2tsolid(:,:) = HFrod%cblk2_2T(:,:,z)
        cblk3_2tsolid(:,:) = HFrod%cblk3_2T(:,:,z);cblk4_2tsolid(:,:) = HFrod%cblk4_2T(:,:,z)

        fblk1_2tsolid0(:,:) = HFrod%fblk1_2T(:,:,z);fblk2_2tsolid0(:,:) = HFrod%fblk2_2T(:,:,z)
        fblk3_2tsolid0(:,:) = HFrod%fblk3_2T(:,:,z);fblk4_2tsolid0(:,:) = HFrod%fblk4_2T(:,:,z)
        fblk5_2tsolid0(:,:) = HFrod%fblk5_2T(:,:,z);fblk6_2tsolid0(:,:) = HFrod%fblk6_2T(:,:,z)
        cblk1_2tsolid0(:,:) = HFrod%cblk1_2T(:,:,z);cblk2_2tsolid0(:,:) = HFrod%cblk2_2T(:,:,z)
        cblk3_2tsolid0(:,:) = HFrod%cblk3_2T(:,:,z);cblk4_2tsolid0(:,:) = HFrod%cblk4_2T(:,:,z)        
        !do row = 1,N1
          !do col = 1,N4 
            !write(*,*) z,row,col,cblk3_tsolid(row,col),HFrod%cblk3_T(row,col,z)
          !enddo
        !enddo                 
        if (debuging) then
         print *,"  -initialize finished"
        end if   
        do iter = 1, itermax
           !�������Բ���
           !Fuel block1
           do row = 1,Nfblk1
              do col = 1,N2
                call get_HF_fuel_props(fblk1_tsolid(row,col),fblk1_kc_HF(row,col),fblk1_cp_HF(row,col))
                call get_HF_fuel_props(fblk1_2tsolid(row,col),fblk1_2kc_HF(row,col),fblk1_2cp_HF(row,col))  !1-2
              end do           
           end do
         
           !Fuel block2
           do row = 1,N6
              do col = 1,N3
                call get_HF_fuel_props(fblk2_tsolid(row,col),fblk2_kc_HF(row,col),fblk2_cp_HF(row,col))
                call get_HF_fuel_props(fblk2_2tsolid(row,col),fblk2_2kc_HF(row,col),fblk2_2cp_HF(row,col))  !2-2
              end do           
           end do        
           !Fuel block3
           do row = 1,N5
              do col = 1,N3
                call get_HF_fuel_props(fblk3_tsolid(row,col),fblk3_kc_HF(row,col),fblk3_cp_HF(row,col))
                call get_HF_fuel_props(fblk3_2tsolid(row,col),fblk3_2kc_HF(row,col),fblk3_2cp_HF(row,col))  !3-2
              end do           
           end do           
           !Fuel block4
           do row = 1,N6
              do col = 1,N4
                call get_HF_fuel_props(fblk4_tsolid(row,col),fblk4_kc_HF(row,col),fblk4_cp_HF(row,col))
                call get_HF_fuel_props(fblk4_2tsolid(row,col),fblk4_2kc_HF(row,col),fblk4_2cp_HF(row,col))  !4-2
              end do           
           end do   
           !Fuel block5
           do row = 1,N5
              do col = 1,N4
                call get_HF_fuel_props(fblk5_tsolid(row,col),fblk5_kc_HF(row,col),fblk5_cp_HF(row,col))
                call get_HF_fuel_props(fblk5_2tsolid(row,col),fblk5_2kc_HF(row,col),fblk5_2cp_HF(row,col))  !5-2
              end do           
           end do          
           !Fuel block6
           do row = 1,N6
              do col = 1,N5
                call get_HF_fuel_props(fblk6_tsolid(row,col),fblk6_kc_HF(row,col),fblk6_cp_HF(row,col))
                call get_HF_fuel_props(fblk6_2tsolid(row,col),fblk6_2kc_HF(row,col),fblk6_2cp_HF(row,col))  !6-2
              end do           
           end do
        
           !Cladding block1
           do row = 1,N1+1
              do col = 1,N2
                call get_HF_clad_props(cblk1_tsolid(row,col),cblk1_kc_HF(row,col),cblk1_cp_HF(row,col))
                call get_HF_clad_props(cblk1_2tsolid(row,col),cblk1_2kc_HF(row,col),cblk1_2cp_HF(row,col))  !1-2
              end do           
           end do          
              !Cladding block2
           do row = 1,N1
              do col = 1,N3
                call get_HF_clad_props(cblk2_tsolid(row,col),cblk2_kc_HF(row,col),cblk2_cp_HF(row,col))
                call get_HF_clad_props(cblk2_2tsolid(row,col),cblk2_2kc_HF(row,col),cblk2_2cp_HF(row,col))  !2-2
              end do           
           end do  
              !Cladding block3
           do row = 1,N1
              do col = 1,N4
                call get_HF_clad_props(cblk3_tsolid(row,col),cblk3_kc_HF(row,col),cblk3_cp_HF(row,col))
                call get_HF_clad_props(cblk3_2tsolid(row,col),cblk3_2kc_HF(row,col),cblk3_2cp_HF(row,col))  !3-2
                !write(*,*) z,iter,"Cblk-3Props",N1,N4,row,col,cblk3_tsolid(row,col),HFrod%cblk3_T(row,col,z)
              end do           
           end do         
              !Cladding block4
           do row = 1,N1
              do col = 1,N5
                call get_HF_clad_props(cblk4_tsolid(row,col),cblk4_kc_HF(row,col),cblk4_cp_HF(row,col))
                call get_HF_clad_props(cblk4_2tsolid(row,col),cblk4_2kc_HF(row,col),cblk4_2cp_HF(row,col))  !4-2
              end do           
           end do
  
           !write(*,*) "Start running get_coef_matrix"
        
           !row = size(blk1_fuel%DsN,1)
           !write(*,*) row
           !ϵ������ֵ���ڲ�����
        
           call get_coef_matrix(Nfblk1,N2,blk1_fuel,RFuel,fblk1_kc_HF,fblk1_cp_HF,fblk1_ae1,fblk1_aw1,fblk1_as1,&
                         fblk1_an1,fblk1_ap0,fblk1_ap1,fblk1_bp,dt_heat,volume_Power)  !fuel block1
           call get_coef_matrix(Nfblk1,N2,blk1_2fuel,RFuel,fblk1_2kc_HF,fblk1_2cp_HF,fblk1_2ae1,fblk1_2aw1,fblk1_2as1,&
                         fblk1_2an1,fblk1_2ap0,fblk1_2ap1,fblk1_2bp,dt_heat,volume_Power)  !fuel block1-2 
           if(z .eq. 1) then
           do row = 1,Nfblk1
             do col = 1, N2
               !write(*,*) iter,row,col,fblk1_2ae1(row,col),fblk1_2aw1(row,col),&
                      !fblk1_2as1(row,col),fblk1_2an1(row,col),fblk1_2bp(row,col)
             enddo
           enddo
           endif          
        
           if(N3>2) then
              call get_coef_matrix(N6,N3,blk2_fuel,RFuel,fblk2_kc_HF,fblk2_cp_HF,fblk2_ae1,fblk2_aw1,fblk2_as1,&
                            fblk2_an1,fblk2_ap0,fblk2_ap1,fblk2_bp,dt_heat,volume_Power)  !fuel block2
              call get_coef_matrix(N6,N3,blk2_2fuel,RFuel,fblk2_2kc_HF,fblk2_2cp_HF,fblk2_2ae1,fblk2_2aw1,fblk2_2as1,&
                            fblk2_2an1,fblk2_2ap0,fblk2_2ap1,fblk2_2bp,dt_heat,volume_Power)  !fuel block2-2
              
              call get_coef_matrix(N5,N3,blk3_fuel,RFuel,fblk3_kc_HF,fblk3_cp_HF,fblk3_ae1,fblk3_aw1,fblk3_as1,&
                            fblk3_an1,fblk3_ap0,fblk3_ap1,fblk3_bp,dt_heat,volume_Power)  !fuel block3
              call get_coef_matrix(N5,N3,blk3_2fuel,RFuel,fblk3_2kc_HF,fblk3_2cp_HF,fblk3_2ae1,fblk3_2aw1,fblk3_2as1,&
                            fblk3_2an1,fblk3_2ap0,fblk3_2ap1,fblk3_2bp,dt_heat,volume_Power)  !fuel block3-2              
           end if
           if(N4>2) then
              call get_coef_matrix(N6,N4,blk4_fuel,RFuel,fblk4_kc_HF,fblk4_cp_HF,fblk4_ae1,fblk4_aw1,fblk4_as1,&
                            fblk4_an1,fblk4_ap0,fblk4_ap1,fblk4_bp,dt_heat,volume_Power)  !fuel block4
              call get_coef_matrix(N6,N4,blk4_2fuel,RFuel,fblk4_2kc_HF,fblk4_2cp_HF,fblk4_2ae1,fblk4_2aw1,fblk4_2as1,&
                            fblk4_2an1,fblk4_2ap0,fblk4_2ap1,fblk4_2bp,dt_heat,volume_Power)  !fuel block4-2
              
              call get_coef_matrix(N5,N4,blk5_fuel,RFuel,fblk5_kc_HF,fblk5_cp_HF,fblk5_ae1,fblk5_aw1,fblk5_as1,&
                            fblk5_an1,fblk5_ap0,fblk5_ap1,fblk5_bp,dt_heat,volume_Power)  !fuel block5
              call get_coef_matrix(N5,N4,blk5_2fuel,RFuel,fblk5_2kc_HF,fblk5_2cp_HF,fblk5_2ae1,fblk5_2aw1,fblk5_2as1,&
                            fblk5_2an1,fblk5_2ap0,fblk5_2ap1,fblk5_2bp,dt_heat,volume_Power)  !fuel block5-2              
           end if
           if (N5>2) then
              call get_coef_matrix(N6,N5,blk6_fuel,RFuel,fblk6_kc_HF,fblk6_cp_HF,fblk6_ae1,fblk6_aw1,fblk6_as1,&
                            fblk6_an1,fblk6_ap0,fblk6_ap1,fblk6_bp,dt_heat,volume_Power)  !fuel block6
              call get_coef_matrix(N6,N5,blk6_2fuel,RFuel,fblk6_2kc_HF,fblk6_2cp_HF,fblk6_2ae1,fblk6_2aw1,fblk6_2as1,&
                            fblk6_2an1,fblk6_2ap0,fblk6_2ap1,fblk6_2bp,dt_heat,volume_Power)  !fuel block6-2              
           end if
           if(N1>2) then
              call get_coef_matrix(N1,N2,blk1_clad,RCladding,cblk1_kc_HF,cblk1_cp_HF,cblk1_ae1,cblk1_aw1,cblk1_as1,&
                            cblk1_an1,cblk1_ap0,cblk1_ap1,cblk1_bp,dt_heat,0.0)  !Cladding block1
              call get_coef_matrix(N1,N3,blk2_clad,RCladding,cblk2_kc_HF,cblk2_cp_HF,cblk2_ae1,cblk2_aw1,cblk2_as1,&
                            cblk2_an1,cblk2_ap0,cblk2_ap1,cblk2_bp,dt_heat,0.0)  !Cladding block2
              call get_coef_matrix(N1,N4,blk3_clad,RCladding,cblk3_kc_HF,cblk3_cp_HF,cblk3_ae1,cblk3_aw1,cblk3_as1,&
                            cblk3_an1,cblk3_ap0,cblk3_ap1,cblk3_bp,dt_heat,0.0)  !Cladding block3
              call get_coef_matrix(N1,N5,blk4_clad,RCladding,cblk4_kc_HF,cblk4_cp_HF,cblk4_ae1,cblk4_aw1,cblk4_as1,&
                            cblk4_an1,cblk4_ap0,cblk4_ap1,cblk4_bp,dt_heat,0.0)  !Cladding block4
              
              call get_coef_matrix(N1,N2,blk1_2clad,RCladding,cblk1_2kc_HF,cblk1_2cp_HF,cblk1_2ae1,cblk1_2aw1,cblk1_2as1,&
                            cblk1_2an1,cblk1_2ap0,cblk1_2ap1,cblk1_2bp,dt_heat,0.0)  !Cladding block1-2
              call get_coef_matrix(N1,N3,blk2_2clad,RCladding,cblk2_2kc_HF,cblk2_2cp_HF,cblk2_2ae1,cblk2_2aw1,cblk2_2as1,&
                            cblk2_2an1,cblk2_2ap0,cblk2_2ap1,cblk2_2bp,dt_heat,0.0)  !Cladding block2-2
              call get_coef_matrix(N1,N4,blk3_2clad,RCladding,cblk3_2kc_HF,cblk3_2cp_HF,cblk3_2ae1,cblk3_2aw1,cblk3_2as1,&
                            cblk3_2an1,cblk3_2ap0,cblk3_2ap1,cblk3_2bp,dt_heat,0.0)  !Cladding block3-2
              call get_coef_matrix(N1,N5,blk4_2clad,RCladding,cblk4_2kc_HF,cblk4_2cp_HF,cblk4_2ae1,cblk4_2aw1,cblk4_2as1,&
                            cblk4_2an1,cblk4_2ap0,cblk4_2ap1,cblk4_2bp,dt_heat,0.0)  !Cladding block4-2              
           end if
        
        
           !!===========================================================�߽�������fuel block1==============================================================================        
           row = 1    !�±߽�           
           do col = 2,N2-1
              !! Region 1
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col+1)/(blk1_fuel%DsE(row,col)*&
                fblk1_kc_HF(row,col+1)+blk1_fuel%DsW(row,col+1)*fblk1_kc_HF(row,col))
              kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
                fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
              ksp = fblk1_kc_HF(row,col)
              knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
                fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
                          
                !ϵ������
              fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
              fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
              fblk1_as1(row,col) = 0.0
              fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
              fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
                            fblk1_an1(row,col)
              fblk1_bp(row,col) =  volume_power*blk1_fuel%area(row,col)
              !! Region 2
              kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col+1)/(blk1_2fuel%DsE(row,col)*&
                fblk1_2kc_HF(row,col+1)+blk1_2fuel%DsW(row,col+1)*fblk1_2kc_HF(row,col))
              kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col-1)/(blk1_2fuel%DsW(row,col)*&
                fblk1_2kc_HF(row,col-1)+blk1_2fuel%DsE(row,col-1)*fblk1_2kc_HF(row,col))
              ksp = fblk1_2kc_HF(row,col)
              knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row+1,col)/(blk1_2fuel%DsN(row,col)*&
                fblk1_2kc_HF(row+1,col)+blk1_2fuel%DsS(row+1,col)*fblk1_2kc_HF(row,col))
                          
                !ϵ������
              fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
              fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
              fblk1_2as1(row,col) = 0.0
              fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
              fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)* blk1_2fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
                            fblk1_2an1(row,col)
              fblk1_2bp(row,col) =  volume_power*blk1_2fuel%area(row,col)              
           end do
        
           col = 1  !Region 1,��߽�;Region 2,�ұ߽�
           do row = 2,Nfblk1-1
              !! Region 1����߽�
              !�ȵ��ʵ���ƽ��ֵ 
              kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col+1)/(blk1_fuel%DsE(row,col)*&
                fblk1_kc_HF(row,col+1)+blk1_fuel%DsW(row,col+1)*fblk1_kc_HF(row,col))
              kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_2kc_HF(row,N2)/(blk1_fuel%DsW(row,col)*&
                fblk1_2kc_HF(row,N2)+blk1_2fuel%DsE(row,N2)*fblk1_kc_HF(row,col))
              ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
                fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
              knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
                fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
              
                !ϵ������
              fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
              fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
              fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
              fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
              fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
              fblk1_an1(row,col)
              fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
              !! Region 2���ұ߽�
              !�ȵ��ʵ���ƽ��ֵ 
              kep = blk1_2fuel%Ds2E(row,N2)*fblk1_2kc_HF(row,N2)*fblk1_kc_HF(row,1)/(blk1_2fuel%DsE(row,N2)*&
                fblk1_kc_HF(row,1)+blk1_fuel%DsW(row,1)*fblk1_2kc_HF(row,N2))
              kwp = blk1_2fuel%Ds2W(row,N2)*fblk1_2kc_HF(row,N2)*fblk1_2kc_HF(row,N2-1)/(blk1_2fuel%DsW(row,N2)*&
                fblk1_2kc_HF(row,N2-1)+blk1_2fuel%DsE(row,N2-1)*fblk1_2kc_HF(row,col))
              ksp = blk1_2fuel%Ds2S(row,N2)*fblk1_2kc_HF(row,N2)*fblk1_2kc_HF(row-1,N2)/(blk1_2fuel%DsS(row,N2)*&
                fblk1_2kc_HF(row-1,N2)+blk1_2fuel%DsN(row-1,N2)*fblk1_2kc_HF(row,N2))
              knp = blk1_2fuel%Ds2N(row,N2)*fblk1_2kc_HF(row,N2)*fblk1_2kc_HF(row+1,N2)/(blk1_2fuel%DsN(row,N2)*&
                fblk1_2kc_HF(row+1,N2)+blk1_2fuel%DsS(row+1,N2)*fblk1_2kc_HF(row,N2))
              
                !ϵ������
              fblk1_2ae1(row,N2) = kep*blk1_2fuel%LsE(row,N2)/blk1_2fuel%Ds2E(row,N2)
              fblk1_2aw1(row,N2) = kwp*blk1_2fuel%LsW(row,N2)/blk1_2fuel%Ds2W(row,N2)
              fblk1_2as1(row,N2) = ksp*blk1_2fuel%LsS(row,N2)/blk1_2fuel%Ds2S(row,N2)
              fblk1_2an1(row,N2) = knp*blk1_2fuel%LsN(row,N2)/blk1_2fuel%Ds2N(row,N2)
              
              fblk1_2ap0(row,N2) = RFuel*fblk1_2cp_HF(row,N2)* blk1_2fuel%area(row,N2)/dt_heat   !ʱ����
              fblk1_2ap1(row,N2) = fblk1_2ap0(row,N2)+fblk1_2ae1(row,N2)+fblk1_2aw1(row,N2)+fblk1_2as1(row,N2)+&
              fblk1_2an1(row,N2)
              fblk1_2bp(row,N2) = volume_power*blk1_2fuel%area(row,N2)              
           end do
        
           row = Nfblk1    !�ϱ߽磬��cladding block1����
           do col = 2,N2-1
              !!Region 1
              !�ȵ��ʵ���ƽ��ֵ 
              kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col+1)/(blk1_fuel%DsE(row,col)*&
                fblk1_kc_HF(row,col+1)+blk1_fuel%DsW(row,col+1)*fblk1_kc_HF(row,col))
              kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
                fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
              ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
                fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
              knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*cblk1_kc_HF(1,col)/(blk1_fuel%DsN(row,col)*&
                cblk1_kc_HF(1,col)+blk1_clad%DsS(1,col)*fblk1_kc_HF(row,col))
              
                !ϵ������
              fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
              fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
              fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
              fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
              fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)*blk1_fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
              fblk1_an1(row,col)
              fblk1_bp(row,col) =  volume_power*blk1_fuel%area(row,col)
              !!Region 2
              !�ȵ��ʵ���ƽ��ֵ 
              kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col+1)/(blk1_2fuel%DsE(row,col)*&
                fblk1_2kc_HF(row,col+1)+blk1_2fuel%DsW(row,col+1)*fblk1_2kc_HF(row,col))
              kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col-1)/(blk1_2fuel%DsW(row,col)*&
                fblk1_2kc_HF(row,col-1)+blk1_2fuel%DsE(row,col-1)*fblk1_2kc_HF(row,col))
              ksp = blk1_2fuel%Ds2S(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row-1,col)/(blk1_2fuel%DsS(row,col)*&
                fblk1_2kc_HF(row-1,col)+blk1_2fuel%DsN(row-1,col)*fblk1_2kc_HF(row,col))
              knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*cblk1_2kc_HF(1,col)/(blk1_2fuel%DsN(row,col)*&
                cblk1_2kc_HF(1,col)+blk1_2clad%DsS(1,col)*fblk1_2kc_HF(row,col))
              
                !ϵ������
              fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
              fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
              fblk1_2as1(row,col) = ksp*blk1_2fuel%LsS(row,col)/blk1_2fuel%Ds2S(row,col)
              fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
              fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)*blk1_2fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
              fblk1_2an1(row,col)
              fblk1_2bp(row,col) =  volume_power*blk1_2fuel%area(row,col)              
              
           end do 
        
           col = N2   !Region 1,�ұ߽�;Region 2,��߽�
           do row = 2,N5   !��fuel block3����
              !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
              kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk3_kc_HF(row,1)/(blk1_fuel%DsE(row,col)*&
                fblk3_kc_HF(row,1)+blk3_fuel%DsW(row,1)*fblk1_kc_HF(row,col))
              kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
                fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
              ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
                fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
              knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
                fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
              
                !ϵ������
              fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
              fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
              fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
              fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
              fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
              fblk1_an1(row,col)
              fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
              !! Region 2,��߽�
              !�ȵ��ʵ���ƽ��ֵ
              kep = blk1_2fuel%Ds2E(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row,2)/(blk1_2fuel%DsE(row,1)*&
                fblk1_2kc_HF(row,2)+blk1_2fuel%DsW(row,2)*fblk1_2kc_HF(row,1))
              kwp = blk1_2fuel%Ds2W(row,1)*fblk1_2kc_HF(row,1)*fblk3_2kc_HF(row,N3)/(blk1_2fuel%DsW(row,1)*&
                fblk3_2kc_HF(row,N3)+blk3_2fuel%DsE(row,N3)*fblk1_2kc_HF(row,1))
              ksp = blk1_2fuel%Ds2S(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row-1,1)/(blk1_2fuel%DsS(row,1)*&
                fblk1_2kc_HF(row-1,1)+blk1_2fuel%DsN(row-1,1)*fblk1_2kc_HF(row,1))
              knp = blk1_2fuel%Ds2N(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row+1,1)/(blk1_2fuel%DsN(row,1)*&
                fblk1_2kc_HF(row+1,1)+blk1_2fuel%DsS(row+1,1)*fblk1_2kc_HF(row,1))
              
                !ϵ������
              fblk1_2ae1(row,1) = kep*blk1_2fuel%LsE(row,1)/blk1_2fuel%Ds2E(row,1)
              fblk1_2aw1(row,1) = kwp*blk1_2fuel%LsW(row,1)/blk1_2fuel%Ds2W(row,1)
              fblk1_2as1(row,1) = ksp*blk1_2fuel%LsS(row,1)/blk1_2fuel%Ds2S(row,1)
              fblk1_2an1(row,1) = knp*blk1_2fuel%LsN(row,1)/blk1_2fuel%Ds2N(row,1)
              
              fblk1_2ap0(row,1) = RFuel*fblk1_2cp_HF(row,1)* blk1_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk1_2ap1(row,1) = fblk1_2ap0(row,1)+fblk1_2ae1(row,1)+fblk1_2aw1(row,1)+fblk1_2as1(row,1)+&
              fblk1_2an1(row,1)
              fblk1_2bp(row,1) = volume_power*blk1_2fuel%area(row,1)                          
           end do
           do row = N5+1,Nfblk1-1   !��fuel block2����
              !! Region 1���ұ߽�
              !�ȵ��ʵ���ƽ��ֵ
              kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk2_kc_HF(row-N5,1)/(blk1_fuel%DsE(row,col)*&
                fblk2_kc_HF(row-N5,1)+blk2_fuel%DsW(row-N5,1)*fblk1_kc_HF(row,col))
              kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
                fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
              ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
                fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
              knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
                fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
              
                !ϵ������
              fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
              fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
              fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
              fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
              fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
              fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
              fblk1_an1(row,col)
              fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
              
              !! Region 2,��߽�
              !�ȵ��ʵ���ƽ��ֵ
              kep = blk1_2fuel%Ds2E(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row,2)/(blk1_2fuel%DsE(row,1)*&
                fblk1_2kc_HF(row,2)+blk1_2fuel%DsW(row,2)*fblk1_2kc_HF(row,1))
              kwp = blk1_2fuel%Ds2W(row,1)*fblk1_2kc_HF(row,1)*fblk2_2kc_HF(row-N5,N3)/(blk1_2fuel%DsW(row,1)*&
                fblk2_2kc_HF(row-N5,N3)+blk2_2fuel%DsE(row-N5,N3)*fblk1_2kc_HF(row,1))
              ksp = blk1_2fuel%Ds2S(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row-1,1)/(blk1_2fuel%DsS(row,1)*&
                fblk1_2kc_HF(row-1,1)+blk1_2fuel%DsN(row-1,1)*fblk1_2kc_HF(row,1))
              knp = blk1_2fuel%Ds2N(row,1)*fblk1_2kc_HF(row,1)*fblk1_2kc_HF(row+1,1)/(blk1_2fuel%DsN(row,1)*&
                fblk1_2kc_HF(row+1,1)+blk1_2fuel%DsS(row+1,1)*fblk1_2kc_HF(row,1))
              !if(z .eq. 1) then
                !write(*,*) iter,row,kwp,blk1_2fuel%Ds2W(row,1),fblk1_2kc_HF(row,1),fblk2_2kc_HF(row,N3),&
                    !blk1_2fuel%DsW(row,1),fblk2_2kc_HF(row,N3)
              !endif                
              
                !ϵ������
              fblk1_2ae1(row,1) = kep*blk1_2fuel%LsE(row,1)/blk1_2fuel%Ds2E(row,1)
              fblk1_2aw1(row,1) = kwp*blk1_2fuel%LsW(row,1)/blk1_2fuel%Ds2W(row,1)
              fblk1_2as1(row,1) = ksp*blk1_2fuel%LsS(row,1)/blk1_2fuel%Ds2S(row,1)
              fblk1_2an1(row,1) = knp*blk1_2fuel%LsN(row,1)/blk1_2fuel%Ds2N(row,1)
              
              fblk1_2ap0(row,1) = RFuel*fblk1_2cp_HF(row,1)* blk1_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk1_2ap1(row,1) = fblk1_2ap0(row,1)+fblk1_2ae1(row,1)+fblk1_2aw1(row,1)+fblk1_2as1(row,1)+&
              fblk1_2an1(row,1)
              fblk1_2bp(row,1) = volume_power*blk1_2fuel%area(row,1)              
           end do
        
           !!!�ĸ�����
           !����
           col = 1
           row = Nfblk1
         !! Region 1
           !�ȵ��ʵ���ƽ��ֵ
           kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col+1)/(blk1_fuel%DsE(row,col)*&
              fblk1_kc_HF(row,col+1)+blk1_fuel%DsW(row,col+1)*fblk1_kc_HF(row,col))
           kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_2kc_HF(row,N2)/(blk1_fuel%DsW(row,col)*&
              fblk1_2kc_HF(row,N2)+blk1_2fuel%DsE(row,N2)*fblk1_kc_HF(row,col))
           ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
              fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
           knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*cblk1_kc_HF(1,col)/(blk1_fuel%DsN(row,col)*&
              cblk1_kc_HF(1,col)+blk1_clad%DsS(1,col)*fblk1_kc_HF(row,col))
              
              !ϵ������
           fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
           fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
           fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
           fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
           fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
           fblk1_an1(row,col)
           fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
           
           !! Region 2
           !�ȵ��ʵ���ƽ��ֵ
           kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col+1)/(blk1_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row,col+1)+blk1_2fuel%DsW(row,col+1)*fblk1_2kc_HF(row,col))
           kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk2_2kc_HF(row-N5,N3)/(blk1_2fuel%DsW(row,col)*&
              fblk2_2kc_HF(row-N5,N3)+blk2_2fuel%DsE(row-N5,N3)*fblk1_2kc_HF(row,col))
           ksp = blk1_2fuel%Ds2S(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row-1,col)/(blk1_2fuel%DsS(row,col)*&
              fblk1_2kc_HF(row-1,col)+blk1_2fuel%DsN(row-1,col)*fblk1_2kc_HF(row,col))
           knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*cblk1_2kc_HF(1,col)/(blk1_2fuel%DsN(row,col)*&
              cblk1_2kc_HF(1,col)+blk1_2clad%DsS(1,col)*fblk1_2kc_HF(row,col))
              
              !ϵ������
           fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
           fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
           fblk1_2as1(row,col) = ksp*blk1_2fuel%LsS(row,col)/blk1_2fuel%Ds2S(row,col)
           fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
           fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)* blk1_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
           fblk1_2an1(row,col)
           fblk1_2bp(row,col) = volume_power*blk1_2fuel%area(row,col)           
           
        
           !����
           col = N2
           row = Nfblk1
           !! Region 1
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk2_kc_HF(row-N5,1)/(blk1_fuel%DsE(row,col)*&
              fblk2_kc_HF(row-N5,1)+blk2_fuel%DsW(row-N5,1)*fblk1_kc_HF(row,col))
           kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
              fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
           ksp = blk1_fuel%Ds2S(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row-1,col)/(blk1_fuel%DsS(row,col)*&
              fblk1_kc_HF(row-1,col)+blk1_fuel%DsN(row-1,col)*fblk1_kc_HF(row,col))
           knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*cblk1_kc_HF(1,col)/(blk1_fuel%DsN(row,col)*&
              cblk1_kc_HF(1,col)+blk1_clad%DsS(1,col)*fblk1_kc_HF(row,col))
              
              !ϵ������
           fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
           fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
           fblk1_as1(row,col) = ksp*blk1_fuel%LsS(row,col)/blk1_fuel%Ds2S(row,col)
           fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
           fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
           fblk1_an1(row,col)
           fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
           
           !! Region 2
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_kc_HF(row,1)/(blk1_2fuel%DsE(row,col)*&
              fblk1_kc_HF(row,1)+blk1_fuel%DsW(row,1)*fblk1_2kc_HF(row,col))
           kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col-1)/(blk1_2fuel%DsW(row,col)*&
              fblk1_2kc_HF(row,col-1)+blk1_2fuel%DsE(row,col-1)*fblk1_2kc_HF(row,col))
           ksp = blk1_2fuel%Ds2S(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row-1,col)/(blk1_2fuel%DsS(row,col)*&
              fblk1_2kc_HF(row-1,col)+blk1_2fuel%DsN(row-1,col)*fblk1_2kc_HF(row,col))
           knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*cblk1_2kc_HF(1,col)/(blk1_2fuel%DsN(row,col)*&
              cblk1_2kc_HF(1,col)+blk1_2clad%DsS(1,col)*fblk1_2kc_HF(row,col))
              
              !ϵ������
           fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
           fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
           fblk1_2as1(row,col) = ksp*blk1_2fuel%LsS(row,col)/blk1_2fuel%Ds2S(row,col)
           fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
           fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)* blk1_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
           fblk1_2an1(row,col)
           fblk1_2bp(row,col) = volume_power*blk1_2fuel%area(row,col)           
        
           !����
           col = 1
           row = 1
           !! Region 1
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col+1)/(blk1_fuel%DsE(row,col)*&
              fblk1_kc_HF(row,col+1)+blk1_fuel%DsW(row,col+1)*fblk1_kc_HF(row,col))
           kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_2kc_HF(row,N2)/(blk1_fuel%DsW(row,col)*&
              fblk1_2kc_HF(row,N2)+blk1_2fuel%DsE(row,N2)*fblk1_kc_HF(row,col))
           ksp = fblk1_kc_HF(row,col)
           knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
              fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
              
              !ϵ������
           fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
           fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
           fblk1_as1(row,col) = 0.0
           fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
           fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
           fblk1_an1(row,col)
           fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)

           !! Region 2
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col+1)/(blk1_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row,col+1)+blk1_2fuel%DsW(row,col+1)*fblk1_2kc_HF(row,col))
           kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk3_2kc_HF(row,N3)/(blk1_2fuel%DsW(row,col)*&
              fblk3_2kc_HF(row,N3)+blk3_2fuel%DsE(row,N3)*fblk1_2kc_HF(row,col))
           ksp = fblk1_2kc_HF(row,col)
           knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row+1,col)/(blk1_2fuel%DsN(row,col)*&
              fblk1_2kc_HF(row+1,col)+blk1_2fuel%DsS(row+1,col)*fblk1_2kc_HF(row,col))
              
              !ϵ������
           fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
           fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
           fblk1_2as1(row,col) = 0.0
           fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
           fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)* blk1_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
           fblk1_2an1(row,col)
           fblk1_2bp(row,col) = volume_power*blk1_2fuel%area(row,col)
           
           !����
           col = N2
           row = 1
           !! Region 1
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_fuel%Ds2E(row,col)*fblk1_kc_HF(row,col)*fblk3_kc_HF(row,1)/(blk1_fuel%DsE(row,col)*&
              fblk3_kc_HF(row,1)+blk3_fuel%DsW(row,1)*fblk1_kc_HF(row,col))
           kwp = blk1_fuel%Ds2W(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row,col-1)/(blk1_fuel%DsW(row,col)*&
              fblk1_kc_HF(row,col-1)+blk1_fuel%DsE(row,col-1)*fblk1_kc_HF(row,col))
           ksp = fblk1_kc_HF(row,col)
           knp = blk1_fuel%Ds2N(row,col)*fblk1_kc_HF(row,col)*fblk1_kc_HF(row+1,col)/(blk1_fuel%DsN(row,col)*&
              fblk1_kc_HF(row+1,col)+blk1_fuel%DsS(row+1,col)*fblk1_kc_HF(row,col))
              
              !ϵ������
           fblk1_ae1(row,col) = kep*blk1_fuel%LsE(row,col)/blk1_fuel%Ds2E(row,col)
           fblk1_aw1(row,col) = kwp*blk1_fuel%LsW(row,col)/blk1_fuel%Ds2W(row,col)
           fblk1_as1(row,col) = 0.0
           fblk1_an1(row,col) = knp*blk1_fuel%LsN(row,col)/blk1_fuel%Ds2N(row,col)
              
           fblk1_ap0(row,col) = RFuel*fblk1_cp_HF(row,col)* blk1_fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_ap1(row,col) = fblk1_ap0(row,col)+fblk1_ae1(row,col)+fblk1_aw1(row,col)+fblk1_as1(row,col)+&
           fblk1_an1(row,col)
           fblk1_bp(row,col) = volume_power*blk1_fuel%area(row,col)
           
           !! Region 2
           !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk1_2fuel%Ds2E(row,col)*fblk1_2kc_HF(row,col)*fblk1_kc_HF(row,1)/(blk1_2fuel%DsE(row,col)*&
              fblk1_kc_HF(row,1)+blk1_fuel%DsW(row,1)*fblk1_2kc_HF(row,col))
           kwp = blk1_2fuel%Ds2W(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row,col-1)/(blk1_2fuel%DsW(row,col)*&
              fblk1_2kc_HF(row,col-1)+blk1_2fuel%DsE(row,col-1)*fblk1_2kc_HF(row,col))
           ksp = fblk1_2kc_HF(row,col)
           knp = blk1_2fuel%Ds2N(row,col)*fblk1_2kc_HF(row,col)*fblk1_2kc_HF(row+1,col)/(blk1_2fuel%DsN(row,col)*&
              fblk1_2kc_HF(row+1,col)+blk1_2fuel%DsS(row+1,col)*fblk1_2kc_HF(row,col))
              
              !ϵ������
           fblk1_2ae1(row,col) = kep*blk1_2fuel%LsE(row,col)/blk1_2fuel%Ds2E(row,col)
           fblk1_2aw1(row,col) = kwp*blk1_2fuel%LsW(row,col)/blk1_2fuel%Ds2W(row,col)
           fblk1_2as1(row,col) = 0.0
           fblk1_2an1(row,col) = knp*blk1_2fuel%LsN(row,col)/blk1_2fuel%Ds2N(row,col)
              
           fblk1_2ap0(row,col) = RFuel*fblk1_2cp_HF(row,col)* blk1_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk1_2ap1(row,col) = fblk1_2ap0(row,col)+fblk1_2ae1(row,col)+fblk1_2aw1(row,col)+fblk1_2as1(row,col)+&
           fblk1_2an1(row,col)
           fblk1_2bp(row,col) = volume_power*blk1_fuel%area(row,col)            
        
           !!===========================================================�߽�������fuel block2==============================================================================          

           if (N3>2) then
              row = 1    !�±߽磬��fuel block3����
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ ,�����غ�
                kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col+1)/(blk2_fuel%DsE(row,col)*&
                   fblk2_kc_HF(row,col+1)+blk2_fuel%DsW(row,col+1)*fblk2_kc_HF(row,col))
                kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col-1)/(blk2_fuel%DsW(row,col)*&
                   fblk2_kc_HF(row,col-1)+blk2_fuel%DsE(row,col-1)*fblk2_kc_HF(row,col))
                ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk3_kc_HF(N5,col)/(blk2_fuel%DsS(row,col)*&
                   fblk3_kc_HF(N5,col)+blk3_fuel%DsN(N5,col)*fblk2_kc_HF(row,col))
                knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row+1,col)/(blk2_fuel%DsN(row,col)*&
                   fblk2_kc_HF(row+1,col)+blk2_fuel%DsS(row+1,col)*fblk2_kc_HF(row,col))
              
                   !ϵ������
                fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
                fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
                fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
                fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
                fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
                fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                              fblk2_an1(row,col)
                fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ ,�����غ�
                kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col+1)/(blk2_2fuel%DsE(row,col)*&
                   fblk2_2kc_HF(row,col+1)+blk2_2fuel%DsW(row,col+1)*fblk2_2kc_HF(row,col))
                kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col-1)/(blk2_2fuel%DsW(row,col)*&
                   fblk2_2kc_HF(row,col-1)+blk2_2fuel%DsE(row,col-1)*fblk2_2kc_HF(row,col))
                ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk3_2kc_HF(N5,col)/(blk2_2fuel%DsS(row,col)*&
                   fblk3_2kc_HF(N5,col)+blk3_2fuel%DsN(N5,col)*fblk2_2kc_HF(row,col))
                knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row+1,col)/(blk2_2fuel%DsN(row,col)*&
                   fblk2_2kc_HF(row+1,col)+blk2_2fuel%DsS(row+1,col)*fblk2_2kc_HF(row,col))
              
                   !ϵ������
                fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
                fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
                fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
                fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
                fblk2_2ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
                fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                              fblk2_2an1(row,col)
                fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)                
              end do
           
              row = N6  !�ϱ߽磬��cladding block2����
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ
                kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col+1)/(blk2_fuel%DsE(row,col)*&
                   fblk2_kc_HF(row,col+1)+blk2_fuel%DsW(row,col+1)*fblk2_kc_HF(row,col))
                kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col-1)/(blk2_fuel%DsW(row,col)*&
                   fblk2_kc_HF(row,col-1)+blk2_fuel%DsE(row,col-1)*fblk2_kc_HF(row,col))
                ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row-1,col)/(blk2_fuel%DsS(row,col)*&
                   fblk2_kc_HF(row-1,col)+blk2_fuel%DsN(row-1,col)*fblk2_kc_HF(row,col))
                knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*cblk2_kc_HF(1,col)/(blk2_fuel%DsN(row,col)*&
                   cblk2_kc_HF(1,col)+blk2_clad%DsS(1,col)*fblk2_kc_HF(row,col))
              
                   !ϵ������
                fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
                fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
                fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
                fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
                fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
                fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                              fblk2_an1(row,col)
                fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col) 
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ
                kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col+1)/(blk2_2fuel%DsE(row,col)*&
                   fblk2_2kc_HF(row,col+1)+blk2_2fuel%DsW(row,col+1)*fblk2_2kc_HF(row,col))
                kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col-1)/(blk2_2fuel%DsW(row,col)*&
                   fblk2_2kc_HF(row,col-1)+blk2_2fuel%DsE(row,col-1)*fblk2_2kc_HF(row,col))
                ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row-1,col)/(blk2_2fuel%DsS(row,col)*&
                   fblk2_2kc_HF(row-1,col)+blk2_2fuel%DsN(row-1,col)*fblk2_2kc_HF(row,col))
                knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*cblk2_2kc_HF(1,col)/(blk2_2fuel%DsN(row,col)*&
                   cblk2_2kc_HF(1,col)+blk2_2clad%DsS(1,col)*fblk2_2kc_HF(row,col))
              
                   !ϵ������
                fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
                fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
                fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
                fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
                fblk2_2ap0(row,col) = RFuel*fblk2_2cp_HF(row,col)* blk2_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                              fblk2_2an1(row,col)
                fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)                 
              end do           
           end if
        
           col = 1  !��fuel block1����
           do row = 2,N6-1
              !! Region 1,��߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col+1)/(blk2_fuel%DsE(row,col)*&
                fblk2_kc_HF(row,col+1)+blk2_fuel%DsW(row,col+1)*fblk2_kc_HF(row,col))
              kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk1_kc_HF(row+N5,N2)/(blk2_fuel%DsW(row,col)*&
                fblk1_kc_HF(row+N5,N2)+blk1_fuel%DsE(row+N5,N2)*fblk2_kc_HF(row,col))
              ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row-1,col)/(blk2_fuel%DsS(row,col)*&
                fblk2_kc_HF(row-1,col)+blk2_fuel%DsN(row-1,col)*fblk2_kc_HF(row,col))
              knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row+1,col)/(blk2_fuel%DsN(row,col)*&
                fblk2_kc_HF(row+1,col)+blk2_fuel%DsS(row+1,col)*fblk2_kc_HF(row,col))
              
                !ϵ������
              fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
              fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
              fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
              fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
              fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
              fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                            fblk2_an1(row,col)
              fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)
              
              !! Region 2,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk2_2fuel%Ds2E(row,N3)*fblk2_2kc_HF(row,N3)*fblk1_2kc_HF(row+N5,1)/(blk2_2fuel%DsE(row,N3)*&
                fblk1_2kc_HF(row+N5,1)+blk1_2fuel%DsW(row+N5,1)*fblk2_2kc_HF(row,N3))
              kwp = blk2_2fuel%Ds2W(row,N3)*fblk2_2kc_HF(row,N3)*fblk2_2kc_HF(row,N3-1)/(blk2_2fuel%DsW(row,N3)*&
                fblk2_2kc_HF(row,N3-1)+blk2_2fuel%DsE(row,N3-1)*fblk2_2kc_HF(row,N3))
              ksp = blk2_2fuel%Ds2S(row,N3)*fblk2_2kc_HF(row,N3)*fblk2_2kc_HF(row-1,N3)/(blk2_2fuel%DsS(row,N3)*&
                fblk2_2kc_HF(row-1,N3)+blk2_2fuel%DsN(row-1,N3)*fblk2_2kc_HF(row,N3))
              knp = blk2_2fuel%Ds2N(row,N3)*fblk2_2kc_HF(row,N3)*fblk2_2kc_HF(row+1,N3)/(blk2_2fuel%DsN(row,N3)*&
                fblk2_2kc_HF(row+1,N3)+blk2_2fuel%DsS(row+1,N3)*fblk2_2kc_HF(row,N3))
              
                !ϵ������
              fblk2_2ae1(row,N3) = kep*blk2_2fuel%LsE(row,N3)/blk2_2fuel%Ds2E(row,N3)
              fblk2_2aw1(row,N3) = kwp*blk2_2fuel%LsW(row,N3)/blk2_2fuel%Ds2W(row,N3)
              fblk2_2as1(row,N3) = ksp*blk2_2fuel%LsS(row,N3)/blk2_2fuel%Ds2S(row,N3)
              fblk2_2an1(row,N3) = knp*blk2_2fuel%LsN(row,N3)/blk2_2fuel%Ds2N(row,N3)
              
              fblk2_2ap0(row,N3) = RFuel*fblk2_2cp_HF(row,N3)* blk2_2fuel%area(row,N3)/dt_heat   !ʱ����
              fblk2_2ap1(row,N3) = fblk2_2ap0(row,N3)+fblk2_2ae1(row,N3)+fblk2_2aw1(row,N3)+fblk2_2as1(row,N3)+&
                            fblk2_2an1(row,N3)
              fblk2_2bp(row,N3) = volume_power*blk2_2fuel%area(row,N3)
              
           end do           

           col = N3  !��fuel block4����
           do row = 2,N6-1
              !! Region 1,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk4_kc_HF(row,1)/(blk2_fuel%DsE(row,col)*&
                fblk4_kc_HF(row,1)+blk4_fuel%DsW(row,1)*fblk2_kc_HF(row,col))
              kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col-1)/(blk2_fuel%DsW(row,col)*&
                fblk2_kc_HF(row,col-1)+blk2_fuel%DsE(row,col-1)*fblk2_kc_HF(row,col))
              ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row-1,col)/(blk2_fuel%DsS(row,col)*&
                fblk2_kc_HF(row-1,col)+blk2_fuel%DsN(row-1,col)*fblk2_kc_HF(row,col))
              knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row+1,col)/(blk2_fuel%DsN(row,col)*&
                fblk2_kc_HF(row+1,col)+blk2_fuel%DsS(row+1,col)*fblk2_kc_HF(row,col))
              
                !ϵ������
              fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
              fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
              fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
              fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
              fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
              fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                            fblk2_an1(row,col)
              fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)
              
              !! Region 2,��߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk2_2fuel%Ds2E(row,1)*fblk2_2kc_HF(row,1)*fblk2_2kc_HF(row,2)/(blk2_2fuel%DsE(row,1)*&
                fblk2_2kc_HF(row,2)+blk2_2fuel%DsW(row,2)*fblk2_2kc_HF(row,1))
              kwp = blk2_2fuel%Ds2W(row,1)*fblk2_2kc_HF(row,1)*fblk4_2kc_HF(row,N4)/(blk2_2fuel%DsW(row,1)*&
                fblk4_2kc_HF(row,N4)+blk4_2fuel%DsE(row,N4)*fblk2_2kc_HF(row,1))
              ksp = blk2_2fuel%Ds2S(row,1)*fblk2_2kc_HF(row,1)*fblk2_2kc_HF(row-1,1)/(blk2_2fuel%DsS(row,1)*&
                fblk2_2kc_HF(row-1,1)+blk2_2fuel%DsN(row-1,1)*fblk2_2kc_HF(row,1))
              knp = blk2_2fuel%Ds2N(row,1)*fblk2_2kc_HF(row,1)*fblk2_2kc_HF(row+1,1)/(blk2_2fuel%DsN(row,1)*&
                fblk2_2kc_HF(row+1,1)+blk2_2fuel%DsS(row+1,1)*fblk2_2kc_HF(row,1))
              
                !ϵ������
              fblk2_2ae1(row,1) = kep*blk2_2fuel%LsE(row,1)/blk2_2fuel%Ds2E(row,1)
              fblk2_2aw1(row,1) = kwp*blk2_2fuel%LsW(row,1)/blk2_2fuel%Ds2W(row,1)
              fblk2_2as1(row,1) = ksp*blk2_2fuel%LsS(row,1)/blk2_2fuel%Ds2S(row,1)
              fblk2_2an1(row,1) = knp*blk2_2fuel%LsN(row,1)/blk2_2fuel%Ds2N(row,1)
              
              fblk2_2ap0(row,1) = RFuel*fblk2_2cp_HF(row,1)* blk2_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk2_2ap1(row,1) = fblk2_2ap0(row,1)+fblk2_2ae1(row,1)+fblk2_2aw1(row,1)+fblk2_2as1(row,1)+&
                            fblk2_2an1(row,1)
              fblk2_2bp(row,1) = volume_power*blk2_2fuel%area(row,1)              
           end do         
        
           !!!!!�ĸ�����
           !����
           col = 1 
           row = N6
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col+1)/(blk2_fuel%DsE(row,col)*&
              fblk2_kc_HF(row,col+1)+blk2_fuel%DsW(row,col+1)*fblk2_kc_HF(row,col))
           kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk1_kc_HF(row+N5,N2)/(blk2_fuel%DsW(row,col)*&
              fblk1_kc_HF(row+N5,N2)+blk1_fuel%DsE(row+N5,N2)*fblk2_kc_HF(row,col))
           ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row-1,col)/(blk2_fuel%DsS(row,col)*&
              fblk2_kc_HF(row-1,col)+blk2_fuel%DsN(row-1,col)*fblk2_kc_HF(row,col))
           knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*cblk2_kc_HF(1,col)/(blk2_fuel%DsN(row,col)*&
              cblk2_kc_HF(1,col)+blk2_clad%DsS(1,col)*fblk2_kc_HF(row,col))
              
              !ϵ������
           fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
           fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
           fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
           fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
           fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                         fblk2_an1(row,col)
           fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)                    
 
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col+1)/(blk2_2fuel%DsE(row,col)*&
              fblk2_2kc_HF(row,col+1)+blk2_2fuel%DsW(row,col+1)*fblk2_2kc_HF(row,col))
           kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk4_2kc_HF(row,N4)/(blk2_2fuel%DsW(row,col)*&
              fblk4_2kc_HF(row,N4)+blk4_2fuel%DsE(row,N4)*fblk2_2kc_HF(row,col))
           ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row-1,col)/(blk2_2fuel%DsS(row,col)*&
              fblk2_2kc_HF(row-1,col)+blk2_2fuel%DsN(row-1,col)*fblk2_2kc_HF(row,col))
           knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*cblk2_2kc_HF(1,col)/(blk2_2fuel%DsN(row,col)*&
              cblk2_2kc_HF(1,col)+blk2_2clad%DsS(1,col)*fblk2_2kc_HF(row,col))
              
              !ϵ������
           fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
           fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
           fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
           fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
           fblk2_2ap0(row,col) = RFuel*fblk2_2cp_HF(row,col)* blk2_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                         fblk2_2an1(row,col)
           fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)
           
           !����
           col = 1 
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col+1)/(blk2_fuel%DsE(row,col)*&
              fblk2_kc_HF(row,col+1)+blk2_fuel%DsW(row,col+1)*fblk2_kc_HF(row,col))
           kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk1_kc_HF(row+N5,N2)/(blk2_fuel%DsW(row,col)*&
              fblk1_kc_HF(row+N5,N2)+blk1_fuel%DsE(row+N5,N2)*fblk2_kc_HF(row,col))
           ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk3_kc_HF(N5,col)/(blk2_fuel%DsS(row,col)*&
              fblk3_kc_HF(N5,col)+blk3_fuel%DsN(N5,col)*fblk2_kc_HF(row,col))
           knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row+1,col)/(blk2_fuel%DsN(row,col)*&
              fblk2_kc_HF(row+1,col)+blk2_fuel%DsS(row+1,col)*fblk2_kc_HF(row,col))
              
              !ϵ������
           fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
           fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
           fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
           fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
           fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                         fblk2_an1(row,col)
           fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col+1)/(blk2_2fuel%DsE(row,col)*&
              fblk2_2kc_HF(row,col+1)+blk2_2fuel%DsW(row,col+1)*fblk2_2kc_HF(row,col))
           kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk4_2kc_HF(row,N4)/(blk2_2fuel%DsW(row,col)*&
              fblk4_2kc_HF(row,N4)+blk4_2fuel%DsE(row,N4)*fblk2_2kc_HF(row,col))
           ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk3_2kc_HF(N5,col)/(blk2_2fuel%DsS(row,col)*&
              fblk3_2kc_HF(N5,col)+blk3_2fuel%DsN(N5,col)*fblk2_2kc_HF(row,col))
           knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row+1,col)/(blk2_2fuel%DsN(row,col)*&
              fblk2_2kc_HF(row+1,col)+blk2_2fuel%DsS(row+1,col)*fblk2_2kc_HF(row,col))
              
              !ϵ������
           fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
           fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
           fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
           fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
           fblk2_2ap0(row,col) = RFuel*fblk2_2cp_HF(row,col)* blk2_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                         fblk2_2an1(row,col)
           fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)            
        
           !����
           col = N3
           row = N6
           !Region 1
              !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk4_kc_HF(row,1)/(blk2_fuel%DsE(row,col)*&
              fblk4_kc_HF(row,1)+blk4_fuel%DsW(row,1)*fblk2_kc_HF(row,col))
           kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col-1)/(blk2_fuel%DsW(row,col)*&
              fblk2_kc_HF(row,col-1)+blk2_fuel%DsE(row,col-1)*fblk2_kc_HF(row,col))
           ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row-1,col)/(blk2_fuel%DsS(row,col)*&
              fblk2_kc_HF(row-1,col)+blk2_fuel%DsN(row-1,col)*fblk2_kc_HF(row,col))
           knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*cblk2_kc_HF(1,col)/(blk2_fuel%DsN(row,col)*&
              cblk2_kc_HF(1,col)+blk2_clad%DsS(1,col)*fblk2_kc_HF(row,col))
              
              !ϵ������
           fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
           fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
           fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
           fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
           fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                         fblk2_an1(row,col)
           fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col) 
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ ,�����غ�
           kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk1_2kc_HF(row+N5,1)/(blk2_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row+N5,1)+blk1_2fuel%DsW(row+N5,1)*fblk2_2kc_HF(row,col))
           kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col-1)/(blk2_2fuel%DsW(row,col)*&
              fblk2_2kc_HF(row,col-1)+blk2_2fuel%DsE(row,col-1)*fblk2_2kc_HF(row,col))
           ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row-1,col)/(blk2_2fuel%DsS(row,col)*&
              fblk2_2kc_HF(row-1,col)+blk2_2fuel%DsN(row-1,col)*fblk2_2kc_HF(row,col))
           knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*cblk2_2kc_HF(1,col)/(blk2_2fuel%DsN(row,col)*&
              cblk2_2kc_HF(1,col)+blk2_2clad%DsS(1,col)*fblk2_2kc_HF(row,col))
              
              !ϵ������
           fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
           fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
           fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
           fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
           fblk2_2ap0(row,col) = RFuel*fblk2_2cp_HF(row,col)* blk2_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                         fblk2_2an1(row,col)
           fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)
           
           !����
           col = N3
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk2_fuel%Ds2E(row,col)*fblk2_kc_HF(row,col)*fblk4_kc_HF(row,1)/(blk2_fuel%DsE(row,col)*&
              fblk4_kc_HF(row,1)+blk4_fuel%DsW(row,1)*fblk2_kc_HF(row,col))
           kwp = blk2_fuel%Ds2W(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row,col-1)/(blk2_fuel%DsW(row,col)*&
              fblk2_kc_HF(row,col-1)+blk2_fuel%DsE(row,col-1)*fblk2_kc_HF(row,col))
           ksp = blk2_fuel%Ds2S(row,col)*fblk2_kc_HF(row,col)*fblk3_kc_HF(N5,col)/(blk2_fuel%DsS(row,col)*&
              fblk3_kc_HF(N5,col)+blk3_fuel%DsN(N5,col)*fblk2_kc_HF(row,col))
           knp = blk2_fuel%Ds2N(row,col)*fblk2_kc_HF(row,col)*fblk2_kc_HF(row+1,col)/(blk2_fuel%DsN(row,col)*&
              fblk2_kc_HF(row+1,col)+blk2_fuel%DsS(row+1,col)*fblk2_kc_HF(row,col))
              
              !ϵ������
           fblk2_ae1(row,col) = kep*blk2_fuel%LsE(row,col)/blk2_fuel%Ds2E(row,col)
           fblk2_aw1(row,col) = kwp*blk2_fuel%LsW(row,col)/blk2_fuel%Ds2W(row,col)
           fblk2_as1(row,col) = ksp*blk2_fuel%LsS(row,col)/blk2_fuel%Ds2S(row,col)
           fblk2_an1(row,col) = knp*blk2_fuel%LsN(row,col)/blk2_fuel%Ds2N(row,col)
              
           fblk2_ap0(row,col) = RFuel*fblk2_cp_HF(row,col)* blk2_fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_ap1(row,col) = fblk2_ap0(row,col)+fblk2_ae1(row,col)+fblk2_aw1(row,col)+fblk2_as1(row,col)+&
                         fblk2_an1(row,col)
           fblk2_bp(row,col) = volume_power*blk2_fuel%area(row,col)          
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk2_2fuel%Ds2E(row,col)*fblk2_2kc_HF(row,col)*fblk1_2kc_HF(row+N5,1)/(blk2_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row+N5,1)+blk1_2fuel%DsW(row+N5,1)*fblk2_2kc_HF(row,col))
           kwp = blk2_2fuel%Ds2W(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row,col-1)/(blk2_2fuel%DsW(row,col)*&
              fblk2_2kc_HF(row,col-1)+blk2_2fuel%DsE(row,col-1)*fblk2_2kc_HF(row,col))
           ksp = blk2_2fuel%Ds2S(row,col)*fblk2_2kc_HF(row,col)*fblk3_2kc_HF(N5,col)/(blk2_2fuel%DsS(row,col)*&
              fblk3_2kc_HF(N5,col)+blk3_2fuel%DsN(N5,col)*fblk2_2kc_HF(row,col))
           knp = blk2_2fuel%Ds2N(row,col)*fblk2_2kc_HF(row,col)*fblk2_2kc_HF(row+1,col)/(blk2_2fuel%DsN(row,col)*&
              fblk2_2kc_HF(row+1,col)+blk2_2fuel%DsS(row+1,col)*fblk2_2kc_HF(row,col))
              
              !ϵ������
           fblk2_2ae1(row,col) = kep*blk2_2fuel%LsE(row,col)/blk2_2fuel%Ds2E(row,col)
           fblk2_2aw1(row,col) = kwp*blk2_2fuel%LsW(row,col)/blk2_2fuel%Ds2W(row,col)
           fblk2_2as1(row,col) = ksp*blk2_2fuel%LsS(row,col)/blk2_2fuel%Ds2S(row,col)
           fblk2_2an1(row,col) = knp*blk2_2fuel%LsN(row,col)/blk2_2fuel%Ds2N(row,col)
              
           fblk2_2ap0(row,col) = RFuel*fblk2_2cp_HF(row,col)* blk2_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk2_2ap1(row,col) = fblk2_2ap0(row,col)+fblk2_2ae1(row,col)+fblk2_2aw1(row,col)+fblk2_2as1(row,col)+&
                         fblk2_2an1(row,col)
           fblk2_2bp(row,col) = volume_power*blk2_2fuel%area(row,col)
        
          !!===========================================================�߽�������fuel block3==============================================================================          

           if (N3>2) then
              row = 1    !�±߽磬���ȱ߽�
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col+1)/(blk3_fuel%DsE(row,col)*&
                   fblk3_kc_HF(row,col+1)+blk3_fuel%DsW(row,col+1)*fblk3_kc_HF(row,col))
                kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col-1)/(blk3_fuel%DsW(row,col)*&
                   fblk3_kc_HF(row,col-1)+blk3_fuel%DsE(row,col-1)*fblk3_kc_HF(row,col))
                ksp = fblk3_kc_HF(row,col)
                knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row+1,col)/(blk3_fuel%DsN(row,col)*&
                   fblk3_kc_HF(row+1,col)+blk3_fuel%DsS(row+1,col)*fblk3_kc_HF(row,col))
              
                   !ϵ������
                fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
                fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
                fblk3_as1(row,col) = 0.0
                fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
                fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)* blk3_fuel%area(row,col)/dt_heat   !ʱ����
                fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                              fblk3_an1(row,col)
                fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)

                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col+1)/(blk3_2fuel%DsE(row,col)*&
                   fblk3_2kc_HF(row,col+1)+blk3_2fuel%DsW(row,col+1)*fblk3_2kc_HF(row,col))
                kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col-1)/(blk3_2fuel%DsW(row,col)*&
                   fblk3_2kc_HF(row,col-1)+blk3_2fuel%DsE(row,col-1)*fblk3_2kc_HF(row,col))
                ksp = fblk3_2kc_HF(row,col)
                knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row+1,col)/(blk3_2fuel%DsN(row,col)*&
                   fblk3_2kc_HF(row+1,col)+blk3_2fuel%DsS(row+1,col)*fblk3_2kc_HF(row,col))
              
                   !ϵ������
                fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
                fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
                fblk3_2as1(row,col) = 0.0
                fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
                fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)* blk3_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                              fblk3_2an1(row,col)
                fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)                
              end do

              row = N5    !�ϱ߽磬��fuel block2����
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ
                kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col+1)/(blk3_fuel%DsE(row,col)*&
                   fblk3_kc_HF(row,col+1)+blk3_fuel%DsW(row,col+1)*fblk3_kc_HF(row,col))
                kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col-1)/(blk3_fuel%DsW(row,col)*&
                   fblk3_kc_HF(row,col-1)+blk3_fuel%DsE(row,col-1)*fblk3_kc_HF(row,col))
                ksp = blk3_fuel%Ds2S(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row-1,col)/(blk3_fuel%DsS(row,col)*&
                   fblk3_kc_HF(row-1,col)+blk3_fuel%DsN(row-1,col)*fblk3_kc_HF(row,col))
                knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk2_kc_HF(1,col)/(blk3_fuel%DsN(row,col)*&
                   fblk2_kc_HF(1,col)+blk2_fuel%DsS(1,col)*fblk3_kc_HF(row,col))
              
                   !ϵ������
                fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
                fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
                fblk3_as1(row,col) = ksp*blk3_fuel%LsS(row,col)/blk3_fuel%Ds2S(row,col)
                fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
                fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)* blk3_fuel%area(row,col)/dt_heat   !ʱ����
                fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                              fblk3_an1(row,col)
                fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ
                kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col+1)/(blk3_2fuel%DsE(row,col)*&
                   fblk3_2kc_HF(row,col+1)+blk3_2fuel%DsW(row,col+1)*fblk3_2kc_HF(row,col))
                kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col-1)/(blk3_2fuel%DsW(row,col)*&
                   fblk3_2kc_HF(row,col-1)+blk3_2fuel%DsE(row,col-1)*fblk3_2kc_HF(row,col))
                ksp = blk3_2fuel%Ds2S(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row-1,col)/(blk3_2fuel%DsS(row,col)*&
                   fblk3_2kc_HF(row-1,col)+blk3_2fuel%DsN(row-1,col)*fblk3_2kc_HF(row,col))
                knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk2_2kc_HF(1,col)/(blk3_2fuel%DsN(row,col)*&
                   fblk2_2kc_HF(1,col)+blk2_2fuel%DsS(1,col)*fblk3_2kc_HF(row,col))
              
                   !ϵ������
                fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
                fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
                fblk3_2as1(row,col) = ksp*blk3_2fuel%LsS(row,col)/blk3_2fuel%Ds2S(row,col)
                fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
                fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)* blk3_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                              fblk3_2an1(row,col)
                fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)                 
              end do           
           end if

           col = 1    !��fuel block1����
           do row = 2,N5-1
              !! Region 1,��߽�
                !�ȵ��ʵ���ƽ��ֵ ,�����غ�
              kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col+1)/(blk3_fuel%DsE(row,col)*&
                fblk3_kc_HF(row,col+1)+blk3_fuel%DsW(row,col+1)*fblk3_kc_HF(row,col))
              kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk1_kc_HF(row,N2)/(blk3_fuel%DsW(row,col)*&
                fblk1_kc_HF(row,N2)+blk1_fuel%DsE(row,N2)*fblk3_kc_HF(row,col))
              ksp = blk3_fuel%Ds2S(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row-1,col)/(blk3_fuel%DsS(row,col)*&
                fblk3_kc_HF(row-1,col)+blk3_fuel%DsN(row-1,col)*fblk3_kc_HF(row,col))
              knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row+1,col)/(blk3_fuel%DsN(row,col)*&
                fblk3_kc_HF(row+1,col)+blk3_fuel%DsS(row+1,col)*fblk3_kc_HF(row,col))
              
                !ϵ������
              fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
              fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
              fblk3_as1(row,col) = ksp*blk3_fuel%LsS(row,col)/blk3_fuel%Ds2S(row,col)
              fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
              fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)*blk3_fuel%area(row,col)/dt_heat   !ʱ����
              fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                            fblk3_an1(row,col)
              fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
              
              !! Region 2,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ ,�����غ�
              kep = blk3_2fuel%Ds2E(row,N3)*fblk3_2kc_HF(row,N3)*fblk1_2kc_HF(row,1)/(blk3_2fuel%DsE(row,N3)*&
                fblk1_2kc_HF(row,1)+blk1_2fuel%DsW(row,1)*fblk3_2kc_HF(row,N3))
              kwp = blk3_2fuel%Ds2W(row,N3)*fblk3_2kc_HF(row,N3)*fblk3_2kc_HF(row,N3-1)/(blk3_2fuel%DsW(row,N3)*&
                fblk3_2kc_HF(row,N3-1)+blk3_2fuel%DsE(row,N3-1)*fblk3_2kc_HF(row,N3))
              ksp = blk3_2fuel%Ds2S(row,N3)*fblk3_2kc_HF(row,N3)*fblk3_2kc_HF(row-1,N3)/(blk3_2fuel%DsS(row,N3)*&
                fblk3_2kc_HF(row-1,N3)+blk3_2fuel%DsN(row-1,N3)*fblk3_2kc_HF(row,N3))
              knp = blk3_2fuel%Ds2N(row,N3)*fblk3_2kc_HF(row,N3)*fblk3_2kc_HF(row+1,N3)/(blk3_2fuel%DsN(row,N3)*&
                fblk3_2kc_HF(row+1,N3)+blk3_2fuel%DsS(row+1,N3)*fblk3_2kc_HF(row,N3))
              
                !ϵ������
              fblk3_2ae1(row,N3) = kep*blk3_2fuel%LsE(row,N3)/blk3_2fuel%Ds2E(row,N3)
              fblk3_2aw1(row,N3) = kwp*blk3_2fuel%LsW(row,N3)/blk3_2fuel%Ds2W(row,N3)
              fblk3_2as1(row,N3) = ksp*blk3_2fuel%LsS(row,N3)/blk3_2fuel%Ds2S(row,N3)
              fblk3_2an1(row,N3) = knp*blk3_2fuel%LsN(row,N3)/blk3_2fuel%Ds2N(row,N3)
              
              fblk3_2ap0(row,N3) = RFuel*fblk3_2cp_HF(row,N3)*blk3_2fuel%area(row,N3)/dt_heat   !ʱ����
              fblk3_2ap1(row,N3) = fblk3_2ap0(row,N3)+fblk3_2ae1(row,N3)+fblk3_2aw1(row,N3)+fblk3_2as1(row,N3)+&
                            fblk3_2an1(row,N3)
              fblk3_2bp(row,N3) = volume_power*blk3_2fuel%area(row,N3)               
           end do

           col = N3    !��fuel block5����
           do row = 2,N5-1
              !! Region 1,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk5_kc_HF(row,1)/(blk3_fuel%DsE(row,col)*&
                fblk5_kc_HF(row,1)+blk5_fuel%DsW(row,1)*fblk3_kc_HF(row,col))
              kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col-1)/(blk3_fuel%DsW(row,col)*&
                fblk3_kc_HF(row,col-1)+blk3_fuel%DsE(row,col-1)*fblk3_kc_HF(row,col))
              ksp = blk3_fuel%Ds2S(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row-1,col)/(blk3_fuel%DsS(row,col)*&
                fblk3_kc_HF(row-1,col)+blk3_fuel%DsN(row-1,col)*fblk3_kc_HF(row,col))
              knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row+1,col)/(blk3_fuel%DsN(row,col)*&
                fblk3_kc_HF(row+1,col)+blk3_fuel%DsS(row+1,col)*fblk3_kc_HF(row,col))
              
                !ϵ������
              fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
              fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
              fblk3_as1(row,col) = ksp*blk3_fuel%LsS(row,col)/blk3_fuel%Ds2S(row,col)
              fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
              fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)*blk3_fuel%area(row,col)/dt_heat   !ʱ����
              fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                            fblk3_an1(row,col)
              fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
              
              !! Region 2,��߽�
                !�ȵ��ʵ���ƽ��ֵ
              kep = blk3_2fuel%Ds2E(row,1)*fblk3_2kc_HF(row,1)*fblk3_2kc_HF(row,2)/(blk3_2fuel%DsE(row,1)*&
                fblk3_2kc_HF(row,1+12)+blk3_2fuel%DsW(row,2)*fblk3_2kc_HF(row,1))
              kwp = blk3_2fuel%Ds2W(row,1)*fblk3_2kc_HF(row,1)*fblk5_2kc_HF(row,N4)/(blk3_2fuel%DsW(row,1)*&
                fblk5_2kc_HF(row,N4)+blk5_2fuel%DsE(row,N4)*fblk3_2kc_HF(row,1))
              ksp = blk3_2fuel%Ds2S(row,1)*fblk3_2kc_HF(row,1)*fblk3_2kc_HF(row-1,1)/(blk3_2fuel%DsS(row,1)*&
                fblk3_2kc_HF(row-1,1)+blk3_2fuel%DsN(row-1,1)*fblk3_2kc_HF(row,1))
              knp = blk3_2fuel%Ds2N(row,1)*fblk3_2kc_HF(row,1)*fblk3_2kc_HF(row+1,1)/(blk3_2fuel%DsN(row,1)*&
                fblk3_2kc_HF(row+1,1)+blk3_2fuel%DsS(row+1,1)*fblk3_2kc_HF(row,1))
              
                !ϵ������
              fblk3_2ae1(row,1) = kep*blk3_2fuel%LsE(row,1)/blk3_2fuel%Ds2E(row,1)
              fblk3_2aw1(row,1) = kwp*blk3_2fuel%LsW(row,1)/blk3_2fuel%Ds2W(row,1)
              fblk3_2as1(row,1) = ksp*blk3_2fuel%LsS(row,1)/blk3_2fuel%Ds2S(row,1)
              fblk3_2an1(row,1) = knp*blk3_2fuel%LsN(row,1)/blk3_2fuel%Ds2N(row,1)
              
              fblk3_2ap0(row,1) = RFuel*fblk3_2cp_HF(row,1)*blk3_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk3_2ap1(row,1) = fblk3_2ap0(row,1)+fblk3_2ae1(row,1)+fblk3_2aw1(row,1)+fblk3_2as1(row,1)+&
                            fblk3_2an1(row,1)
              fblk3_2bp(row,1) = volume_power*blk3_2fuel%area(row,1)              
           end do
        
           !!�ĸ�����
           !����
           col = 1   
           row = N5
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col+1)/(blk3_fuel%DsE(row,col)*&
              fblk3_kc_HF(row,col+1)+blk3_fuel%DsW(row,col+1)*fblk3_kc_HF(row,col))
           kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk1_kc_HF(row,N2)/(blk3_fuel%DsW(row,col)*&
              fblk1_kc_HF(row,N2)+blk1_fuel%DsE(row,N2)*fblk3_kc_HF(row,col))
           ksp = blk3_fuel%Ds2S(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row-1,col)/(blk3_fuel%DsS(row,col)*&
              fblk3_kc_HF(row-1,col)+blk3_fuel%DsN(row-1,col)*fblk3_kc_HF(row,col))
           knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk2_kc_HF(1,col)/(blk3_fuel%DsN(row,col)*&
              fblk2_kc_HF(1,col)+blk2_fuel%DsS(1,col)*fblk3_kc_HF(row,col))
              
              !ϵ������
           fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
           fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
           fblk3_as1(row,col) = ksp*blk3_fuel%LsS(row,col)/blk3_fuel%Ds2S(row,col)
           fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
           fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)*blk3_fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                         fblk3_an1(row,col)
           fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col+1)/(blk3_2fuel%DsE(row,col)*&
              fblk3_2kc_HF(row,col+1)+blk3_2fuel%DsW(row,col+1)*fblk3_2kc_HF(row,col))
           kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk5_2kc_HF(row,N4)/(blk3_2fuel%DsW(row,col)*&
              fblk5_2kc_HF(row,N4)+blk5_2fuel%DsE(row,N4)*fblk3_2kc_HF(row,col))
           ksp = blk3_2fuel%Ds2S(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row-1,col)/(blk3_2fuel%DsS(row,col)*&
              fblk3_2kc_HF(row-1,col)+blk3_2fuel%DsN(row-1,col)*fblk3_2kc_HF(row,col))
           knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk2_2kc_HF(1,col)/(blk3_2fuel%DsN(row,col)*&
              fblk2_2kc_HF(1,col)+blk2_2fuel%DsS(1,col)*fblk3_2kc_HF(row,col))
              
              !ϵ������
           fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
           fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
           fblk3_2as1(row,col) = ksp*blk3_2fuel%LsS(row,col)/blk3_2fuel%Ds2S(row,col)
           fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
           fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)*blk3_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                         fblk3_2an1(row,col)
           fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)             
        
           !����
           col = 1
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col+1)/(blk3_fuel%DsE(row,col)*&
              fblk3_kc_HF(row,col+1)+blk3_fuel%DsW(row,col+1)*fblk3_kc_HF(row,col))
           kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk1_kc_HF(row,N2)/(blk3_fuel%DsW(row,col)*&
              fblk1_kc_HF(row,N2)+blk1_fuel%DsE(row,N2)*fblk3_kc_HF(row,col))
           ksp = fblk3_kc_HF(row,col)
           knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row+1,col)/(blk3_fuel%DsN(row,col)*&
              fblk3_kc_HF(row+1,col)+blk3_fuel%DsS(row+1,col)*fblk3_kc_HF(row,col))
              
              !ϵ������
           fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
           fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
           fblk3_as1(row,col) = 0.0
           fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
           fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)* blk3_fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                         fblk3_an1(row,col)
           fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col+1)/(blk3_2fuel%DsE(row,col)*&
              fblk3_2kc_HF(row,col+1)+blk3_2fuel%DsW(row,col+1)*fblk3_2kc_HF(row,col))
           kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk5_2kc_HF(row,N4)/(blk3_2fuel%DsW(row,col)*&
              fblk5_2kc_HF(row,N4)+blk5_2fuel%DsE(row,N4)*fblk3_2kc_HF(row,col))
           ksp = fblk3_2kc_HF(row,col)
           knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row+1,col)/(blk3_2fuel%DsN(row,col)*&
              fblk3_2kc_HF(row+1,col)+blk3_2fuel%DsS(row+1,col)*fblk3_2kc_HF(row,col))
              
              !ϵ������
           fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
           fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
           fblk3_2as1(row,col) = 0.0
           fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
           fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)* blk3_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                         fblk3_2an1(row,col)
           fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)            
        
           !����
           col = N3
           row = N5
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk5_kc_HF(row,1)/(blk3_fuel%DsE(row,col)*&
              fblk5_kc_HF(row,1)+blk5_fuel%DsW(row,1)*fblk3_kc_HF(row,col))
           kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col-1)/(blk3_fuel%DsW(row,col)*&
              fblk3_kc_HF(row,col-1)+blk3_fuel%DsE(row,col-1)*fblk3_kc_HF(row,col))
           ksp = blk3_fuel%Ds2S(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row-1,col)/(blk3_fuel%DsS(row,col)*&
              fblk3_kc_HF(row-1,col)+blk3_fuel%DsN(row-1,col)*fblk3_kc_HF(row,col))
           knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk2_kc_HF(1,col)/(blk3_fuel%DsN(row,col)*&
              fblk2_kc_HF(1,col)+blk2_fuel%DsS(1,col)*fblk3_kc_HF(row,col))
              
              !ϵ������
           fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
           fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
           fblk3_as1(row,col) = ksp*blk3_fuel%LsS(row,col)/blk3_fuel%Ds2S(row,col)
           fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
           fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)*blk3_fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                         fblk3_an1(row,col)
           fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)          

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk1_2kc_HF(row,1)/(blk3_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row,1)+blk1_2fuel%DsW(row,1)*fblk3_2kc_HF(row,col))
           kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col-1)/(blk3_2fuel%DsW(row,col)*&
              fblk3_2kc_HF(row,col-1)+blk3_2fuel%DsE(row,col-1)*fblk3_2kc_HF(row,col))
           ksp = blk3_2fuel%Ds2S(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row-1,col)/(blk3_2fuel%DsS(row,col)*&
              fblk3_2kc_HF(row-1,col)+blk3_2fuel%DsN(row-1,col)*fblk3_2kc_HF(row,col))
           knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk2_2kc_HF(1,col)/(blk3_2fuel%DsN(row,col)*&
              fblk2_2kc_HF(1,col)+blk2_2fuel%DsS(1,col)*fblk3_2kc_HF(row,col))
              
              !ϵ������
           fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
           fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
           fblk3_2as1(row,col) = ksp*blk3_2fuel%LsS(row,col)/blk3_2fuel%Ds2S(row,col)
           fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
           fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)*blk3_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                         fblk3_2an1(row,col)
           fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)            
           !����
           col = N3
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_fuel%Ds2E(row,col)*fblk3_kc_HF(row,col)*fblk5_kc_HF(row,1)/(blk3_fuel%DsE(row,col)*&
              fblk5_kc_HF(row,1)+blk5_fuel%DsW(row,1)*fblk3_kc_HF(row,col))
           kwp = blk3_fuel%Ds2W(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row,col-1)/(blk3_fuel%DsW(row,col)*&
              fblk3_kc_HF(row,col-1)+blk3_fuel%DsE(row,col-1)*fblk3_kc_HF(row,col))
           ksp = fblk3_kc_HF(row,col)
           knp = blk3_fuel%Ds2N(row,col)*fblk3_kc_HF(row,col)*fblk3_kc_HF(row+1,col)/(blk3_fuel%DsN(row,col)*&
              fblk3_kc_HF(row+1,col)+blk3_fuel%DsS(row+1,col)*fblk3_kc_HF(row,col))
              
              !ϵ������
           fblk3_ae1(row,col) = kep*blk3_fuel%LsE(row,col)/blk3_fuel%Ds2E(row,col)
           fblk3_aw1(row,col) = kwp*blk3_fuel%LsW(row,col)/blk3_fuel%Ds2W(row,col)
           fblk3_as1(row,col) = 0.0
           fblk3_an1(row,col) = knp*blk3_fuel%LsN(row,col)/blk3_fuel%Ds2N(row,col)
              
           fblk3_ap0(row,col) = RFuel*fblk3_cp_HF(row,col)* blk3_fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_ap1(row,col) = fblk3_ap0(row,col)+fblk3_ae1(row,col)+fblk3_aw1(row,col)+fblk3_as1(row,col)+&
                         fblk3_an1(row,col)
           fblk3_bp(row,col) = volume_power*blk3_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ
           kep = blk3_2fuel%Ds2E(row,col)*fblk3_2kc_HF(row,col)*fblk1_2kc_HF(row,1)/(blk3_2fuel%DsE(row,col)*&
              fblk1_2kc_HF(row,1)+blk1_2fuel%DsW(row,1)*fblk3_2kc_HF(row,col))
           kwp = blk3_2fuel%Ds2W(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row,col-1)/(blk3_2fuel%DsW(row,col)*&
              fblk3_2kc_HF(row,col-1)+blk3_2fuel%DsE(row,col-1)*fblk3_2kc_HF(row,col))
           ksp = fblk3_2kc_HF(row,col)
           knp = blk3_2fuel%Ds2N(row,col)*fblk3_2kc_HF(row,col)*fblk3_2kc_HF(row+1,col)/(blk3_2fuel%DsN(row,col)*&
              fblk3_2kc_HF(row+1,col)+blk3_2fuel%DsS(row+1,col)*fblk3_2kc_HF(row,col))
              
              !ϵ������
           fblk3_2ae1(row,col) = kep*blk3_2fuel%LsE(row,col)/blk3_2fuel%Ds2E(row,col)
           fblk3_2aw1(row,col) = kwp*blk3_2fuel%LsW(row,col)/blk3_2fuel%Ds2W(row,col)
           fblk3_2as1(row,col) = 0.0
           fblk3_2an1(row,col) = knp*blk3_2fuel%LsN(row,col)/blk3_2fuel%Ds2N(row,col)
              
           fblk3_2ap0(row,col) = RFuel*fblk3_2cp_HF(row,col)* blk3_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk3_2ap1(row,col) = fblk3_2ap0(row,col)+fblk3_2ae1(row,col)+fblk3_2aw1(row,col)+fblk3_2as1(row,col)+&
                         fblk3_2an1(row,col)
           fblk3_2bp(row,col) = volume_power*blk3_2fuel%area(row,col)           

        
          !!===========================================================�߽�������fuel block4==============================================================================          

           if (N4>2) then
              row = 1    !�±߽磬��fuel block5����              
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col+1)/(blk4_fuel%DsE(row,col)*&
                   fblk4_kc_HF(row,col+1)+blk4_fuel%DsW(row,col+1)*fblk4_kc_HF(row,col))
                kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col-1)/(blk4_fuel%DsW(row,col)*&
                   fblk4_kc_HF(row,col-1)+blk4_fuel%DsE(row,col-1)*fblk4_kc_HF(row,col))
                ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk5_kc_HF(N5,col)/(blk4_fuel%DsS(row,col)*&
                   fblk5_kc_HF(N5,col)+blk5_fuel%DsN(N5,col)*fblk4_kc_HF(row,col))
                knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row+1,col)/(blk4_fuel%DsN(row,col)*&
                   fblk4_kc_HF(row+1,col)+blk4_fuel%DsS(row+1,col)*fblk4_kc_HF(row,col))
              
                   !ϵ������
                fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
                fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
                fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
                fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
                fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
                fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                              fblk4_an1(row,col)
                fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col+1)/(blk4_2fuel%DsE(row,col)*&
                   fblk4_2kc_HF(row,col+1)+blk4_2fuel%DsW(row,col+1)*fblk4_2kc_HF(row,col))
                kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col-1)/(blk4_2fuel%DsW(row,col)*&
                   fblk4_2kc_HF(row,col-1)+blk4_2fuel%DsE(row,col-1)*fblk4_2kc_HF(row,col))
                ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk5_2kc_HF(N5,col)/(blk4_2fuel%DsS(row,col)*&
                   fblk5_2kc_HF(N5,col)+blk5_2fuel%DsN(N5,col)*fblk4_2kc_HF(row,col))
                knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row+1,col)/(blk4_2fuel%DsN(row,col)*&
                   fblk4_2kc_HF(row+1,col)+blk4_2fuel%DsS(row+1,col)*fblk4_2kc_HF(row,col))
              
                   !ϵ������
                fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
                fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
                fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
                fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)
              
                fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                              fblk4_2an1(row,col)
                fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)                
              end do 

              row = N6    !�ϱ߽磬��cladding block3����
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col+1)/(blk4_fuel%DsE(row,col)*&
                   fblk4_kc_HF(row,col+1)+blk4_fuel%DsW(row,col+1)*fblk4_kc_HF(row,col))
                kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col-1)/(blk4_fuel%DsW(row,col)*&
                   fblk4_kc_HF(row,col-1)+blk4_fuel%DsE(row,col-1)*fblk4_kc_HF(row,col))
                ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row-1,col)/(blk4_fuel%DsS(row,col)*&
                   fblk4_kc_HF(row-1,col)+blk4_fuel%DsN(row-1,col)*fblk4_kc_HF(row,col))
                knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*cblk3_kc_HF(1,col)/(blk4_fuel%DsN(row,col)*&
                   cblk3_kc_HF(1,col)+blk3_clad%DsS(1,col)*fblk4_kc_HF(row,col))
              
                   !ϵ������
                fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
                fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
                fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
                fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
                fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
                fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                              fblk4_an1(row,col)
                fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)

                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col+1)/(blk4_2fuel%DsE(row,col)*&
                   fblk4_2kc_HF(row,col+1)+blk4_2fuel%DsW(row,col+1)*fblk4_2kc_HF(row,col))
                kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col-1)/(blk4_2fuel%DsW(row,col)*&
                   fblk4_2kc_HF(row,col-1)+blk4_2fuel%DsE(row,col-1)*fblk4_2kc_HF(row,col))
                ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row-1,col)/(blk4_2fuel%DsS(row,col)*&
                   fblk4_2kc_HF(row-1,col)+blk4_2fuel%DsN(row-1,col)*fblk4_2kc_HF(row,col))
                knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*cblk3_2kc_HF(1,col)/(blk4_2fuel%DsN(row,col)*&
                   cblk3_2kc_HF(1,col)+blk3_2clad%DsS(1,col)*fblk4_2kc_HF(row,col))
              
                   !ϵ������
                fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
                fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
                fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
                fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)
              
                fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                              fblk4_2an1(row,col)
                fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)                
              end do                     
           end if
        
           col = 1    !��fuel block2����
           do row = 2,N6-1
              !! Region 1, ��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col+1)/(blk4_fuel%DsE(row,col)*&
                fblk4_kc_HF(row,col+1)+blk4_fuel%DsW(row,col+1)*fblk4_kc_HF(row,col))
              kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk2_kc_HF(row,N3)/(blk4_fuel%DsW(row,col)*&
                fblk2_kc_HF(row,N3)+blk2_fuel%DsE(row,N3)*fblk4_kc_HF(row,col))
              ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row-1,col)/(blk4_fuel%DsS(row,col)*&
                fblk4_kc_HF(row-1,col)+blk4_fuel%DsN(row-1,col)*fblk4_kc_HF(row,col))
              knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row+1,col)/(blk4_fuel%DsN(row,col)*&
                fblk4_kc_HF(row+1,col)+blk4_fuel%DsS(row+1,col)*fblk4_kc_HF(row,col))
              
              !ϵ������
              fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
              fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
              fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
              fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
              fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
              fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                            fblk4_an1(row,col)
              fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col) 
              
              !! Region 2, �ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk4_2fuel%Ds2E(row,N4)*fblk4_2kc_HF(row,N4)*fblk2_2kc_HF(row,1)/(blk4_2fuel%DsE(row,N4)*&
                fblk2_2kc_HF(row,1)+blk2_2fuel%DsW(row,1)*fblk4_2kc_HF(row,N4))
              kwp = blk4_2fuel%Ds2W(row,N4)*fblk4_2kc_HF(row,N4)*fblk4_2kc_HF(row,N4-1)/(blk4_2fuel%DsW(row,N4)*&
                fblk4_2kc_HF(row,N4-1)+blk4_2fuel%DsE(row,N4-1)*fblk4_2kc_HF(row,N4))
              ksp = blk4_2fuel%Ds2S(row,N4)*fblk4_2kc_HF(row,N4)*fblk4_2kc_HF(row-1,N4)/(blk4_2fuel%DsS(row,N4)*&
                fblk4_2kc_HF(row-1,N4)+blk4_2fuel%DsN(row-1,N4)*fblk4_2kc_HF(row,N4))
              knp = blk4_2fuel%Ds2N(row,N4)*fblk4_2kc_HF(row,N4)*fblk4_2kc_HF(row+1,N4)/(blk4_2fuel%DsN(row,N4)*&
                fblk4_2kc_HF(row+1,N4)+blk4_2fuel%DsS(row+1,N4)*fblk4_2kc_HF(row,N4))
              
              !ϵ������
              fblk4_2ae1(row,N4) = kep*blk4_2fuel%LsE(row,N4)/blk4_2fuel%Ds2E(row,N4)
              fblk4_2aw1(row,N4) = kwp*blk4_2fuel%LsW(row,N4)/blk4_2fuel%Ds2W(row,N4)
              fblk4_2as1(row,N4) = ksp*blk4_2fuel%LsS(row,N4)/blk4_2fuel%Ds2S(row,N4)
              fblk4_2an1(row,N4) = knp*blk4_2fuel%LsN(row,N4)/blk4_2fuel%Ds2N(row,N4)
              
              fblk4_2ap0(row,N4) = RFuel*fblk4_2cp_HF(row,N4)* blk4_2fuel%area(row,N4)/dt_heat   !ʱ����
              fblk4_2ap1(row,N4) = fblk4_2ap0(row,N4)+fblk4_2ae1(row,N4)+fblk4_2aw1(row,N4)+fblk4_2as1(row,N4)+&
                            fblk4_2an1(row,N4)
              fblk4_2bp(row,N4) = volume_power*blk4_2fuel%area(row,N4)              
           end do 

           col = N4    !��fuel block6����
           do row = 2,N6-1
              !! Region 1,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk6_kc_HF(row,1)/(blk4_fuel%DsE(row,col)*&
                fblk6_kc_HF(row,1)+blk6_fuel%DsW(row,1)*fblk4_kc_HF(row,col))
              kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col-1)/(blk4_fuel%DsW(row,col)*&
                fblk4_kc_HF(row,col-1)+blk4_fuel%DsE(row,col-1)*fblk4_kc_HF(row,col))
              ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row-1,col)/(blk4_fuel%DsS(row,col)*&
                fblk4_kc_HF(row-1,col)+blk4_fuel%DsN(row-1,col)*fblk4_kc_HF(row,col))
              knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row+1,col)/(blk4_fuel%DsN(row,col)*&
                fblk4_kc_HF(row+1,col)+blk4_fuel%DsS(row+1,col)*fblk4_kc_HF(row,col))
              
                !ϵ������
              fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
              fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
              fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
              fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
              fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
              fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                            fblk4_an1(row,col)
              fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)
              
              !! Region 2,��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk4_2fuel%Ds2E(row,1)*fblk4_2kc_HF(row,1)*fblk4_2kc_HF(row,1+1)/(blk4_2fuel%DsE(row,1)*&
                fblk4_2kc_HF(row,1+1)+blk4_2fuel%DsW(row,1+1)*fblk4_2kc_HF(row,1))
              kwp = blk4_2fuel%Ds2W(row,1)*fblk4_2kc_HF(row,1)*fblk6_2kc_HF(row,N5)/(blk4_2fuel%DsW(row,1)*&
                fblk6_2kc_HF(row,N5)+blk6_2fuel%DsE(row,N5)*fblk4_2kc_HF(row,1))
              ksp = blk4_2fuel%Ds2S(row,1)*fblk4_2kc_HF(row,1)*fblk4_2kc_HF(row-1,1)/(blk4_2fuel%DsS(row,1)*&
                fblk4_2kc_HF(row-1,1)+blk4_2fuel%DsN(row-1,1)*fblk4_2kc_HF(row,1))
              knp = blk4_2fuel%Ds2N(row,1)*fblk4_2kc_HF(row,1)*fblk4_2kc_HF(row+1,1)/(blk4_2fuel%DsN(row,1)*&
                fblk4_2kc_HF(row+1,1)+blk4_2fuel%DsS(row+1,1)*fblk4_2kc_HF(row,1))
              
                !ϵ������
              fblk4_2ae1(row,1) = kep*blk4_2fuel%LsE(row,1)/blk4_2fuel%Ds2E(row,1)
              fblk4_2aw1(row,1) = kwp*blk4_2fuel%LsW(row,1)/blk4_2fuel%Ds2W(row,1)
              fblk4_2as1(row,1) = ksp*blk4_2fuel%LsS(row,1)/blk4_2fuel%Ds2S(row,1)
              fblk4_2an1(row,1) = knp*blk4_2fuel%LsN(row,1)/blk4_2fuel%Ds2N(row,1)
              
              fblk4_2ap0(row,1) = RFuel*fblk4_2cp_HF(row,1)* blk4_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk4_2ap1(row,1) = fblk4_2ap0(row,1)+fblk4_2ae1(row,1)+fblk4_2aw1(row,1)+fblk4_2as1(row,1)+&
                            fblk4_2an1(row,1)
              fblk4_2bp(row,1) = volume_power*blk4_2fuel%area(row,1)              
           end do
        
           !!�ĸ�����
           !����
           col = 1    
           row = N6
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col+1)/(blk4_fuel%DsE(row,col)*&
              fblk4_kc_HF(row,col+1)+blk4_fuel%DsW(row,col+1)*fblk4_kc_HF(row,col))
           kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk2_kc_HF(row,N3)/(blk4_fuel%DsW(row,col)*&
              fblk2_kc_HF(row,N3)+blk2_fuel%DsE(row,N3)*fblk4_kc_HF(row,col))
           ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row-1,col)/(blk4_fuel%DsS(row,col)*&
              fblk4_kc_HF(row-1,col)+blk4_fuel%DsN(row-1,col)*fblk4_kc_HF(row,col))
           knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*cblk3_kc_HF(1,col)/(blk4_fuel%DsN(row,col)*&
              cblk3_kc_HF(1,col)+blk3_clad%DsS(1,col)*fblk4_kc_HF(row,col))
              
              !ϵ������
           fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
           fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
           fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
           fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
           fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                         fblk4_an1(row,col)
           fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)          

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col+1)/(blk4_2fuel%DsE(row,col)*&
              fblk4_2kc_HF(row,col+1)+blk4_2fuel%DsW(row,col+1)*fblk4_2kc_HF(row,col))
           kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk6_2kc_HF(row,N5)/(blk4_2fuel%DsW(row,col)*&
              fblk6_2kc_HF(row,N5)+blk6_2fuel%DsE(row,N5)*fblk4_2kc_HF(row,col))
           ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row-1,col)/(blk4_2fuel%DsS(row,col)*&
              fblk4_2kc_HF(row-1,col)+blk4_2fuel%DsN(row-1,col)*fblk4_2kc_HF(row,col))
           knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*cblk3_2kc_HF(1,col)/(blk4_2fuel%DsN(row,col)*&
              cblk3_2kc_HF(1,col)+blk3_2clad%DsS(1,col)*fblk4_2kc_HF(row,col))
              
              !ϵ������
           fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
           fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
           fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
           fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)       
              
           fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                         fblk4_2an1(row,col)
           fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)            
           
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col+1)/(blk4_fuel%DsE(row,col)*&
              fblk4_kc_HF(row,col+1)+blk4_fuel%DsW(row,col+1)*fblk4_kc_HF(row,col))
           kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk2_kc_HF(row,N3)/(blk4_fuel%DsW(row,col)*&
              fblk2_kc_HF(row,N3)+blk2_fuel%DsE(row,N3)*fblk4_kc_HF(row,col))
           ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk5_kc_HF(N5,col)/(blk4_fuel%DsS(row,col)*&
              fblk5_kc_HF(N5,col)+blk5_fuel%DsN(N5,col)*fblk4_kc_HF(row,col))
           knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row+1,col)/(blk4_fuel%DsN(row,col)*&
              fblk4_kc_HF(row+1,col)+blk4_fuel%DsS(row+1,col)*fblk4_kc_HF(row,col))
              
              !ϵ������
           fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
           fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
           fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
           fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
           fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                         fblk4_an1(row,col)
           fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)           

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col+1)/(blk4_2fuel%DsE(row,col)*&
              fblk4_2kc_HF(row,col+1)+blk4_2fuel%DsW(row,col+1)*fblk4_2kc_HF(row,col))
           kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk6_2kc_HF(row,N5)/(blk4_2fuel%DsW(row,col)*&
              fblk6_2kc_HF(row,N5)+blk6_2fuel%DsE(row,N5)*fblk4_2kc_HF(row,col))
           ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk5_2kc_HF(N5,col)/(blk4_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(N5,col)+blk5_2fuel%DsN(N5,col)*fblk4_2kc_HF(row,col))
           knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row+1,col)/(blk4_2fuel%DsN(row,col)*&
              fblk4_2kc_HF(row+1,col)+blk4_2fuel%DsS(row+1,col)*fblk4_2kc_HF(row,col))
              
              !ϵ������
           fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
           fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
           fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
           fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)
              
           fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                         fblk4_2an1(row,col)
           fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)           
           !����
           col = N4
           row =N6
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk6_kc_HF(row,1)/(blk4_fuel%DsE(row,col)*&
              fblk6_kc_HF(row,1)+blk6_fuel%DsW(row,1)*fblk4_kc_HF(row,col))
           kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col-1)/(blk4_fuel%DsW(row,col)*&
              fblk4_kc_HF(row,col-1)+blk4_fuel%DsE(row,col-1)*fblk4_kc_HF(row,col))
           ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row-1,col)/(blk4_fuel%DsS(row,col)*&
              fblk4_kc_HF(row-1,col)+blk4_fuel%DsN(row-1,col)*fblk4_kc_HF(row,col))
           knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*cblk3_kc_HF(1,col)/(blk4_fuel%DsN(row,col)*&
              cblk3_kc_HF(1,col)+blk3_clad%DsS(1,col)*fblk4_kc_HF(row,col))
              
              !ϵ������
           fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
           fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
           fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
           fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
           fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                         fblk4_an1(row,col)
           fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)          
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk2_2kc_HF(row,1)/(blk4_2fuel%DsE(row,col)*&
              fblk2_2kc_HF(row,1)+blk2_2fuel%DsW(row,1)*fblk4_2kc_HF(row,col))
           kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col-1)/(blk4_2fuel%DsW(row,col)*&
              fblk4_2kc_HF(row,col-1)+blk4_2fuel%DsE(row,col-1)*fblk4_2kc_HF(row,col))
           ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row-1,col)/(blk4_2fuel%DsS(row,col)*&
              fblk4_2kc_HF(row-1,col)+blk4_2fuel%DsN(row-1,col)*fblk4_2kc_HF(row,col))
           knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*cblk3_2kc_HF(1,col)/(blk4_2fuel%DsN(row,col)*&
              cblk3_2kc_HF(1,col)+blk3_2clad%DsS(1,col)*fblk4_2kc_HF(row,col))
              
              !ϵ������
           fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
           fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
           fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
           fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)
              
           fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                         fblk4_2an1(row,col)
           fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)  
           
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_fuel%Ds2E(row,col)*fblk4_kc_HF(row,col)*fblk6_kc_HF(row,1)/(blk4_fuel%DsE(row,col)*&
              fblk6_kc_HF(row,1)+blk6_fuel%DsW(row,1)*fblk4_kc_HF(row,col))
           kwp = blk4_fuel%Ds2W(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row,col-1)/(blk4_fuel%DsW(row,col)*&
              fblk4_kc_HF(row,col-1)+blk4_fuel%DsE(row,col-1)*fblk4_kc_HF(row,col))
           ksp = blk4_fuel%Ds2S(row,col)*fblk4_kc_HF(row,col)*fblk5_kc_HF(N5,col)/(blk4_fuel%DsS(row,col)*&
              fblk5_kc_HF(N5,col)+blk5_fuel%DsN(N5,col)*fblk4_kc_HF(row,col))
           knp = blk4_fuel%Ds2N(row,col)*fblk4_kc_HF(row,col)*fblk4_kc_HF(row+1,col)/(blk4_fuel%DsN(row,col)*&
              fblk4_kc_HF(row+1,col)+blk4_fuel%DsS(row+1,col)*fblk4_kc_HF(row,col))
              
              !ϵ������
           fblk4_ae1(row,col) = kep*blk4_fuel%LsE(row,col)/blk4_fuel%Ds2E(row,col)
           fblk4_aw1(row,col) = kwp*blk4_fuel%LsW(row,col)/blk4_fuel%Ds2W(row,col)
           fblk4_as1(row,col) = ksp*blk4_fuel%LsS(row,col)/blk4_fuel%Ds2S(row,col)
           fblk4_an1(row,col) = knp*blk4_fuel%LsN(row,col)/blk4_fuel%Ds2N(row,col)
              
           fblk4_ap0(row,col) = RFuel*fblk4_cp_HF(row,col)* blk4_fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_ap1(row,col) = fblk4_ap0(row,col)+fblk4_ae1(row,col)+fblk4_aw1(row,col)+fblk4_as1(row,col)+&
                         fblk4_an1(row,col)
           fblk4_bp(row,col) = volume_power*blk4_fuel%area(row,col)
        
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2fuel%Ds2E(row,col)*fblk4_2kc_HF(row,col)*fblk2_2kc_HF(row,1)/(blk4_2fuel%DsE(row,col)*&
              fblk2_2kc_HF(row,1)+blk2_2fuel%DsW(row,1)*fblk4_2kc_HF(row,col))
           kwp = blk4_2fuel%Ds2W(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row,col-1)/(blk4_2fuel%DsW(row,col)*&
              fblk4_2kc_HF(row,col-1)+blk4_2fuel%DsE(row,col-1)*fblk4_2kc_HF(row,col))
           ksp = blk4_2fuel%Ds2S(row,col)*fblk4_2kc_HF(row,col)*fblk5_2kc_HF(N5,col)/(blk4_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(N5,col)+blk5_2fuel%DsN(N5,col)*fblk4_2kc_HF(row,col))
           knp = blk4_2fuel%Ds2N(row,col)*fblk4_2kc_HF(row,col)*fblk4_2kc_HF(row+1,col)/(blk4_2fuel%DsN(row,col)*&
              fblk4_2kc_HF(row+1,col)+blk4_2fuel%DsS(row+1,col)*fblk4_2kc_HF(row,col))
              
              !ϵ������
           fblk4_2ae1(row,col) = kep*blk4_2fuel%LsE(row,col)/blk4_2fuel%Ds2E(row,col)
           fblk4_2aw1(row,col) = kwp*blk4_2fuel%LsW(row,col)/blk4_2fuel%Ds2W(row,col)
           fblk4_2as1(row,col) = ksp*blk4_2fuel%LsS(row,col)/blk4_2fuel%Ds2S(row,col)
           fblk4_2an1(row,col) = knp*blk4_2fuel%LsN(row,col)/blk4_2fuel%Ds2N(row,col)
              
           fblk4_2ap0(row,col) = RFuel*fblk4_2cp_HF(row,col)* blk4_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk4_2ap1(row,col) = fblk4_2ap0(row,col)+fblk4_2ae1(row,col)+fblk4_2aw1(row,col)+fblk4_2as1(row,col)+&
                         fblk4_2an1(row,col)
           fblk4_2bp(row,col) = volume_power*blk4_2fuel%area(row,col)        
          !!===========================================================�߽�������fuel block5==============================================================================          

           if (N4>2) then
              row = 1    !�±߽磬��������
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col+1)/(blk5_fuel%DsE(row,col)*&
                   fblk5_kc_HF(row,col+1)+blk5_fuel%DsW(row,col+1)*fblk5_kc_HF(row,col))
                kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col-1)/(blk5_fuel%DsW(row,col)*&
                   fblk5_kc_HF(row,col-1)+blk5_fuel%DsE(row,col-1)*fblk5_kc_HF(row,col))
                ksp =fblk5_kc_HF(row,col)
                knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row+1,col)/(blk5_fuel%DsN(row,col)*&
                   fblk5_kc_HF(row+1,col)+blk5_fuel%DsS(row+1,col)*fblk5_kc_HF(row,col))
              
                   !ϵ������
                fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
                fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
                fblk5_as1(row,col) = 0.0
                fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
                fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
                fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                              fblk5_an1(row,col)
                fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk5_2fuel%Ds2E(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col+1)/(blk5_2fuel%DsE(row,col)*&
                   fblk5_2kc_HF(row,col+1)+blk5_2fuel%DsW(row,col+1)*fblk5_2kc_HF(row,col))
                kwp = blk5_2fuel%Ds2W(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col-1)/(blk5_2fuel%DsW(row,col)*&
                   fblk5_2kc_HF(row,col-1)+blk5_2fuel%DsE(row,col-1)*fblk5_2kc_HF(row,col))
                ksp =fblk5_2kc_HF(row,col)
                knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row+1,col)/(blk5_2fuel%DsN(row,col)*&
                   fblk5_2kc_HF(row+1,col)+blk5_2fuel%DsS(row+1,col)*fblk5_2kc_HF(row,col))
              
                   !ϵ������
                fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
                fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
                fblk5_2as1(row,col) = 0.0
                fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
                fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)* blk5_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                              fblk5_2an1(row,col)
                fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)                 
              end do 

              row = N5    !�ϱ߽磬��fuel block4����
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col+1)/(blk5_fuel%DsE(row,col)*&
                   fblk5_kc_HF(row,col+1)+blk5_fuel%DsW(row,col+1)*fblk5_kc_HF(row,col))
                kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col-1)/(blk5_fuel%DsW(row,col)*&
                   fblk5_kc_HF(row,col-1)+blk5_fuel%DsE(row,col-1)*fblk5_kc_HF(row,col))
                ksp = blk5_fuel%Ds2S(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row-1,col)/(blk5_fuel%DsS(row,col)*&
                   fblk5_kc_HF(row-1,col)+blk5_fuel%DsN(row-1,col)*fblk5_kc_HF(row,col))
                knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk4_kc_HF(1,col)/(blk5_fuel%DsN(row,col)*&
                   fblk4_kc_HF(1,col)+blk4_fuel%DsS(1,col)*fblk5_kc_HF(row,col))
              
                   !ϵ������
                fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
                fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
                fblk5_as1(row,col) = ksp*blk5_fuel%LsS(row,col)/blk5_fuel%Ds2S(row,col)
                fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
                fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
                fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                              fblk5_an1(row,col)
                fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk5_2fuel%Ds2E(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col+1)/(blk5_2fuel%DsE(row,col)*&
                   fblk5_2kc_HF(row,col+1)+blk5_2fuel%DsW(row,col+1)*fblk5_2kc_HF(row,col))
                kwp = blk5_2fuel%Ds2W(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col-1)/(blk5_2fuel%DsW(row,col)*&
                   fblk5_2kc_HF(row,col-1)+blk5_2fuel%DsE(row,col-1)*fblk5_2kc_HF(row,col))
                ksp = blk5_2fuel%Ds2S(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row-1,col)/(blk5_2fuel%DsS(row,col)*&
                   fblk5_2kc_HF(row-1,col)+blk5_2fuel%DsN(row-1,col)*fblk5_2kc_HF(row,col))
                knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk4_2kc_HF(1,col)/(blk5_2fuel%DsN(row,col)*&
                   fblk4_2kc_HF(1,col)+blk4_2fuel%DsS(1,col)*fblk5_2kc_HF(row,col))
              
                   !ϵ������
                fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
                fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
                fblk5_2as1(row,col) = ksp*blk5_2fuel%LsS(row,col)/blk5_2fuel%Ds2S(row,col)
                fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
                fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)* blk5_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                              fblk5_2an1(row,col)
                fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)                  
              end do            
           end if        

           col = 1    !��fuel block3����
           do row = 2,N5-1
              !! Region 1,��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col+1)/(blk5_fuel%DsE(row,col)*&
                fblk5_kc_HF(row,col+1)+blk5_fuel%DsW(row,col+1)*fblk5_kc_HF(row,col))
              kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk3_kc_HF(row,N3)/(blk5_fuel%DsW(row,col)*&
                fblk3_kc_HF(row,N3)+blk3_fuel%DsE(row,N3)*fblk5_kc_HF(row,col))
              ksp = blk5_fuel%Ds2S(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row-1,col)/(blk5_fuel%DsS(row,col)*&
                fblk5_kc_HF(row-1,col)+blk5_fuel%DsN(row-1,col)*fblk5_kc_HF(row,col))
              knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row+1,col)/(blk5_fuel%DsN(row,col)*&
                fblk5_kc_HF(row+1,col)+blk5_fuel%DsS(row+1,col)*fblk5_kc_HF(row,col))
              
                !ϵ������
              fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
              fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
              fblk5_as1(row,col) = ksp*blk5_fuel%LsS(row,col)/blk5_fuel%Ds2S(row,col)
              fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
              fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
              fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                            fblk5_an1(row,col)
              fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col) 
              
              !! Region 2,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk5_2fuel%Ds2E(row,N4)*fblk5_2kc_HF(row,N4)*fblk3_2kc_HF(row,1)/(blk5_2fuel%DsE(row,N4)*&
                fblk3_2kc_HF(row,1)+blk3_2fuel%DsW(row,1)*fblk5_2kc_HF(row,N4))
              kwp = blk5_2fuel%Ds2W(row,N4)*fblk5_2kc_HF(row,N4)*fblk5_2kc_HF(row,N4-1)/(blk5_2fuel%DsW(row,N4)*&
                fblk5_2kc_HF(row,N4-1)+blk5_2fuel%DsE(row,N4-1)*fblk5_2kc_HF(row,N4))
              ksp = blk5_2fuel%Ds2S(row,N4)*fblk5_2kc_HF(row,N4)*fblk5_2kc_HF(row-1,N4)/(blk5_2fuel%DsS(row,N4)*&
                fblk5_2kc_HF(row-1,N4)+blk5_2fuel%DsN(row-1,N4)*fblk5_2kc_HF(row,N4))
              knp = blk5_2fuel%Ds2N(row,N4)*fblk5_2kc_HF(row,N4)*fblk5_2kc_HF(row+1,N4)/(blk5_2fuel%DsN(row,N4)*&
                fblk5_2kc_HF(row+1,N4)+blk5_2fuel%DsS(row+1,N4)*fblk5_2kc_HF(row,N4))
              
                !ϵ������
              fblk5_2ae1(row,N4) = kep*blk5_2fuel%LsE(row,N4)/blk5_2fuel%Ds2E(row,N4)
              fblk5_2aw1(row,N4) = kwp*blk5_2fuel%LsW(row,N4)/blk5_2fuel%Ds2W(row,N4)
              fblk5_2as1(row,N4) = ksp*blk5_2fuel%LsS(row,N4)/blk5_2fuel%Ds2S(row,N4)
              fblk5_2an1(row,N4) = knp*blk5_2fuel%LsN(row,N4)/blk5_2fuel%Ds2N(row,N4)
              
              fblk5_2ap0(row,N4) = RFuel*fblk5_2cp_HF(row,N4)* blk5_2fuel%area(row,N4)/dt_heat   !ʱ����
              fblk5_2ap1(row,N4) = fblk5_2ap0(row,N4)+fblk5_2ae1(row,N4)+fblk5_2aw1(row,N4)+fblk5_2as1(row,N4)+&
                            fblk5_2an1(row,N4)
              fblk5_2bp(row,N4) = volume_power*blk5_2fuel%area(row,N4)                
           end do         
        
           col = N4    !��fuel block6����
           do row = 2,N5-1
              !! Region 1, �ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk6_kc_HF(1,N5-row+1)/(blk5_fuel%DsE(row,col)*&
                fblk6_kc_HF(1,N5-row+1)+blk5_fuel%DsW(1,N5-row+1)*fblk5_kc_HF(row,col))
              kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col-1)/(blk5_fuel%DsW(row,col)*&
                fblk5_kc_HF(row,col-1)+blk5_fuel%DsE(row,col-1)*fblk5_kc_HF(row,col))
              ksp = blk5_fuel%Ds2S(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row-1,col)/(blk5_fuel%DsS(row,col)*&
                fblk5_kc_HF(row-1,col)+blk5_fuel%DsN(row-1,col)*fblk5_kc_HF(row,col))
              knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row+1,col)/(blk5_fuel%DsN(row,col)*&
                fblk5_kc_HF(row+1,col)+blk5_fuel%DsS(row+1,col)*fblk5_kc_HF(row,col))
              
                !ϵ������
              fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
              fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
              fblk5_as1(row,col) = ksp*blk5_fuel%LsS(row,col)/blk5_fuel%Ds2S(row,col)
              fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
              fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
              fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                            fblk5_an1(row,col)
              fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)
              
              !! Region 2, ��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk5_2fuel%Ds2E(row,1)*fblk5_2kc_HF(row,1)*fblk5_2kc_HF(row,2)/(blk5_2fuel%DsE(row,1)*&
                fblk5_2kc_HF(row,2)+blk5_2fuel%DsW(row,2)*fblk5_2kc_HF(row,1))
              kwp = blk5_2fuel%Ds2W(row,1)*fblk5_2kc_HF(row,1)*fblk6_2kc_HF(1,row)/(blk5_2fuel%DsW(row,1)*&
                fblk6_2kc_HF(1,row)+blk6_2fuel%DsE(1,row)*fblk5_2kc_HF(row,1))
              ksp = blk5_2fuel%Ds2S(row,1)*fblk5_2kc_HF(row,1)*fblk5_2kc_HF(row-1,1)/(blk5_2fuel%DsS(row,1)*&
                fblk5_2kc_HF(row-1,1)+blk5_2fuel%DsN(row-1,1)*fblk5_2kc_HF(row,1))
              knp = blk5_2fuel%Ds2N(row,1)*fblk5_2kc_HF(row,1)*fblk5_2kc_HF(row+1,1)/(blk5_2fuel%DsN(row,1)*&
                fblk5_2kc_HF(row+1,1)+blk5_2fuel%DsS(row+1,1)*fblk5_2kc_HF(row,1))
              
                !ϵ������
              fblk5_2ae1(row,1) = kep*blk5_2fuel%LsE(row,1)/blk5_2fuel%Ds2E(row,1)
              fblk5_2aw1(row,1) = kwp*blk5_2fuel%LsW(row,1)/blk5_2fuel%Ds2W(row,1)
              fblk5_2as1(row,1) = ksp*blk5_2fuel%LsS(row,1)/blk5_2fuel%Ds2S(row,1)
              fblk5_2an1(row,1) = knp*blk5_2fuel%LsN(row,1)/blk5_2fuel%Ds2N(row,1)
              
              fblk5_2ap0(row,1) = RFuel*fblk5_2cp_HF(row,1)* blk5_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk5_2ap1(row,1) = fblk5_2ap0(row,1)+fblk5_2ae1(row,1)+fblk5_2aw1(row,1)+fblk5_2as1(row,1)+&
                            fblk5_2an1(row,1)
              fblk5_2bp(row,1) = volume_power*blk5_2fuel%area(row,1)               
           end do
        
           !!�ĸ�����
           !����
           col = 1 
           row = N5
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col+1)/(blk5_fuel%DsE(row,col)*&
              fblk5_kc_HF(row,col+1)+blk5_fuel%DsW(row,col+1)*fblk5_kc_HF(row,col))
           kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk3_kc_HF(row,N3)/(blk5_fuel%DsW(row,col)*&
              fblk3_kc_HF(row,N3)+blk3_fuel%DsE(row,N3)*fblk5_kc_HF(row,col))
           ksp = blk5_fuel%Ds2S(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row-1,col)/(blk5_fuel%DsS(row,col)*&
              fblk5_kc_HF(row-1,col)+blk5_fuel%DsN(row-1,col)*fblk5_kc_HF(row,col))
           knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk4_kc_HF(1,col)/(blk5_fuel%DsN(row,col)*&
              fblk4_kc_HF(1,col)+blk4_fuel%DsS(1,col)*fblk5_kc_HF(row,col))
              
              !ϵ������
           fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
           fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
           fblk5_as1(row,col) = ksp*blk5_fuel%LsS(row,col)/blk5_fuel%Ds2S(row,col)
           fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
           fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                         fblk5_an1(row,col)
           fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)            
 
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_2fuel%Ds2E(row,1)*fblk5_2kc_HF(row,1)*fblk5_2kc_HF(row,2)/(blk5_2fuel%DsE(row,1)*&
              fblk5_2kc_HF(row,2)+blk5_2fuel%DsW(row,2)*fblk5_2kc_HF(row,1))
           kwp = blk5_2fuel%Ds2W(row,1)*fblk5_2kc_HF(row,1)*fblk6_2kc_HF(1,row)/(blk5_2fuel%DsW(row,1)*&
              fblk6_2kc_HF(1,row)+blk6_2fuel%DsE(1,row)*fblk5_2kc_HF(row,1))
           ksp = blk5_2fuel%Ds2S(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row-1,col)/(blk5_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(row-1,col)+blk5_2fuel%DsN(row-1,col)*fblk5_2kc_HF(row,col))
           knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk4_2kc_HF(1,col)/(blk5_2fuel%DsN(row,col)*&
              fblk4_2kc_HF(1,col)+blk4_2fuel%DsS(1,col)*fblk5_2kc_HF(row,col))
              
              !ϵ������
           fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
           fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
           fblk5_2as1(row,col) = ksp*blk5_2fuel%LsS(row,col)/blk5_2fuel%Ds2S(row,col)
           fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
           fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)* blk5_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                         fblk5_2an1(row,col)
           fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)            
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col+1)/(blk5_fuel%DsE(row,col)*&
              fblk5_kc_HF(row,col+1)+blk5_fuel%DsW(row,col+1)*fblk5_kc_HF(row,col))
           kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk3_kc_HF(row,N3)/(blk5_fuel%DsW(row,col)*&
              fblk3_kc_HF(row,N3)+blk3_fuel%DsE(row,N3)*fblk5_kc_HF(row,col))
           ksp = fblk5_kc_HF(row,col)
           knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row+1,col)/(blk5_fuel%DsN(row,col)*&
              fblk5_kc_HF(row+1,col)+blk5_fuel%DsS(row+1,col)*fblk5_kc_HF(row,col))
              
              !ϵ������
           fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
           fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
           fblk5_as1(row,col) = 0.0
           fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
           fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                         fblk5_an1(row,col)
           fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)           

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_2fuel%Ds2E(row,1)*fblk5_2kc_HF(row,1)*fblk5_2kc_HF(row,2)/(blk5_2fuel%DsE(row,1)*&
              fblk5_2kc_HF(row,2)+blk5_2fuel%DsW(row,2)*fblk5_2kc_HF(row,1))
           kwp = blk5_2fuel%Ds2W(row,1)*fblk5_2kc_HF(row,1)*fblk6_2kc_HF(1,row)/(blk5_2fuel%DsW(row,1)*&
              fblk6_2kc_HF(1,row)+blk6_2fuel%DsE(1,row)*fblk5_2kc_HF(row,1))
           ksp = fblk5_2kc_HF(row,col)
           knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row+1,col)/(blk5_2fuel%DsN(row,col)*&
              fblk5_2kc_HF(row+1,col)+blk5_2fuel%DsS(row+1,col)*fblk5_2kc_HF(row,col))
              
              !ϵ������
           fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
           fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
           fblk5_2as1(row,col) = 0.0
           fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
           fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)* blk5_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                         fblk5_2an1(row,col)
           fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)
           
           !����
           col = N4
           row = N5          
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk6_kc_HF(1,N5-row+1)/(blk5_fuel%DsE(row,col)*&
              fblk6_kc_HF(1,N5-row+1)+blk5_fuel%DsW(1,N5-row+1)*fblk5_kc_HF(row,col))
           kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col-1)/(blk5_fuel%DsW(row,col)*&
              fblk5_kc_HF(row,col-1)+blk5_fuel%DsE(row,col-1)*fblk5_kc_HF(row,col))
           ksp = blk5_fuel%Ds2S(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row-1,col)/(blk5_fuel%DsS(row,col)*&
              fblk5_kc_HF(row-1,col)+blk5_fuel%DsN(row-1,col)*fblk5_kc_HF(row,col))
           knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk4_kc_HF(1,col)/(blk5_fuel%DsN(row,col)*&
              fblk4_kc_HF(1,col)+blk4_fuel%DsS(1,col)*fblk5_kc_HF(row,col))
              
              !ϵ������
           fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
           fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
           fblk5_as1(row,col) = ksp*blk5_fuel%LsS(row,col)/blk5_fuel%Ds2S(row,col)
           fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
           fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)* blk5_fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                         fblk5_an1(row,col)
           fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_2fuel%Ds2E(row,col)*fblk5_2kc_HF(row,col)*fblk3_2kc_HF(row,1)/(blk5_2fuel%DsE(row,col)*&
              fblk3_2kc_HF(row,1)+blk3_2fuel%DsW(row,1)*fblk5_2kc_HF(row,col))
           kwp = blk5_2fuel%Ds2W(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col-1)/(blk5_2fuel%DsW(row,col)*&
              fblk5_2kc_HF(row,col-1)+blk5_2fuel%DsE(row,col-1)*fblk5_2kc_HF(row,col))
           ksp = blk5_2fuel%Ds2S(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row-1,col)/(blk5_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(row-1,col)+blk5_2fuel%DsN(row-1,col)*fblk5_2kc_HF(row,col))
           knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk4_2kc_HF(1,col)/(blk5_2fuel%DsN(row,col)*&
              fblk4_2kc_HF(1,col)+blk4_2fuel%DsS(1,col)*fblk5_2kc_HF(row,col))
              
              !ϵ������
           fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
           fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
           fblk5_2as1(row,col) = ksp*blk5_2fuel%LsS(row,col)/blk5_2fuel%Ds2S(row,col)
           fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
           fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)* blk5_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                         fblk5_2an1(row,col)
           fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)  
           
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_fuel%Ds2E(row,col)*fblk5_kc_HF(row,col)*fblk6_kc_HF(1,N5-row+1)/(blk5_fuel%DsE(row,col)*&
              fblk6_kc_HF(1,N5-row+1)+blk5_fuel%DsW(1,N5-row+1)*fblk5_kc_HF(row,col))
           kwp = blk5_fuel%Ds2W(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row,col-1)/(blk5_fuel%DsW(row,col)*&
              fblk5_kc_HF(row,col-1)+blk5_fuel%DsE(row,col-1)*fblk5_kc_HF(row,col))
           ksp = fblk5_kc_HF(row,col)
           knp = blk5_fuel%Ds2N(row,col)*fblk5_kc_HF(row,col)*fblk5_kc_HF(row+1,col)/(blk5_fuel%DsN(row,col)*&
              fblk5_kc_HF(row+1,col)+blk5_fuel%DsS(row+1,col)*fblk5_kc_HF(row,col))
              
              !ϵ������
           fblk5_ae1(row,col) = kep*blk5_fuel%LsE(row,col)/blk5_fuel%Ds2E(row,col)
           fblk5_aw1(row,col) = kwp*blk5_fuel%LsW(row,col)/blk5_fuel%Ds2W(row,col)
           fblk5_as1(row,col) = 0.0
           fblk5_an1(row,col) = knp*blk5_fuel%LsN(row,col)/blk5_fuel%Ds2N(row,col)
              
           fblk5_ap0(row,col) = RFuel*fblk5_cp_HF(row,col)*blk5_fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_ap1(row,col) = fblk5_ap0(row,col)+fblk5_ae1(row,col)+fblk5_aw1(row,col)+fblk5_as1(row,col)+&
                         fblk5_an1(row,col)
           fblk5_bp(row,col) = volume_power*blk5_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk5_2fuel%Ds2E(row,col)*fblk5_2kc_HF(row,col)*fblk3_2kc_HF(row,1)/(blk5_2fuel%DsE(row,col)*&
              fblk3_2kc_HF(row,1)+blk3_2fuel%DsW(row,1)*fblk5_2kc_HF(row,col))
           kwp = blk5_2fuel%Ds2W(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row,col-1)/(blk5_2fuel%DsW(row,col)*&
              fblk5_2kc_HF(row,col-1)+blk5_2fuel%DsE(row,col-1)*fblk5_2kc_HF(row,col))
           ksp = fblk5_2kc_HF(row,col)
           knp = blk5_2fuel%Ds2N(row,col)*fblk5_2kc_HF(row,col)*fblk5_2kc_HF(row+1,col)/(blk5_2fuel%DsN(row,col)*&
              fblk5_2kc_HF(row+1,col)+blk5_2fuel%DsS(row+1,col)*fblk5_2kc_HF(row,col))
              
              !ϵ������
           fblk5_2ae1(row,col) = kep*blk5_2fuel%LsE(row,col)/blk5_2fuel%Ds2E(row,col)
           fblk5_2aw1(row,col) = kwp*blk5_2fuel%LsW(row,col)/blk5_2fuel%Ds2W(row,col)
           fblk5_2as1(row,col) = 0.0
           fblk5_2an1(row,col) = knp*blk5_2fuel%LsN(row,col)/blk5_2fuel%Ds2N(row,col)
              
           fblk5_2ap0(row,col) = RFuel*fblk5_2cp_HF(row,col)*blk5_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk5_2ap1(row,col) = fblk5_2ap0(row,col)+fblk5_2ae1(row,col)+fblk5_2aw1(row,col)+fblk5_2as1(row,col)+&
                         fblk5_2an1(row,col)
           fblk5_2bp(row,col) = volume_power*blk5_2fuel%area(row,col)           
        
          !!===========================================================�߽�������fuel block 6==============================================================================          

           if (N5>2) then
              row = 1    !�±߽磬��fuel block 5����
              do col = 2,N5-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk6_fuel%Ds2E(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col+1)/(blk6_fuel%DsE(row,col)*&
                   fblk6_kc_HF(row,col+1)+blk6_fuel%DsW(row,col+1)*fblk6_kc_HF(row,col))
                kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col-1)/(blk6_fuel%DsW(row,col)*&
                   fblk6_kc_HF(row,col-1)+blk6_fuel%DsE(row,col-1)*fblk6_kc_HF(row,col))
                ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk5_kc_HF(N5+1-col,N4)/(blk6_fuel%DsS(row,col)*&
                   fblk5_kc_HF(N5+1-col,N4)+blk5_fuel%DsN(N5+1-col,N4)*fblk6_kc_HF(row,col))
                knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row+1,col)/(blk6_fuel%DsN(row,col)*&
                   fblk6_kc_HF(row+1,col)+blk6_fuel%DsS(row+1,col)*fblk6_kc_HF(row,col))
              
                   !ϵ������
                fblk6_ae1(row,col) = kep*blk6_fuel%LsE(row,col)/blk6_fuel%Ds2E(row,col)
                fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
                fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
                fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
                fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)*blk6_fuel%area(row,col)/dt_heat   !ʱ����
                fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                              fblk6_an1(row,col)
                fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col+1)/(blk6_2fuel%DsE(row,col)*&
                   fblk6_2kc_HF(row,col+1)+blk6_2fuel%DsW(row,col+1)*fblk6_2kc_HF(row,col))
                kwp = blk6_2fuel%Ds2W(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col-1)/(blk6_2fuel%DsW(row,col)*&
                   fblk6_2kc_HF(row,col-1)+blk6_2fuel%DsE(row,col-1)*fblk6_2kc_HF(row,col))
                ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk5_2kc_HF(col,1)/(blk6_2fuel%DsS(row,col)*&
                   fblk5_2kc_HF(col,1)+blk5_2fuel%DsN(col,N4)*fblk6_2kc_HF(row,col))
                knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row+1,col)/(blk6_2fuel%DsN(row,col)*&
                   fblk6_2kc_HF(row+1,col)+blk6_2fuel%DsS(row+1,col)*fblk6_2kc_HF(row,col))
              
                   !ϵ������
                fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
                fblk6_2aw1(row,col) = kwp*blk6_2fuel%LsW(row,col)/blk6_2fuel%Ds2W(row,col)
                fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
                fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
                fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)*blk6_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                              fblk6_2an1(row,col)
                fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col)                 
              end do 

              row = N6    !�ϱ߽磬��cladding block 4����
              do col = 2,N5-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk6_fuel%Ds2E(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col+1)/(blk6_fuel%DsE(row,col)*&
                   fblk6_kc_HF(row,col+1)+blk6_fuel%DsW(row,col+1)*fblk6_kc_HF(row,col))
                kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col-1)/(blk6_fuel%DsW(row,col)*&
                   fblk6_kc_HF(row,col-1)+blk6_fuel%DsE(row,col-1)*fblk6_kc_HF(row,col))
                ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row-1,col)/(blk6_fuel%DsS(row,col)*&
                   fblk6_kc_HF(row-1,col)+blk6_fuel%DsN(row-1,col)*fblk6_kc_HF(row,col))
                knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*cblk4_kc_HF(1,col)/(blk6_fuel%DsN(row,col)*&
                   cblk4_kc_HF(1,col)+blk4_clad%DsS(1,col)*fblk6_kc_HF(row,col))
              
                   !ϵ������
                fblk6_ae1(row,col) = kep*blk6_fuel%LsE(row,col)/blk6_fuel%Ds2E(row,col)
                fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
                fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
                fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
                fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
                fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                              fblk6_an1(row,col)
                fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col+1)/(blk6_2fuel%DsE(row,col)*&
                   fblk6_2kc_HF(row,col+1)+blk6_2fuel%DsW(row,col+1)*fblk6_2kc_HF(row,col))
                kwp = blk6_2fuel%Ds2W(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col-1)/(blk6_2fuel%DsW(row,col)*&
                   fblk6_2kc_HF(row,col-1)+blk6_2fuel%DsE(row,col-1)*fblk6_2kc_HF(row,col))
                ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row-1,col)/(blk6_2fuel%DsS(row,col)*&
                   fblk6_2kc_HF(row-1,col)+blk6_2fuel%DsN(row-1,col)*fblk6_2kc_HF(row,col))
                knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*cblk4_2kc_HF(1,col)/(blk6_2fuel%DsN(row,col)*&
                   cblk4_2kc_HF(1,col)+blk4_2clad%DsS(1,col)*fblk6_2kc_HF(row,col))
              
                   !ϵ������
                fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
                fblk6_2aw1(row,col) = kwp*blk6_2fuel%LsW(row,col)/blk6_2fuel%Ds2W(row,col)
                fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
                fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
                fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)* blk6_2fuel%area(row,col)/dt_heat   !ʱ����
                fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                              fblk6_2an1(row,col)
                fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col)                
              end do          
           end if
        
           col = 1    !��fuel block 4����
           do row = 2,N6-1
              !! Region 1, ��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk6_fuel%Ds2E(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col+1)/(blk6_fuel%DsE(row,col)*&
                fblk6_kc_HF(row,col+1)+blk6_fuel%DsW(row,col+1)*fblk6_kc_HF(row,col))
              kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk4_kc_HF(row,N4)/(blk6_fuel%DsW(row,col)*&
                fblk4_kc_HF(row,N4)+blk4_fuel%DsE(row,N4)*fblk6_kc_HF(row,col))
              ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row-1,col)/(blk6_fuel%DsS(row,col)*&
                fblk6_kc_HF(row-1,col)+blk6_fuel%DsN(row-1,col)*fblk6_kc_HF(row,col))
              knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row+1,col)/(blk6_fuel%DsN(row,col)*&
                fblk6_kc_HF(row+1,col)+blk6_fuel%DsS(row+1,col)*fblk6_kc_HF(row,col))
              
                !ϵ������
              fblk6_ae1(row,col) = kep*blk6_fuel%LsE(row,col)/blk6_fuel%Ds2E(row,col)
              fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
              fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
              fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
              fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
              fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                            fblk6_an1(row,col)
              fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)
              
              !! Region 2, �ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep = blk6_2fuel%Ds2E(row,N5)*fblk6_2kc_HF(row,N5)*fblk4_2kc_HF(row,1)/(blk6_2fuel%DsE(row,N5)*&
                fblk4_2kc_HF(row,1)+blk4_2fuel%DsW(row,1)*fblk6_2kc_HF(row,N5))
              kwp = blk6_2fuel%Ds2W(row,N5)*fblk6_2kc_HF(row,N5)*fblk6_2kc_HF(row,N5-1)/(blk6_2fuel%DsW(row,N5)*&
                fblk6_2kc_HF(row,N5-1)+blk6_2fuel%DsE(row,N5-1)*fblk6_2kc_HF(row,N5))
              ksp = blk6_2fuel%Ds2S(row,N5)*fblk6_2kc_HF(row,N5)*fblk6_2kc_HF(row-1,N5)/(blk6_2fuel%DsS(row,N5)*&
                fblk6_2kc_HF(row-1,N5)+blk6_2fuel%DsN(row-1,N5)*fblk6_2kc_HF(row,N5))
              knp = blk6_2fuel%Ds2N(row,N5)*fblk6_2kc_HF(row,N5)*fblk6_2kc_HF(row+1,N5)/(blk6_2fuel%DsN(row,N5)*&
                fblk6_2kc_HF(row+1,N5)+blk6_2fuel%DsS(row+1,N5)*fblk6_2kc_HF(row,N5))
              
                !ϵ������
              fblk6_2ae1(row,N5) = kep*blk6_2fuel%LsE(row,N5)/blk6_2fuel%Ds2E(row,N5)
              fblk6_2aw1(row,N5) = kwp*blk6_2fuel%LsW(row,N5)/blk6_2fuel%Ds2W(row,N5)
              fblk6_2as1(row,N5) = ksp*blk6_2fuel%LsS(row,N5)/blk6_2fuel%Ds2S(row,N5)
              fblk6_2an1(row,N5) = knp*blk6_2fuel%LsN(row,N5)/blk6_2fuel%Ds2N(row,N5)
              
              fblk6_2ap0(row,N5) = RFuel*fblk6_2cp_HF(row,N5)* blk6_2fuel%area(row,N5)/dt_heat   !ʱ����
              fblk6_2ap1(row,N5) = fblk6_2ap0(row,N5)+fblk6_2ae1(row,N5)+fblk6_2aw1(row,N5)+fblk6_2as1(row,N5)+&
                            fblk6_2an1(row,N5)
              fblk6_2bp(row,N5) = volume_power*blk6_2fuel%area(row,N5)              
           end do         

           col = N5    !���ȱ߽�
           do row = 2,N6-1
              !! Region 1,�ұ߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep =fblk6_kc_HF(row,col)
              kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col-1)/(blk6_fuel%DsW(row,col)*&
                fblk6_kc_HF(row,col-1)+blk6_fuel%DsE(row,col-1)*fblk6_kc_HF(row,col))
              ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row-1,col)/(blk6_fuel%DsS(row,col)*&
                fblk6_kc_HF(row-1,col)+blk6_fuel%DsN(row-1,col)*fblk6_kc_HF(row,col))
              knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row+1,col)/(blk6_fuel%DsN(row,col)*&
                fblk6_kc_HF(row+1,col)+blk6_fuel%DsS(row+1,col)*fblk6_kc_HF(row,col))
              
                !ϵ������
              fblk6_ae1(row,col) = 0.0
              fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
              fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
              fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
              fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
              fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                            fblk6_an1(row,col)
              fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col) 
              !! Region 2,��߽�
                !�ȵ��ʵ���ƽ��ֵ 
              kep =blk6_2fuel%Ds2E(row,1)*fblk6_2kc_HF(row,1)*fblk6_2kc_HF(row,2)/(blk6_2fuel%DsE(row,1)*&
                fblk6_2kc_HF(row,2)+blk6_2fuel%DsW(row,2)*fblk6_2kc_HF(row,1))
              kwp = fblk6_2kc_HF(row,1)
              ksp = blk6_2fuel%Ds2S(row,1)*fblk6_2kc_HF(row,1)*fblk6_2kc_HF(row-1,1)/(blk6_2fuel%DsS(row,1)*&
                fblk6_2kc_HF(row-1,1)+blk6_2fuel%DsN(row-1,1)*fblk6_2kc_HF(row,1))
              knp = blk6_2fuel%Ds2N(row,1)*fblk6_2kc_HF(row,1)*fblk6_2kc_HF(row+1,1)/(blk6_2fuel%DsN(row,1)*&
                fblk6_2kc_HF(row+1,1)+blk6_2fuel%DsS(row+1,1)*fblk6_2kc_HF(row,1))
              
                !ϵ������
              fblk6_2ae1(row,1) = kep*blk6_2fuel%LsE(row,1)/blk6_2fuel%Ds2E(row,1)
              fblk6_2aw1(row,1) = 0.0
              fblk6_2as1(row,1) = ksp*blk6_2fuel%LsS(row,1)/blk6_2fuel%Ds2S(row,1)
              fblk6_2an1(row,1) = knp*blk6_2fuel%LsN(row,1)/blk6_2fuel%Ds2N(row,1)
              
              fblk6_2ap0(row,1) = RFuel*fblk6_2cp_HF(row,1)* blk6_2fuel%area(row,1)/dt_heat   !ʱ����
              fblk6_2ap1(row,1) = fblk6_2ap0(row,1)+fblk6_2ae1(row,1)+fblk6_2aw1(row,1)+fblk6_2as1(row,1)+&
                            fblk6_2an1(row,1)
              fblk6_2bp(row,1) = volume_power*blk6_2fuel%area(row,1)               
           end do
        
           !!�ĸ�����
           !����
           col = 1
           row = N6
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_fuel%Ds2E(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col+1)/(blk6_fuel%DsE(row,col)*&
              fblk6_kc_HF(row,col+1)+blk6_fuel%DsW(row,col+1)*fblk6_kc_HF(row,col))
           kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk4_kc_HF(row,N4)/(blk6_fuel%DsW(row,col)*&
              fblk4_kc_HF(row,N4)+blk4_fuel%DsE(row,N4)*fblk6_kc_HF(row,col))
           ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row-1,col)/(blk6_fuel%DsS(row,col)*&
              fblk6_kc_HF(row-1,col)+blk6_fuel%DsN(row-1,col)*fblk6_kc_HF(row,col))
           knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*cblk4_kc_HF(1,col)/(blk6_fuel%DsN(row,col)*&
              cblk4_kc_HF(1,col)+blk4_clad%DsS(1,col)*fblk6_kc_HF(row,col))
              
           !ϵ������
           fblk6_ae1(row,col) = kep*blk6_fuel%LsE(row,col)/blk6_fuel%Ds2E(row,col)
           fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
           fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
           fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
           fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                         fblk6_an1(row,col)
           fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)           
 
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col+1)/(blk6_2fuel%DsE(row,col)*&
              fblk6_2kc_HF(row,col+1)+blk6_2fuel%DsW(row,col+1)*fblk6_2kc_HF(row,col))
           kwp = fblk6_2kc_HF(row,col)
           ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row-1,col)/(blk6_2fuel%DsS(row,col)*&
              fblk6_2kc_HF(row-1,col)+blk6_2fuel%DsN(row-1,col)*fblk6_2kc_HF(row,col))
           knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*cblk4_2kc_HF(1,col)/(blk6_2fuel%DsN(row,col)*&
              cblk4_2kc_HF(1,col)+blk4_2clad%DsS(1,col)*fblk6_2kc_HF(row,col))
              
           !ϵ������
           fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
           fblk6_2aw1(row,col) = 0.0
           fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
           fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
           fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)* blk6_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                         fblk6_2an1(row,col)
           fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col)            
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_fuel%Ds2E(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col+1)/(blk6_fuel%DsE(row,col)*&
              fblk6_kc_HF(row,col+1)+blk6_fuel%DsW(row,col+1)*fblk6_kc_HF(row,col))
           kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk4_kc_HF(row,N4)/(blk6_fuel%DsW(row,col)*&
              fblk4_kc_HF(row,N4)+blk4_fuel%DsE(row,N4)*fblk6_kc_HF(row,col))
           ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk5_kc_HF(N5+1-col,N4)/(blk6_fuel%DsS(row,col)*&
              fblk5_kc_HF(N5+1-col,N4)+blk5_fuel%DsN(N5+1-col,N4)*fblk6_kc_HF(row,col))
           knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row+1,col)/(blk6_fuel%DsN(row,col)*&
              fblk6_kc_HF(row+1,col)+blk6_fuel%DsS(row+1,col)*fblk6_kc_HF(row,col))
              
              !ϵ������
           fblk6_ae1(row,col) = kep*blk6_fuel%LsE(row,col)/blk6_fuel%Ds2E(row,col)
           fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
           fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
           fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
           fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                         fblk6_an1(row,col)
           fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col) 
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col+1)/(blk6_2fuel%DsE(row,col)*&
              fblk6_2kc_HF(row,col+1)+blk6_2fuel%DsW(row,col+1)*fblk6_2kc_HF(row,col))
           kwp = fblk6_2kc_HF(row,col)
           ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk5_2kc_HF(col,1)/(blk6_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(col,1)+blk5_2fuel%DsN(col,1)*fblk6_2kc_HF(row,col))
           knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row+1,col)/(blk6_2fuel%DsN(row,col)*&
              fblk6_2kc_HF(row+1,col)+blk6_2fuel%DsS(row+1,col)*fblk6_2kc_HF(row,col))
              
              !ϵ������
           fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
           fblk6_2aw1(row,col) = 0.0
           fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
           fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
           fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)* blk6_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                         fblk6_2an1(row,col)
           fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col)         
           !����
           col = N5
           row = N6
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep =fblk6_kc_HF(row,col)
           kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col-1)/(blk6_fuel%DsW(row,col)*&
              fblk6_kc_HF(row,col-1)+blk6_fuel%DsE(row,col-1)*fblk6_kc_HF(row,col))
           ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row-1,col)/(blk6_fuel%DsS(row,col)*&
              fblk6_kc_HF(row-1,col)+blk6_fuel%DsN(row-1,col)*fblk6_kc_HF(row,col))
           knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*cblk4_kc_HF(1,col)/(blk6_fuel%DsN(row,col)*&
              cblk4_kc_HF(1,col)+blk4_clad%DsS(1,col)*fblk6_kc_HF(row,col))
              
              !ϵ������
           fblk6_ae1(row,col) = 0.0
           fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
           fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
           fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
           fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)* blk6_fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                         fblk6_an1(row,col)
           fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk4_2kc_HF(row,1)/(blk6_2fuel%DsE(row,col)*&
              fblk4_2kc_HF(row,1)+blk4_2fuel%DsW(row,1)*fblk6_2kc_HF(row,col))
           kwp = blk6_2fuel%Ds2W(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col-1)/(blk6_2fuel%DsW(row,col)*&
              fblk6_2kc_HF(row,col-1)+blk6_2fuel%DsE(row,col-1)*fblk6_2kc_HF(row,col))
           ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row-1,col)/(blk6_2fuel%DsS(row,col)*&
              fblk6_2kc_HF(row-1,col)+blk6_2fuel%DsN(row-1,col)*fblk6_2kc_HF(row,col))
           knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*cblk4_2kc_HF(1,col)/(blk6_2fuel%DsN(row,col)*&
              cblk4_2kc_HF(1,col)+blk4_2clad%DsS(1,col)*fblk6_2kc_HF(row,col))
              
              !ϵ������
           fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
           fblk6_2aw1(row,col) = kwp*blk6_2fuel%LsW(row,col)/blk6_2fuel%Ds2W(row,col)
           fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
           fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
           fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)* blk6_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                         fblk6_2an1(row,col)
           fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col)          
          
           !����
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep =fblk6_kc_HF(row,col)
           kwp = blk6_fuel%Ds2W(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row,col-1)/(blk6_fuel%DsW(row,col)*&
              fblk6_kc_HF(row,col-1)+blk6_fuel%DsE(row,col-1)*fblk6_kc_HF(row,col))
           ksp = blk6_fuel%Ds2S(row,col)*fblk6_kc_HF(row,col)*fblk5_kc_HF(N5+1-col,N4)/(blk6_fuel%DsS(row,col)*&
              fblk5_kc_HF(N5+1-col,N4)+blk5_fuel%DsN(N5+1-col,N4)*fblk6_kc_HF(row,col))
           knp = blk6_fuel%Ds2N(row,col)*fblk6_kc_HF(row,col)*fblk6_kc_HF(row+1,col)/(blk6_fuel%DsN(row,col)*&
              fblk6_kc_HF(row+1,col)+blk6_fuel%DsS(row+1,col)*fblk6_kc_HF(row,col))
              
              !ϵ������
           fblk6_ae1(row,col) = 0.0
           fblk6_aw1(row,col) = kwp*blk6_fuel%LsW(row,col)/blk6_fuel%Ds2W(row,col)
           fblk6_as1(row,col) = ksp*blk6_fuel%LsS(row,col)/blk6_fuel%Ds2S(row,col)
           fblk6_an1(row,col) = knp*blk6_fuel%LsN(row,col)/blk6_fuel%Ds2N(row,col)
              
           fblk6_ap0(row,col) = RFuel*fblk6_cp_HF(row,col)*blk6_fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_ap1(row,col) = fblk6_ap0(row,col)+fblk6_ae1(row,col)+fblk6_aw1(row,col)+fblk6_as1(row,col)+&
                         fblk6_an1(row,col)
           fblk6_bp(row,col) = volume_power*blk6_fuel%area(row,col)
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk6_2fuel%Ds2E(row,col)*fblk6_2kc_HF(row,col)*fblk4_2kc_HF(row,1)/(blk6_2fuel%DsE(row,col)*&
              fblk4_2kc_HF(row,1)+blk4_2fuel%DsW(row,1)*fblk6_2kc_HF(row,col))
           kwp = blk6_2fuel%Ds2W(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row,col-1)/(blk6_2fuel%DsW(row,col)*&
              fblk6_2kc_HF(row,col-1)+blk6_2fuel%DsE(row,col-1)*fblk6_2kc_HF(row,col))
           ksp = blk6_2fuel%Ds2S(row,col)*fblk6_2kc_HF(row,col)*fblk5_2kc_HF(col,1)/(blk6_2fuel%DsS(row,col)*&
              fblk5_2kc_HF(col,1)+blk5_2fuel%DsN(col,1)*fblk6_2kc_HF(row,col))
           knp = blk6_2fuel%Ds2N(row,col)*fblk6_2kc_HF(row,col)*fblk6_2kc_HF(row+1,col)/(blk6_2fuel%DsN(row,col)*&
              fblk6_2kc_HF(row+1,col)+blk6_2fuel%DsS(row+1,col)*fblk6_2kc_HF(row,col))
              
              !ϵ������
           fblk6_2ae1(row,col) = kep*blk6_2fuel%LsE(row,col)/blk6_2fuel%Ds2E(row,col)
           fblk6_2aw1(row,col) = kwp*blk6_2fuel%LsW(row,col)/blk6_2fuel%Ds2W(row,col)
           fblk6_2as1(row,col) = ksp*blk6_2fuel%LsS(row,col)/blk6_2fuel%Ds2S(row,col)
           fblk6_2an1(row,col) = knp*blk6_2fuel%LsN(row,col)/blk6_2fuel%Ds2N(row,col)
              
           fblk6_2ap0(row,col) = RFuel*fblk6_2cp_HF(row,col)* blk6_2fuel%area(row,col)/dt_heat   !ʱ����
           fblk6_2ap1(row,col) = fblk6_2ap0(row,col)+fblk6_2ae1(row,col)+fblk6_2aw1(row,col)+fblk6_2as1(row,col)+&
                         fblk6_2an1(row,col)
           fblk6_2bp(row,col) = volume_power*blk6_2fuel%area(row,col) 
           
           !!===========================================================�߽�������clad block1==============================================================================
           !if (HF_first_iter .eqv. .false.) then
           !   open(unit = 8,file = 'Debug.out',form="formatted")              
           !   write(8,*) "  iter  ","  row  ","  col  ","  knp  ",  "  Ds2N  ","  kc  ",&
           !          "  kc(n+1)","  DsN  ","DsS(n+1)"
           !endif
           if (N2>2) then
              row = 1    !�±߽�,�� fuel block1����
              do col = 2,N2-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col+1)/(blk1_clad%DsE(row,col)*&
                   cblk1_kc_HF(row,col+1)+blk1_clad%DsW(row,col+1)*cblk1_kc_HF(row,col))
                kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col-1)/(blk1_clad%DsW(row,col)*&
                   cblk1_kc_HF(row,col-1)+blk1_clad%DsE(row,col-1)*cblk1_kc_HF(row,col))
                ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*fblk1_kc_HF(N5+N6,col)/(blk1_clad%DsS(row,col)*&
                   fblk1_kc_HF(N5+N6,col)+blk1_fuel%DsN(N5+N6,col)*cblk1_kc_HF(row,col))
                knp = blk1_clad%Ds2N(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(blk1_clad%DsN(row,col)*&
                   cblk1_kc_HF(row+1,col)+blk1_clad%DsS(row+1,col)*cblk1_kc_HF(row,col))
              
                   !ϵ������
                cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
                cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
                cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
                cblk1_an1(row,col) = knp*blk1_clad%LsN(row,col)/blk1_clad%Ds2N(row,col)
              
                cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
                cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                              cblk1_an1(row,col)
                cblk1_bp(row,col) = 0.0
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col+1)/(blk1_2clad%DsE(row,col)*&
                   cblk1_2kc_HF(row,col+1)+blk1_2clad%DsW(row,col+1)*cblk1_2kc_HF(row,col))
                kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col-1)/(blk1_2clad%DsW(row,col)*&
                   cblk1_2kc_HF(row,col-1)+blk1_2clad%DsE(row,col-1)*cblk1_2kc_HF(row,col))
                ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*fblk1_2kc_HF(N5+N6,col)/(blk1_2clad%DsS(row,col)*&
                   fblk1_2kc_HF(N5+N6,col)+blk1_2fuel%DsN(N5+N6,col)*cblk1_2kc_HF(row,col))
                knp = blk1_2clad%Ds2N(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(blk1_2clad%DsN(row,col)*&
                   cblk1_2kc_HF(row+1,col)+blk1_2clad%DsS(row+1,col)*cblk1_2kc_HF(row,col))
              
                   !ϵ������
                cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
                cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
                cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
                cblk1_2an1(row,col) = knp*blk1_2clad%LsN(row,col)/blk1_2clad%Ds2N(row,col)
              
                cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
                cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                              cblk1_2an1(row,col)
                cblk1_2bp(row,col) = 0.0                            
              end do
10001        format(4x,i4,4x,i3,4x,i3,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8)               

              row = N1    !�ϱ߽�,������߽�����
              do col = 2,N2-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col+1)/(blk1_clad%DsE(row,col)*&
                   cblk1_kc_HF(row,col+1)+blk1_clad%DsW(row,col+1)*cblk1_kc_HF(row,col))
                kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col-1)/(blk1_clad%DsW(row,col)*&
                   cblk1_kc_HF(row,col-1)+blk1_clad%DsE(row,col-1)*cblk1_kc_HF(row,col))
                ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row-1,col)/(blk1_clad%DsS(row,col)*&
                   cblk1_kc_HF(row-1,col)+blk1_clad%DsN(row-1,col)*cblk1_kc_HF(row,col))
                !knp = 2*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(cblk1_kc_HF(row,col)+cblk1_kc_HF(row+1,col))
                knp = cblk1_kc_HF(row,col)
              
                   !ϵ������
                cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
                cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
                cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
                cblk1_an1(row,col) = 0.0

                CoordX = blk1_clad%n_x(row,col);CoordY = blk1_clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)
                
                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                              
                              
                damp = knp*blk1_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_clad%DsN(row,col)+knp)
                cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
                cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                              cblk1_an1(row,col)+damp
                cblk1_bp(row,col) = damp*tfluid
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col+1)/(blk1_2clad%DsE(row,col)*&
                   cblk1_2kc_HF(row,col+1)+blk1_2clad%DsW(row,col+1)*cblk1_2kc_HF(row,col))
                kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col-1)/(blk1_2clad%DsW(row,col)*&
                   cblk1_2kc_HF(row,col-1)+blk1_2clad%DsE(row,col-1)*cblk1_2kc_HF(row,col))
                ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row-1,col)/(blk1_2clad%DsS(row,col)*&
                   cblk1_2kc_HF(row-1,col)+blk1_2clad%DsN(row-1,col)*cblk1_2kc_HF(row,col))
                !knp = 2*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(cblk1_2kc_HF(row,col)+cblk1_2kc_HF(row+1,col))
                knp = cblk1_kc_HF(row,col)
              
                   !ϵ������
                cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
                cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
                cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
                cblk1_2an1(row,col) = 0.0

                CoordX = blk1_2clad%n_x(row,col);CoordY = blk1_2clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)
                
                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                               
                              
                damp = knp*blk1_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_2clad%DsN(row,col)+knp)
                cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
                cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                              cblk1_2an1(row,col)+damp
                cblk1_2bp(row,col) = damp*tfluid                 
              end do                     
           end if
        
           if (N1>2) then
              col = 1    
              do row = 2,N1-1
                !! Region 1,��߽�,��cladding 1-2����
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col+1)/(blk1_clad%DsE(row,col)*&
                   cblk1_kc_HF(row,col+1)+blk1_clad%DsW(row,col+1)*cblk1_kc_HF(row,col))
                kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_2kc_HF(row,N2)/(blk1_clad%DsW(row,col)*&
                   cblk1_2kc_HF(row,N2)+blk1_2clad%DsE(row,N2)*cblk1_kc_HF(row,col))
                ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row-1,col)/(blk1_clad%DsS(row,col)*&
                   cblk1_kc_HF(row-1,col)+blk1_clad%DsN(row-1,col)*cblk1_kc_HF(row,col))
                knp = blk1_clad%Ds2N(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(blk1_clad%DsN(row,col)*&
                   cblk1_kc_HF(row+1,col)+blk1_clad%DsS(row+1,col)*cblk1_kc_HF(row,col))
              
                   !ϵ������
                cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
                cblk1_aw1(row,col) = kep*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
                cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
                cblk1_an1(row,col) = knp*blk1_clad%LsN(row,col)/blk1_clad%Ds2N(row,col)
              
                cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
                cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                              cblk1_an1(row,col)
                cblk1_bp(row,col) = 0.0
                
                !! Region 2,�ұ߽�,��cladding 1����
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_2clad%Ds2E(row,N2)*cblk1_2kc_HF(row,N2)*cblk1_kc_HF(row,1)/(blk1_2clad%DsE(row,N2)*&
                   cblk1_kc_HF(row,1)+blk1_clad%DsW(row,1)*cblk1_2kc_HF(row,N2))
                kwp = blk1_2clad%Ds2W(row,N2)*cblk1_2kc_HF(row,N2)*cblk1_2kc_HF(row,N2-1)/(blk1_2clad%DsW(row,N2)*&
                   cblk1_2kc_HF(row,N2-1)+blk1_2clad%DsE(row,N2-1)*cblk1_2kc_HF(row,N2))
                ksp = blk1_2clad%Ds2S(row,N2)*cblk1_2kc_HF(row,N2)*cblk1_2kc_HF(row-1,N2)/(blk1_2clad%DsS(row,N2)*&
                   cblk1_2kc_HF(row-1,N2)+blk1_2clad%DsN(row-1,N2)*cblk1_2kc_HF(row,N2))
                knp = blk1_2clad%Ds2N(row,N2)*cblk1_2kc_HF(row,N2)*cblk1_2kc_HF(row+1,N2)/(blk1_2clad%DsN(row,N2)*&
                   cblk1_2kc_HF(row+1,N2)+blk1_2clad%DsS(row+1,N2)*cblk1_2kc_HF(row,N2))
              
                   !ϵ������
                cblk1_2ae1(row,N2) = kep*blk1_2clad%LsE(row,N2)/blk1_2clad%Ds2E(row,N2)
                cblk1_2aw1(row,N2) = kep*blk1_2clad%LsW(row,N2)/blk1_2clad%Ds2W(row,N2)
                cblk1_2as1(row,N2) = ksp*blk1_2clad%LsS(row,N2)/blk1_2clad%Ds2S(row,N2)
                cblk1_2an1(row,N2) = knp*blk1_2clad%LsN(row,N2)/blk1_2clad%Ds2N(row,N2)
              
                cblk1_2ap0(row,N2) = RCladding*cblk1_2cp_HF(row,N2)* blk1_2clad%area(row,N2)/dt_heat   !ʱ����
                cblk1_2ap1(row,N2) = cblk1_2ap0(row,N2)+cblk1_2ae1(row,N2)+cblk1_2aw1(row,N2)+cblk1_2as1(row,N2)+&
                              cblk1_2an1(row,N2)
                cblk1_2bp(row,N2) = 0.0                 
              end do

              col = N2    !�� cladding block2����
              do row = 2,N1-1
                !! Region 1,�ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk2_kc_HF(row,1)/(blk1_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,1)+blk2_clad%DsW(row,1)*cblk1_kc_HF(row,col))
                kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col-1)/(blk1_clad%DsW(row,col)*&
                   cblk1_kc_HF(row,col-1)+blk1_clad%DsE(row,col-1)*cblk1_kc_HF(row,col))
                ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row-1,col)/(blk1_clad%DsS(row,col)*&
                   cblk1_kc_HF(row-1,col)+blk1_clad%DsN(row-1,col)*cblk1_kc_HF(row,col))
                knp = blk1_clad%Ds2N(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(blk1_clad%DsN(row,col)*&
                   cblk1_kc_HF(row+1,col)+blk1_clad%DsS(row+1,col)*cblk1_kc_HF(row,col))
              
                   !ϵ������
                cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
                cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
                cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
                cblk1_an1(row,col) = knp*blk1_clad%LsN(row,col)/blk1_clad%Ds2N(row,col)
              
                cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
                cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                              cblk1_an1(row,col)
                cblk1_bp(row,col) = 0.0

                !! Region 2,��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk1_2clad%Ds2E(row,1)*cblk1_2kc_HF(row,1)*cblk1_2kc_HF(row,1+1)/(blk1_2clad%DsE(row,1)*&
                   cblk1_2kc_HF(row,1+1)+blk1_2clad%DsW(row,1+1)*cblk1_2kc_HF(row,1))
                kwp = blk1_2clad%Ds2W(row,1)*cblk1_2kc_HF(row,1)*cblk2_2kc_HF(row,N3)/(blk1_2clad%DsW(row,1)*&
                   cblk2_2kc_HF(row,N3)+blk2_2clad%DsE(row,N3)*cblk1_2kc_HF(row,1))
                ksp = blk1_2clad%Ds2S(row,1)*cblk1_2kc_HF(row,1)*cblk1_2kc_HF(row-1,1)/(blk1_2clad%DsS(row,1)*&
                   cblk1_2kc_HF(row-1,1)+blk1_2clad%DsN(row-1,1)*cblk1_2kc_HF(row,1))
                knp = blk1_2clad%Ds2N(row,1)*cblk1_2kc_HF(row,1)*cblk1_2kc_HF(row+1,1)/(blk1_2clad%DsN(row,1)*&
                   cblk1_2kc_HF(row+1,1)+blk1_2clad%DsS(row+1,1)*cblk1_2kc_HF(row,1))
              
                   !ϵ������
                cblk1_2ae1(row,1) = kep*blk1_2clad%LsE(row,1)/blk1_2clad%Ds2E(row,1)
                cblk1_2aw1(row,1) = kwp*blk1_2clad%LsW(row,1)/blk1_2clad%Ds2W(row,1)
                cblk1_2as1(row,1) = ksp*blk1_2clad%LsS(row,1)/blk1_2clad%Ds2S(row,1)
                cblk1_2an1(row,1) = knp*blk1_2clad%LsN(row,1)/blk1_2clad%Ds2N(row,1)
              
                cblk1_2ap0(row,1) = RCladding*cblk1_2cp_HF(row,1)* blk1_2clad%area(row,1)/dt_heat   !ʱ����
                cblk1_2ap1(row,1) = cblk1_2ap0(row,1)+cblk1_2ae1(row,1)+cblk1_2aw1(row,1)+cblk1_2as1(row,1)+&
                              cblk1_2an1(row,1)
                cblk1_2bp(row,1) = 0.0                
              end do                      
           end if
        
           !!�ĸ�����
           !����
           row = N1
           col = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col+1)/(blk1_clad%DsE(row,col)*&
              cblk1_kc_HF(row,col+1)+blk1_clad%DsW(row,col+1)*cblk1_kc_HF(row,col))
           kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_2kc_HF(row,N2)/(blk1_clad%DsW(row,col)*&
              cblk1_2kc_HF(row,N2)+blk1_2clad%DsE(row,N2)*cblk1_kc_HF(row,col))
           ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row-1,col)/(blk1_clad%DsS(row,col)*&
              cblk1_kc_HF(row-1,col)+blk1_clad%DsN(row-1,col)*cblk1_kc_HF(row,col))
           !knp = 2*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(cblk1_kc_HF(row,col)+cblk1_kc_HF(row+1,col))
           knp = cblk1_kc_HF(row,col)
              
              !ϵ������
           cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
           cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
           cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
           cblk1_an1(row,col) = 0.0

           CoordX = blk1_clad%n_x(row,col);CoordY = blk1_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if            
                        
           damp = knp*blk1_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_clad%DsN(row,col)+knp)
           cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
           cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                         cblk1_an1(row,col)+damp
           cblk1_bp(row,col) = damp*tfluid                  
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col+1)/(blk1_2clad%DsE(row,col)*&
              cblk1_2kc_HF(row,col+1)+blk1_2clad%DsW(row,col+1)*cblk1_2kc_HF(row,col))
           kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk2_2kc_HF(row,N3)/(blk1_2clad%DsW(row,col)*&
              cblk2_2kc_HF(row,N3)+blk2_2clad%DsE(row,N3)*cblk1_2kc_HF(row,col))
           ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row-1,col)/(blk1_2clad%DsS(row,col)*&
              cblk1_2kc_HF(row-1,col)+blk1_2clad%DsN(row-1,col)*cblk1_2kc_HF(row,col))
           !knp = 2*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(cblk1_2kc_HF(row,col)+cblk1_2kc_HF(row+1,col))
           knp = cblk1_kc_HF(row,col)
           !write(*,*) iter, z, row,col,cblk1_2kc_HF(row,col),cblk2_2kc_HF(row,N3)
              
              !ϵ������
           cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
           cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
           cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
           cblk1_2an1(row,col) = 0.0

           CoordX = blk1_2clad%n_x(row,col);CoordY = blk1_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)
                           
           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if            
                        
           damp = knp*blk1_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_2clad%DsN(row,col)+knp)
           cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
           cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                         cblk1_2an1(row,col)+damp
           cblk1_2bp(row,col) = damp*tfluid        
        
           !����
           col = 1
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col+1)/(blk1_clad%DsE(row,col)*&
              cblk1_kc_HF(row,col+1)+blk1_clad%DsW(row,col+1)*cblk1_kc_HF(row,col))
           kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_2kc_HF(row,N2)/(blk1_clad%DsW(row,col)*&
              cblk1_2kc_HF(row,N2)+blk1_2clad%DsE(row,N2)*cblk1_kc_HF(row,col))
           ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*fblk1_kc_HF(N5+N6,col)/(blk1_clad%DsS(row,col)*&
              fblk1_kc_HF(N5+N6,col)+blk1_fuel%DsN(N5+N6,col)*cblk1_kc_HF(row,col))
           knp = blk1_clad%Ds2N(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(blk1_clad%DsN(row,col)*&
              cblk1_kc_HF(row+1,col)+blk1_clad%DsS(row+1,col)*cblk1_kc_HF(row,col))
              
              !ϵ������
           cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
           cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
           cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
           cblk1_an1(row,col) = knp*blk1_clad%LsN(row,col)/blk1_clad%Ds2N(row,col)
              
           cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
           cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                         cblk1_an1(row,col)
           cblk1_bp(row,col) = 0.0 
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col+1)/(blk1_2clad%DsE(row,col)*&
              cblk1_2kc_HF(row,col+1)+blk1_2clad%DsW(row,col+1)*cblk1_2kc_HF(row,col))
           kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk2_2kc_HF(row,N3)/(blk1_2clad%DsW(row,col)*&
              cblk2_2kc_HF(row,N3)+blk2_2clad%DsE(row,N3)*cblk1_2kc_HF(row,col))
           ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*fblk1_2kc_HF(N5+N6,col)/(blk1_2clad%DsS(row,col)*&
              fblk1_2kc_HF(N5+N6,col)+blk1_2fuel%DsN(N5+N6,col)*cblk1_2kc_HF(row,col))
           knp = blk1_2clad%Ds2N(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(blk1_2clad%DsN(row,col)*&
              cblk1_2kc_HF(row+1,col)+blk1_2clad%DsS(row+1,col)*cblk1_2kc_HF(row,col))
              
              !ϵ������
           cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
           cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
           cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
           cblk1_2an1(row,col) = knp*blk1_2clad%LsN(row,col)/blk1_2clad%Ds2N(row,col)
              
           cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
           cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                         cblk1_2an1(row,col)
           cblk1_2bp(row,col) = 0.0            
           
  
           !����
           row = N1
           col = N2
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk2_kc_HF(row,1)/(blk1_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,1)+blk2_clad%DsW(row,1)*cblk1_kc_HF(row,col))
           kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col-1)/(blk1_clad%DsW(row,col)*&
              cblk1_kc_HF(row,col-1)+blk1_clad%DsE(row,col-1)*cblk1_kc_HF(row,col))
           ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row-1,col)/(blk1_clad%DsS(row,col)*&
              cblk1_kc_HF(row-1,col)+blk1_clad%DsN(row-1,col)*cblk1_kc_HF(row,col))
           !knp = 2*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(cblk1_kc_HF(row,col)+cblk1_kc_HF(row+1,col))
           knp = cblk1_kc_HF(row,col)
              
              !ϵ������
           cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
           cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
           cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
           cblk1_an1(row,col) = 0.0

           CoordX = blk1_clad%n_x(row,col);CoordY = blk1_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)
           
           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if  
                    
           damp = knp*blk1_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_clad%DsN(row,col)+knp)
           cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
           cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                         cblk1_an1(row,col)+damp
           cblk1_bp(row,col) = damp*tfluid                

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_kc_HF(row,1)/(blk1_2clad%DsE(row,col)*&
                   cblk1_kc_HF(row,1)+blk1_clad%DsW(row,1)*cblk1_2kc_HF(row,col))
           kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col-1)/(blk1_2clad%DsW(row,col)*&
              cblk1_2kc_HF(row,col-1)+blk1_2clad%DsE(row,col-1)*cblk1_2kc_HF(row,col))
           ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row-1,col)/(blk1_2clad%DsS(row,col)*&
              cblk1_2kc_HF(row-1,col)+blk1_2clad%DsN(row-1,col)*cblk1_2kc_HF(row,col))
           !knp = 2*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(cblk1_2kc_HF(row,col)+cblk1_2kc_HF(row+1,col))
           knp = cblk1_2kc_HF(row,col)
              
              !ϵ������
           cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
           cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
           cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
           cblk1_2an1(row,col) = 0.0

           CoordX = blk1_2clad%n_x(row,col);CoordY = blk1_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)
           
           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if  
                    
           damp = knp*blk1_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk1_2clad%DsN(row,col)+knp)
           cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
           cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                         cblk1_2an1(row,col)+damp
           cblk1_2bp(row,col) = damp*tfluid 
           
           !����
           row = 1
           col = N2
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_clad%Ds2E(row,col)*cblk1_kc_HF(row,col)*cblk2_kc_HF(row,1)/(blk1_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,1)+blk2_clad%DsW(row,1)*cblk1_kc_HF(row,col))
           kwp = blk1_clad%Ds2W(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row,col-1)/(blk1_clad%DsW(row,col)*&
              cblk1_kc_HF(row,col-1)+blk1_clad%DsE(row,col-1)*cblk1_kc_HF(row,col))
           ksp = blk1_clad%Ds2S(row,col)*cblk1_kc_HF(row,col)*fblk1_kc_HF(N5+N6,col)/(blk1_clad%DsS(row,col)*&
               fblk1_kc_HF(N5+N6,col)+blk1_fuel%DsN(N5+N6,col)*cblk1_kc_HF(row,col))
           knp = blk1_clad%Ds2N(row,col)*cblk1_kc_HF(row,col)*cblk1_kc_HF(row+1,col)/(blk1_clad%DsN(row,col)*&
              cblk1_kc_HF(row+1,col)+blk1_clad%DsS(row+1,col)*cblk1_kc_HF(row,col))
              
              !ϵ������
           cblk1_ae1(row,col) = kep*blk1_clad%LsE(row,col)/blk1_clad%Ds2E(row,col)
           cblk1_aw1(row,col) = kwp*blk1_clad%LsW(row,col)/blk1_clad%Ds2W(row,col)
           cblk1_as1(row,col) = ksp*blk1_clad%LsS(row,col)/blk1_clad%Ds2S(row,col)
           cblk1_an1(row,col) = knp*blk1_clad%LsN(row,col)/blk1_clad%Ds2N(row,col)
              
           cblk1_ap0(row,col) = RCladding*cblk1_cp_HF(row,col)* blk1_clad%area(row,col)/dt_heat   !ʱ����
           cblk1_ap1(row,col) = cblk1_ap0(row,col)+cblk1_ae1(row,col)+cblk1_aw1(row,col)+cblk1_as1(row,col)+&
                         cblk1_an1(row,col)
           cblk1_bp(row,col) = 0.0                  

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk1_2clad%Ds2E(row,col)*cblk1_2kc_HF(row,col)*cblk1_kc_HF(row,1)/(blk1_2clad%DsE(row,col)*&
                   cblk1_kc_HF(row,1)+blk1_clad%DsW(row,1)*cblk1_2kc_HF(row,col))
           kwp = blk1_2clad%Ds2W(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row,col-1)/(blk1_2clad%DsW(row,col)*&
              cblk1_2kc_HF(row,col-1)+blk1_2clad%DsE(row,col-1)*cblk1_2kc_HF(row,col))
           ksp = blk1_2clad%Ds2S(row,col)*cblk1_2kc_HF(row,col)*fblk1_2kc_HF(N5+N6,col)/(blk1_2clad%DsS(row,col)*&
               fblk1_2kc_HF(N5+N6,col)+blk1_2fuel%DsN(N5+N6,col)*cblk1_2kc_HF(row,col))
           knp = blk1_2clad%Ds2N(row,col)*cblk1_2kc_HF(row,col)*cblk1_2kc_HF(row+1,col)/(blk1_2clad%DsN(row,col)*&
              cblk1_2kc_HF(row+1,col)+blk1_2clad%DsS(row+1,col)*cblk1_2kc_HF(row,col))
              
              !ϵ������
           cblk1_2ae1(row,col) = kep*blk1_2clad%LsE(row,col)/blk1_2clad%Ds2E(row,col)
           cblk1_2aw1(row,col) = kwp*blk1_2clad%LsW(row,col)/blk1_2clad%Ds2W(row,col)
           cblk1_2as1(row,col) = ksp*blk1_2clad%LsS(row,col)/blk1_2clad%Ds2S(row,col)
           cblk1_2an1(row,col) = knp*blk1_2clad%LsN(row,col)/blk1_2clad%Ds2N(row,col)
              
           cblk1_2ap0(row,col) = RCladding*cblk1_2cp_HF(row,col)* blk1_2clad%area(row,col)/dt_heat   !ʱ����
           cblk1_2ap1(row,col) = cblk1_2ap0(row,col)+cblk1_2ae1(row,col)+cblk1_2aw1(row,col)+cblk1_2as1(row,col)+&
                         cblk1_2an1(row,col)
           cblk1_2bp(row,col) = 0.0           
           !!===========================================================�߽�������clad block2============================================================================== 
           if (N3>2) then
              row = 1    !�±߽�,�� fuel block2����
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col+1)/(blk2_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,col+1)+blk2_clad%DsW(row,col+1)*cblk2_kc_HF(row,col))
                kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col-1)/(blk2_clad%DsW(row,col)*&
                   cblk2_kc_HF(row,col-1)+blk2_clad%DsE(row,col-1)*cblk2_kc_HF(row,col))
                ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*fblk2_kc_HF(N6,col)/(blk2_clad%DsS(row,col)*&
                   fblk2_kc_HF(N6,col)+blk2_fuel%DsN(N6,col)*cblk2_kc_HF(row,col))
                knp = blk2_clad%Ds2N(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row+1,col)/(blk2_clad%DsN(row,col)*&
                   cblk2_kc_HF(row+1,col)+blk2_clad%DsS(row+1,col)*cblk2_kc_HF(row,col))
              
                   !ϵ������
                cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
                cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
                cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
                cblk2_an1(row,col) = knp*blk2_clad%LsN(row,col)/blk2_clad%Ds2N(row,col)
              
                cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
                cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                              cblk2_an1(row,col)
                cblk2_bp(row,col) = 0.0 
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col+1)/(blk2_2clad%DsE(row,col)*&
                   cblk2_2kc_HF(row,col+1)+blk2_2clad%DsW(row,col+1)*cblk2_2kc_HF(row,col))
                kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col-1)/(blk2_2clad%DsW(row,col)*&
                   cblk2_2kc_HF(row,col-1)+blk2_2clad%DsE(row,col-1)*cblk2_2kc_HF(row,col))
                ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*fblk2_2kc_HF(N6,col)/(blk2_2clad%DsS(row,col)*&
                   fblk2_2kc_HF(N6,col)+blk2_2fuel%DsN(N6,col)*cblk2_2kc_HF(row,col))
                knp = blk2_2clad%Ds2N(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row+1,col)/(blk2_2clad%DsN(row,col)*&
                   cblk2_2kc_HF(row+1,col)+blk2_2clad%DsS(row+1,col)*cblk2_2kc_HF(row,col))
              
                   !ϵ������
                cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
                cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
                cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
                cblk2_2an1(row,col) = knp*blk2_2clad%LsN(row,col)/blk2_2clad%Ds2N(row,col)
              
                cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)* blk2_2clad%area(row,col)/dt_heat   !ʱ����
                cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                              cblk2_2an1(row,col)
                cblk2_2bp(row,col) = 0.0                                
              end do
           
              row = N1    !�ϱ߽�,������߽�����
              do col = 2,N3-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col+1)/(blk2_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,col+1)+blk2_clad%DsW(row,col+1)*cblk2_kc_HF(row,col))
                kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col-1)/(blk2_clad%DsW(row,col)*&
                   cblk2_kc_HF(row,col-1)+blk2_clad%DsE(row,col-1)*cblk2_kc_HF(row,col))
                ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row-1,col)/(blk2_clad%DsS(row,col)*&
                   cblk2_kc_HF(row-1,col)+blk2_clad%DsN(row-1,col)*cblk2_kc_HF(row,col))
                knp = cblk2_kc_HF(row,col)
              
                   !ϵ������
                cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
                cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
                cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
                cblk2_an1(row,col) = 0.0
                
                CoordX = blk2_clad%n_x(row,col);CoordY = blk2_clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)

                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                
                              
                damp = knp*blk2_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_clad%DsN(row,col)+knp)
                cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
                cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                              cblk2_an1(row,col)+damp
                cblk2_bp(row,col) = damp*tfluid 

                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col+1)/(blk2_2clad%DsE(row,col)*&
                   cblk2_2kc_HF(row,col+1)+blk2_2clad%DsW(row,col+1)*cblk2_2kc_HF(row,col))
                kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col-1)/(blk2_2clad%DsW(row,col)*&
                   cblk2_2kc_HF(row,col-1)+blk2_2clad%DsE(row,col-1)*cblk2_2kc_HF(row,col))
                ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row-1,col)/(blk2_2clad%DsS(row,col)*&
                   cblk2_2kc_HF(row-1,col)+blk2_2clad%DsN(row-1,col)*cblk2_2kc_HF(row,col))
                knp = cblk2_2kc_HF(row,col)
              
                   !ϵ������
                cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
                cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
                cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
                cblk2_2an1(row,col) = 0.0
                
                CoordX = blk2_2clad%n_x(row,col);CoordY = blk2_2clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)

                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                
                              
                damp = knp*blk2_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_2clad%DsN(row,col)+knp)
                cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)* blk2_2clad%area(row,col)/dt_heat   !ʱ����
                cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                              cblk2_2an1(row,col)+damp
                cblk2_2bp(row,col) = damp*tfluid                 
              end do           
           end if

           if (N1>2) then
              col = 1    !�� cladding block1����
              do row = 2,N1-1
                !! Region 1,��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col+1)/(blk2_clad%DsE(row,col)*&
                   cblk2_kc_HF(row,col+1)+blk2_clad%DsW(row,col+1)*cblk2_kc_HF(row,col))
                kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk1_kc_HF(row,N2)/(blk2_clad%DsW(row,col)*&
                   cblk1_kc_HF(row,N2)+blk1_clad%DsE(row,N2)*cblk2_kc_HF(row,col))
                ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row-1,col)/(blk2_clad%DsS(row,col)*&
                   cblk2_kc_HF(row-1,col)+blk2_clad%DsN(row-1,col)*cblk2_kc_HF(row,col))
                knp = blk2_clad%Ds2N(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row+1,col)/(blk2_clad%DsN(row,col)*&
                   cblk2_kc_HF(row+1,col)+blk2_clad%DsS(row+1,col)*cblk2_kc_HF(row,col))
              
                   !ϵ������
                cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
                cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
                cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
                cblk2_an1(row,col) = knp*blk2_clad%LsN(row,col)/blk2_clad%Ds2N(row,col)
              
                cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
                cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                              cblk2_an1(row,col)
                cblk2_bp(row,col) = 0.0

                !! Region 2,�ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_2clad%Ds2E(row,N3)*cblk2_2kc_HF(row,N3)*cblk1_2kc_HF(row,1)/(blk2_2clad%DsE(row,N3)*&
                   cblk1_2kc_HF(row,1)+blk1_2clad%DsW(row,1)*cblk2_2kc_HF(row,N3))
                kwp = blk2_2clad%Ds2W(row,N3)*cblk2_2kc_HF(row,N3)*cblk2_2kc_HF(row,N3-1)/(blk2_2clad%DsW(row,N3)*&
                   cblk2_2kc_HF(row,N3-1)+blk2_2clad%DsE(row,N3-1)*cblk2_2kc_HF(row,N3))
                ksp = blk2_2clad%Ds2S(row,N3)*cblk2_2kc_HF(row,N3)*cblk2_2kc_HF(row-1,N3)/(blk2_2clad%DsS(row,N3)*&
                   cblk2_2kc_HF(row-1,N3)+blk2_2clad%DsN(row-1,N3)*cblk2_2kc_HF(row,N3))
                knp = blk2_2clad%Ds2N(row,N3)*cblk2_2kc_HF(row,N3)*cblk2_2kc_HF(row+1,N3)/(blk2_2clad%DsN(row,N3)*&
                   cblk2_2kc_HF(row+1,N3)+blk2_2clad%DsS(row+1,N3)*cblk2_2kc_HF(row,N3))
              
                   !ϵ������
                cblk2_2ae1(row,N3) = kep*blk2_2clad%LsE(row,N3)/blk2_2clad%Ds2E(row,N3)
                cblk2_2aw1(row,N3) = kwp*blk2_2clad%LsW(row,N3)/blk2_2clad%Ds2W(row,N3)
                cblk2_2as1(row,N3) = ksp*blk2_2clad%LsS(row,N3)/blk2_2clad%Ds2S(row,N3)
                cblk2_2an1(row,N3) = knp*blk2_2clad%LsN(row,N3)/blk2_2clad%Ds2N(row,N3)
              
                cblk2_2ap0(row,N3) = RCladding*cblk2_2cp_HF(row,N3)* blk2_2clad%area(row,N3)/dt_heat   !ʱ����
                cblk2_2ap1(row,N3) = cblk2_2ap0(row,N3)+cblk2_2ae1(row,N3)+cblk2_2aw1(row,N3)+cblk2_2as1(row,N3)+&
                              cblk2_2an1(row,N3)
                cblk2_2bp(row,N3) = 0.0                
              end do
           
              col = N3    !��cladding block3����
              do row = 2,N1-1
                !! Region 1,�ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk3_kc_HF(row,1)/(blk2_clad%DsE(row,col)*&
                   cblk3_kc_HF(row,1)+blk3_clad%DsW(row,1)*cblk2_kc_HF(row,col))
                kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col-1)/(blk2_clad%DsW(row,col)*&
                   cblk2_kc_HF(row,col-1)+blk2_clad%DsE(row,col-1)*cblk2_kc_HF(row,col))
                ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row-1,col)/(blk2_clad%DsS(row,col)*&
                   cblk2_kc_HF(row-1,col)+blk2_clad%DsN(row-1,col)*cblk2_kc_HF(row,col))
                knp = blk2_clad%Ds2N(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row+1,col)/(blk2_clad%DsN(row,col)*&
                   cblk2_kc_HF(row+1,col)+blk2_clad%DsS(row+1,col)*cblk2_kc_HF(row,col))
              
                   !ϵ������
                cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
                cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
                cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
                cblk2_an1(row,col) = knp*blk2_clad%LsN(row,col)/blk2_clad%Ds2N(row,col)
              
                cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
                cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                              cblk2_an1(row,col)
                cblk2_bp(row,col) = 0.0
                
                !! Region 2,��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk2_2clad%Ds2E(row,1)*cblk2_2kc_HF(row,1)*cblk2_2kc_HF(row,2)/(blk2_2clad%DsE(row,1)*&
                   cblk2_2kc_HF(row,2)+blk2_2clad%DsW(row,2)*cblk2_2kc_HF(row,1))
                kwp = blk2_2clad%Ds2W(row,1)*cblk2_2kc_HF(row,1)*cblk3_2kc_HF(row,N4)/(blk2_2clad%DsW(row,1)*&
                   cblk3_2kc_HF(row,N4)+blk3_2clad%DsE(row,N4)*cblk2_2kc_HF(row,1))
                ksp = blk2_2clad%Ds2S(row,1)*cblk2_2kc_HF(row,1)*cblk2_2kc_HF(row-1,1)/(blk2_2clad%DsS(row,1)*&
                   cblk2_2kc_HF(row-1,1)+blk2_2clad%DsN(row-1,1)*cblk2_2kc_HF(row,1))
                knp = blk2_2clad%Ds2N(row,1)*cblk2_2kc_HF(row,1)*cblk2_2kc_HF(row+1,1)/(blk2_2clad%DsN(row,1)*&
                   cblk2_2kc_HF(row+1,1)+blk2_2clad%DsS(row+1,1)*cblk2_2kc_HF(row,1))
              
                   !ϵ������
                cblk2_2ae1(row,1) = kep*blk2_2clad%LsE(row,1)/blk2_2clad%Ds2E(row,1)
                cblk2_2aw1(row,1) = kwp*blk2_2clad%LsW(row,1)/blk2_2clad%Ds2W(row,1)
                cblk2_2as1(row,1) = ksp*blk2_2clad%LsS(row,1)/blk2_2clad%Ds2S(row,1)
                cblk2_2an1(row,1) = knp*blk2_2clad%LsN(row,1)/blk2_2clad%Ds2N(row,1)
              
                cblk2_2ap0(row,1) = RCladding*cblk2_2cp_HF(row,1)* blk2_2clad%area(row,1)/dt_heat   !ʱ����
                cblk2_2ap1(row,1) = cblk2_2ap0(row,1)+cblk2_2ae1(row,1)+cblk2_2aw1(row,1)+cblk2_2as1(row,1)+&
                              cblk2_2an1(row,1)
                cblk2_2bp(row,1) = 0.0                 
              end do           
           end if
        
           !!�ĸ�����
           !����
           row = N1
           col = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col+1)/(blk2_clad%DsE(row,col)*&
              cblk2_kc_HF(row,col+1)+blk2_clad%DsW(row,col+1)*cblk2_kc_HF(row,col))
           kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk1_kc_HF(row,N2)/(blk2_clad%DsW(row,col)*&
              cblk1_kc_HF(row,N2)+blk1_clad%DsE(row,N2)*cblk2_kc_HF(row,col))
           ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row-1,col)/(blk2_clad%DsS(row,col)*&
              cblk2_kc_HF(row-1,col)+blk2_clad%DsN(row-1,col)*cblk2_kc_HF(row,col))
           knp = cblk2_kc_HF(row,col)
              
              !ϵ������
           cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
           cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
           cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
           cblk2_an1(row,col) = 0.0

           CoordX = blk2_clad%n_x(row,col);CoordY = blk2_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                                
                       
           damp = knp*blk2_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_clad%DsN(row,col)+knp)
           cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
           cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                         cblk2_an1(row,col)+damp
           cblk2_bp(row,col) = damp*tfluid 
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col+1)/(blk2_2clad%DsE(row,col)*&
              cblk2_2kc_HF(row,col+1)+blk2_2clad%DsW(row,col+1)*cblk2_2kc_HF(row,col))
           kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk3_2kc_HF(row,N4)/(blk2_2clad%DsW(row,col)*&
              cblk3_2kc_HF(row,N4)+blk3_2clad%DsE(row,N4)*cblk2_2kc_HF(row,col))
           ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row-1,col)/(blk2_2clad%DsS(row,col)*&
              cblk2_2kc_HF(row-1,col)+blk2_2clad%DsN(row-1,col)*cblk2_2kc_HF(row,col))
           knp = cblk2_2kc_HF(row,col)          
              
              !ϵ������
           cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
           cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
           cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
           cblk2_2an1(row,col) = 0.0

           CoordX = blk2_2clad%n_x(row,col);CoordY = blk2_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                                 
                       
           damp = knp*blk2_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_2clad%DsN(row,col)+knp)
           cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)* blk2_2clad%area(row,col)/dt_heat   !ʱ����
           cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                         cblk2_2an1(row,col)+damp
           cblk2_2bp(row,col) = damp*tfluid            
        
           !����
           col = 1  
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col+1)/(blk2_clad%DsE(row,col)*&
              cblk2_kc_HF(row,col+1)+blk2_clad%DsW(row,col+1)*cblk2_kc_HF(row,col))
           kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk1_kc_HF(row,N2)/(blk2_clad%DsW(row,col)*&
              cblk1_kc_HF(row,N2)+blk1_clad%DsE(row,N2)*cblk2_kc_HF(row,col))
           ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*fblk2_kc_HF(N6,col)/(blk2_clad%DsS(row,col)*&
              fblk2_kc_HF(N6,col)+blk2_fuel%DsN(N6,col)*cblk2_kc_HF(row,col))
           knp = blk2_clad%Ds2N(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row+1,col)/(blk2_clad%DsN(row,col)*&
              cblk2_kc_HF(row+1,col)+blk2_clad%DsS(row+1,col)*cblk2_kc_HF(row,col))
              
              !ϵ������
           cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
           cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
           cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
           cblk2_an1(row,col) = knp*blk2_clad%LsN(row,col)/blk2_clad%Ds2N(row,col)
              
           cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)*blk2_clad%area(row,col)/dt_heat   !ʱ����
           cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                         cblk2_an1(row,col)
           cblk2_bp(row,col) = 0.0              

           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col+1)/(blk2_2clad%DsE(row,col)*&
              cblk2_2kc_HF(row,col+1)+blk2_2clad%DsW(row,col+1)*cblk2_2kc_HF(row,col))
           kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk3_2kc_HF(row,N4)/(blk2_2clad%DsW(row,col)*&
              cblk3_2kc_HF(row,N4)+blk3_2clad%DsE(row,N4)*cblk2_2kc_HF(row,col))
           ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*fblk2_2kc_HF(N6,col)/(blk2_2clad%DsS(row,col)*&
              fblk2_2kc_HF(N6,col)+blk2_2fuel%DsN(N6,col)*cblk2_2kc_HF(row,col))
           knp = blk2_2clad%Ds2N(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row+1,col)/(blk2_2clad%DsN(row,col)*&
              cblk2_2kc_HF(row+1,col)+blk2_2clad%DsS(row+1,col)*cblk2_2kc_HF(row,col))
              
              !ϵ������
           cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
           cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
           cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
           cblk2_2an1(row,col) = knp*blk2_2clad%LsN(row,col)/blk2_2clad%Ds2N(row,col)
              
           cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)*blk2_2clad%area(row,col)/dt_heat   !ʱ����
           cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                         cblk2_2an1(row,col)
           cblk2_2bp(row,col) = 0.0            
           !����
           row = N1
           col = N3
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk3_kc_HF(row,1)/(blk2_clad%DsE(row,col)*&
              cblk3_kc_HF(row,1)+blk3_clad%DsW(row,1)*cblk2_kc_HF(row,col))
           kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col-1)/(blk2_clad%DsW(row,col)*&
              cblk2_kc_HF(row,col-1)+blk2_clad%DsE(row,col-1)*cblk2_kc_HF(row,col))
           ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row-1,col)/(blk2_clad%DsS(row,col)*&
              cblk2_kc_HF(row-1,col)+blk2_clad%DsN(row-1,col)*cblk2_kc_HF(row,col))
           knp = cblk2_kc_HF(row,col)
              
              !ϵ������
           cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
           cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
           cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
           cblk2_an1(row,col) = 0.0

           CoordX = blk2_clad%n_x(row,col);CoordY = blk2_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                              
                         
           damp = knp*blk2_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_clad%DsN(row,col)+knp)
           cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
           cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                         cblk2_an1(row,col)+damp
           cblk2_bp(row,col) = damp*tfluid
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk1_2kc_HF(row,1)/(blk2_2clad%DsE(row,col)*&
              cblk1_2kc_HF(row,1)+blk1_2clad%DsW(row,1)*cblk2_2kc_HF(row,col))
           kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col-1)/(blk2_2clad%DsW(row,col)*&
              cblk2_2kc_HF(row,col-1)+blk2_2clad%DsE(row,col-1)*cblk2_2kc_HF(row,col))
           ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row-1,col)/(blk2_2clad%DsS(row,col)*&
              cblk2_2kc_HF(row-1,col)+blk2_2clad%DsN(row-1,col)*cblk2_2kc_HF(row,col))
           knp = cblk2_2kc_HF(row,col)
              
              !ϵ������
           cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
           cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
           cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
           cblk2_2an1(row,col) = 0.0

           CoordX = blk2_2clad%n_x(row,col);CoordY = blk2_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                              
                                                
                         
           damp = knp*blk2_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk2_2clad%DsN(row,col)+knp)
           cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)* blk2_2clad%area(row,col)/dt_heat   !ʱ����
           cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                         cblk2_2an1(row,col)+damp
           cblk2_2bp(row,col) = damp*tfluid          
           !����
           col = N3  
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_clad%Ds2E(row,col)*cblk2_kc_HF(row,col)*cblk3_kc_HF(row,1)/(blk2_clad%DsE(row,col)*&
              cblk3_kc_HF(row,1)+blk3_clad%DsW(row,1)*cblk2_kc_HF(row,col))
           kwp = blk2_clad%Ds2W(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row,col-1)/(blk2_clad%DsW(row,col)*&
              cblk2_kc_HF(row,col-1)+blk2_clad%DsE(row,col-1)*cblk2_kc_HF(row,col))
           ksp = blk2_clad%Ds2S(row,col)*cblk2_kc_HF(row,col)*fblk2_kc_HF(N6,col)/(blk2_clad%DsS(row,col)*&
              fblk2_kc_HF(N6,col)+blk2_fuel%DsN(N6,col)*cblk2_kc_HF(row,col))
           knp = blk2_clad%Ds2N(row,col)*cblk2_kc_HF(row,col)*cblk2_kc_HF(row+1,col)/(blk2_clad%DsN(row,col)*&
              cblk2_kc_HF(row+1,col)+blk2_clad%DsS(row+1,col)*cblk2_kc_HF(row,col))
              
              !ϵ������
           cblk2_ae1(row,col) = kep*blk2_clad%LsE(row,col)/blk2_clad%Ds2E(row,col)
           cblk2_aw1(row,col) = kwp*blk2_clad%LsW(row,col)/blk2_clad%Ds2W(row,col)
           cblk2_as1(row,col) = ksp*blk2_clad%LsS(row,col)/blk2_clad%Ds2S(row,col)
           cblk2_an1(row,col) = knp*blk2_clad%LsN(row,col)/blk2_clad%Ds2N(row,col)
              
           cblk2_ap0(row,col) = RCladding*cblk2_cp_HF(row,col)* blk2_clad%area(row,col)/dt_heat   !ʱ����
           cblk2_ap1(row,col) = cblk2_ap0(row,col)+cblk2_ae1(row,col)+cblk2_aw1(row,col)+cblk2_as1(row,col)+&
                         cblk2_an1(row,col)
           cblk2_bp(row,col) = 0.0        
        
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk2_2clad%Ds2E(row,col)*cblk2_2kc_HF(row,col)*cblk1_2kc_HF(row,1)/(blk2_2clad%DsE(row,col)*&
              cblk1_2kc_HF(row,1)+blk1_2clad%DsW(row,1)*cblk2_2kc_HF(row,col))
           kwp = blk2_2clad%Ds2W(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row,col-1)/(blk2_2clad%DsW(row,col)*&
              cblk2_2kc_HF(row,col-1)+blk2_2clad%DsE(row,col-1)*cblk2_2kc_HF(row,col))
           ksp = blk2_2clad%Ds2S(row,col)*cblk2_2kc_HF(row,col)*fblk2_2kc_HF(N6,col)/(blk2_2clad%DsS(row,col)*&
              fblk2_2kc_HF(N6,col)+blk2_2fuel%DsN(N6,col)*cblk2_2kc_HF(row,col))
           knp = blk2_2clad%Ds2N(row,col)*cblk2_2kc_HF(row,col)*cblk2_2kc_HF(row+1,col)/(blk2_2clad%DsN(row,col)*&
              cblk2_2kc_HF(row+1,col)+blk2_2clad%DsS(row+1,col)*cblk2_2kc_HF(row,col))
              
              !ϵ������
           cblk2_2ae1(row,col) = kep*blk2_2clad%LsE(row,col)/blk2_2clad%Ds2E(row,col)
           cblk2_2aw1(row,col) = kwp*blk2_2clad%LsW(row,col)/blk2_2clad%Ds2W(row,col)
           cblk2_2as1(row,col) = ksp*blk2_2clad%LsS(row,col)/blk2_2clad%Ds2S(row,col)
           cblk2_2an1(row,col) = knp*blk2_2clad%LsN(row,col)/blk2_2clad%Ds2N(row,col)
              
           cblk2_2ap0(row,col) = RCladding*cblk2_2cp_HF(row,col)* blk2_2clad%area(row,col)/dt_heat   !ʱ����
           cblk2_2ap1(row,col) = cblk2_2ap0(row,col)+cblk2_2ae1(row,col)+cblk2_2aw1(row,col)+cblk2_2as1(row,col)+&
                         cblk2_2an1(row,col)
           cblk2_2bp(row,col) = 0.0         
           !!===========================================================�߽�������clad block3==============================================================================
           if (N4>2) then
              row = 1    !�±߽�,�� fuel block4����
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col+1)/(blk3_clad%DsE(row,col)*&
                   cblk3_kc_HF(row,col+1)+blk3_clad%DsW(row,col+1)*cblk3_kc_HF(row,col))
                kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col-1)/(blk3_clad%DsW(row,col)*&
                   cblk3_kc_HF(row,col-1)+blk3_clad%DsE(row,col-1)*cblk3_kc_HF(row,col))
                ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*fblk4_kc_HF(N6,col)/(blk3_clad%DsS(row,col)*&
                   fblk4_kc_HF(N6,col)+blk4_fuel%DsN(N6,col)*cblk3_kc_HF(row,col))
                knp = blk3_clad%Ds2N(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row+1,col)/(blk3_clad%DsN(row,col)*&
                   cblk3_kc_HF(row+1,col)+blk3_clad%DsS(row+1,col)*cblk3_kc_HF(row,col))
              
                   !ϵ������
                cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
                cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
                cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
                cblk3_an1(row,col) = knp*blk3_clad%LsN(row,col)/blk3_clad%Ds2N(row,col)
              
                cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
                cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                              cblk3_an1(row,col)
                cblk3_bp(row,col) = 0.0

                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_2clad%Ds2E(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col+1)/(blk3_2clad%DsE(row,col)*&
                   cblk3_2kc_HF(row,col+1)+blk3_2clad%DsW(row,col+1)*cblk3_2kc_HF(row,col))
                kwp = blk3_2clad%Ds2W(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col-1)/(blk3_2clad%DsW(row,col)*&
                   cblk3_2kc_HF(row,col-1)+blk3_2clad%DsE(row,col-1)*cblk3_2kc_HF(row,col))
                ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*fblk4_2kc_HF(N6,col)/(blk3_2clad%DsS(row,col)*&
                   fblk4_2kc_HF(N6,col)+blk4_2fuel%DsN(N6,col)*cblk3_2kc_HF(row,col))
                knp = blk3_2clad%Ds2N(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row+1,col)/(blk3_2clad%DsN(row,col)*&
                   cblk3_2kc_HF(row+1,col)+blk3_2clad%DsS(row+1,col)*cblk3_2kc_HF(row,col))
              
                   !ϵ������
                cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
                cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
                cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
                cblk3_2an1(row,col) = knp*blk3_2clad%LsN(row,col)/blk3_2clad%Ds2N(row,col)
              
                cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
                cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                              cblk3_2an1(row,col)
                cblk3_2bp(row,col) = 0.0                
                
              end do

              row = N1    !�ϱ߽�,������߽�����
              do col = 2,N4-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col+1)/(blk3_clad%DsE(row,col)*&
                   cblk3_kc_HF(row,col+1)+blk3_clad%DsW(row,col+1)*cblk3_kc_HF(row,col))
                kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col-1)/(blk3_clad%DsW(row,col)*&
                   cblk3_kc_HF(row,col-1)+blk3_clad%DsE(row,col-1)*cblk3_kc_HF(row,col))
                ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row-1,col)/(blk3_clad%DsS(row,col)*&
                   cblk3_kc_HF(row-1,col)+blk3_clad%DsN(row-1,col)*cblk3_kc_HF(row,col))
                knp = cblk3_kc_HF(row,col)
              
                   !ϵ������
                cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
                cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
                cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
                cblk3_an1(row,col) = 0.0

                CoordX = blk3_clad%n_x(row,col);CoordY = blk3_clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)

                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                              
                                                    
                              
                damp = knp*blk3_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_clad%DsN(row,col)+knp)
                cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
                cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                              cblk3_an1(row,col)+damp
                cblk3_bp(row,col) = damp*tfluid 
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_2clad%Ds2E(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col+1)/(blk3_2clad%DsE(row,col)*&
                   cblk3_2kc_HF(row,col+1)+blk3_2clad%DsW(row,col+1)*cblk3_2kc_HF(row,col))
                kwp = blk3_2clad%Ds2W(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col-1)/(blk3_2clad%DsW(row,col)*&
                   cblk3_2kc_HF(row,col-1)+blk3_2clad%DsE(row,col-1)*cblk3_2kc_HF(row,col))
                ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row-1,col)/(blk3_2clad%DsS(row,col)*&
                   cblk3_2kc_HF(row-1,col)+blk3_2clad%DsN(row-1,col)*cblk3_2kc_HF(row,col))
                knp = cblk3_2kc_HF(row,col)
              
                   !ϵ������
                cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
                cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
                cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
                cblk3_2an1(row,col) = 0.0

                CoordX = blk3_2clad%n_x(row,col);CoordY = blk3_2clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)


                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                                  
                              
                damp = knp*blk3_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_2clad%DsN(row,col)+knp)
                cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
                cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                              cblk3_2an1(row,col)+damp
                cblk3_2bp(row,col) = damp*tfluid                
              end do                     
           end if
        
           if (N1>2) then
              col = 1    !�� cladding block2����
              do row = 2,N1-1
                !! Region 1, ��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col+1)/(blk3_clad%DsE(row,col)*&
                   cblk3_kc_HF(row,col+1)+blk3_clad%DsW(row,col+1)*cblk3_kc_HF(row,col))
                kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk2_kc_HF(row,N3)/(blk3_clad%DsW(row,col)*&
                   cblk2_kc_HF(row,N3)+blk2_clad%DsE(row,N3)*cblk3_kc_HF(row,col))
                ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row-1,col)/(blk3_clad%DsS(row,col)*&
                   cblk3_kc_HF(row-1,col)+blk3_clad%DsN(row-1,col)*cblk3_kc_HF(row,col))
                knp = blk3_clad%Ds2N(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row+1,col)/(blk3_clad%DsN(row,col)*&
                   cblk3_kc_HF(row+1,col)+blk3_clad%DsS(row+1,col)*cblk3_kc_HF(row,col))
              
                   !ϵ������
                cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
                cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
                cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
                cblk3_an1(row,col) = knp*blk3_clad%LsN(row,col)/blk3_clad%Ds2N(row,col)
              
                cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
                cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                              cblk3_an1(row,col)
                cblk3_bp(row,col) = 0.0
                
                !! Region 2, �ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_2clad%Ds2E(row,N4)*cblk3_2kc_HF(row,N4)*cblk2_2kc_HF(row,1)/(blk3_2clad%DsE(row,N4)*&
                   cblk2_2kc_HF(row,1)+blk2_2clad%DsW(row,1)*cblk3_2kc_HF(row,N4))
                kwp = blk3_2clad%Ds2W(row,N4)*cblk3_2kc_HF(row,N4)*cblk3_2kc_HF(row,N4-1)/(blk3_2clad%DsW(row,N4)*&
                   cblk3_2kc_HF(row,N4-1)+blk3_2clad%DsE(row,N4-1)*cblk3_2kc_HF(row,N4))
                ksp = blk3_2clad%Ds2S(row,N4)*cblk3_2kc_HF(row,N4)*cblk3_2kc_HF(row-1,N4)/(blk3_2clad%DsS(row,N4)*&
                   cblk3_2kc_HF(row-1,N4)+blk3_2clad%DsN(row-1,N4)*cblk3_2kc_HF(row,N4))
                knp = blk3_2clad%Ds2N(row,N4)*cblk3_2kc_HF(row,N4)*cblk3_2kc_HF(row+1,N4)/(blk3_2clad%DsN(row,N4)*&
                   cblk3_2kc_HF(row+1,N4)+blk3_2clad%DsS(row+1,N4)*cblk3_2kc_HF(row,N4))
              
                   !ϵ������
                cblk3_2ae1(row,N4) = kep*blk3_2clad%LsE(row,N4)/blk3_2clad%Ds2E(row,N4)
                cblk3_2aw1(row,N4) = kwp*blk3_2clad%LsW(row,N4)/blk3_2clad%Ds2W(row,N4)
                cblk3_2as1(row,N4) = ksp*blk3_2clad%LsS(row,N4)/blk3_2clad%Ds2S(row,N4)
                cblk3_2an1(row,N4) = knp*blk3_2clad%LsN(row,N4)/blk3_2clad%Ds2N(row,N4)
              
                cblk3_2ap0(row,N4) = RCladding*cblk3_2cp_HF(row,N4)* blk3_2clad%area(row,N4)/dt_heat   !ʱ����
                cblk3_2ap1(row,N4) = cblk3_2ap0(row,N4)+cblk3_2ae1(row,N4)+cblk3_2aw1(row,N4)+cblk3_2as1(row,N4)+&
                              cblk3_2an1(row,N4)
                cblk3_2bp(row,N4) = 0.0                 
              end do

              col = N4    !�� cladding block4����
              do row = 2,N1-1
                !! Region 1,�ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk4_kc_HF(row,1)/(blk3_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,1)+blk4_clad%DsW(row,1)*cblk3_kc_HF(row,col))
                kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col-1)/(blk3_clad%DsW(row,col)*&
                   cblk3_kc_HF(row,col-1)+blk3_clad%DsE(row,col-1)*cblk3_kc_HF(row,col))
                ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row-1,col)/(blk3_clad%DsS(row,col)*&
                   cblk3_kc_HF(row-1,col)+blk3_clad%DsN(row-1,col)*cblk3_kc_HF(row,col))
                knp = blk3_clad%Ds2N(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row+1,col)/(blk3_clad%DsN(row,col)*&
                   cblk3_kc_HF(row+1,col)+blk3_clad%DsS(row+1,col)*cblk3_kc_HF(row,col))
              
                   !ϵ������
                cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
                cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
                cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
                cblk3_an1(row,col) = knp*blk3_clad%LsN(row,col)/blk3_clad%Ds2N(row,col)
              
                cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
                cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                              cblk3_an1(row,col)
                cblk3_bp(row,col) = 0.0
                
                 !! Region 2,��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk3_2clad%Ds2E(row,1)*cblk3_2kc_HF(row,1)*cblk3_2kc_HF(row,2)/(blk3_2clad%DsE(row,2)*&
                   cblk3_2kc_HF(row,2)+blk3_2clad%DsW(row,2)*cblk3_2kc_HF(row,1))
                kwp = blk3_2clad%Ds2W(row,1)*cblk3_2kc_HF(row,1)*cblk4_2kc_HF(row,N5)/(blk3_2clad%DsW(row,1)*&
                   cblk4_2kc_HF(row,N5)+blk4_2clad%DsE(row,N5)*cblk3_2kc_HF(row,1))
                ksp = blk3_2clad%Ds2S(row,1)*cblk3_2kc_HF(row,1)*cblk3_2kc_HF(row-1,1)/(blk3_2clad%DsS(row,1)*&
                   cblk3_2kc_HF(row-1,1)+blk3_2clad%DsN(row-1,1)*cblk3_2kc_HF(row,1))
                knp = blk3_2clad%Ds2N(row,1)*cblk3_2kc_HF(row,1)*cblk3_2kc_HF(row+1,1)/(blk3_2clad%DsN(row,1)*&
                   cblk3_2kc_HF(row+1,1)+blk3_2clad%DsS(row+1,1)*cblk3_2kc_HF(row,1))
              
                   !ϵ������
                cblk3_2ae1(row,1) = kep*blk3_2clad%LsE(row,1)/blk3_2clad%Ds2E(row,1)
                cblk3_2aw1(row,1) = kwp*blk3_2clad%LsW(row,1)/blk3_2clad%Ds2W(row,1)
                cblk3_2as1(row,1) = ksp*blk3_2clad%LsS(row,1)/blk3_2clad%Ds2S(row,1)
                cblk3_2an1(row,1) = knp*blk3_2clad%LsN(row,1)/blk3_2clad%Ds2N(row,1)
              
                cblk3_2ap0(row,1) = RCladding*cblk3_2cp_HF(row,1)* blk3_2clad%area(row,1)/dt_heat   !ʱ����
                cblk3_2ap1(row,1) = cblk3_2ap0(row,1)+cblk3_2ae1(row,1)+cblk3_2aw1(row,1)+cblk3_2as1(row,1)+&
                              cblk3_2an1(row,1)
                cblk3_2bp(row,1) = 0.0                
              end do                      
           end if
        
           !!�ĸ�����
           !����
           row = N1
           col = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col+1)/(blk3_clad%DsE(row,col)*&
              cblk3_kc_HF(row,col+1)+blk3_clad%DsW(row,col+1)*cblk3_kc_HF(row,col))
           kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk2_kc_HF(row,N3)/(blk3_clad%DsW(row,col)*&
              cblk2_kc_HF(row,N3)+blk2_clad%DsE(row,N3)*cblk3_kc_HF(row,col))
           ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row-1,col)/(blk3_clad%DsS(row,col)*&
              cblk3_kc_HF(row-1,col)+blk3_clad%DsN(row-1,col)*cblk3_kc_HF(row,col))
           knp = cblk3_kc_HF(row,col)
              
              !ϵ������
           cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
           cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
           cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
           cblk3_an1(row,col) = 0.0

           CoordX = blk3_clad%n_x(row,col);CoordY = blk3_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)


           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if    
                
           damp = knp*blk3_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_clad%DsN(row,col)+knp)
           cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
           cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                         cblk3_an1(row,col)+damp
           cblk3_bp(row,col) = damp*tfluid                  
        
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_2clad%Ds2E(row,1)*cblk3_2kc_HF(row,1)*cblk3_2kc_HF(row,2)/(blk3_2clad%DsE(row,2)*&
              cblk3_2kc_HF(row,2)+blk3_2clad%DsW(row,2)*cblk3_2kc_HF(row,1))
           kwp = blk3_2clad%Ds2W(row,1)*cblk3_2kc_HF(row,1)*cblk4_2kc_HF(row,N5)/(blk3_2clad%DsW(row,1)*&
              cblk4_2kc_HF(row,N5)+blk4_2clad%DsE(row,N5)*cblk3_2kc_HF(row,1))
           ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row-1,col)/(blk3_2clad%DsS(row,col)*&
              cblk3_2kc_HF(row-1,col)+blk3_2clad%DsN(row-1,col)*cblk3_2kc_HF(row,col))
           knp = cblk3_2kc_HF(row,col)
              
              !ϵ������
           cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
           cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
           cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
           cblk3_2an1(row,col) = 0.0

           CoordX = blk3_2clad%n_x(row,col);CoordY = blk3_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)


           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then              
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if           
                        
           damp = knp*blk3_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_2clad%DsN(row,col)+knp)
           cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
           cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                         cblk3_2an1(row,col)+damp
           cblk3_2bp(row,col) = damp*tfluid
           
           !����
           col = 1
           row = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col+1)/(blk3_clad%DsE(row,col)*&
              cblk3_kc_HF(row,col+1)+blk3_clad%DsW(row,col+1)*cblk3_kc_HF(row,col))
           kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk2_kc_HF(row,N3)/(blk3_clad%DsW(row,col)*&
              cblk2_kc_HF(row,N3)+blk2_clad%DsE(row,N3)*cblk3_kc_HF(row,col))
           ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*fblk4_kc_HF(N6,col)/(blk3_clad%DsS(row,col)*&
              fblk4_kc_HF(N6,col)+blk4_fuel%DsN(N6,col)*cblk3_kc_HF(row,col))
           knp = blk3_clad%Ds2N(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row+1,col)/(blk3_clad%DsN(row,col)*&
              cblk3_kc_HF(row+1,col)+blk3_clad%DsS(row+1,col)*cblk3_kc_HF(row,col))
              
              !ϵ������
           cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
           cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
           cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
           cblk3_an1(row,col) = knp*blk3_clad%LsN(row,col)/blk3_clad%Ds2N(row,col)
              
           cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
           cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                         cblk3_an1(row,col)
           cblk3_bp(row,col) = 0.0
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_2clad%Ds2E(row,1)*cblk3_2kc_HF(row,1)*cblk3_2kc_HF(row,2)/(blk3_2clad%DsE(row,2)*&
              cblk3_2kc_HF(row,2)+blk3_2clad%DsW(row,2)*cblk3_2kc_HF(row,1))
           kwp = blk3_2clad%Ds2W(row,1)*cblk3_2kc_HF(row,1)*cblk4_2kc_HF(row,N5)/(blk3_2clad%DsW(row,1)*&
              cblk4_2kc_HF(row,N5)+blk4_2clad%DsE(row,N5)*cblk3_2kc_HF(row,1))
           ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*fblk4_2kc_HF(N6,col)/(blk3_2clad%DsS(row,col)*&
              fblk4_2kc_HF(N6,col)+blk4_2fuel%DsN(N6,col)*cblk3_2kc_HF(row,col))
           knp = blk3_2clad%Ds2N(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row+1,col)/(blk3_2clad%DsN(row,col)*&
              cblk3_2kc_HF(row+1,col)+blk3_2clad%DsS(row+1,col)*cblk3_2kc_HF(row,col))
              
              !ϵ������
           cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
           cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
           cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
           cblk3_2an1(row,col) = knp*blk3_2clad%LsN(row,col)/blk3_2clad%Ds2N(row,col)
              
           cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
           cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                         cblk3_2an1(row,col)
           cblk3_2bp(row,col) = 0.0           
  
           !����
           row = N1
           col = N4
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk4_kc_HF(row,1)/(blk3_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,1)+blk4_clad%DsW(row,1)*cblk3_kc_HF(row,col))
           kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col-1)/(blk3_clad%DsW(row,col)*&
              cblk3_kc_HF(row,col-1)+blk3_clad%DsE(row,col-1)*cblk3_kc_HF(row,col))
           ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row-1,col)/(blk3_clad%DsS(row,col)*&
              cblk3_kc_HF(row-1,col)+blk3_clad%DsN(row-1,col)*cblk3_kc_HF(row,col))
           knp = cblk3_kc_HF(row,col)
              
              !ϵ������
           cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
           cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
           cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
           cblk3_an1(row,col) = 0.0

           CoordX = blk3_clad%n_x(row,col);CoordY = blk3_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)
           

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if             
                    
           damp = knp*blk3_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_clad%DsN(row,col)+knp)
           cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
           cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                         cblk3_an1(row,col)+damp
           cblk3_bp(row,col) = damp*tfluid
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_2clad%Ds2E(row,col)*cblk3_2kc_HF(row,col)*cblk2_2kc_HF(row,1)/(blk3_2clad%DsE(row,col)*&
                   cblk2_2kc_HF(row,1)+blk2_2clad%DsW(row,1)*cblk3_2kc_HF(row,col))
           kwp = blk3_2clad%Ds2W(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col-1)/(blk3_2clad%DsW(row,col)*&
              cblk3_2kc_HF(row,col-1)+blk3_2clad%DsE(row,col-1)*cblk3_2kc_HF(row,col))
           ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row-1,col)/(blk3_2clad%DsS(row,col)*&
              cblk3_2kc_HF(row-1,col)+blk3_2clad%DsN(row-1,col)*cblk3_2kc_HF(row,col))
           knp = cblk3_2kc_HF(row,col)
              
              !ϵ������
           cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
           cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
           cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
           cblk3_2an1(row,col) = 0.0

           CoordX = blk3_2clad%n_x(row,col);CoordY = blk3_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)
           

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if             
                    
           damp = knp*blk3_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk3_2clad%DsN(row,col)+knp)
           cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
           cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                         cblk3_2an1(row,col)+damp
           cblk3_2bp(row,col) = damp*tfluid            
                
           !����
           row = 1
           col = N4
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_clad%Ds2E(row,col)*cblk3_kc_HF(row,col)*cblk4_kc_HF(row,1)/(blk3_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,1)+blk4_clad%DsW(row,1)*cblk3_kc_HF(row,col))
           kwp = blk3_clad%Ds2W(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row,col-1)/(blk3_clad%DsW(row,col)*&
              cblk3_kc_HF(row,col-1)+blk3_clad%DsE(row,col-1)*cblk3_kc_HF(row,col))
           ksp = blk3_clad%Ds2S(row,col)*cblk3_kc_HF(row,col)*fblk4_kc_HF(N6,col)/(blk3_clad%DsS(row,col)*&
               fblk4_kc_HF(N6,col)+blk4_fuel%DsN(N6,col)*cblk3_kc_HF(row,col))
           knp = blk3_clad%Ds2N(row,col)*cblk3_kc_HF(row,col)*cblk3_kc_HF(row+1,col)/(blk3_clad%DsN(row,col)*&
              cblk3_kc_HF(row+1,col)+blk3_clad%DsS(row+1,col)*cblk3_kc_HF(row,col))
              
              !ϵ������
           cblk3_ae1(row,col) = kep*blk3_clad%LsE(row,col)/blk3_clad%Ds2E(row,col)
           cblk3_aw1(row,col) = kwp*blk3_clad%LsW(row,col)/blk3_clad%Ds2W(row,col)
           cblk3_as1(row,col) = ksp*blk3_clad%LsS(row,col)/blk3_clad%Ds2S(row,col)
           cblk3_an1(row,col) = knp*blk3_clad%LsN(row,col)/blk3_clad%Ds2N(row,col)
              
           cblk3_ap0(row,col) = RCladding*cblk3_cp_HF(row,col)* blk3_clad%area(row,col)/dt_heat   !ʱ����
           cblk3_ap1(row,col) = cblk3_ap0(row,col)+cblk3_ae1(row,col)+cblk3_aw1(row,col)+cblk3_as1(row,col)+&
                         cblk3_an1(row,col)
           cblk3_bp(row,col) = 0.0 
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk3_2clad%Ds2E(row,col)*cblk3_2kc_HF(row,col)*cblk2_2kc_HF(row,1)/(blk3_2clad%DsE(row,col)*&
                   cblk2_2kc_HF(row,1)+blk2_2clad%DsW(row,1)*cblk3_2kc_HF(row,col))
           kwp = blk3_2clad%Ds2W(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row,col-1)/(blk3_2clad%DsW(row,col)*&
              cblk3_2kc_HF(row,col-1)+blk3_2clad%DsE(row,col-1)*cblk3_2kc_HF(row,col))
           ksp = blk3_2clad%Ds2S(row,col)*cblk3_2kc_HF(row,col)*fblk4_2kc_HF(N6,col)/(blk3_2clad%DsS(row,col)*&
               fblk4_2kc_HF(N6,col)+blk4_2fuel%DsN(N6,col)*cblk3_2kc_HF(row,col))
           knp = blk3_2clad%Ds2N(row,col)*cblk3_2kc_HF(row,col)*cblk3_2kc_HF(row+1,col)/(blk3_2clad%DsN(row,col)*&
              cblk3_2kc_HF(row+1,col)+blk3_2clad%DsS(row+1,col)*cblk3_2kc_HF(row,col))
              
              !ϵ������
           cblk3_2ae1(row,col) = kep*blk3_2clad%LsE(row,col)/blk3_2clad%Ds2E(row,col)
           cblk3_2aw1(row,col) = kwp*blk3_2clad%LsW(row,col)/blk3_2clad%Ds2W(row,col)
           cblk3_2as1(row,col) = ksp*blk3_2clad%LsS(row,col)/blk3_2clad%Ds2S(row,col)
           cblk3_2an1(row,col) = knp*blk3_2clad%LsN(row,col)/blk3_2clad%Ds2N(row,col)
              
           cblk3_2ap0(row,col) = RCladding*cblk3_2cp_HF(row,col)* blk3_2clad%area(row,col)/dt_heat   !ʱ����
           cblk3_2ap1(row,col) = cblk3_2ap0(row,col)+cblk3_2ae1(row,col)+cblk3_2aw1(row,col)+cblk3_2as1(row,col)+&
                         cblk3_2an1(row,col)
           cblk3_2bp(row,col) = 0.0            
        
           !!===========================================================�߽�������clad block4==============================================================================
           if (N5>2) then
              row = 1    !�±߽�,�� fuel block6����
              do col = 2,N5-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_clad%Ds2E(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col+1)/(blk4_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,col+1)+blk4_clad%DsW(row,col+1)*cblk4_kc_HF(row,col))
                kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col-1)/(blk4_clad%DsW(row,col)*&
                   cblk4_kc_HF(row,col-1)+blk4_clad%DsE(row,col-1)*cblk4_kc_HF(row,col))
                ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*fblk6_kc_HF(N6,col)/(blk4_clad%DsS(row,col)*&
                   fblk6_kc_HF(N6,col)+blk6_fuel%DsN(N6,col)*cblk4_kc_HF(row,col))
                knp = blk4_clad%Ds2N(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row+1,col)/(blk4_clad%DsN(row,col)*&
                   cblk4_kc_HF(row+1,col)+blk4_clad%DsS(row+1,col)*cblk4_kc_HF(row,col))
              
                   !ϵ������
                cblk4_ae1(row,col) = kep*blk4_clad%LsE(row,col)/blk4_clad%Ds2E(row,col)
                cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
                cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
                cblk4_an1(row,col) = knp*blk4_clad%LsN(row,col)/blk4_clad%Ds2N(row,col)
              
                cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
                cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                              cblk4_an1(row,col)
                cblk4_bp(row,col) = 0.0 
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2clad%Ds2E(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col+1)/(blk4_2clad%DsE(row,col)*&
                   cblk4_2kc_HF(row,col+1)+blk4_2clad%DsW(row,col+1)*cblk4_2kc_HF(row,col))
                kwp = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col-1)/(blk4_2clad%DsW(row,col)*&
                   cblk4_2kc_HF(row,col-1)+blk4_2clad%DsE(row,col-1)*cblk4_2kc_HF(row,col))
                ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*fblk6_2kc_HF(N6,col)/(blk4_2clad%DsS(row,col)*&
                   fblk6_2kc_HF(N6,col)+blk6_2fuel%DsN(N6,col)*cblk4_2kc_HF(row,col))
                knp = blk4_2clad%Ds2N(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row+1,col)/(blk4_2clad%DsN(row,col)*&
                   cblk4_2kc_HF(row+1,col)+blk4_2clad%DsS(row+1,col)*cblk4_2kc_HF(row,col))
              
                   !ϵ������
                cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
                cblk4_2aw1(row,col) = kwp*blk4_2clad%LsW(row,col)/blk4_2clad%Ds2W(row,col)
                cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
                cblk4_2an1(row,col) = knp*blk4_2clad%LsN(row,col)/blk4_2clad%Ds2N(row,col)
              
                cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col)/dt_heat   !ʱ����
                cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+cblk4_2as1(row,col)+&
                              cblk4_2an1(row,col)
                cblk4_2bp(row,col) = 0.0                 
              end do
           
              row = N1    !�ϱ߽�,������߽�����
              do col = 2,N5-1
                !! Region 1
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_clad%Ds2E(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col+1)/(blk4_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,col+1)+blk4_clad%DsW(row,col+1)*cblk4_kc_HF(row,col))
                kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col-1)/(blk4_clad%DsW(row,col)*&
                   cblk4_kc_HF(row,col-1)+blk4_clad%DsE(row,col-1)*cblk4_kc_HF(row,col))
                ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row-1,col)/(blk4_clad%DsS(row,col)*&
                   cblk4_kc_HF(row-1,col)+blk4_clad%DsN(row-1,col)*cblk4_kc_HF(row,col))
                knp = cblk4_kc_HF(row,col)
              
                   !ϵ������
                cblk4_ae1(row,col) = kep*blk4_clad%LsE(row,col)/blk4_clad%Ds2E(row,col)
                cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
                cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
                cblk4_an1(row,col) = 0.0

                CoordX = blk4_clad%n_x(row,col);CoordY = blk4_clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)


                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if                
                              
                damp = knp*blk4_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_clad%DsN(row,col)+knp)
                cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
                cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                              cblk4_an1(row,col)+damp
                cblk4_bp(row,col) = tfluid*damp
                
                !! Region 2
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2clad%Ds2E(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col+1)/(blk4_2clad%DsE(row,col)*&
                   cblk4_2kc_HF(row,col+1)+blk4_2clad%DsW(row,col+1)*cblk4_2kc_HF(row,col))
                kwp = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col-1)/(blk4_2clad%DsW(row,col)*&
                   cblk4_2kc_HF(row,col-1)+blk4_2clad%DsE(row,col-1)*cblk4_2kc_HF(row,col))
                ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row-1,col)/(blk4_2clad%DsS(row,col)*&
                   cblk4_2kc_HF(row-1,col)+blk4_2clad%DsN(row-1,col)*cblk4_2kc_HF(row,col))
                knp = cblk4_2kc_HF(row,col)
              
                   !ϵ������
                cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
                cblk4_2aw1(row,col) = kwp*blk4_2clad%LsW(row,col)/blk4_2clad%Ds2W(row,col)
                cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
                cblk4_2an1(row,col) = 0.0

                CoordX = blk4_2clad%n_x(row,col);CoordY = blk4_2clad%n_y(row,col)
                Angle = atan(CoordY/CoordX)

                NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
                if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
                   NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
                elseif ( Angle > 1.24) then
                   NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
                end if               
                              
                damp = knp*blk4_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_2clad%DsN(row,col)+knp)
                cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col)/dt_heat   !ʱ����
                cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+cblk4_2as1(row,col)+&
                              cblk4_2an1(row,col)+damp
                cblk4_2bp(row,col) = tfluid*damp                
              end do           
           end if

           if (N1>2) then
              col = 1    !�� cladding block3����
              do row = 2,N1-1
                !! Region 1, ��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_clad%Ds2E(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col+1)/(blk4_clad%DsE(row,col)*&
                   cblk4_kc_HF(row,col+1)+blk4_clad%DsW(row,col+1)*cblk4_kc_HF(row,col))
                kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk3_kc_HF(row,N4)/(blk4_clad%DsW(row,col)*&
                   cblk3_kc_HF(row,N4)+blk3_clad%DsE(row,N4)*cblk4_kc_HF(row,col))
                ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row-1,col)/(blk4_clad%DsS(row,col)*&
                   cblk4_kc_HF(row-1,col)+blk4_clad%DsN(row-1,col)*cblk4_kc_HF(row,col))
                knp = blk4_clad%Ds2N(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row+1,col)/(blk4_clad%DsN(row,col)*&
                   cblk4_kc_HF(row+1,col)+blk4_clad%DsS(row+1,col)*cblk4_kc_HF(row,col))
              
                   !ϵ������
                cblk4_ae1(row,col) = kep*blk4_clad%LsE(row,col)/blk4_clad%Ds2E(row,col)
                cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
                cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
                cblk4_an1(row,col) = knp*blk4_clad%LsN(row,col)/blk4_clad%Ds2N(row,col)
              
                cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
                cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                              cblk4_an1(row,col)
                cblk4_bp(row,col) = 0.0
                
                !! Region 2, �ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2clad%Ds2E(row,N5)*cblk4_2kc_HF(row,N5)*cblk3_2kc_HF(row,1)/(blk4_2clad%DsE(row,N5)*&
                   cblk3_2kc_HF(row,1)+blk3_2clad%DsW(row,1)*cblk4_2kc_HF(row,N5))
                kwp = blk4_2clad%Ds2W(row,N5)*cblk4_2kc_HF(row,N5)*cblk4_2kc_HF(row,N5-1)/(blk4_2clad%DsW(row,N5)*&
                   cblk4_2kc_HF(row,N5-1)+blk4_2clad%DsE(row,N5-1)*cblk4_2kc_HF(row,N5))
                ksp = blk4_2clad%Ds2S(row,N5)*cblk4_2kc_HF(row,N5)*cblk4_2kc_HF(row-1,N5)/(blk4_2clad%DsS(row,N5)*&
                   cblk4_2kc_HF(row-1,N5)+blk4_2clad%DsN(row-1,N5)*cblk4_2kc_HF(row,N5))
                knp = blk4_2clad%Ds2N(row,N5)*cblk4_2kc_HF(row,N5)*cblk4_2kc_HF(row+1,N5)/(blk4_2clad%DsN(row,N5)*&
                   cblk4_2kc_HF(row+1,N5)+blk4_2clad%DsS(row+1,N5)*cblk4_2kc_HF(row,N5))
              
                   !ϵ������
                cblk4_2ae1(row,N5) = kep*blk4_2clad%LsE(row,N5)/blk4_2clad%Ds2E(row,N5)
                cblk4_2aw1(row,N5) = kwp*blk4_2clad%LsW(row,N5)/blk4_2clad%Ds2W(row,N5)
                cblk4_2as1(row,N5) = ksp*blk4_2clad%LsS(row,N5)/blk4_2clad%Ds2S(row,N5)
                cblk4_2an1(row,N5) = knp*blk4_2clad%LsN(row,N5)/blk4_2clad%Ds2N(row,N5)
              
                cblk4_2ap0(row,N5) = RCladding*cblk4_2cp_HF(row,N5)* blk4_2clad%area(row,N5)/dt_heat   !ʱ����
                cblk4_2ap1(row,N5) = cblk4_2ap0(row,N5)+cblk4_2ae1(row,N5)+cblk4_2aw1(row,N5)+cblk4_2as1(row,N5)+&
                              cblk4_2an1(row,N5)
                cblk4_2bp(row,N5) = 0.0                
              end do

              col = N5    !��������
              do row = 2,N1-1
                !! Region 1,�ұ߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = cblk4_kc_HF(row,col)
                kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col-1)/(blk4_clad%DsW(row,col)*&
                   cblk4_kc_HF(row,col-1)+blk4_clad%DsE(row,col-1)*cblk4_kc_HF(row,col))
                ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row-1,col)/(blk4_clad%DsS(row,col)*&
                   cblk4_kc_HF(row-1,col)+blk4_clad%DsN(row-1,col)*cblk4_kc_HF(row,col))
                knp = blk4_clad%Ds2N(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row+1,col)/(blk4_clad%DsN(row,col)*&
                   cblk4_kc_HF(row+1,col)+blk4_clad%DsS(row+1,col)*cblk4_kc_HF(row,col))
              
                   !ϵ������
                cblk4_ae1(row,col) = 0.0
                cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
                cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
                cblk4_an1(row,col) = knp*blk4_clad%LsN(row,col)/blk4_clad%Ds2N(row,col)
              
                cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
                cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                              cblk4_an1(row,col)
                cblk4_bp(row,col) = 0.0
                
                !! Region 2,��߽�
                   !�ȵ��ʵ���ƽ��ֵ 
                kep = blk4_2clad%Ds2E(row,1)*cblk4_2kc_HF(row,1)*cblk4_2kc_HF(row,2)/(blk4_2clad%DsE(row,1)*&
                   cblk4_2kc_HF(row,2)+blk4_2clad%DsW(row,2)*cblk4_2kc_HF(row,1))
                kwp = cblk4_2kc_HF(row,1)
                ksp = blk4_2clad%Ds2S(row,1)*cblk4_2kc_HF(row,1)*cblk4_2kc_HF(row-1,1)/(blk4_2clad%DsS(row,1)*&
                   cblk4_2kc_HF(row-1,1)+blk4_2clad%DsN(row-1,1)*cblk4_2kc_HF(row,1))
                knp = blk4_2clad%Ds2N(row,1)*cblk4_2kc_HF(row,1)*cblk4_2kc_HF(row+1,1)/(blk4_2clad%DsN(row,1)*&
                   cblk4_2kc_HF(row+1,1)+blk4_2clad%DsS(row+1,1)*cblk4_2kc_HF(row,1))
              
                   !ϵ������
                cblk4_2ae1(row,1) = kep*blk4_2clad%LsE(row,1)/blk4_2clad%Ds2E(row,1)
                cblk4_2aw1(row,1) = 0.0
                cblk4_2as1(row,1) = ksp*blk4_2clad%LsS(row,1)/blk4_2clad%Ds2S(row,1)
                cblk4_2an1(row,1) = knp*blk4_2clad%LsN(row,1)/blk4_2clad%Ds2N(row,1)
              
                cblk4_2ap0(row,1) = RCladding*cblk4_2cp_HF(row,1)* blk4_2clad%area(row,1)/dt_heat   !ʱ����
                cblk4_2ap1(row,1) = cblk4_2ap0(row,1)+cblk4_2ae1(row,1)+cblk4_2aw1(row,1)+cblk4_2as1(row,1)+&
                              cblk4_2an1(row,1)
                cblk4_2bp(row,1) = 0.0                
              end do           
           end if
        
           !!�ĸ�����
           !����
           row = N1 
           col = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_clad%Ds2E(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col+1)/(blk4_clad%DsE(row,col)*&
              cblk4_kc_HF(row,col+1)+blk4_clad%DsW(row,col+1)*cblk4_kc_HF(row,col))
           kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk3_kc_HF(row,N4)/(blk4_clad%DsW(row,col)*&
              cblk3_kc_HF(row,N4)+blk3_clad%DsE(row,N4)*cblk4_kc_HF(row,col))
           ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row-1,col)/(blk4_clad%DsS(row,col)*&
              cblk4_kc_HF(row-1,col)+blk4_clad%DsN(row-1,col)*cblk4_kc_HF(row,col))
           knp = cblk4_kc_HF(row,col)
              
              !ϵ������
           cblk4_ae1(row,col) = kep*blk4_clad%LsE(row,col)/blk4_clad%Ds2E(row,col)
           cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
           cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
           cblk4_an1(row,col) = 0.0

           CoordX = blk4_clad%n_x(row,col);CoordY = blk4_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                              
                          
           damp = knp*blk4_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_clad%DsN(row,col)+knp)
           cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
           cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                         cblk4_an1(row,col)+damp
           cblk4_bp(row,col) = tfluid*damp          
          
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2clad%Ds2E(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col+1)/(blk4_2clad%DsE(row,col)*&
              cblk4_2kc_HF(row,col+1)+blk4_2clad%DsW(row,col+1)*cblk4_2kc_HF(row,col))
           kwp = cblk4_2kc_HF(row,col)
           ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row-1,col)/(blk4_2clad%DsS(row,col)*&
              cblk4_2kc_HF(row-1,col)+blk4_2clad%DsN(row-1,col)*cblk4_2kc_HF(row,col))
           knp = cblk4_2kc_HF(row,col)
              
              !ϵ������
           cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
           cblk4_2aw1(row,col) = 0.0
           cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
           cblk4_2an1(row,col) = 0.0

           CoordX = blk4_2clad%n_x(row,col);CoordY = blk4_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                                
                          
           damp = knp*blk4_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_2clad%DsN(row,col)+knp)
           cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col)/dt_heat   !ʱ����
           cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+cblk4_2as1(row,col)+&
                         cblk4_2an1(row,col)+damp
           cblk4_2bp(row,col) = tfluid*damp 
           
           !����
           row = 1 
           col = 1
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_clad%Ds2E(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col+1)/(blk4_clad%DsE(row,col)*&
              cblk4_kc_HF(row,col+1)+blk4_clad%DsW(row,col+1)*cblk4_kc_HF(row,col))
           kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk3_kc_HF(row,N4)/(blk4_clad%DsW(row,col)*&
              cblk3_kc_HF(row,N4)+blk3_clad%DsE(row,N4)*cblk4_kc_HF(row,col))
           ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*fblk6_kc_HF(N6,col)/(blk4_clad%DsS(row,col)*&
              fblk6_kc_HF(N6,col)+blk6_fuel%DsN(N6,col)*cblk4_kc_HF(row,col))
           knp = blk4_clad%Ds2N(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row+1,col)/(blk4_clad%DsN(row,col)*&
              cblk4_kc_HF(row+1,col)+blk4_clad%DsS(row+1,col)*cblk4_kc_HF(row,col))
              
              !ϵ������
           cblk4_ae1(row,col) = kep*blk4_clad%LsE(row,col)/blk4_clad%Ds2E(row,col)
           cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
           cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
           cblk4_an1(row,col) = knp*blk4_clad%LsN(row,col)/blk4_clad%Ds2N(row,col)
              
           cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
           cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                         cblk4_an1(row,col)
           cblk4_bp(row,col) = 0.0       
        
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2clad%Ds2E(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col+1)/(blk4_2clad%DsE(row,col)*&
              cblk4_2kc_HF(row,col+1)+blk4_2clad%DsW(row,col+1)*cblk4_2kc_HF(row,col))
           kwp = cblk4_2kc_HF(row,col)
           ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*fblk6_2kc_HF(N6,col)/(blk4_2clad%DsS(row,col)*&
              fblk6_2kc_HF(N6,col)+blk6_2fuel%DsN(N6,col)*cblk4_2kc_HF(row,col))
           knp = blk4_2clad%Ds2N(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row+1,col)/(blk4_2clad%DsN(row,col)*&
              cblk4_2kc_HF(row+1,col)+blk4_2clad%DsS(row+1,col)*cblk4_2kc_HF(row,col))
              
              !ϵ������
           cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
           cblk4_2aw1(row,col) = 0.0
           cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
           cblk4_2an1(row,col) = knp*blk4_2clad%LsN(row,col)/blk4_2clad%Ds2N(row,col)
              
           cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col)/dt_heat   !ʱ����
           cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+cblk4_2as1(row,col)+&
                         cblk4_2an1(row,col)
           cblk4_2bp(row,col) = 0.0        
           !����
           row = N1 
           col = N5
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = cblk4_kc_HF(row,col)
           kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col-1)/(blk4_clad%DsW(row,col)*&
              cblk4_kc_HF(row,col-1)+blk4_clad%DsE(row,col-1)*cblk4_kc_HF(row,col))
           ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row-1,col)/(blk4_clad%DsS(row,col)*&
              cblk4_kc_HF(row-1,col)+blk4_clad%DsN(row-1,col)*cblk4_kc_HF(row,col))
           knp = cblk4_kc_HF(row,col)
              
              !ϵ������
           cblk4_ae1(row,col) = 0.0
           cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
           cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
           cblk4_an1(row,col) = 0.0

           CoordX = blk4_clad%n_x(row,col);CoordY = blk4_clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512 .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                    
                          
           damp = knp*blk4_clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_clad%DsN(row,col)+knp)
           cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col)/dt_heat   !ʱ����
           cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+&
                         cblk4_an1(row,col)+damp
           cblk4_bp(row,col) = tfluid*damp
           
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk3_2kc_HF(row,1)/(blk4_2clad%DsE(row,col)*&
              cblk3_2kc_HF(row,1)+blk3_2clad%DsW(row,1)*cblk4_2kc_HF(row,col))
           kwp = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col-1)/(blk4_2clad%DsW(row,col)*&
              cblk4_2kc_HF(row,col-1)+blk4_2clad%DsE(row,col-1)*cblk4_2kc_HF(row,col))
           ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row-1,col)/(blk4_2clad%DsS(row,col)*&
              cblk4_2kc_HF(row-1,col)+blk4_2clad%DsN(row-1,col)*cblk4_2kc_HF(row,col))
           knp = cblk4_2kc_HF(row,col)
              
              !ϵ������
           cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
           cblk4_2aw1(row,col) = kwp*blk4_2clad%LsW(row,col)/blk4_2clad%Ds2W(row,col)
           cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
           cblk4_2an1(row,col) = 0.0

           CoordX = blk4_2clad%n_x(row,col);CoordY = blk4_2clad%n_y(row,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                    
                          
           damp = knp*blk4_2clad%LsN(row,col)*htcavg*NonUniF/(htcavg*NonUniF*blk4_2clad%DsN(row,col)+knp)
           cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col)/dt_heat   !ʱ����
           cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+cblk4_2as1(row,col)+&
                         cblk4_2an1(row,col)+damp
           cblk4_2bp(row,col) = tfluid*damp           
        
        
           !����
           row = 1 
           col = N5
           !! Region 1
              !�ȵ��ʵ���ƽ��ֵ 
           kep = cblk4_kc_HF(row,col)
           kwp = blk4_clad%Ds2W(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row,col-1)/(blk4_clad%DsW(row,col)*&
              cblk4_kc_HF(row,col-1)+blk4_clad%DsE(row,col-1)*cblk4_kc_HF(row,col))
           ksp = blk4_clad%Ds2S(row,col)*cblk4_kc_HF(row,col)*fblk6_kc_HF(N6,col)/(blk4_clad%DsS(row,col)*&
              fblk6_kc_HF(N6,col)+blk6_fuel%DsN(N6,col)*cblk4_kc_HF(row,col))
           knp = blk4_clad%Ds2N(row,col)*cblk4_kc_HF(row,col)*cblk4_kc_HF(row+1,col)/(blk4_clad%DsN(row,col)*&
              cblk4_kc_HF(row+1,col)+blk4_clad%DsS(row+1,col)*cblk4_kc_HF(row,col))
              
              !ϵ������
           cblk4_ae1(row,col) = 0.0
           cblk4_aw1(row,col) = kwp*blk4_clad%LsW(row,col)/blk4_clad%Ds2W(row,col)
           cblk4_as1(row,col) = ksp*blk4_clad%LsS(row,col)/blk4_clad%Ds2S(row,col)
           cblk4_an1(row,col) = knp*blk4_clad%LsN(row,col)/blk4_clad%Ds2N(row,col)
              
           cblk4_ap0(row,col) = RCladding*cblk4_cp_HF(row,col)* blk4_clad%area(row,col) /dt_heat   !ʱ����
           cblk4_ap1(row,col) = cblk4_ap0(row,col)+cblk4_ae1(row,col)+cblk4_aw1(row,col)+cblk4_as1(row,col)+cblk4_an1(row,col)
           cblk4_bp(row,col) = 0.0
        
           !! Region 2
              !�ȵ��ʵ���ƽ��ֵ 
           kep = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk3_2kc_HF(row,1)/(blk4_2clad%DsE(row,col)*&
              cblk3_2kc_HF(row,1)+blk3_2clad%DsW(row,1)*cblk4_2kc_HF(row,col))
           kwp = blk4_2clad%Ds2W(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row,col-1)/(blk4_2clad%DsW(row,col)*&
              cblk4_2kc_HF(row,col-1)+blk4_2clad%DsE(row,col-1)*cblk4_2kc_HF(row,col))
           ksp = blk4_2clad%Ds2S(row,col)*cblk4_2kc_HF(row,col)*fblk6_2kc_HF(N6,col)/(blk4_2clad%DsS(row,col)*&
              fblk6_2kc_HF(N6,col)+blk6_2fuel%DsN(N6,col)*cblk4_2kc_HF(row,col))
           knp = blk4_2clad%Ds2N(row,col)*cblk4_2kc_HF(row,col)*cblk4_2kc_HF(row+1,col)/(blk4_2clad%DsN(row,col)*&
              cblk4_2kc_HF(row+1,col)+blk4_2clad%DsS(row+1,col)*cblk4_2kc_HF(row,col))
              
              !ϵ������
           cblk4_2ae1(row,col) = kep*blk4_2clad%LsE(row,col)/blk4_2clad%Ds2E(row,col)
           cblk4_2aw1(row,col) = kwp*blk4_2clad%LsW(row,col)/blk4_2clad%Ds2W(row,col)
           cblk4_2as1(row,col) = ksp*blk4_2clad%LsS(row,col)/blk4_2clad%Ds2S(row,col)
           cblk4_2an1(row,col) = knp*blk4_2clad%LsN(row,col)/blk4_2clad%Ds2N(row,col)
              
           cblk4_2ap0(row,col) = RCladding*cblk4_2cp_HF(row,col)* blk4_2clad%area(row,col) /dt_heat   !ʱ����
           cblk4_2ap1(row,col) = cblk4_2ap0(row,col)+cblk4_2ae1(row,col)+cblk4_2aw1(row,col)+&
                        cblk4_2as1(row,col)+cblk4_2an1(row,col)
           cblk4_2bp(row,col) = 0.0        
        !!=====================================================Thermal conduction equation solution========================================================        
        !�¶ȳ����
        !! Region 1
           fblk1_tnsolid(:,:) = fblk1_tsolid(:,:);fblk2_tnsolid(:,:) = fblk2_tsolid(:,:)
           fblk3_tnsolid(:,:) = fblk3_tsolid(:,:);fblk4_tnsolid(:,:) = fblk4_tsolid(:,:)
           fblk5_tnsolid(:,:) = fblk5_tsolid(:,:);fblk6_tnsolid(:,:) = fblk6_tsolid(:,:)
           cblk1_tnsolid(:,:) = cblk1_tsolid(:,:);cblk2_tnsolid(:,:) = cblk2_tsolid(:,:)
           cblk3_tnsolid(:,:) = cblk3_tsolid(:,:);cblk4_tnsolid(:,:) = cblk4_tsolid(:,:)
           
        !! Region 2
           fblk1_2tnsolid(:,:) = fblk1_2tsolid(:,:);fblk2_2tnsolid(:,:) = fblk2_2tsolid(:,:)
           fblk3_2tnsolid(:,:) = fblk3_2tsolid(:,:);fblk4_2tnsolid(:,:) = fblk4_2tsolid(:,:)
           fblk5_2tnsolid(:,:) = fblk5_2tsolid(:,:);fblk6_2tnsolid(:,:) = fblk6_2tsolid(:,:)
           cblk1_2tnsolid(:,:) = cblk1_2tsolid(:,:);cblk2_2tnsolid(:,:) = cblk2_2tsolid(:,:)
           cblk3_2tnsolid(:,:) = cblk3_2tsolid(:,:);cblk4_2tnsolid(:,:) = cblk4_2tsolid(:,:)           
        
        !! �߽縳ֵ
        !! Region 1
           fblk1_te(1:N5)= fblk3_tsolid(1:N5,1);fblk1_te(N5+1:N5+N6) = fblk2_tsolid(1:N6,1)
           fblk1_tw(:)=fblk1_2tsolid(:,N2);fblk1_tn(:)=cblk1_tsolid(1,:);fblk1_ts(:)= fblk1_tsolid(1,:)
           fblk2_te(:)= fblk4_tsolid(:,1)
           fblk2_tn(:)=cblk2_tsolid(1,:)
           fblk3_te(:)= fblk5_tsolid(:,1)
           fblk3_tn(:)=fblk2_tsolid(1,:);fblk3_ts(:)= fblk3_tsolid(1,:)
           fblk4_te(:)= fblk6_tsolid(:,1)
           fblk4_tn(:)=cblk3_tsolid(1,:)
           fblk5_te(:)= fblk6_tsolid(1,N5:1:-1)
           fblk5_tn(:)=fblk4_tsolid(1,:);fblk5_ts(:)= fblk5_tsolid(1,:)
           fblk6_te(:)= fblk6_tsolid(:,N5)
           fblk6_tn(:)=cblk4_tsolid(1,:)
           
           cblk1_te(:) = cblk2_tsolid(:,1);cblk1_tw(:)=cblk1_2tsolid(:,N2)
           cblk1_tn(:)=cblk1_tsolid(N1,:)
           cblk2_te(:) = cblk3_tsolid(:,1)
           cblk2_tn(:)=cblk2_tsolid(N1,:)
           cblk3_te(:) = cblk4_tsolid(:,1)
           cblk3_tn(:)=cblk3_tsolid(N1,:)
           cblk4_te(:) = cblk4_tsolid(:,N5)
           cblk4_tn(:)=cblk4_tsolid(N1,:)                    
                        
           call equation_solution(Nfblk1,N2,fblk1_tsolid,fblk1_tnsolid,fblk1_tsolid0,fblk1_ap1,fblk1_ae1,fblk1_aw1,&
           fblk1_an1,fblk1_as1,fblk1_ap0,fblk1_bp,fblk1_te,fblk1_tw,fblk1_tn,fblk1_ts,fblk1_resmax)   !Fuel block 1

           fblk2_tw(:)=fblk1_tsolid(N5+1:N5+N6,N2);fblk3_tw(:)=fblk1_tsolid(1:N5,N2);cblk1_ts(:)= fblk1_tsolid(Nfblk1,:)           
                     
           call equation_solution(N5,N3,fblk3_tsolid,fblk3_tnsolid,fblk3_tsolid0,fblk3_ap1,fblk3_ae1,fblk3_aw1,&
           fblk3_an1,fblk3_as1,fblk3_ap0,fblk3_bp,fblk3_te,fblk3_tw,fblk3_tn,fblk3_ts,fblk3_resmax)  !Fuel block 3           

           fblk2_ts(:)= fblk3_tsolid(N5,:);fblk5_tw(:)=fblk3_tsolid(1:N5,N3)
                      
           call equation_solution(N6,N3,fblk2_tsolid,fblk2_tnsolid,fblk2_tsolid0,fblk2_ap1,fblk2_ae1,fblk2_aw1,&
           fblk2_an1,fblk2_as1,fblk2_ap0,fblk2_bp,fblk2_te,fblk2_tw,fblk2_tn,fblk2_ts,fblk2_resmax)  !Fuel block 2

           fblk4_tw(:)=fblk2_tsolid(1:N6,N3);cblk2_ts(:)= fblk2_tsolid(N6,:)
           
           call equation_solution(N5,N4,fblk5_tsolid,fblk5_tnsolid,fblk5_tsolid0,fblk5_ap1,fblk5_ae1,fblk5_aw1,&
           fblk5_an1,fblk5_as1,fblk5_ap0,fblk5_bp,fblk5_te,fblk5_tw,fblk5_tn,fblk5_ts,fblk5_resmax)  !Fuel block 5
           
           fblk4_ts(:)= fblk5_tsolid(N5,:);fblk6_ts(:)= fblk5_tsolid(N5:1:-1,N4)
           
           call equation_solution(N6,N4,fblk4_tsolid,fblk4_tnsolid,fblk4_tsolid0,fblk4_ap1,fblk4_ae1,fblk4_aw1,&
           fblk4_an1,fblk4_as1,fblk4_ap0,fblk4_bp,fblk4_te,fblk4_tw,fblk4_tn,fblk4_ts,fblk4_resmax)  !Fuel block 4 
           
           fblk6_tw(:)=fblk4_tsolid(:,N4);cblk3_ts(:)= fblk4_tsolid(N6,:)
           
           call equation_solution(N6,N5,fblk6_tsolid,fblk6_tnsolid,fblk6_tsolid0,fblk6_ap1,fblk6_ae1,fblk6_aw1,&
           fblk6_an1,fblk6_as1,fblk6_ap0,fblk6_bp,fblk6_te,fblk6_tw,fblk6_tn,fblk6_ts,fblk6_resmax)  !Fuel block 6
           
           cblk4_ts(:)= fblk6_tsolid(N6,:)
           
           call equation_solution(N1,N2,cblk1_tsolid,cblk1_tnsolid,cblk1_tsolid0,cblk1_ap1,cblk1_ae1,cblk1_aw1,&
           cblk1_an1,cblk1_as1,cblk1_ap0,cblk1_bp,cblk1_te,cblk1_tw,cblk1_tn,cblk1_ts,cblk1_resmax)   !Cladding block 1
           
           cblk2_tw(:)=cblk1_tsolid(1:N1,N2);cblk1_2te(:) = cblk1_tsolid(:,1)
           
           call equation_solution(N1,N3,cblk2_tsolid,cblk2_tnsolid,cblk2_tsolid0,cblk2_ap1,cblk2_ae1,cblk2_aw1,&
           cblk2_an1,cblk2_as1,cblk2_ap0,cblk2_bp,cblk2_te,cblk2_tw,cblk2_tn,cblk2_ts,cblk2_resmax)   !Cladding block 2
           
           cblk3_tw(:)=cblk2_tsolid(:,N3)
           
           call equation_solution(N1,N4,cblk3_tsolid,cblk3_tnsolid,cblk3_tsolid0,cblk3_ap1,cblk3_ae1,cblk3_aw1,&
           cblk3_an1,cblk3_as1,cblk3_ap0,cblk3_bp,cblk3_te,cblk3_tw,cblk3_tn,cblk3_ts,cblk3_resmax)   !Cladding block 3
           
           cblk4_tw(:)=cblk3_tsolid(:,N4)
           
           call equation_solution(N1,N5,cblk4_tsolid,cblk4_tnsolid,cblk4_tsolid0,cblk4_ap1,cblk4_ae1,cblk4_aw1,&
           cblk4_an1,cblk4_as1,cblk4_ap0,cblk4_bp,cblk4_te,cblk4_tw,cblk4_tn,cblk4_ts,cblk4_resmax)   !Cladding block 4

        !! Region 2
           
           fblk6_2tw(:)= fblk6_2tsolid(:,1);fblk6_2te(:)= fblk4_2tsolid(:,1)
           fblk6_2tn(:)=cblk4_2tsolid(1,:);fblk6_2ts(:)=fblk5_2tsolid(:,1)
           fblk5_2te(:)= fblk3_2tsolid(:,1);
           fblk5_2tn(:)=fblk4_2tsolid(1,:);fblk5_2ts(:)= fblk5_2tsolid(1,:)
           fblk4_2te(:)= fblk2_2tsolid(:,1)
           fblk4_2tn(:)=cblk3_2tsolid(1,:)
           fblk3_2te(1:N5)= fblk1_2tsolid(1:N5,1)
           fblk3_2tn(:)=fblk2_2tsolid(1,:);fblk3_2ts(:)= fblk3_2tsolid(1,:)
           fblk2_2te(1:N6)= fblk1_2tsolid(N5+1:N5+N6,1);fblk2_2tn(:)=cblk2_2tsolid(1,:)         
           fblk1_2te(:)= fblk1_tsolid(:,1);
           fblk1_2tn(:)=cblk1_2tsolid(1,:);fblk1_2ts(:)= fblk1_2tsolid(1,:)
           
           cblk4_2tw(:) = cblk4_2tsolid(:,1);cblk4_2te(:) = cblk3_2tsolid(:,1);
           cblk4_2tn(:)=cblk4_2tsolid(N1,:)
           cblk3_2te(:) = cblk2_2tsolid(:,1);cblk3_2tn(:)=cblk3_2tsolid(N1,:)           
           cblk2_2te(:) = cblk1_2tsolid(:,1);cblk2_2tn(:)=cblk2_2tsolid(N1,:)                      
          
           cblk1_2tn(:)=cblk1_2tsolid(N1,:)
           
           call equation_solution(N6,N5,fblk6_2tsolid,fblk6_2tnsolid,fblk6_2tsolid0,fblk6_2ap1,fblk6_2ae1,fblk6_2aw1,&
           fblk6_2an1,fblk6_2as1,fblk6_2ap0,fblk6_2bp,fblk6_2te,fblk6_2tw,fblk6_2tn,fblk6_2ts,fblk6_2resmax)  !Fuel block 6
           
           cblk4_2ts(:)= fblk6_2tsolid(N6,:);fblk5_2tw(:)=fblk6_2tsolid(1,:); fblk4_2tw(:)=fblk6_2tsolid(:,N5)
           if(z .eq. 1) then
             !write(*,*) iter,fblk4_2tw(N6),fblk4_2tsolid(N6-1,1),fblk4_2tsolid(N6,2),fblk4_2tn(1)
           endif
           
           call equation_solution(N5,N4,fblk5_2tsolid,fblk5_2tnsolid,fblk5_2tsolid0,fblk5_2ap1,fblk5_2ae1,fblk5_2aw1,&
           fblk5_2an1,fblk5_2as1,fblk5_2ap0,fblk5_2bp,fblk5_2te,fblk5_2tw,fblk5_2tn,fblk5_2ts,fblk5_2resmax)  !Fuel block 5
           
           fblk4_2ts(:)= fblk5_2tsolid(N5,:); fblk3_2tw(:)= fblk5_2tsolid(:,N4)
           
           call equation_solution(N6,N4,fblk4_2tsolid,fblk4_2tnsolid,fblk4_2tsolid0,fblk4_2ap1,fblk4_2ae1,fblk4_2aw1,&
           fblk4_2an1,fblk4_2as1,fblk4_2ap0,fblk4_2bp,fblk4_2te,fblk4_2tw,fblk4_2tn,fblk4_2ts,fblk4_2resmax)  !Fuel block 4 
           
           fblk2_2tw(:)= fblk4_2tsolid(:,N4);cblk3_2ts(:)=fblk4_2tsolid(N6,:)   
           
           call equation_solution(N5,N3,fblk3_2tsolid,fblk3_2tnsolid,fblk3_2tsolid0,fblk3_2ap1,fblk3_2ae1,fblk3_2aw1,&
           fblk3_2an1,fblk3_2as1,fblk3_2ap0,fblk3_2bp,fblk3_2te,fblk3_2tw,fblk3_2tn,fblk3_2ts,fblk3_2resmax)  !Fuel block 3           

           fblk2_2ts(:)= fblk3_2tsolid(N5,:);fblk1_2tw(1:N5)= fblk3_2tsolid(1:N5,N3) 
           
           call equation_solution(N6,N3,fblk2_2tsolid,fblk2_2tnsolid,fblk2_2tsolid0,fblk2_2ap1,fblk2_2ae1,fblk2_2aw1,&
           fblk2_2an1,fblk2_2as1,fblk2_2ap0,fblk2_2bp,fblk2_2te,fblk2_2tw,fblk2_2tn,fblk2_2ts,fblk2_2resmax)  !Fuel block 2

           cblk2_2ts(:)= fblk2_2tsolid(N6,:); fblk1_2tw(N5+1:N5+N6)= fblk2_2tsolid(1:N6,N3);cblk2_2tn(:)=fblk2_2tsolid(N6,:)           
           
           call equation_solution(Nfblk1,N2,fblk1_2tsolid,fblk1_2tnsolid,fblk1_2tsolid0,fblk1_2ap1,fblk1_2ae1,fblk1_2aw1,&
           fblk1_2an1,fblk1_2as1,fblk1_2ap0,fblk1_2bp,fblk1_2te,fblk1_2tw,fblk1_2tn,fblk1_2ts,fblk1_2resmax)   !Fuel block 1

           cblk1_2ts(:)= fblk1_2tsolid(Nfblk1,:)                   

           call equation_solution(N1,N5,cblk4_2tsolid,cblk4_2tnsolid,cblk4_2tsolid0,cblk4_2ap1,cblk4_2ae1,cblk4_2aw1,&
           cblk4_2an1,cblk4_2as1,cblk4_2ap0,cblk4_2bp,cblk4_2te,cblk4_2tw,cblk4_2tn,cblk4_2ts,cblk4_2resmax)   !Cladding block 4
           
           cblk3_2tw(:)= cblk4_2tsolid(:,N5)

           call equation_solution(N1,N4,cblk3_2tsolid,cblk3_2tnsolid,cblk3_2tsolid0,cblk3_2ap1,cblk3_2ae1,cblk3_2aw1,&
           cblk3_2an1,cblk3_2as1,cblk3_2ap0,cblk3_2bp,cblk3_2te,cblk3_2tw,cblk3_2tn,cblk3_2ts,cblk3_2resmax)   !Cladding block 3
           
           cblk2_2tw(:) = cblk3_2tsolid(:,N4)
           
           call equation_solution(N1,N3,cblk2_2tsolid,cblk2_2tnsolid,cblk2_2tsolid0,cblk2_2ap1,cblk2_2ae1,cblk2_2aw1,&
           cblk2_2an1,cblk2_2as1,cblk2_2ap0,cblk2_2bp,cblk2_2te,cblk2_2tw,cblk2_2tn,cblk2_2ts,cblk2_2resmax)   !Cladding block 2
           
           cblk1_2tw(:)=cblk2_2tsolid(:,N3)
           
           call equation_solution(N1,N2,cblk1_2tsolid,cblk1_2tnsolid,cblk1_2tsolid0,cblk1_2ap1,cblk1_2ae1,cblk1_2aw1,&
           cblk1_2an1,cblk1_2as1,cblk1_2ap0,cblk1_2bp,cblk1_2te,cblk1_2tw,cblk1_2tn,cblk1_2ts,cblk1_2resmax)   !Cladding block 1        
           
           resmax1 = max(fblk1_resmax,fblk2_resmax,fblk3_resmax,fblk4_resmax,fblk5_resmax,&
                    fblk6_resmax,cblk1_resmax,cblk2_resmax,cblk3_resmax,cblk4_resmax,&
                    fblk1_2resmax,fblk2_2resmax,fblk3_2resmax,fblk4_2resmax,fblk5_2resmax,&
                    fblk6_2resmax,cblk1_2resmax,cblk2_2resmax,cblk3_2resmax,cblk4_2resmax)

                    if (debuging) then
                     if(mod(iter,3000)==0) then
                       print *,"    --iter:",iter,' solved'
                     end if
                    end if 
           !if(z .eq. 1) then
           !do row = 1,Nfblk1
           !  do col = 1, N2
           !    !write(*,*) "fblk1-2",iter, row,col,fblk1_2tsolid(row,col)
           !  enddo
           !enddo
           !do row = 1, N6
           !  do col = 1,N3
           !     !write(*,*) "fblk2-2",iter, row,col,fblk2_2tsolid(row,col)            
           !  enddo
           !enddo
           !do row = 1, N6
             !do col = 1,N4
                !write(*,*) "fblk4-2",iter, row,col,fblk4_2tsolid(row,col),fblk4_tsolid(row,N4+1-col),&
                    !fblk4_2ae1(row,col),fblk4_aw1(row,N4+1-col),fblk4_2aw1(row,col),fblk4_ae1(row,N4+1-col),&
                    !fblk4_2as1(row,col),fblk4_as1(row,N4+1-col),fblk4_2an1(row,col),fblk4_an1(row,N4+1-col),&
                    !fblk4_2aP1(row,col),fblk4_aP1(row,N4+1-col),fblk4_2bp(row,col),fblk4_bp(row,N4+1-col)           
             !enddo
           !enddo 
           ! do row = 1, N6
           !  do col = 1,N4
           !     !write(*,*) "fblk4",iter, row,col,fblk4_tsolid(row,col),fblk4_ae1(row,col),fblk4_aw1(row,col),&
           !                    !fblk4_as1(row,col),fblk4_an1(row,col),fblk4_bp(row,col)           
           !  enddo
           !enddo            
            !do row = 1, N6
             !do col = 1,N5
                !write(*,*) "fblk6-2",iter, row,col,fblk6_2tsolid(row,col),fblk6_tsolid(row,N5+1-col),&
                    !fblk6_2ae1(row,col),fblk6_aw1(row,N5+1-col),fblk6_2aw1(row,col),fblk6_ae1(row,N5+1-col),&
                    !fblk6_2as1(row,col),fblk6_as1(row,N5+1-col),fblk6_2an1(row,col),fblk6_an1(row,N5+1-col),&
                    !fblk6_2aP1(row,col),fblk6_aP1(row,N5+1-col),fblk6_2bp(row,col),fblk6_bp(row,N5+1-col) 
             !enddo
           !enddo
           !if (HF_first_iter .eqv. .false.) then
             !write(*,*) "1"
             !HF_first_iter = .True.
             !open(unit = 21,file = 'HF_heat_conduction-Clad1-debug.out',form="formatted") 
             !write(21,*) "    z  ","  iter  "," row  ","  col  ","  ae  ","  aw  ","  an  ",&
           !  "  as  ","  ap1  ","  bp  ","  T  ","  kc  "
           !  open(unit = 22,file = 'HF_heat_conduction-Fuel1-debug.out',form="formatted") 
           !  write(22,*)  "    z  ","  iter  "," row  ","  col  ","  ae  ","  aw  ","  an  ",&
           !  "  as  ","  ap1  ","  bp  ","  T  ","  kc  "
           !  open(unit = 23,file = 'HF_heat_conduction-Clad3-debug.out',form="formatted") 
           !  write(23,*)  "    z  ","  iter  "," row  ","  col  ","  ae  ","  aw  ","  an  ",&
           !  "  as  ","  ap1  ","  bp  ","  T  ","  kc  "          
           !endif
           !if(z .eq. 1) then
           !do row = 1,N1
             !do col = 1,N2
               !write(21,10016) z,iter,row,col,cblk1_ae1(row,col),cblk1_aw1(row,col),cblk1_an1(row,col),&
               !cblk1_as1(row,col),cblk1_ap1(row,col), cblk1_bp(row,col),cblk1_tsolid(row,col),cblk1_kc_HF(row,col)
             !enddo        
           !end do 
           !do row = 1,Nfblk1
           !  do col = 1,N2
           !    write(22,10016) z,iter,row,col,fblk1_ae1(row,col),fblk1_aw1(row,col),fblk1_an1(row,col),&
           !    fblk1_as1(row,col),fblk1_ap1(row,col),fblk1_bp(row,col),fblk1_tsolid(row,col),fblk1_kc_HF(row,col)
           !  enddo        
           !end do
           !do row = 1,N1
           !  do col = 1,N4
           !    write(23,10016) z,iter,row,col,cblk3_ae1(row,col),cblk3_aw1(row,col),cblk3_an1(row,col),&
           !    cblk3_as1(row,col),cblk3_ap1(row,col),cblk3_bp(row,col),cblk3_tsolid(row,col),cblk3_kc_HF(row,col)
           !  enddo        
           !end do
           !endif           
10016        format(4x,i3,4x,i4,4x,i3,4x,i3,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8,4x,f15.8)              
           !write(*,*) z,iter,fblk1_resmax,cblk1_resmax,cblk3_resmax
           if(resmax1 < maxresi) exit           
        end do
        
        !!==============================�¶ȳ���������������ºͱ��������ܶ�====================================
        !! Region 1
        !Cladding block1
        do col = 1,N2
           !knp = 2*cblk1_kc_HF(N1,col)*cblk1_kc_HF(N1+1,col)/(cblk1_kc_HF(N1,col)+cblk1_kc_HF(N1+1,col))
           knp = cblk1_kc_HF(N1,col)
           CoordX = blk1_clad%n_x(N1,col);CoordY = blk1_clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                    
                                    
           cblk1_twal(col)=cblk1_tsolid(N1,col)-(cblk1_tsolid(N1,col)-tfluid)/(1+knp/&
           (htcavg*NonUniF*blk1_clad%DsN(N1,col)))
           cblk1_tsolid(N1+1,col) = cblk1_twal(col)
           !cblk1_heatflux(col)=(cblk1_tsolid(N1,col)-tfluid)/(blk1_clad%DsN(N1,col)/cblk1_kc_HF(N1,col)+1/(htcavg*NonUniF))
           cblk1_heatflux(col) = (cblk1_twal(col)-tfluid)*(htcavg*NonUniF)
          
        end do
        !Cladding block2
        do col = 1,N3
           CoordX = blk2_clad%n_x(N1,col);CoordY = blk2_clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)
           
           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if              
                            
           cblk2_twal(col)=cblk2_tsolid(N1,col)-(cblk2_tsolid(N1,col)-tfluid)/(1+cblk2_kc_HF(N1,col)/&
           (htcavg*NonUniF*blk2_clad%DsN(N1,col)))
           !cblk2_heatflux(col) = (cblk2_tsolid(N1,col)-tfluid)/(blk2_clad%DsN(N1,col)/cblk2_kc_HF(N1,col)+1/(htcavg*NonUniF))
           cblk2_heatflux(col) = (cblk2_twal(col)-tfluid)*(htcavg*NonUniF)
        end do
        !Cladding block3
        do col = 1,N4
           CoordX = blk3_clad%n_x(N1,col);CoordY = blk3_clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if           
                   
           cblk3_twal(col)=cblk3_tsolid(N1,col)-(cblk3_tsolid(N1,col)-tfluid)/(1+cblk3_kc_HF(N1,col)/&
           (htcavg*NonUniF*blk3_clad%DsN(N1,col)))
           !cblk3_heatflux(col) = (cblk3_tsolid(N1,col)-tfluid)/(blk3_clad%DsN(N1,col)/cblk3_kc_HF(N1,col)+1/(htcavg*NonUniF))
           cblk3_heatflux(col) = (cblk3_twal(col)-tfluid)*(htcavg*NonUniF)
        end do        
        !Cladding block4
        do col = 1,N5
           CoordX = blk4_clad%n_x(N1,col);CoordY = blk4_clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if 
                   
           cblk4_twal(col)=cblk4_tsolid(N1,col)-(cblk4_tsolid(N1,col)-tfluid)/(1+cblk4_kc_HF(N1,col)/&
           (htcavg*NonUniF*blk4_clad%DsN(N1,col)))
           !cblk4_heatflux(col) = (cblk4_tsolid(N1,col)-tfluid)/(blk4_clad%DsN(N1,col)/cblk4_kc_HF(N1,col)+1/(htcavg*NonUniF))
           cblk4_heatflux(col) = (cblk4_twal(col)-tfluid)*(htcavg*NonUniF)          
        end do
        
        !! Region 2
        !Cladding block1_2
        do col = 1,N2
           !knp = 2*cblk1_kc_HF(N1,col)*cblk1_kc_HF(N1+1,col)/(cblk1_kc_HF(N1,col)+cblk1_kc_HF(N1+1,col))
           knp = cblk1_2kc_HF(N1,col)
           CoordX = blk1_2clad%n_x(N1,col);CoordY = blk1_2clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if                    
                                    
           cblk1_2twal(col)=cblk1_2tsolid(N1,col)-(cblk1_2tsolid(N1,col)-tfluid)/(1+knp/&
           (htcavg*NonUniF*blk1_2clad%DsN(N1,col)))
           cblk1_2tsolid(N1+1,col) = cblk1_2twal(col)
           !cblk1_heatflux(col)=(cblk1_tsolid(N1,col)-tfluid)/(blk1_clad%DsN(N1,col)/cblk1_kc_HF(N1,col)+1/(htcavg*NonUniF))
           cblk1_2heatflux(col) = (cblk1_2twal(col)-tfluid)*(htcavg*NonUniF)
          
        end do
        !Cladding block2_2
        do col = 1,N3
           CoordX = blk2_2clad%n_x(N1,col);CoordY = blk2_2clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)
           
           
           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if             
                            
           cblk2_2twal(col)=cblk2_2tsolid(N1,col)-(cblk2_2tsolid(N1,col)-tfluid)/(1+cblk2_2kc_HF(N1,col)/&
           (htcavg*NonUniF*blk2_2clad%DsN(N1,col)))
           !cblk2_2heatflux(col) = (cblk2_2tsolid(N1,col)-tfluid)/(blk2_2clad%DsN(N1,col)/cblk2_2kc_2HF(N1,col)+1/(htcavg*NonUniF))
           cblk2_2heatflux(col) = (cblk2_2twal(col)-tfluid)*(htcavg*NonUniF)
        end do
        !Cladding block3_2
        do col = 1,N4
           CoordX = blk3_2clad%n_x(N1,col);CoordY = blk3_2clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then               
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if            
                   
           cblk3_2twal(col)=cblk3_2tsolid(N1,col)-(cblk3_2tsolid(N1,col)-tfluid)/(1+cblk3_2kc_HF(N1,col)/&
           (htcavg*NonUniF*blk3_2clad%DsN(N1,col)))
           !cblk3_2heatflux(col) = (cblk3_2tsolid(N1,col)-tfluid)/(blk3_2clad%DsN(N1,col)/cblk3_2kc_2HF(N1,col)+1/(htcavg*NonUniF))
           cblk3_2heatflux(col) = (cblk3_2twal(col)-tfluid)*(htcavg*NonUniF)
        end do        
        !Cladding block4_2
        do col = 1,N5
           CoordX = blk4_2clad%n_x(N1,col);CoordY = blk4_2clad%n_y(N1,col)
           Angle = atan(CoordY/CoordX)

           NonUniF = a1*angle**3+a2*angle**2+a3*angle+a4
                           
           if ( Angle .gt. 0.3512  .and. Angle .le. 1.24) then                
              NonUniF = b1*angle**3+b2*angle**2+b3*angle+b4
           elseif ( Angle > 1.24) then
              NonUniF = c1*angle**3+c2*angle**2+c3*angle+c4
           end if 
                   
           cblk4_2twal(col)=cblk4_2tsolid(N1,col)-(cblk4_2tsolid(N1,col)-tfluid)/(1+cblk4_2kc_HF(N1,col)/&
           (htcavg*NonUniF*blk4_2clad%DsN(N1,col)))
           !cblk4_2heatflux(col) = (cblk4_2tsolid(N1,col)-tfluid)/(blk4_2clad%DsN(N1,col)/cblk4_2kc_2HF(N1,col)+1/(htcavg*NonUniF))
           cblk4_2heatflux(col) = (cblk4_2twal(col)-tfluid)*(htcavg*NonUniF)          
        end do        
        
        !! ��������
        !! Region 1
        HFrod%fblk1_T(:,:,z) = fblk1_tsolid(:,:);HFrod%fblk2_T(:,:,z) = fblk2_tsolid(:,:)
        HFrod%fblk3_T(:,:,z) = fblk3_tsolid(:,:);HFrod%fblk4_T(:,:,z) = fblk4_tsolid(:,:)
        HFrod%fblk5_T(:,:,z) = fblk5_tsolid(:,:);HFrod%fblk6_T(:,:,z) = fblk6_tsolid(:,:)
        HFrod%cblk1_T(:,:,z) = cblk1_tsolid(:,:);HFrod%cblk2_T(:,:,z) = cblk2_tsolid(:,:)
        HFrod%cblk3_T(:,:,z) = cblk3_tsolid(:,:);HFrod%cblk4_T(:,:,z) = cblk4_tsolid(:,:)
        HFrod%twall(N5+N4+N3+N2+1:N5+N4+N3+N2+N2,z) = cblk1_twal(1:N2)
        HFrod%twall(N5+N4+N3+N2+N2+1:N5+N4+N3+N2+N2+N3,z) = cblk2_twal(1:N3)
        HFrod%twall(N5+N4+N3+N2+N2+N3+1:N5+N4+N3+N2+N2+N3+N4,z) = cblk3_twal(1:N4)
        HFrod%twall(N5+N4+N3+N2+N2+N3+N4+1:N5+N4+N3+N2+N2+N3+N4+N5,z) = cblk4_twal(1:N5)
        
        HFrod%heatflux(N5+N4+N3+N2+1:N5+N4+N3+N2+N2,z) = cblk1_heatflux(1:N2);
        HFrod%heatflux(N5+N4+N3+N2+N2+1:N5+N4+N3+N2+N2+N3,z) = cblk2_heatflux(1:N3)
        HFrod%heatflux(N5+N4+N3+N2+N2+N3+1:N5+N4+N3+N2+N2+N3+N4,z) = cblk3_heatflux(1:N4)
        HFrod%heatflux(N5+N4+N3+N2+N2+N3+N4+1:N5+N4+N3+N2+N2+N3+N4+N5,z)=cblk4_heatflux(1:N5)
        
        !! Region 2
        HFrod%fblk1_2T(:,:,z) = fblk1_2tsolid(:,:);HFrod%fblk2_2T(:,:,z) = fblk2_2tsolid(:,:)
        HFrod%fblk3_2T(:,:,z) = fblk3_2tsolid(:,:);HFrod%fblk4_2T(:,:,z) = fblk4_2tsolid(:,:)
        HFrod%fblk5_2T(:,:,z) = fblk5_2tsolid(:,:);HFrod%fblk6_2T(:,:,z) = fblk6_2tsolid(:,:)
        HFrod%cblk1_2T(:,:,z) = cblk1_2tsolid(:,:);HFrod%cblk2_2T(:,:,z) = cblk2_2tsolid(:,:)
        HFrod%cblk3_2T(:,:,z) = cblk3_2tsolid(:,:);HFrod%cblk4_2T(:,:,z) = cblk4_2tsolid(:,:)
        HFrod%twall(1:N5,z) = cblk4_2twal(1:N5); HFrod%twall(N5+1:N5+N4,z) = cblk3_2twal(1:N4)
        HFrod%twall(N5+N4+1:N5+N4+N3,z) = cblk2_2twal(1:N3)
        HFrod%twall(N5+N4+N3+1:N5+N4+N3+N2,z) = cblk1_2twal(1:N2)
        
        HFrod%heatflux(1:N5,z) = cblk4_2heatflux(1:N5); HFrod%heatflux(N5+1:N5+N4,z) = cblk3_2heatflux(1:N4)
        HFrod%heatflux(N5+N4+1:N5+N4+N3,z) = cblk2_2heatflux(1:N3)
        HFrod%heatflux(N5+N4+N3+1:N5+N4+N3+N2,z)=cblk1_2heatflux(1:N2)        
        
        !do row = 1,Nfblk1
          !do col = 1,N2
            !write(*,*) row,col,z,fblk1_kc_HF(row,col),fblk1_cp_HF(row,col),blk1_fuel%DsW(row,col),blk1_fuel%LsE(row,col)
            !write(*,*) row,col,z,fblk1_ae1(row,col),fblk1_aw1(row,col),fblk1_an1(row,col),fblk1_as1(row,col)
            !write(*,*) row,col,z,fblk1_tsolid(row,col),fblk1_tnsolid(row,col),fblk1_tsolid0(row,col),fblk1_ap1(row,col),&
                    !fblk1_ae1(row,col),fblk1_aw1(row,col),fblk1_an1(row,col),fblk1_as1(row,col),fblk1_ap0(row,col),&
                    !fblk1_bp(row,col),fblk1_te(row),fblk1_tw(row),fblk1_tn(col),fblk1_ts(col)
          !enddo        
        !end do                
       end do
    end do          
    
  end subroutine solve_HF_heat_conduction
  
  subroutine get_coef_matrix(row_num,col_num,blk,rho,kc,cp,blk_ae1,blk_aw1,blk_as1,blk_an1,blk_ap0,blk_ap1,blk_bp,dt,vPower)
  
   implicit none
   integer, intent(in) :: row_num,col_num
   real,intent(in) :: rho,dt,vPower
   real,intent(in) :: kc(:,:),cp(:,:)
   type(block), intent(in) :: blk
   real,intent(in out) :: blk_ae1(:,:),blk_aw1(:,:),blk_as1(:,:),blk_an1(:,:),blk_ap0(:,:),blk_ap1(:,:),blk_bp(:,:) 
   real :: ap1,ap2,ae,aw,an,as,kep,kwp,ksp,knp
   integer :: row,col
      
   do row = 2,row_num-1
      do col = 2,col_num-1
        !�ȵ��ʵ���ƽ��ֵ              
        kep = blk%Ds2E(row,col)*kc(row,col)*kc(row,col+1)/(blk%DsE(row,col)*kc(row,col+1)+blk%DsW(row,col+1)*kc(row,col))
        kwp = blk%Ds2W(row,col)*kc(row,col)*kc(row,col-1)/(blk%DsW(row,col)*kc(row,col-1)+blk%DsE(row,col-1)*kc(row,col))
        ksp = blk%Ds2S(row,col)*kc(row,col)*kc(row-1,col)/(blk%DsS(row,col)*kc(row-1,col)+blk%DsN(row-1,col)*kc(row,col))
        knp = blk%Ds2N(row,col)*kc(row,col)*kc(row+1,col)/(blk%DsN(row,col)*kc(row+1,col)+blk%DsS(row+1,col)*kc(row,col))
              
        !ϵ������
        blk_ae1(row,col) = kep*blk%LsE(row,col)/blk%Ds2E(row,col)
        blk_aw1(row,col) = kwp*blk%LsW(row,col)/blk%Ds2W(row,col)
        blk_as1(row,col) = ksp*blk%LsS(row,col)/blk%Ds2S(row,col)
        blk_an1(row,col) = knp*blk%LsN(row,col)/blk%Ds2N(row,col)
              
        blk_ap0(row,col) = rho*cp(row,col)* blk%area(row,col)/dt  !ʱ����
        blk_ap1(row,col) = blk_ap0(row,col)+blk_ae1(row,col)+blk_aw1(row,col)+blk_as1(row,col)+blk_an1(row,col)
        blk_bp(row,col) = vPower*blk%area(row,col)        
      end do                 
   end do
  end subroutine get_coef_matrix
  
  subroutine get_HF_fuel_props(T,kc_fuel,cp_fuel) !temp to kc and cp
    !use Solidprops,   only: material_props
    use HF_init , only : material_props
    implicit none
    real, intent(in) :: T
    real, intent(out) :: kc_fuel,cp_fuel
    real :: tref,tref_k,tref_C,kc,cp
    
    !fuel
    tref = T
    call material_props(imat=matfuel,t=tref,cp=cp,k=kc) ! guess kc = heat transfer eff. 
    kc_fuel = kc
    cp_fuel = CP
    
    return
   end subroutine get_HF_fuel_props 
   
  subroutine get_HF_clad_props(T,kc_clad,cp_clad)
    !use Solidprops,   only: material_props
    use HF_init , only : material_props
    implicit none
    real, intent(in) :: T
    real, intent(out) :: kc_clad,cp_clad
    real :: tref,tref_C,tref_k,kc,cp      
    !clad
    a0 = 7.73e-2
    a1 = 3.15e-4
    a2 = -2.87e-7
    a3 = 1.5523e-10
    
    b0 = 286.5
    b1 = 0.1    
    
    tref = T
    tref_C = (tref-32)/1.8
    tref_k = tref_C+273.15
    if (matclad ==0) then
      kc = a0+a1*tref_C+a2*tref_C**2+a3*tref_C**3  !W/cm-C
      if (tref_C < 750) then
        cp = b0 + b1*tref_C              !J/(kg-C)
      else
        cp = 360
      endif
      kc = kc * 57.8      !W/cm-C -> btu/(h-ft-F)
      cp = cp * 2.38842e-4     !J/(kg-C) ->btu/(lbm-F)
    else
      call material_props(imat=matclad,t=tref,cp=cp,k=kc)
    endif
    kc_clad = kc
    cp_clad = cp
    
    return
  end subroutine get_HF_clad_props
  
  subroutine equation_solution(M,N,tsolid,tnsolid,tsolid0,ap1,ae1,aw1,an1,as1,ap0,bp,te,tw,tn,ts,resmax)
    
    implicit none
    integer,intent(in) :: M,N
    real,intent(in)::tnsolid(:,:),tsolid0(:,:),ap1(:,:),ae1(:,:),aw1(:,:),an1(:,:),as1(:,:),ap0(:,:),bp(:,:)
    real,intent(in)::te(:),tw(:),tn(:),ts(:)
    real,intent(in out) :: tsolid(:,:),resmax
    !Local variables
    integer :: ii,jj
    
    !����
    ii = 1; jj = 1
    tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tw(jj)+&
                an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*ts(jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj) 
    !�±߽� 
    ii = 1
    do jj = 2,N-1
       tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tsolid(ii,jj-1)+&
                an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*ts(jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj) 
    end do 
    !����
    ii = 1; jj = N
    tsolid(ii,jj) = (ae1(ii,jj)*te(ii)+aw1(ii,jj)*tsolid(ii,jj-1)+&
              an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*ts(jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)    
    !��߽�
    jj = 1
    do ii = 2,M-1
       tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tw(ii)+&
       an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)
    end do       
    if ((M>2).and.(N>2)) then
       do ii = 2,M-1
          do jj = 2,N-1
            tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tsolid(ii,jj-1)+&
            an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                       ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)
          end do
       end do
    end if
    !�ұ߽�
    jj = N
    do ii = 2,M-1
       tsolid(ii,jj) = (ae1(ii,jj)*te(ii)+aw1(ii,jj)*tsolid(ii,jj-1)+&
       an1(ii,jj)*tnsolid(ii+1,jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)
    end do 
    !����
    ii = M; jj = 1
    tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tw(ii)+&
                an1(ii,jj)*tn(jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)    
    !�ϱ߽�
    ii = M
    do jj = 2,N-1
       tsolid(ii,jj) = (ae1(ii,jj)*tnsolid(ii,jj+1)+aw1(ii,jj)*tsolid(ii,jj-1)+&
       an1(ii,jj)*tn(jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)
    end do 
    !����
    ii = M; jj = N
    tsolid(ii,jj) = (ae1(ii,jj)*te(ii)+aw1(ii,jj)*tsolid(ii,jj-1)+&
    an1(ii,jj)*tn(jj)+as1(ii,jj)*tsolid(ii-1,jj)+&
                    ap0(ii,jj)*tsolid0(ii,jj)+bp(ii,jj))/ap1(ii,jj)    

      !write(*,*) ii,ae1(ii,jj),aw1(ii,jj),an1(ii,jj),as1(ii,jj)
      !write(*,*) ii,tnsolid(ii,jj+1),tw(ii),tnsolid(ii+1,jj),tsolid(ii-1,jj)

    
    resmax = maxval(abs(tsolid-tnsolid))
    
    return
  
  end subroutine equation_solution
  
  subroutine result_hf_conduction   
    return  
  end subroutine result_hf_conduction
  
end module hf_heat_conduction
