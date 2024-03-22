module input
    integer :: nrrod = 1
    real :: dt = 1e-3
    real :: material_temp(10)=(/300,300,300,300,300,300,300,300,300,300/)
    real :: material_kc(10)=1e4
    real :: material_cp(10)=1e4
    real :: pitch,roughness
    real :: dhc=1,phc=1,lhc=1,rhc=1,dmhc=1
    real :: thick=1
    real :: facnonu
    integer :: hf_c,hcf
    !integer :: TotMeshNum
    !integer :: FuelMeshNum,CladMeshNum
    
    !!!!!Helical fuel heat conduction
    integer :: matfuel,matclad                 !material index of fuel and clad
    integer :: itermax=30000
    integer :: N1=20,N2=20,N3=20,N4=20,N5=20,N6=20                      !网格划分参数

    real ::rod_linear_power=42e3
    integer::jmax=1,kmax=1

end module