# Stress Distribution in a Hollow Rotating Disk


### Introduction

Calibration of the FEA procedure starts with benchmarking of the numerical results with a known analytical solution. The KCS flywheel rotates at a high speed; thus, calibration is made against the solid disk rotating at a constant angular velocity. A schematic of a rotating disk with a hole in the center of the disk is depicted in Figure 1a. Figure 1b illustrates the free body diagram with stresses acting on a differential element due to a centrifugal force. The analytical solution assumes that the disk has a constant thickness that is much smaller than the outer diameter (~ 10 times).

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/solid-disk/solid_disk_schematics.png "")

**Figure 1.** (a) A schematic of a solid disk with a hole in the center subjected to centrifugal force, and (b) a differential element with a distribution of radial (σ_r) and tangential (σ_t) stress. [1].

By solving the equations of equilibrium, compatibility equations, and the stress-strain relationship equations simultaneously, the exact solution for radial and tangential stress in a solid disk can be obtained [2]. 

For a solid disk with a hole in the center made of isotropic material, e.g., stainless steel, stress variations along the radial and tangential directions are:

	σ_t=(3+ν)/8⋅ρω^2⋅[r_out^2+r_in^2  +(〖r_in^2⋅r〗_out^2)/(r_out^2 )-r_in^2⋅  (3⋅ν+1)/(3+ν)  ]	(1)
	σ_r=(3+ν)/8⋅ρω^2⋅[r_out^2+r^2  +(〖r_in^2⋅r〗_out^2)/(r_out^2 )-r^2  ]	(2)

where 𝜌 is the material density, ω is the rotational speed, r_in is the inner radius of the disk, r_out is the outer radius of the disk, and ν is a Poisson ratio for the material under investigation. Note that neither tangential nor radial stress depends on the elasticity modulus. The material properties of a stainless steel – a solid disk material – are outlined in Table 1. The analytical solution is obtained for a disk rotating with the angular velocity ω=5,000 (rpm) which is equivalent to ω=523.6 (rad).

Table 1. Material properties of the stainless steel and the geometry of the disk.
TODO: Add nice table with mat properties
Material	E(GPa)	ν	ρ(kg/m^3 )	r_in (m)	r_out (m)
Stainless Steel	210	0.3	7850	0.0508	0.15

A variation of tangential 𝜎𝑡 and radial 𝜎𝑟 stress in a stainless steel hollow disk rotating at ω=5,000 rpm is shown in Figure 2. The maximum tangential stress occurs in the inner surface of the disk and steadily decreases as it propagates to the outer surface. The radial stress is much smaller than the tangential stress. The maximum stress occurs at r=√(r_in⋅r_out).
 
![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/solid-disk/solid_disk_analytical_stress_graph.png "")

**Figure 2.** The distribution of tangential (σ_t) and radial (σ_r) stress in a solid stainless steel hollow disk rotating at ω=5000 (rpm).

The analytical solution for maximum tangential (σ_t) and radial (σ_t) stresses in a rotating disk are:
	σ_(t,max)=(3+ν)/8⋅ρω^2⋅[r_in^2+r_out^2⋅ (1-ν)/(3+ν)  ]	(3)
	σ_(r,max)=(3+ν)/8⋅ρω^2⋅(r_it-r_out )^2	(4)

Using the material properties and disk’s geometry from Table 1, the maximum tangential stress and radial stresses are:
	σ_(t,max)=(3+ν)/8⋅ρω^2⋅[r_in^2+r_out^2⋅ (1-ν)/(3+ν)  ]=	
	=(3+0.3)/8⋅7850⋅〖523.60〗^2⋅[〖0.0508〗^2+〖0.15〗^2⋅ (1-0.3)/(3+0.3)  ]=41.75 (MPa)	
	σ_(r,max)=(3+ν)/8⋅ρω^2⋅(r_it-r_out )^2=(3+0.3)/8⋅7850⋅〖523.60〗^2⋅(0.0508-0.15)^2=8.74 (MPa)	

The analytical approach works great for simple geometries; however, the availability of analytical solution is limited to simple, predominantly two-dimensional, geometries. Oftentimes, the only tool that can be used for the analysis of 3D structures is FEA. The analysis of complex structures can be tricky due to intricated distribution of stresses in the structure. Hence, the first step encompasses verification of a FEA procedure to ensure that the structural analysis is sound.

To benchmark the numerical results against the analytical solution, the distribution of stresses is performed via FEA in a rotating disk with a hole in the center. FEA analysis encompasses several essential pre-processing steps. The first essential step of the FEA procedure is to partition a CAD geometry into small 3D elements. Due to symmetry, only a quarter of a hollow solid disk is considered in the analysis. Of note, the model is generated in Autodesk Fusion and the pre-processing steps are performed in PrePoMax. A disk is meshed with first-order tetrahedral elements (C3D10). A meshed model of the quarter disk is shown in Figure 3. It contains 41,642 elements and 60,906 nodes. The nodes on the right-hand-side surface of the quarter disk are constrained to move in the x direction, i.e., u_1=0. The nodes on the left-hand-side surface are constrained to move in the z direction, i.e., u_3=0. To prevent the rigid body motion along the y axis, one node is restricted to move in the y direction (u_2=0). A centrifugal force due to angular rotation of the disk is prescribed as a body force. A solid disk consists of a homogeneous isotropic material with properties outlined in Table 1.


![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/solid-disk/solid_disk_fea_mesh.png "")

**Figure 3.** The quarter disk meshed with tetrahedral elements (C3D10) with symmetry boundary conditions subjected to centrifugal force due to high-speed rotation.
The FEA analysis is performed using the CalculiX built-in iterative solver on an Intel i7-1065G7 CPU with 16 GB RAM installed. Only 2 CPU cores were used for analysis. The total computation time is ~ 25 s. The analysis is performed in the framework of linear elasticity, i.e., no large displacement or rotations are considered in the analysis. 
Due to inability of PrePoMax to visualize stresses in cylindrical coordinates, the GraphiX software was used for visualization of the FEA results. Tangential and radial stresses are shown in Figure 4a and Figure 4b respectively. The maximum tangential stress is σ_(t,max)^FEA=41.6 (MPa), and the maximum radial stress is σ_(r,max)^FEA=8.8 (MPa). The FEA results are perfectly aligned which the analytical solution.

### Results

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/solid-disk/solid_disk_fea_results.png "")

**Figure 4.** The distribution of stresses in a solid disk: a) tangential (𝜎𝑡) stress, and b) radial (𝜎𝑟) stress.

### References
1. Abdul Mubeen. “15. Rotating Rings, Discs and Cylinders” In Mechanics of Solids, 2nd edition, 515-547. New Delhi: Pearson, 2011. 
2. Sayem Uddin, “Finite element modeling and analysis of composite flywheel disk including effects of filament-winding mosaic pattern”. Doctoral Dissertation, The university of New South Wales, 2013.
