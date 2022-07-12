## Introduction
umerical analysis on the flywheel rotor assembly is performed in the open-source FEA software package CalculiX [1]. The solver is capable of linear and nonlinear static and dynamic analysis as well as coupled thermo-mechanical analysis. Mesh generation, model preparation, and post-processing of the results can be done in another open-source software package, GraphiX, developed by Klaus Witting. It enables the user to create the geometry of the model, to assign boundary conditions, to generate mech, and to visualize the FEA results, all in 3D. The pre- and post-processing capabilities of GraphiX fall behind commercial FEA packages, e.g., Abaqus or MSC Marc, however, it has the essential tools that are sufficient to meet current simulation needs. Another open-source software package, PrePoMax, developed by Matej Borovinšek can be used as an alternative for either commercial software packages or GraphiX [2]. It enables pre- and post-processing of FEA results in a custom graphical user interface (GUI). PrePoMax exploits CalculiX at the back end to solve governing equations.

Calibration of the FEA procedure starts with benchmarking of the numerical results with a known analytical solution. The KCS flywheel rotates at a high speed; thus, calibration is made against the solid disk rotating at a constant angular velocity. A schematic of a rotating disk with a hole in the center of the disk is depicted in Figure 1a. Figure 1b illustrates the free body diagram with stresses acting on a differential element due to a centrifugal force. The analytical solution assumes that the disk has a constant thickness that is much smaller than the outer diameter (~ 10 times).

TODO: Add figure 1
Figure 1. (a) A schematic of a solid disk with a hole in the center subjected to centrifugal force, and (b) a differential element with a distribution of radial (σ_r) and tangential (σ_t) stress. [6].

By solving the equations of equilibrium, compatibility equations, and the stress-strain relationship equations simultaneously, the exact solution for radial and tangential stress in a solid disk can be obtained [7]. 

For a solid disk with a hole in the center made of isotropic material, e.g., stainless steel, stress variations along the radial and tangential directions are:

	σ_t=(3+ν)/8⋅ρω^2⋅[r_out^2+r_in^2  +(〖r_in^2⋅r〗_out^2)/(r_out^2 )-r_in^2⋅  (3⋅ν+1)/(3+ν)  ]	(1)
	σ_r=(3+ν)/8⋅ρω^2⋅[r_out^2+r^2  +(〖r_in^2⋅r〗_out^2)/(r_out^2 )-r^2  ]	(2)

where 𝜌 is the material density, ω is the rotational speed, r_in is the inner radius of the disk, r_out is the outer radius of the disk, and ν is a Poisson ratio for the material under investigation. Note that neither tangential nor radial stress depends on the elasticity modulus. The material properties of a stainless steel – a solid disk material – are outlined in Table 1. The analytical solution is obtained for a disk rotating with the angular velocity ω=5,000 (rpm) which is equivalent to ω=523.6 (rad).

Table 1. Material properties of the stainless steel and the geometry of the disk.
TODO: Add nice table with mat properties
Material	E(GPa)	ν	ρ(kg/m^3 )	r_in (m)	r_out (m)
Stainless Steel	210	0.3	7850	0.0508	0.15

A variation of tangential 𝜎𝑡 and radial 𝜎𝑟 stress in a stainless steel hollow disk rotating at ω=5,000 rpm is shown in Figure 2. The maximum tangential stress occurs in the inner surface of the disk and steadily decreases as it propagates to the outer surface. The radial stress is much smaller than the tangential stress. The maximum stress occurs at r=√(r_in⋅r_out).
 
TODO: Add figure 2
Figure 2. The distribution of tangential (σ_t) and radial (σ_r) stress in a solid stainless steel hollow disk rotating at ω=5000 (rpm).

The analytical solution for maximum tangential (σ_t) and radial (σ_t) stresses in a rotating disk are:
	σ_(t,max)=(3+ν)/8⋅ρω^2⋅[r_in^2+r_out^2⋅ (1-ν)/(3+ν)  ]	(3)
	σ_(r,max)=(3+ν)/8⋅ρω^2⋅(r_it-r_out )^2	(4)

Using the material properties and disk’s geometry from Table 1, the maximum tangential stress and radial stresses are:
	σ_(t,max)=(3+ν)/8⋅ρω^2⋅[r_in^2+r_out^2⋅ (1-ν)/(3+ν)  ]=	
	=(3+0.3)/8⋅7850⋅〖523.60〗^2⋅[〖0.0508〗^2+〖0.15〗^2⋅ (1-0.3)/(3+0.3)  ]=41.75 (MPa)	
	σ_(r,max)=(3+ν)/8⋅ρω^2⋅(r_it-r_out )^2=(3+0.3)/8⋅7850⋅〖523.60〗^2⋅(0.0508-0.15)^2=8.74 (MPa)	

The analytical approach works great for simple geometries; however, the availability of analytical solution is limited to simple, predominantly two-dimensional, geometries. Oftentimes, the only tool that can be used for the analysis of 3D structures is FEA. The analysis of complex structures can be tricky due to intricated distribution of stresses in the structure. Hence, the first step encompasses verification of a FEA procedure to ensure that the structural analysis is sound.

To benchmark the numerical results against the analytical solution, the distribution of stresses is performed via FEA in a rotating disk with a hole in the center. FEA analysis encompasses several essential pre-processing steps. The first essential step of the FEA procedure is to partition a CAD geometry into small 3D elements. Due to symmetry, only a quarter of a hollow solid disk is considered in the analysis. Of note, the model is generated in Autodesk Fusion and the pre-processing steps are performed in PrePoMax. A disk is meshed with first-order tetrahedral elements (C3D10). A meshed model of the quarter disk is shown in Figure 3. It contains 41,642 elements and 60,906 nodes. The nodes on the right-hand-side surface of the quarter disk are constrained to move in the x direction, i.e., u_1=0. The nodes on the left-hand-side surface are constrained to move in the z direction, i.e., u_3=0. To prevent the rigid body motion along the y axis, one node is restricted to move in the y direction (u_2=0). A centrifugal force due to angular rotation of the disk is prescribed as a body force. A solid disk consists of a homogeneous isotropic material with properties outlined in Table 1.


TODO: Add figure 3 
Figure 3. The quarter disk meshed with tetrahedral elements (C3D10) with symmetry boundary conditions subjected to centrifugal force due to high-speed rotation.
The FEA analysis is performed using the CalculiX built-in iterative solver on an Intel i7-1065G7 CPU with 16 GB RAM installed. Only 2 CPU cores were used for analysis. The total computation time is ~ 25 s. The analysis is performed in the framework of linear elasticity, i.e., no large displacement or rotations are considered in the analysis. 
Due to inability of PrePoMax to visualize stresses in cylindrical coordinates, the GraphiX software was used for visualization of the FEA results. Tangential and radial stresses are shown in Figure 4a and Figure 4b respectively. The maximum tangential stress is σ_(t,max)^FEA=41.6 (MPa), and the maximum radial stress is σ_(r,max)^FEA=8.8 (MPa). The FEA results are perfectly aligned which the analytical solution.
 

TODO: Add figure 4 
Figure 4. The distribution of stresses in a solid disk: a) tangential (𝜎𝑡) stress, and b) radial (𝜎𝑟) stress.

## References
1. Guido Dhondt & Klaus Witting, “CALCULIX: A Free Three-Dimensional Structural Finite Element Program”, accessed February 17, 2022, http://www.calculix.de/. 
2. Matej Borovinšek, “PrePoMax”, accessed February 17, 2022, https://prepomax.fs.um.si/  
3. Matej Borovinšek, “PrePoMax Documentation”, accessed February 17, 2022, https://gitlab.com/MatejB/PrePoMax 
4. “CalculiX: Developers Area of CalculiX Finite Element Software”, accessed February 17, 2022, https://github.com/calculix 
5. Abdul Mubeen. “15. Rotating Rings, Discs and Cylinders” In Mechanics of Solids, 2nd edition, 515-547. New Delhi: Pearson, 2011. 
6. Sayem Uddin, “Finite element modeling and analysis of composite flywheel disk including effects of filament-winding mosaic pattern”. Doctoral Dissertation, The university of New South Wales, 2013.

## Appendix A: Installation of the PrePoMax and CalculiX
The installation of open-source packages can be a daunting and time-consuming endeavor. However, the benefits of open-source software outweigh this shortcoming. Sometimes, a step-by-step guide provided by the developers lacks guidelines on the essential installation steps. To circumvent this limitation, this section outlines the steps for the installation of PrePoMax and CalculiX.
PrePoMax can be downloaded as a ZIP archive from [2]. Another alternative is the installation from source code that is available from Gitlab [3]. The former is the best way, since a ZIP archive contains a portable version of PrePoMax that doesn’t require installation. After unpacking the archive, one can run the executable file, PrePoMax.exe, to start working with the software package. A user manual can be found in [4].
CalculiX can be installed either on Linux- or Windows-based machines. The source code is available on GitHub [5]. A Linux system can be installed on either a Virtual Machine, e.g., Oracle Virtual Box, or run on a Windows Subsystem for Linux (WSL). The latest version of the code can be either cloned from the master branch or downloaded as a TAR archive from the main GitHub page. After unpacking the archive, the next step is to allow all files to be executable by typing the following command.
sudo chmod - R 777 ./*
The necessary dependencies must be installed by executing the requirements.txt file with pip using the following command:
pip3 install -r requirements.txt
The aforementioned two steps should be sufficient to run the Linux executable. Of note, the source-code should be recompiled after altering any of the files or appending new files to the source code.
