<h1 align="center">Finite Element Analysis (FEA) Project Portfolio</h1> 

Selected FEA projects that cover linear & nonlinear static analysis, buckling analysis, dynamic transient and frequency analysis, and modal analysis of composite & metallic structures.

**Technical Stack:**

*3D CAD*: Autodesk Fusion, Cubit

*FEA Solvers*: NASTRAN, CalculiX - ABAQUS-like finite element solver

*Pre- & Post-Processing Software*: FEMAP, PrePoMax

*Programming Languages*: Python, FORTRAN

### [Composite Laminate under Uniform Pressure](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-plate-bending)

This project is designed to assess the performance of the Linde material model in CalculiX. The model is designed to predict damage initiation and propagation in carbon composites. It's implemeted in CalculiX via the FORTRAN user subroutine. A simply supported laminate subjected to uniform pressure is used to assess the peformance of the Linde material model.     

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/cfrp-plate-bending/disp-stress-Linde.png"
 width="800">
</p>

<p align="center">
Displecement (left) and von Mises stress (right) in the carbon composite laminate subjected to uniform pressure.
</p>

### [Three-Point Bending of a Composite Laminate](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-three-point-bend)

The stress analysis of the fiber-reinforced composite laminate subjected to three-point bending. The analysis evaluates the performance of the Linde material model implemented via the FORTRAN user subroutine in CalculiX. 

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/cfrp-three-point-bend/three-point-bending-s11.png"
 width="800">
</p>

<p align="center">
Prediction of the $\sigma_{11}$ stress distribution in a carbon composite laminate subjected to three-point bending using built-in (left) and Linde (right) material models.
</p>

### [Filament-Wound Thick Pipe under Internal Pressure](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cylindrical-shell)

Stress analysis of a thick pipe consisting of steel (inner portrion) and a filament-wound carbon composite (outer portion). The pipe is subjected to internal pressure. The project is designed to evaluate the assginment of orthotropic material properties in cylindrical coordiante system using the open-source FEA software CalculiX.  

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/cylindrical-shell/cylindrical-shell-fea-results.png"
 width="800">
</p>

<p align="center">
The prediction of $\sigma_{\theta}$ stress in a pressure pipe made out of steel and filament-wound carbon composite.
</p>

### [User Material Models (UMAT) for Damage Initiation and Propagation in Carbon Composites](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/umat-single-element)

Complex failure mechanisms of carbon composites necessitate the application of advanced material models to accurately simulate damage initiation and propagation. This project describes the implementation and performance assessment of several custom material models - Maximum Stress, Maximum Strain, Tsai-Wu, Linde, and Hashin, for prediction of damage in carbon composites. 

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/umat-single-element/single-element-umat-stress-strain.png"
 width="500">
</p>

<p align="center">
Prediction of damage initiation and propagaiton using different UMATs for carbon composites in a single-element FEA model subjected to displacement in longitudinal direction(fibers' direction). 
</p>

### [Composite Wing Design, Analysis, and Optimization](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/composite-wing)

The static stress analysis and linear buckling analysis of a composite wing subjected to drag and lift forces. The desing optimization of the carbon composite wing is performed to avoid buckling under the applied forces.

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/composite-wing/composite-wing-static-analysis.png"
 width="800">
</p>

<p align="center">
A distribution of a Hoffman failure index in a composite wing subjected to drag and lift forces. 
</p>

### [Buckling Analysis of a Cylindrical Composite Shell](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-shell-buckling)


[<img
  src="cfrp-shell-buckling/fea-buckling-composite.png"
  width="750"
  title="Composite Plate with Built-In Calculix Material Properties">
](cfrp-shell-buckling/)


### [Stress Distribution in a Solid Circular Rotating Disk](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/solid-disk)


[<img
  src="solid-disk/solid_disk_analytical_stress_graph.png"
  width="500">
](solid-disk/)

[<img
  src="solid-disk/solid_disk_fea_results.png"
  width="500">
](solid-disk/)

### [Static Analysis of the Metallic Assembly](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-assembly)


[<img
  src="femap-assembly/assembly-analysis.png"
  width="500">
](femap-assebmly/)


### [Modal Frequency Analysis of a Hinge](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-modal-frequency-hinge)

[<img
  src="femap-modal-frequency-hinge/hinge-normal-frequencies.png"
  width="500">
](femap-modal-frequency-hinge/)


### [Random Response of a Hinge Model](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-random-response-hinge)

[<img
  src="femap-random-response-hinge/random-vibration-hinge.png"
  width="500">
](femap-random-response-hinge/)


### [Shock Spectrum Response Analysis of Composite Pressure Vessel](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-random-response-hinge)

### [Boeing Landing Gear Stress Analysis](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-random-response-hinge)
