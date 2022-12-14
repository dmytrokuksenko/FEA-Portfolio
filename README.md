<h1 align="center">Finite Element Analysis (FEA) Project Portfolio</h1> 

Selected FEA projects that cover linear & nonlinear static analysis, buckling analysis, dynamic transient and frequency analysis, and modal analysis of composite & metallic structures.

**Technical Stack:**

*3D CAD*: Autodesk Fusion, Cubit

*FEA Solvers*: NASTRAN, CalculiX - ABAQUS-like finite element solver

*Pre- & Post-Processing Software*: FEMAP, PrePoMax

*Programming Languages*: Python, FORTRAN

### [Static, Buckling, & Modal Analysis of a Composite Inner Shell](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/inner-shell)

The analysis of a composite flywheel energy storage subjected to cenrtrigufal force. 

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/inner-shell/inner-shell-FI.png"
 width="800">
</p>

<p align="center">
A deformed FEA model of the flywheel rotor assembly depicting the failure index of 2D inner shell predicted by Hoffman Failure Index. 
</p>


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

### [Modal Frequency Analysis of a Hinge Bracket](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-modal-frequency-hinge)

The natural frequency analysis and modal analysis of a hinge bracket. The model is excited by a unit inpulse in the frequence domain to analyze the response of the hinge bracket FEA model.

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/femap-modal-frequency-hinge/hinge-natural-frequencies.png"
 width="800">
</p>

<p align="center">
Natural friequencies and modal shapes of a hinge bracket.
</p>

### [Stress Distribution in a Rotating Disk](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/solid-disk)

Static analysis of a solid metallic hollow disk subjected to a centrifugal force. The distribution of stresses (tangential and radial) are predicted by FEA and benchmarked against the analytical solution.

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/solid-disk/solid-disk-main-readme.png"
 width="800">
</p>

<p align="center">
Distribution of a radial and tangential stresses in a solid metallic disk using analytical solution (left) and FEA model (right). 
</p>

### [Buckling Analysis of a Cylindrical Composite Shell](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-shell-buckling)

Linear buckling analysis of an aluminum and carbon composite cylindrical shells under the applied compression force. The composite shell has a hoop layup (90 degrees orientation of fibers). 

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/cfrp-shell-buckling/buckling-analysis-cfrp.png"
 width="800">
</p>

<p align="center">
First buckling mode of an aluminum and carbon composite (hoop wrap) thin cylindrical shell under applied compressive force.
</p>

### [Static Analysis of a Metallic Assembly](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/femap-assembly)

Static stress analysis of the metallic assebmly. The assembly contains of two pieces with a glued connection between them.

<p align="center">
<img
  src="https://github.com/dmytrokuksenko/FEA-Portfolio/blob/main/femap-assembly/assebmly-analysis-mises .png"
 width="800">
</p>

<p align="center">
FEA of a glued assebmly pieces in FEMAP. 
</p>
