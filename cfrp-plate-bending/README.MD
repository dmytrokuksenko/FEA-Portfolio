# Composite Plate under Uniform Pressure

### Introduction

The validation of the built-in and custom material model for fiber-reinforced plastic composites begins with the finite element analysis of a simply supported plate subjected to the uniform pressure. The plate has a width of 4,000 mm and a length of 2,000 mm. The thickness of the plate is 10 mm.

The geometry of the plate is generated in Cubit, a commercial software package for advance meshing of complex structures [2]. Second-order shell finite elements (SR8) are chosen to discretize the plate for the numerical analysis. The finite element model is imported into the PrePoMax, the advanced pre- and post-processing graphical user interface for the open-source finite element solver CalculiX [3,4].

The plate is modeled as a transversely isotropic composite material in the framework of linear elasticity. The effective elastic properties of a single AS4/9310 lamina are outlined in Table 1.

*Table 1. Material properties of the AS4/9310 CFRP composite constituents.*

| Material | E<sub>1</sub> (GPa)| E<sub>2</sub> (GPa) | G<sub>12</sub> (GPa) | G<sub>23</sub> (GPa)| ν<sub>12</sub> | ν<sub>23</sub> |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| AS4/9310 |133.86|7.706|4.306|2.76|0.3|0.396|


CalculiX has built-in capabilities to simulate the response of a composite material by either directly specifying the components of a stiffness tensor or by defining engineering constants. However, the simulation of damage initiation and propagation in composites can be accomplished only by deploying user-defined material model. The CalculiX user manual guidelines on the implementation of custom material models, but an in-depth description of the necessary steps is out of scope for this report. A ready-to-use ABAQUS Linde user material model for damage initiation and propagation in composites has been incorporated "as-is" into CalculiX to compare results [5].

Figure 1 depicts a finite element model of the plate highlighting the node sets for the boundary conditions prescription. The prescribed displacements/rotations for each node set are summarized in Table 2. The plate is subjected to the uniform pressure of $12\cdot10^{-3}$  MPa. Only a quarter of the plate is considered in FEA due to symmetry.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/deflection-plate-mesh.png "")

**Figure 1.** Symmetric part of a simply supported composite plate discretized with the second-order shell finite elements (S8R). The node sets for the prescription of boundary conditions are also highlighted.

*Table 2. Boundary conditions for the node sets of the composite plate under investigation.*

|Node Set|Displacement/Rotation|
|:---:|:---:|
|X-, Y-|$u_3=0$|
|X+|$u_1=0$, $u_2(\theta)=0$, $u_3(\theta)=0$|
|Y+|$u_2=0$, $u_1(\theta)=0$, $u_3(\theta)=0$|

### Results

Figure 2a-d shows the simulation results of the composite plate subjected to uniform pressure. Both material models, built-in and Linde, accurately predict the maxim displacement of the composite plate. The maximum displacement, 17.45 mm, appears in the middle of the composite plate. The difference between von Mises stress distributions between the current model and the benchmark model is almost indistinguishable.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/disp-stress-built-in.png "")
![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/disp-stress-Linde.png "")   

**Figure 2.** Displacement and von Mises distributions in the finite element plate model simulated with a built-in material model (first row) and Linde material model (second row).

Table 3 shows a comparison between predicted maximum displacement, u<sub>3</sub>, for built-in CalculiX and Linde material models.

*Table 3. Predicted maximum displacement in the composite plate for built-in and Linde material models.*

|| Barbero et al [6] | Built-in CalculiX | Linde ABAQUS |
|:---:|:---:|:---:|:---:|
| $u_3$ (mm) |17.43|17.45|17.45|

### Conclusions

The "as-is" ABAQUS Linde material model has been implemented in the open-source software package CalculiX. The predicted displacement in a simply supported CFRP shell predicted by the material model correlates well with the analytical solution available in the literature. The Linde material model can be used for the analysis of composites in the open-source software package CalculiX.

### References

[1]	I. Daniel, O. Ishai, Engineering mechanics of composite materials, Oxford University Press, 2006.

[2]	Coreform Cubit, (n.d.). https://coreform.com/ (accessed June 12, 2022).

[3]	M. Borovinšek, PrePoMax, (n.d.). https://prepomax.fs.um.si/ (accessed June 12, 2022).

[4]	G. Dhondt, Witting Klaus, CALCULIX: A Free Three-Dimensional Structural Finite Element Program, (n.d.). http://www.calculix.de/ (accessed June 12, 2022).

[5]	Abaqus Benchmarks Guide, Failure of Blunt Notched Fiber Metal Laminates, (n.d.). https://abaqus-docs.mit.edu/2017/English/SIMACAEEXARefMap/simaexa-c-damagefailfml.htm (accessed June 12, 2022).

[6]	E. Barbero, Finite element analysis of composite materials using Abaqus, CRC Press, 2013.
