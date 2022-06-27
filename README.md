# Portfolio of Finite Element Analysis (FEA) Projects


## Executvie Summary

TODO: Add a synopsis of the projects that are 

## 1. [Composite Plate under Uniform Pressure](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-plate-bending)

The validation of the built-in and custom material model for fiber-reinforced plastic composites begins with the finite element analysis of a simply supported plate subjected to the uniform pressure. The plate has a width of 4,000 mm and a length of 2,000 mm. The thickness of the plate is 10 mm.

The geometry of the plate is generated in Cubit, a commercial software package for advance meshing of complex structures [2]. Second-order shell finite elements (SR8) are chosen to discretize the plate for the numerical analysis. The finite element model is imported into the PrePoMax, the advanced pre- and post-processing graphical user interface for the open-source finite element solver CalculiX [3,4].

The plate is modeled as a transversely isotropic composite material in the framework of linear elasticity. The effective elastic properties of a single AS4/9310 lamina are outlined in Table 1.

Table 1. Material properties of the AS4/9310 CFRP composite constituents.

| Material | E1 (GPa) | E2 (GPa) | G12(GPa) | G23 (GPa)|   ν12    |    ν23   |
|----------|----------|----------|----------|----------|----------|----------|
| AS4/9310 |  133.86  |  7.706   |  4.306   |   2.76   |   0.3    |   0.396  |


CalculiX has built-in capabilities to simulate the response of a composite material by either directly specifying the components of a stiffness tensor or by defining engineering constants. However, the simulation of damage initiation and propagation in composites can be accomplished only by deploying user-defined material model. The CalculiX user manual guidelines on the implementation of custom material models, but an in-depth description of the necessary steps is out of scope for this report. A ready-to-use Abaqus Linde user material model for damage initiation and propagation in composites has been incorporated "as-is" into CalculiX to compare results [5].

Figure 1 depicts a finite element model of the plate highlighting the node sets for the boundary conditions prescription. The prescribed displacements/rotations for each node set are summarized in Table 2. The plate is subjected to the uniform pressure of 0.12〖⋅10〗^(-3)  MPa. Only a quarter of the plate is considered in FEA due to symmetry.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/deflection-plate-mesh.png "")

Figure 1. Symmetric part of a simply supported composite plate discretized with the second-order shell finite elements (S8R). The node sets for the prescription of boundary conditions are also highlighted.

Table 2. Boundary conditions for the node sets of the composite plate under investigation.

|         Node Set        | Displacement / Rotation |
|-------------------------|-------------------------|
|           X-, Y-        |          u3=0           |
|             X+          |  u1=0,u2(θ)=0,u3(θ)=0   |
|             Y+          |  u2=0,u1(θ)=0,u3(θ)=0   |


Figure 2a-d shows simulation results of the composite plate subjected to the uniform pressure. Both material models, built-in and Linde, accurately predicts the maxim displacement of the composite plate. The maximum displacement, 17.45 mm, appears in the middle of the composite plate. The difference between von Mises stress distributions between the current model and the benchmark model is almost indistinguishable.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/disp-stress-built-in.png "")
![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-plate-bending/disp-stress-Linde.png "")   

Figure 2. Displacement and von Mises distributions in the finite element plate model simulated with a built-in material model (first row) and Linde material model (second row).

Table 3 shows a comparison between predicted maximum displacement, u3, for built-in CalculiX and Linde material models.

Table 3. Predicted maximum displacement in the composite plate for built-in and Linde material models.

|          | Barbero et al [6] | Built-in CalculiX | Linde ABAQUS |
|----------|-------------------|-------------------|--------------|
| u3 (mm)  |        17.43      |        17.45      |      17.45   |

## 2. [Three-Point Bending of a Unidirectional Composite Laminate](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-plate-bending)

Three-point bending test is a common test for the strength estimation of unidirectional composite laminates subjected to bending loads. The National Agency for Finite Element and Standards (NAFEMS) has a benchmark problem that can be used for verification purposes. The ABAQUS Benchmarks Manual contains a comprehensive FEA study of a laminated strip under three-point bending [7]. The aforementioned results have been used to benchmark the FEA results obtained by CalculiX.

The geometry of the composite laminate is generated in Cubit which has a wide array of advanced geometry generation and meshing tools. The plate has a width of 10 mm and a length of 100 mm. The thickness of the plate is 1 mm. The model is discretized with a second-order shell elements (SR8). The ABAQUS Benchmarks Guide contains a comprehensive evaluation of the effect of the finite element type on the numerical results; hence, only second-order shell elements are benchmarked in this study.

The laminated strip is modeled as transversely isotropic material with the material properties outlined in Table 4. Two material models are used to simulate the response of the laminated strip, the built-in orthotropic material model and the “as-is” Linde material model from ABAQUS.

Table 4. Material properties of the laminated strip under investigation.

| Material | E1 (GPa) | E2 (GPa) | G12(GPa) | G23 (GPa)|   ν12    |    ν23   |
|----------|----------|----------|----------|----------|----------|----------|
| AS4/9310 |    100   |    5     |    2     |     3    |   0.4    |    0.3   |

A discretized finite element model of the laminated strip is presented in Figure 4a. It also highlighted three node sets that are used to prescribe the appropriate boundary conditions. The model contains 20 shell elements, 10 elements in the longitudinal direction, and 2 elements in the transverse direction. Figure 4b illustrates a layup of the composite under consideration. The laminated strip contains 0°- and 90°-oriented composite plies. Figure 4b also highlights three points, C, D, and E, that are used for measuring the displacement, normal, and shear stress in the composite coupon.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-three-point-bend/three-point-bending-mesh-bc.png "")
Figure 3. The finite element model of a three-point bending coupon meshed with shell elements (a) with a demonstration of a laminated layup (b).


A synopsis of prescribed boundary conditions for the three-point bend specimen is outlined in Table 5. The node set X- represents a pin support, whereas the X+is presented as the roller support. A concentrated force of 10 N/mm is applied to the Xcenter node set.

Table 5. Boundary conditions for the three-point bending stripe under investigation.

|         Node Set        | Displacement / Rotation |
|-------------------------|-------------------------|
|             X-          |        u1=u2=u3=0       |
|             X+          |           u3=0          |
|          Xcenter        |        f3=10 N/mm       |

Figure 4 shows the finite element analysis results of the three-point bending coupon. The first row, Figure 5a-b, compares the maximum displacement (point C) in the laminated coupon. The difference between the two material models, built-in and Linde, are almost indistinguishable. The second row, Figure 5c-d, depicts the distribution of the bending stress in the laminated coupon. The maximum bending stress (point E) is on par between the two material models. The third row of Figure 5e-f compares the interlaminar shear stress between predicted by two material models under investigation. The interlaminar stress is measured at point D for both material models. Both models overestimate the interlaminar stress in comparison with the benchmark results. The Linde model, however, results in a more realistic prediction of interlaminar shear stress.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-three-point-bend/three-point-bending-disp.png "")
![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-three-point-bend/three-point-bending-s11.png "")
![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cfrp-three-point-bend/three-point-bending-s13.png "")

Figure 4. Simulation results for the maximum displacement at point C (a-b), maximum bending stress at point E (c-d), maximum interlaminar shear stress at point D (e-f) for the built-in orthotropic material model (a, c, e), and Linde material model (b, d, f).

Table 6 provides the comparison between the simulation results for the built-in orthotropic material model and the Linde material model vs. the NAFEMS benchmarking results. In sum, the Linder material model shows a better performance due to a better prediction of the interlaminar shear strength prediction. The built-in orthotropic material modes, on the other hand, significantly overestimate the interlaminar shear stress. The prediction of maximum displacement and the bending stress is very similar between the two models in comparison with the NAFEMS benchmark model.

Table 6. The simulation results for the three-point bending coupon under investigation.

|   Model  |     S11 (MPA)     |      S13 (MPA)    |     u3 (mm)  |
|----------|-------------------|-------------------|--------------|
|  NAFEMS  |       684         |        4.1        |     1.06     |
| Built-in |   671.5(1.83%)    |   5.477(26.27%)   |  1.058(0.19%)|
|  Linde   |   672.5(1.68%)    |   4.587(11.39%)   |  1.051(0.85%)|


## 3. [Composite Cylindrical Shell under Internal Pressure](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-plate-bending)

Cylindrical shell is another model provided by NAFEMS for benchmarking of the FEA results [8]. The model has two materials, steel linear wrapped around with a carbon fiber-reinforced composite material. Both materials have a thickness of 2 mm. The inner radius of the cylindrical shell is 23 mm. The overall length of the shell is 200 mm. Due to the symmetry, only 18 of the overall finite element model is considered in the analysis to reduce the computation burden of the analysis. The material properties of both constituents of the composite shell are outlined in Table 7.

Table 7. Mechanical properties of the cylindrical shell materials.
TODO: Add table with material properties

Figure 5 shows a finite element model of the cylindrical shell. The model is meshed with second-order shell elements with reduced integration (SR8). Symmetric boundary conditions are prescribed to Ysym and Xsym. The nodes belonging to the bottom node set, Zbottom, are constrained to move in the z-direction. The shell is subjected to an internal pressure of 200 MPa. 

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cylindrical-shell/cylindrical-shell-mesh-bc.png "")
Figure 5. A finite element model of a composite shell (CFRP composite & steel) subjected to the internal pressure.

Table 8. Boundary conditions of the cylindrical shell.

TODO: Add table with boundary conditions

Open-source finite element solver CalculiX is used to perform the analysis of the cylindrical shell. Two material models, built-in orthotropic and Linde ABAQUS, are used to predict the response of the CFRP composite material of the cylindrical shell subjected to the internal pressure. Figure 6 shows the stress distribution in the cylindrical shell with the built-on orthotropic material model for the CFRP composite. It is worth mentioning that both materials of the cylindrical shell are simulated in the framework of linear elasticity without the possibility to undergo damage.


![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/cylindrical-shell/cylindrical-shell-fea-results.png "")
Figure 6. Stress distribution in the cylindrical shell is modeled with a built-in orthotropic material model.

The simulation results comparing the prediction between two material models, built-in orthotropic and Linde, are outlined in Table 9. The stress is compared at four points, inner and outer radii for both cylindrical shell constituents. The results are compared to the benchmark NAFEMS model presented in [8].

Table 9. Finite element results of the stress in the cylindrical shell at four points.
TODO: Add table with results

## 4. [Stress Analysis of a Composite Rotating Disk](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-rotating-disk-stress)

## 5. [Vibration Analysis of a Composite Rotating Disk](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/cfrp-rotating-disk-vibration)

## 6. [CALCULIX UMAT for Damage Initiation and Propagaiton in Composites](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/umat-single-element)

The implementation of the custom material models is verified on a single element at the initial step. The results are benchmarked against the built-in orthotropic material model available in CalculiX. Note the built-in orthotropic material model doesn’t have the capacity to track damage. Figure 7 illustrates a sketch of a single element under investigation. The width and the height of the single element are 203.2 mm and 101.6 mm. A discretized finite element model has a single quadratic shell element with reduced integration (S8R). The thickness of the single element is 1 mm. Figure 7 also highlights the constraints applied to the appropriate node sets. The stress-strain response of the finite element model is verified under the displacement and force control to verify the implementation.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/umat-single-element/single-element-sketch-bc.png "")
Figure 7. A finite element model of a single element for the verification of custom material models in CalculiX.

Table 10 outlines the material properties of a fiber-reinforced material under investigation. The principal direction of the fiber’s orientation is aligned with the x axis of the finite element model.

Table 10. Material properties of a composite material for the finite element model.
|   Material  | E1 (GPa) | E2 (GPa) | G12(GPa) | G23 (GPa)|   ν12    |    ν23   |
|-------------|----------|----------|----------|----------|----------|----------|
|             |  159.96  |   8.96   |   6.205  |   3.447  |   0.28   |   0.28   |
|             |F1t (GPa) |F1c (GPa) |F2t (MPa) |F2c (MPa) | F4 (MPa) |     -    |
|  Composite  |   2.84   |   1.76   |   60.12  |  167.54  |   60.12  |          |
|             |Gamma_f(J)|Gamma_m(J)|    nu    |     -    |     -    |     -    |
|             |   12.5   |    1     |    1     |     -    |     -    |     -    |

Figure 8 illustrates displacement in the finite element model under the displacement control. Deformed and undeformed shapes are shown to illustrate the effect of boundary conditions on the displacement distribution. The FEA results are shown for the orthotropic material model available in CalculiX.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/umat-single-element/single-element-umat-disp.png "")
Figure 8. The FEA results of a single element model subjected to 1 mm displacement in the x-direction.

To benchmark strength prediction in the direction of the fibers, a 2.5 mm displacement is applied to the model in the x direction. Figure 9 shows the stress-strain curves for all custom material models under investigation. The results are benchmarked against the built-in orthotropic material model that doesn’t have a capacity to predict the onset of damage. All custom models accurately predict the composite’s strength in the fiber’s direction under applied displacement. Despite the differences in strength onset prediction among various failure criteria, under the tension in fiber’s direction all failure criteria converge to the same response. Up to onset of damage, all stress-strain curves are perfectly aligned with the built-in orthotropic material model. A difference in stress-strain response between custom material models is manifested after the onset of damage. The difference between Linde material model and the rest stems from different implementation of damage evolution after the onset of damage.

![Alt text](https://github.com/dmytrokuksenko/finite-element-analysis-porftofolio/blob/main/umat-single-element/single-element-umat-stress-strain.png "")
Figure 9. The stress-strain curves of the finite element model of a single element subjected to the 2.5 mm in the fiber’s direction. 
 
## 7. [Open-hole Coupon Analysis](https://github.com/dmytrokuksenko/FEA-Portfolio/tree/main/open-hole-coupon)


## References

[1]	I. Daniel, O. Ishai, Engineering mechanics of composite materials, Oxford University Press, 2006.

[2]	Coreform Cubit, (n.d.). https://coreform.com/ (accessed June 12, 2022).

[3]	M. Borovinšek, PrePoMax, (n.d.). https://prepomax.fs.um.si/ (accessed June 12, 2022).

[4]	G. Dhondt, Witting Klaus, CALCULIX: A Free Three-Dimensional Structural Finite Element Program, (n.d.). http://www.calculix.de/ (accessed June 12, 2022).

[5]	Abaqus Benchmarks Guide, Failure of Blunt Notched Fiber Metal Laminates, (n.d.). https://abaqus-docs.mit.edu/2017/English/SIMACAEEXARefMap/simaexa-c-damagefailfml.htm (accessed June 12, 2022).

[6]	E. Barbero, Finite element analysis of composite materials using Abaqus, CRC Press, 2013.

[7]	ABAQUS Benchmarks Manual, Laminated strip under three-point bending, (n.d.). https://classes.engineering.wustl.edu/2009/spring/mase5513/abaqus/docs/v6.6/books/bmk/default.htm?startat=undefined (accessed June 13, 2022).

[8]	ABAQUS Benchmarks Manual, Wrapped thick cylinder under pressure and thermal loading, (n.d.). https://classes.engineering.wustl.edu/2009/spring/mase5513/abaqus/docs/v6.5/books/bmk/default.htm (accessed June 15, 2022).

