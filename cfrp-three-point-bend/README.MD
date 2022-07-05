
## Three-Point Bending of a Unidirectional Composite Laminate

Three-point bending test is a common test for the strength estimation of unidirectional composite laminates subjected to bending loads. The National Agency for Finite Element and Standards (NAFEMS) has a benchmark problem that can be used for verification purposes. The ABAQUS Benchmarks Manual contains a comprehensive FEA study of a laminated strip under three-point bending [1]. The aforementioned results have been used to benchmark the FEA results obtained by CalculiX.

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

## References

[1]	ABAQUS Benchmarks Manual, Laminated strip under three-point bending, (n.d.). https://classes.engineering.wustl.edu/2009/spring/mase5513/abaqus/docs/v6.6/books/bmk/default.htm?startat=undefined (accessed June 13, 2022).
