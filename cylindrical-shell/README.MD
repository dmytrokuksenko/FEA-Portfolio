## Composite Cylindrical Shell under Internal Pressure

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

The simulation results comparing the prediction between two material models, built-in orthotropic and Linde, are outlined in Table 9. The stress is compared at four points, inner and outer radii for both cylindrical shell constituents. The results are compared to the benchmark NAFEMS model presented in [1].

Table 9. Finite element results of the stress in the cylindrical shell at four points.
TODO: Add table with results

## References

[1]	ABAQUS Benchmarks Manual, Wrapped thick cylinder under pressure and thermal loading, (n.d.). https://classes.engineering.wustl.edu/2009/spring/mase5513/abaqus/docs/v6.5/books/bmk/default.htm (accessed June 15, 2022).