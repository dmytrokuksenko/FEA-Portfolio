# Portfolio of Finite Element Analysis (FEA) Projects

# Unidirectional Composite Plate under Uniform Pressure

The validation of the built-in and custom material model for fiber-reinforced plastic composites begins with the finite element analysis of a simply supported plate subjected to the uniform pressure. The plate has a width of 4,000 mm and a length of 2,000 mm. The thickness of the plate is 10 mm.

The geometry of the plate is generated in Cubit, a commercial software package for advance meshing of complex structures [2]. Second-order shell finite elements (SR8) are chosen to discretize the plate for the numerical analysis. The finite element model is imported into the PrePoMax, the advanced pre- and post-processing graphical user interface for the open-source finite element solver CalculiX [3,4].

The plate is modeled as a transversely isotropic composite material in the framework of linear elasticity. The effective elastic properties of a single AS4/9310 lamina are outlined in Table 1.

TODO: add the table with properties!

CalculiX has built-in capabilities to simulate the response of a composite material by either directly specifying the components of a stiffness tensor or by defining engineering constants. However, the simulation of damage initiation and propagation in composites can be accomplished only by deploying user-defined material model. The CalculiX user manual guidelines on the implementation of custom material models, but an in-depth description of the necessary steps is out of scope for this report. A ready-to-use Abaqus Linde user material model for damage initiation and propagation in composites has been incorporates as-is into CalculiX to compare results [5].

Figure 1 depicts a finite element model of the plate highlighting the node sets for the boundary conditions prescription. The prescribed displacements/rotations for each node set are summarized in Table 2. The plate is subjected to the uniform pressure of 0.12〖⋅10〗^(-3)  MPa. Only a quarter of the plate is considered in FEA due to symmetry.

TODO: add figure with boundary conditions and talbe with boundary conditions

Figure 3a-d shows simulation results of the composite plate subjected to the uniform pressure. Both material models, built-in and Linde, accurately predicts the maxim displacement of the composite plate. The maximum displacement, 17.45 mm, appears in the middle of the composite plate. The difference between von Mises stress distribution is almost indistinguishable.

TODO: add figure with sim results

Table 3 shows a comparison between predicted maximum displacement, u_3, for built-in CalculiX and Linde material models.

TODO: add table with disp comparison

# References

[1]	I. Daniel, O. Ishai, Engineering mechanics of composite materials, Oxford University Press, 2006.

[2]	Coreform Cubit, (n.d.). https://coreform.com/ (accessed June 12, 2022).

[3]	M. Borovinšek, PrePoMax, (n.d.). https://prepomax.fs.um.si/ (accessed June 12, 2022).

[4]	G. Dhondt, Witting Klaus, CALCULIX: A Free Three-Dimensional Structural Finite Element Program, (n.d.). http://www.calculix.de/ (accessed June 12, 2022).

[5]	Abaqus Benchmarks Guide, Failure of Blunt Notched Fiber Metal Laminates, (n.d.). https://abaqus-docs.mit.edu/2017/English/SIMACAEEXARefMap/simaexa-c-damagefailfml.htm (accessed June 12, 2022).

[6]	E. Barbero, Finite element analysis of composite materials using Abaqus, CRC Press, 2013.

