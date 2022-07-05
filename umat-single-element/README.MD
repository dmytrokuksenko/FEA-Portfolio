## User Material Subroutine for Damage Initiation & Propagation in Composites

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