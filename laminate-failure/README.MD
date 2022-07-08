## Failure Prediction in the Multidirectional Laminate

Multidirectional laminates are the building blocks of the high-performance lightweight composite structures. To ensure structural soundness of the designed composites parts, a granular stress and failure analysis must be performed to ensure that the composite structure operates within the safety factor. Multidirectional composite laminates exhibit complex failure behavior when subjected to a general loading condition. Hence, the failure analysis is more complex than the of the traditional metals, e.g., aluminum or steel. The complexity of failure mechanisms stems from a varying arrangement of unidirectional plies with respect to principal material axis. This results in varying stiffness, strengths, and hygrothermal effects that lead to nonhomogeneous failure of plies. A failure of a single ply doesn’t necessarily result in a substantial reduction of load-bearing capacity. However, in some critical applications, e.g., aerospace, the failure of the first ply may be considered as the ultimate failure of a composite laminate. In some instances, however, the first ply failure is too conservative. It may lead to underutilization of composite laminate properties. The peak of load-bearing capacity, i.e., maxim stress within the composite structure, under a general loading conditions is considered as the ultimate failure of the composite laminate. Either first ply failure or ultimate ply failure can be applied to estimate the load-bearing capacity of the composite structure. The ultimate call stems from the analysis of the applied loads, arrangement of plies, and the application of the composite structure.

Finite Element Analysis (FEA) is an indispensable tool that can be deployed to predict the onset of failure in a multidirectional laminate under a general loading scenario. CalculiX – an open-source finite element solver – has the capacity to model the response of thin composite structures. The prediction of damage initiation, however, necessitates the development of the user subroutines with implemented failure criterion for the carbon-fiber composites. The in-house user subroutines for damage initiation and propagation are used to assess the performance of multidirectional laminates. Two most common material configurations, carbon and glass fiber-reinforced composites are used to predict the properties of the laminates. Table 12 and Table 13 outlines the effective properties of the unidirectional ply for both material systems.

Table 12. Material properties of the AS4 carbon fibers impregnated in the 3501-6 resin.
TODO: Add table with CFRP properties

Table 13. Material properties of the glass fibers impregnated in polymer resin.
TODO: Add table with GFRP properties

Two stacking sequences, [0/90]_2  and [0_2/±45]_s  , of the composite laminate are analyzed to predict onset of failure for carbon and glass fiber composites. The results are benchmarked with the results from the literature. Table 14 summarizes the simulation result for the carbon-epoxy composite, whereas Table 15 outlines the results for the glass-fiber composite. Two custom material models are utilized to predict the onset of damage in the multidirectional laminates.


Table 14. Strength prediction of the [0/90]_s carbon & glass epoxy laminates.
Table 15. Strength prediction of the [0_2/±45]_s carbon & glass epoxy laminates.

TODO: Add tables with results