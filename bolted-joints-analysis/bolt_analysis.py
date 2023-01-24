# bolted-joints stress analysis [1]
#
# [1] https://mechanicalc.com/reference/bolted-joint-analysis

import numpy as np
from scipy.optimize import fsolve


def bolt_stresses(a_tensile, a_share, f_preload, f_tension, m_bending, f_shear):
    """Calculates stresses in a bolted joint."""

    s_preload = f_preload / a_tensile  # preload stress (MPa)
    s_bending = (32 * m_bending) / (np.pi * d**3)  # bending stress (MPa)
    s_tension = f_tension / a_tensile  # tension stress (MPa)
    s_shear = f_shear / a_share  # shear stress (MPa)

    print(f"Preload stress in the bolt is: {s_preload:.2f} (MPa)")
    print(f"Bending stress in the bolt is: {s_bending:.2f} (MPa)")
    print(f"Tension stress in the bolt is: {s_tension:.2f} (MPa)")
    print(f"Shear stress in the bolt is: {s_shear:.2f} (MPa)")

    return s_preload, s_tension, s_shear, s_bending


def vol_misses(s_preload, s_bending, s_tension, s_shear):
    """Calculates von Mises stress in a bolted joint."""

    s_mises = np.sqrt(
        (s_preload + n * (s_tension + s_bending)) ** 2 + 3 * (n * s_shear) ** 2
    )
    print(f"von Mises stress is: {s_mises:.2f} (MPa)")

    return s_mises


if __name__ == "__main__":
    print("Running Bolt Analysis...")

    # dimensions
    d = 6  # bolt's nominal diameter (mm)
    p = 1.5  # bolt's pitch (mm)
    tpi = 1 / p  # threads per unit length
    h = np.sqrt(3) * p / 2  # height of a fundamental triangle

    # Areas
    a_nomimal = np.pi * (d) ** 2 / 4  # nominal area (mm^2)
    a_tensile = (np.pi / 4) * (d - 0.9382 * p) ** 2  # tensile stress area (mm^2)
    a_share = (np.pi / 4) * (d - 1.226869 * p) ** 2  # shear stress area (mm^2)

    # mechanical properties
    n = 1  # safety factor
    s_yield = 1035  # bolt's yield strength (MPa)

    # applied forces
    f_preload = 0.75 * s_yield * a_tensile  # preload force (N)
    print(f"Bolt preload is: {f_preload:.2f} (N)")
    t = 0.2 * d * f_preload  # applied torque (N*mm)
    print(f"Bolt torque is: {t:.2f} (Nm)")
    f_tension = 1000  # tensile force (N)
    m_bending = 0  # bending moment (N*mm)
    f_shear = 0  # share force (N)

    s_preload, s_bending, s_tension, s_shear = bolt_stresses(
        a_tensile, a_share, f_preload, f_tension, m_bending, f_shear
    )

    s_mises = vol_misses(s_preload, s_bending, s_tension, s_shear)
