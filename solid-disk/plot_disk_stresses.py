"""
    Stresses in a hollow solid disk. 
    
    @author: Dmytro Kuksenko
    @date: Nov 17, 2022
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib


def disk_stresses(nu, rho, w_rpm, r0, R, n_steps, plot=False): 
	
    w = w_rpm * 2 * np.pi / 60

    r = [r0 + x * (R - r0) / n_steps for x in range(n_steps + 1)]

    # Radial stress
    sig_r = [((3 + nu) / 8 * rho * (w ** 2) * (r0 ** 2 + R ** 2 - (R * r0 / x) ** 2 - x ** 2))/10**6 for x in r]
    sig_r_max = ((3 + nu) / 8) * (rho * w ** 2) * (R - r0) ** 2

    sig_theta = [
        ((3 + nu) / 8 * rho * (w ** 2) * (r0 ** 2 + R ** 2 + (R * r0 / x) ** 2 - (1 + 3 * nu) * (x ** 2) / (3 + nu)))/10**6 for
        x in r]
    sig_theta_max = rho * (w ** 2) / 4 * ((3 + nu) * (R ** 2) + (1 + nu) * (r0 ** 2))

    print(f"Rotatonal Speed is: {w:.2f} (rad/s) ")
    print(f"Max radial stress: {sig_r_max / 10 ** 6:.2f} (MPa)")
    print(f"Max circumferential stress: {sig_theta_max / 10 ** 6:.2f} (MPa)")

    if plot:

        plt.rcParams.update({'font.size': 12})

        fig, ax = plt.subplots()
        ax.plot(r, sig_r, 'bo--', label='Radial Stress ($\sigma_r$)')
        ax.plot(r, sig_theta, 'ro--', label='Circumferential Stress ($\sigma_{\Theta}$)')
        ax.set(xlabel='Radius (m)', ylabel= 'Stress (MPa)', title="Stresses in a Solid Disk at $\omega$ = 10,000 (rpm)")
        ax.legend()

        fig.savefig("solid-disk-stress-strain-5000rpm.png")

        ax.grid()
        plt.show()


# Input Parameters

nu = 0.3        # Poisson ratio 
rho = 7800      # material density (kg/m^3)
w_rpm = 10000   # rotational speed (rpm)
r_in = 0.391    # input radius (m)
r_out = 0.43    # output radius (m)
n_steps = 20    # number of steps


if __name__ == '__main__':
    disk_stresses(nu, rho, w_rpm, r_in, r_out, 20, plot=True)