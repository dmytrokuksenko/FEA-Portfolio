"""
    Plots the stress-strain curve for selected TXT files
"""
import matplotlib.pyplot as plt
import os


def read_txt(file_name, idx=None):
    """
    Reads the data from a TXT file.
    """
    if not idx:
        raise Exception("Element index has not been defined!")
    file = open(file_name,"r")
    values = [0]

    for line in file:
        data = line.split()
        if len(data) > 1:
            try: 
                float(data[idx])
            except ValueError:
                data[idx] = 0
            values.append(abs(float(data[idx]))) 

    return values

def peak_values(stress, strain, model=None):
    """
    Finds peak stress/strain pair.
    """

    max_stress = max(stress)
    idx = [pos for pos, val in enumerate(stress) if val == max_stress]
    max_strain = strain[idx[0]]
    print('Max stress is: ', max_stress, '('+ model +')')
    print('Max strain is: ', max_strain, '('+ model +')')

    return max_stress, max_strain


files = []

path = os.getcwd()+'\\data'
for (_, _, file) in os.walk(path):
    if '.txt' in file:
        files.append(file)


os.chdir(os.getcwd()+'\\data')
stress_linde = read_txt('stress-linde.txt', idx=3)
strain_linde = read_txt('strain-linde.txt', idx=3)

stress_ortho = read_txt('stress-ortho.txt', idx=3)
strain_ortho = read_txt('strain-ortho.txt', idx=3)

stress_tsaiwu = read_txt('stress-tsaiwu.txt', idx=3)
strain_tsaiwu = read_txt('strain-tsaiwu.txt', idx=3)

stress_maxstress = read_txt('stress-maxstress.txt', idx=3)
strain_maxstress = read_txt('strain-maxstress.txt', idx=3)

stress_hashin = read_txt('stress-hashin.txt', idx=3)
strain_hashin = read_txt('strain-hashin.txt', idx=3)

stress_maxstrain = read_txt('stress-maxstrain.txt', idx=3)
strain_maxstrain = read_txt('strain-maxstrain.txt', idx=3)


peak_values(stress_linde, strain_linde, model='Linde')
peak_values(stress_tsaiwu, strain_tsaiwu, model='Tsai-Wu')
peak_values(stress_maxstress, strain_maxstress, model='Max Stress')
peak_values(stress_hashin, strain_hashin, model='Hashin')
peak_values(stress_maxstrain, strain_maxstrain, model='Max Strain')


ax = plt.axes()
fontdict = {'fontname':'sans-serif', 'size':14, 'fontweight': 'bold'}
plt.title("Stress-Strain Relationship", fontdict=fontdict)
plt.xlabel(r'$\epsilon_{11}$', fontdict=fontdict)
plt.ylabel(r'$\sigma_{11}$ (MPa)', fontdict=fontdict)
# plt.hlines(max_stress, 0, max_strain, color='red', linestyle='--')
# plt.vlines(max_strain, 0, max_stress, color='red', linestyle='--')
plt.plot(strain_linde, stress_linde, color='blue', linestyle='--', marker='o', markersize=4, label='Linde')
plt.plot(strain_tsaiwu, stress_tsaiwu, color='green', linestyle='--', marker='o', markersize=4, label='Tsai-Wu')
plt.plot(strain_maxstress, stress_maxstress, color='orange', linestyle='--', marker='d', markersize=4, label='Max Stress')
plt.plot(strain_maxstrain, stress_maxstrain, color='purple', linestyle='--', marker='x', markersize=4, label='Max Strain')
plt.plot(strain_hashin, stress_hashin, color='red', linestyle='--', marker='o', markersize=4, label='Hashin')
plt.plot(strain_ortho, stress_ortho, color='black', linestyle='--', marker='o', markersize=4, label='Built-in Ortho (no damage)')
# plt.plot(max_strain, max_stress, color='red', marker='o', markersize=6)
plt.xlim(left=0)
plt.ylim(bottom=0)
plt.legend()
plt.yticks()
plt.show()