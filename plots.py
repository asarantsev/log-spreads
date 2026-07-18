import numpy
import pandas
from matplotlib import pyplot as plt

ratings = ['AAA', 'AA', 'A', 'BBB', 'BB', 'B', 'CCC']
for rate in ratings:
    data = pandas.read_excel('BofA-ICE-corporates-monthly.xlsx', sheet_name = rate)
    yields = data['Yield'].values
    months = data['Month'].values
    plt.plot(months, yields, label = rate)
plt.legend()
plt.xlabel('Months')
plt.ylabel('Rates')
plt.show()
    
vixData = pandas.read_excel('vix-monthly.xlsx', sheet_name = 'data')
vix = vixData['VIX'].values
months = vixData['Month'].values
plt.plot(months, vix)
plt.xlabel('Months')
plt.ylabel('VIX')
plt.show()
