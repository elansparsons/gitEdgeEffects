###heatmaps for meta-analysis

import matplotlib.pyplot as plt
import numpy as np

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42


###AT
ATdata = np.genfromtxt('heatpointsAT.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

x = ATdata[:,0]
y = ATdata[:,1]

fig, (ax,ax2) = plt.subplots(nrows=2,sharex=True)

extent = [x[0]-(x[1]-x[0])/2., x[-1]+(x[1]-x[0])/2.,0,1]
ax.imshow(y[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent)
ax.set_yticks([])
ax.set_xlim([-10,200])

ax2.plot(x,y)

plt.tight_layout()


plt.savefig("ATfig.pdf", transparent=True)




###RH
RHdata = np.genfromtxt('heatpointsRH.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

a = RHdata[:,0]
b = RHdata[:,1]

fig2, (ax3,ax4) = plt.subplots(nrows=2,sharex=True)

extent2 = [a[0]-(a[1]-a[0])/2., a[-1]+(a[1]-a[0])/2.,0,1]
ax3.imshow(-b[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent2)
ax3.set_yticks([])
ax3.set_xlim([-10,200])

ax4.plot(a,b)

plt.tight_layout()

plt.savefig("RHfig.pdf", transparent=True)



###VPD
VPDdata = np.genfromtxt('heatpointsVPD.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

c = VPDdata[:,0]
d = VPDdata[:,1]

fig3, (ax5,ax6) = plt.subplots(nrows=2,sharex=True)

extent3 = [c[0]-(c[1]-c[0])/2., c[-1]+(c[1]-c[0])/2.,0,1]
ax5.imshow(d[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent3)
ax5.set_yticks([])
ax5.set_xlim([-10,200])

ax6.plot(c,d)

plt.tight_layout()

plt.savefig("VPDfig.pdf", transparent=True)


###ST
STdata = np.genfromtxt('heatpointsST.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

e = STdata[:,0]
f = STdata[:,1]

fig4, (ax7,ax8) = plt.subplots(nrows=2,sharex=True)

extent4 = [e[0]-(e[1]-e[0])/2., e[-1]+(e[1]-e[0])/2.,0,1]
ax7.imshow(f[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent4)
ax7.set_yticks([])
ax7.set_xlim([-10,200])

ax8.plot(e,f)

plt.tight_layout()

plt.savefig("STfig.pdf", transparent=True)


###SM
SMdata = np.genfromtxt('heatpointsSM.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

g = SMdata[:,0]
h = SMdata[:,1]

fig5, (ax9,ax10) = plt.subplots(nrows=2,sharex=True)

extent5 = [g[0]-(g[1]-g[0])/2., g[-1]+(g[1]-g[0])/2.,0,1]
ax9.imshow(-h[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent5)
ax9.set_yticks([])
ax9.set_xlim([-10,200])

ax10.plot(g,h)

plt.tight_layout()

plt.savefig("SMfig.pdf", transparent=True)


###PAR
PARdata = np.genfromtxt('heatpointsPAR.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

i = PARdata[:,0]
j = PARdata[:,1]

fig6, (ax11,ax12) = plt.subplots(nrows=2,sharex=True)

extent6 = [i[0]-(i[1]-i[0])/2., i[-1]+(i[1]-i[0])/2.,0,1]
ax11.imshow(j[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent6)
ax11.set_yticks([])
ax11.set_xlim([-10,200])

ax12.plot(i,j)

plt.tight_layout()

plt.savefig("PARfig.pdf", transparent=True)


###WS
WSdata = np.genfromtxt('heatpointsWS.csv',delimiter=',',skip_header=1)

plt.rcParams["figure.figsize"] = 5,2

k = WSdata[:,0]
l = WSdata[:,1]

fig7, (ax13,ax14) = plt.subplots(nrows=2,sharex=True)

extent7 = [k[0]-(k[1]-k[0])/2., k[-1]+(k[1]-k[0])/2.,0,1]
ax13.imshow(l[np.newaxis,:],cmap="plasma",aspect="auto",extent=extent7)
ax13.set_yticks([])
ax13.set_xlim([-10,200])

ax14.plot(k,l)

plt.tight_layout()

plt.savefig("WSfig.pdf", transparent=True)




