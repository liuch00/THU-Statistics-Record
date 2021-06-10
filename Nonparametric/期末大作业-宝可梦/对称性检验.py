import numpy as np
from scipy.stats import ks_2samp
import matplotlib.pyplot as plt



def Original_Symmetric_Test(X, method = np.median):
    m = method(X)
    Xp = X[np.where(X > m)]
    Xn = 2*m - X[np.where(X < m)]
    p = ks_2samp(Xp, Xn).pvalue
    return p

def KS_Distance(X):
    l = len(X)
    candidates = np.unique((X.reshape(1, l) + X.reshape(l, 1))/2)
    Ds = []
    for c in candidates:
        Ds.append(ks_2samp(X, 2*c-X).statistic)
    index = np.argmin(Ds)
    m = candidates[index]
    return m, np.mean(X), np.median(X), ks_2samp(X, 2*m-X).statistic

def Modified_Symmetry_Test(X):
    if type(X) != np.ndarray:
        X = np.array(X)
    l = len(X)
    Dstar = KS_Distance(X)[3]
    Ds = []
    for i in range(1200):
        m, mean, median,D = KS_Distance(np.random.randn(l))
        Ds.append(D)
    pvalue = np.mean(np.array(Ds) > (D - 1 / (2 * l)))
    return pvalue

if __name__ == '__main__':

    data=np.loadtxt('data.txt')
    dragon=data[358:392]
    ele = data[313:358]
    fire = data[257:313]
    grass = data[165:257]
    steel = data[121:165]
    water = data[0:121]
    print(Original_Symmetric_Test(dragon))
    print(Original_Symmetric_Test(ele))
    print(Original_Symmetric_Test(fire))
    print(Original_Symmetric_Test(grass))
    print( Original_Symmetric_Test(steel))
    print(Original_Symmetric_Test(water))

    print(Modified_Symmetry_Test(dragon))
    print(Modified_Symmetry_Test(ele))
    print(Modified_Symmetry_Test(fire))
    print(Modified_Symmetry_Test(grass))
    print(Modified_Symmetry_Test(steel))
    print(Modified_Symmetry_Test(water))
