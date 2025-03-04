import idx2numpy
import numpy as np

def ecrire_ligne(l,f):
    f.write(str(l[0]));
    for i in range(len(l)):
        f.write(' ' + str(l[i]))
    f.write('\n')

l = ['train-images-idx3', 't10k-images-idx3']
l2 = ['train-labels-idx1', 't10k-labels-idx1']

for x in l:
    file = x+'-ubyte'
    direct = x+'.txt'
    f = open(direct, 'w')
    i = 0
    arr = idx2numpy.convert_from_file(file)
    for x in arr:
        if i%(len(arr)//100) == 0:
            print(i,len(arr))
        for y in x:
            ecrire_ligne(y,f)
        f.write('\n')
        i=i+1
    f.close()

for x in l2:
    file = x+'-ubyte'
    direct = x+'.txt'
    f = open(direct, 'w')
    i = 0
    arr = idx2numpy.convert_from_file(file)
    for x in arr:
        print(i,len(arr),x)
        f.write(str(x)+'\n')
        i=i+1
    f.close()

