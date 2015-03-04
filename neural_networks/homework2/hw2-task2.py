
# coding: utf-8

# In[40]:

import pylab as pl
import scipy.stats as st

from sklearn.datasets import fetch_mldata

MINIBATCH = 10

class Knn:
    def __init__(self, data, targets):
        self.data  = pl.reshape(data, (1, data.shape[0], data.shape[1]))
        print self.data.shape
        self.targets = targets

    def choose_class(self, testing_data_points, k = 5):
        testing_data = pl.reshape(testing_data_points, (testing_data_points.shape[0], 1, testing_data_points.shape[1]))
        print testing_data.shape
        diff = self.data - testing_data
        dists = pl.sum(pl.square(diff), 2)
        results = []
        for probe in dists:
            results.append(st.mode([self.targets[i] for i in probe.argsort()[:k]])[0][0])
        return results
#         data_distances = [ self.euclidean_distance(point, known_point) for known_point in self.points ]
#         data           = np.array(zip(self.targets, data_distances))
#         indices        = np.argsort( data, 0 )[:, 1]
#         sorted_data    = data[indices][0:k]
#         most_popular   = st.mode(sorted_data)[0][0][0]
#         return most_popular

#     def euclidean_distance(self, point1, point2):
        # return np.sqrt( np.square(point1[0] - point2[0]) + np.square(point1[1] - point2[1]) + np.square(point1[2] - point2[2]) + np.square(point1[3] - point2[3]))
#         return np.sqrt(np.sum(np.square( point1 - point2 )))

mnist = fetch_mldata('MNIST original')

real_data = mnist['data'][:60000]
real_target = mnist['target'][:60000]
testing_data = mnist['data'][60000:]


knn = Knn(real_data, real_target)
# dists = knn.choose_class(testing_data)
sliced_testing_data = testing_data.reshape(-1, 5, 784)
for index, testing_slice in enumerate(sliced_testing_data):
    print "Iteracja %i" % index
    results = knn.choose_class(testing_slice)
# print sliced_testing_data[0][0]



# print dists.shape
# print dists
# print [real_target[i] for i in dists[0].argsort()[:5]]




# data_samples = []
# for index, target in enumerate(mnist['target']):
#     if len(data_samples) == int(target):
#         data_samples.append(mnist['data'][index])

# for index, data in enumerate(data_samples):
#     pl.subplot(2, 5, index + 1)
#     pl.axis('off')
#     pl.title("Obrazek %i" % index)
#     pl.imshow(pl.reshape(data, (28, 28)), cmap = pl.cm.gray_r, interpolation = 'nearest')

# pl.show()


# In[ ]:



