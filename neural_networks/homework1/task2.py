import numpy       as np
import scipy.stats as st

import matplotlib.pyplot as plt

from sklearn import datasets

class Knn:
    def __init__(self, points, targets):
        self.points  = points
        self.targets = targets

    def choose_class(self, point, k = 5):
        data_distances = [ self.euclidean_distance(point, known_point) for known_point in self.points ]
        data           = np.array(zip(self.targets, data_distances))
        indices        = np.argsort( data, 0 )[:, 1]
        sorted_data    = data[indices][0:k]
        most_popular   = st.mode(sorted_data)[0][0][0]
        return most_popular

    def euclidean_distance(self, point1, point2):
        # return np.sqrt( np.square(point1[0] - point2[0]) + np.square(point1[1] - point2[1]) + np.square(point1[2] - point2[2]) + np.square(point1[3] - point2[3]))
        return np.sqrt(np.sum(np.square( point1 - point2 )))

def run(k_array, trials):
    iris = datasets.load_iris()
    results = {}
    for k in k_array:
        results[k] = []
        print "Generowanie K =", k
        for i in range(trials):
            current_misses = 0
            data = np.array(zip(iris.data, iris.target))
            np.random.shuffle(data)
            shuffled_data   = data[:, 0]
            shuffled_target = data[:, 1]
            teaching_data   = shuffled_data[0:100]
            teaching_target = shuffled_target[0:100]
            learning_data   = shuffled_data[100:]
            learning_target = shuffled_target[100:]
            knn = Knn(teaching_data, teaching_target)
            for (point, right_klass) in zip(learning_data, learning_target):
                guessed_class = knn.choose_class(point, k)
                if right_klass != guessed_class:
                    current_misses += 1
            miss_ratio = float(current_misses) / 50
            results[k].append(miss_ratio)
    return results

def generate_chart():
    results = run([1, 3, 5, 7, 9, 11, 13, 15, 17, 19], 500)
    means = {}
    for k, miss_ratios in results.iteritems():
        means[k] = np.mean(miss_ratios) * 100
    means_items = means.items()
    means_items.sort( key=lambda a:a[0])
    sorted_means = np.array(means_items)
    plt.plot( sorted_means[:, 0], sorted_means[:, 1], '-x')
    plt.ylabel("srednia ilosc blednych odpowiedzi [%]")
    plt.xlabel("K")
    plt.savefig("task2.png")

generate_chart()


