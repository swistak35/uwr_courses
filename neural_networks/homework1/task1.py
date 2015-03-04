from numpy import *
import matplotlib.pyplot as plt
from sklearn import datasets

iris = datasets.load_iris()

def initialize_subplot(row_index, col_index):
    subplot_number = row_index * 4 + col_index + 1
    ax = plt.subplot(4, 4, subplot_number)

def generate_hist(row_index, col_index):
    initialize_subplot(row_index, col_index)
    col_data = iris.data[:, col_index]
    for target in set(iris.target):
        plt.hist(col_data, color = ['blue'])

def generate_scatter(row_index, col_index):
    initialize_subplot(row_index, col_index)
    row_data = iris.data[:, row_index]
    col_data = iris.data[:, col_index]
    for target in set(iris.target):
        example_ids = target == iris.target
        plt.scatter(col_data[example_ids], row_data[example_ids], s = 40, label = iris.target_names[target], color = 'bgr'[target], alpha = 0.7)


for row in iris.feature_names:
    row_index = iris.feature_names.index(row)
    for col in iris.feature_names:
        col_index = iris.feature_names.index(col)

        if col_index == row_index:
            generate_hist(row_index, col_index)
        else:
            generate_scatter(row_index, col_index)

        if col_index == 0:
            plt.ylabel(row, fontsize = 12)
        if row_index == 3:
            plt.xlabel(col, fontsize = 12)


ax = plt.subplot(4, 4, 2)
ax.legend(loc = 'upper center', bbox_to_anchor = (1.0, 1.25), ncol = 3, fancybox = True)

fig = plt.figure(1)
fig.set_size_inches(15,15)
fig.savefig("plik.png")

