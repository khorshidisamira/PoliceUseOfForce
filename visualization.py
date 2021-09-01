# -*- coding: utf-8 -*-
"""
Created on Sun Jun 21 13:32:10 2020

@author: Samira
"""
# Importing the libraries
import numpy as np
import pandas as pd
from scipy import sparse
import random
import networkx as nx
import matplotlib.pyplot as plt
random.seed(7)

# Importing the dataset
dataset = pd.read_csv('complaints.csv')
X = dataset.loc[:,["CRID","OfficerID"]]


Gbipartite = nx.Graph()

e = zip(X['CRID'],X['OfficerID'])

Gbipartite.add_edges_from(zip(X['CRID'],X['OfficerID']))

largest_cc = max(nx.connected_components(Gbipartite), key = len)

pos = nx.spring_layout(Gbipartite)

Gcomplaint = nx.Graph()
Gcomplaint.add_nodes_from(X['CRID'])

    
fig = plt.figure(figsize = (8,6))
nx.draw(Gbipartite, pos = pos, node_size = 8, node_color = 'blue')
nx.draw(Gcomplaint, pos = pos, node_size = 14, node_color = 'red')
plt.show()


largest_cc = max(nx.connected_components(Gbipartite), key = len)
Gc = nx.subgraph(Gbipartite, largest_cc)
nx.shortest_path(Gbipartite, 7727, 2360)
 
Gofficers = nx.algorithms.bipartite.projected_graph(Gbipartite, set(X['OfficerID']))
Gdummy = nx.algorithms.bipartite.projected_graph(Gbipartite, set(X['OfficerID']), multigraph = True)
dummy_edges = Gdummy.edges()

from collections import Counter
c = Counter(Gdummy.edges())  # Contains frequencies of each directed edge.


for u, v, d in Gofficers.edges(data=True):
    d['weight'] = c[u, v]

Gofficers.edges(data=True)

Gfrequent = nx.Graph()
for u, v, d in Gofficers.edges(data=True):
    if d['weight']>1: Gfrequent.add_edge(u,v)
    


largest_cc = max(nx.connected_components(Gfrequent), key = len)
Gc = nx.subgraph(Gfrequent, largest_cc)
len(Gc)

pos0 = nx.spectral_layout(Gc)
pos = nx.kamada_kawai_layout(Gc, pos = pos0)


fig = plt.figure(figsize = (8,6))
nx.draw(Gc, pos = pos, node_size = 8, node_color = 'blue')
plt.show()
fig.savefig("connection between  officers with more than 1 common complains in the dataset.pdf", bbox_inches='tight')

shortest_path = nx.average_shortest_path_length(Gc)#On avarage 12 bsteps is between two arbitary officers
