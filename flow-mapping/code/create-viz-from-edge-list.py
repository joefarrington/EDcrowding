# import libraries
import pandas as pd
import numpy as np
import pygraphviz as pgv

# read data

filename = 'EDcrowding/flow-mapping/data-raw/edgelist_summ_gt1002020-06-24.csv'
df = pd.read_csv(filename, sep = ';')

# initialise graph for example 1
G = pgv.AGraph(name='ED-pre-COVID', directed=True)
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '20'

for index, row in df.iterrows():
    G.add_edge(row['from'], row['to'],
               weight=row['weight'],
               dir="forward",
               arrowhead="normal",
               arrowsize=1,
               style="solid",
               penwidth=200 * row['weight'] / sum(df['weight']))

G.draw("EDCrowding/flow-mapping/media/example.png", prog='dot')

# initialise graph for example 2
G2 = pgv.AGraph(name='ED-pre-COVID', directed=True)
G2.node_attr['shape'] = 'ellipse'
G2.node_attr['fixedsize'] = 'false'
G2.node_attr['fontsize'] = '20'

for index, row in df.iterrows():
    G2.add_edge(row['from'], row['to'],
                weight=row['weight'],
                label=row['weight'],
                dir="forward",
                arrowhead="normal",
                arrowsize=1,
                style="solid",
                color='darkseagreen',
                penwidth=200 * row['weight'] / sum(df['weight']))

G2.draw("EDCrowding/flow-mapping/media/example2.png", prog='dot')




# to interrogate graph
#
# G.number_of_nodes()
# G.number_of_edges()
# list(G.nodes())
# list(G.edges())
# G['ADULT TRIAGE']  # list of adjacent nodes to the node specified
# G.degree()
# G.degree('DIAGNOSTICS')
# G.remove_edge('ED', 5)