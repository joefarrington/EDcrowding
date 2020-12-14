# import libraries
import pandas as pd
import numpy as np
import pygraphviz as pgv

# Create graph for missing ED
# =============================

# read data

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/missing_ED_edgelist_summ_2020-12-09.csv'
edgelist_sum = pd.read_csv(filename, sep =',', dtype = {"weight_mean" : "float64"})


# create edge label
edge_label_num = edgelist_sum['weight']
edgelist_sum['edge_label'] = edge_label_num


# initialise graph
G = pgv.AGraph(name='MissingED', directed=True,
               labelloc = 't', label='Missing ED rows', fontsize = '16')
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '10'
G.graph_attr['rankdir'] = 'LR'

# add all edges to graph -  where weight > 1
for index, row in edgelist_sum[edgelist_sum['weight'] > 1].iterrows():
    if row['weight'] > 1:
        G.add_edge(row['from'], row['to'],
                   weight=row['weight'],
                   label=row['edge_label'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='darkseagreen',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight'] / sum(edgelist_sum['weight'])
                   )
#s = G.subgraph()
#s.graph_attr['rank']='same'
#s.add_node('TRIAGE')
#s.add_node('RAT')

#G.draw("flow-mapping/media/Jan-Feb-all-wtgt3.png", prog='dot')
G.draw("flow-mapping/media/missing_ED.png", prog='dot')


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