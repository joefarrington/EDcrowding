# import libraries
import pandas as pd
import numpy as np
import pygraphviz as pgv

# Create graph for all patients
# =============================

# read data

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/edgelist_stats_JanFeb_2020-07-21.csv'
edgelist_stats = pd.read_csv(filename, sep =',', dtype = {"weight_mean" : "float64"})

filename = 'flow-mapping/data-output/node_stats_JanFeb_2020-07-22.csv'
node_stats = pd.read_csv(filename, sep =',')

# cheat workaround to get mean to round
edgelist_stats.weight_mean = edgelist_stats.weight_mean.astype(int)

# create node label
node_label_num = ' ('+node_stats['num_pat_mean'].astype(int).copy().astype(str)+')'
node_stats['node_label'] = node_stats['room4'].str.cat(node_label_num)

# initialise graph
G = pgv.AGraph(name='Jan-Feb-all', directed=True,
               labelloc = 't', label='\nDaily means for all patients in Jan and Feb', fontsize = '20')
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '10'

# add all nodes to graph

for index, row in node_stats.iterrows():
    G.add_node(row['room4'],
               label = row['node_label']
               )

# add all edges to graph
for index, row in edgelist_stats.iterrows():
    if row['from'] != 'Admitted' and row['weight_mean'] > 1:
        G.add_edge(row['from'], row['to'],
                   weight=row['weight_mean'],
                   label=row['weight_mean'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='darkseagreen',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight_mean'] / sum(edgelist_stats['weight_mean'])
                   )

G.draw("flow-mapping/media/Jan-Feb-all.png", prog='dot')

# Create graph for breach patients
# ================================

# read data

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/edgelist_stats_JanFeb_breach_2020-07-21.csv'
edgelist_stats_JanFeb_breach = pd.read_csv(filename, sep =',', dtype = {"weight_mean" : "float64"})

# cheat workaround to get mean to round
edgelist_stats_JanFeb_breach.weight_mean = edgelist_stats_JanFeb_breach.weight_mean.astype(int)

# create node label
node_label_num_breach = ' ('+node_stats['num_pat_mean_breach'].astype(int).copy().astype(str)+')'
node_stats['node_label_breach'] = node_stats['room4'].str.cat(node_label_num_breach)

# initialise graph
G = pgv.AGraph(name='Jan-Feb-breach', directed=True,
               labelloc = 't', label='\nDaily means for patients who breached in Jan and Feb', fontsize = '20')
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '10'

# add all nodes to graph

for index, row in node_stats.iterrows():
    G.add_node(row['room4'],
               label = row['node_label_breach']
               )

# add all edges to graph
for index, row in edgelist_stats_JanFeb_breach.iterrows():
    if row['from'] != 'Admitted' and row['weight_mean'] > 1:
        G.add_edge(row['from'], row['to'],
                   weight=row['weight_mean'],
                   label=row['weight_mean'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='DarkSalmon',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight_mean'] / sum(edgelist_stats_JanFeb_breach['weight_mean'])
                   )

G.draw("flow-mapping/media/Jan-Feb-breach.png", prog='dot')



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