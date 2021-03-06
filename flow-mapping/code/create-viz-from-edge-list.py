# import libraries
import pandas as pd
import numpy as np
import pygraphviz as pgv

# Create graph for all patients
# =============================

# read data

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/edgelist_stats_with_meas_JanFeb_2020-08-26.csv'
edgelist_stats = pd.read_csv(filename, sep =',', dtype = {"weight_mean" : "float64"})

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/node_stats_with_meas_JanFeb_2020-08-26.csv'
node_stats = pd.read_csv(filename, sep =',')

# reorder nodes
#node_stats = node_stats.loc[['Waiting', 'RAT', 'Triage', 'RESUS', 'MAJORS', 'UTC', 'RAT', 'Admitted', 'Discharged'], :]
#node_stats = node_stats.loc[[4,0,3,9,6,10,1,7,2,11,8,5], :]

# cheat workaround to get mean to round
#edgelist_stats.weight_mean = edgelist_stats.weight_mean.astype(int)
edgelist_stats.weight_mean = edgelist_stats.weight_mean.round(1)

# create node label
node_label_num = ' ('+node_stats['num_pat_mean'].astype(int).copy().astype(str)+')'
node_stats['node_label'] = node_stats['room7'].str.cat(node_label_num)

# create edge label
edgelist_stats['pct_disc_mean'] = edgelist_stats['pct_disc_mean'].replace(np.nan, 999)
edge_label_num = edgelist_stats['weight_mean'].round(1).copy().astype(str)+' ('+((1-edgelist_stats['pct_disc_mean'])*100).astype(int).copy().astype(str)+'%)'
edgelist_stats['edge_label'] = edge_label_num

# initialise graph
G = pgv.AGraph(name='JanFeb', directed=True,
               labelloc = 't', label='Daily mean node and edge totals for all patients Jan and Feb\nwith % admitted for each edge', fontsize = '16')
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '10'
G.graph_attr['rankdir'] = 'LR'

# add all nodes to graph
for index, row in node_stats[node_stats['num_pat_mean']>3].iterrows():
    # note this if statement is a workaround because Admitted and Discharged node numbers are wrong
    if row['room7'] != 'Discharged' and  row['room7'] != 'Admitted':
        G.add_node(row['room7'],
                   label = row['node_label']
                   )
    else:
        G.add_node(row['room7'],
                   label = row['room7']
                   )

# add all edges to graph -  where weight > 1
for index, row in edgelist_stats[edgelist_stats['weight_mean']>3].iterrows():
    if row['from'] != 'Admitted' and row['weight_mean'] > 3:
        G.add_edge(row['from'], row['to'],
                   weight=row['weight_mean'],
                   label=row['edge_label'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='darkseagreen',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight_mean'] / sum(edgelist_stats['weight_mean'])
                   )
#s = G.subgraph()
#s.graph_attr['rank']='same'
#s.add_node('TRIAGE')
#s.add_node('RAT')

#G.draw("flow-mapping/media/Jan-Feb-all-wtgt3.png", prog='dot')
G.draw("flow-mapping/media/Jan-Feb-all-wtgt3.png", prog='dot')

# Create graph for average day
# ============================
filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/edgelist_summ_with_meas_Feb_21_2020-08-26.csv'
edgelist_summ = pd.read_csv(filename, sep =',')

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/node_daily_with_meas_Feb_21_2020-08-26.csv'
node_stats = pd.read_csv(filename, sep =',')

edge_label_num = edgelist_summ['weight'].astype(int).copy().astype(str)
edgelist_summ['edge_label'] = edge_label_num

# create node label
node_label_num = ' ('+node_stats['daily_num_pat'].astype(int).copy().astype(str)+')'
node_stats['node_label'] = node_stats['room7'].str.cat(node_label_num)

# initialise graph
G4 = pgv.AGraph(name='21-Feb-all', directed=True,
               labelloc = 't', label='Flows for all patients on 21 Feb', fontsize = '16')
G4.node_attr['shape'] = 'ellipse'
G4.node_attr['fixedsize'] = 'false'
G4.node_attr['fontsize'] = '10'
G4.graph_attr['rankdir'] = 'LR'

# add all nodes to graph
for index, row in node_stats.iterrows():
    G4.add_node(row['room7'],
               label = row['node_label']
               )

# add all edges to graph -  where weight > 1
for index, row in edgelist_summ[edgelist_summ['weight']>0].iterrows():
    if row['from'] != 'Admitted' and row['weight'] > 0:
        G4.add_edge(row['from'], row['to'],
                   weight=row['weight'],
                   label=row['edge_label'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='darkseagreen',
                   penwidth=100 * row['weight'] / sum(edgelist_summ['weight']))

G4.draw("flow-mapping/media/Jan-Feb-all-wtgt0.png", prog='dot')

# Create graph for breach patients
# ================================

# read data

filename = '/Users/zellaking/GitHubRepos/EDcrowding/flow-mapping/data-output/edgelist_stats_JanFeb_breach_2020-07-27.csv'
edgelist_stats_JanFeb_breach = pd.read_csv(filename, sep =',', dtype = {"weight_mean" : "float64"})

# cheat workaround to get mean to round
edgelist_stats_JanFeb_breach.weight_mean = edgelist_stats_JanFeb_breach.weight_mean.astype(int)

# create node label
node_label_num_breach = ' ('+node_stats['num_pat_mean_breach'].astype(int).copy().astype(str)+')'
node_stats['node_label_breach'] = node_stats['room4_new'].str.cat(node_label_num_breach)

# create edge label
edge_label_num_breach = edgelist_stats_JanFeb_breach['weight_mean'].astype(int).copy().astype(str)+' ('+((1-edgelist_stats_JanFeb_breach['pct_disc_mean'])*100).astype(int).copy().astype(str)+'%)'
edgelist_stats_JanFeb_breach['edge_label'] = edge_label_num_breach


# initialise graph
G = pgv.AGraph(name='Jan-Feb-breach', directed=True,
               labelloc='t',
               label='Daily mean node and edge totals for all breach patients in Jan and Feb\nwith % admitted for each edge',
               fontsize='16')
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '10'
G.graph_attr['rankdir'] = 'LR'

# add all nodes to graph

# add all nodes to graph
for index, row in node_stats.iterrows():
    # note this if statement is a workaround because Admitted and Discharged node numbers are wrong
    if row['room4_new'] != 'Discharged' and  row['room4_new'] != 'Admitted':
        G.add_node(row['room4_new'],
                   label = row['node_label_breach']
                   )
    else:
        G.add_node(row['room4_new'],
                   label = row['room4_new']
                   )

# add all edges to graph - only where weight > 2
for index, row in edgelist_stats_JanFeb_breach[edgelist_stats_JanFeb_breach['weight_mean']>2].iterrows():
    if row['from'] != 'Admitted' and row['weight_mean'] > 1:
        G.add_edge(row['from'], row['to'],
                   weight=row['weight_mean'],
                   label=row['edge_label'],
                   dir="forward",
                   fontsize='10',
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='DarkSalmon',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight_mean'] / sum(edgelist_stats_JanFeb_breach['weight_mean'])
                   )

G.draw("flow-mapping/media/JanFeb_breach_2020-07-28.png", prog='dot')



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