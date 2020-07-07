# import libraries
import pandas as pd
import numpy as np
import pygraphviz as pgv

# read data

filename = 'flow-mapping/data-output/edgelist_summ-grouped_JanFeb_breachless_2020-07-06.csv'
df = pd.read_csv(filename, sep = ';')

# initialise graph for example 1
G = pgv.AGraph(name='Jan-Feb-breachless', directed=True)
G.node_attr['shape'] = 'ellipse'
G.node_attr['fixedsize'] = 'false'
G.node_attr['fontsize'] = '20'

for index, row in df.iterrows():
    if row['from'] != 'Admitted':
        G.add_edge(row['from'], row['to'],
                   weight=row['weight'],
                   label=row['weight'],
                   dir="forward",
                   arrowhead="normal",
                   arrowsize=1,
                   style="solid",
                   color='darkseagreen',
     #              penwidth=np.log10(row['weight'],
                   penwidth=100 * row['weight'] / sum(df['weight'])
                   )

G.draw("flow-mapping/media/Jan-Feb-breachless.png", prog='dot')

filename = 'flow-mapping/data-output/edgelist_summ_grouped_JanFeb_2020-07-06.csv'
df = pd.read_csv(filename, sep = ';')

# initialise graph for example 2
G2 = pgv.AGraph(name='Jan-Feb', directed=True)
G2.node_attr['shape'] = 'ellipse'
G2.node_attr['fixedsize'] = 'false'
G2.node_attr['fontsize'] = '20'

for index, row in df.iterrows():
    if row['from'] != 'Admitted':
        G2.add_edge(row['from'], row['to'],
                    weight=row['weight'],
                    label=row['weight'],
                    dir="forward",
                    arrowhead="normal",
                    arrowsize=1,
                    style="solid",
                    color='darkseagreen',
                    penwidth=100 * row['weight'] / sum(df['weight']))

G2.draw("flow-mapping/media/Jan-Feb.png", prog='dot')




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