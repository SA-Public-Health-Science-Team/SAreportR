
library(DiagrammeR)



 DiagrammeR::grViz("digraph {
    graph [layout = dot, rankdir = TB]

    # define the global styles of the nodes. We can override these in box if we wish

    node [shape     = rectangle,
          style     = filled,
          fillcolor = Linen]

  subgraph cluster_0 {

    label = 'SAprocessR'

    EDAV [label     = 'EDAV',
          shape     = cylinder,
          fillcolor = Grey]

    Grasp [label     = 'Grasp \n SQL \n Server',
           shape     = cylinder, 
           fillcolor = Grey]

    External [label     = 'External \n Source',
              shape     = cylinder,
              fillcolor = Grey]

    Process [label     =  'Process \n Data',
             shape     = polygon,
             fillcolor = Linen]
  }


    subgraph cluster_1 {

    label = 'SAreportR'
    Store [label     = 'Data \n Store',
           shape     = parallelogram,
           fillcolor = Beige]

    Archive [label = 'Report \n Archive',
             shape = folder,
             fillcolor = Beige]

    Template [label     = 'Templates',
              shape     = memo,
              fillcolor = Beige]

    Reports [label     = 'Reports',
             shape     =  document,
             fillcolor = White]
    }


  subgraph cluster_2 {

    label = 'SAdistributeR'
     

    Etemplate [label = 'Email \n Template',
           shape     = signature,
           fillcolor = White]

    Escript [label   = 'Email \n Script',
           shape     = document,
           fillcolor = White]

   }

 subgraph cluster_3 {

    label = 'SAviewR'
    Dashboard [label     = 'Shiny \n Dashboard',
               shape     = rect,
               fillcolor = Beige, margin = 0.25]
    Viewer [label     = 'Viewer']

     Owner [label     = 'Owner']

     Admin [label     = 'Administrator']

     Contributor [label     = 'Contributor']

   }
  # edge definitions with the node IDs
    {Grasp External}  -> Process
    EDAV -> Process
    Process -> EDAV [label = 'Store Data',
                              fontcolor = blue,
                              color = black]

    Process -> Store
    Store -> {Template Dashboard}



    Template -> Reports -> Archive -> Dashboard

    {Viewer  Owner  Admin Contributor} ->  Dashboard
    Reports -> Etemplate -> Escript
}
  ")



