package com.faacets.ariadne

import java.awt.Dimension
import javax.swing.JFrame

import renderer._

object Arbre extends App {

  val racine = Node("Racine", 1.0f, 0)
  val client = Node("Client", 1.0f, 0)
  val demandeClient = Node("DemandeClient", 1.0f, 0)
  val entretienClient = Node("EntretienClient", 1.0f, 0)
  val infosClient = Node("InfosClient", 1.0f, 0)
  val marketRP = Node("MarketRP", 1.0f, 0)
  val decrireMarche = Node("DecrireMarche", 1.0f, 0)
  val calculPrix = Node("CalculPrix", 1.0f, 0)
  val nodes: Seq[Node] = Seq(racine, client, demandeClient, entretienClient, infosClient, marketRP, decrireMarche, calculPrix)
  val edges: Seq[Edge] = Seq(
    Edge(racine, client, 1.0f),
    Edge(client, demandeClient, 1.0f),
    Edge(client, entretienClient, 1.0f),
    Edge(client, infosClient, 1.0f),
    Edge(racine, marketRP, 1.0f),
    Edge(marketRP, decrireMarche, 1.0f),
    Edge(marketRP, calculPrix, 1.0f)
  )
    
  val graph = ThisDirectedGraph(nodes, edges)

  val layout = new SpringLayout(graph)

  val vis = new OpenGLInteractiveGraphRenderer(graph, layout)
  
  val frame = new JFrame("Arbre")
  frame.setPreferredSize(new Dimension(920,720))
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.getContentPane().add(vis) 
  frame.pack()
  frame.setVisible(true)
  
  vis.start
  
}
