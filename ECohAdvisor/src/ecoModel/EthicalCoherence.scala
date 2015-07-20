package ecoModel

import scala.swing._
import scala.swing.Panel
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import BorderPanel.Position._
import ScrollPane._
import event._

import scala.io.Source
import java.io._

import java.awt.Color

import java.awt.Paint
import java.awt.Shape
import javax.swing.JRootPane;

import java.awt.geom.AffineTransform
import java.awt.geom.Ellipse2D

import javax.swing.JComponent

import org.apache.commons.collections15.Transformer;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout
import edu.uci.ics.jung.algorithms.layout.FRLayout2
import edu.uci.ics.jung.algorithms.layout.util.Relaxer
import edu.uci.ics.jung.graph.UndirectedSparseMultigraph
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.ObservableGraph
import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer

import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory

import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.NodeList
import org.xml.sax.InputSource;

import java.io.File;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import scala.collection.mutable.Map



object EthicalCoherence extends SimpleSwingApplication {
  var g: Graph[String,Int] = null;
  var og: ObservableGraph[String,Int] =null;
  var ig = Graphs.synchronizedUndirectedGraph[String,Int](new UndirectedSparseMultigraph[String,Int]())
  og = new ObservableGraph[String,Int](ig)   
  g = og; 
  var vv : VisualizationViewer[String,Int] = null
  var layout : AbstractLayout[String,Int]  = null
  val ACTIVATION = 0.1
  val ACTIVATIONTHRESHOLD = 0.50
  val INITIALTHRESHOLD = 0.5
  val CTHRESHOLD = 0.001 
  val MAXITERATION = 300
  val MAX = 1.0
  val MIN = -1.0
  val DECAYRATE = 0.05
  var activations:Map[String,Double]  = Map()
  var edgeWeights:Map[Int,Double] = Map()
  
  def top =new MainFrame {
    title = "Ethical Coherence Advisor Tool"
  
    var compileButton = new Button {
      text = "Compile"
    }
    
    var displayButton = new Button {
      text = "Display Network"
    }
    
    var computeButton = new Button {
      text = "Compute Active Nodes"
    }
     
    var simulateButton = new Button {
      text = "Simulate Process"
    }
    
    var clearButton = new Button {
      text = "Clear Model"
    }
    
    
   var clearConsoleButton = new Button {
      text = "Clear Console"
    }
    
    var controlPanel = new FlowPanel() { 
      contents+=compileButton
      contents+=displayButton
      contents+=computeButton
      contents+=simulateButton
      contents+=clearButton
      contents+=clearConsoleButton
    }
    
    var text = ""
    var textArea1 = new TextArea(text, 10, 80);
    textArea1.preferredSize_=(new Dimension(100,100))
    textArea1.lineWrap_=(true)
    var textArea2 = new TextArea(text, 300, 25);
    textArea2.lineWrap_=(true)
    textArea2.preferredSize_=(new Dimension(100, 100));
    
     
    var scrollPane = new ScrollPane(textArea2) {
      verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
      horizontalScrollBarPolicy=ScrollPane.BarPolicy.Always
    }
    
     var scrollPaneR = new ScrollPane(textArea1) {
      verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
      horizontalScrollBarPolicy=ScrollPane.BarPolicy.Always
    }
    
    var modelPanel = new BoxPanel(Orientation.Vertical) {
      contents+=scrollPane
    }
    
    var resultsPanel = new BoxPanel(Orientation.Horizontal) {
      contents+=scrollPaneR
    }
    
    
    
    layout = new FRLayout2[String,Int](g);
    vv = new VisualizationViewer[String,Int](layout, new Dimension(600,600));
    
    var vvWrap = Component.wrap(vv)
    
    var visPanel = new BoxPanel(Orientation.Vertical) {
       contents+= vvWrap
   
     }
    visPanel.preferredSize=new Dimension(670,600)
    
     //visPanel.size.setSize(new Dimension(500,500))
    
    
     preferredSize= new Dimension(1000,1000)
    
    contents = new BorderPanel() {
      layout(controlPanel)=North
      layout(modelPanel)=East
      layout(visPanel)=West
      layout(resultsPanel)=South
      
    }

    var modelFileName : String = null
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Load") {
            var chooser = new FileChooser
            val result=chooser.showOpenDialog(null)
            if (result == FileChooser.Result.Approve) {
              println("Approve -- " + chooser.selectedFile)
              Some(chooser.selectedFile)
            } else None
            
            
            try {
               Source.fromFile(chooser.selectedFile).foreach { x => textArea2.text+=x } 
             } 
            catch {
                case ex: FileNotFoundException  => {
                println("Missing file exception")
              }
          }
 
        }   );
        contents += new MenuItem(Action("Save") {
          if (modelFileName != null) {
            val fw = new FileWriter(modelFileName) ; fw.write(textArea2.text) ; fw.close() 
          }
        });
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    
    listenTo(clearButton)
    listenTo(computeButton)
    listenTo(compileButton)
    listenTo(displayButton)
    listenTo(clearConsoleButton)
    
    reactions += {
      case ButtonClicked(component) if component == clearButton => 
        initializeCoherenceGraph()
        //textArea2.text=" "
        display()
        
      case ButtonClicked(component) if component == clearConsoleButton => 
        textArea1.text=" "
       
        
      case ButtonClicked(component) if component == compileButton => 
        textArea2.selectAll()
        textArea2.text=textArea2.selected     
        xmlModelParse(textArea2)
        graphIterator()
             
      case ButtonClicked(component) if component == displayButton =>
          display()
         
         
       case ButtonClicked(component) if component == computeButton =>
          
         textArea1.append("Activation levels before coherence maximizer\n")
         reportActivationLevels(textArea1)
         activationCompute()
         textArea1.append("Activation levels after coherence maximizer\n")
         reportActivationLevels(textArea1)
           
           
        var relaxer : Relaxer = vv.getModel().getRelaxer();
        var vertexPaint = new Transformer[String,Paint]() {
                  def transform (i:String) : Paint = {
                    var R=0
                    var B=0;
                    if (activations(i) > ACTIVATIONTHRESHOLD) {R=1;B=0} else {R=0;B=1}
                    new Color(new java.lang.Float(R),new java.lang.Float(0),new java.lang.Float(B))         
                  }
              };
   
         var vertexSize =  new Transformer[String,Shape](){
                  def  transform(i: String) : Shape = {
                      var circle = new Ellipse2D.Double(-15, -15, 50, 50);
                      // in this case, the vertex is twice as large
                       return AffineTransform.getScaleInstance(1,1).createTransformedShape(circle);
                  }
              };
         
         var colorTransformer = new Transformer[Int,Paint]() {
                 def transform(i:Int):Paint = { 
                    if (edgeWeights(i)<0) {return Color.RED}
                    else return Color.BLACK
                 }
         }        
         
         vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint)
         vv.getRenderContext().setVertexShapeTransformer(vertexSize)
         vv.getRenderContext().setArrowFillPaintTransformer(colorTransformer)
         vv.getRenderContext().setArrowDrawPaintTransformer(colorTransformer)
         vv.getRenderContext().setEdgeDrawPaintTransformer(colorTransformer)
         layout.initialize();
         relaxer.resume();
       
       
       case ButtonClicked(component) if component == simulateButton =>
       
    }
    
}
  
    def display() {
      var relaxer = vv.getModel().getRelaxer();
          vv.getModel().getRelaxer().setSleepTime(500);
          vv.setGraphMouse(new DefaultModalGraphMouse[Number,Number]());
          vv.getRenderer().getVertexLabelRenderer().setPosition(Renderer.VertexLabel.Position.CNTR);
          vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller[String]());
          vv.setForeground(Color.white);
           
       
        var vertexPaint = new Transformer[String,Paint]() {
                  def transform (i:String) : Paint = {
                    var R=0
                    var B=0;
                    if (activations(i) > INITIALTHRESHOLD) {R=1;B=0} else {R=0;B=1}
                    new Color(new java.lang.Float(R),new java.lang.Float(0),new java.lang.Float(B))          
                  }
              };
   
         var vertexSize =  new Transformer[String,Shape](){
                  def  transform(i: String) : Shape = {
                      var circle = new Ellipse2D.Double(-15, -15, 50, 50);
                      // in this case, the vertex is twice as large
                       return AffineTransform.getScaleInstance(1,1).createTransformedShape(circle);
                  }
              };
              
         var colorTransformer = new Transformer[Int,Paint]() {
                 def transform(i:Int):Paint = { 
                    if (edgeWeights(i)<0) {return Color.RED}
                    else return Color.BLACK
                 }
         }     
              
         vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint)
         vv.getRenderContext().setVertexShapeTransformer(vertexSize)
         vv.getRenderContext().setArrowFillPaintTransformer(colorTransformer)
         vv.getRenderContext().setArrowDrawPaintTransformer(colorTransformer)
         vv.getRenderContext().setEdgeDrawPaintTransformer(colorTransformer)
         
         layout.initialize();
         relaxer.resume();
    }
  
    def reportActivationLevels(textArea:TextArea) {
      var V = g.getVertices
      var itr = V.iterator()
     
      while (itr.hasNext()) {
        var myN = itr.next()
        System.out.print("Node name: "+myN+ " ")
        textArea.append(""+myN+ ":")
        System.out.println("Activation: "+ activations(myN))
        textArea.append(""+ activations(myN))
        textArea.append("  ")
      }
      textArea.append("\n")
    }
  
    def graphIterator() {
      
      var V = g.getVertices
      var itr = V.iterator()
      System.out.println("Vertex List: ")
      while (itr.hasNext()) {
        var myN = itr.next()
        System.out.print("Node name: "+myN+ " ")
        System.out.println("Activation: "+ activations(myN))
         
        var incIter = g.getIncidentEdges(myN).iterator()
        while (incIter.hasNext()) {
          var myEdge = incIter.next()
          System.out.println("Incident edge number: "+ myEdge+" Weight is " + edgeWeights(myEdge) +" Source is "+g.getEndpoints(myEdge).getFirst+" Target is "+ g.getEndpoints(myEdge).getSecond)
        }
      }   
    }
    
    
    def activationCompute() {
      
      var maxChange = 1.0
      var count =0;
      var V = g.getVertices
      var itr = V.iterator()
      var activationsatT = activations.clone()
      while ((maxChange > CTHRESHOLD) && (count < MAXITERATION)) {
        while (itr.hasNext()) {
          var myNode=itr.next()
          var newA = update(myNode,activationsatT)
          var diffA = Math.abs(newA-activations(myNode))
          if (diffA < maxChange) maxChange = diffA 
          activations(myNode)=newA       
        }
        itr=V.iterator()
        activationsatT = activations.clone()
        count+=1
      }
      activations.foreach(p => println("node= "+p._1 + ", activation= "+p._2))
    }
    
    def update (str:String, actAtT:Map[String,Double]): Double = {
      var cActivation = actAtT(str)
      var nActivation=0.0
      var netFlow = 0.0;
      var itr = g.getIncidentEdges(str).iterator()
      while (itr.hasNext()) {
        var myEdge = itr.next()
        var vt = g.getOpposite(str,myEdge)
        netFlow += edgeWeights(myEdge)*actAtT(vt) 
      }
      
      if (netFlow > 0) {
        nActivation=(cActivation*(1.0-DECAYRATE))+(netFlow*(MAX-cActivation))
        nActivation=Math.max(-1.0,Math.min(1.0, nActivation))
      }
      else {
        nActivation=(cActivation*(1.0-DECAYRATE))+(netFlow*(cActivation-MIN))
        nActivation = Math.min(Math.max(-1.0, nActivation),1.0)
      }
      
      return nActivation
    }
    
    
    def initializeCoherenceGraph() {
      
      if (g.getVertexCount>0) {
        var V = g.getVertices
        var vertices=V.toArray()
        for (i <- 0 to g.getVertexCount-1) {
          g.removeVertex(vertices(i).asInstanceOf[String])
        }
      } 
      var eC= g.getEdgeCount()
      if (eC>0) {
        for (i <- 0 to eC-1) {
          g.removeEdge(i)
        }
      }
      
    }
  
    def xmlModelParse(textArea2:TextArea) {
        var dbFactory = DocumentBuilderFactory.newInstance()
        var dBuilder = dbFactory.newDocumentBuilder()
        var is = new InputSource(new StringReader(textArea2.text)) 
        var doc = dBuilder.parse(is)
        initializeCoherenceGraph()
        doc.getDocumentElement().normalize()
           
        var goalList = doc.getElementsByTagName("goal")
        var myStr : String = null
        for (a <- 0 to goalList.getLength()-1) {
          var nNode = goalList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        }
        var evidenceList = doc.getElementsByTagName("evidence")
       
        for (a <- 0 to evidenceList.getLength()-1) {
          var nNode = evidenceList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        } 
        
        var beliefList = doc.getElementsByTagName("belief")
       
        for (a <- 0 to beliefList.getLength()-1) {
          var nNode = beliefList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        }   
        
         var actionList = doc.getElementsByTagName("action")
       
        for (a <- 0 to actionList.getLength()-1) {
          var nNode = actionList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          try {
            var activationLevel = eElement.getElementsByTagName("activation").item(0).getTextContent()
            activations+=(myStr -> activationLevel.toDouble)
          
          }
          catch  {
            case e: Exception => activations+=(myStr -> ACTIVATION)
          }
        } 
         
         var sourceStr : String = null
         var targetStr : String = null
         var weight : Double = 0.0
         var expConstraints = doc.getElementsByTagName("explain")
         for (a <- 0 to expConstraints.getLength()-1) {
           var sNode = expConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)   
         }
         
         var deduceConstraints = doc.getElementsByTagName("deduce")
         for (a <- 0 to deduceConstraints.getLength()-1) {
           var sNode = deduceConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var facilitateConstraints = doc.getElementsByTagName("facilitate")
         for (a <- 0 to facilitateConstraints.getLength()-1) {
           var sNode = facilitateConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var triggerConstraints = doc.getElementsByTagName("trigger")
         for (a <- 0 to triggerConstraints.getLength()-1) {
           var sNode = triggerConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var inhibitConstraints = doc.getElementsByTagName("inhibit")
         for (a <- 0 to inhibitConstraints.getLength()-1) {
           var sNode = inhibitConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var incompatibleConstraints = doc.getElementsByTagName("incompatible")
         for (a <- 0 to incompatibleConstraints.getLength()-1) {
           var sNode = incompatibleConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var contradictConstraints = doc.getElementsByTagName("contradict")
         for (a <- 0 to contradictConstraints.getLength()-1) {
           var sNode = contradictConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var similarConstraints = doc.getElementsByTagName("similar")
         for (a <- 0 to similarConstraints.getLength()-1) {
           var sNode = similarConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var competeConstraints = doc.getElementsByTagName("compete")
         for (a <- 0 to competeConstraints.getLength()-1) {
           var sNode = competeConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = new String(eElement.getElementsByTagName("source").item(0).getTextContent())
           targetStr = new String(eElement.getElementsByTagName("target").item(0).getTextContent())
           weight = 0-eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
    }

}







