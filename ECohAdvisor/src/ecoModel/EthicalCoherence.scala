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
  var g: Graph[String,Number] = null;
  var og: ObservableGraph[String,Number] =null;
  var ig = Graphs.synchronizedUndirectedGraph[String,Number](new UndirectedSparseMultigraph[String,Number]())
  og = new ObservableGraph[String,Number](ig)   
  g = og; 
  var vv : VisualizationViewer[String,Number] = null
  var layout : AbstractLayout[String,Number]  = null
  val ACTIVATION = 0.1
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
    
    var exportButton = new Button {
      text = "Export Model"
    }
    
    
    var controlPanel = new FlowPanel() { 
      contents+=compileButton
      contents+=displayButton
      contents+=computeButton
      contents+=simulateButton
      contents+=exportButton
    }
    
    var text = ""
    var textArea1 = new TextArea(text, 10, 80);
    textArea1.preferredSize_=(new Dimension(100,100))
    textArea1.lineWrap_=(true)
    var textArea2 = new TextArea(text, 100, 25);
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
    
    
    
    layout = new FRLayout2[String,Number](g);
    vv = new VisualizationViewer[String,Number](layout, new Dimension(600,600));
    
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
           val sf = new Frame {secondFrame => 
             title = "Load File"
             preferredSize = new Dimension(300,300)
             visible = true
             val textfield:TextField = new TextField {text=""; columns =15}
             val label:Label = new Label("")
             contents = new FlowPanel {
                contents += new Label(" Enter Model Filename")
                contents += textfield
                contents += new Button(Action("Select") {
                     label.text_=("Selected filename: "+textfield.text) 
                     //val writer = new PrintWriter(new File("ecoDSLTest1.emdl" ))
                     //writer.write("Hello Scala")
                     //writer.close()
                     modelFileName =textfield.text
                     try {
                       Source.fromFile(modelFileName).foreach { x => textArea2.text+=x }
                     } catch {
                       case ex: FileNotFoundException  => {
                         println("Missing file exception")
                       }
                     }
                     secondFrame.dispose()})
                contents += label
                
                //contents += new Button(Action("Select Model File")     {quit()})
             }
             
             listenTo(textfield.keys)
             
             reactions += {
                case KeyPressed(_, Key.Enter, _, _) => 
                     label.text_=("Selected filename: "+textfield.text)
                     modelFileName = new String(textfield.text)
              }
             
           }
           
        });
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
    
    listenTo(exportButton)
    listenTo(computeButton)
    listenTo(compileButton)
    listenTo(displayButton)
    
    reactions += {
      case ButtonClicked(component) if component == exportButton =>
             
      case ButtonClicked(component) if component == compileButton => 
        xmlModelParse(textArea2)
        graphIterator()
             
      case ButtonClicked(component) if component == displayButton =>
          var relaxer = vv.getModel().getRelaxer();
          vv.getModel().getRelaxer().setSleepTime(500);
          vv.setGraphMouse(new DefaultModalGraphMouse[Number,Number]());
          vv.getRenderer().getVertexLabelRenderer().setPosition(Renderer.VertexLabel.Position.CNTR);
          vv.getRenderContext().setVertexLabelTransformer(new ToStringLabeller[String]());
          vv.setForeground(Color.white);
           
       
        var vertexPaint = new Transformer[String,Paint]() {
                  def transform (i:String) : Paint = {
                    new Color(new java.lang.Float(0),new java.lang.Float(0),new java.lang.Float(1))         
                  }
              };
   
         var vertexSize =  new Transformer[String,Shape](){
                  def  transform(i: String) : Shape = {
                      var circle = new Ellipse2D.Double(-15, -15, 50, 50);
                      // in this case, the vertex is twice as large
                       return AffineTransform.getScaleInstance(1,1).createTransformedShape(circle);
                  }
              };
              
         var colorTransformer = new Transformer[Number,Paint]() {
                 def transform(i:Number):Paint = {                    
                    return Color.RED
                 }
         }     
              
         vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);
         vv.getRenderContext().setVertexShapeTransformer(vertexSize);
         vv.getRenderContext().setArrowFillPaintTransformer(colorTransformer)
         vv.getRenderContext().setArrowDrawPaintTransformer(colorTransformer)
         vv.getRenderContext().setEdgeDrawPaintTransformer(colorTransformer)
         
         layout.initialize();
         relaxer.resume();
         
         
       case ButtonClicked(component) if component == computeButton =>
     
           
           
        var relaxer : Relaxer = vv.getModel().getRelaxer();
        var vertexPaint = new Transformer[String,Paint]() {
                  def transform (i:String) : Paint = {
                    new Color(new java.lang.Float(1),new java.lang.Float(0),new java.lang.Float(0))         
                  }
              };
   
         var vertexSize =  new Transformer[String,Shape](){
                  def  transform(i: String) : Shape = {
                      var circle = new Ellipse2D.Double(-15, -15, 50, 50);
                      // in this case, the vertex is twice as large
                       return AffineTransform.getScaleInstance(1,1).createTransformedShape(circle);
                  }
              };
         vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);
         vv.getRenderContext().setVertexShapeTransformer(vertexSize);
         layout.initialize();
         relaxer.resume();
       
       
       case ButtonClicked(component) if component == simulateButton =>
       
    }
    
}
  
    def graphIterator() {
      
    }
  
    def xmlModelParse(textArea2:TextArea) {
        var dbFactory = DocumentBuilderFactory.newInstance()
        var dBuilder = dbFactory.newDocumentBuilder()
        var is = new InputSource(new StringReader(textArea2.text)) 
        var doc = dBuilder.parse(is)
      
        doc.getDocumentElement().normalize()
       
        System.out.println("Root element :" + doc.getDocumentElement().getNodeName())
         
        var goalList = doc.getElementsByTagName("goal")
        var myStr : String = null
        for (a <- 0 to goalList.getLength()-1) {
          var nNode = goalList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          activations+=(myStr -> ACTIVATION)
          System.out.println(myStr)
        }
        
        var evidenceList = doc.getElementsByTagName("evidence")
       
        for (a <- 0 to evidenceList.getLength()-1) {
          var nNode = evidenceList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          activations+=(myStr -> ACTIVATION)
          System.out.println(myStr)
        } 
        
        var beliefList = doc.getElementsByTagName("belief")
       
        for (a <- 0 to beliefList.getLength()-1) {
          var nNode = beliefList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          activations+=(myStr -> ACTIVATION)
          System.out.println(myStr)
        }   
        
         var actionList = doc.getElementsByTagName("action")
       
        for (a <- 0 to actionList.getLength()-1) {
          var nNode = actionList.item(a)
          var eElement = nNode.asInstanceOf[Element]
          myStr = eElement.getElementsByTagName("name").item(0).getTextContent()
          g.addVertex(myStr)
          activations+=(myStr -> ACTIVATION)
          System.out.println(myStr)
        } 
         
         var sourceStr : String = null
         var targetStr : String = null
         var weight : Double = 0.0
         var expConstraints = doc.getElementsByTagName("explain")
         for (a <- 0 to expConstraints.getLength()-1) {
           var sNode = expConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)
         }
         
         var deduceConstraints = doc.getElementsByTagName("deduce")
         for (a <- 0 to deduceConstraints.getLength()-1) {
           var sNode = deduceConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var facilitateConstraints = doc.getElementsByTagName("facilitate")
         for (a <- 0 to facilitateConstraints.getLength()-1) {
           var sNode = facilitateConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var triggerConstraints = doc.getElementsByTagName("trigger")
         for (a <- 0 to triggerConstraints.getLength()-1) {
           var sNode = triggerConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var inhibitConstraints = doc.getElementsByTagName("inhibit")
         for (a <- 0 to inhibitConstraints.getLength()-1) {
           var sNode = inhibitConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var incompatibleConstraints = doc.getElementsByTagName("incompatible")
         for (a <- 0 to incompatibleConstraints.getLength()-1) {
           var sNode = incompatibleConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var contradictConstraints = doc.getElementsByTagName("contradict")
         for (a <- 0 to contradictConstraints.getLength()-1) {
           var sNode = contradictConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)          
         }
         
         var similarConstraints = doc.getElementsByTagName("similar")
         for (a <- 0 to similarConstraints.getLength()-1) {
           var sNode = similarConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
         
         var competeConstraints = doc.getElementsByTagName("compete")
         for (a <- 0 to competeConstraints.getLength()-1) {
           var sNode = competeConstraints.item(a)
           var eElement = sNode.asInstanceOf[Element]
           sourceStr = eElement.getElementsByTagName("source").item(0).getTextContent()
           targetStr = eElement.getElementsByTagName("target").item(0).getTextContent()
           weight = eElement.getElementsByTagName("weight").item(0).getTextContent().toDouble
           var edgeCount = g.getEdgeCount()
           g.addEdge(g.getEdgeCount(),sourceStr,targetStr)
           edgeWeights+=(edgeCount -> weight)           
         }
    }

}







