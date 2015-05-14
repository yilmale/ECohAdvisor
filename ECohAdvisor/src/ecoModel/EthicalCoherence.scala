package ecoModel

import scala.swing._
import scala.swing.Panel
import scala.swing.BorderPanel
import scala.swing.BoxPanel
import BorderPanel.Position._
import ScrollPane._

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
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.graph.Graph
import edu.uci.ics.jung.graph.ObservableGraph
import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.visualization.VisualizationViewer
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import edu.uci.ics.jung.visualization.renderers.Renderer



object EthicalCoherence extends SimpleSwingApplication {
  var g: Graph[String,Number] = null;
  var og: ObservableGraph[String,Number] =null;
  var ig = Graphs.synchronizedDirectedGraph[String,Number](new DirectedSparseMultigraph[String,Number]())
  og = new ObservableGraph[String,Number](ig)   
  g = og; 
  var vv : VisualizationViewer[String,Number] = null
  var layout : AbstractLayout[String,Number]  = null
  def top =new MainFrame {
    title = "Ethical Coherence Advisor Tool"
   
    var loadButton = new Button {
      text = "Load"
    }
    
    var computeButton = new Button {
      text = "Compute Active Nodes"
    }
     
    var evaluateButton = new Button {
      text = "Evaluate"
    }
   
    var controlPanel = new FlowPanel() {
      contents+=loadButton
      contents+=computeButton
      contents+=evaluateButton
    }
    
    var text = "Test String"
    var textArea1 = new TextArea(text, 10, 80);
    textArea1.preferredSize_=(new Dimension(100,100))
    textArea1.lineWrap_=(true)
    var textArea2 = new TextArea(text, 35, 25);
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
    
    var v1 = new String("Test")
    var v2 = new String("Test2")
    g.addVertex(v1)    
    g.addVertex(v2)
    g.addEdge(g.getEdgeCount(),v1,v2)
    
    layout = new FRLayout2[String,Number](g);
    vv = new VisualizationViewer[String,Number](layout, new Dimension(600,600));
    
    
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
    vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);
    vv.getRenderContext().setVertexShapeTransformer(vertexSize);
    
    var vvWrap = Component.wrap(vv)
    
    var visPanel = new BoxPanel(Orientation.Vertical) {
       contents+= vvWrap
   
     }
     visPanel.size.setSize(new Dimension(500,500))
    
    
     size = new Dimension(1000,1000)
    
    contents = new BorderPanel() {
      layout(controlPanel)=North
      layout(modelPanel)=East
      layout(visPanel)=West
      layout(resultsPanel)=South
      
    }
    

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    
 
    
}
    

}







