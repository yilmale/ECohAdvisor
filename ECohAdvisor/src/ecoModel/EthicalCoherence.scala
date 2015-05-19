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
   
    var translateButton = new Button {
      text = "Translate Model"
    }
    
    var generateButton = new Button {
      text = "Generate Network"
    }
    
    var computeButton = new Button {
      text = "Compute Active Nodes"
    }
     
    var simulateButton = new Button {
      text = "Simulate Activation Process"
    }
    
    
    var controlPanel = new FlowPanel() {
      contents+=translateButton
      contents+=generateButton
      contents+=computeButton
      contents+=simulateButton
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
                    // writer.close()
                     modelFileName ="ECoModel.xml" 
                     Source.fromFile(modelFileName).foreach { x => textArea2.text+=x }
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
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
    
    listenTo(translateButton)
    listenTo(computeButton)
    
    reactions += {
      case ButtonClicked(component) if component == translateButton =>
           
           
           
      case ButtonClicked(component) if component == computeButton =>
        var v3 = new String("Test"+g.getVertexCount()+1)
           g.addVertex(v3)
           g.addEdge(g.getEdgeCount(),v2,v3)
           
           
           var relaxer : Relaxer = vv.getModel().getRelaxer();
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
           layout.initialize();
           relaxer.resume();
    }
    
}
    

}







