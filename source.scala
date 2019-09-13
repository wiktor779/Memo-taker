package hellobutton

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.geometry.Orientation
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.BorderPane
import scala.collection.SortedMap
import scala.xml._
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.Calendar
import java.io.PrintWriter

class DateTitle(date: Calendar, title: String) {
  override def toString: String = {
    title
  }

  def getDate: Calendar = {
    date
  }
}


object DrawingMain extends JFXApp {

  var xmlFile = XML.load("notes.xml")
  var myNotes: SortedMap[Calendar, (String, String)] = SortedMap.empty
  val sdf = new SimpleDateFormat("EEE MMM dd HH:mm:ss z yyyy", Locale.ENGLISH)

  val children = (xmlFile \ "note")
  children.foreach(child => {
    val title: String = (child \ "title").text
    val contents: String = (child \ "content").text
    val dateString = (child \ "date").text
    val date: Calendar = Calendar.getInstance
    date.setTime(sdf.parse(dateString))
    myNotes += date -> (title, contents)
  })

  override def stopApp(): Unit = {
    xmlFile = emptyXml()
    for (element <- myNotes) {
      val date = element._1
      val dateString = sdf.format(date.getTime)
      val title = element._2._1
      val contents = element._2._2
      xmlFile = addNewEntry(xmlFile, title, dateString, contents)
    }
    scala.xml.XML.save("notes.xml", xmlFile)
  }

  def emptyXml(): Elem = {
    <notes>
    </notes>
  }

  var characters = ObservableBuffer[DateTitle]()
  for (element <- myNotes) {
    characters += new DateTitle(element._1, element._2._1)
  }

  stage = new JFXApp.PrimaryStage {
    title = "Memo taker"


    scene = new Scene(800, 600) {
      val title = new TextArea()
      val contents = new TextArea()
      val date = new TextField()
      date.editable = false
      val wholeTime = new Button("All")
      val lastDay = new Button("Last Day")
      val lastWeek = new Button("Last Week")
      val saveButton = new Button("Save")
      val newButton = new Button("New")
      val deleteButton = new Button("Delete")

      val leftPaneButtons = new SplitPane()
      leftPaneButtons.items ++= List(wholeTime, lastDay, lastWeek)
      val rightPaneButtons = new SplitPane()
      rightPaneButtons.items ++= List(deleteButton, saveButton, newButton)

      val rightSplitPanel = new SplitPane()
      rightSplitPanel.orientation = Orientation.Vertical
      rightSplitPanel.items ++= List(title, contents)
      rightSplitPanel.dividerPositions = 0.1

      val listView = new ListView[DateTitle] {
        items = characters
        selectionModel().selectedItem.onChange {
          (_, _, newValue) => {
            if (newValue != null) {
              val (titleValue, contentsValue): (String, String) = myNotes.get(newValue.getDate).get
              title.text = titleValue
              contents.text = contentsValue
              date.text = sdf.format(newValue.getDate.getTime)
            }
          }
        }
      }

      val leftBorderPane = new BorderPane()
      leftBorderPane.top = leftPaneButtons
      leftBorderPane.center = listView

      val rightBorderPane = new BorderPane()
      rightBorderPane.top = rightPaneButtons
      rightBorderPane.center = rightSplitPanel
      rightBorderPane.bottom = date

      val rootPane = new BorderPane()
      rootPane.right = rightBorderPane
      rootPane.left = leftBorderPane
      root = rootPane

      saveButton.onAction = (event: ActionEvent) => {
        val timeStamp: String = sdf.format(Calendar.getInstance.getTime)
        val now: Calendar = Calendar.getInstance
        now.setTime(sdf.parse(timeStamp))
        val previousDate: Calendar = Calendar.getInstance
        if (date.text.value != "") {
          previousDate.setTime(sdf.parse(date.text.value))
          myNotes = myNotes - previousDate
        }
        myNotes = myNotes + (now -> (title.text.value, contents.text.value))
        characters.clear()
        for (element <- myNotes) {
          characters += new DateTitle(element._1, element._2._1)
        }
        date.text = timeStamp
      }

      newButton.onAction = (event: ActionEvent) => {
        title.text = ""
        contents.text = ""
        date.text = ""
      }

      deleteButton.onAction = (event: ActionEvent) => {
        val dateToDelete: Calendar = Calendar.getInstance
        if (date.text.value != "") {
          dateToDelete.setTime(sdf.parse(date.text.value))
          myNotes = myNotes - dateToDelete
        }
        characters.clear()
        for (element <- myNotes) {
          characters += new DateTitle(element._1, element._2._1)
        }
        title.text = ""
        contents.text = ""
        date.text = ""
      }

      lastDay.onAction = (event: ActionEvent) => {
        val yesterday : Calendar = Calendar.getInstance(); // this would default to now
        yesterday.add(Calendar.DAY_OF_MONTH, -1)
        characters.clear()
        for (element <- myNotes) {
          val date = element._1
          if( date.compareTo(yesterday) == 1){
            characters += new DateTitle(date, element._2._1)
          }
        }
      }
      lastWeek.onAction = (event: ActionEvent) => {
        val weekAgo : Calendar = Calendar.getInstance();
        weekAgo.add(Calendar.DAY_OF_MONTH, -7)
        characters.clear()
        for (element <- myNotes) {
          val date = element._1
          if( date.compareTo(weekAgo) == 1){
            characters += new DateTitle(date, element._2._1)
          }
        }
      }

      wholeTime.onAction = (event: ActionEvent) => {
        characters.clear()
        for (element <- myNotes) {
          characters += new DateTitle(element._1, element._2._1)
        }
      }
    }
  }

  def toBeAddedEntry(title: String, date: String, content: String) ={
    <note>
      <title>{ title }</title>
      <content>{ content }</content>
      <date>{ date }</date>
    </note>

  }

  def addNewEntry(originalXML: Elem, title: String, date: String, content: String) = {
    originalXML match {
      case <notes>{ innerProps @ _* }</notes> => {
        <notes> {
          innerProps ++ toBeAddedEntry(title: String, date: String, content: String)
          }</notes>
      }
      case other => other
    }
  }
}
