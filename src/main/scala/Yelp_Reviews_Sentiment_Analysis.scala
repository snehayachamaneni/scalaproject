import java.io.{File, PrintWriter}
import java.util.Properties
import Sentiment.Sentiment
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.neural.rnn.RNNCoreAnnotations
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import scala.collection.convert.wrapAll._
import scala.io.{BufferedSource, Source}
import scala.util.parsing.json.JSON

object Main extends App {
  //Defined input file  location in run configuration
  val review_json = args ( 0 )
  //Defined output file  location in run configuration
  val output = args ( 1 )
  var counterVariable: Int = 0
  //Create print write instance for output
  lazy val writer = new PrintWriter ( new File ( output ) )
  //Creating an instance of buffered source to buffer input file
  val source: BufferedSource = Source.fromFile ( new File ( review_json ) )
  //Created iterator instance for the buffered source and got all the lines of the buffered source
  val iterator: Iterator[String] = source.getLines ()
  //Checking if iterator has next line to view
  while (iterator.hasNext) {
    //Parsed each line and take it as Map
    val option = JSON.parseFull ( iterator.next () )
    //Extract each line as Map by matching with below cases
    option match {
      case None => println ( "nothing matched" );
      case Some ( m: Map[String, Any] ) => {
        // Reading Text filed which consists of review from the customer
        val input = m ( "text" ).toString
        // Extracting only first sentence of the review
        //val input = i.split ( "\\." ).head
        // Passing review to the Sentiment Analyzer which provides sentiment of the review as Positive/Negative/Neutral
        val sentiment = SentimentAnalyzer.mainSentiment ( input.toString )
        // Converting enumerated sentiment value to string
        val Predict_Sentiment = sentiment.toString
        // Reading the Stars filed which consists of rating given by the customer
        val star = m ( "stars" )
        // Providing labeling for calculating accuracy
        var appendedOutputToStar = Map [String, Any]()
        if (star == 4 || star == 5) {
          appendedOutputToStar = m + ("Actual_Sentiment" -> "POSITIVE")
        }
        else if (star == 3) {
          appendedOutputToStar = m + ("Actual_Sentiment" -> "NEUTRAL")
        }
        else if (star == 1 || star == 2) {
          appendedOutputToStar = m + ("Actual_Sentiment" -> "NEGATIVE")
        }
        //Appending the Actual_Sentiment and Predict_Sentiment fields along with the value to the map
        val appendedOutput = appendedOutputToStar + ("Predict_Sentiment" -> Predict_Sentiment)
        //Creating string buffer instance
        val sb: StringBuffer = new StringBuffer ()
        sb.append ( "{" )
        //Get the last key in the map key set
        val lastKeyEntry = appendedOutput.keys.last
        var predict: Any = ""
        var actual: Any = ""
        //Iterating through each key set and appending custom string to (key,value) pairs
        for ((key, value) <- appendedOutput) {
          if (key == "Predict_Sentiment") {
            predict = appendedOutput ( key )
          } else if (key == "Actual_Sentiment") {
            actual = appendedOutput ( key )
          }
          sb.append ( "\"" + key.toString + "\"" + ":" + "\"" + value.toString + "\"" )
          //Check if the current key is the last key of the map
          if (lastKeyEntry != key)
            sb.append ( "," )
        }
        sb.append ( "}" )
        //Getting count of all rows where Predict_Sentiment is not equal to Actual_Sentiment
        if (predict != actual) {
          counterVariable = counterVariable + 1
        }
        //writing the result into the output file along with next line as new line
        writer.write ( sb.toString () )
        writer.write ( "\n" )
      }
    }
  }
  println ( "CounterVariable Value:" + counterVariable )
  //Calculating Error_Rate
  val error_rate = counterVariable.floatValue () / 5000
  //Calculating Accuracy
  val accuracy = (1 - error_rate) * 100
  println ( "Total number of reviews:5000" )
  println ( "Number of reviews for which Actual_Sentimnet is not equals to Predict_Sentiment:" + counterVariable )
  println ( "Error_Rate:" + error_rate )
  println ( "Accuracy:" + accuracy )
  writer.close ()
}

object SentimentAnalyzer {
  val props = new Properties ()
  //The properties defines which all annotators will be used by the pipeline
  //For this application tokenize, ssplit, parse, sentiment annotators will be used
  //Check out this link to know more details of each of this annotator https://stanfordnlp.github.io/CoreNLP/annotators.html
  props.setProperty ( "annotators", "tokenize, ssplit, parse, sentiment" )
  val pipeline: StanfordCoreNLP = new StanfordCoreNLP ( props )


  //The mainSentiment method checks if string is valid and if valid it calls the extractSentiment with the input text
  def mainSentiment(input: String): Sentiment = Option ( input ) match {
    case Some ( text ) if !text.isEmpty => extractSentiment ( text )
    case _ => throw new IllegalArgumentException ( "input can't be null or empty" )
  }

  //The extractSentiment method calls maxBy operation on a list of two value tuple returned by extractSentiments
  //The tuple contains a sentence annotated text and sentiment score
  //The maxBy operation compares values based on sentence length i.e. a sentence with largest length will be used as main sentiment of text
  private def extractSentiment(text: String): Sentiment = {
    val (_, sentiment) = extractSentiments ( text )
      .maxBy { case (sentence, _) => sentence.length }
    sentiment
  }

  //The extractSentiments method uses StanfordCoreNLP to process the text
  //The process method processes the text by running the pipeline on the input text
  def extractSentiments(text: String): List[(String, Sentiment.Sentiment)] = {
    val annotation: Annotation = pipeline.process ( text )
    val sentences = annotation.get ( classOf [CoreAnnotations.SentencesAnnotation] )
    sentences
      .map ( sentence => (sentence, sentence.get ( classOf [SentimentCoreAnnotations.SentimentAnnotatedTree] )) )
      .map { case (sentence, tree) => (sentence.toString, Sentiment.toSentiment ( RNNCoreAnnotations.getPredictedClass ( tree ) )) }
      .toList
  }
}

//The toSentiment method is used by SentimentAnalyzer(discussed above) to convert integer sentiment value returned by stanford-corenlp API to enum
//The stanford-corenlp gives sentiment score of 0|1 when text has negative emotion, 2 when text is neutral, 3|4 when text has positive emotion
object Sentiment extends Enumeration {
  type Sentiment = Value
  val POSITIVE, NEGATIVE, NEUTRAL = Value

  def toSentiment(sentiment: Int): Sentiment = sentiment match {
    case x if x == 0 || x == 1 => Sentiment.NEGATIVE
    case 2 => Sentiment.NEUTRAL
    case x if x == 3 || x == 4 => Sentiment.POSITIVE
  }
}