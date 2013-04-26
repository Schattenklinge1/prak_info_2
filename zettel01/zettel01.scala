/*
  Kurs: Praktische Informatik 2
  Gruppe: Simon Schmitt, Benjamin Heidelmeier, Martin Lellep
  Tutor: Florian Dobener
  Zettel: 01
*/

// =======================================================================================
// Aufgabe 1
// =======================================================================================

  import scala.io._
  import java.io._

// a

  def taskA:Unit={
    val file = Source.fromFile("mobydick.txt").getLines()
    val newFileUnbuffered = new FileWriter("MobyDickArray.txt")
    val newFileBuffered = new BufferedWriter(newFileUnbuffered)
    for(l <- file){
      l.split(Array(' ','"',';','.',',','\'','-',':','?','!')).filter((a:String) => a.length>=9).foreach((a:String) => newFileBuffered.write(a+"\n"))
    // Bemerkung: es ginge natürlich auch mit einer for-Schleife!
    }
    newFileBuffered.flush
    newFileBuffered.close
  }


// b

// man könnte hier die Array Funktion "union" nutzen, aber genau kann man erst eine Liste erstellen, die man anschließend konvertiert, weil diese nativ wachsen kann! Das Problem ist nämlich die feststehende Größe des Arrays.

  def taskB:Array[String]={
    val file = Source.fromFile("MobyDickArray.txt").getLines
    var result:List[String] = Nil
    for(l <- file){
      result = l::result
    }
    result.toArray
  }
// wie ließ sich das rekursiv und ohne mutable vars machen?

  var mobyWords:Array[String] = taskB


// c

// erstes
  val cOne = mobyWords.filter(a => a(0)=='a' && a.reverse(0)=='e')
// zweites
  val cTwo = mobyWords.forall(a => a.contains('a') || a.contains('e'))
// drittes
  val cThree = mobyWords.count(a => a.length==16)
// viertes
  val cFour = mobyWords.exists(a => mobyWords.count(e => a==e)>=10)

  // Rest siehe Anhang! (pdf)

// =======================================================================================
// Aufgabe 2
// =======================================================================================

// Aufgabe 2

  def exists[T](a:Array[T],p:T=>Boolean):Boolean={
    for(elem <- a if p(elem)){
      return true
    }
    false
    // man könnte auch die Mächtigkeit der Menge überprüfen, die man
    // erhält, wenn man a nach p filtert! Würde auf filter aufbauen.
  }

  def forall[T](a:Array[T],p:T=>Boolean):Boolean={
    for(elem <- a if !(p(elem))){
      return false
    }
    true
  }


// =======================================================================================
// Aufgabe 3
// =======================================================================================

// Aufgabe 3

  def map[T,T2:Manifest](a:Array[T],p:T=>T2):Array[T2]={
    var result:Array[T2]=new Array(a.size)
    for(i <- 0 to a.size-1){
      result(i) = p(a(i))
    }
    result
  }

  map[String,String](mobyWords,(_.toUpperCase))
  map[String,Int](mobyWords,(_.size))
  map[Int,Boolean](Array[Int](2,3,4,5,6,8),(_%2==0))

// =======================================================================================
// Aufgabe 4
// =======================================================================================

  case class Land(Name:String, Capital:String, Abbrev:String, UNCode:Int)

// a

  def makeArray:Array[Land]={
    val file = Source.fromFile("countries.txt").getLines
    var resultList:List[Land] = List()
    file.next // den Iterator um eins inkrementieren, damit die Titelzeile nicht eingelesen wird!
    for(line <- file){
      val pureData:Array[String] = line.split(';')
      val landData:Land = Land(pureData(0),pureData(1),pureData(2),pureData(3).toInt)
      resultList = landData::resultList
    }
    resultList.reverse.toArray
  }

  def binSearch(laender:Array[Land], landsname:String):Option[Land]={
    def recHelp(lo:Int,hi:Int):Option[Land]={
      if (lo > hi) return None // hier is return wichtig! damit aussteigen ...
      val mid = lo + (hi-lo)/2
      if(landsname < laender(mid).Name) recHelp(lo,mid-1)
      else if(landsname == laender(mid).Name) Some(laender(mid))
      else recHelp(mid+1,hi)
    }
    recHelp(0,laender.length-1)
  }


// b

// für diese Fkt. muss aber auf B eine Ordnung definiert sein, richtig?!
  val countryArraySorted = makeArray.sortBy[Int]((e:Land) => e.UNCode)
// oder:
  val countryArraySorted2 = makeArray.sortBy[Int](_.UNCode) //!

  def binSearchUN(laender:Array[Land],unCode:Int):Option[Land]={
    def recHelp(lo:Int,hi:Int):Option[Land]={
      if(lo > hi) return None
      val mid = lo + (hi-lo)/2
      if(unCode < laender(mid).UNCode) recHelp(lo,mid-1)
      else if(unCode == laender(mid).UNCode) Some(laender(mid))
      else recHelp(mid+1,hi)
    }
    recHelp(0,laender.length-1)
  }