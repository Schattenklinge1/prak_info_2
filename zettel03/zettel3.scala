/*
  Kurs: Praktische Informatik 2
  Gruppe: Simon Schmitt, Benjamin Heidelmeier, Martin Lellep
  Tutor: Florian Dobener
  Zettel: 03
*/

// =======================================================================================
// Aufgabe 1
// =======================================================================================

// Definition:

def mS[T <% Ordered[T]](a:Array[T]) {

  // Hilfsarray nur einmal erstellt!
  val aux = a.clone

  def recHelp(lo:Int,hi:Int) {
    if(lo < hi){
      val mid = lo + (hi-lo)/2
      recHelp(lo,mid)
      recHelp(mid+1,hi)
      merge(lo,mid,hi)
    }
  }

  def merge(lo:Int,mid:Int,hi:Int) {
    // die teile kopieren:
    for(i <- lo to mid){
      aux(i) = a(i)
    }
    for(i <- mid+1 to hi){
      // dreht den slice um! Sonst brächte man einen
      // Laufindex und zwei Indizes, die diesmal nicht
      // aufeinander zu laufen, sondern bis zum Ende
      // des Teilarray -> damit würde die Abbruchbedingung
      // komplizierter werden!
      aux(hi-i+mid+1) = a(i)
    }
    var (li,re) = (lo,hi)
    for(i <- lo to hi){
      if(aux(li) < aux(re)){ // wrm ist <= nicht dabei?!
        a(i)=aux(li)
        li+=1
      }else{
        a(i)=aux(re)
        re-=1
      }
    }
  }
  recHelp(0,a.length-1)
}

// Einlesen: (muss nix geschrieben werden, deshalb kein java.io._!)

import scala.io._

def readDaShit():Array[String]={

  // hier wird alles eingelesen und die leeren dinger fot geschmissen
  // ginge bestimmt besser durch richtige Trennzeichen!
  // z.B. mit RegEx und einfach einen Ausdruck, der keine Leerzeichen zulässt! (\b)
  val iterator = Source.fromFile("faust.txt").getLines()

  var result = List[String]()

  for(line <- iterator){
    line.split(Array(' ',',',':',';','?','!','.','"','(',')','-','\'')).filter(e => e!="").foreach(a => result = a::result)
    // '\n' wird nicht benötigt, da getLines!!

    // man müsste auf alle Sonderzeichen eingehen UND auf einzelne Buchstaben, Zahlen etc. !! -> nutze am besten RegEx! :-)

    // wie gesagt: ReeeegEx!! :-)

    // Frage: warum gibt es einen exception, wenn ich die file in iso-blabla
    // und nicht in utf8 codiert habe?!
    // der exception ist: "java.nio.charset.MalfomedInputException: Input length = 1"
    // :-O

  }

  result.reverse.toArray

}


// es wird nix aussortiert (a.k.a. Menge!!)

// =======================================================================================
// Aufgabe 2
// =======================================================================================

//Aufgabe 2 a)-------------------------------------

trait PartialOrdered[T] {
  def <(t:T):Boolean = {
    this.below(t) && !(this==t)
  }
  def <=(t:T):Boolean = {
    this.below(t)
  }
  def >(t:T):Boolean = {
    !this.below(t)
  }
  def >=(t:T):Boolean = {
    !this.below(t) || this==t
  }
  def ||(t:T):Boolean= {
    !this.below(t) && !t.asInstanceOf[PartialOrdered[T]].below(this.asInstanceOf[T])
  }

  def below(t:T):Boolean
}

//b) -------------------------------------------------
def isPartialSorted[T <% PartialOrdered[T]](arr: Array[T]):Boolean = {
  for (i <- 0 until arr.length-1) {
    for (j <- i+1 until arr.length) {
      if (arr(i).below(arr(j)) && !(arr(i) == arr(j) || i < j)) {
        return false
      }
      if (arr(j).below(arr(i)) && !(arr(j) == arr(i) || j < i)) {
        return false
      }
    }
  }
  true
}

def swap[T](a:Array[T],i:Int,j:Int){
  val temp = a(i)
  a(i) = a(j)
  a(j) = temp
}

def selectionSort[T <% PartialOrdered[T]](arr: Array[T]) {
  for(j <- 0 until arr.length) {
    var minIndex = j
    for(k <- j until arr.length) {
      if(arr(k) < arr(minIndex)) minIndex = k
    }
    swap(arr,j,minIndex)
  }
}

def insertionSort[T <% PartialOrdered[T]](arr: Array[T]) {
  for(j <- 1 until arr.length) {
    val nextVal = arr(j)
    var k = j-1
    while(k >= 0 && arr(k) > nextVal) {
      arr(k+1) = arr(k)
      k -= 1
    }
    arr(k+1) = nextVal
  }
}

//c) -----------------------------------------------------------
class Rechteck(val x1:Int, val y1:Int, val x2:Int, val y2:Int) extends PartialOrdered[Rechteck] {
  import scala.math._
  def below(t:Rechteck) = {
    min(this.x1,this.x2) >= min(t.x1,t.x2) && max(this.x1,this.x2) <= max(t.x1,t.x2) && min(this.y1,this.y2) >= min(t.y1,t.y2) && max(this.y1,this.y2) <= max(t.y1,t.y2)
  }
  def ==(t:Rechteck):Boolean = (this.x1 == t.x1) && (this.x2 == t.x2) && (this.y1 == t.y1) && (this.y2 == t.y2)
}

def rechteckToArray:Array[Rechteck] = {
  var ergebnis:Array[Rechteck] = new Array[Rechteck](0)
  val iterator = scala.io.Source.fromFile("rechtecke.txt").getLines()
  for (z <- iterator){
    val merke = z.split(',')
    val rechteck:Rechteck = new Rechteck(merke(0).toInt,merke(1).toInt,merke(2).toInt,merke(3).toInt)
    ergebnis = ergebnis.:+(rechteck)
  }
  ergebnis
}

var rechteck = rechteckToArray
println("ispartialsorted after selectionsort:" + isPartialSorted(rechteck))

rechteck = rechteckToArray
insertionSort(rechteck)
println("ispartialsorted after insertionsort:" + isPartialSorted(rechteck))

// =======================================================================================
// Aufgabe 3
// =======================================================================================

//Aufgabe 3 a)---------------------------------------------------------------------------------
def sort(a:Array[Int]) {
  var h:Int = 1
  while(h*3+1 < a.length) {
    h = 3*h+1
  }
  while(h>0){
    for(i <- h-1 until a.length) {
      val s = a(i)
      var j = i
      while (j>=h && a(j-h) > s) {
        a(j) = a(j-h)
        j-=h
      }
      a(j) = s
    }
    h = h/3
  }
}

//b)-------------------------------------------------------------
def zufallsArray(laenge:Int, bereich:Range):Array[Int] = {
  var arr:Array[Int] = new Array[Int](laenge)
  def getRndInt(range:Range):Int = {
    val rnd = new scala.util.Random
    rnd.nextInt(range.length)
  }
  for (i <- 0 until arr.length) {
    arr(i) = getRndInt(bereich)
  }
  arr
}

var ara = zufallsArray(10000,1 to 10000)
sort(ara)
ara
// c)--------------------------
def sortAllgemein[T <% Ordered[T]](a:Array[T]) {
  var h:Int = 1
  while(h*3+1 < a.length) {
    h = 3*h+1
  }
  while(h>0){
    for(i <- h-1 until a.length) {
      val s = a(i)
      var j = i
      while (j>=h && a(j-h) > s) {
        a(j) = a(j-h)
        j-=h
      }
      a(j) = s
    }
    h = h/3
  }
}

// d.)

/*
Die Funktion terminiert, weil die while-Schleife nur so lange läuft, wie h>0 ist, allerdings
wird h/3 am Ende eines jeden Durchlaufs!! Also irgendwann h=0 (s. Regeln des Modulo!).

Idee: Prinzipiell ist durch den Anfang eine Menge definiert mit A={x|x=3*n+1 für n aus nat Zahlen}.
Nun schneidet man A und [arr.length-1] und nimmt zunächst das Maximum dieses Schnittes. Man erstellt
prinzipiell eine Matrix, die nun h Spalten hat, wobei man die Matrix zeilenweise füllt. Durch die for-
Schleife in der while-Schleife werden die Spalten nun geordnet (also Elemete, die einen bestimmten
Abstand im ursprünglichen Array haben sind nun geordnet - nämlich genau den Abstand h).
Dabei ähnelt das Ordnen sehr dem InsectionSort ohne Swap: das zwischengespeicherte s entsprich der Variablen
"nextVal" aus VL. Diese wird am Ende wiederhergestellt. Es wird nur über Zuweisugen geordnet, also müsste
man noch sicherstellen, ob immernoch Permutation!
Dieses Ordnen der Zeilen und das folgende Verkleinern der Spaltenzahl kann man wie eine "Transformation" in
eine schmalere Matrix bewirkt: dabei werden die Elemente zeilenweise neu angeordnet.
Nach der for-Schleife wird das nächst kleinere Element aus A genommen und alles wiederholt, so lange bis
es eine e*1 Matrix ist, die nach dem letzten Durchlaufen der while-Schleife vollständig geordnet ist.
Man interpretiere e*1 Matrix als Array und ist fertig.
*/