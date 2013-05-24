/*
  Kurs: Praktische Informatik 2
  Gruppe: Simon Schmitt, Benjamin Heidelmeier, Martin Lellep
  Tutor: Florian Dobener
  Zettel: 01
*/

// =======================================================================================
// Aufgabe 1
// =======================================================================================

/*

Man nutze Dominanzkriterium: zz: es existiert lim_{n\to\infty}\frac{n*log(n)}{n^{1+\epsilon}}

-> Forme um: \frac{n*log(n)}{n^{1+\epsilon}}=\frac{log(n)}{n^{\epsilon}}.
-> Nutze L'Hospital: d/dn(log(n))=1/n, d/dn(e^epsilon)=\epsilon\cdot n^{\epsilon-1}
    -> daraus folgt: lim_{n\to\infty}\frac{1/n}{\epsilon\cdot n^{\epsilon-1}}=lim...\frac{1}{\epsilon\cdot n^{\epsilon}}=0
       für alle \epsilon>0!

Bemerkung: Die obige Reihenfolge ist keineswegs mathematisch (d.h. nicht rechnerisch) korrekt.

Frage: Wie zeigt man es gescheit mit Definition? Ich ende damit, dass folgendes zz ist:
		log(n) <= 2*e^{\epsilon} \forall n>=1 mit k=2,n_0=1
		-> das dürfte auch stimmen, aber wie zu zeigen?

*/

// =======================================================================================
// Aufgabe 2
// =======================================================================================

/*

Man erhält: T(n)=1+2*T(n). Durch Substitution und vorallem frühzeitiges Ausklammern erhält man:
		T(n)=\sum_{i=0}^{n-1}(2^i)=2^n-1
		
Also folgt: T(n) = O(2^n) = O(a^n) für a>=2 (für a>1 dürfte es nicht gelten!!)

*/

// =======================================================================================
// Aufgabe 3
// =======================================================================================

// a.)

/*
Das mühselige Ausrechnen bzw. analoge Verwenden der Musterlösung ergibt die Bewungen:
moves(n) = 1/6 n^3 + 3/2 n^2 + 4/3 n +1
Daraus folgt: moves(n) = O(n^3).
*/

// b.)

/*

b.1: Man teilt das Array in zwei Teile. Nun wendet man die selbe Funktion auf die beiden Teil-
arrays an. Dabei ist zu beachten, dass man auch eine mittlere Max. Summe erstellt und nachher
alle drei Summen vergleich, davon das Max. nehmen! Bei der mittleren ist zu beachten, dass
man die Schleifen _nicht_ schachtelt, da dies vglweise so ineffizient sein würde, wie in Aufgabe
a.)! Es reicht die Schleifen nacheinander abzuarbeiten, da außerhalb davon ja die rekursive
Funktion die Arbeit verrichtet (sie kann eben nur nicht genau Teilarray-übergreifend agieren).
Durch die Addition vermeidet man die Multiplikation durch verschachtelte Schleifen


b.2: In T(n), aus dem sich O bestimmen lässt, befindet sich a=const, 2*T(n/2) und die Schleifenauf-
rufe, also O(n^2) sein dürften (da zwei verschachtelte Schleifen (sum und jew Schleife)) iteriert werden.

*/


// c.)

// Erste Implementierung
def maxSum1(a:Array[Int])={
  def recHelp(lo:Int,hi:Int):Int={
    if(lo<hi){
      // mid erstellen
      val mid = lo + (hi-lo)/2
      // lower und higher erhalten
      val lowerPart = recHelp(lo,mid)
      val higherPart = recHelp(mid+1,hi)
      // mittlere Ã¼berprÃ¼fen
      var max = 0 // anders als beim implementierten, braucht man hier nicht 
      def sum(i:Int,j:Int)={
        var max = 0
        for(o <- i to j){
          max += a(o)
        }
        max
      }
      for(i <- lo to mid reverse){
        for(j <- mid+1 to hi){
          val maxTmp = sum(i,j)
          if(maxTmp > max){
            max = maxTmp
          }
        }
      }
      scala.math.max(lowerPart,scala.math.max(higherPart,max))
    }else{
      0 // hier problematisch!! Wenn nämlich max eig < 0!!
    }
  }
  recHelp(0,a.length-1)
}

// Zweite Implementierung
def maxSum2(a:Array[Int])={
  def recHelp(lo:Int,hi:Int):Int={
    if(lo<hi){
      // mid erstellen
      val mid = lo + (hi-lo)/2
      // lower und higher erhalten
      val lowerPart = recHelp(lo,mid)
      val higherPart = recHelp(mid+1,hi)
      // mittlere Ã¼berprÃ¼fen
      var maxLo = 0
      var maxHi = 0
      def sum(i:Int,j:Int)={
        var max = 0
        for(o <- i to j){
          max += a(o)
        }
        max
      }
      for(i <- lo to mid){
	val maxTmp = sum(i,mid)
	if(maxTmp > maxLo){
	  maxLo = maxTmp
	}
      }
      for(j <- mid to hi){	// warum bei beiden mit mid?
        val maxTmp = sum(mid,j)
        if(maxTmp > maxHi){
          maxHi = maxTmp
        }
      }
      // Ausgabe des Maximums
      scala.math.max(lowerPart,scala.math.max(higherPart,maxLo+maxHi))
    }else{
      0 // hier problematisch!! Wenn nämlich max eig < 0!!
      // Bem.: man könnte so anpassen, dass -1 bei neg. Maximum
      // ausgibt: bei Ausgabe des Max. noch if-Abfrage!
    }
  }
  recHelp(0,a.length-1)
}

// dieser Alg. funktioniert mit dem Array aus Aufg _nicht_ !! => iwie fehlerhaft


// d.)

def zufallsArray(laenge:Int,low:Int,high:Int):Array[Int] = {
  val a:Array[Int] = new Array[Int](laenge)
  for(x <- 0 until laenge) {a(x) = scala.math.random.*(high-low).round.toInt.+(low)}
  return a
}
val randomArray = zufallsArray(100000,-5,5)

// =======================================================================================
// Aufgabe 4
// =======================================================================================

// Typ implementieren:
case class Person(val name:String,val vorname:String, val geburtsjahr:Int)

// Hilfsmethoden:
def swap[T](a:Array[T],i:Int,j:Int):Unit={
  require(i>=0 && j<= a.length-1,"index exception")
  val tmp:T = a(i)
  a(i) = a(j)
  a(j) = tmp
}

def vornameQS(a:Array[Person]):Unit={
  def partition(a:Array[Person],lo:Int,hi:Int):Int={
    require(lo<hi)
    val pivot = a(hi).vorname
    var k = lo
    for(i <- lo to hi-1){
      if(a(i).vorname<pivot){
        swap(a,i,k)
        k+=1
      }
    }
    swap(a,k,hi)
    k
  }
  def qSort(a:Array[Person],lo:Int,hi:Int):Unit={
    if(lo<hi){
      val q = partition(a,lo,hi)
      qSort(a,lo,q-1)
      qSort(a,q+1,hi)
    }
  }
    qSort(a,0,a.length-1)
}

def nameMSVL(a:Array[Person])={
  val aux = a.clone
  def mergeSort(lo:Int,hi:Int):Unit={
    if(lo<hi){
      val mid = lo + (hi-lo)/2
      mergeSort(lo,mid)
      mergeSort(mid+1,hi)
      merge(lo,mid,hi)
    }
  }
  def merge(lo:Int,mid:Int,hi:Int)={
    for(i <- lo to mid) aux(i) = a(i)
    for(i <- mid+1 to hi) aux(hi-i+mid+1) = a(i)
    var (li,re) = (lo,hi)
    for(k <- lo to hi)
      if(aux(li).name<=aux(re).name) {a(k)=aux(li);li+=1}
    else {a(k)=aux(re);re-=1}
  }
  mergeSort(0,a.length-1)
}

def jahrDistri(a:Array[Person])={
  def key(number:Int,digit:Int):Int={
    if(digit<=0) number%10
    else key(number/10,digit-1)
  }
  def einordnen(keyPos:Int)={
    val temp = new Array[Person](a.length);
    val fachGroesse = new Array[Int](10);
    for(i <- 0 to a.length-1){
      temp(i)=a(i) // umkopieren
      fachGroesse(key(a(i).geburtsjahr,keyPos)) += 1
    }
    val fachZeiger = new Array[Int](10)
    for(i <- 1 to 9){
      fachZeiger(i) = fachZeiger(i-1)+fachGroesse(i-1)
    }
    for(i <- 0 to a.length-1){
      val v = key(temp(i).geburtsjahr,keyPos)
      a(fachZeiger(v)) = temp(i)
      fachZeiger(v) += 1
    }
  }
  for(i <- 0 to 4-1) einordnen(i)
}

val tabelle:Array[Person] = Array(new Person("Meier","Hans",1946),
new Person("Schmidt","Hans",1958),new Person("Schmidt","Harald",1965),
new Person("Schmidt","Helmut",1927),new Person("Waalkes","Otto",1958),
new Person("Raab","Stefan",1965),new Person("Meier","Ernst",1978),
new Person("Schmidt","Jonas",2001),new Person("Meier","Otto",1935),
new Person("Raab","Stefan",2006))



def radixFreakinSort(p:Array[Person])={
  vornameQS(p)
  nameMSVL(p)
  jahrDistri(p)
}

def printArray(a:Array[Person]){
  for(i <- 0 to a.length-1){
    println(a(i))
  }
}

/* Erläuterungen:

Vorname ist beliebig zu sortieren, Name und Geburtsjahr muss stabil sein. Über die Nachnamen
und Vornamen ist soweit nicht viel bekannt, deshalb wurde auf schnelle Algorithmen Wert gelegt.
Beim Vornamen kommt aus Variationsgründen Quicksort und bei den Namen Mergesort. Abschließend
wird bei den Jahreszahlen ein DistriSort genutzt, da über die Jahre (aka Sortierschlüssel) bekannt
ist, dass sie sich in einem Range befinden. Die Tatsache, dass viele Personen im gleichen Jahr
geboren sind, macht hier meiner Meinung nach keinen Sinn (da u.a. 3WayQS nicht stabil ist und mir
kein weiterer Algorithmus bekannt ist, der besonders gut oder schlecht für viele selbe Elemente
funktionert! (QS ist besonders schlecht an sich, aber eben auch instabil))
Somit ist der Alg für die Jahre der schnellst mögliche Sortieralgorithmus. Die Tatsache, dass die
Schlüssel in [1900;2011] liegen hat keine weiteren Effizienzsteigerungen zugelassen (also z.B. Fächer
zwischen 1,2 bzw. 9,0), da dies durch den Alg selber "entschieden" wird.

Frage: Welche Komplexität hat der Algorithmus? Vermutung ist, dass es n*log(n) ist, da QS und MS
diese Komplexität haben, zwar hat DistriSort n aber ist damit eben auch O(n*log(n)) und nach Summen-
regel folgt Behauptung.

*/

/* Hypothetisches Hauptprogramm:

object Task4{
  def main(args:Array[String]):Unit={
    println("Array wird ausgegeben:")
    printArray(tabelle)
    radixFreakinSort(tabelle)
    Thread.sleep(1000)
    println("Array wird ausgegeben: [sortier]")
    printArray(tabelle)
  }
}

*/