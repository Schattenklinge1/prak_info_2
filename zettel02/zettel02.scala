/*
  Kurs: Praktische Informatik 2
  Gruppe: Simon Schmitt, Benjamin Heidelmeier, Martin Lellep
  Tutor: Florian Dobener
  Zettel: 02
*/

// =======================================================================================
// Aufgabe 1
// =======================================================================================

// a.)


// Unterschied: In der Vorlesung wandert der erste Iterator von oben nach unten und der zweite von unten nach oben.
// In der Animation hingegen wandert der erste Iterator von unten nach oben und der zweite von oben nach unten.
// Man kann also sagen, dass sich die Bereiche des bereits sortierten einmal oben (VL) und einmal unten (Animation)
// befinden.
// Gemeinsamkeit ist, dass beide gegenläufig gehen!
// Alle Grenzen und Richtungen einfach umkehren!

  def swap[T](a:Array[T],i:Int,j:Int)={
    var temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def bubbleInternet[T <% Ordered[T]](a:Array[T]):Unit={

    var done = false
    for(j <- 0 to a.length-2){
      if(done) return else done=true
      for(i <- j+1 to a.length-1 reverse){
	if(a(i) < a(i-1)){
	  done=false
	  swap(a,i,i-1)
	}
      }
    }

  }


// b.)

// Hier wird in jedem Schritt überprüft, ob das Teilintervall sortiert ist und zusätzlich noch ob
// ein Element aus dem noch nicht betrachteten Intervall existiert, welches größer ist als die in dem
// sortierten!

  def isSortedIntervall[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Boolean={
    for(i <- lo to hi){
      if(a(i)>a(i+1)) return false
    }
    true
  }

  /**
  @param b:Int The index number the array is sorted from on.
  */

  def biggerThan[T <% Ordered[T]](a:Array[T],b:Int):Boolean={

    val unsorted:Array[T] = a.drop(b+1)
    val sorted:Array[T] = a.dropRight(a.length-1-b)
    !unsorted.exists(e => sorted.exists(x => x>e))
  }

  def bubbleInternetAssertions[T <% Ordered[T]](a:Array[T]):Unit={        // ginge auch "(a:Array[Ordered])"?

    var done = false
    for(j <- 0 to a.length-2){
      if(done) return else done=true
      for(i <- j+1 to a.length-1 reverse){
	if(a(i) < a(i-1)){
	  done=false
	  swap(a,i,i-1)
	  assert(a(i) > a(i-1),"swap not successful")
	}
      }
      assert(isSortedIntervall(a,0,j),"not sorted")
      assert(biggerThan(a,j),"some elements are not sorted correctly")
    }

  }

// c.)

  import scala.io._

  case class Company(name:String,money:Int) extends Ordered[Company]{
    override def compare(that:Company):Int = this.money.compare(that.money)
  }

  def makeArray():Array[Company]={
    // Einlesen wie immer
    var file = Source.fromFile("fortunes.txt").getLines
    var result:List[Company] = List()
    for(line <- file){
      val pureData:Array[String]=line.split(';')
      val data = Company(pureData(0),pureData(1).toInt)
      result = data :: result
    }
    result.reverse.toArray
  }


  def bubbleInternetAssertionsCounter[T <% Ordered[T]](a:Array[T]):Int={        // ginge auch "(a:Array[Ordered])"?
    var done = false
    var counter = 0;
    for(j <- 0 to a.length-2){
      if(done) return counter else done=true
      for(i <- j+1 to a.length-1 reverse){
	if(a(i) < a(i-1)){
	  done=false
	  swap(a,i,i-1)
	  counter += 1
	  assert(a(i) > a(i-1),"swap not successful")
	}
      }
      assert(isSortedIntervall(a,0,j),"not sorted")
      assert(biggerThan(a,j),"some elements are not sorted correctly")
    }
    counter
  }

// man erhält hier: 252644

  def vorlesungCounter[T <% Ordered[T]](a:Array[T]):Int={

    var done = false
    var counter = 0
    for(j <- 1 to a.length-1 reverse){
      if(done) return counter else done = true
      for(i <- 0 to j-1){
	if(a(i) > a(i+1)){
	  done = false
	  swap(a,i,i+1)
	  counter += 1
	}
      }
    }
    counter
  }

// man erhält hier: 252644 Swaps!

// Der einzige Unterschied ist wie o.g. nur, dass beide Programme spiegelverkehrt vorgehene. Damit ist auch die benötigte Anzahl
// der Swaps die selbe!

// =======================================================================================
// Aufgabe 2
// =======================================================================================

// a.) und b.)

  def partition[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Int={
    require(lo < hi,"no termination is asumed")
    val pivot = a(hi)
    var k=lo
    for(i <- lo to hi-1){
      if(a(i)< pivot){
	swap(a,i,k)
	k += 1
      }
      require(a.slice(lo,k).forall(_< pivot),"partitioning didnt work out quite well")
    }
    swap(a,k,hi)
    k
  }

  def quickSort[T <% Ordered[T]](ar:Array[T])={
    def qSort[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Unit = {
      if(lo< hi){
	val q = partition(a,lo,hi)
	qSort(a,lo,q-1)
	qSort(a,q+1,hi)
      }
    }
    qSort(ar,0,ar.length-1)
  }

  // man könnte noch isSorted am Ende des Gerüstes "quickSort" setzen!
  // genauso könnte man am Ende einer (jeden) Partitionierung noch überprüfen,
  // ob alles geklappt hat (wäre so ziemlich die selbe wie die in Partitionsfkt!


// c.)

  def makeArray(size:Int):Array[Int]={
    val result = new Array[Int](size)
    val randomGenerator = scala.util.Random
    for(i <- 0 to size-1){
      result(i)=randomGenerator.nextInt(1000)
    }
    result
  }

  val randomNumbers = makeArray(100000)

// d.)

  def zeit[T](f: => T)={          // ist das über callbyvalue?! (also das "val r = f" schon, und das "=> T") oder nur von was es ausgeht ist egal?!
    val now=System.nanoTime
    val result=f
    println((System.nanoTime-now)/1000000.0+" msec")
  }

  def threeWayQuickSort[T <% Ordered[T]](a:Array[T]):Unit={
    def recHelp(a:Array[T],lo:Int,hi:Int):Unit={
      if(lo < hi){
	val v = a(lo)
	var (lt,i,gt) = (lo,lo,hi)
	while(i <= gt){
	  if(a(i)>v) {swap(a,i,gt);gt-=1}
	  else if(a(i)<v) {swap(a,lt,i);lt+=1;i+=1}
	  else i+=1
	}
	recHelp(a,lo,lt-1)
	recHelp(a,gt+1,hi)
      }
    }
    recHelp(a,0,a.length-1)
  }

// FÃœr n=100000 funktioniert nur 3WayQS (zeit ca 120 ms)
// Für andere ähnlich große Arrays hab ich es mal versehentlich laufen lassen und es kam
// zum Ende, hat aber ewig gedauert!

// Für n=1000: 3WayQS ca 7.34 ms
//             quickSort ca 102.5 ms!

// Viel viel länger!!

// Begründung: Der größte Unterschied ist, dass es nun drei Bereiche gibt. Deshalb wandert nicht nur die untere Grenze nach unten, sondern auch dabei die
// obere nach unten. Es geht schneller, bis durch iteriert ist (bzw. while abbricht!)!

// =======================================================================================
// Aufgabe 3
// =======================================================================================

  /**
  @param range:Int The upper barrier (excluded!)
  */
  def generateRandomInts(n:Int,range:Int,add:Int):Array[Int]={
    val result = new Array[Int](n)
    for(i <- 0 to n-1){
      result(i)=scala.util.Random.nextInt(range)+add
    }
    result
  }
// es gibt sicherlich iwo eine Methode, die Random in Bereich liefert!

  def compareThreeInts[T <% Ordered[T]](n1:T,n2:T,n3:T):T={
    val list = List(n1,n2,n3).sortBy(e => e)
    list(1)
    // ginge SICHERLICH eleganter, aber erfüllt (wenn auch nur
    // statischen) Zweck!
  }

  def partition[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Int={
    require(lo < hi,"no termination is asumed")
    val randomNumber = generateRandomInts(3,hi-lo+1,lo)
    val pivot = compareThreeInts(a(randomNumber(0)),a(randomNumber(1)),a(randomNumber(2)))
    var k=lo
    for(i <- lo to hi-1){
      if(a(i)< pivot){
	swap(a,i,k)
	k += 1
      }
      require(a.slice(lo,k).forall(_< pivot),"partitioning didnt work out quite well")
    }
    swap(a,k,hi)
    k
  }

  def quickSort[T <% Ordered[T]](ar:Array[T])={
    def qSort[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Unit = {
      if(lo< hi){
	val q = partition(a,lo,hi)
	qSort(a,lo,q-1)
	qSort(a,q+1,hi)
      }
    }
    qSort(ar,0,ar.length-1)
  }

  def makeArray(n:Int):Array[Int]={
    val result = new Array[Int](n)
    for(i <- 0 to n-1){
      result(i) = scala.util.Random.nextInt(1000)
    }
    result
  }


  def isPermutation[T <% Ordered[T]](a:Array[T],b:Array[T]):Boolean={
    a.forall(x => (a.count(_==x) == b.count(_==x)))
  }

  def isSorted[T <% Ordered[T]](a:Array[T]):Boolean={
    for(i <- 0 to a.length-2){
      if(a(i)> a(i+1)) return false
    }
    true
  }

  def insertionSort[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Unit={
    for(j <- lo to hi-1){                     // Index, der angibt, bis wohin bereits sortiert ist! In der VL sind Grenzen etc um eins nach rechts verschoben, aber ist egal!
      var k = j
      while(k>=0 && a(k)>a(k+1)){             // sortiert an richtige Stelle
	swap(a,k,k+1)
	k -= 1
      }
    }

  }

  def partition[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Int={
    require(lo < hi,"no termination is asumed")
    val pivot = a(hi)
    var k=lo
    for(i <- lo to hi-1){
      if(a(i) < pivot){
	swap(a,i,k)
	k += 1
      }
      require(a.slice(lo,k).forall(_< pivot),"partitioning didnt work out quite well")
    }
    swap(a,k,hi)
    k
  }

// man kann sagen, dass das Parameter über die Größe des Teilarrays also die Differenz
// hi-lo ist!

  def quickSortComb[T <% Ordered[T]](ar:Array[T],border:Int)={
    def isSmall(n:Int,krit:Int) = n < krit
    def qSort[T <% Ordered[T]](a:Array[T],lo:Int,hi:Int):Unit = {
      if(lo< hi){
	// Partitionierung muss auf jeden Fall gemacht werden!
	val q = partition(a,lo,hi)
	// Dann bei den 2 Teilarrays eben abfragen ob isSmall!
	if(isSmall((q-1)-lo,border)){
	  insertionSort(a,lo,q-1)
	}else{
	  qSort(a,lo,q-1)
	}
	if(isSmall(hi-(q+1),border)){
	  insertionSort(a,q+1,hi)
	}else{
	  qSort(a,q+1,hi)
	}
      }
    }
    qSort(ar,0,ar.length-1)
  }

  def zeit[T](f: => T)={
    val now = System.nanoTime
    val result = f
    println((System.nanoTime-now)/1000000.0+" msec ")
  }


// Getestet mit makeArray(10000) und man konnte glaube ich feststellen, dann border=1000 ok war - relativ unerwartet ziemlich hoch!!
// Aber insgesamt nicht so großen Speed-Zuwachs!! :-(

