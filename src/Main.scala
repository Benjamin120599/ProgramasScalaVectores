import scala.io.StdIn

object Main {

  def main(args: Array[String]): Unit = {

    /*var datos = new Array[Double](8)
    datos(0) = 9
    datos(1) = 3
    datos(2) = 8
    datos(3) = 8
    datos(4) = 9
    datos(5) = 8
    datos(6) = 9
    datos(7) = 18

    var datos = llenarVector()

    println("Media: "+calcularMedia(datos, 0, 0))

    println("Moda: "+calcularModa(datos))

    //Ordenar el Vector
    println("Mediana: "+calcularMediana(datos))

    println("VALORES DESVIACIÓN RESPECTO A LA MEDIA: ")
    calcularDesviacionRespectoMedia(datos).foreach(println)

    println("Desviación Media: "+calcularDesviacionMedia(datos, calcularMedia(datos, 0, 0), 0, 0))

    println("Varianza: "+calcularVarianza(datos, calcularMedia(datos, 0, 0), 0, 0))

    println("Desviación Estándar: "+calcularDesviacionEstandar(datos))

    imprimirVector(datos, 0)

    imprimirVector(ordenarVector(datos, 0, 0), 0)*/


    var opcion = 0

    var datos = llenarVector()

    do {

      println("================== M E N U ==================")
      println("")
      println("1) Calcular la Media")
      println("2) Calcular la Moda")
      println("3) Calcular Mediana")
      println("4) Calcular la desviación respecto de la media")
      println("5) Calcular la desviación media")
      println("6) Calcular la Varianza")
      println("7) Calcular la Desviación Estándar")
      println("8) Imprimir Vector")
      println("9) Imprimir Vector Ordenado")
      println("0) Salir.")
      println("")
      println("------------------------------------------------")
      println("")
      println("Elige una opción: ")
      opcion = StdIn.readInt()

      if(opcion == 1) {

        println("================== Calcular la Media ==================\n")
        println("La media de los valores ingresados es: "+calcularMedia(datos, 0, 0) )

      } else if(opcion == 2) {

        println("================== Calcular la Moda ==================\n")
        println("La Moda de los valores ingresados es: "+calcularModa(datos))

      } else if(opcion == 3) {

        println("================== Calcular la Mediana ==================\n")
        println("La Mediana de los valores ingresados es: "+calcularMediana(ordenarVector(datos, 0, 0)))

      } else if(opcion == 4) {

        println("================== Calcular La Desviación Respecto de la Media ==================\n")
        println("VALORES DESVIACIÓN RESPECTO A LA MEDIA: ")
        calcularDesviacionRespectoMedia(datos).foreach(println)

      } else if(opcion == 5) {

        println("================== Calcular la Desviación Media ==================\n")
        println("La Desviación Media de los valores ingresados es: "+calcularDesviacionMedia(datos, calcularMedia(datos, 0, 0), 0, 0))

      } else if(opcion == 6) {

        println("================== Calcular la Varianza ==================\n")
        println("La Varianza de los valores ingresados es: "+calcularVarianza(datos, calcularMedia(datos, 0, 0), 0, 0))

      } else if(opcion == 7) {

        println("================== Calcular la Desviación Estándar ==================\n")
        println("La Desviación Estándar de los valores ingresados es: "+calcularDesviacionEstandar(datos))

      } else if(opcion == 8) {

        println("================== Imprimir Vector ==================\n")
        imprimirVector(datos, 0)

      } else if(opcion == 9) {

        println("================== Imprimir Vector Ordenado ==================\n")
        imprimirVector(ordenarVector(datos, 0, 0), 0)

      }

    } while(opcion != 0)


  }

  def calcularMedia(datos:Array[Double], posicion:Int, media:Double) : Double = {
    var resultado = media
    if(posicion < datos.length) {
      resultado = resultado + datos(posicion)
      calcularMedia(datos, posicion+1, resultado)
    } else {
      return resultado/datos.length
    }
  }

  def calcularModa(datos:Array[Double]): Double = {

    var ultimaModa = 0
    var moda = 0.0

    for(i<-0 until datos.length) {
      var cant = 0

      for(j<-0 until( datos.length)){
        if(datos(i) == datos(j)) {
          cant += 1
        }
        if(cant>ultimaModa){
          moda = datos(i)
          ultimaModa = cant
        }
      }
    }

    return moda

  }

  def calcularMediana(datos:Array[Double]): Double = {

    var mediana = 0.0

    if((datos.length)%2 == 0) {
      mediana = datos((datos.length/2)-1)
    } else {
      mediana = datos((datos.length/2))
    }
    return mediana
  }

  def calcularDesviacionRespectoMedia(datos:Array[Double]) : Array[Double] = {
    var desvMedia = new Array[Double](datos.length)
    for(i<-0 until datos.length) {
      desvMedia(i) = datos(i) - calcularMedia(datos, 0, 0)//Math.abs(datos(i) - calcularMedia(datos, 0, 0))
    }
    return desvMedia
  }

  def calcularDesviacionMedia(datos:Array[Double], media:Double,posicion:Int, desviacion:Double) : Double = {

    var desvMedia = desviacion

    if(posicion < datos.length) {
      desvMedia = desvMedia + Math.abs(calcularDesviacionRespectoMedia(datos)(posicion))//desvMedia + (Math.abs(datos(posicion) - media))
      calcularDesviacionMedia(datos, media, posicion+1, desvMedia)
    } else  {
      desvMedia = desvMedia / datos.length
      return  desvMedia
    }
  }

  def calcularVarianza(datos:Array[Double], media:Double,posicion:Int, vari:Double) : Double = {

    var varianza = vari

    if(posicion < datos.length) {
      varianza = varianza + Math.pow(calcularDesviacionRespectoMedia(datos)(posicion), 2)
      calcularVarianza(datos, media, posicion+1, varianza)
    } else  {
      varianza = varianza / datos.length
      return  varianza
    }

  }

  def calcularDesviacionEstandar(datos:Array[Double]): Double = {
    return math.sqrt(calcularVarianza(datos, calcularMedia(datos, 0, 0), 0, 0))
  }

  def llenarVector() : Array[Double] = {
    println("Ingresa el tamaño del vector: ")
    var tamaño = StdIn.readInt()

    var arreglo = new Array[Double](tamaño)

    for(i<-0 until tamaño) {
      println("Ingresa un valor: ")
      var valor = StdIn.readDouble()
      arreglo(i) = valor
    }

    return arreglo

  }

  def imprimirVector(arreglo:Array[Double], posicion:Int): Unit = {
    if(posicion < arreglo.length) {
      println("El valor en la posición "+(posicion+1)+" es "+arreglo(posicion))
      imprimirVector(arreglo, posicion+1)
    }
  }

  def ordenarVector(arreglo:Array[Double], i:Int, valor:Double) : Array[Double] = {
    for(x<-0 until arreglo.length) {
      for(i<-0 until arreglo.length-x-1) {
        if( arreglo(i) > arreglo(i+1) ){
          var tmp = arreglo(i+1)
          arreglo(i+1) = arreglo(i)
          arreglo(i) = tmp
        }
      }
    }
    return arreglo
  }

}
