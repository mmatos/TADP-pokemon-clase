package ar.org.tadp.pokemon
import scala.util.Try
import ar.org.tadp.pokemon.Actividades._

  case class Pokemon(
		especie: Especie,
		genero: Genero,
		caracteristicas: Caracteristicas,
		ataques: Map[Ataque, (Int, Int)] = Map(), // Parte 6
		estado: Estado = Sano //Parte 3
	) {
  
		require(experiencia > 0, "La Experiencia debe ser positiva")
		require(energiaMaxima > 0, "La Energia Maxima debe ser positiva")
		require(energia > 0 && energia < energiaMaxima, s"La Energia debe ir de 0 a $energiaMaxima")
		require(fuerza >= 1 && fuerza <= 100, "La Fuerza debe ir de 1 a 100")
		require(velocidad >= 1 && velocidad <= 100, "La Velocidad debe ir de 1 a 100")
		
  	def energia = caracteristicas.energia
		def experiencia = caracteristicas.experiencia
		def energiaMaxima = caracteristicas.energiaMaxima + nivel * especie.incrementos.energiaMaxima
		def fuerza = caracteristicas.fuerza + nivel * especie.incrementos.fuerza
		def velocidad = caracteristicas.velocidad + nivel * especie.incrementos.velocidad
		
		// Parte 2
		def nivel: Int = {
			def nivelR(expNivelAnterior: Int, nivelAnterior: Int): Int = {
				val expNivelSiguiente = 2 * expNivelAnterior + especie.resistenciaEvolutiva
				if (experiencia > expNivelSiguiente) nivelAnterior else nivelR(expNivelSiguiente, nivelAnterior + 1)
			}
			nivelR(0, 1)
		}
		
		def ganarExp(exp: Int) = {
			val crecido = copy(caracteristicas = caracteristicas.copy(experiencia = experiencia + exp))
			
			//Parte 5
			if(crecido.nivel > this.nivel){
			  especie.condicionEvolutiva
			    .map(_.subioDeNivel(crecido))
			    .foldLeft(crecido)({(semilla, potencialmenteEvolucionado) => 
			      if(potencialmenteEvolucionado.especie != this.especie)
			        potencialmenteEvolucionado
			      else semilla})
			} else {
			  crecido
			}
		}

		def energia(delta: Int) = copy(caracteristicas = caracteristicas.copy(energia = energia + delta min energiaMaxima))
		def fuerza(delta: Int) = copy(caracteristicas = caracteristicas.copy(fuerza = fuerza + delta))
		def velocidad(delta: Int) = copy(caracteristicas = caracteristicas.copy(velocidad = velocidad + delta))
		def estado(estado: Estado) = copy(estado = estado)
		
		//Parte 3
		def hacer(actividad: Actividad): Try[Pokemon] = Try{
		  require(estado != KO, "No puede hacer actividades si está K.O.")
		  
			estado match {
				case Dormido(n) => estado(Dormido(n - 1))
				case _ => actividad(this)
			}
		}
}

	case class Caracteristicas(
		experiencia: Int,
		energia: Int,
		energiaMaxima: Int,
		peso: Int,
		fuerza: Int,
		velocidad: Int
	)
	
	sealed trait Genero
	case object Macho extends Genero
	case object Hembra extends Genero

	//Parte 3
	sealed trait Estado
	case class Dormido(turnosPendientes: Int = 3) extends Estado
	case object KO extends Estado
	case object Sano extends Estado
	case object Paralizado extends Estado
	
