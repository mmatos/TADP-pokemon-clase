package ar.org.tadp.pokemon
import scala.util.Try

object Actividades {
  //Parte 1
	type Actividad = Pokemon => Pokemon
  
 	def descansar(pokemon: Pokemon) = {
	  //Parte 1
		val descansado = pokemon.energia(pokemon.energiaMaxima)
		//Parte 3
		(if (pokemon.estado == Sano && pokemon.energia < pokemon.energiaMaxima / 2) 
		   descansado.estado(Dormido()) 
		else descansado)
		  .copy(ataques = pokemon.ataques.mapValues{ case (_, maxAP) => (maxAP, maxAP) }) // Parte 6
	}

	def levantarPesas(kilos: Int)(pokemon: Pokemon) = {
		require(! pokemon.especie.esDeTipo(Fantasma), "Los fantasmas no levantan pesas")

		pokemon match {
		  //Parte 3
			case Pokemon(_, _, _, _, Paralizado) => pokemon.estado(KO)
			//Partes 1 y 3
			case _ if pokemon.fuerza * 10 < kilos => pokemon.energia(-10).estado(Paralizado)
			//Parte 1
			case _ if pokemon.especie.esDeTipo(Pelea) => pokemon.ganarExp(kilos * 2)
			case _ => pokemon.ganarExp(kilos)
		}
	}

	def nadar(minutos: Int)(pokemon: Pokemon) = {
		pokemon.especie.tipos match {
		  //Parte 3
			case (tipo, _) if Agua leGanaA tipo => pokemon.estado(KO)
			case (_, Some(tipo)) if Agua leGanaA tipo => pokemon.estado(KO)
			//Parte 1
			case (Agua, _) => pokemon.energia(-minutos).velocidad(minutos / 60).ganarExp(minutos * 200)
			case _ => pokemon.energia(-minutos).ganarExp(minutos * 200)
		}
	}
	
	//Parte 5
	def fingirIntercambio(pokemon: Pokemon) = {
	  val pokemonTriste = pokemon.energia(-10)
		pokemon.especie.condicionEvolutiva.foldLeft(pokemonTriste)({(poke, condicion)=>
		      condicion match {
		        case Intercambiar(evolucion) => pokemon.copy(especie = evolucion)
		        case _ => poke
		      }
		})
	}
	
	def usarPiedra(piedra: Piedra)(pokemon: Pokemon) = {
	  
	  pokemon.especie.condicionEvolutiva.foldLeft(pokemon)( { (poke, condicion) => 
	    condicion match {
	      case UsarPiedra(piedraRequerida, evolucion) if piedra == piedraRequerida =>
	        pokemon.copy(especie = evolucion)
	      case _ => poke
	    }
	  })
	  
	}
	
	//Parte 6
	def realizar(ataque: Ataque)(pokemon: Pokemon) = {
		val (ap, apMax) = pokemon.ataques(ataque)

		require(ap > 0, "El pokémon debe saber el ataque y tener APs suficientes")

		val pokemonCrecido = pokemon.copy(ataques = pokemon.ataques.updated(ataque, (ap - 1, apMax))).ganarExp(
			(ataque.tipo, pokemon.especie.tipos,pokemon.genero) match {
				case (Dragon, _, _) => 80
				case (tipoAtaque, (tipo,_), _) if tipoAtaque == tipo => 50
				case (tipoAtaque, (_, Some(tipo)), _) if tipoAtaque == tipo => 25
			}
		)

		ataque.efecto.fold(pokemonCrecido)(f => f(pokemonCrecido))
	}
	
	//Parte 4
	class Rutina(val nombre: String)(actividades: Actividad*) {
		def apply(pokemon: Pokemon) = Try((pokemon /: actividades){ (p, a) => a(p) })
	}

	//Parte 7
	def mejorRutina(criterio: Pokemon => Int)(pokemon: Pokemon)(rutinas: Rutina*) = {
		val resultados = for {
			rutina <- rutinas
			pokemonEntrenado <- rutina(pokemon).toOption
			valor = criterio(pokemonEntrenado)
		} yield (rutina.nombre, valor)

		resultados.sortBy(_._1).reverse.headOption
	}
}