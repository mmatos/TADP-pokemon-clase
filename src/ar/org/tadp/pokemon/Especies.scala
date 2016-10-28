package ar.org.tadp.pokemon


	case class Especie(
		tipos: (Tipo, Option[Tipo]),
		//Parte 5
		condicionEvolutiva: List[CondicionEvolutiva] = Nil)(
		val resistenciaEvolutiva: Int = 200,
		val pesoMaximo: Int = 80,
		val incrementos: Incrementos
	){
    def esDeTipo(tipo : Tipo) : Boolean = {
      tipos match {
        case (t, _) if t == tipo => true
        case (_, Some(t)) if t == tipo => true
        case _ => false
      }
    }
  }
	
	case class Incrementos (
		energiaMaxima: Int = 0,
		peso: Int = 0,
		fuerza: Int = 0,
		velocidad: Int = 0
	)
	
	//Parte 5
	class CondicionEvolutiva(evolucion: Especie) { def subioDeNivel(pokemon: Pokemon) = pokemon	}
	case class Intercambiar(evolucion: Especie) extends CondicionEvolutiva(evolucion)
	case class UsarPiedra(piedra: Piedra, evolucion: Especie) extends CondicionEvolutiva(evolucion)
	case class Nivel(n: Int, evolucion: Especie) extends CondicionEvolutiva(evolucion) {
		override def subioDeNivel(pokemon: Pokemon) = if (pokemon.nivel >= n) pokemon.copy(especie = evolucion) else pokemon
	}
	
	//Parte 5
	trait Piedra
	case class PiedraDeTipo(tipo: Tipo) extends Piedra
	case object PiedraLunar extends Piedra
	
	
	trait Tipo {
		def tiposInferiores: List[Tipo]
		def leGanaA(otro: Tipo) = tiposInferiores.contains(otro)
	}
	case object Fuego extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Hielo, Bicho) }
	case object Agua extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Tierra, Roca) }
	case object Planta extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Agua, Tierra, Roca) }
	case object Tierra extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Electrico, Veneno, Roca) }
	case object Hielo extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Tierra, Volador, Dragon) }
	case object Roca extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Fuego, Hielo, Volador, Bicho) }
	case object Electrico extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Agua, Volador) }
	case object Psiquico extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Pelea, Veneno) }
	case object Pelea extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Normal, Hielo, Roca) }
	case object Fantasma extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Psiquico, Fantasma) }
	case object Volador extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Pelea, Bicho) }
	case object Bicho extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta, Psiquico) }
	case object Veneno extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Planta) }
	case object Dragon extends Tipo { lazy val tiposInferiores: List[Tipo] = List(Dragon) }
	case object Normal extends Tipo { lazy val tiposInferiores: List[Tipo] = List() }