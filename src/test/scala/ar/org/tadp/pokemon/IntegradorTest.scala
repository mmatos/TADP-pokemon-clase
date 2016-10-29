package ar.org.tadp.pokemon
import scala.util.{ Success, Failure, Try }
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import ar.org.tadp.pokemon.Actividades._


class IntegradorTest extends FreeSpec with Matchers with BeforeAndAfter {
		
  // Parte 1 
  
  val caracteristicas = new Caracteristicas(1, 100, 200, 20, 10, 10)
	val gastly = new Especie(tipos = (Fantasma, Some(Veneno)))(20, 80, new Incrementos) 
	val pikachu = new Especie(tipos = (Electrico, None))(80, 80, new Incrementos) 
	val machop = new Especie(tipos = (Pelea, None))(1, 80, new Incrementos) 
	val charmander = new Especie(tipos = (Fuego, None))(350, 20, new Incrementos)
	
	
	
	val unGastly = new Pokemon(gastly, Macho, caracteristicas)
	val unMachop = new Pokemon(machop, Macho, caracteristicas.copy(fuerza = 99))
	val unCharmander = new Pokemon(charmander, Macho, caracteristicas)
	
	
	val actividades : List[Actividad] = List(descansar, nadar(10), nadar(3), nadar(1))
	
	// Parte 5 

  val actividadesConPesas : List[Actividad] = List(descansar, levantarPesas(1))

  "Integrador" - {

    "Parte 1" - {

      "Al hacer que un machop levante pesas, gana el doble de kg en experiencia" in {
        
        val caracteristicasLuegoDePesas = caracteristicas.copy(fuerza = 99, experiencia = 3)

        levantarPesas(1)(unMachop) should be(unMachop.copy(caracteristicas = caracteristicasLuegoDePesas)) 
      }
      
      "Si es un pokemon de tipo fantasma, al intentar levantar pesas deberia tirar una excepcion" in {
        a[Exception] should be thrownBy{
        levantarPesas(10)(unGastly)  
        }
        
      }
      
      }
    
    "Parte 2" - {
      
      "Al tener mas experiencia que la que necesitaba para levelear, sube de nivel" ignore {
        val caracteristicasParaLevelear = caracteristicas.copy(experiencia = 351)
        unCharmander.copy(caracteristicas = caracteristicasParaLevelear).nivel should be(2)
 
      }
    }
    
    "Parte 3 : Estados" - {
      "Al nadar, si pierde contra el agua deberia estar KO" in {
        nadar(1)(unCharmander).estado should be(KO)
      }
      
      "Al levantar pesas, si son mas de 10 kg por punto de fuerza" in {
        levantarPesas(10000)(unMachop).estado should be(Paralizado)
      }
      
    }

    }
  
}