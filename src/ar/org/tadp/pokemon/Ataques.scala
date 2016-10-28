package ar.org.tadp.pokemon

case class Ataque(tipo: Tipo, puntosDeAtaqueMaximo: Int, efecto: Option[Pokemon => Pokemon])

object EfectosDeAtaque {
  
  def reposar(pokemon : Pokemon) : Pokemon = pokemon.energia(pokemon.energiaMaxima).estado(Dormido())
  def enfocarse(pokemon : Pokemon) : Pokemon = pokemon.velocidad(1)
  def endurecerse(pokemon : Pokemon) : Pokemon = pokemon.energia(5).estado(Paralizado)
}