package ar.org.tadp.pokemon

case class Ataque(tipo: Tipo, puntosDeAtaqueMaximo: Int, efecto: Option[Pokemon => Pokemon])
