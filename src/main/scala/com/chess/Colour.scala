package com.chess

/**
  * Created by Karl-Mattias on 30.12.2016.
  */
sealed trait Colour

case object White extends Colour
case object Black extends Colour
case object NoColour extends Colour
