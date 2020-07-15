package de.bwhc.util.ddd


import java.time.Instant


trait Entity
{
  type Id

  val id: Id
  val lastUpdate: Instant
}

abstract class BaseEntity[I] extends Entity
{
  type Id = I
}
