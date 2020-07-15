package de.bwhc.util.ddd



import java.time.Instant


trait Event
{
  val timestamp: Instant
}
