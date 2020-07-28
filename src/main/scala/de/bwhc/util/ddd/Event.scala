package de.bwhc.util.ddd



import java.time.Instant


trait Event
{
  val timestamp: Instant
}


trait EventPublisher[E <: Event, Response, F[_]]
{
  def publish(ev: E): F[Response]
}
