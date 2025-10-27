module Point2 : sig
  type t = { x : float; y : float }

  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : float -> t -> t
  val norm2 : t -> float
  val norm : t -> float
  val dot : t -> t -> float
  val det : t -> t -> float
end

module Point3 : sig
  type t = { x : float; y : float; z : float }
end

type point = Point2.t
type point3 = Point3.t
