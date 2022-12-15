module Point2 : sig
  type t = { x : float; y : float }
end

module Point3 : sig
  type t = { x : float; y : float; z : float }
end

type point = Point2.t
type point3 = Point3.t
